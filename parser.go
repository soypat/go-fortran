package fortran

import (
	"errors"
	"fmt"
	"io"
	"slices"
	"strconv"
	"strings"
	"unsafe"

	"github.com/soypat/go-fortran/ast"
	"github.com/soypat/go-fortran/token"
)

// Set via -ldflags: go test -ldflags="-X 'github.com/soypat/go-fortran.debugNoStuckCheck=1'"
var debugNoStuckCheck string

// VarFlags tracks attributes and semantic properties of Fortran variables.
// Flags are set during parsing and used during transpilation to determine
// the correct Go representation and access patterns.
type VarFlags uint64

const (
	// VFlagImplicit: Type was inferred from IMPLICIT rules rather than explicit declaration.
	// Set when: Variable used without prior declaration, type derived from first letter.
	VFlagImplicit VarFlags = 1 << iota
	// VFlagUsed: Symbol is referenced somewhere in the code.
	// Set when: Any use of the identifier in expressions or statements.
	VFlagUsed
	// VFlagPointer: Variable has POINTER attribute or is a Cray-style pointer.
	// Set when: "INTEGER, POINTER :: x" (F90) or "POINTER (ptr, pointee)" (Cray ptr).
	// For Cray: the pointer variable (ptr) holds an address, not auto-dereferenced.
	// For F90 POINTER: typically combined with VFlagDimension for pointer arrays.
	VFlagPointer
	// VFlagTarget: Variable has TARGET attribute (can be pointed to by F90 pointers).
	// Set when: "INTEGER, TARGET :: x"
	VFlagTarget
	// VFlagParameter: Is a dummy argument (parameter) of a function/subroutine.
	// Set when: Variable appears in procedure's parameter list.
	VFlagParameter
	// VFlagAllocatable: Variable has ALLOCATABLE attribute.
	// Set when: "INTEGER, ALLOCATABLE :: arr(:)" - dynamic allocation via ALLOCATE.
	VFlagAllocatable
	// VFlagCommon: Variable is in a COMMON block (shared storage).
	// Set when: Variable appears in a COMMON statement.
	VFlagCommon
	// VFlagPointee: Cray-style pointee accessed through a pointer variable.
	// Set when: "POINTER (ptr, pointee)" - pointee is accessed via ptr.
	// Access to pointee requires dereferencing the pointer variable.
	VFlagPointee
	// VFlagDimension: Variable is an array (has DIMENSION attribute or explicit bounds).
	// Set when: "INTEGER :: arr(10)" or "INTEGER, DIMENSION(:) :: arr"
	// Go type: *intrinsic.Array[T]
	VFlagDimension
	// VFlagIntentOut: Parameter has INTENT(OUT) - callee provides value.
	VFlagIntentOut
	// VFlagIntentIn: Parameter has INTENT(IN) - caller provides value.
	VFlagIntentIn
	// VFlagArrayInit: Array has been initialized via DATA or inline initializer.
	VFlagArrayInit
	// VFlagArraySpec: ArraySpec was used in the type declaration.
	VFlagArraySpec
	// VFlagReturned: Variable is the function return value.
	VFlagReturned
	// VFlagRecursive: Function/subroutine has RECURSIVE attribute.
	VFlagRecursive
	// VFlagEquivalenced: Variable shares storage via EQUIVALENCE statement.
	// Set when: "EQUIVALENCE (a, b)" - scalars become PointerTo[T] in Go.
	VFlagEquivalenced
	// VFlagConstantParameter: Compile-time constant (PARAMETER statement or attribute).
	// Set when: "PARAMETER (PI = 3.14159)" or "REAL, PARAMETER :: PI = 3.14"
	VFlagConstantParameter
	// VFlagStmtFunc: Statement function (one-line inline function).
	// Set when: "AREA(R) = 3.14159 * R * R"
	VFlagStmtFunc
)

func (f VarFlags) HasAny(hasBits VarFlags) bool { return f&hasBits != 0 }
func (f VarFlags) HasAll(hasBits VarFlags) bool { return f&hasBits == hasBits }
func (f VarFlags) With(mask VarFlags, setBits bool) VarFlags {
	if setBits {
		return f | mask
	} else {
		return f &^ mask
	}
}

func flagsFromTypespec(ts *ast.TypeSpec) (flags VarFlags) {
	for i := range ts.Attributes {
		attr := &ts.Attributes[i]
		switch attr.Token {
		case token.INTENT:
			if tok, ok := attr.Expr.(*ast.TokenExpr); ok {
				switch tok.Token {
				case token.IN:
					flags |= VFlagIntentIn
				case token.OUT:
					flags |= VFlagIntentOut
				case token.INOUT:
					flags |= VFlagIntentIn | VFlagIntentOut
				}
			}
		case token.PARAMETER:
			flags |= VFlagConstantParameter
		case token.POINTER:
			flags |= VFlagPointer
		case token.ALLOCATABLE:
			flags |= VFlagAllocatable
		case token.TARGET:
			flags |= VFlagTarget
		case token.RECURSIVE:
			flags |= VFlagRecursive
		case token.DIMENSION:
			if len(attr.Dimension.Bounds) > 0 {
				flags |= VFlagDimension
			}
		}
	}
	return flags
}

type ParserError struct {
	sp  sourcePos
	msg string
}

func (pe *ParserError) Error() string {
	var dst []byte
	dst = pe.sp.AppendString(dst)
	dst = append(dst, ':', ' ')
	dst = append(dst, pe.msg...)
	return string(dst)
}

func (pe ParserError) String() string {
	return pe.Error()
}

type sourcePos struct {
	Source string
	Line   int
	Col    int
	Pos    int
}

func (l *sourcePos) String() string {
	return string(l.AppendString(nil))
}

func (l *sourcePos) AppendString(b []byte) []byte {
	if b == nil {
		b = make([]byte, 0, len(l.Source)+3+3)
	}
	b = append(b, l.Source...)
	b = append(b, ':')

	b = strconv.AppendInt(b, int64(l.Line), 10)
	if l.Col > 0 {
		b = append(b, ':')
		b = strconv.AppendInt(b, int64(l.Col), 10)
	}
	return b
}

type Parser90 struct {
	l        Lexer90
	current  toktuple
	peek     toktuple
	uberpeek toktuple
	errors   []ParserError // Collected parsing errors
	vars     ParserUnitData

	maxStatements int
	maxErrs       int
	nStatements   int
	// nSamePosCheckCount counts amount of times a check was performed on the same sourcePosition
	// after reaching a thrshold the parser dies.
	nSamePosCheckCount int
	lastPosCheck       int
	died               bool

	ignoreUndeclaredVars bool
}

func (p *Parser90) makeUnitData(name string, token token.Token) *ParserUnitData {
	return &ParserUnitData{
		name:                     name,
		tok:                      token,
		vars:                     slices.Clone(p.vars.vars),
		implicits:                slices.Clone(p.vars.implicits),
		usesKeywordAsIdentifiers: slices.Clone(p.vars.usesKeywordAsIdentifiers),
		namelists:                slices.Clone(p.vars.namelists),
		returnType:               p.vars.returnType,
		source:                   p.l.Source(),
	}
}

type ParserUnitData struct {
	name                     string
	tok                      token.Token
	vars                     []Varinfo
	returnType               *Varinfo
	implicits                []*ast.ImplicitStatement
	usesKeywordAsIdentifiers []token.Token
	namelists                []ast.NamelistGroup // NAMELIST groups declared in this unit
	source                   string              // Source path.
}

func (p *ParserUnitData) AppendVarinfo(dst []Varinfo) []Varinfo {
	return append(dst, p.vars...)
}

// ProcedureParams returns the varinfos corresponding to parameters of the function, in order.
func (p *ParserUnitData) ProcedureParams() []Varinfo {
	for i := range p.vars {
		if !p.vars[i].flags.HasAny(VFlagParameter) {
			return p.vars[:i] // First varinfo are always function parameters.
		}
	}
	return p.vars
}

// AltReturnCount returns the number of alternate return (*) parameters declared.
func (p *ParserUnitData) AltReturnCount() int {
	n := 0
	for _, v := range p.ProcedureParams() {
		if v._varname == "*" {
			n++
		}
	}
	return n
}

func (p *ParserUnitData) resolveParameterTypes(params []ast.Parameter) error {
	if len(params) > len(p.vars) {
		return errors.New("too many parameters for varinfo size")
	}
	for i := range params {
		vi := &p.vars[i]
		if vi._varname != params[i].Name {
			return fmt.Errorf("mismatched param/varinfo name")

		}
		params[i].Decl = vi.decl
	}
	return nil
}

func (p *ParserUnitData) Varb(name []byte) (vi *Varinfo) {
	return p.Var(unsafe.String(&name[0], len(name)))
}

func (p *ParserUnitData) Var(name string) (vi *Varinfo) {
	for i := len(p.vars) - 1; i >= 0; i-- {
		if strings.EqualFold(p.vars[i]._varname, name) {
			return &p.vars[i]
		}
	}
	return nil
}

// Namelist returns the NAMELIST group with the given name, or nil if not found.
func (p *ParserUnitData) Namelist(name string) *ast.NamelistGroup {
	for i := range p.namelists {
		if strings.EqualFold(p.namelists[i].Name, name) {
			return &p.namelists[i]
		}
	}
	return nil
}

func (pud *ParserUnitData) reset() {
	*pud = ParserUnitData{
		vars:                     pud.vars[:0],
		implicits:                pud.implicits[:0],
		usesKeywordAsIdentifiers: pud.usesKeywordAsIdentifiers[:0],
		namelists:                pud.namelists[:0],
	}
}

func (pud *ParserUnitData) copyFrom(src *ParserUnitData) {
	*pud = ParserUnitData{
		name:       src.name,
		tok:        src.tok,
		vars:       append(pud.vars[:0], src.vars...),
		returnType: src.returnType,
		implicits:  append(pud.implicits[:0], src.implicits...),
		namelists:  append(pud.namelists[:0], src.namelists...),
	}
	for i := range pud.vars {
		if pud.vars[i].flags.HasAny(VFlagReturned) {
			pud.returnType = &pud.vars[i]
			break
		}
	}

}

// isImplicitNone returns true if IMPLICIT NONE is active in this unit.
func (pud *ParserUnitData) isImplicitNone() bool {
	for _, impl := range pud.implicits {
		if impl.IsNone {
			return true
		}
	}
	return false
}

// implicitDeclFor returns a DeclEntity with the implicit type for the given identifier.
// Returns nil if IMPLICIT NONE is active.
func (pud *ParserUnitData) implicitDeclFor(ident string) *ast.DeclEntity {
	if pud.isImplicitNone() {
		return nil
	}
	// Check custom IMPLICIT rules first
	for _, impl := range pud.implicits {
		ts := impl.ImplicitTypeFor(ident)
		if ts != nil {
			return &ast.DeclEntity{Name: ident, Type: ts}
		}
	}
	// Use default Fortran I-N convention
	letter := ident[0]
	if letter >= 'a' && letter <= 'z' {
		letter -= 'a' - 'A'
	}
	if letter >= 'I' && letter <= 'N' {
		return &ast.DeclEntity{Name: ident, Type: _implicitInteger.Type}
	}
	return &ast.DeclEntity{Name: ident, Type: _implicitReal.Type}
}

// resolveImplicitTypes assigns types to variables with nil decl based on IMPLICIT rules.
// Called after specification section is parsed, before executable section.
func (pud *ParserUnitData) resolveImplicitTypes() {
	if pud.isImplicitNone() {
		return // No type resolving.
	}
	for i := range pud.vars {
		vi := &pud.vars[i]
		// Skip variables that already have explicit type declarations.
		// But handle partial decls (e.g., from Cray POINTER with ArraySpec but no Type).
		if vi.decl != nil && vi.decl.Type != nil || vi._varname == "" {
			continue // Already has explicit type
		}
		ident := vi.Identifier()
		var implicitType *ast.TypeSpec
		for _, impl := range pud.implicits {
			ts := impl.ImplicitTypeFor(ident)
			if ts != nil {
				implicitType = ts
				vi.flags |= VFlagImplicit
				break
			}
		}
		if implicitType == nil {
			// Use default Fortran I-N convention
			letter := ident[0]
			if letter >= 'a' && letter <= 'z' {
				letter -= 'a' - 'A'
			}
			if letter >= 'I' && letter <= 'N' {
				implicitType = _implicitInteger.Type
			} else {
				implicitType = _implicitReal.Type
			}
			// Mark as implicit so transformImplicitTypeDeclarations generates var declaration.
			// Skip VFlagPointer (Cray pointers) - they have special handling in transformTypeDeclEntity.
			if !vi.flags.HasAny(VFlagPointer) {
				vi.flags |= VFlagImplicit
			}
		}
		// TODO: From cray pointer we now have special case where decl is non-nil but type is nil. Can we just consolidate both cases?
		// Assign type, preserving any existing ArraySpec from partial decl
		if vi.decl == nil {
			vi.decl = &ast.DeclEntity{Name: vi._varname, Type: implicitType}
		} else {
			// Partial decl exists (e.g., from Cray POINTER, DIMENSION statement) - just fill in Type
			vi.decl.Type = implicitType
		}
	}
}

var (
	_implicitInteger = &ast.DeclEntity{
		Name: "IMPLICIT INTEGER(default)",
		Type: &ast.TypeSpec{Token: token.INTEGER},
	}
	_implicitReal = &ast.DeclEntity{
		Name: "IMPLICIT REAL(default)",
		Type: &ast.TypeSpec{Token: token.REAL},
	}
)

func (pud *ParserUnitData) varInit(sp sourcePos, name string, decl *ast.DeclEntity, initFlags VarFlags, namespace string) (vi *Varinfo, err error) {
	if decl != nil && name != decl.Name {
		panic("bad varInit name argument mismatch with decl")
	}
	if name != "*" { // Alternate return slots are positional; allow duplicates.
		vi = pud.Var(name)
	}
	if vi != nil {
		if decl != nil && vi.decl != nil {
			// Both have decls - check if we can merge array specs
			if vi.decl.ArraySpec == nil && decl.ArraySpec != nil {
				// Existing decl has no array spec, new one does - merge it
				vi.decl.ArraySpec = decl.ArraySpec
			} else if vi.decl.ArraySpec != nil && decl.ArraySpec != nil {
				// Both have array specs - error (conflicting definitions)
				err = errors.New("double variable initialization with " + vi.declPos.String())
				return vi, err
			}
			// If existing has array spec and new doesn't, keep existing (no change needed)
		}
	} else {
		maybeIdentifier := token.LookupKeyword(unsafe.Slice(unsafe.StringData(name), len(name)))
		if maybeIdentifier != token.Identifier {
			pud.usesKeywordAsIdentifiers = append(pud.usesKeywordAsIdentifiers, maybeIdentifier)
		}
		pud.vars = slices.Grow(pud.vars, 1)
		pud.vars = pud.vars[:len(pud.vars)+1]
		vi = &pud.vars[len(pud.vars)-1]
		vi.reset()
	}
	// Set fields.
	vi.flags |= initFlags
	if vi.decl == nil {
		vi.decl = decl
		vi._varname = name
		vi.declPos = sp
	}
	if initFlags&VFlagCommon != 0 {
		vi.common = namespace
	} else if initFlags&VFlagPointer != 0 {
		vi.pointee = namespace
	} else if initFlags&VFlagReturned != 0 {
		pud.returnType = vi
	}

	return vi, nil
}

type Varinfo struct {
	decl     *ast.DeclEntity
	flags    VarFlags
	_varname string
	common   string // set to COMMON block name if found in COMMON statement
	pointee  string // the pointer alias this variable references in EQUIVALENCE or cray style POINTER statement.
	declPos  sourcePos
	val      Value // Runtime value for REPL evaluation
	kindFlag int   // 0 when uninitialized. -1 when unspecified, -2..-99 error. else is kind value.
	// Statement function fields (only set if VFlagStmtFunc)
	stmtFuncExpr   ast.Expression // RHS expression
	stmtFuncParams []string       // parameter names
}

// Varinfo methods should be called correctly.

func (p *Varinfo) Flags() VarFlags            { return p.flags }
func (p *Varinfo) Value() Value               { return p.val }
func (p *Varinfo) Charlen() ast.Expression    { return p.decl.Charlen() }
func (p *Varinfo) Kind() ast.Expression       { return p.decl.Kind() }
func (p *Varinfo) Dimensions() *ast.ArraySpec { return p.decl.Dimension() }
func (p *Varinfo) Identifier() string         { return p._varname }
func (p *Varinfo) IsParameter() bool          { return p.flags.HasAny(VFlagParameter) }
func (p *Varinfo) IsAllocatable() bool        { return p.flags.HasAny(VFlagAllocatable) }

func (p *Varinfo) IsStmtFunc() bool             { return p.flags.HasAny(VFlagStmtFunc) }
func (p *Varinfo) StmtFuncExpr() ast.Expression { return p.stmtFuncExpr }
func (p *Varinfo) StmtFuncParams() []string     { return p.stmtFuncParams }
func (p *Varinfo) CommonBlock() string          { return p.common }
func (p *Varinfo) DeclPos() (source string, line, col int) {
	return p.declPos.Source, p.declPos.Line, p.declPos.Col
}
func (p *Varinfo) TypeToken() token.Token {
	if p.val.tok != 0 {
		return p.val.tok
	} else if p.decl == nil || p.decl.Type == nil {
		return token.Undefined
	}
	return p.decl.Type.Token
}

// IsArray returns true if this is an array (has DIMENSION).
func (p *Varinfo) IsArray() bool {
	return p.flags.HasAny(VFlagDimension)
}

// IsChar returns true if this is a character type (not an array of characters).
func (p *Varinfo) IsChar() bool {
	if p.flags.HasAny(VFlagDimension) {
		return false
	}
	tok := p.TypeToken()
	return tok == token.CHARACTER || tok == token.StringLit
}

// IsCharArray returns true if this is an array of character type.
func (p *Varinfo) IsCharArray() bool {
	tok := p.TypeToken()
	return p.flags.HasAny(VFlagDimension) && (tok == token.CHARACTER || tok == token.StringLit)
}

// IsPointer returns true if accessing this variable requires automatic pointer dereferencing.
//
// Fortran pointer semantics:
//   - VFlagPointer (Cray pointer): In "POINTER (NPAA, AA(1))", NPAA holds an address.
//     Accessing NPAA returns the address VALUE, not the pointed-to data. Never auto-deref.
//   - VFlagPointee: AA in the above example. Accessing AA(i) implicitly dereferences NPAA
//     to reach the data. Pointees need auto-dereferencing.
//   - VFlagEquivalenced: Variables sharing storage via EQUIVALENCE. In Go, we use pointers
//     so they share memory, and accessing them requires dereferencing.
//
// Returns false for VFlagPointer because you want the address, not what it points to.
// Returns false for arrays (VFlagDimension) which have their own access patterns.
// Returns false for CHARACTER types which use intrinsic.CharacterArray.
//
// Note: VFlagIntentOut is handled separately via wrapPointer() in function call transpilation.
func (p *Varinfo) IsPointer() bool {
	if p.flags.HasAny(VFlagPointer | VFlagDimension) {
		return false
	}
	if p.TypeToken() == token.CHARACTER {
		return false
	}
	return p.flags.HasAny(VFlagEquivalenced | VFlagPointee)
}
func (p *Varinfo) reset()                             { *p = Varinfo{} }
func (p *Parser90) varResetAll()                      { p.vars.reset() }
func (p *Parser90) varGet(name []byte) (vi *Varinfo)  { return p.vars.Varb(name) }
func (p *Parser90) varSGet(name string) (vi *Varinfo) { return p.vars.Var(name) }
func (p *Parser90) varInit(name string, decl *ast.DeclEntity, initFlags VarFlags, namespace string) (vi *Varinfo) {
	vi, err := p.vars.varInit(p.sourcePos(), name, decl, initFlags, namespace)
	if err != nil {
		p.addError(err.Error())
	}
	return vi
}

func (p *Parser90) Reset(source string, r io.Reader) error {
	err := p.l.Reset(source, r)
	if err != nil {
		return err
	}
	if p.maxErrs == 0 {
		p.maxErrs = 10000 // Increased from 20 to handle large legacy codebases with many warnings
		p.maxStatements = 1_000_000
	}
	*p = Parser90{
		l: p.l,
		// Reuse memory but clear later.
		maxErrs:       p.maxErrs,
		maxStatements: p.maxStatements,
		errors:        p.errors[:0], // Reuse slice, clear contents
		vars:          p.vars,
	}
	p.vars.reset()

	// Initialize token stream
	p.nextToken()
	p.nextToken()
	p.nextToken()
	return nil
}

func (p *Parser90) nextToken() {
	if p.current.tok == token.EOF {
		return
	}
	if p.current.tok == token.EndParse {
		p.addErrorFatal("end parse token found", 0)
	}
	tok, start, lit := p.l.NextToken()
	// Get the line/col where this token started
	line, col := p.l.TokenLineCol()
	// Cycle buffers towards current. The latest peek will use current buffer.
	currBuf := p.current.lit // is clobbered by peek, reused by new peek.
	p.current = p.peek
	p.peek = p.uberpeek

	p.uberpeek.lit = append(currBuf[:0], lit...)
	p.uberpeek.start = start
	p.uberpeek.tok = tok
	p.uberpeek.line = line
	p.uberpeek.col = col
	if p.uberpeek.tok == token.Illegal {
		err := "illegal token"
		if p.l.Err() != nil {
			err = p.l.Err().Error()
		}
		p.addErrorWithPos(p.l.sourcePos(), err)
	}
}

// posCheck is called in control structure methods like loop*, currentTokenIs, consumeIf* methods.
// Should not be called from higher level parser functions.
func (p *Parser90) posCheck() {
	if debugNoStuckCheck == "1" {
		return
	}
	if p.current.start == p.lastPosCheck {
		p.nSamePosCheckCount++
		if p.nSamePosCheckCount == 100000 { // F 1000 is too low for this!
			p.addErrorFatal("parser stuck in forever loop", 3)
		}
	} else {
		p.lastPosCheck = p.current.start
		p.nSamePosCheckCount = 0
	}
}

func (p *Parser90) sourcePos() sourcePos {
	return sourcePos{
		Source: p.l.Source(),
		Line:   p.current.line,
		Col:    p.current.col,
		Pos:    p.current.start,
	}
}

func (p *Parser90) currentAstPos() ast.Position {
	return ast.Pos(p.current.start, p.current.start+len(p.current.lit))
}

// IsDone returns true if the parser is done parsing, whether it be by EOF or error(s) encountered.
func (p *Parser90) IsDone() bool {
	p.posCheck()
	errExceed := p.maxErrs > 0 && len(p.errors) >= p.maxErrs
	return p.died || p.current.tok == token.EOF || errExceed
}

type toktuple struct {
	tok   token.Token
	start int
	lit   []byte
	line  int // Line number where this token starts
	col   int // Column number where this token starts
}

func (tt toktuple) String() string {
	return fmt.Sprintf("%q %s", tt.lit, tt.tok.String())
}

// ParseNextProgramUnit parses and returns the next program unit from the input.
// Returns nil when EOF is reached or no more units are available.
// This method can be called repeatedly to incrementally parse a Fortran file.
// Phase 1: Parses only top-level program units (PROGRAM, SUBROUTINE, FUNCTION, MODULE)
func (p *Parser90) ParseNextProgramUnit() (unit ast.Unit) {
	puStart := p.sourcePos()
	panicked := true
	defer func() {
		if panicked {
			p.addErrorWithPos(puStart, "panicked in program unit after parsing "+strconv.Itoa(p.nStatements)+" statements")
			p.addError("panic position")
			fmt.Printf("%v\n%v\n", &p.errors[len(p.errors)-1], &p.errors[len(p.errors)-2])
		}
	}()

	for !p.IsDone() && !unit.IsValid() {
		// Skip leading newlines and comments
		// Parse one program unit
		puStart = p.sourcePos()
		unit = p.parseTopLevelUnit()
	}
	// If parsing succeeded, return the unit, nil or otherwise.
	panicked = false
	return unit
}

// registerTopLevelParsers registers all statement-level parsing functions

// parseTopLevelUnit dispatches to the appropriate registered statement parser
func (p *Parser90) parseTopLevelUnit() (unit ast.Unit) {
	p.skipNewlinesAndComments()
	if p.IsDone() || p.current.tok.IsEnd() {
		return unit
	}
	p.varResetAll()
	start := p.sourcePos()
	switch p.current.tok {
	case token.SUBROUTINE:
		unit = p.parseSubroutine()
	case token.FUNCTION:
		unit = p.parseFunction()
	case token.RECURSIVE, token.PURE, token.ELEMENTAL:
		unit = p.parseProcedureWithAttributes()
	case token.INTEGER, token.REAL, token.LOGICAL, token.CHARACTER,
		token.DOUBLEPRECISION, token.DOUBLE, token.COMPLEX, token.DOUBLECOMPLEX:
		unit = p.parseTypePrefixedUnit()
	case token.MODULE, token.PROGRAM:
		unit.Token = p.current.tok
		p.nextToken() // consume MODULE/PROGRAM token.
		// Parse program name (keywords can be used as program names)
		p.expectIdentifier(&unit.Name, "PROGRAM/MODULE name")
		p.skipNewlinesAndComments()
		unit.Body = p.parseBody(nil) // Parse body statements
		// CONTAINS processed after ParserUnitData generated to not mix up variables.
	case token.BLOCK:
		unit = p.parseBlockData()
	case token.INCLUDE:
		p.nextToken() // consume INCLUDE
		if p.currentTokenIs(token.StringLit) {
			p.nextToken() // consume filename
		}
		return unit // return empty unit; ParseNextProgramUnit loops to next
	default:
		p.addError("unexpected token at top level: " + p.current.String())
		p.nextToken()
		return unit
	}
	if !unit.IsValid() {
		p.addError("bad program unit for token: " + p.current.String())
		return unit
	}
	pud := p.makeUnitData(unit.Name, unit.Token)
	switch unit.Token {
	case token.FUNCTION:
		unit.Data = pud
		// Check for type-prefixed function (e.g., "INTEGER FUNCTION foo()")
		hasExplicitType := unit.ResultType.Token != 0
		missingResult := pud.returnType == nil
		if missingResult {
			// For bare FUNCTION without RESULT clause, the function name is the return variable.
			// Look for the variable with the function name and mark it as returned.
			vinfo := pud.Var(unit.Name)
			if vinfo != nil {
				vinfo.flags |= VFlagReturned
				// If type-prefixed, override the declaration type
				if hasExplicitType {
					if vinfo.decl != nil {
						vinfo.decl.Type = &unit.ResultType
					} else {
						vinfo.decl = &ast.DeclEntity{Name: unit.Name, Type: &unit.ResultType}
					}
				}
				pud.returnType = vinfo
			} else {
				// Function name not used in body - create return variable.
				var decl *ast.DeclEntity
				if hasExplicitType {
					decl = &ast.DeclEntity{Name: unit.Name, Type: &unit.ResultType}
				}
				// resolveImplicitTypes will assign implicit type if decl is nil.
				pud.returnType, _ = pud.varInit(start, unit.Name, decl, VFlagReturned, "")
			}
		}
	case token.MODULE, token.PROGRAM:
		if p.consumeIf(token.CONTAINS) {
			unit.Contains = p.parseAppendProgramUnits(unit.Contains[:0])
		}
		p.expectEndProgramUnit(unit.Token, unit.Token.EndConstructComposite(), start, unit.Name)
		unit.Position = ast.Pos(start.Pos, p.current.start)
	}
	pud.resolveImplicitTypes()
	unit.Data = pud
	return unit
}

func (p *Parser90) parseAppendProgramUnits(dst []ast.Unit) []ast.Unit {
	for p.loopUntil(token.END, token.ENDMODULE, token.ENDPROGRAM, token.ENDFUNCTION, token.ENDSUBROUTINE) {
		unit := p.parseTopLevelUnit()
		if unit.IsValid() {
			dst = append(dst, unit)
		} else {
			// parseTopLevelUnit returns nil when it encounters END tokens
			break
		}
	}
	return dst
}

// Semantic parsing functions for top-level constructs

// parseProgramBlock parses a PROGRAM...END PROGRAM block
// Precondition: current token is PROGRAM
func (p *Parser90) parseProgramBlock() (unit ast.Unit) {
	p.varResetAll()
	start := p.sourcePos()
	if !p.expect(token.PROGRAM, "") {
		return unit
	}
	unit.Token = token.PROGRAM
	// Parse program name (keywords can be used as program names)
	p.expectIdentifier(&unit.Name, "program name")

	p.skipNewlinesAndComments()

	// Parse body statements
	unit.Body = p.parseBody(nil)
	// Handle CONTAINS section (internal procedures)
	if p.consumeIf(token.CONTAINS) {
		unit.Contains = p.parseAppendProgramUnits(unit.Contains[:0])
	}
	p.expectEndProgramUnit(token.PROGRAM, token.ENDPROGRAM, start, unit.Name)
	unit.Position = ast.Pos(start.Pos, p.current.start)
	return unit
}

// parseModule parses a MODULE...END MODULE block
// Precondition: current token is MODULE
func (p *Parser90) parseModule() (unit ast.Unit) {
	start := p.sourcePos()
	if !p.expect(token.MODULE, "") {
		return unit
	}
	unit.Token = token.MODULE
	// Parse module name (keywords can be used as module names)
	p.expectIdentifier(&unit.Name, "module name")
	p.skipNewlinesAndComments()
	// Parse body statements
	unit.Body = p.parseBody(nil)
	// Handle CONTAINS section with recursive parsing
	if p.consumeIf(token.CONTAINS) {
		unit.Contains = p.parseAppendProgramUnits(unit.Contains[:0])
	}
	p.expectEndProgramUnit(token.MODULE, token.ENDMODULE, start, unit.Name)
	unit.Position = ast.Pos(start.Pos, p.current.start)
	return unit
}

// parseSubroutine parses a SUBROUTINE...END SUBROUTINE block
// Precondition: current token is SUBROUTINE
func (p *Parser90) parseSubroutine() (unit ast.Unit) {
	p.varResetAll()
	start := p.sourcePos()
	unit.Token = token.SUBROUTINE

	p.expect(token.SUBROUTINE, "")

	// Parse subroutine name (keywords can be used as subroutine names)
	p.expectIdentifier(&unit.Name, "subroutine name")

	// Parse parameter list if present
	if p.currentTokenIs(token.LParen) {
		unit.Parameters = p.parseParameterList()
	}

	p.skipNewlinesAndComments()

	// Parse body statements
	unit.Body = p.parseBody(unit.Parameters)

	p.expectEndProgramUnit(token.SUBROUTINE, token.ENDSUBROUTINE, start, unit.Name)
	unit.Position = ast.Pos(start.Pos, p.current.start)
	return unit
}

// parseTypePrefixedUnit handles type-prefixed functions like "INTEGER FUNCTION foo()"
func (p *Parser90) parseTypePrefixedUnit() (unit ast.Unit) {
	ts := p.parseTypeSpecIntrinsic() // Save the type specification to set result type.
	unit = p.parseFunction()
	if !unit.IsValid() {
		return unit
	}
	unit.ResultType = ts
	return unit
}

// parseFunction parses a FUNCTION...END FUNCTION block
// Precondition: current token is FUNCTION
func (p *Parser90) parseFunction() (unit ast.Unit) {
	p.varResetAll()
	start := p.sourcePos()
	if !p.expect(token.FUNCTION, "") {
		return unit
	}
	unit.Token = token.FUNCTION
	// Parse function name (can be Identifier, FormatSpec, or keyword used as identifier)
	p.expectIdentifier(&unit.Name, "function name")
	// Parse parameter list
	if p.currentTokenIs(token.LParen) {
		unit.Parameters = p.parseParameterList()
	}

	// Check for RESULT clause
	if p.consumeIf(token.RESULT) {
		if p.expect(token.LParen, "RESULT open") {
			var resultVarName string
			if p.expectIdentifier(&resultVarName, "function RESULT variable specification") {
				p.varInit(resultVarName, nil, VFlagReturned, "")
			}
			p.expect(token.RParen, "RESULT close")
		}
	}

	// Parse body statements
	unit.Body = p.parseBody(unit.Parameters)
	p.expectEndProgramUnit(token.FUNCTION, token.ENDFUNCTION, start, unit.Name)
	unit.Position = ast.Pos(start.Pos, p.current.start)
	return unit
}

// parseBlockData parses a BLOCK DATA...END [BLOCK DATA] block
func (p *Parser90) parseBlockData() (unit ast.Unit) {
	start := p.sourcePos()
	// Consume BLOCK identifier
	if !p.consumeIf2(token.BLOCK, token.DATA) {
		p.addError("DATA BLOCK expected: " + p.current.String())
		return unit
	}
	unit.Token = token.BLOCK
	// Parse optional block data name
	p.consumeIdentifier(&unit.Name)
	p.skipNewlinesAndComments()
	// Parse body statements
	unit.Body = p.parseBody(nil)
	p.expectEndProgramUnit(token.BLOCK, 0, start, unit.Name)
	unit.Position = ast.Pos(start.Pos, p.current.start)
	return unit
}

// parseProcedureWithAttributes handles procedures with attributes like RECURSIVE, PURE, ELEMENTAL
func (p *Parser90) parseProcedureWithAttributes() (unit ast.Unit) {
	// Collect all attributes
	attributes := []token.Token{}
	for p.current.tok.IsAttributeKeyword() && !p.IsDone() {
		attributes = append(attributes, p.current.tok)
		p.nextToken()
	}
	if p.current.tok.IsTypeDeclaration() {
		// Is a function.
		return p.parseTypePrefixedUnit()
	}
	// Now must be SUBROUTINE or FUNCTION
	switch p.current.tok {
	case token.SUBROUTINE:
		unit = p.parseSubroutine()
	case token.FUNCTION:
		unit = p.parseFunction()
	default:
		p.addError("want FUNCTION|SUBROUTINE, got " + p.current.String())
		return unit
	}
	if unit.IsValid() {
		unit.ResultType.Attributes = toTypeAttributes(attributes)
	}
	return unit
}

// Helper methods

func (p *Parser90) loopUntilEndElseOr(t ...token.Token) bool {
	p.posCheck()
	return !p.current.tok.IsEndOrElse() && p.loopUntil(t...)
}

// loopUntil returns true as long as current token not in set and EOF not hit.
func (p *Parser90) loopUntil(t ...token.Token) bool {
	if p.IsDone() {
		return false
	}
	for i := range t {
		if t[i] == p.current.tok {
			return false
		}
	}
	return true
}

func (p *Parser90) loopWhile(t ...token.Token) bool {
	if p.IsDone() {
		return false
	}
	for i := range t {
		if t[i] == p.current.tok {
			return true
		}
	}
	return false
}

func (p *Parser90) currentTokenIs(t token.Token) bool {
	p.posCheck()
	return p.current.tok == t
}

func (p *Parser90) peekTokenIs(t token.Token) bool {
	p.posCheck()
	return p.peek.tok == t
}

func (p *Parser90) expectCurrent(t token.Token) bool {
	if !p.currentTokenIs(t) {
		p.addError("expected " + t.String() + ", got " + p.current.String())
		return false
	}
	return true
}

// expect checks if current token matches t, consumes it if so, and reports error if not.
// Returns true if token matched and was consumed, false otherwise.
func (p *Parser90) expect(t token.Token, reason string) bool {
	if !p.currentTokenIs(t) {
		p.addError(reason + ": " + "expected " + t.String() + ", got " + p.current.String())
		return false
	}
	p.nextToken()
	return true
}

// consumeIf consumes the current token if it matches t, otherwise does nothing.
// Returns true if token was consumed, false otherwise.
// Use this for optional tokens where absence is not an error.
func (p *Parser90) consumeIf(t token.Token) bool {
	if p.currentTokenIs(t) {
		p.nextToken()
		return true
	}

	return false
}

// consumeIf2 is same as consumeIf but must match current and peek token to consume at least 2 tokens, else 0 tokens consumed.
func (p *Parser90) consumeIf2(current, next token.Token) bool {
	if p.currentTokenIs(current) && p.peekTokenIs(next) {
		p.nextToken()
		p.nextToken()
		return true
	}
	return false
}

// looksLikeImpliedDoLoop checks if the current position (at comma) looks like an implied DO loop
// rather than a complex literal. Called when parsing (expr, ...) - current token is comma.
// Complex literal: (real, imag) - peek is literal/sign, then followed by )
// Implied DO loop: (expr, ..., var = start, end) - will have identifier = pattern
func (p *Parser90) looksLikeImpliedDoLoop() bool {
	// If peek is a literal followed by ), it's definitely a complex literal
	if p.peekTokenIs(token.IntLit) || p.peekTokenIs(token.FloatLit) {
		if p.uberpeek.tok == token.RParen {
			return false // It's (expr, literal) - complex literal
		}
	}
	// Handle signed imaginary part: (expr, -literal) or (expr, +literal)
	if p.peekTokenIs(token.Minus) || p.peekTokenIs(token.Plus) {
		if p.uberpeek.tok == token.IntLit || p.uberpeek.tok == token.FloatLit {
			return false // It's (expr, ±literal) - complex literal
		}
	}
	return true // Likely an implied DO loop
}

// expect2IfFirst checks if current token is present, if present also expects a next token to be present, else makes no checks.
func (p *Parser90) expect2IfFirst(current, next token.Token, reason string) {
	if p.consumeIf(current) {
		p.expect(next, reason)
	}
}

// expectEndConstruct handles END <keyword> for control flow constructs (IF, DO).
// The keyword is REQUIRED. If END is found without the keyword, reports error
// and does NOT consume END (it likely belongs to parent construct).
// Accepts both F77 style (ENDIF, ENDDO) and F90 style (END IF, END DO) forms.
// Returns true if END <keyword> was successfully consumed.
func (p *Parser90) expectEndConstruct(keyword, singleEndForm token.Token, start sourcePos) bool {
	// Check for F77 single-token form (e.g., ENDIF, ENDDO) or F90 two-token form (e.g., END IF, END DO)
	if p.consumeIf(singleEndForm) || p.consumeIf2(token.END, keyword) {
		return true
	}
	if p.currentTokenIs(token.END) {
		// END without expected keyword - belongs to parent
		p.addErrorMismatchedEnd(start, keyword, "")
		return false
	} else {
		p.addError("expected END " + keyword.String())
		return false
	}
}

func (p *Parser90) expectEndProgramUnit(keyword, singleEndForm token.Token, start sourcePos, name string) bool {
	// Check for F77 single-token form e.g: ENDPROGRAM or F90 two-token form e.g: END PROGRAM
	if p.consumeIf(singleEndForm) || p.consumeIf2(token.END, keyword) {
		if keyword == token.BLOCK {
			p.consumeIf(token.DATA)
		}
		// Consume optional program unit name (can be keyword used as identifier, e.g. "END SUBROUTINE Print")
		if p.canUseAsIdentifier() {
			p.nextToken()
		}
		return true
	} else if p.consumeIf(token.END) {
		// Program units do not need keyword specifier, can be single END form.
		return true
	}
	p.addErrorMismatchedEnd(start, keyword, name)
	return false
}

func (p *Parser90) addErrorMismatchedEnd(start sourcePos, keyword token.Token, label string) {
	p.addErrorWithPos(start, keyword.String()+" missing END with keyword "+label)
	p.addError("expected 'END " + keyword.String() + "'; got END without keyword (may belong to enclosing program unit or construct) " + label)
}

func (p *Parser90) skipUnexpectedEndConstructs(msg string) (skipped bool) {
	for {
		n := token.IsEndConstruct(p.current.tok, p.peek.tok, p.uberpeek.tok)
		switch n {
		case 0:
			return skipped
		case 1:
			msg += ": unexpected end construct " + p.current.tok.String()
			p.nextToken()
		case 2:
			msg += ": unexpected end construct " + p.current.tok.String() + " " + p.peek.tok.String()
			p.nextToken()
			p.nextToken()
		case 3:
			msg += ": unexpected end construct " + string(p.current.lit) + " " + p.peek.tok.String() + " " + p.uberpeek.tok.String()
			p.nextToken()
			p.nextToken()
			p.nextToken()
		}
		skipped = true
		p.addErrorFatal(msg, 1)
		p.skipNewlinesAndComments()
	}
}

func (p *Parser90) skipNewlinesAndComments() {
	for p.loopWhile(token.NewLine, token.LineComment) {
		p.nextToken()
	}
}

func (p *Parser90) Errors() []ParserError {
	return p.errors
}

func (p *Parser90) consumeIdentifier(dst *string, allowTokens ...token.Token) bool {
	if p.canUseAsIdentifier() || slices.Contains(allowTokens, p.current.tok) {
		*dst = p.currentLiteral()
		p.nextToken()
		return true
	}
	return false
}

func (p *Parser90) expectIdentifier(dst *string, reason string, allowTokens ...token.Token) bool {
	consumed := p.consumeIdentifier(dst, allowTokens...)
	if !consumed {
		p.addError("expected identifier got " + p.current.String() + ": " + reason)
	}
	return consumed
}

// canUseAsIdentifier returns true if the current token can be used as an identifier.
// In Fortran, keywords can be used as variable/function/subroutine names in many contexts.
func (p *Parser90) canUseAsIdentifier() bool {
	return p.current.tok.CanBeUsedAsIdentifier()
}

func (p *Parser90) currentLiteral() string {
	if len(p.current.lit) > 0 {
		return string(p.current.lit)
	}
	return p.current.tok.String()
}

func (p *Parser90) currentAsVarString(bitset VarFlags, common string) string {
	v := p.varGet(p.current.lit)
	if v == nil {
		// Variable not declared - check if implicit typing is allowed.
		lit := p.currentLiteral()
		if p.vars.isImplicitNone() {
			if !p.ignoreUndeclaredVars {
				p.addError("variable " + lit + " undefined")
			}
			return lit
		}
		// Create implicitly typed variable with type resolved immediately.
		implicitDecl := p.vars.implicitDeclFor(lit)
		v = p.varInit(lit, implicitDecl, VFlagImplicit, "")
	}
	if bitset&VFlagCommon != 0 {
		v.common = common
	}
	v.flags |= bitset
	return v._varname
}

// parseParameterList parses a parameter list like (a, b, c)
// In Fortran, keywords can be used as variable names
func (p *Parser90) parseParameterList() []ast.Parameter {
	params := []ast.Parameter{}

	if !p.expectCurrent(token.LParen) {
		return params
	}
	p.nextToken()

	// Empty parameter list
	if p.currentTokenIs(token.RParen) {
		p.nextToken()
		return params
	}

	// Parse parameters using generic helper
	parseOneParam := func() (ast.Parameter, error) {
		// Accept identifiers, keywords, or *, even DATA (alternate return specifier in F77)
		var paramName string
		if !p.expectIdentifier(&paramName, "parameter name", token.DATA, token.Asterisk) {
			return ast.Parameter{}, errors.New("bad parameter")
		}
		// Create Parameter with just the name for now
		// Type information will be filled in by parseBody when it sees type declarations
		return ast.Parameter{Name: paramName}, nil
	}

	// Parse comma-separated parameters manually to avoid loopUntilEndElseOr
	// which would stop on END token (which can be a valid parameter name)
	for p.loopUntil(token.RParen) {
		param, err := parseOneParam()
		if err != nil {
			p.addError(err.Error())
			break
		}
		p.varInit(param.Name, nil, VFlagParameter, "") // parameters have no type.
		params = append(params, param)
		if p.currentTokenIs(token.Comma) {
			p.nextToken()
		} else if !p.currentTokenIs(token.RParen) {
			p.addError("expected ',' or ')' in parameter list")
			break
		}
	}

	p.expect(token.RParen, "closing parameter list")

	return params
}

// parseBody parses specification statements, then executable statements
// If parameters are provided, it will populate their type information when type declarations are found
func (p *Parser90) parseBody(params []ast.Parameter) []ast.Statement {
	defer p.vars.resolveParameterTypes(params)
	var stmts []ast.Statement
	inExec := false
	p.vars.implicits = p.vars.implicits[:0] // TODO: shouldn't anytime vars is reset implicits also be reset? This seems like an edge case.
	for {
		if p.skipUnexpectedEndConstructs("at body parsing spec statements") {
			return stmts
		}
		p.skipNewlinesAndComments()
		// Check loop condition AFTER skipping newlines so we see CONTAINS/END tokens.
		if !p.loopUntil(token.CONTAINS) || p.isEndOfProgramUnit() {
			break
		}
		stmt := p.parseStatement(inExec)
		if stmt == nil {
			// p.addError("nil statement, skipping")
			p.skipToNextStatement() // Should never happen, but likely a bug.
			continue
		}
		stmts = append(stmts, stmt)
		p.nStatements++
		if !inExec {
			inExec = stmt.IsExecutable()
			if inExec {
				// p.vars.resolveImplicitTypes() // Resolve implicit types once on spec statement ends.
			}
		}
	}
	return stmts
}

func (p *Parser90) parseExecutableStatement() ast.Statement { return p.parseStatement(true) }

func (p *Parser90) parseStatement(inExec bool) ast.Statement {
	var label, constructLabel string
	var stmt ast.Statement
	if p.currentTokenIs(token.IntLit) {
		if p.peek.tok.IsEnd() {
			return nil // Is a Label to an END, should be parsed in parent
		}
		label = string(p.current.lit)
		p.nextToken()
	} else if p.currentTokenIs(token.Identifier) && p.peekTokenIs(token.Colon) && p.uberpeek.tok.IsConstruct() {
		constructLabel = string(p.current.lit)
		p.nextToken()
		p.nextToken()
	} else if p.isLikelyAssignment(inExec) {
		stmt = p.parseAssignmentStmt()
		if label != "" && stmt != nil {
			*stmt.GetLabel() = label
		}
		return stmt
	}
	switch p.current.tok {
	// SPECIFICATION STATEMENTS:
	case token.IMPLICIT:
		stmt = p.parseImplicit()
	case token.USE:
		stmt = p.parseUse()
	case token.FORMAT:
		stmt = p.parseFormatStmt()
	case token.INTEGER, token.REAL, token.DOUBLE, token.DOUBLEPRECISION, token.DOUBLECOMPLEX, token.COMPLEX, token.LOGICAL, token.CHARACTER:
		stmt = p.parseTypeDecl()
	case token.TYPE:
		// Distinguish between TYPE definition and TYPE(typename) declaration
		if p.peekTokenIs(token.LParen) {
			// TYPE(typename) :: var - treat as type declaration
			return p.parseTypeDecl()
		} else {
			// TYPE :: name ... END TYPE - parse derived type definition
			return p.parseDerivedTypeStmt()
		}
	case token.INTERFACE:
		// INTERFACE block - skip entire block
		p.skipConstruct(token.INTERFACE, token.ENDINTERFACE)
		return &ast.InterfaceStmt{} // Return non-nil to indicate success
	case token.DATA:
		// DATA statement - skip to end of statement (complex to parse fully)
		stmt = p.parseDataStmt()
	case token.COMMON:
		stmt = p.parseCommonStmt()
	case token.DIMENSION:
		stmt = p.parseDimensionStmt()
	case token.EQUIVALENCE:
		stmt = p.parseEquivalenceStmt()
	case token.POINTER:
		stmt = p.parsePointerCrayStmt()
	case token.EXTERNAL:
		stmt = p.parseExternalStmt()
	case token.INTRINSIC:
		stmt = p.parseIntrinsicStmt()
	case token.NAMELIST:
		stmt = p.parseNamelistStmt()
	case token.PARAMETER:
		stmt = p.parseParameterStmt()

	// EXECUTABLE STATEMENTS
	case token.END:
		if p.peek.tok != token.FILE {
			return nil // END construct? let parent handle it.
		}
		fallthrough
	case token.ENDFILE:
		stmt = p.parseEndfileStmt()
	case token.GOTO, token.GO:
		stmt = p.parseGotoStmt()
	case token.IF:
		stmt = p.parseIfStmt()
	case token.DO:
		stmt = p.parseDoLoop()
	case token.SELECT:
		stmt = p.parseSelectCaseStmt()
	case token.CALL:
		stmt = p.parseCallStmt()
	case token.ENTRY:
		stmt = p.parseEntryStmt()
	case token.RETURN:
		stmt = p.parseReturnStmt()
	case token.CYCLE:
		stmt = p.parseCycleStmt()
	case token.EXIT:
		stmt = p.parseExitStmt()
	case token.CONTINUE:
		stmt = p.parseContinueStmt()
	case token.ASSIGN:
		stmt = p.parseAssignStmt()
	case token.READ, token.WRITE, token.INQUIRE:
		stmt = p.parseIOStmt()
	case token.PRINT:
		stmt = p.parsePrintStmt()
	case token.OPEN:
		stmt = p.parseOpenStmt()
	case token.CLOSE:
		stmt = p.parseCloseStmt()
	case token.BACKSPACE:
		stmt = p.parseBackspaceStmt()
	case token.REWIND:
		stmt = p.parseRewindStmt()

	case token.STOP:
		stmt = p.parseStopStmt()
	case token.ALLOCATE, token.DEALLOCATE:
		stmt = p.parseAllocateOrDeallocateStmt()

	case token.INCLUDE:
		p.nextToken() // consume INCLUDE
		if p.currentTokenIs(token.StringLit) {
			p.nextToken() // consume filename
		}
		return nil // not represented in AST

	case token.Identifier, token.FormatSpec: // TODO: don't generate FormatSpec tokens in lexer- interpret them exclusively in parseIOStmt
		stmt = p.parseAssignmentStmt()
	}
	if stmt != nil {
		if label != "" {
			ptrToLabel := stmt.GetLabel()
			if ptrToLabel == nil {
				p.addError(fmt.Sprintf("statement %T does not support labelling", stmt))
			} else {
				*ptrToLabel = label
			}
		}
		if constructLabel != "" {
			switch s := stmt.(type) {
			default:
				p.addError(fmt.Sprintf("statement %T does not support construct labels", stmt))
			case *ast.IfStmt:
				s.ConstructLabel = constructLabel
			case *ast.DoLoop:
				s.ConstructLabel = constructLabel
			case *ast.SelectCaseStmt:
				s.ConstructLabel = constructLabel
			}
			if string(p.current.lit) == constructLabel {
				p.nextToken() // Consume ending construct label.
			}
		}
	}
	return stmt
}

// isLikelyAssignment checks if current token starts an assignment statement.
// It handles both simple assignments (RESULT=1) and array assignments (RESULT(N)=1).
// This requires lookahead beyond the standard 3-token window for cases like KEYWORD(...=.
//
// Postcondition: Does not consume any tokens; parser state unchanged.
func (p *Parser90) isLikelyAssignment(inExec bool) bool {
	if inExec && len(p.vars.usesKeywordAsIdentifiers) > 0 {
		if (p.peek.tok == token.Equals || p.peek.tok == token.PointerAssign ||
			(p.peek.tok == token.LParen && p.current.tok != token.IF)) &&
			slices.Contains(p.vars.usesKeywordAsIdentifiers, p.current.tok) {
			return true
		}
	}
	return token.IsAssignment(p.current.tok, p.peek.tok)
}

// parseContinueStmt parses a CONTINUE statement
// Precondition: current token is CONTINUE
func (p *Parser90) parseContinueStmt() ast.Statement {
	start := p.current.start
	p.expect(token.CONTINUE, "")
	return &ast.ContinueStmt{
		Position: ast.Pos(start, p.current.start),
	}
}

// parseGotoStmt parses a GOTO or GO TO statement
// Handles both GOTO (single token) and GO (identifier) followed by TO
func (p *Parser90) parseGotoStmt() ast.Statement {
	start := p.current.start
	ok := p.consumeIf2(token.GO, token.TO) || p.expect(token.GOTO, "")
	if !ok {
		return nil
	}
	// Check for computed GOTO: GOTO (label-list) expression
	if p.currentTokenIs(token.LParen) {
		computedStmt := &ast.ComputedGotoStmt{}
		p.nextToken() // consume (

		// Parse label list using parseCommaSeparatedList
		parseOneLabel := func() (string, error) {
			if !p.currentTokenIs(token.IntLit) {
				return "", fmt.Errorf("expected label in computed GOTO label list")
			}
			label := string(p.current.lit)
			p.nextToken() // consume label
			return label, nil
		}

		labels, err := parseCommaSeparatedList(p, token.RParen, parseOneLabel)
		if err != nil {
			p.addError(err.Error())
			return nil
		}
		computedStmt.Labels = labels

		if !p.expect(token.RParen, "closing computed GOTO label list") {
			return nil
		}

		// Optional comma before the index expression (both forms are valid in Fortran)
		p.consumeIf(token.Comma)

		// Parse the index expression
		computedStmt.Expression = p.parseExpression(0)
		if computedStmt.Expression == nil {
			p.addError("expected expression after computed GOTO label list")
			return nil
		}

		computedStmt.Position = ast.Pos(start, p.current.start)
		return computedStmt
	}

	// Check for assigned GOTO: GO TO variable [, (label-list)]
	if p.currentTokenIs(token.Identifier) {
		assignedStmt := &ast.AssignedGotoStmt{
			Variable: string(p.current.lit),
		}
		p.nextToken() // consume variable name

		// Check for optional comma and label list
		if p.consumeIf(token.Comma) {
			if !p.expect(token.LParen, "expected '(' after comma in assigned GOTO") {
				return nil
			}

			// Parse label list
			parseOneLabel := func() (string, error) {
				if !p.currentTokenIs(token.IntLit) {
					return "", fmt.Errorf("expected label in assigned GOTO label list")
				}
				label := string(p.current.lit)
				p.nextToken() // consume label
				return label, nil
			}

			labels, err := parseCommaSeparatedList(p, token.RParen, parseOneLabel)
			if err != nil {
				p.addError(err.Error())
				return nil
			}
			assignedStmt.Labels = labels

			if !p.expect(token.RParen, "closing assigned GOTO label list") {
				return nil
			}
		}

		assignedStmt.Position = ast.Pos(start, p.current.start)
		return assignedStmt
	}

	// Simple GOTO: GOTO label
	if p.currentTokenIs(token.IntLit) {
		stmt := &ast.GotoStmt{
			Target: string(p.current.lit),
		}
		p.nextToken()
		stmt.Position = ast.Pos(start, p.current.start)
		return stmt
	}

	p.addError("expected label or variable after GO TO")
	return nil
}

// parseAssignStmt parses an ASSIGN statement (Fortran 77 feature)
// Syntax: ASSIGN <label> TO <variable>
// Example: ASSIGN 100 TO jump_target
func (p *Parser90) parseAssignStmt() ast.Statement {
	start := p.current.start
	p.expect(token.ASSIGN, "")

	stmt := &ast.AssignStmt{}

	// Parse the label
	if !p.currentTokenIs(token.IntLit) {
		p.addError("expected label after ASSIGN")
		return nil
	}
	stmt.LabelValue = string(p.current.lit)
	p.nextToken()
	p.expect(token.TO, "after ASSIGN label")

	// Parse the variable
	if !p.currentTokenIs(token.Identifier) {
		p.addError("expected variable after ASSIGN <label> TO")
		return nil
	}
	stmt.Variable = string(p.current.lit)
	p.nextToken()

	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseIOStmt parses READ, WRITE, or INQUIRE statements
// Format: READ/WRITE/INQUIRE(ctrl_specs) [io_list]
func (p *Parser90) parseIOStmt() ast.Statement {
	start := p.current.start
	var isRead, isInquire bool
	if p.consumeIf(token.READ) {
		isRead = true
	} else if p.consumeIf(token.INQUIRE) {
		isInquire = true
	} else if !p.consumeIf(token.WRITE) {
		p.addError("expected READ/WRITE/INQUIRE")
	}

	if !p.expect(token.LParen, "in I/O statement") {
		return nil
	}

	// Parse control spec list: just expressions/assignments until RParen
	// Track paren depth so END keyword doesn't confuse us
	var specs []ast.Expression
	parenDepth := 1

	for parenDepth > 0 && !p.IsDone() {
		if p.consumeIf(token.RParen) {
			parenDepth--
			if parenDepth == 0 {
				break
			}
			continue
		}

		if p.consumeIf(token.LParen) {
			parenDepth++
			continue
		}

		// Parse one spec (expression, keyword=value, etc.)
		// I/O specs can be: unit, format, KEYWORD=value
		var spec ast.Expression

		// Special case: * for list-directed I/O (unit or format)
		if p.currentTokenIs(token.Asterisk) {
			spec = &ast.Identifier{
				Value:    "*",
				Position: ast.Pos(p.current.start, p.current.start),
			}
			p.nextToken()
		} else if p.currentTokenIs(token.END) {
			// Special case: END is a keyword but can be used as identifier in I/O specs
			spec = &ast.Identifier{
				Value:    "END",
				Position: ast.Pos(p.current.start, p.current.start+len(p.current.lit)),
			}
			p.nextToken()
			// Check for assignment (END=500)
			if p.consumeIf(token.Equals) {
				value := p.parseExpression(0)
				if value != nil {
					spec = &ast.BinaryExpr{
						Left:     spec,
						Op:       token.Equals,
						Right:    value,
						Position: ast.Pos(spec.SourcePos().Start(), value.SourcePos().End()),
					}
				}
			}
		} else {
			// Parse a spec: could be expr or KEYWORD=value
			spec = p.parseExpression(0)
			// Check if this is a keyword=value pattern
			if spec != nil && p.consumeIf(token.Equals) {
				// This is keyword=value
				value := p.parseExpression(0)
				if value != nil {
					spec = &ast.BinaryExpr{
						Left:     spec,
						Op:       token.Equals,
						Right:    value,
						Position: ast.Pos(spec.SourcePos().Start(), value.SourcePos().End()),
					}
				}
			}
		}

		if spec != nil {
			specs = append(specs, spec)
		} else {
			// If expression parsing fails, break to avoid infinite loop
			break
		}

		// Loop continuation logic for comma-separated I/O control specs:
		// - If comma found: consume it and continue to next spec
		// - If no comma but at RParen: continue (RParen will be handled at top of loop)
		// - If no comma and not at RParen: malformed spec list, break
		// This handles both trailing comma cases: "READ(14,)" and "READ(14)"
		if !p.consumeIf(token.Comma) && !p.currentTokenIs(token.RParen) {
			break
		}
	}

	// Parse I/O list (comma-separated expressions)
	// Note: Use loopUntil instead of loopUntilEndElseOr because END can be a variable name
	// in the output list (e.g., WRITE(1,100) s,END,t)
	var ioList []ast.Expression
	for p.loopUntil(token.NewLine) {
		if expr := p.parseExpression(0); expr != nil {
			ioList = append(ioList, expr)
		}
		if !p.consumeIf(token.Comma) {
			break
		}
	}
	if isRead {
		for _, expr := range ioList {
			p.registerImplicitFromTarget(expr)
		}
	}

	// Build appropriate statement type
	pos := ast.Pos(start, p.current.start)

	if isInquire {
		// For INQUIRE, convert specs to []IOSpecifier
		var specList []ast.IOSpecifier
		isFirstSpec := true
		for _, spec := range specs {
			if binExpr, ok := spec.(*ast.BinaryExpr); ok && binExpr.Op == token.Equals {
				// keyword=value form
				if ident, ok := binExpr.Left.(*ast.Identifier); ok {
					key := strings.ToUpper(ident.Value)
					specList = append(specList, ast.IOSpecifier{
						Name:  key,
						Value: binExpr.Right,
					})
					p.registerIOOutputSpecVar(key, binExpr.Right)
				}
			} else if isFirstSpec {
				// First positional argument is UNIT
				specList = append(specList, ast.IOSpecifier{Name: "UNIT", Value: spec})
			}
			isFirstSpec = false
		}
		return &ast.InquireStmt{
			Specifiers: specList,
			OutputList: ioList,
			Position:   pos,
		}
	}

	// Extract unit, format, and specifiers from specs for READ/WRITE
	var unit, format ast.Expression
	var specList []ast.IOSpecifier
	posIdx := 0 // track positional argument position

	for _, spec := range specs {
		if binExpr, ok := spec.(*ast.BinaryExpr); ok && binExpr.Op == token.Equals {
			// keyword=value form
			if ident, ok := binExpr.Left.(*ast.Identifier); ok {
				key := strings.ToUpper(ident.Value)
				switch key {
				case "UNIT":
					unit = binExpr.Right
				case "FMT":
					format = binExpr.Right
				default:
					specList = append(specList, ast.IOSpecifier{Name: key, Value: binExpr.Right})
					p.registerIOOutputSpecVar(key, binExpr.Right)
				}
			}
		} else {
			// Positional argument: first is unit, second is format
			if posIdx == 0 {
				unit = spec
			} else if posIdx == 1 {
				format = spec
			}
			posIdx++
		}
	}

	if isRead {
		return &ast.ReadStmt{
			Unit:       unit,
			Format:     format,
			Specifiers: specList,
			InputList:  ioList,
			Position:   pos,
		}
	} else {
		return &ast.WriteStmt{
			Unit:       unit,
			Format:     format,
			Specifiers: specList,
			OutputList: ioList,
			Position:   pos,
		}
	}
}

// parsePrintStmt parses a PRINT statement
// Precondition: current token is PRINT
// Syntax: PRINT format [, output-item-list]
// Postcondition: current token is the first token after the PRINT statement
func (p *Parser90) parsePrintStmt() ast.Statement {
	start := p.current.start
	p.expect(token.PRINT, "")

	// Parse format (required): *, integer label, or character expression
	var format ast.Expression

	// Special handling for * (list-directed format)
	if p.currentTokenIs(token.Asterisk) {
		// Create an identifier node for the * format
		format = &ast.Identifier{
			Value:    "*",
			Position: ast.Pos(p.current.start, p.current.start),
		}
		p.nextToken() // consume *
	} else {
		format = p.parseExpression(0)
		if format == nil {
			p.addError("expected format specifier in PRINT statement")
			return nil
		}
	}

	stmt := &ast.PrintStmt{
		Format:   format,
		Position: ast.Pos(start, p.current.start),
	}

	// Parse optional output list (comma-separated expressions)
	if p.consumeIf(token.Comma) {
		for !p.currentTokenIs(token.NewLine) && !p.IsDone() && !p.current.tok.IsEnd() {
			if expr := p.parseExpression(0); expr != nil {
				stmt.OutputList = append(stmt.OutputList, expr)
			} else {
				break
			}

			if !p.consumeIf(token.Comma) {
				break
			}
		}
	}

	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseOpenStmt parses an OPEN statement
// Precondition: current token is OPEN
// Syntax: OPEN([UNIT=]u [,specifier-list])
// Postcondition: current token is the first token after the OPEN statement
func (p *Parser90) parseOpenStmt() ast.Statement {
	start := p.current.start
	p.expect(token.OPEN, "")

	if !p.expect(token.LParen, "in OPEN statement") {
		return nil
	}

	var specList []ast.IOSpecifier

	// Track if we've seen the first positional argument (which would be UNIT)
	isFirstArg := true

	// Parse comma-separated specifier list
	for !p.currentTokenIs(token.RParen) && !p.IsDone() && !p.current.tok.IsEnd() {
		// Parse one specifier: either expression or keyword=value
		spec := p.parseExpression(0)
		if spec == nil {
			break
		}

		// Check if this is a keyword=value pattern
		if p.currentTokenIs(token.Equals) {
			// Extract keyword name
			var keyword string
			if ident, ok := spec.(*ast.Identifier); ok {
				keyword = strings.ToUpper(ident.Value)
			} else {
				p.addError("expected identifier before = in OPEN specifier")
				if !p.consumeIf(token.Comma) {
					break
				}
				continue
			}

			p.nextToken() // consume =
			value := p.parseExpression(0)
			if value != nil {
				specList = append(specList, ast.IOSpecifier{Name: keyword, Value: value})
				p.registerIOOutputSpecVar(keyword, value)
			}
			isFirstArg = false
		} else if isFirstArg {
			// First positional argument is UNIT
			specList = append(specList, ast.IOSpecifier{Name: "UNIT", Value: spec})
			isFirstArg = false
		} else {
			p.addError("unexpected expression in OPEN statement (expected keyword=value)")
		}

		// Consume comma if present, otherwise should be at RParen
		if !p.consumeIf(token.Comma) {
			break
		}
	}

	// Consume the closing parenthesis
	if !p.expect(token.RParen, "closing OPEN statement") {
		return nil
	}

	return &ast.OpenStmt{
		Specifiers: specList,
		Position:   ast.Pos(start, p.current.start),
	}
}

// parseCloseStmt parses a CLOSE statement
// Precondition: current token is CLOSE
// Syntax: CLOSE([UNIT=]u [,specifier-list]) or CLOSE unit
// Postcondition: current token is the first token after the CLOSE statement
func (p *Parser90) parseCloseStmt() ast.Statement {
	start := p.current.start
	p.expect(token.CLOSE, "")

	var specList []ast.IOSpecifier

	// Handle simple form: CLOSE unit (without parentheses)
	if !p.currentTokenIs(token.LParen) {
		unit := p.parseExpression(0)
		if unit != nil {
			specList = append(specList, ast.IOSpecifier{Name: "UNIT", Value: unit})
		}
		return &ast.CloseStmt{
			Specifiers: specList,
			Position:   ast.Pos(start, p.current.start),
		}
	}

	// Handle full form with parentheses
	if !p.expect(token.LParen, "in CLOSE statement") {
		return nil
	}

	// Track if we've seen the first positional argument (which would be UNIT)
	isFirstArg := true

	// Parse comma-separated specifier list
	for !p.currentTokenIs(token.RParen) && !p.IsDone() && !p.current.tok.IsEnd() {
		// Parse one specifier: either expression or keyword=value
		spec := p.parseExpression(0)
		if spec == nil {
			break
		}

		// Check if this is a keyword=value pattern
		if p.currentTokenIs(token.Equals) {
			// Extract keyword name
			var keyword string
			if ident, ok := spec.(*ast.Identifier); ok {
				keyword = strings.ToUpper(ident.Value)
			} else {
				p.addError("expected identifier before = in CLOSE specifier")
				if !p.consumeIf(token.Comma) {
					break
				}
				continue
			}

			p.nextToken() // consume =
			value := p.parseExpression(0)
			if value != nil {
				specList = append(specList, ast.IOSpecifier{Name: keyword, Value: value})
				p.registerIOOutputSpecVar(keyword, value)
			}
			isFirstArg = false
		} else if isFirstArg {
			// First positional argument is UNIT
			specList = append(specList, ast.IOSpecifier{Name: "UNIT", Value: spec})
			isFirstArg = false
		} else {
			p.addError("unexpected expression in CLOSE statement (expected keyword=value)")
		}

		// Consume comma if present, otherwise should be at RParen
		if !p.consumeIf(token.Comma) {
			break
		}
	}

	// Consume the closing parenthesis
	if !p.expect(token.RParen, "closing CLOSE statement") {
		return nil
	}

	return &ast.CloseStmt{
		Specifiers: specList,
		Position:   ast.Pos(start, p.current.start),
	}
}

// parseBackspaceStmt parses a BACKSPACE statement
// Precondition: current token is BACKSPACE
// Syntax: BACKSPACE([UNIT=]u [,specifier-list]) or BACKSPACE unit
// Postcondition: current token is the first token after the BACKSPACE statement
func (p *Parser90) parseBackspaceStmt() ast.Statement {
	start := p.current.start
	p.expect(token.BACKSPACE, "")

	var specList []ast.IOSpecifier

	// Handle simple form: BACKSPACE unit (without parentheses)
	if !p.currentTokenIs(token.LParen) {
		unit := p.parseExpression(0)
		if unit != nil {
			specList = append(specList, ast.IOSpecifier{Name: "UNIT", Value: unit})
		}
		return &ast.BackspaceStmt{
			Specifiers: specList,
			Position:   ast.Pos(start, p.current.start),
		}
	}

	// Handle full form with parentheses
	if !p.expect(token.LParen, "in BACKSPACE statement") {
		return nil
	}

	// Track if we've seen the first positional argument (which would be UNIT)
	isFirstArg := true

	// Parse comma-separated specifier list
	for !p.currentTokenIs(token.RParen) && !p.IsDone() && !p.current.tok.IsEnd() {
		// Parse one specifier: either expression or keyword=value
		spec := p.parseExpression(0)
		if spec == nil {
			break
		}

		// Check if this is a keyword=value pattern
		if p.currentTokenIs(token.Equals) {
			// Extract keyword name
			var keyword string
			if ident, ok := spec.(*ast.Identifier); ok {
				keyword = strings.ToUpper(ident.Value)
			} else {
				p.addError("expected identifier before = in BACKSPACE specifier")
				if !p.consumeIf(token.Comma) {
					break
				}
				continue
			}

			p.nextToken() // consume =
			value := p.parseExpression(0)
			if value != nil {
				specList = append(specList, ast.IOSpecifier{Name: keyword, Value: value})
			}
			isFirstArg = false
		} else if isFirstArg {
			// First positional argument is UNIT
			specList = append(specList, ast.IOSpecifier{Name: "UNIT", Value: spec})
			isFirstArg = false
		} else {
			p.addError("unexpected expression in BACKSPACE statement (expected keyword=value)")
		}

		// Consume comma if present, otherwise should be at RParen
		if !p.consumeIf(token.Comma) {
			break
		}
	}

	// Consume the closing parenthesis
	if !p.expect(token.RParen, "closing BACKSPACE statement") {
		return nil
	}

	return &ast.BackspaceStmt{
		Specifiers: specList,
		Position:   ast.Pos(start, p.current.start),
	}
}

// parseRewindStmt parses a REWIND statement
// Precondition: current token is REWIND
// Syntax: REWIND([UNIT=]u [,specifier-list]) or REWIND unit
// Postcondition: current token is the first token after the REWIND statement
func (p *Parser90) parseRewindStmt() ast.Statement {
	start := p.current.start
	p.expect(token.REWIND, "")

	var specList []ast.IOSpecifier

	// Handle simple form: REWIND unit (without parentheses)
	if !p.currentTokenIs(token.LParen) {
		unit := p.parseExpression(0)
		if unit != nil {
			specList = append(specList, ast.IOSpecifier{Name: "UNIT", Value: unit})
		}
		return &ast.RewindStmt{
			Specifiers: specList,
			Position:   ast.Pos(start, p.current.start),
		}
	}

	// Handle full form with parentheses
	if !p.expect(token.LParen, "in REWIND statement") {
		return nil
	}

	// Track if we've seen the first positional argument (which would be UNIT)
	isFirstArg := true

	// Parse comma-separated specifier list
	for !p.currentTokenIs(token.RParen) && !p.IsDone() && !p.current.tok.IsEnd() {
		// Parse one specifier: either expression or keyword=value
		spec := p.parseExpression(0)
		if spec == nil {
			break
		}

		// Check if this is a keyword=value pattern
		if p.currentTokenIs(token.Equals) {
			// Extract keyword name
			var keyword string
			if ident, ok := spec.(*ast.Identifier); ok {
				keyword = strings.ToUpper(ident.Value)
			} else {
				p.addError("expected identifier before = in REWIND specifier")
				if !p.consumeIf(token.Comma) {
					break
				}
				continue
			}

			p.nextToken() // consume =
			value := p.parseExpression(0)
			if value != nil {
				specList = append(specList, ast.IOSpecifier{Name: keyword, Value: value})
			}
			isFirstArg = false
		} else if isFirstArg {
			// First positional argument is UNIT
			specList = append(specList, ast.IOSpecifier{Name: "UNIT", Value: spec})
			isFirstArg = false
		} else {
			p.addError("unexpected expression in REWIND statement (expected keyword=value)")
		}

		// Consume comma if present, otherwise should be at RParen
		if !p.consumeIf(token.Comma) {
			break
		}
	}

	// Consume the closing parenthesis
	if !p.expect(token.RParen, "closing REWIND statement") {
		return nil
	}

	return &ast.RewindStmt{
		Specifiers: specList,
		Position:   ast.Pos(start, p.current.start),
	}
}

// parseEndfileStmt parses an ENDFILE statement
// Precondition: current token is ENDFILE or FILE (if END was already consumed)
func (p *Parser90) parseEndfileStmt() ast.Statement {
	start := p.current.start
	if !p.consumeIf2(token.END, token.FILE) && !p.expect(token.ENDFILE, "") {
		return nil
	}

	var specList []ast.IOSpecifier

	// Handle simple form: ENDFILE unit (without parentheses)
	if !p.currentTokenIs(token.LParen) {
		unit := p.parseExpression(0)
		if unit != nil {
			specList = append(specList, ast.IOSpecifier{Name: "UNIT", Value: unit})
		}
		return &ast.EndfileStmt{
			Specifiers: specList,
			Position:   ast.Pos(start, p.current.start),
		}
	}

	// Handle full form with parentheses
	if !p.expect(token.LParen, "in ENDFILE statement") {
		return nil
	}

	// Track if we've seen the first positional argument (which would be UNIT)
	isFirstArg := true

	// Parse comma-separated specifier list
	for !p.currentTokenIs(token.RParen) && !p.IsDone() && !p.current.tok.IsEnd() {
		// Parse one specifier: either expression or keyword=value
		spec := p.parseExpression(0)
		if spec == nil {
			break
		}

		// Check if this is a keyword=value pattern
		if p.currentTokenIs(token.Equals) {
			// Extract keyword name
			var keyword string
			if ident, ok := spec.(*ast.Identifier); ok {
				keyword = strings.ToUpper(ident.Value)
			} else {
				p.addError("expected identifier before = in ENDFILE specifier")
				if !p.consumeIf(token.Comma) {
					break
				}
				continue
			}

			p.nextToken() // consume =
			value := p.parseExpression(0)
			if value != nil {
				specList = append(specList, ast.IOSpecifier{Name: keyword, Value: value})
			}
			isFirstArg = false
		} else if isFirstArg {
			// First positional argument is UNIT
			specList = append(specList, ast.IOSpecifier{Name: "UNIT", Value: spec})
			isFirstArg = false
		} else {
			p.addError("unexpected expression in ENDFILE statement (expected keyword=value)")
		}

		// Consume comma if present, otherwise should be at RParen
		if !p.consumeIf(token.Comma) {
			break
		}
	}

	// Consume the closing parenthesis
	if !p.expect(token.RParen, "closing ENDFILE statement") {
		return nil
	}

	return &ast.EndfileStmt{
		Specifiers: specList,
		Position:   ast.Pos(start, p.current.start),
	}
}

// parseStopStmt parses a STOP statement
// Precondition: current token is STOP
// Syntax: STOP [code]
// Postcondition: current token is the first token after the STOP statement
func (p *Parser90) parseStopStmt() ast.Statement {
	start := p.current.start
	p.expect(token.STOP, "")

	stmt := &ast.StopStmt{
		Position: ast.Pos(start, p.current.start),
	}

	// Check if there's an optional stop code (integer or string)
	if !p.currentTokenIs(token.NewLine) && !p.IsDone() && !p.current.tok.IsEnd() {
		code := p.parseExpression(0)
		if code != nil {
			stmt.Code = code
		}
	}

	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseFormatStmt parses a FORMAT statement
// Precondition: current token is FORMAT
// Syntax: label FORMAT(format-spec)
// Postcondition: current token is the first token after the FORMAT statement
func (p *Parser90) parseFormatStmt() ast.Statement {
	start := p.current.start
	p.expect(token.FORMAT, "")

	if !p.expect(token.LParen, "in FORMAT statement") {
		return nil
	}

	stmt := &ast.FormatStmt{
		Position: ast.Pos(start, p.current.start),
	}

	stmt.Specs = p.parseFormatSpecs()
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseFormatSpecs parses format specifications until closing paren.
// Handles: descriptors, groups, string literals, control characters.
func (p *Parser90) parseFormatSpecs() []ast.FormatSpec {
	var specs []ast.FormatSpec

	for !p.currentTokenIs(token.RParen) && !p.IsDone() {
		// Skip commas
		if p.currentTokenIs(token.Comma) {
			p.nextToken()
			continue
		}

		spec := p.parseOneFormatSpec()
		specs = append(specs, spec)
	}

	if p.currentTokenIs(token.RParen) {
		p.nextToken() // consume closing )
	}

	return specs
}

// parseOneFormatSpec parses a single format specification element.
func (p *Parser90) parseOneFormatSpec() ast.FormatSpec {
	var spec ast.FormatSpec

	switch p.current.tok {
	case token.FormatSpec, token.Identifier:
		// Parse format spec token like I3, F10.2, ES12.5, 6I3, E12.5E3
		spec = parseFormatSpecLiteral(string(p.current.lit))
		p.nextToken()

	case token.IntLit:
		// Could be repeat count followed by group:  3(I3,F6.2)
		repeat := p.parseSimpleInt()
		if p.currentTokenIs(token.LParen) {
			// Grouped repeat: 3(I3,F6.2)
			p.nextToken() // consume (
			spec.Repeat = repeat
			spec.Group = p.parseFormatSpecs()
		} else {
			p.addError("unexpected token after integer in FORMAT")
		}

	case token.StringLit:
		spec.StringLit = string(p.current.lit)
		p.nextToken()

	case token.Slash:
		// Record terminator /
		spec.Descriptor[0] = '/'
		p.nextToken()

	case token.Colon:
		// Conditional terminator :
		spec.Descriptor[0] = ':'
		p.nextToken()

	case token.Asterisk:
		// Unlimited repeat: *(...)
		p.nextToken()
		if p.currentTokenIs(token.LParen) {
			p.nextToken() // consume (
			spec.Repeat = -1
			spec.Group = p.parseFormatSpecs()
		}

	case token.LParen:
		// Ungrouped parentheses (just a group without repeat)
		p.nextToken() // consume (
		spec.Group = p.parseFormatSpecs()

	default:
		// Unknown token - skip it
		p.nextToken()
	}

	return spec
}

// parseSimpleInt parses an integer literal and returns its value.
func (p *Parser90) parseSimpleInt() int {
	if !p.currentTokenIs(token.IntLit) {
		return 0
	}
	val := 0
	for _, b := range p.current.lit {
		if b >= '0' && b <= '9' {
			val = val*10 + int(b-'0')
		}
	}
	p.nextToken()
	return val
}

// parseFormatSpecLiteral parses a format spec string like "I3", "F10.2", "ES12.5", "6I3", "E12.5E3"
func parseFormatSpecLiteral(lit string) (spec ast.FormatSpec) {
	spec.SetFromString(lit)
	return spec
}

// parseAllocateOrDeallocateStmt parses ALLOCATE or DEALLOCATE statements
// Precondition: current token is ALLOCATE or DEALLOCATE
// Syntax: ALLOCATE(allocation-list [, option-list])
//
//	DEALLOCATE(object-list [, option-list])
//
// Postcondition: current token is the first token after the statement
func (p *Parser90) parseAllocateOrDeallocateStmt() ast.Statement {
	start := p.current.start
	stmtName := p.current.tok.String()
	isAllocate := p.consumeIf(token.ALLOCATE)
	if !isAllocate && !p.consumeIf(token.DEALLOCATE) {
		p.addError("expected ALLOCATE or DEALLOCATE")
		return nil
	}

	if !p.expect(token.LParen, "in "+stmtName+" statement") {
		return nil
	}

	// Parse comma-separated list of objects and options
	var objects []ast.Expression
	options := make(map[string]ast.Expression)

	for !p.currentTokenIs(token.RParen) && !p.IsDone() && !p.current.tok.IsEnd() {
		expr := p.parseExpression(0)
		if expr == nil {
			break
		}

		// Check if this is a keyword=value option (STAT, ERRMSG, SOURCE, MOLD)
		if p.currentTokenIs(token.Equals) {
			var keyword string
			if ident, ok := expr.(*ast.Identifier); ok {
				keyword = strings.ToUpper(ident.Value)
			} else {
				p.addError("expected identifier before = in " + stmtName + " option")
				if !p.consumeIf(token.Comma) {
					break
				}
				continue
			}

			p.nextToken() // consume =
			value := p.parseExpression(0)
			if value != nil {
				options[keyword] = value
			}
		} else {
			// This is an allocation/deallocation object
			objects = append(objects, expr)
		}

		if !p.consumeIf(token.Comma) {
			break
		}
	}

	if !p.expect(token.RParen, "closing "+stmtName+" statement") {
		return nil
	}

	pos := ast.Pos(start, p.current.start)

	// Build appropriate statement type
	if isAllocate {
		return &ast.AllocateStmt{
			Objects:  objects,
			Options:  options,
			Position: pos,
		}
	} else {
		return &ast.DeallocateStmt{
			Objects:  objects,
			Options:  options,
			Position: pos,
		}
	}
}

// parseIfStmt parses an IF construct
// Precondition: current token is IF
func (p *Parser90) parseIfStmt() ast.Statement {
	start := p.sourcePos()
	p.expect(token.IF, "")

	condition, assign := p.parseIndexCallOrAssignment("opening IF")
	if assign != nil {
		assign.Target = &ast.Identifier{
			Value:    "IF",
			Position: ast.Pos(start.Pos, start.Pos+2),
		}
		return assign
	} else if condition == nil {
		p.addError("expected condition in IF statement")
		return nil
	}

	// Check for arithmetic IF (F77): IF (expr) label1, label2, label3
	// This branches to label1 if expr < 0, label2 if expr == 0, label3 if expr > 0
	if p.currentTokenIs(token.IntLit) && p.peekTokenIs(token.Comma) {
		return p.parseArithmeticIfStmt(start, condition)
	}

	stmt := &ast.IfStmt{Condition: condition}
	// INLINE IF check (no THEN) vs block IF (with THEN)
	if !p.currentTokenIs(token.THEN) {
		// Inline IF: IF (condition) statement
		// Parse single executable statement
		if s := p.parseExecutableStatement(); s != nil {
			stmt.ThenPart = append(stmt.ThenPart, s)
		} else {
			p.addError("expected executable statement after IF condition")
			return nil
		}
		stmt.Position = ast.Pos(start.Pos, p.current.start)
		return stmt
	}
	// Block IF: IF (condition) THEN ... END IF
	p.nextToken() // consume THEN
	p.skipNewlinesAndComments()

	// THEN block parsing.
	for p.loopUntil(token.ELSE, token.ELSEIF, token.ENDIF) {
		// Stop if we see END not followed by = (this is the end of the IF block)
		// Allow END = to be parsed as an assignment statement
		if p.currentTokenIs(token.END) && !p.peekTokenIs(token.Equals) {
			break
		}
		if s := p.parseExecutableStatement(); s != nil {
			stmt.ThenPart = append(stmt.ThenPart, s)
		} else {
			p.consumeEndLabelIfPresent(&stmt.EndLabel, token.IF, "")
			p.skipToNextStatement()
		}
		p.skipNewlinesAndComments()
	}

	// Parse ELSE IF parts
	for p.consumeIf(token.ELSEIF) || p.consumeIf2(token.ELSE, token.IF) {
		clause := ast.ElseIfClause{}
		clauseStart := p.current.start
		// Parse ELSE IF condition before THEN.
		p.expect(token.LParen, "after ELSE IF")
		clause.Condition = p.parseExpression(0)
		if clause.Condition == nil {
			p.addError("expected condition in ELSE IF statement")
			return nil
		}
		p.expect(token.RParen, "after ELSE IF")

		// Parse ELSE IF block
		p.expect(token.THEN, "after ELSE IF") // This is required in fortran, no inline ELSE IF.
		p.skipNewlinesAndComments()
		for p.loopUntil(token.ELSE, token.ENDIF, token.ELSEIF) {
			// Stop if we see END not followed by = (this is the end of the IF block)
			// Allow END = to be parsed as an assignment statement
			if p.currentTokenIs(token.END) && !p.peekTokenIs(token.Equals) {
				break
			}
			if s := p.parseExecutableStatement(); s != nil {
				clause.ThenPart = append(clause.ThenPart, s)
			} else {
				p.consumeEndLabelIfPresent(&stmt.EndLabel, token.IF, "")
				p.skipToNextStatement()
			}
			p.skipNewlinesAndComments()
		}
		clause.Position = ast.Pos(clauseStart, p.current.start)
		stmt.ElseIfParts = append(stmt.ElseIfParts, clause)
	}

	// Parse ELSE part
	if p.consumeIf(token.ELSE) {
		p.skipNewlinesAndComments()
		for p.loopUntil(token.ENDIF) {
			// Stop if we see END not followed by = (this is the end of the IF block)
			// Allow END = to be parsed as an assignment statement
			if p.currentTokenIs(token.END) && !p.peekTokenIs(token.Equals) {
				break
			}
			if s := p.parseExecutableStatement(); s != nil {
				stmt.ElsePart = append(stmt.ElsePart, s)
			} else {
				p.consumeEndLabelIfPresent(&stmt.EndLabel, token.IF, "")
				p.skipToNextStatement()
			}
			p.skipNewlinesAndComments()
		}
	}

	// Expect END IF
	p.consumeEndLabelIfPresent(&stmt.EndLabel, token.IF, "")
	p.expectEndConstruct(token.IF, token.ENDIF, start)
	stmt.Position = ast.Pos(start.Pos, p.current.start)
	return stmt
}

func (p *Parser90) parseArithmeticIfStmt(start sourcePos, condition ast.Expression) ast.Statement {
	arithmeticStmt := &ast.ArithmeticIfStmt{Condition: condition}
	// Parse negative label
	arithmeticStmt.NegativeLabel = string(p.current.lit)
	if !p.expect(token.IntLit, "first label in arithmetic IF") {
		return nil
	}
	if !p.expect(token.Comma, "after first label in arithmetic IF") {
		return nil
	}

	// Parse zero label
	if !p.currentTokenIs(token.IntLit) {
		p.addError("expected label for zero case in arithmetic IF")
		return nil
	}
	arithmeticStmt.ZeroLabel = string(p.current.lit)
	p.nextToken() // consume label
	if !p.expect(token.Comma, "after second label in arithmetic IF") {
		return nil
	}

	// Parse positive label
	if !p.currentTokenIs(token.IntLit) {
		p.addError("expected label for positive case in arithmetic IF")
		return nil
	}
	arithmeticStmt.PositiveLabel = string(p.current.lit)
	p.nextToken() // consume label

	arithmeticStmt.Position = ast.Pos(start.Pos, p.current.start)
	return arithmeticStmt
}

// parseSelectCaseStmt parses a SELECT CASE construct
// Precondition: current token is SELECT
func (p *Parser90) parseSelectCaseStmt() ast.Statement {
	start := p.sourcePos()
	// Start parsing SELECT CASE opening statement and case expression.
	if !p.expect(token.SELECT, "") || !p.expect(token.CASE, "after SELECT") ||
		!p.expect(token.LParen, "after SELECT CASE") {
		return nil
	}
	expression := p.parseExpression(0)
	if expression == nil {
		p.addError("expected expression in SELECT CASE")
		return nil
	}
	if !p.expect(token.RParen, "closing parenthesis in SELECT CASE") {
		return nil
	}

	stmt := &ast.SelectCaseStmt{
		Expression: expression,
	}
	p.skipNewlinesAndComments()
	// Parse CASE clauses until END SELECT
	for p.loopUntil(token.END, token.ENDSELECT) {
		if p.currentTokenIs(token.CASE) {
			caseClause := p.parseCaseClause()
			if caseClause != nil {
				stmt.Cases = append(stmt.Cases, *caseClause)
			}
		} else {
			// Not a CASE clause, skip to next statement
			p.consumeEndLabelIfPresent(&stmt.EndLabel, token.SELECT, "")
			p.skipToNextStatement()
		}
		p.skipNewlinesAndComments()
	}

	// Expect END SELECT
	p.consumeEndLabelIfPresent(&stmt.EndLabel, token.SELECT, "")
	p.expectEndConstruct(token.SELECT, token.ENDSELECT, start)
	stmt.Position = ast.Pos(start.Pos, p.current.start)
	return stmt
}

// parseCaseClause parses a single CASE clause
// Precondition: current token is CASE
func (p *Parser90) parseCaseClause() *ast.CaseClause {
	start := p.sourcePos()
	p.expect(token.CASE, "")
	clause := &ast.CaseClause{}
	// Check for CASE DEFAULT
	if p.consumeIf(token.DEFAULT) {
		clause.IsDefault = true
	} else if p.consumeIf(token.LParen) {
		// CASE (value-list)
		// Parse comma-separated list of values
		parseOneValue := func() (ast.Expression, error) {
			val := p.parseExpression(0)
			if val == nil {
				return nil, fmt.Errorf("expected expression in CASE value list")
			}
			return val, nil
		}

		values, err := parseCommaSeparatedList(p, token.RParen, parseOneValue)
		if err != nil {
			p.addError(err.Error())
		}
		clause.Values = values
		p.expect(token.RParen, "end CASE expression")
	}

	p.skipNewlinesAndComments()

	// Parse body statements until next CASE or END SELECT
	for p.loopUntil(token.CASE, token.END, token.ENDSELECT) {
		if s := p.parseExecutableStatement(); s != nil {
			clause.Body = append(clause.Body, s)
		} else {
			p.skipToNextStatement()
		}
		p.skipNewlinesAndComments()
	}
	clause.Position = ast.Pos(start.Pos, p.current.start)
	return clause
}

// parseDoLoop parses a DO loop
func (p *Parser90) parseDoLoop() ast.Statement {
	start := p.sourcePos()
	stmt := &ast.DoLoop{}
	p.expect(token.DO, "")

	if p.currentTokenIs(token.IntLit) {
		stmt.TargetLabel = string(p.current.lit)
		p.nextToken()
	}

	// Check for DO WHILE
	if p.consumeIf(token.WHILE) {
		p.expect(token.LParen, "after DO WHILE")

		stmt.Start = p.parseExpression(0) // Condition stored in Start for DO WHILE
		if stmt.Start == nil {
			p.addError("expected condition in DO WHILE statement")
			return nil
		}
		p.expect(token.RParen, "after DO WHILE")
	} else {
		// Counter-controlled DO loop
		if p.canUseAsIdentifier() {
			stmt.Var = p.currentAsVarString(VFlagUsed, "")
			p.nextToken()
			p.expect(token.Equals, "after loop variable")

			stmt.Start = p.parseExpression(0)
			if stmt.Start == nil {
				p.addError("expected start expression in DO loop")
				return nil
			}
			p.expect(token.Comma, "after start expression")

			stmt.End = p.parseExpression(0)
			if stmt.End == nil {
				p.addError("expected end expression in DO loop")
				return nil
			}

			if p.currentTokenIs(token.Comma) {
				p.nextToken() // consume ,
				stmt.Step = p.parseExpression(0)
				if stmt.Step == nil {
					p.addError("expected step expression in DO loop")
					return nil
				}
			}
		}
	}

	p.skipNewlinesAndComments()

	// Parse loop body
	// NOTE: For F77-style DO loops with labels, we parse until END (of containing unit)
	// Label verification is skipped since we don't maintain a symbol table
	for !p.IsDone() {
		p.skipNewlinesAndComments()

		if p.consumeEndLabelIfPresent(&stmt.EndLabel, token.DO, stmt.TargetLabel) {
			if p.peekTokenIs(token.CONTINUE) {
				// Found closing target continue statement - consume both label and CONTINUE
				p.nextToken() // consume the label
				p.nextToken() // consume CONTINUE
				stmt.Position = ast.Pos(start.Pos, p.current.start)
				return stmt
			}
		}

		// Check for END DO or ENDDO
		if p.currentTokenIs(token.END) && p.peekTokenIs(token.DO) {
			break
		}
		if p.currentTokenIs(token.ENDDO) {
			break
		}

		// Check for END of containing program unit (SUBROUTINE, FUNCTION, PROGRAM, etc.)
		// This handles F77 DO loops with labels where there's no END DO
		if p.currentTokenIs(token.END) {
			break
		}

		if p.currentTokenIs(token.EOF) {
			break
		}

		if s := p.parseExecutableStatement(); s != nil {
			stmt.Body = append(stmt.Body, s)
			// Check if this statement has the target label (F77 DO loop termination)
			// This handles both direct labeled statements and shared DO termination
			if stmt.TargetLabel != "" {
				// Check if the statement itself has the target label
				label := s.GetLabel()
				if label != nil && *label == stmt.TargetLabel {
					break
				}
				// Check for shared DO termination: nested DO that ends at our target
				if nestedDO, ok := s.(*ast.DoLoop); ok {
					if nestedDO.TargetLabel == stmt.TargetLabel || nestedDO.EndLabel == stmt.TargetLabel {
						break
					}
				}
			}
		} else {
			p.skipToNextStatement()
		}
	}

	// Expect END DO terminator (labels are not verified without symbol table)
	// Validate target label matches end label if both present
	if stmt.TargetLabel != "" && stmt.EndLabel != "" && stmt.TargetLabel != stmt.EndLabel {
		p.addError("DO target label '" + stmt.TargetLabel + "' does not match end label '" + stmt.EndLabel + "'")
	}
	// F77 DO loops with target labels may not have END DO
	if stmt.TargetLabel == "" || stmt.TargetLabel == stmt.EndLabel {
		p.expectEndConstruct(token.DO, token.ENDDO, start)
	}
	stmt.Position = ast.Pos(start.Pos, p.current.start)
	return stmt
}

// parseCallStmt parses a CALL statement
func (p *Parser90) parseCallStmt() ast.Statement {
	start := p.current.start
	stmt := &ast.CallStmt{}
	p.expect(token.CALL, "") // consume CALL

	if !p.canUseAsIdentifier() {
		p.addError("expected subroutine name after CALL")
		return nil
	}
	stmt.Name = string(p.current.lit)
	p.nextToken()

	if p.consumeIf(token.LParen) {
		parseOneCallArg := func() (ast.Expression, error) {
			// Check for alternate return argument (Fortran 77): *<label>
			if p.current.tok == token.Asterisk {
				argStart := p.current.start
				p.nextToken() // consume *
				if !p.canUseAsIdentifier() && p.current.tok != token.IntLit {
					return nil, fmt.Errorf("expected label after * in alternate return")
				}
				label := string(p.current.lit)
				argEnd := p.current.start + len(p.current.lit)
				p.nextToken()
				return &ast.AlternateReturnArg{
					Label:    label,
					Position: ast.Pos(argStart, argEnd),
				}, nil
			}
			return p.parseOneArg() // handles keyword=value and range subscripts
		}

		args, err := parseCommaSeparatedList(p, token.RParen, parseOneCallArg)
		if err != nil {
			p.addError(err.Error())
		}
		stmt.Args = args
		p.consumeIf(token.RParen) // TODO: should be expect?
		// Register implicit variables from CALL arguments.
		for _, arg := range args {
			p.registerImplicitFromTarget(arg)
		}
	}
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseEntryStmt parses an ENTRY statement (F77 feature for alternate entry points)
func (p *Parser90) parseEntryStmt() ast.Statement {
	start := p.current.start
	stmt := &ast.EntryStmt{}
	p.expect(token.ENTRY, "") // consume ENTRY

	if !p.canUseAsIdentifier() {
		p.addError("expected entry point name after ENTRY")
		return nil
	}
	stmt.Name = string(p.current.lit)
	p.nextToken()

	if p.currentTokenIs(token.LParen) {
		stmt.Parameters = p.parseParameterList()
	}
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseReturnStmt parses a RETURN statement, including Fortran 77 alternate returns
// RETURN or RETURN <integer-expression>
func (p *Parser90) parseReturnStmt() ast.Statement {
	start := p.current.start
	stmt := &ast.ReturnStmt{}
	p.expect(token.RETURN, "") // consume RETURN

	// Check for alternate return (Fortran 77): RETURN <integer>
	// The integer expression indicates which alternate return to use
	if !p.current.tok.IsEnd() && p.current.tok != token.NewLine && p.current.tok != token.Semicolon {
		// Parse the integer expression
		stmt.AlternateReturn = p.parseExpression(0)
	}

	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseCycleStmt parses a CYCLE statement
// Syntax: CYCLE [construct-name]
func (p *Parser90) parseCycleStmt() ast.Statement {
	start := p.current.start
	stmt := &ast.CycleStmt{}
	p.expect(token.CYCLE, "") // consume CYCLE

	// Check for optional construct name
	// TODO: this should likely be parsed in parseExecStmt like for other constructs. Consider token.Token.CanHaveConstructLabel()
	if p.currentTokenIs(token.Identifier) {
		stmt.ConstructName = string(p.current.lit)
		p.nextToken()
	}

	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseExitStmt parses an EXIT statement
// Syntax: EXIT [construct-name]
func (p *Parser90) parseExitStmt() ast.Statement {
	start := p.current.start
	stmt := &ast.ExitStmt{}
	p.expect(token.EXIT, "") // consume EXIT

	// Check for optional construct name
	if p.currentTokenIs(token.Identifier) {
		stmt.ConstructName = string(p.current.lit)
		p.nextToken()
	}

	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseAssignmentStmt parses an assignment statement or statement function definition.
// Statement functions look like: FUNCNAME(ARG1, ARG2) = expr
// They are distinguished from assignments by having an undeclared function name
// with simple identifier arguments.
func (p *Parser90) parseAssignmentStmt() ast.Statement {
	startPos := p.current.start

	// The "target" of an assignment can be a complex expression itself (e.g., array slice, derived type component)
	// We parse it as a general expression.
	target := p.parseExpression(0)
	if target == nil {
		p.addError("invalid target for assignment")
		return nil
	}
	// Register implicit variable if the target is an undeclared identifier.
	p.registerImplicitFromTarget(target)
	assignment := &ast.AssignmentStmt{
		Target: target,
	}
	if !p.parseAssignmentRHS(assignment, startPos) {
		return nil
	}
	return assignment
}

// registerIOOutputSpecVar registers a variable as implicit when used as an IO write-context specifier (IOSTAT, IOMSG, SIZE).
// These specifiers receive values written by the IO runtime, so they act as assignment targets.
func (p *Parser90) registerIOOutputSpecVar(keyword string, value ast.Expression) {
	switch keyword {
	case "IOSTAT", "IOMSG", "SIZE", "EXIST", "OPENED", "NUMBER", "NAMED", "NEXTREC":
		p.registerImplicitFromTarget(value)
	}
}

// registerImplicitFromTarget registers implicit variables found in assignment targets.
func (p *Parser90) registerImplicitFromTarget(target ast.Expression) {
	if p.vars.isImplicitNone() {
		return // IMPLICIT NONE - don't auto-register
	}
	var name string
	switch t := target.(type) {
	case *ast.Identifier:
		name = t.Value
	case *ast.CallExpr:
		name = t.Name
		// Register ALL CallExpr targets - transpiler will disambiguate
		// between array access, function calls, and intrinsics.
	default:
		return
	}
	vi := p.vars.Var(name)
	if vi == nil {
		implicitDecl := p.vars.implicitDeclFor(name)
		p.varInit(name, implicitDecl, VFlagImplicit, "")
	}
}

func (p *Parser90) parseAssignmentRHS(dst *ast.AssignmentStmt, startPos int) bool {
	isPtrAssign := p.currentTokenIs(token.PointerAssign)
	isEquals := p.currentTokenIs(token.Equals)
	if !isPtrAssign && !isEquals {
		p.addError("expected '=' or '=>' for assignment statement")
		return false
	}
	p.nextToken()
	value := p.parseExpression(0)
	if value == nil {
		p.addError("expected expression after '='")
		return false
	}
	dst.IsPointerAssignment = isPtrAssign
	dst.Value = value
	dst.Position = ast.Pos(startPos, p.current.start)
	return true
}

// parseIndexCallOrAssignment handles ambiguous constructs like POINTER(...) that could be either:
// - An assignment: pointer(i) = value
// - A declaration/statement: POINTER(x,y) or READ(10,*) x
//
// Strategy: Parse the expression including parentheses, then check what follows.
// If '=' or '=>' follows, it's an assignment. Otherwise, return the parsed expression
// for the caller to interpret in context.
//
// Precondition: current token is the keyword/identifier (POINTER, READ, etc.)
// Postcondition: If assignment, returns (nil, assignmentStmt). Otherwise returns (parenExpr, nil)
//
//	and parser is positioned after the closing parenthesis.
func (p *Parser90) parseIndexCallOrAssignment(context string) (parenExpr ast.Expression, assignment *ast.AssignmentStmt) {
	startPos := p.current.start
	if !p.expect(token.LParen, context) {
		return nil, nil
	}
	// Parse the full expression: keyword(...) or identifier(...)
	// This handles: pointer(i), pointer(i,j), pointer(a+b(k)), etc.
	lhs := p.parseExpression(0)
	if lhs == nil {
		return nil, nil
	}
	if !p.expect(token.RParen, context) {
		return nil, nil
	}

	switch p.current.tok {
	default:
		// Not an assignment, just an expression inside parentheses.
		// i.e: IF(blabla)THEN or POINTER(blabla) (blabla)
		return lhs, nil
	case token.Equals, token.PointerAssign:
		assignmentConcrete := &ast.AssignmentStmt{
			Target: lhs,
		}
		if !p.parseAssignmentRHS(assignmentConcrete, startPos) {
			return nil, nil
		}
		return nil, assignmentConcrete
	}
}

// skipToNextStatement skips tokens until the next newline or construct-ending keyword
func (p *Parser90) skipToNextStatement() {
	for p.loopUntilEndElseOr(token.NewLine, token.LineComment) {
		p.nextToken()
	}
	// Consume trailing line comment and newline
	p.consumeIf(token.LineComment)
	p.consumeIf(token.NewLine)
}

// parseDerivedTypeStmt parses a derived type definition: TYPE :: name ... END TYPE
// Example: TYPE :: person
//
//	  INTEGER :: age
//	  REAL :: height
//	END TYPE person
func (p *Parser90) parseDerivedTypeStmt() *ast.DerivedTypeStmt {
	startPos := p.sourcePos()
	p.expect(token.TYPE, "parse derived type")
	attrs := p.parseTypeAttributess()
	// if intent != 0 || spec != nil { // TODO: validate here?
	// 	p.addError("derived type statement should not have array spec nor intent attribute")
	// }
	p.consumeIf(token.DoubleColon)                                // Optional :: after TYPE
	if !p.canUseAsIdentifier() || !p.peekTokenIs(token.NewLine) { // Type name
		p.addError("expected type name after TYPE and newline")
		return nil
	}
	typeName := string(p.current.lit)
	p.nextToken() // consume identifier.
	p.skipNewlinesAndComments()
	// Parse component declarations until END TYPE
	var components []ast.ComponentDecl
	for p.loopUntil(token.END, token.ENDTYPE) {
		// Parse component declaration (check for type tokens)
		if p.current.tok.IsTypeDeclaration() {
			comp := p.parseComponentDecl()
			if comp != nil {
				components = append(components, *comp)
			}
		} else {
			p.addError("expected component declaration, got " + p.current.tok.String())
			p.nextToken() // Consume the bad token to avoid infinite loop
		}
		p.skipNewlinesAndComments()
	}
	p.expectEndConstruct(token.TYPE, token.ENDTYPE, startPos)
	if p.canUseAsIdentifier() {
		if string(p.current.lit) != typeName {
			p.addErrorWithPos(startPos, "name not matched with end label")
			p.addError("mismatched type name")
		}
		p.nextToken()
	}
	stmt := &ast.DerivedTypeStmt{
		Name:       typeName,
		Attributes: attrs,
		Components: components,
		Position:   ast.Pos(startPos.Pos, p.current.start),
	}
	return stmt
}

// parseComponentDecl parses a component declaration within a derived type
// Example: INTEGER :: age
//
//	REAL, DIMENSION(3) :: velocity
//	integer, len :: rows, cols
//	integer, kind :: k = kind(0.0)
func (p *Parser90) parseComponentDecl() *ast.ComponentDecl {
	startPos := p.current.start
	ts := p.expectTypeSpec(false)
	// KIND and LEN are allowed in component declarations (e.g., CHARACTER(LEN=50), REAL(KIND=8))
	// Parse optional attributes (POINTER, DIMENSION(:), ALLOCATABLE, etc.)
	attributes := p.parseTypeAttributess()
	// F77: REAL lat, lon (no ::)
	// F90: REAL :: lat, lon (with ::)
	p.consumeIf(token.DoubleColon)
	// Parse component names (similar to entity list)
	// Note: DATA can be used as component name, even though tok.CanBeUsedAsIdentifier() returns false for it
	var components []ast.DeclEntity
	for p.canUseAsIdentifier() || p.currentTokenIs(token.DATA) {
		entity := ast.DeclEntity{
			Name:     string(p.current.lit),
			Position: p.currentAstPos(),
		}
		p.nextToken() // consume identifier.
		if p.currentTokenIs(token.LParen) {
			entity.ArraySpec = p.parseArraySpec() // Found array spec.
		}
		components = append(components, entity)
		if !p.consumeIf(token.Comma) {
			// No comma, so we're done with the entity list
			// Don't consume newline here - let the outer loop handle it
			break
		}
		// Continue loop if there was a comma (more entities)
	}

	return &ast.ComponentDecl{
		Type:       ts,
		Attributes: attributes,
		Components: components,
		Position:   ast.Pos(startPos, p.current.start),
	}
}

func (p *Parser90) skipConstruct(keyword, endComposed token.Token) {
	if !p.expect(keyword, "skip") {
		panic("invalid skip")
	}
	depth := 1
	for depth > 0 && !p.IsDone() {
		if p.consumeIf(keyword) {
			depth++
		} else if p.consumeIf(endComposed) || p.consumeIf2(token.END, keyword) {
			depth--
			if depth == 0 {
				// Consume optional name after END <keyword>
				if p.canUseAsIdentifier() {
					p.nextToken()
				}
			}
		} else {
			p.nextToken()
		}
	}
}

// isEndOfProgramUnit checks if current END token ends a program unit
func (p *Parser90) isEndOfProgramUnit() bool {
	nEndTok := token.IsEndProgramUnit(p.current.tok, p.peek.tok)
	if nEndTok == 0 {
		return false
	} else if nEndTok == 2 && p.current.tok == token.END && strings.EqualFold(string(p.peek.lit), "FILE") {
		return false
	}
	return true
}

// parseDataStmt parses a DATA statement
// Precondition: current token is DATA
// Syntax: DATA var1, var2, ... / value1, value2, ... /
func (p *Parser90) parseDataStmt() ast.Statement {
	//TODO: this could be simplified and modularized a lot...
	start := p.current.start
	p.expect(token.DATA, "")

	stmt := &ast.DataStmt{
		Position: ast.Pos(start, p.current.start),
	}

	// F77 allows multiple var-list/value-list pairs:
	// DATA var1 /val1/, var2 /val2/, var3 /val3/
	// Loop to handle all pairs
	for p.loopUntil(token.NewLine) {
		vl := p.parseVarlist()
		stmt.Varlists = append(stmt.Varlists, vl)
		// Check for comma or another variable (continuation case)
		p.consumeIf(token.Comma)
		// If next token is not an identifier, we're done
		if !p.canUseAsIdentifier() {
			break
		}
	}

	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

func (p *Parser90) parseVarlist() (vl ast.Varlist) {
	// Parse Variables.
	for p.loopUntil(token.Slash, token.NewLine) {
		var varName string
		varStart := p.sourcePos()
		if p.currentTokenIs(token.LParen) {
			startPos := p.sourcePos()
			p.nextToken()
			doExpr := p.parseExpression(0)
			impliedDoLoop := p.tryParseImpliedDoLoop(startPos.Pos, doExpr)
			if impliedDoLoop == nil {
				return vl
			}
			vl.Variables = append(vl.Variables, impliedDoLoop)
		} else if p.expectIdentifier(&varName, "DATA statement") {
			// p.varInit(varName,nil,)
			// Register implicit variable if not already declared.
			if p.vars.Var(varName) == nil && !p.vars.isImplicitNone() {
				implicitDecl := p.vars.implicitDeclFor(varName)
				p.varInit(varName, implicitDecl, VFlagImplicit, "")
			}
			// Array subscript: arr(1,2,3)
			var subscripts []ast.Expression
			if p.consumeIf(token.LParen) {
				parseExpr := func() (ast.Expression, error) {
					expr := p.parseExpression(0)
					if expr == nil {
						return nil, errors.New("failed to parse DATA array reference")
					}
					return expr, nil
				}
				exprs, err := parseCommaSeparatedList(p, token.RParen, parseExpr)
				if err != nil {
					return vl
				}
				subscripts = exprs
				p.expect(token.RParen, "closing parentheses for DATA array reference")
			}
			vl.Variables = append(vl.Variables, &ast.CallExpr{
				Name:     varName,
				Args:     subscripts,
				Position: ast.Pos(varStart.Pos, p.currentAstPos().End()),
			})
		}
		p.consumeIf(token.Comma)
	}
	// Expect opening slash
	if !p.expect(token.Slash, "before DATA values") {
		return vl
	}
	// Parse value list - handle repeat specifier N*value
	for p.loopUntil(token.Slash, token.NewLine) {
		value := p.parseDataValue()
		if value != nil {
			vl.Values = append(vl.Values, value)
		} else {
			break
		}
		if !p.consumeIf(token.Comma) {
			break
		}
	}
	// Expect closing slash
	p.expect(token.Slash, "after DATA values")
	return vl
}

// parseDataValue parses a DATA value, handling repeat specifier N*constant.
// Per Fortran spec, DATA values must be constants (literal or named).
// The repeat specifier syntax is: repeat-count * constant
func (p *Parser90) parseDataValue() ast.Expression {
	start := p.current.start

	// Parse first part (could be count or the value itself)
	first := p.parseExpression(0, token.Slash, token.Comma, token.Asterisk)
	if first == nil {
		return nil
	}

	// Check for repeat specifier: N*value
	if p.consumeIf(token.Asterisk) {
		value := p.parseExpression(0, token.Slash, token.Comma)
		return &ast.DataRepeatExpr{
			Count:    first,
			Value:    value,
			Position: ast.Pos(start, p.currentAstPos().End()),
		}
	}

	return first
}

// parseImplicit parses IMPLICIT statements:
//   - IMPLICIT NONE
//   - IMPLICIT type-spec (letter-spec-list)
//   - IMPLICIT type-spec (letter-spec-list), type-spec (letter-spec-list), ...
//
// Examples:
//
//	IMPLICIT NONE
//	IMPLICIT REAL (A-H, O-Z)
//	IMPLICIT INTEGER (I-N)
//	IMPLICIT REAL(KIND=8) (A-C, X-Z), INTEGER (I-N)
func (p *Parser90) parseImplicit() ast.Statement {
	start := p.current.start
	stmt := &ast.ImplicitStatement{}
	p.expect(token.IMPLICIT, "")
	// Check for IMPLICIT NONE
	if p.currentTokenIs(token.Identifier) && strings.EqualFold(string(p.current.lit), "NONE") {
		if len(p.vars.implicits) > 0 {
			if p.vars.implicits[0].IsNone {
				p.addError("duplicate IMPLICIT NONE")
			} else {
				p.addError("IMPLICIT NONE not allowed after IMPLICIT type-spec")
			}
		}
		stmt.IsNone = true
		p.vars.implicits = append(p.vars.implicits, stmt)
		p.nextToken()
		stmt.Position = ast.Pos(start, p.current.start)
		return stmt
	}

	// Parse type rules: type-spec (letter-spec-list) [, type-spec (letter-spec-list)]...
	for {
		rule := ast.ImplicitRule{}

		// Parse type spec (INTEGER, REAL, CHARACTER, etc.)
		if !p.current.tok.IsTypeDeclaration() {
			p.addError("expected type specification in IMPLICIT statement")
			break
		}
		tok := p.parseTypeToken()
		// Parse optional KIND or CHARACTER length
		// In IMPLICIT statements, KIND/length must be specified before letter ranges
		// Valid: IMPLICIT REAL*8 (A-H) or IMPLICIT REAL(KIND=8) (A-H)
		// Invalid: IMPLICIT REAL(8) (A-H) - ambiguous, use KIND= form
		var kindOrLen ast.Expression
		if tok == token.CHARACTER {
			// CHARACTER can have length: CHARACTER*10 or CHARACTER(LEN=10)
			if p.consumeIf(token.Asterisk) {
				kindOrLen = p.parseExpression(0)
			} else if p.currentTokenIs(token.LParen) && p.peek.tok == token.LEN {
				kindOrLen = p.parseCharacterLength()
			}
		} else if tok != token.DOUBLEPRECISION {
			// Other types can have KIND with * or (KIND=...)
			if p.consumeIf(token.Asterisk) {
				kindOrLen = p.parseExpression(0)
			} else if p.currentTokenIs(token.LParen) && p.peek.tok == token.KIND {
				kindOrLen = p.parseKindSelector()
			}
		}

		// Create TypeSpec
		rule.Type = ast.TypeSpec{
			Token:     tok,
			Name:      "", // IMPLICIT doesn't use TYPE(typename) syntax
			KindOrLen: kindOrLen,
		}

		// Parse letter ranges: (A-H, O-Z) or (I, J, K-N)
		if !p.expect(token.LParen, "in IMPLICIT statement letter range") {
			break
		}

		// Parse comma-separated letter ranges
		for !p.currentTokenIs(token.RParen) && !p.IsDone() {
			// Parse a single letter or letter range
			if !p.canUseAsIdentifier() {
				p.addError("expected letter in IMPLICIT range")
				break
			}

			startLetter := p.current.lit
			if len(startLetter) != 1 {
				p.addError("IMPLICIT letter range must be a single letter")
				p.nextToken()
				continue
			}
			letterStart := byte(strings.ToUpper(string(startLetter))[0])

			// Validate it's A-Z
			if letterStart < 'A' || letterStart > 'Z' {
				p.addError("IMPLICIT letter must be A-Z")
			}

			p.nextToken()

			// Check for range (A-H) vs single letter (I)
			letterEnd := letterStart
			if p.consumeIf(token.Minus) {
				if !p.canUseAsIdentifier() {
					p.addError("expected letter after '-' in IMPLICIT range")
					break
				}
				endLetter := p.current.lit
				if len(endLetter) != 1 {
					p.addError("IMPLICIT letter range must be a single letter")
				} else {
					letterEnd = byte(strings.ToUpper(string(endLetter))[0])
					if letterEnd < 'A' || letterEnd > 'Z' {
						p.addError("IMPLICIT letter must be A-Z")
					}
					if letterEnd < letterStart {
						p.addError("IMPLICIT letter range end must be >= start")
					}
				}
				p.nextToken()
			}

			rule.LetterRanges = append(rule.LetterRanges, ast.LetterRange{
				Start: letterStart,
				End:   letterEnd,
			})

			if !p.consumeIf(token.Comma) {
				break
			}
		}

		if !p.expect(token.RParen, "closing IMPLICIT letter range") {
			break
		}

		stmt.Rules = append(stmt.Rules, rule)

		// Check for another type spec
		if !p.consumeIf(token.Comma) {
			break
		}
	}

	// Check if IMPLICIT NONE was already declared - no other IMPLICIT statements allowed after it
	for _, impl := range p.vars.implicits {
		if impl.IsNone {
			p.addError("IMPLICIT type-spec not allowed after IMPLICIT NONE")
			break
		}
	}

	p.vars.implicits = append(p.vars.implicits, stmt)

	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseUse parses USE module [, ONLY: list]
func (p *Parser90) parseUse() ast.Statement {
	start := p.current.start
	stmt := &ast.UseStatement{}
	p.expect(token.USE, "")
	if !p.canUseAsIdentifier() {
		p.addError("expected module name after USE")
		return nil
	}
	stmt.ModuleName = string(p.current.lit)
	p.nextToken()
	if p.consumeIf(token.Comma) {
		if p.consumeIf(token.ONLY) {
			if p.expect(token.Colon, "after ONLY") {
				for p.canUseAsIdentifier() {
					stmt.Only = append(stmt.Only, string(p.current.lit))
					p.nextToken()
					if !p.consumeIf(token.Comma) {
						break
					}
				}
			}
		}
	}
	stmt.Position = ast.Pos(start, p.current.start)
	return stmt
}

// parseTypeSpecIntrinsic parses an intrinsic type specification and returns a TypeSpec.
//
// Fortran Spec: intrinsic-type-spec (F90 R502, F77 Table 5)
//
// Syntax:
//   - INTEGER [ kind-selector ]
//   - REAL [ kind-selector ]
//   - DOUBLE PRECISION or DOUBLE COMPLEX
//   - COMPLEX [ kind-selector ]
//   - LOGICAL [ kind-selector ]
//   - CHARACTER [ char-selector ]
//
// Kind selectors:
//   - ( KIND = scalar-int-expr )
//   - ( scalar-int-expr )
//   - * digit-string   (F77 syntax)
//
// Character selectors:
//   - ( LEN = char-length )
//   - ( char-length )
//   - * char-length   (F77 syntax)
//
// Examples:
//   - INTEGER(KIND=8)   → TypeSpec{Token: INTEGER, KindOrLen: 8}
//   - REAL*8            → TypeSpec{Token: REAL, KindOrLen: 8}
//   - CHARACTER(LEN=20) → TypeSpec{Token: CHARACTER, KindOrLen: 20}
//   - DOUBLE PRECISION  → TypeSpec{Token: DOUBLEPRECISION}
func (p *Parser90) parseTypeSpecIntrinsic() (ts ast.TypeSpec) {
	if !p.current.tok.IsTypeIntrinsic() {
		p.addError("expected type instrinsic, got " + p.current.String())
		return ast.TypeSpec{}
	}
	ts.Token = p.parseTypeToken()
	// Parse optional KIND selector or CHARACTER length
	if p.currentTokenIs(token.LParen) || p.currentTokenIs(token.Asterisk) {
		switch ts.Token {
		case token.CHARACTER:
			// CHARACTER uses LEN= not KIND=
			ts.KindOrLen = p.parseCharacterLength()
		case token.INTEGER, token.REAL, token.COMPLEX, token.LOGICAL:
			ts.KindOrLen = p.parseKindSelector()
		default:
			p.addError("unexpected KIND usage for " + ts.Token.String())
		}
	}
	return ts
}

func (p *Parser90) parseTypeToken() (tok token.Token) {
	tok = p.current.tok
	p.nextToken()
	// Handle DOUBLE PRECISION as two tokens → DOUBLEPRECISION
	if tok == token.DOUBLE {
		switch p.current.tok {
		case token.PRECISION:
			tok = token.DOUBLEPRECISION
			p.nextToken()
		case token.COMPLEX:
			tok = token.DOUBLECOMPLEX
			p.nextToken()
		}
	}
	return tok
}

// expectTypeSpec parses a type specification and returns a TypeSpec.
//
// Fortran Spec: type-spec (F90 R501)
//
// Syntax:
//   - intrinsic-type-spec  → handled by expectTypeSpecIntrinsic()
//   - TYPE ( type-name )   → derived type
//
// The type-spec appears in:
//   - Type declaration statements: INTEGER :: x, REAL :: y
//   - FUNCTION result types: REAL FUNCTION foo()
//   - IMPLICIT statements: IMPLICIT REAL (A-H)
//   - Component declarations: TYPE person; INTEGER :: age; END TYPE
//
// Examples:
//   - INTEGER           → TypeSpec{Token: INTEGER}
//   - REAL(KIND=8)      → TypeSpec{Token: REAL, KindOrLen: 8}
//   - TYPE(person)      → TypeSpec{Token: TYPE, Name: "person"}
//   - CHARACTER(LEN=20) → TypeSpec{Token: CHARACTER, KindOrLen: 20}
func (p *Parser90) expectTypeSpec(withAttrs bool) (ts ast.TypeSpec) {
	ts.Token = p.current.tok
	if ts.Token == token.TYPE {
		// Derived type: TYPE(typename)
		p.nextToken()
		if !p.consumeIf(token.LParen) || !p.canUseAsIdentifier() || !p.peekTokenIs(token.RParen) {
			p.addError("expected TYPE(<typename>)")
			return ast.TypeSpec{}
		}
		ts.Name = string(p.current.lit)
		p.nextToken() // consume identifier
		p.nextToken() // consume )
		if withAttrs {
			ts.Attributes = p.parseTypeAttributess()
		}
		return ts // tok=TYPE
	} else {
		ts = p.parseTypeSpecIntrinsic()
	}
	if withAttrs {
		ts.Attributes = p.parseTypeAttributess()
	}
	// Intrinsic type
	return ts
}

// parseTypeDecl parses INTEGER :: x, y or REAL, PARAMETER :: pi = 3.14 or TYPE(typename) :: var
// If paramMap is provided, it will populate type information for any parameters found
func (p *Parser90) parseTypeDecl() ast.Statement {
	start := p.sourcePos()
	stmt := &ast.TypeDeclaration{
		Type:     p.expectTypeSpec(true),
		Position: ast.Pos(start.Pos, p.current.start),
	}
	// F77: INTEGER A, B, C (no ::)
	// F90: INTEGER :: A, B, C (with ::)
	p.consumeIf(token.DoubleColon)

	specFlags := flagsFromTypespec(&stmt.Type)
	// Parse entity list
	// Note: DATA can be used as a variable name here, even though tok.CanBeUsedAsIdentifier() returns false for it
	var entityName string
	for p.consumeIdentifier(&entityName, token.DATA) {
		entFlags := specFlags
		stmt.Entities = append(stmt.Entities, ast.DeclEntity{
			Name:     entityName,
			Type:     &stmt.Type,
			Position: p.currentAstPos(),
		})
		entity := &stmt.Entities[len(stmt.Entities)-1]
		if p.currentTokenIs(token.LParen) {
			entity.ArraySpec = p.parseArraySpec()
			if len(entity.ArraySpec.Bounds) > 0 {
				entFlags |= VFlagDimension
			}
		}
		if p.currentTokenIs(token.Asterisk) {
			// Character length or Kind, old F77
			p.nextToken()
			// Check for assumed-length character: *(*)
			if p.currentTokenIs(token.LParen) && p.peekTokenIs(token.Asterisk) {
				p.nextToken() // consume (
				pos := p.current.start
				p.nextToken() // consume *
				p.expect(token.RParen, "closing ')' after *(*)")
				entity.KindOrLen = &ast.Identifier{Value: "*", Position: ast.Pos(pos, pos+1)}
			} else {
				entity.KindOrLen = p.parseExpression(0, token.Comma, token.Equals, token.PointerAssign)
			}
		}
		switch p.current.tok {
		case token.Comma:
			p.nextToken() // No initializer.
		case token.PointerAssign:
			entFlags |= VFlagPointer
			fallthrough
		case token.Equals:
			p.nextToken()
			if p.currentTokenIs(token.LParen) && p.peekTokenIs(token.Slash) {
				entity.Init = p.parseArrayConstructor()
				entFlags |= VFlagArrayInit
			} else {
				entity.Init = p.parseExpression(0, token.Comma)
			}
		case token.Slash:
			entity.Init = p.parseExpression(0, token.Slash)
		case token.NewLine, token.LineComment:
			// End of type decl, append last declared variable.
		default:
			p.addError("unexpected token in type declaration of " + entityName + ": " + p.current.String())
		}
		// Add entity to statement (now that all fields are populated)
		p.varInit(entityName, entity, entFlags, "")
		// consume identifier separating comma.
		p.consumeIf(token.Comma)
	}
	return stmt
}

func (p *Parser90) currentTokenExpr() *ast.TokenExpr {
	return &ast.TokenExpr{Token: p.current.tok, Position: p.currentAstPos()}
}

func (p *Parser90) parseTypeAttributess() (attrs []ast.TypeAttribute) {
	for p.loopWhile(token.Comma) {
		p.nextToken() // consume Comma.
		if !p.current.tok.IsAttributeKeyword() {
			p.addError("parsing type decl attributes: " + p.current.String())
			break
		}
		attr := ast.TypeAttribute{Token: p.current.tok}
		switch {
		case p.consumeIf(token.INTENT):
			p.expect(token.LParen, "INTENT attribute")
			tok := p.current.tok
			pos := p.currentAstPos()
			p.nextToken()
			if tok == token.IN && p.current.tok == token.OUT {
				tok = token.INOUT // Join IN OUT -> INOUT
				p.nextToken()
			}
			attr.Expr = &ast.TokenExpr{Token: tok, Position: pos}
			p.expect(token.RParen, "INTENT attribute")
		case p.consumeIf(token.DIMENSION):
			attr.Dimension = p.parseArraySpec()
		default:
			if p.current.tok.IsAttributeKeyword() {
				attr.Expr = p.currentTokenExpr()
			} else {
				p.addError("unexpected attribute: " + p.current.String())
			}
			p.nextToken()
		}
		attrs = append(attrs, attr)

	}
	return attrs
}

func (p *Parser90) parseArrayRef() *ast.CallExpr {
	var varname string
	if !p.expectIdentifier(&varname, "array reference variable name") {
		return nil
	}
	stmt := &ast.CallExpr{Name: varname}
	if p.consumeIf(token.LParen) {
		for {
			expr := p.parseExpression(0, token.Comma)
			stmt.Args = append(stmt.Args, expr)
			if !p.consumeIf(token.Comma) {
				break
			}
		}
		p.expect(token.RParen, "closing left parentheses")
	}
	return stmt
}

// parseArraySpec parses array dimension specification from DIMENSION(...) or entity declarator
// Expects current token to be '(' and consumes up to and including ')'
// Supports: (10), (1:10), (:), (*), (10,20), (1:10,1:20), (:,:), etc.
func (p *Parser90) parseArraySpec() *ast.ArraySpec {
	if !p.expect(token.LParen, "ARRAY SPEC") {
		return nil
	}

	spec := &ast.ArraySpec{
		Kind:   ast.ArraySpecExplicit, // Default, may be changed
		Bounds: []ast.ArrayBound{},
	}

	// Track if we see any assumed-shape (:) or assumed-size (*) dimensions
	hasAssumedShape := false
	hasAssumedSize := false
	hasExplicit := false

	// Parse array bounds, stop at RParen or construct endings for safety
	for p.loopUntilEndElseOr(token.RParen) {
		var bound ast.ArrayBound

		// Check for assumed-size: *
		if p.currentTokenIs(token.Asterisk) {
			hasAssumedSize = true
			bound.Lower = nil
			// Represent * as an Identifier for consistency
			bound.Upper = &ast.Identifier{
				Value:    "*",
				Position: ast.Pos(p.current.start, p.current.start+1),
			}
			p.nextToken()
		} else if p.consumeIf(token.Colon) {
			// Assumed-shape: :
			hasAssumedShape = true
			bound.Lower = nil
			bound.Upper = nil
		} else {
			// Parse explicit bound expression
			// parseExpression will stop at :, ,, or ) since they have precedence 0
			lowerExpr := p.parseExpression(0)
			if lowerExpr == nil {
				// Invalid expression
				break
			}

			if p.consumeIf(token.Colon) {
				// This was the lower bound, now get upper bound
				bound.Lower = lowerExpr
				// Parse upper bound expression
				upperExpr := p.parseExpression(0)
				if upperExpr == nil {
					// Invalid expression
					break
				}
				bound.Upper = upperExpr
				hasExplicit = true
			} else {
				// No colon, so this is just upper bound (lower defaults to 1)
				bound.Lower = nil
				bound.Upper = lowerExpr
				hasExplicit = true
			}
		}

		spec.Bounds = append(spec.Bounds, bound)

		// Move past comma if present
		p.consumeIf(token.Comma)
	}

	// Consume closing paren
	p.expect(token.RParen, "closing array spec")

	// Determine the kind based on what we saw
	if hasAssumedSize {
		spec.Kind = ast.ArraySpecAssumedSize
	} else if hasAssumedShape {
		spec.Kind = ast.ArraySpecAssumed
	} else if hasExplicit {
		spec.Kind = ast.ArraySpecExplicit
	}

	return spec
}

// parseCharacterLength parses CHARACTER length specification from (LEN=n), (n), (*), or (LEN=:)
// Expects current token to be '(' and consumes up to and including ')'
// Returns the length specification as an Expression
func (p *Parser90) parseCharacterLength() ast.Expression {
	inParens := false
	if p.consumeIf(token.LParen) {
		p.consumeIf2(token.LEN, token.Equals) // Consume Length prefix.
		inParens = true
	} else if !p.consumeIf(token.Asterisk) {
		p.addError("char length expected '*' or '('")
	}

	// Check for assumed length: CHARACTER(*) or CHARACTER(LEN=*)
	if p.currentTokenIs(token.Asterisk) {
		pos := p.current.start
		p.nextToken() // consume *
		if inParens {
			p.expect(token.RParen, "closing ')'")
		}
		// Return identifier "*" to represent assumed length
		return &ast.Identifier{
			Value:    "*",
			Position: ast.Pos(pos, pos+1),
		}
	}

	// Parse as expression (handles :, integer literals, identifiers, etc.)
	expr := p.parseExpression(0, token.RParen, token.Comma)
	if inParens {
		p.expect(token.RParen, "closing ')'")
	}
	return expr
}

// parseKindSelector parses KIND parameter specification
// Handles: (KIND=expr), (expr), or *N
// Returns the KIND parameter as an Expression, or nil if not present
func (p *Parser90) parseKindSelector() ast.Expression {
	// Handle F77 syntax: *N (e.g., INTEGER*4, REAL*8)
	if p.consumeIf(token.Asterisk) {

		if p.currentTokenIs(token.IntLit) {
			kind := p.parseExpression(0)
			return kind
		}
		// If not followed by integer, this might be something else
		// Back up would be complex, so we'll require proper syntax
		p.addError("expected integer after * in type kind specification")
		return nil
	}

	// Handle F90 syntax: (KIND=expr) or (expr)
	if p.consumeIf(token.LParen) {
		// Check for KIND= prefix
		p.expect2IfFirst(token.KIND, token.Equals, "KIND param")
		// Parse kind expression
		kind := p.parseExpression(0)
		p.expect(token.RParen, "after KIND parameter")
		return kind
	}

	return nil
}

// getOperatorPrecedence returns the precedence level for operators
// Higher number = higher precedence
// Fortran precedence (highest to lowest):
// 10: % (component access)
// 9: ** (exponentiation, right associative)
// 8: unary +, -, .NOT.
// 7: *, /
// 6: binary +, -
// 5: // (string concatenation)
// 4: relational (.EQ., .NE., .LT., .LE., .GT., .GE., ==, /=, <, <=, >, >=)
// 3: .AND.
// 2: .OR.
// 1: .EQV., .NEQV.
func (p *Parser90) getOperatorPrecedence(op token.Token) int {
	switch op {
	case token.Percent:
		return 10
	case token.DoubleStar: // **
		return 9
	case token.Asterisk, token.Slash: // *, /
		return 7
	case token.Plus, token.Minus: // +, -
		return 6
	case token.StringConcat: // //
		return 5
	case token.EQ, token.NE, token.LT, token.LE, token.GT, token.GE: // .EQ., .NE., etc.
		return 4
	case token.EqEq, token.NotEquals, token.Less, token.LessEq, token.Greater, token.GreaterEq: // ==, /=, <, <=, >, >=
		return 4
	case token.AND: // .AND.
		return 3
	case token.OR: // .OR.
		return 2
	case token.EQV, token.NEQV: // .EQV., .NEQV.
		return 1
	default:
		return 0
	}
}

// isRightAssociative returns true for right-associative operators
func (p *Parser90) isRightAssociative(op token.Token) bool {
	return op == token.DoubleStar // ** is right associative
}

// parseExpression parses a Fortran expression using precedence climbing
// minPrec is the minimum precedence level to parse
func (p *Parser90) parseExpression(minPrec int, terminators ...token.Token) ast.Expression {
	// Parse primary expression (literal, identifier, function call, etc.)
	left := p.parsePrimaryExpr()
	if left == nil {
		return nil
	}
	start := left.SourcePos().Start()
	// Precedence climbing while checking for terminator tokens.
	for p.loopUntil(terminators...) {
		// Check if current token is a binary operator
		prec := p.getOperatorPrecedence(p.current.tok)
		if prec == 0 || prec < minPrec {
			break
		}

		// Special case: Check for /) which ends array constructor
		// The / should not be treated as division operator in this context
		if p.currentTokenIs(token.Slash) && p.peekTokenIs(token.RParen) {
			break
		}

		op := p.current.tok
		p.nextToken() // consume operator

		if op == token.Percent {
			if !p.canUseAsIdentifier() {
				p.addError("expected component name after '%'")
				return left
			}
			componentName := string(p.current.lit)
			p.nextToken()
			ca := &ast.ComponentAccess{
				Base:      left,
				Component: componentName,
			}
			// Parse optional subscripts: a%x(i,j) or a%vec(:)
			if p.currentTokenIs(token.LParen) {
				p.nextToken() // consume (
				for !p.currentTokenIs(token.RParen) && !p.IsDone() {
					arg, err := p.parseOneArg()
					if err != nil || arg == nil {
						break
					}
					ca.Args = append(ca.Args, arg)
					if !p.consumeIf(token.Comma) {
						break
					}
				}
				p.expect(token.RParen, "closing ) in component subscript")
			}
			ca.Position = ast.Pos(start, p.current.start)
			left = ca
			continue
		}

		// Determine the minimum precedence for the right side
		nextMinPrec := prec
		if !p.isRightAssociative(op) {
			nextMinPrec = prec + 1
		}

		// Parse right side
		right := p.parseExpression(nextMinPrec, terminators...)
		if right == nil {
			p.addError("expected expression after operator")
			return left
		}

		// Create binary expression node
		left = &ast.BinaryExpr{
			Op:       op,
			Left:     left,
			Right:    right,
			Position: ast.Pos(start, right.SourcePos().End()),
		}
	}

	return left
}

// tryParseImpliedDoLoop attempts to parse an implied DO loop after seeing ( expr ,
// Syntax: ( expression-list, loop-var = start, end [, stride] )
// Returns nil and adds an error if parsing fails
func (p *Parser90) tryParseImpliedDoLoop(startPos int, firstExpr ast.Expression) *ast.ImpliedDoLoop {
	expressions := []ast.Expression{firstExpr}
	start := p.sourcePos()
	// Parse comma-separated expressions until we find identifier =
	for p.loopWhile(token.Comma) {
		p.nextToken() // consume comma

		// Check if this is the loop control part: identifier =
		// Accept any token that can be used as identifier (includes keywords used as variables)
		if p.canUseAsIdentifier() && p.peekTokenIs(token.Equals) {
			// Found the loop variable
			loopVar := string(p.current.lit)
			p.nextToken() // consume identifier
			p.nextToken() // consume =

			// Parse start expression
			start := p.parseExpression(0)
			if start == nil {
				p.addError("expected start expression in implied DO loop")
				return nil
			}

			// Expect comma
			if !p.expect(token.Comma, "after start expression in implied DO loop") {
				return nil
			}

			// Parse end expression
			end := p.parseExpression(0)
			if end == nil {
				p.addError("expected end expression in implied DO loop")
				return nil
			}

			// Check for optional stride
			var stride ast.Expression
			if p.consumeIf(token.Comma) {
				stride = p.parseExpression(0)
				if stride == nil {
					p.addError("expected stride expression in implied DO loop")
					return nil
				}
			}

			// Expect closing paren
			endPos := p.current.start
			if !p.expect(token.RParen, "to close implied DO loop") {
				return nil
			}

			return &ast.ImpliedDoLoop{
				Expressions: expressions,
				LoopVar:     loopVar,
				Start:       start,
				End:         end,
				Stride:      stride,
				Position:    ast.Pos(startPos, endPos),
			}
		}

		// Not the loop control yet, parse another expression
		expr := p.parseExpression(0)
		if expr == nil {
			p.addError("expected expression in implied DO loop")
			return nil
		}
		expressions = append(expressions, expr)
	}

	// If we got here, we didn't find the identifier = pattern
	p.addErrorWithPos(start, "do loop start")
	p.addError("failed to parse implied DO loop: expected 'identifier =' pattern")
	return nil
}

// parsePrimaryExpr parses primary expressions: literals, identifiers, function calls,
// array references, and parenthesized expressions
func (p *Parser90) parsePrimaryExpr() ast.Expression {
	startPos := p.current.start

	// Handle unary operators: +, -, .NOT.
	if p.current.tok == token.Plus || p.current.tok == token.Minus || p.current.tok == token.NOT {
		op := p.current.tok
		p.nextToken()
		// .NOT. has lower precedence than relational ops: .NOT.x.LT.y = .NOT.(x.LT.y)
		// +/- bind tighter (only consume ** at prec 9)
		operandPrec := 8
		if op == token.NOT {
			operandPrec = 4 // consume through relational (4) but not .AND.(3)/.OR.(2)
		}
		operand := p.parseExpression(operandPrec)
		if operand == nil {
			p.addError("expected expression after unary operator")
			return nil
		}
		return &ast.UnaryExpr{
			Op:       op,
			Operand:  operand,
			Position: ast.Pos(startPos, operand.SourcePos().End()),
		}
	}

	// Handle parenthesized expressions and array constructors
	if p.currentTokenIs(token.LParen) {
		if p.peekTokenIs(token.Slash) {
			return p.parseArrayConstructor()
		}
		p.nextToken() // consume (
		expr := p.parseExpression(0)
		if expr == nil {
			p.addError("expected expression after '('")
			return nil
		}

		// Check for comma - could be implied DO loop or complex literal (real, imag)
		if p.currentTokenIs(token.Comma) {
			// Implied DO loop requires pattern: (expr, ..., identifier = start, end)
			// Complex literal is: (real, imag) where imag is followed by )
			// Peek ahead to check: if next is a simple expression followed by ), it's complex
			if p.peekTokenIs(token.IntLit) || p.peekTokenIs(token.FloatLit) || p.peekTokenIs(token.Minus) || p.peekTokenIs(token.Plus) {
				// Could be complex literal - check if we have simple (expr, expr) pattern
				// by looking for identifier = pattern which would indicate implied DO
				if !p.looksLikeImpliedDoLoop() {
					// Parse as complex literal
					p.nextToken() // consume comma
					imagExpr := p.parseExpression(0)
					if imagExpr == nil {
						p.addError("expected expression after comma in parenthesized expression")
						return nil
					}
					endPos := p.current.start
					if !p.expect(token.RParen, "after complex literal") {
						return nil
					}
					return &ast.ParenExpr{
						Expr:     expr,
						Imag:     imagExpr,
						Position: ast.Pos(startPos, endPos),
					}
				}
			}
			// Try to parse as implied DO loop
			if impliedDo := p.tryParseImpliedDoLoop(startPos, expr); impliedDo != nil {
				return impliedDo
			}
			// If tryParseImpliedDoLoop returns nil, it has already added an error
			return nil
		}

		endPos := p.current.start
		if !p.expect(token.RParen, "after expression") {
			return nil
		}
		return &ast.ParenExpr{
			Expr:     expr,
			Position: ast.Pos(startPos, endPos),
		}
	}
	pos := ast.Pos(startPos, p.current.start+len(p.current.lit))
	if flit := p.tryParseFloatLit(); flit != nil {
		return flit
	}
	if ilit := p.tryParseIntLit(); ilit != nil {
		return ilit
	}
	// Handle string literals
	if p.currentTokenIs(token.StringLit) {
		lit := &ast.StringLiteral{
			Value:    string(p.current.lit),
			Position: pos,
		}
		p.nextToken()
		return lit
	}

	// Handle logical literals
	if p.currentTokenIs(token.TRUE) {
		lit := &ast.LogicalLiteral{
			Value:    true,
			Position: pos,
		}
		p.nextToken()
		return lit
	}
	if p.currentTokenIs(token.FALSE) {
		lit := &ast.LogicalLiteral{
			Value:    false,
			Position: pos,
		}
		p.nextToken()
		return lit
	}

	// Handle identifiers, function calls, and array references
	if p.current.tok.IsKeyword() || p.canUseAsIdentifier() {
		name := string(p.current.lit)
		endPos := p.current.start + len(p.current.lit)
		p.nextToken()

		// Check for function call or array reference/section
		// Can be chained for substring notation: array(i)(1:5)
		var result ast.Expression
		for p.consumeIf(token.LParen) {

			// Parse argument/subscript list
			args, err := parseCommaSeparatedList(p, token.RParen, p.parseOneArg)
			if err != nil {
				p.addError(err.Error())
			}

			if p.currentTokenIs(token.RParen) {
				endPos = p.current.start
				p.nextToken() // consume )
			}

			if result == nil {
				// Always create CallExpr - transpiler will disambiguate
				// between array access, function calls, and intrinsics.
				result = &ast.CallExpr{
					Name:     name,
					Args:     args,
					Position: ast.Pos(startPos, endPos),
				}
			} else {
				// Chained subscript/substring: arr(i)(2:3), func()(i), etc.
				// SecondaryAccess handles single expression (range or index).
				// For multiple subscripts, this is a parsing error.
				if prevCall, ok := result.(*ast.CallExpr); ok && len(args) == 1 {
					prevCall.SecondaryAccess = args[0]
				} else if prevCall, ok := result.(*ast.CallExpr); ok {
					// Multiple chained subscripts - create new CallExpr wrapper
					// This handles cases like func()(i,j) where result is from function call
					result = &ast.CallExpr{
						Args:     args,
						Position: ast.Pos(startPos, endPos),
					}
					// TODO: handle properly if needed
					_ = prevCall
				}
			}
		}

		if result != nil {
			return result
		}

		// Just an identifier
		return &ast.Identifier{
			Value:    name,
			Position: ast.Pos(startPos, endPos),
		}
	}

	return nil
}

func (p *Parser90) tryParseFloatLit() *ast.RealLiteral {
	if !p.currentTokenIs(token.FloatLit) {
		return nil
	}
	rawStr := string(p.current.lit)
	value, err := p.parseFloatValue(rawStr)
	if err != nil {
		p.addError(fmt.Sprintf("invalid float literal: %s", rawStr))
		value = 0.0
	}
	lit := &ast.RealLiteral{
		Value:    value,
		Raw:      rawStr,
		Position: p.currentAstPos(),
	}
	p.nextToken()
	return lit
}

func (p *Parser90) tryParseIntLit() *ast.IntegerLiteral {
	if !p.currentTokenIs(token.IntLit) {
		return nil
	}
	rawStr := string(p.current.lit)
	value, err := parseInt64(rawStr)
	if err != nil {
		p.addError(fmt.Sprintf("invalid integer literal: %s", rawStr))
		value = 0
	}
	lit := &ast.IntegerLiteral{
		Value:    value,
		Raw:      rawStr,
		Position: p.currentAstPos(),
	}
	p.nextToken()
	return lit
}

// parseOneArg parses an argument in a parentheses preceding an identifier.
// since identifier could be a function or an array we must handle both cases here.
// Also handles keyword arguments like KIND=value in intrinsic function calls.
func (p *Parser90) parseOneArg() (ast.Expression, error) {
	start := p.sourcePos()
	// Handle array slice syntax: : or start:end or start:end:stride
	if p.consumeIf(token.Colon) {
		// Lone colon means "all elements" (:)
		rangeExpr := &ast.RangeExpr{
			Start:    nil, // implicit start
			End:      nil, // implicit end
			Position: ast.Pos(start.Pos, p.current.start),
		}
		// Check for stride (:stride)
		if !p.currentTokenIs(token.Comma) && !p.currentTokenIs(token.RParen) {
			rangeExpr.End = p.parseExpression(0) // This is actually end, not stride
		}
		return rangeExpr, nil
	}

	// Check for keyword argument (name=value) in function calls
	// Look ahead to see if this is a keyword argument
	// TODO: can this be simplified?
	if p.canUseAsIdentifier() && p.peekTokenIs(token.Equals) {
		// Parse keyword argument manually: name=value
		keywordStart := p.current.start
		keywordName := &ast.Identifier{
			Value:    string(p.current.lit),
			Position: ast.Pos(p.current.start, p.current.start+len(p.current.lit)),
		}
		p.nextToken() // consume keyword name
		p.nextToken() // consume =

		// Parse the value expression
		value := p.parseExpression(0)
		if value == nil {
			return nil, fmt.Errorf("expected expression after '=' in keyword argument")
		}

		// Return as a BinaryExpr with = operator to represent name=value
		return &ast.BinaryExpr{
			Op:       token.Equals,
			Left:     keywordName,
			Right:    value,
			Position: ast.Pos(keywordStart, value.SourcePos().End()),
		}, nil
	}

	arg := p.parseExpression(0)
	if arg == nil {
		return nil, fmt.Errorf("expected expression in argument list")
	}

	// Check for range syntax: expr:expr or expr:expr:stride
	if p.consumeIf(token.Colon) {
		rangeExpr := &ast.RangeExpr{
			Start:    arg,
			Position: ast.Pos(start.Pos, p.current.start),
		}
		// Parse end expression (optional)
		if !p.currentTokenIs(token.Comma) && !p.currentTokenIs(token.RParen) && !p.currentTokenIs(token.Colon) {
			rangeExpr.End = p.parseExpression(0)
		}
		// Check for stride (F90 feature: start:end:stride)
		if p.consumeIf(token.Colon) {
			if !p.currentTokenIs(token.Comma) && !p.currentTokenIs(token.RParen) {
				rangeExpr.Stride = p.parseExpression(0)
			}
		}
		return rangeExpr, nil
	}

	return arg, nil
}

// parseArrayConstructor parses an array constructor
func (p *Parser90) parseArrayConstructor() ast.Expression {
	start := p.current.start
	stmt := &ast.ArrayConstructor{}
	p.expect(token.LParen, "array start '\\('") // consume (
	p.expect(token.Slash, "array start '\\('")  // consume /

	// Parse array elements until we find /
	// Use parseCommaSeparatedList with / as terminator
	parseOneElement := func() (ast.Expression, error) {
		p.skipNewlinesAndComments()
		val := p.parseExpression(0)
		if val == nil {
			return nil, fmt.Errorf("expected expression in array constructor")
		}
		return val, nil
	}

	values, err := parseCommaSeparatedList(p, token.Slash, parseOneElement)
	if err != nil {
		p.addError(err.Error())
	}
	stmt.Values = values

	if p.expect(token.Slash, "end of array constructor") {
		p.expect(token.RParen, "after '/' in array constructor")
	}

	stmt.Position = ast.Pos(start, p.current.start+len(p.current.lit))
	return stmt
}

func (p *Parser90) consumeEndLabelIfPresent(tgt *string, endConstruct token.Token, continueMatch string) bool {
	// Check for labeled END IF/ENDIF  or END DO/ENDDO/CONTINUE
	isLabelled := p.currentTokenIs(token.IntLit) &&
		(p.peek.tok == token.END || // 10 END DO or 10 END IF
			p.peek.tok == endConstruct.EndConstructComposite() || // 10 ENDDO or 10 ENDIF
			endConstruct == token.DO && p.peek.tok == token.CONTINUE && continueMatch == string(p.current.lit)) // 10 CONTINUE
	if isLabelled {
		// Capture the end label and advance past it
		*tgt = string(p.current.lit)
		if p.peek.tok != token.CONTINUE {
			p.nextToken() // we only consume END labels. CONTINUE statements are parsed as part of AST with their label.
		}
		if p.current.tok == token.END && p.peek.tok != endConstruct {
			p.addError("consumed label " + *tgt + " which did not correspond to " + endConstruct.String() + " construct")
			*tgt = ""
		}
		return true
	}
	return false
}

// parseCommaSeparatedList parses items separated by commas until terminator
// parser is a function that parses one item
func parseCommaSeparatedList[T any](p *Parser90, terminator token.Token, parser func() (T, error)) ([]T, error) {
	var items []T
	for p.loopUntilEndElseOr(terminator) {
		item, err := parser()
		if err != nil {
			return items, err
		}
		items = append(items, item)
		if p.currentTokenIs(token.Comma) {
			p.nextToken()
		} else if !p.currentTokenIs(terminator) {
			return items, fmt.Errorf("expected ',' or '%s'", terminator)
		}
	}
	return items, nil
}

// parseInt64 parses a Fortran integer literal string to int64
// Handles decimal integers with optional kind suffix (e.g., 123_8, 1_INT32)
func parseInt64(s string) (int64, error) {
	// Handle kind suffix: 123_8 or 1_INT32
	// Split on underscore and take only the numeric part
	if idx := strings.Index(s, "_"); idx >= 0 {
		s = s[:idx] // Take only the part before underscore
	}

	// Parse as base 10 integer
	return strconv.ParseInt(s, 10, 64)
}

// parseCommonStmt parses a COMMON statement
// Precondition: current token is COMMON
// Examples:
//
//	COMMON /block1/ a, b, c
//	COMMON // x, y, z          (blank COMMON)
//	COMMON /name/ var1, var2
func (p *Parser90) parseCommonStmt() ast.Statement {
	startPos := p.current.start
	p.expect(token.COMMON, "")

	stmt := &ast.CommonStmt{}

	// Check for named COMMON: /name/ or blank COMMON: //
	// Note: // is tokenized as StringConcat, not two separate slashes
	if p.consumeIf(token.StringConcat) {
		// Blank COMMON: //
	} else if p.consumeIf(token.Slash) {
		// Named COMMON: /blockname/
		if !p.expectIdentifier(&stmt.BlockName, "common identifier", token.DATA) || // Parse block name
			!p.expect(token.Slash, "closing COMMON block name") { // Consume closing slash
			return nil
		}
	}

	// Parse comma-separated variable list
	var commonVar string
	for p.consumeIdentifier(&commonVar) {
		stmt.Variables = append(stmt.Variables, commonVar)

		// Check for array specification: var(dims)
		// COMMON blocks can contain arrays with dimension specs
		var arraySpec *ast.ArraySpec
		if p.currentTokenIs(token.LParen) {
			// Parse array dimensions
			arraySpec = p.parseArraySpec()
		}
		stmt.ArraySpecs = append(stmt.ArraySpecs, arraySpec)

		// Initialize variable with array spec if present
		var decl *ast.DeclEntity
		flags := VFlagCommon
		if arraySpec != nil {
			decl = &ast.DeclEntity{
				Name:      commonVar,
				ArraySpec: arraySpec,
			}
			flags |= VFlagDimension
		}
		p.varInit(commonVar, decl, flags, stmt.BlockName)

		// Check for comma (more variables) or end of statement
		if !p.consumeIf(token.Comma) {
			break
		}
	}

	stmt.Position = ast.Pos(startPos, p.current.start)
	return stmt
}

// parseNamelistStmt parses a NAMELIST statement
// Precondition: current token is NAMELIST
// Examples:
//
//	NAMELIST /NLIST/ A, B, C
//	NAMELIST /INPUT/ x, y, /OUTPUT/ result
func (p *Parser90) parseNamelistStmt() ast.Statement {
	startPos := p.current.start
	p.expect(token.NAMELIST, "")

	stmt := &ast.NamelistStmt{}

	// Parse one or more namelist groups: /name/ var-list
	for p.currentTokenIs(token.Slash) {
		p.nextToken() // consume opening /

		var grp ast.NamelistGroup
		if !p.expectIdentifier(&grp.Name, "namelist group name") {
			return nil
		}
		if !p.expect(token.Slash, "closing / after namelist group name") {
			return nil
		}

		// Parse comma-separated variable list until next / or end of statement
		var varName string
		for p.consumeIdentifier(&varName) {
			grp.Variables = append(grp.Variables, varName)
			// Check for comma (more variables) or end of group
			if !p.consumeIf(token.Comma) {
				break
			}
			// If next token is /, we're starting a new group
			if p.currentTokenIs(token.Slash) {
				break
			}
		}

		stmt.Groups = append(stmt.Groups, grp)

		// Register group in parser state (append to existing if same name)
		if existing := p.vars.Namelist(grp.Name); existing != nil {
			existing.Variables = append(existing.Variables, grp.Variables...)
		} else {
			p.vars.namelists = append(p.vars.namelists, grp)
		}
	}

	stmt.Position = ast.Pos(startPos, p.current.start)
	return stmt
}

// parseDimensionStmt parses a DIMENSION statement
// Precondition: current token is DIMENSION
// Example: DIMENSION AA(100), BB(10,20), CC(5)
func (p *Parser90) parseDimensionStmt() ast.Statement {
	startPos := p.current.start
	p.expect(token.DIMENSION, "")

	stmt := &ast.DimensionStmt{}
	// Parse comma-separated variable list with array specs
	for p.canUseAsIdentifier() {
		varName := string(p.current.lit)
		stmt.Variables = append(stmt.Variables, varName)
		p.nextToken()

		// Array specification is required for DIMENSION statement
		if p.currentTokenIs(token.LParen) {
			arraySpec := p.parseArraySpec()
			stmt.ArraySpecs = append(stmt.ArraySpecs, arraySpec)

			// Register the variable as an array so parser recognizes it later
			// Check if variable already exists (might have prior type declaration)
			vi := p.varSGet(varName)
			if vi == nil {
				// Note we don't set Type here- in any case will be set in implicit resolving.
				decl := &ast.DeclEntity{
					Name:      varName,
					ArraySpec: arraySpec,
				}
				// VFlagImplicit ensures the variable will be declared in generated code
				p.varInit(varName, decl, VFlagDimension|VFlagImplicit, "")
			} else {
				// Variable exists, update with dimension info
				if vi.decl != nil {
					vi.decl.ArraySpec = arraySpec
				} else {
					vi.decl = &ast.DeclEntity{
						Name:      varName,
						ArraySpec: arraySpec,
					}
				}
				vi.flags |= VFlagDimension
			}
		} else {
			p.addError("expected array dimension specification after variable in DIMENSION statement")
		}

		// Check for comma (more variables) or end of statement
		if !p.consumeIf(token.Comma) {
			break
		}
	}

	stmt.Position = ast.Pos(startPos, p.current.start)
	return stmt
}

// parseEquivalenceStmt parses an EQUIVALENCE statement
// Precondition: current token is EQUIVALENCE
// Example: EQUIVALENCE (A, B, C), (X, Y)
func (p *Parser90) parseEquivalenceStmt() ast.Statement {
	startPos := p.current.start
	p.nextToken() // consume EQUIVALENCE

	stmt := &ast.EquivalenceStmt{}

	// Parse comma-separated equivalence sets: (var1, var2, ...), (var3, var4, ...), ...
	for {
		// Expect opening parenthesis for equivalence set
		if !p.expect(token.LParen, "open EQUIVALENCE set") {
			break
		}
		// Parse variable list within this set
		var set []ast.CallExpr
		for {
			// Parse variable (can be simple identifier or array reference)
			ref := p.parseArrayRef()
			if ref == nil {
				break
			}
			// Flag the variable as participating in EQUIVALENCE
			if vi := p.varSGet(ref.Name); vi != nil {
				vi.flags |= VFlagEquivalenced
			} else {
				p.varInit(ref.Name, nil, VFlagEquivalenced, "")
			}
			set = append(set, *ref)
			// Check for comma (more variables) or closing paren
			if !p.consumeIf(token.Comma) {
				break
			}
		}
		// Add this set to the statement
		if len(set) > 0 {
			stmt.Sets = append(stmt.Sets, set)
		}
		// Expect closing parenthesis
		if !p.expect(token.RParen, "closing parenthesis in EQUIVALENCE set") {
			break
		}
		// Check for comma (more sets) or end of statement
		if !p.consumeIf(token.Comma) {
			break
		}
	}
	stmt.Position = ast.Pos(startPos, p.current.start)
	return stmt
}

// parseExternalStmt parses an EXTERNAL statement
// Precondition: current token is EXTERNAL
// Example: EXTERNAL mysub, myfunc
func (p *Parser90) parseExternalStmt() ast.Statement {
	startPos := p.current.start
	p.nextToken() // consume EXTERNAL

	stmt := &ast.ExternalStmt{}

	// Parse comma-separated list of procedure names
	for p.canUseAsIdentifier() {
		name := string(p.current.lit)
		stmt.Names = append(stmt.Names, name)
		p.nextToken()

		// Check for comma (more names) or end of statement
		if !p.consumeIf(token.Comma) {
			break
		}
	}

	stmt.Position = ast.Pos(startPos, p.current.start)
	return stmt
}

// parseIntrinsicStmt parses an INTRINSIC statement
// Precondition: current token is INTRINSIC
// Example: INTRINSIC sin, cos, sqrt
func (p *Parser90) parseIntrinsicStmt() ast.Statement {
	startPos := p.current.start
	p.nextToken() // consume INTRINSIC

	stmt := &ast.IntrinsicStmt{}

	// Parse comma-separated list of function names
	for p.canUseAsIdentifier() {
		name := string(p.current.lit)
		stmt.Names = append(stmt.Names, name)
		p.nextToken()

		// Check for comma (more names) or end of statement
		if !p.consumeIf(token.Comma) {
			break
		}
	}

	stmt.Position = ast.Pos(startPos, p.current.start)
	return stmt
}

// parseParameterStmt parses an F77 standalone PARAMETER statement
// Precondition: current token is PARAMETER
// Syntax: PARAMETER (name=value, name2=value2, ...)
// Example: PARAMETER (NUMET=20, NUMGRP=4)
func (p *Parser90) parseParameterStmt() ast.Statement {
	start := p.sourcePos()
	if !p.expect(token.PARAMETER, "") ||
		!p.expect(token.LParen, "PARAMETER statement") { // Expect opening parenthesis
		return nil
	}
	stmt := &ast.ParameterStmt{}
	// Parse comma-separated list of name=value pairs
	// Each parameter is stored as a variable via varInit
	for p.loopUntil(token.RParen) {
		var name string
		declStart := p.current.start
		if !p.expectIdentifier(&name, "PARAMETER name") || !p.expect(token.Equals, "PARAMETER assignment") {
			break
		}
		initVal := p.parseExpression(0, token.Comma, token.RParen)
		declEnd := p.current.start
		stmt.Decls = append(stmt.Decls, ast.DeclEntity{
			Name:     name,
			Init:     initVal,
			Position: ast.Pos(declStart, declEnd),
		})
		vi := p.varInit(name, nil, VFlagConstantParameter, "")
		if vi.decl == nil {
			vi.decl = &stmt.Decls[len(stmt.Decls)-1]
		} else if vi.decl.Init == nil {
			vi.decl.Init = stmt.Decls[len(stmt.Decls)-1].Init
		}
		if !p.consumeIf(token.Comma) {
			break
		}
	}
	p.expect(token.RParen, "closing PARAMETER statement")
	stmt.Position = ast.Pos(start.Pos, p.current.start)
	return stmt
}

// parsePointerCrayStmt parses a Cray-style POINTER statement (Fortran 77 extension)
// Precondition: current token is POINTER
// Syntax: POINTER (pointer_var, pointee), (ptr2, pointee2), ...
// Example: POINTER (NPAA,AA(1)), (NPII,II(1)), (NPLL,LL(1))
func (p *Parser90) parsePointerCrayStmt() ast.Statement {
	startPos := p.current.start
	p.expect(token.POINTER, "")

	stmt := &ast.PointerCrayStmt{}

	// Parse comma-separated list of (pointer_var, pointee) pairs
	for {
		var ptrVar, pointeeVar string
		// Parse pointer variable name
		ok := p.expect(token.LParen, "opening cray POINTER statement")
		ok = ok && p.expectIdentifier(&ptrVar, "POINTER cray pointer variable")
		ok = ok && p.expect(token.Comma, "comma after pointer variable")
		ok = ok && p.expectIdentifier(&pointeeVar, "POINTER cray pointee name")
		// Parse optional array spec for pointee
		var arraySpec *ast.ArraySpec
		if p.currentTokenIs(token.LParen) {
			arraySpec = p.parseArraySpec()
			ok = ok && arraySpec != nil
		}
		ok = ok && p.expect(token.RParen, "close cray POINTER statement")
		// Add the pointer pair to statement
		stmt.Pointers = append(stmt.Pointers, ast.PointerCrayPair{
			PointerVar:       ptrVar,
			Pointee:          pointeeVar,
			PointeeArraySpec: arraySpec,
		})
		p.varInit(ptrVar, nil, VFlagPointer, pointeeVar)
		// Create partial DeclEntity with ArraySpec if present.
		// Type will be filled in by resolveImplicitTypes.
		// TODO: what? is this really necessary?
		var pointeeDecl *ast.DeclEntity
		pointeeFlags := VFlagPointee
		if arraySpec != nil {
			pointeeDecl = &ast.DeclEntity{
				Name:      pointeeVar,
				ArraySpec: arraySpec,
			}
			pointeeFlags |= VFlagDimension
		}
		p.varInit(pointeeVar, pointeeDecl, pointeeFlags, "")
		// Check for comma (more pairs) or end of statement
		if !ok || !p.consumeIf(token.Comma) {
			break
		}
	}
	stmt.Position = ast.Pos(startPos, p.current.start)
	return stmt
}

func (p *Parser90) parseFloatValue(v string) (float64, error) {
	// Parse FORTRAN 77 and Fortran 90 formatted float literal to float64 value.
	// Handles:
	// - D/d exponent notation (double precision): 1.0D0, 2.5D-3
	// - Q/q exponent notation (quad precision): 1.0Q0
	// - Kind suffixes: 1.5_8, 2.0_REAL64
	// - Standard E/e notation: 1.5E3, 2.0e-5

	// Strip kind suffix: 1.5_8 → 1.5, 2.0_REAL64 → 2.0
	if idx := strings.Index(v, "_"); idx >= 0 {
		v = v[:idx]
	}

	// Replace D/d/Q/q exponent notation with e notation
	v = strings.ReplaceAll(v, "D", "e")
	v = strings.ReplaceAll(v, "d", "e")
	v = strings.ReplaceAll(v, "Q", "e")
	v = strings.ReplaceAll(v, "q", "e")

	return strconv.ParseFloat(v, 64)
}

// toTypeAttributes converts a slice of tokens (e.g., RECURSIVE, PURE, ELEMENTAL)
// to a slice of TypeAttributes for use in Unit.ResultType.Attributes.
func toTypeAttributes(tokens []token.Token) []ast.TypeAttribute {
	attrs := make([]ast.TypeAttribute, len(tokens))
	for i, tok := range tokens {
		attrs[i] = ast.TypeAttribute{Token: tok}
	}
	return attrs
}
