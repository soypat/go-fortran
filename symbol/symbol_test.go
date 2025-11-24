package symbol

import (
	"testing"

	"github.com/soypat/go-fortran/token"
)

// TestNewSymbolTable verifies that a new symbol table is initialized correctly
func TestNewSymbolTable(t *testing.T) {
	st := NewSymbolTable()

	if st == nil {
		t.Fatal("NewSymbolTable returned nil")
	}

	if st.GlobalScope() == nil {
		t.Fatal("GlobalScope is nil")
	}

	if st.CurrentScope() != st.GlobalScope() {
		t.Error("CurrentScope should initially be GlobalScope")
	}

	if st.GlobalScope().Type() != ScopeGlobal {
		t.Errorf("GlobalScope type: expected %v, got %v", ScopeGlobal, st.GlobalScope().Type())
	}

	if st.GlobalScope().Parent() != nil {
		t.Error("GlobalScope should have nil parent")
	}

	if st.GlobalScope().Symbols() == nil {
		t.Error("GlobalScope.Symbols() map is nil")
	}

	// Check that registries are initialized (these are unexported, so just verify non-nil via API)
	if st.Intrinsic("SIN") == nil {
		t.Error("Intrinsics database not loaded")
	}
}

// TestDefaultImplicitRules verifies the default F77 implicit typing rules
func TestDefaultImplicitRules(t *testing.T) {
	rules := defaultImplicitRules()

	if rules == nil {
		t.Fatal("defaultImplicitRules returned nil")
	}

	if rules.IsNone {
		t.Error("Default rules should not have IsNone set")
	}

	// Test I-N are INTEGER
	for letter := 'I'; letter <= 'N'; letter++ {
		idx := letter - 'A'
		if rules.LetterTypes[idx] != "INTEGER" {
			t.Errorf("Letter %c: expected INTEGER, got %q", letter, rules.LetterTypes[idx])
		}
	}

	// Test A-H are REAL
	for letter := 'A'; letter <= 'H'; letter++ {
		idx := letter - 'A'
		if rules.LetterTypes[idx] != "REAL" {
			t.Errorf("Letter %c: expected REAL, got %q", letter, rules.LetterTypes[idx])
		}
	}

	// Test O-Z are REAL
	for letter := 'O'; letter <= 'Z'; letter++ {
		idx := letter - 'A'
		if rules.LetterTypes[idx] != "REAL" {
			t.Errorf("Letter %c: expected REAL, got %q", letter, rules.LetterTypes[idx])
		}
	}
}

// TestImplicitRulesCopy verifies that Copy creates an independent copy
func TestImplicitRulesCopy(t *testing.T) {
	original := defaultImplicitRules()
	copy := original.Copy()

	if copy == nil {
		t.Fatal("Copy returned nil")
	}

	// Verify values match
	if copy.IsNone != original.IsNone {
		t.Error("IsNone not copied correctly")
	}

	for i := 0; i < 26; i++ {
		if copy.LetterTypes[i] != original.LetterTypes[i] {
			t.Errorf("LetterTypes[%d]: expected %q, got %q",
				i, original.LetterTypes[i], copy.LetterTypes[i])
		}
		if copy.LetterKinds[i] != original.LetterKinds[i] {
			t.Errorf("LetterKinds[%d]: expected %d, got %d",
				i, original.LetterKinds[i], copy.LetterKinds[i])
		}
	}

	// Verify independence: modifying copy doesn't affect original
	copy.IsNone = true
	copy.LetterTypes[0] = "MODIFIED"

	if original.IsNone {
		t.Error("Modifying copy affected original IsNone")
	}
	if original.LetterTypes[0] == "MODIFIED" {
		t.Error("Modifying copy affected original LetterTypes")
	}
}

// TestNormalizeCase verifies case-insensitive identifier normalization
func TestNormalizeCase(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"abc", "ABC"},
		{"ABC", "ABC"},
		{"AbC", "ABC"},
		{"x", "X"},
		{"VeryLongIdentifier", "VERYLONGIDENTIFIER"},
		{"i", "I"},
		{"INTEGER", "INTEGER"},
	}

	for _, tt := range tests {
		result := normalizeCase(tt.input)
		if result != tt.expected {
			t.Errorf("normalizeCase(%q): expected %q, got %q", tt.input, tt.expected, result)
		}
	}
}

// TestEnterExitScope verifies scope nesting operations
func TestEnterExitScope(t *testing.T) {
	st := NewSymbolTable()

	// Initially in global scope
	if st.CurrentScope() != st.GlobalScope() {
		t.Fatal("Should start in global scope")
	}

	// Enter a program scope
	progScope := st.EnterScope(nil, ScopeProgram)
	if progScope == nil {
		t.Fatal("EnterScope returned nil")
	}

	if st.CurrentScope() != progScope {
		t.Error("CurrentScope should be the new program scope")
	}

	if progScope.Parent() != st.GlobalScope() {
		t.Error("Program scope parent should be global scope")
	}

	if progScope.Type() != ScopeProgram {
		t.Errorf("Scope type: expected %v, got %v", ScopeProgram, progScope.Type())
	}

	// Verify implicit rules were inherited
	if progScope.Implicit() == nil {
		t.Fatal("Implicit rules not inherited")
	}

	// Verify it's a copy, not same instance
	if progScope.Implicit() == st.GlobalScope().Implicit() {
		t.Error("Implicit rules should be copied, not shared")
	}

	// Enter a procedure scope
	procScope := st.EnterScope(nil, ScopeProcedure)
	if st.CurrentScope() != procScope {
		t.Error("CurrentScope should be procedure scope")
	}

	if procScope.Parent() != progScope {
		t.Error("Procedure scope parent should be program scope")
	}

	// Exit back to program scope
	st.ExitScope()
	if st.CurrentScope() != progScope {
		t.Error("After ExitScope, should be back in program scope")
	}

	// Exit back to global scope
	st.ExitScope()
	if st.CurrentScope() != st.GlobalScope() {
		t.Error("After ExitScope, should be back in global scope")
	}

	// Verify children were registered
	if len(st.GlobalScope().Children()) != 1 {
		t.Errorf("GlobalScope should have 1 child, got %d", len(st.GlobalScope().Children()))
	}

	if st.GlobalScope().Children()[0] != progScope {
		t.Error("GlobalScope child should be program scope")
	}

	if len(progScope.Children()) != 1 {
		t.Errorf("Program scope should have 1 child, got %d", len(progScope.Children()))
	}

	if progScope.Children()[0] != procScope {
		t.Error("Program scope child should be procedure scope")
	}
}

// TestDefineAndLookup verifies symbol definition and lookup
func TestDefineAndLookup(t *testing.T) {
	st := NewSymbolTable()

	// Define a symbol in global scope
	sym1 := NewSymbol("globalVar", SymVariable)
	sym1.SetType(&ResolvedType{BaseType: "INTEGER"})

	err := st.CurrentScope().Define(sym1)
	if err != nil {
		t.Fatalf("Define failed: %v", err)
	}

	// Lookup should find it (case-insensitive)
	found := st.CurrentScope().Lookup("globalVar")
	if found == nil {
		t.Fatal("Lookup failed to find globalVar")
	}
	if found != sym1 {
		t.Error("Lookup returned wrong symbol")
	}

	// Case-insensitive lookup
	found = st.CurrentScope().Lookup("GLOBALVAR")
	if found != sym1 {
		t.Error("Case-insensitive lookup failed")
	}

	found = st.CurrentScope().Lookup("GlobalVar")
	if found != sym1 {
		t.Error("Mixed-case lookup failed")
	}

	// Enter nested scope
	st.EnterScope(nil, ScopeProgram)

	// Should still find global symbol
	found = st.CurrentScope().Lookup("globalVar")
	if found != sym1 {
		t.Error("Should find global symbol from nested scope")
	}

	// Define local symbol
	sym2 := NewSymbol("localVar", SymVariable)
	sym2.SetType(&ResolvedType{BaseType: "REAL"})

	err = st.CurrentScope().Define(sym2)
	if err != nil {
		t.Fatalf("Define failed: %v", err)
	}

	// Should find local symbol
	found = st.CurrentScope().Lookup("localVar")
	if found != sym2 {
		t.Error("Should find local symbol in current scope")
	}

	// Exit scope
	st.ExitScope()

	// Local symbol should not be found from global scope
	found = st.CurrentScope().Lookup("localVar")
	if found != nil {
		t.Error("Should not find local symbol from parent scope")
	}

	// Global symbol should still be found
	found = st.CurrentScope().Lookup("globalVar")
	if found != sym1 {
		t.Error("Should still find global symbol")
	}
}

// TestLookupLocal verifies local-only symbol lookup
func TestLookupLocal(t *testing.T) {
	st := NewSymbolTable()

	// Define global symbol
	globalSym := NewSymbol("x", SymVariable)
	globalSym.SetType(&ResolvedType{BaseType: "INTEGER"})
	st.CurrentScope().Define(globalSym)

	// Enter nested scope
	st.EnterScope(nil, ScopeProgram)

	// LookupLocal should NOT find global symbol
	found := st.CurrentScope().LookupLocal("x")
	if found != nil {
		t.Error("LookupLocal should not find symbol in parent scope")
	}

	// Define local symbol with same name
	localSym := NewSymbol("x", SymVariable)
	localSym.SetType(&ResolvedType{BaseType: "REAL"})
	st.CurrentScope().Define(localSym)

	// LookupLocal should find local symbol
	found = st.CurrentScope().LookupLocal("x")
	if found != localSym {
		t.Error("LookupLocal should find local symbol")
	}

	// Regular Lookup should also find local symbol (shadowing)
	found = st.CurrentScope().Lookup("x")
	if found != localSym {
		t.Error("Lookup should find local symbol (shadowing global)")
	}
}

// TestSymbolKindString verifies SymbolKind.String()
func TestSymbolKindString(t *testing.T) {
	tests := []struct {
		kind     SymbolKind
		expected string
	}{
		{SymUnknown, "Unknown"},
		{SymVariable, "Variable"},
		{SymParameter, "Parameter"},
		{SymFunction, "Function"},
		{SymSubroutine, "Subroutine"},
		{SymModule, "Module"},
		{SymProgram, "Program"},
		{SymCommonBlock, "CommonBlock"},
		{SymDerivedType, "DerivedType"},
		{SymIntrinsic, "Intrinsic"},
		{SymExternal, "External"},
	}

	for _, tt := range tests {
		result := tt.kind.String()
		if result != tt.expected {
			t.Errorf("SymbolKind(%d).String(): expected %q, got %q", tt.kind, tt.expected, result)
		}
	}
}

// TestScopeTypeString verifies ScopeType.String()
func TestScopeTypeString(t *testing.T) {
	tests := []struct {
		scopeType ScopeType
		expected  string
	}{
		{ScopeGlobal, "Global"},
		{ScopeProgram, "Program"},
		{ScopeProcedure, "Procedure"},
		{ScopeModule, "Module"},
		{ScopeBlock, "Block"},
	}

	for _, tt := range tests {
		result := tt.scopeType.String()
		if result != tt.expected {
			t.Errorf("ScopeType(%d).String(): expected %q, got %q", tt.scopeType, tt.expected, result)
		}
	}
}

// TestLoadIntrinsics verifies that intrinsic functions are loaded
func TestLoadIntrinsics(t *testing.T) {
	intrinsics := loadIntrinsics()

	if len(intrinsics) == 0 {
		t.Fatal("No intrinsics loaded")
	}

	// Test some common intrinsics
	testCases := []struct {
		name       string
		kind       IntrinsicKind
		returnType string
	}{
		{"SIN", IntrinsicFunction, "REAL"},
		{"COS", IntrinsicFunction, "REAL"},
		{"EXP", IntrinsicFunction, "REAL"},
		{"LOG", IntrinsicFunction, "REAL"},
		{"SQRT", IntrinsicFunction, "REAL"},
		{"ABS", IntrinsicFunction, "REAL"},
		{"INT", IntrinsicFunction, "INTEGER"},
		{"REAL", IntrinsicFunction, "REAL"},
		{"SIZE", IntrinsicFunction, "INTEGER"},
		{"LEN", IntrinsicFunction, "INTEGER"},
		{"MIN", IntrinsicFunction, "REAL"},
		{"MAX", IntrinsicFunction, "REAL"},
	}

	for _, tc := range testCases {
		intrinsic, ok := intrinsics[tc.name]
		if !ok {
			t.Errorf("Intrinsic %s not found", tc.name)
			continue
		}

		if intrinsic.Name() != tc.name {
			t.Errorf("%s: expected name %q, got %q", tc.name, tc.name, intrinsic.Name())
		}

		if intrinsic.Kind() != tc.kind {
			t.Errorf("%s: expected kind %v, got %v", tc.name, tc.kind, intrinsic.Kind())
		}

		if intrinsic.ReturnType() != tc.returnType {
			t.Errorf("%s: expected return type %q, got %q", tc.name, tc.returnType, intrinsic.ReturnType())
		}

		if intrinsic.GoMapping() == "" {
			t.Errorf("%s: GoMapping is empty", tc.name)
		}
	}
}

// TestSymbolAttributes verifies symbol attribute storage
func TestSymbolAttributes(t *testing.T) {
	sym := NewSymbol("x", SymVariable)
	sym.SetType(&ResolvedType{BaseType: "INTEGER"})
	sym.SetAttributes([]token.Token{token.SAVE, token.PARAMETER})

	if len(sym.Attributes()) != 2 {
		t.Errorf("Expected 2 attributes, got %d", len(sym.Attributes()))
	}

	if sym.Attributes()[0] != token.SAVE {
		t.Errorf("Attribute[0]: expected SAVE, got %v", sym.Attributes()[0])
	}

	if sym.Attributes()[1] != token.PARAMETER {
		t.Errorf("Attribute[1]: expected PARAMETER, got %v", sym.Attributes()[1])
	}
}
