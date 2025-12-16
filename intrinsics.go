package fortran

import (
	"fmt"
	"go/ast"
	"go/token"
	"math"
	"strings"

	f90 "github.com/soypat/go-fortran/ast"
	f90token "github.com/soypat/go-fortran/token"
)

type intrinsicFn struct {
	redirectTo f90token.Intrinsic // signal this intrinsic is effectively replaced by another.
	calls      []intrinsicCall
	isVariadic bool
	f1         func(float64) float64
	f2         func(float64, float64) float64
}

type intrinsicCall struct {
	args []*Varinfo
	// if is nil return type decided by args[0] on instantiation.
	returnType *Varinfo
	// Rules for deciding how to use this prioritized in order of appearance:
	//  - If args[0] is array or character we know this is a method.
	//  - If first letter is upper case then is function in intrinsic package.
	//  - Else is a built in in Go.
	methodOrCall string
}

func makeCall(methodOrGoCall string, returnType *Varinfo, args ...*Varinfo) intrinsicCall {
	return intrinsicCall{
		methodOrCall: methodOrGoCall,
		args:         args,
		returnType:   returnType,
	}
}

// isAllCaps returns true if the string consists only of uppercase letters.
func isAllCaps(s string) bool {
	for _, c := range s {
		if c < 'A' || c > 'Z' {
			return false
		}
	}
	return len(s) > 0
}

// getIntrinsicV2 looks up a V2 intrinsic by token.
// Returns nil if not found or if redirectTo chain leads to undefined.
func getIntrinsicV2(lookup f90token.Intrinsic) *intrinsicFn {
	if int(lookup) >= len(intrinsicsv2) {
		return nil
	}
	fn := &intrinsicsv2[lookup]
	// Follow redirect chain
	for fn.redirectTo != 0 {
		if int(fn.redirectTo) >= len(intrinsicsv2) {
			return nil
		}
		fn = &intrinsicsv2[fn.redirectTo]
	}
	if len(fn.calls) == 0 {
		return nil // Not defined in V2
	}
	return fn
}

// findBestCall finds the best matching intrinsicCall for the given arguments.
// Returns nil if no match found.
func (fn *intrinsicFn) findBestCall(nargs int) *intrinsicCall {
	for i := range fn.calls {
		c := &fn.calls[i]
		if fn.isVariadic {
			if nargs >= len(c.args) {
				return c
			}
		} else if len(c.args) == nargs {
			return c
		}
	}
	return nil
}

// findBestCallWithTypes finds the best matching intrinsicCall considering argument types.
func (fn *intrinsicFn) findBestCallWithTypes(argTypes []*Varinfo) *intrinsicCall {
	nargs := len(argTypes)
	var genericMatch *intrinsicCall
	for i := range fn.calls {
		c := &fn.calls[i]
		if fn.isVariadic {
			if nargs < len(c.args) {
				continue
			}
		} else if len(c.args) != nargs {
			continue
		}
		// Check if all argument types are compatible
		match := true
		isGeneric := false
		for j := range c.args {
			argIdx := j
			if argIdx >= nargs {
				break
			}
			paramType := c.args[j]
			argType := argTypes[argIdx]
			if !typeCompatible(paramType, argType) {
				match = false
				break
			}
			if isGenericVarinfo(paramType) {
				isGeneric = true
			}
		}
		if match {
			if !isGeneric {
				return c // Prefer exact matches over generic matches
			}
			if genericMatch == nil {
				genericMatch = c
			}
		}
	}
	return genericMatch
}

// intrinsicExprV2 transforms an intrinsic call using the V2 system.
func (tg *ToGo) intrinsicExprV2(vitgt *Varinfo, fn *intrinsicFn, call *intrinsicCall, args ...f90.Expression) (*ast.CallExpr, *Varinfo, error) {
	// Transform arguments
	var gargs []ast.Expr
	var firstArgType *Varinfo
	for i := range args {
		paramIdx := i
		if paramIdx >= len(call.args) {
			paramIdx = 0 // For variadic, use first param type
		}
		expr, argType, err := tg.transformExpression(call.args[paramIdx], args[i])
		if err != nil {
			return nil, nil, err
		}
		if i == 0 {
			firstArgType = argType
		}
		gargs = append(gargs, expr)
	}

	// Determine return type
	resultType := call.returnType
	if resultType == nil {
		resultType = firstArgType
	}

	// Determine function expression based on methodOrCall convention
	var funcExpr ast.Expr
	name := call.methodOrCall

	// Check if this is a method call
	// Methods are on array/char types and have CamelCase names (e.g., "Size", "Len")
	// Functions have SCREAMING_CASE names (e.g., "DOT_PRODUCT", "MIN", "ALL")
	isMethod := len(call.args) > 0 && (call.args[0].IsArray() || call.args[0].IsChar()) &&
		len(name) > 0 && name[0] >= 'A' && name[0] <= 'Z' && !strings.Contains(name, "_") &&
		!isAllCaps(name)
	if isMethod {
		// Method call: gargs[0].methodName(gargs[1:]...)
		methodCall := &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   gargs[0],
				Sel: ast.NewIdent(name),
			},
			Args: gargs[1:],
		}
		// Wrap method result to match Fortran return types
		// Go methods return native types (int, CharacterArray) but Fortran expects specific types
		if resultType != nil {
			switch resultType.typeToken() {
			case f90token.INTEGER:
				// Go method returns int, Fortran expects INTEGER (int32)
				return &ast.CallExpr{Fun: ast.NewIdent("int32"), Args: []ast.Expr{methodCall}}, resultType, nil
			case f90token.CHARACTER:
				// Go method returns CharacterArray, convert to string via .String()
				return &ast.CallExpr{
					Fun: &ast.SelectorExpr{X: methodCall, Sel: ast.NewIdent("String")},
				}, resultType, nil
			}
		}
		return methodCall, resultType, nil
	}

	// Check naming convention
	if len(name) > 0 && name[0] >= 'A' && name[0] <= 'Z' {
		// Uppercase: intrinsic.NAME or intrinsic.NAME[T]
		sel := &ast.SelectorExpr{X: _astIntrinsic, Sel: ast.NewIdent(name)}
		// Add type parameter only if first param is generic (function needs type instantiation)
		isGenericFn := len(call.args) > 0 && isGenericVarinfo(call.args[0])
		if isGenericFn {
			var goType ast.Expr
			if firstArgType != nil && !isGenericVarinfo(firstArgType) {
				// Use actual argument type
				goType = goTypeBasic(firstArgType.typeToken(), 0)
			} else if vitgt != nil && !isGenericVarinfo(vitgt) && typeCompatible(call.args[0], vitgt) {
				// Use target type when argument is generic (e.g., literals) and types are compatible
				// This handles cases like SQRT(3.0D0) where literal could be float32 or float64
				goType = goTypeBasic(vitgt.typeToken(), 0)
			} else {
				// Use default type for generic params: int32 for integers, float32 for floats
				if call.args[0] == _tgtGenericInt {
					goType = ast.NewIdent("int32")
				} else {
					goType = ast.NewIdent("float32")
				}
			}
			funcExpr = &ast.IndexExpr{X: sel, Index: goType}
		} else {
			funcExpr = sel
		}
	} else {
		// Lowercase: Go builtin (int32, float32, etc.)
		funcExpr = ast.NewIdent(name)
	}

	return &ast.CallExpr{
		Fun:  funcExpr,
		Args: gargs,
	}, resultType, nil
}

var intrinsicsv2 = []intrinsicFn{
	f90token.IntrinsicREAL: {
		calls: []intrinsicCall{
			makeCall("float32", _tgtFloat32, _tgtGenericInt),
			makeCall("float32", _tgtFloat32, _tgtGenericFloat),
			makeCall("REALPART", _tgtFloat32, _tgtComplex64), // REAL(complex) returns real part
		},
	},
	f90token.IntrinsicINT: {
		calls: []intrinsicCall{
			makeCall("int32", _tgtInt32, _tgtGenericInt),
			makeCall("int32", _tgtInt32, _tgtGenericFloat),
		},
	},
	f90token.IntrinsicIFIX: {redirectTo: f90token.IntrinsicINT},
	f90token.IntrinsicMAX: {
		isVariadic: true,
		calls: []intrinsicCall{
			makeCall("MAX", nil, _tgtGenericFloat),
			makeCall("MAX", nil, _tgtGenericInt),
		},
	},
	f90token.IntrinsicCMPLX: {
		calls: []intrinsicCall{
			// CMPLX(x) - single arg, imaginary = 0
			makeCall("CMPLX", _tgtComplex64, _tgtGenericFloat),
			makeCall("CMPLX", _tgtComplex64, _tgtGenericInt),
			// CMPLX(x, y) - two args, real and imaginary parts
			makeCall("CMPLX2", _tgtComplex64, _tgtGenericFloat, _tgtGenericFloat),
			makeCall("CMPLX2", _tgtComplex64, _tgtGenericInt, _tgtGenericInt),
		},
	},
	f90token.IntrinsicSQRT: {
		calls: []intrinsicCall{makeCall("SQRT", nil, _tgtGenericFloat)},
		f1:    math.Sqrt,
	},
	f90token.IntrinsicDSQRT: {redirectTo: f90token.IntrinsicSQRT},
	f90token.IntrinsicAIMAG: {
		calls: []intrinsicCall{makeCall("AIMAG", _tgtFloat32, _tgtComplex64)},
	},
	f90token.IntrinsicDIMAG: {
		calls: []intrinsicCall{makeCall("DIMAG", _tgtFloat64, _tgtComplex128)},
	},
	f90token.IntrinsicCDABS: {
		calls: []intrinsicCall{makeCall("CDABS", _tgtFloat64, _tgtComplex128)},
	},
	f90token.IntrinsicDREAL: {
		calls: []intrinsicCall{makeCall("DREALPART", _tgtFloat64, _tgtComplex128)},
	},
	f90token.IntrinsicDCMPLX: {
		calls: []intrinsicCall{
			// DCMPLX(x) - single arg, imaginary = 0
			makeCall("DCMPLX", _tgtComplex128, _tgtGenericFloat),
			makeCall("DCMPLX", _tgtComplex128, _tgtGenericInt),
			// DCMPLX(x, y) - two args, real and imaginary parts
			makeCall("DCMPLX2", _tgtComplex128, _tgtGenericFloat, _tgtGenericFloat),
			makeCall("DCMPLX2", _tgtComplex128, _tgtGenericInt, _tgtGenericInt),
		},
	},

	// Type conversions
	f90token.IntrinsicDBLE: {
		calls: []intrinsicCall{
			makeCall("float64", _tgtFloat64, _tgtGenericInt),
			makeCall("float64", _tgtFloat64, _tgtGenericFloat),
		},
	},

	// Trig intrinsics
	f90token.IntrinsicSIN: {
		calls: []intrinsicCall{makeCall("SIN", nil, _tgtGenericFloat)},
		f1:    math.Sin,
	},
	f90token.IntrinsicDSIN: {redirectTo: f90token.IntrinsicSIN},
	f90token.IntrinsicCOS: {
		calls: []intrinsicCall{makeCall("COS", nil, _tgtGenericFloat)},
		f1:    math.Cos,
	},
	f90token.IntrinsicDCOS: {redirectTo: f90token.IntrinsicCOS},
	f90token.IntrinsicTAN: {
		calls: []intrinsicCall{makeCall("TAN", nil, _tgtGenericFloat)},
		f1:    math.Tan,
	},
	f90token.IntrinsicDTAN: {redirectTo: f90token.IntrinsicTAN},
	f90token.IntrinsicASIN: {
		calls: []intrinsicCall{makeCall("ASIN", nil, _tgtGenericFloat)},
		f1:    math.Asin,
	},
	f90token.IntrinsicDASIN: {redirectTo: f90token.IntrinsicASIN},
	f90token.IntrinsicACOS: {
		calls: []intrinsicCall{makeCall("ACOS", nil, _tgtGenericFloat)},
		f1:    math.Acos,
	},
	f90token.IntrinsicDACOS: {redirectTo: f90token.IntrinsicACOS},
	f90token.IntrinsicATAN: {
		calls: []intrinsicCall{makeCall("ATAN", nil, _tgtGenericFloat)},
		f1:    math.Atan,
	},
	f90token.IntrinsicDATAN: {redirectTo: f90token.IntrinsicATAN},
	f90token.IntrinsicATAN2: {
		calls: []intrinsicCall{makeCall("ATAN2", nil, _tgtGenericFloat, _tgtGenericFloat)},
		f2:    math.Atan2,
	},
	f90token.IntrinsicDATAN2: {redirectTo: f90token.IntrinsicATAN2},

	// Hyperbolic intrinsics
	f90token.IntrinsicSINH: {
		calls: []intrinsicCall{makeCall("SINH", nil, _tgtGenericFloat)},
		f1:    math.Sinh,
	},
	f90token.IntrinsicDSINH: {redirectTo: f90token.IntrinsicSINH},
	f90token.IntrinsicCOSH: {
		calls: []intrinsicCall{makeCall("COSH", nil, _tgtGenericFloat)},
		f1:    math.Cosh,
	},
	f90token.IntrinsicDCOSH: {redirectTo: f90token.IntrinsicCOSH},
	f90token.IntrinsicTANH: {
		calls: []intrinsicCall{makeCall("TANH", nil, _tgtGenericFloat)},
		f1:    math.Tanh,
	},
	f90token.IntrinsicDTANH: {redirectTo: f90token.IntrinsicTANH},

	// Exponential and logarithmic
	f90token.IntrinsicEXP: {
		calls: []intrinsicCall{makeCall("EXP", nil, _tgtGenericFloat)},
		f1:    math.Exp,
	},
	f90token.IntrinsicDEXP: {redirectTo: f90token.IntrinsicEXP},
	f90token.IntrinsicLOG: {
		calls: []intrinsicCall{makeCall("LOG", nil, _tgtGenericFloat)},
		f1:    math.Log,
	},
	f90token.IntrinsicDLOG: {redirectTo: f90token.IntrinsicLOG},
	f90token.IntrinsicLOG10: {
		calls: []intrinsicCall{makeCall("LOG10", nil, _tgtGenericFloat)},
		f1:    math.Log10,
	},
	f90token.IntrinsicDLOG10: {redirectTo: f90token.IntrinsicLOG10},

	// Truncation and rounding
	f90token.IntrinsicFLOOR: {
		calls: []intrinsicCall{makeCall("FLOOR", nil, _tgtGenericFloat)},
		f1:    math.Floor,
	},
	f90token.IntrinsicCEILING: {
		calls: []intrinsicCall{makeCall("CEILING", nil, _tgtGenericFloat)},
		f1:    math.Ceil,
	},
	f90token.IntrinsicAINT: {
		calls: []intrinsicCall{makeCall("AINT", nil, _tgtGenericFloat)},
		f1:    math.Trunc,
	},
	f90token.IntrinsicANINT: {
		calls: []intrinsicCall{makeCall("ANINT", nil, _tgtGenericFloat)},
		f1:    math.Round,
	},
	f90token.IntrinsicNINT: {
		calls: []intrinsicCall{makeCall("NINT", _tgtInt32, _tgtGenericFloat)},
	},
	f90token.IntrinsicDNINT:  {redirectTo: f90token.IntrinsicANINT},
	f90token.IntrinsicIDNINT: {redirectTo: f90token.IntrinsicNINT},

	// Absolute value and sign
	f90token.IntrinsicABS: {
		calls: []intrinsicCall{
			makeCall("ABS", nil, _tgtGenericFloat),
			makeCall("CABS", _tgtFloat32, _tgtComplex64),   // ABS(complex) → float32
			makeCall("CDABS", _tgtFloat64, _tgtComplex128), // ABS(double complex) → float64
		},
		f1: math.Abs,
	},
	f90token.IntrinsicDABS: {redirectTo: f90token.IntrinsicABS},
	f90token.IntrinsicIABS: {
		calls: []intrinsicCall{makeCall("IABS", nil, _tgtGenericInt)},
	},
	f90token.IntrinsicCABS: {
		calls: []intrinsicCall{makeCall("CABS", _tgtFloat32, _tgtComplex64)},
	},
	f90token.IntrinsicSIGN: {
		calls: []intrinsicCall{makeCall("SIGN", nil, _tgtGenericFloat, _tgtGenericFloat)},
	},
	f90token.IntrinsicISIGN: {
		calls: []intrinsicCall{makeCall("SIGN", nil, _tgtGenericInt, _tgtGenericInt)},
	},
	f90token.IntrinsicDSIGN: {redirectTo: f90token.IntrinsicSIGN},

	// Modulo and remainder
	f90token.IntrinsicMOD: {
		calls: []intrinsicCall{
			makeCall("MOD", nil, _tgtGenericInt, _tgtGenericInt),
			makeCall("MODREAL", nil, _tgtGenericFloat, _tgtGenericFloat),
		},
	},
	f90token.IntrinsicDIM: {
		calls: []intrinsicCall{makeCall("DIM", nil, _tgtGenericFloat, _tgtGenericFloat)},
	},
	f90token.IntrinsicIDIM: {
		calls: []intrinsicCall{makeCall("DIM", nil, _tgtGenericInt, _tgtGenericInt)},
	},
	f90token.IntrinsicDDIM: {redirectTo: f90token.IntrinsicDIM},
	f90token.IntrinsicDPROD: {
		calls: []intrinsicCall{makeCall("DPROD", _tgtFloat64, _tgtFloat32, _tgtFloat32)},
	},

	// Variadic min
	f90token.IntrinsicMIN: {
		isVariadic: true,
		calls: []intrinsicCall{
			makeCall("MIN", nil, _tgtGenericFloat),
			makeCall("MIN", nil, _tgtGenericInt),
		},
	},
	f90token.IntrinsicMAX0: {
		isVariadic: true,
		calls:      []intrinsicCall{makeCall("MAX", nil, _tgtGenericInt)},
	},
	f90token.IntrinsicMIN0: {
		isVariadic: true,
		calls:      []intrinsicCall{makeCall("MIN", nil, _tgtGenericInt)},
	},
	f90token.IntrinsicAMAX1: {redirectTo: f90token.IntrinsicMAX},
	f90token.IntrinsicAMIN1: {redirectTo: f90token.IntrinsicMIN},
	f90token.IntrinsicDMAX1: {redirectTo: f90token.IntrinsicMAX},
	f90token.IntrinsicDMIN1: {redirectTo: f90token.IntrinsicMIN},

	// Character methods
	f90token.IntrinsicLEN: {
		calls: []intrinsicCall{makeCall("Len", _tgtInt32, _tgtChar)},
	},
	f90token.IntrinsicLEN_TRIM: {
		calls: []intrinsicCall{makeCall("LenTrim", _tgtInt32, _tgtChar)},
	},
	f90token.IntrinsicTRIM: {
		calls: []intrinsicCall{makeCall("Trim", _tgtChar, _tgtChar)},
	},
	f90token.IntrinsicADJUSTL: {
		calls: []intrinsicCall{makeCall("AdjustL", _tgtChar, _tgtChar)},
	},
	f90token.IntrinsicADJUSTR: {
		calls: []intrinsicCall{makeCall("AdjustR", _tgtChar, _tgtChar)},
	},
	f90token.IntrinsicINDEX: {
		calls: []intrinsicCall{makeCall("Index", _tgtInt32, _tgtChar, _tgtChar)},
	},

	// Array methods
	f90token.IntrinsicSIZE: {
		calls: []intrinsicCall{
			makeCall("Size", _tgtInt32, _tgtArrayAny),
			makeCall("SizeDim", _tgtInt32, _tgtArrayAny, _tgtInt32),
		},
	},
	f90token.IntrinsicSHAPE: {
		calls: []intrinsicCall{makeCall("Shape", nil, _tgtArrayAny)},
	},
	f90token.IntrinsicLBOUND: {
		calls: []intrinsicCall{makeCall("LowerDim", _tgtInt32, _tgtArrayAny, _tgtInt32)},
	},
	f90token.IntrinsicUBOUND: {
		calls: []intrinsicCall{makeCall("UpperDim", _tgtInt32, _tgtArrayAny, _tgtInt32)},
	},

	// Array reduction intrinsics
	f90token.IntrinsicDOT_PRODUCT: {
		calls: []intrinsicCall{makeCall("DOT_PRODUCT", nil, _tgtArray(f90token.FloatLit), _tgtArray(f90token.FloatLit))},
	},
	f90token.IntrinsicALL: {
		calls: []intrinsicCall{makeCall("ALL", _tgtBool, _tgtArray(f90token.LOGICAL))},
	},
}

func defaultVarinfo(tok f90token.Token) *Varinfo {
	return &Varinfo{
		_varname: fmt.Sprintf("<default %s varinfo>", tok.String()),
		decl:     &f90.DeclEntity{Type: &f90.TypeSpec{Token: tok}},
	}
}

var (
	_astFalse        = ast.NewIdent("false")
	_astTrue         = ast.NewIdent("true")
	_astSet          = ast.NewIdent("Set")
	_astOne          = &ast.BasicLit{Kind: token.INT, Value: "1"}
	_astZero         = &ast.BasicLit{Kind: token.INT, Value: "0"}
	_tgtInt32        = defaultVarinfo(f90token.INTEGER)
	_tgtInt          = defaultVarinfo(f90token.INTEGER)
	_tgtFloat32      = defaultVarinfo(f90token.REAL)
	_tgtBool         = defaultVarinfo(f90token.LOGICAL)
	_tgtFloat64      = defaultVarinfo(f90token.DOUBLEPRECISION)
	_tgtChar         = defaultVarinfo(f90token.CHARACTER)
	_tgtStringLit    = defaultVarinfo(f90token.StringLit)
	_tgtGenericFloat = defaultVarinfo(f90token.FloatLit)
	_tgtGenericInt   = defaultVarinfo(f90token.IntLit)
	_tgtComplex64    = defaultVarinfo(f90token.COMPLEX)
	_tgtComplex128   = defaultVarinfo(f90token.DOUBLECOMPLEX)
	_tgtArrayAny     = _tgtArray(f90token.DIMENSION)
)

func _tgtArray(elem f90token.Token) *Varinfo {
	di := defaultVarinfo(elem)
	di.flags |= VFlagDimension
	return di
}

func isGenericVarinfo(vi *Varinfo) bool {
	return vi == _tgtGenericFloat || vi == _tgtGenericInt
}
