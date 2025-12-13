package token

import "strings"

type Intrinsic uint

const (
	intrinsicUndefined Intrinsic = 0 // undefined
)

// Fortran 66 function Intrinsics (FORTRAN IV compatible)
const (
	fortran66Start Intrinsic = iota + intrinsicUndefined + 1
	// Mathematical functions
	IntrinsicABS   // ABS
	IntrinsicMOD   // MOD
	IntrinsicSIGN  // SIGN
	IntrinsicDIM   // DIM
	IntrinsicDPROD // DPROD
	// Trigonometric
	IntrinsicSIN   // SIN
	IntrinsicCOS   // COS
	IntrinsicTAN   // TAN
	IntrinsicASIN  // ASIN
	IntrinsicACOS  // ACOS
	IntrinsicATAN  // ATAN
	IntrinsicATAN2 // ATAN2
	// Hyperbolic
	IntrinsicSINH // SINH
	IntrinsicCOSH // COSH
	IntrinsicTANH // TANH
	// Exponential and logarithmic
	IntrinsicEXP   // EXP
	IntrinsicLOG   // LOG
	IntrinsicLOG10 // LOG10
	IntrinsicSQRT  // SQRT
	// Type conversion
	IntrinsicINT   // INT
	IntrinsicREAL  // REAL
	IntrinsicDBLE  // DBLE
	IntrinsicCMPLX // CMPLX
	IntrinsicFLOAT // FLOAT
	IntrinsicIFIX  // IFIX
	IntrinsicSNGL  // SNGL
	// Truncation and rounding
	IntrinsicAINT  // AINT
	IntrinsicANINT // ANINT
	IntrinsicNINT  // NINT
	// Min/Max
	IntrinsicMAX   // MAX
	IntrinsicMIN   // MIN
	IntrinsicMAX0  // MAX0
	IntrinsicMAX1  // MAX1
	IntrinsicMIN0  // MIN0
	IntrinsicMIN1  // MIN1
	IntrinsicAMAX0 // AMAX0
	IntrinsicAMAX1 // AMAX1
	IntrinsicAMIN0 // AMIN0
	IntrinsicAMIN1 // AMIN1
	IntrinsicDMAX1 // DMAX1
	IntrinsicDMIN1 // DMIN1
	// Complex conjugate and imaginary part
	IntrinsicCONJG // CONJG
	IntrinsicAIMAG // AIMAG
	// Specific names for type variants
	IntrinsicIABS   // IABS
	IntrinsicDABS   // DABS
	IntrinsicCABS   // CABS
	IntrinsicDSIN   // DSIN
	IntrinsicDCOS   // DCOS
	IntrinsicDTAN   // DTAN
	IntrinsicDASIN  // DASIN
	IntrinsicDACOS  // DACOS
	IntrinsicDATAN  // DATAN
	IntrinsicDATAN2 // DATAN2
	IntrinsicDSINH  // DSINH
	IntrinsicDCOSH  // DCOSH
	IntrinsicDTANH  // DTANH
	IntrinsicDEXP   // DEXP
	IntrinsicDLOG   // DLOG
	IntrinsicDLOG10 // DLOG10
	IntrinsicDSQRT  // DSQRT
	IntrinsicCSIN   // CSIN
	IntrinsicCCOS   // CCOS
	IntrinsicCEXP   // CEXP
	IntrinsicCLOG   // CLOG
	IntrinsicCSQRT  // CSQRT
	IntrinsicIDIM   // IDIM
	IntrinsicDDIM   // DDIM
	IntrinsicIDINT  // IDINT
	IntrinsicISIGN  // ISIGN
	IntrinsicDSIGN  // DSIGN
	IntrinsicDNINT  // DNINT
	IntrinsicIDNINT // IDNINT
	IntrinsicDIMAG  // DIMAG
	IntrinsicDCONJG // DCONJG
	fortran66End
)

// Fortran 77 function Intrinsics (added string handling)
const (
	fortran77Start Intrinsic = iota + fortran66End
	// Character functions
	IntrinsicCHAR  // CHAR
	IntrinsicICHAR // ICHAR
	IntrinsicLEN   // LEN
	IntrinsicINDEX // INDEX
	// Lexical comparison
	IntrinsicLGE // LGE
	IntrinsicLGT // LGT
	IntrinsicLLE // LLE
	IntrinsicLLT // LLT
	fortran77End
)

// Fortran 90 function Intrinsics
const (
	fortran90Start Intrinsic = iota + fortran77End
	// Array reduction functions
	IntrinsicSUM     // SUM
	IntrinsicPRODUCT // PRODUCT
	IntrinsicMAXVAL  // MAXVAL
	IntrinsicMINVAL  // MINVAL
	IntrinsicALL     // ALL
	IntrinsicANY     // ANY
	IntrinsicCOUNT   // COUNT
	// Array inquiry functions
	IntrinsicSIZE      // SIZE
	IntrinsicSHAPE     // SHAPE
	IntrinsicLBOUND    // LBOUND
	IntrinsicUBOUND    // UBOUND
	IntrinsicALLOCATED // ALLOCATED
	// Array construction functions
	IntrinsicRESHAPE // RESHAPE
	IntrinsicSPREAD  // SPREAD
	IntrinsicPACK    // PACK
	IntrinsicUNPACK  // UNPACK
	IntrinsicMERGE   // MERGE
	// Array manipulation functions
	IntrinsicTRANSPOSE // TRANSPOSE
	IntrinsicCSHIFT    // CSHIFT
	IntrinsicEOSHIFT   // EOSHIFT
	// Array location functions
	IntrinsicMAXLOC // MAXLOC
	IntrinsicMINLOC // MINLOC
	// Matrix functions
	IntrinsicMATMUL      // MATMUL
	IntrinsicDOT_PRODUCT // DOT_PRODUCT
	// Bit manipulation functions
	IntrinsicIAND   // IAND
	IntrinsicIOR    // IOR
	IntrinsicIEOR   // IEOR
	IntrinsicNOT    // NOT
	IntrinsicBTEST  // BTEST
	IntrinsicIBSET  // IBSET
	IntrinsicIBCLR  // IBCLR
	IntrinsicIBITS  // IBITS
	IntrinsicISHFT  // ISHFT
	IntrinsicISHFTC // ISHFTC
	IntrinsicMVBITS // MVBITS
	// Floating point inquiry
	IntrinsicHUGE         // HUGE
	IntrinsicTINY         // TINY
	IntrinsicEPSILON      // EPSILON
	IntrinsicPRECISION    // PRECISION
	IntrinsicRANGE        // RANGE
	IntrinsicRADIX        // RADIX
	IntrinsicDIGITS       // DIGITS
	IntrinsicBIT_SIZE     // BIT_SIZE
	IntrinsicEXPONENT     // EXPONENT
	IntrinsicFRACTION     // FRACTION
	IntrinsicNEAREST      // NEAREST
	IntrinsicRRSPACING    // RRSPACING
	IntrinsicSPACING      // SPACING
	IntrinsicSCALE        // SCALE
	IntrinsicSET_EXPONENT // SET_EXPONENT
	// Kind functions
	IntrinsicKIND               // KIND
	IntrinsicSELECTED_INT_KIND  // SELECTED_INT_KIND
	IntrinsicSELECTED_REAL_KIND // SELECTED_REAL_KIND
	// String functions
	IntrinsicLEN_TRIM // LEN_TRIM
	IntrinsicTRIM     // TRIM
	IntrinsicADJUSTL  // ADJUSTL
	IntrinsicADJUSTR  // ADJUSTR
	IntrinsicREPEAT   // REPEAT
	IntrinsicSCAN     // SCAN
	IntrinsicVERIFY   // VERIFY
	// Pointer inquiry
	IntrinsicASSOCIATED // ASSOCIATED
	// Argument presence
	IntrinsicPRESENT // PRESENT
	// Transfer and conversion
	IntrinsicTRANSFER // TRANSFER
	IntrinsicLOGICAL  // LOGICAL
	// Miscellaneous
	IntrinsicCEILING // CEILING
	IntrinsicFLOOR   // FLOOR
	IntrinsicMODULO  // MODULO
	IntrinsicNULL    // NULL
	fortran90End
)

// Fortran 95 Intrinsics
const (
	fortran95Start    Intrinsic = iota + fortran90End
	IntrinsicCPU_TIME           // CPU_TIME
	fortran95End
)

// Fortran 2003 Intrinsics
const (
	fortran2003Start                  Intrinsic = iota + fortran95End
	IntrinsicMOVE_ALLOC                         // MOVE_ALLOC
	IntrinsicIS_IOSTAT_END                      // IS_IOSTAT_END
	IntrinsicIS_IOSTAT_EOR                      // IS_IOSTAT_EOR
	IntrinsicNEW_LINE                           // NEW_LINE
	IntrinsicCOMMAND_ARGUMENT_COUNT             // COMMAND_ARGUMENT_COUNT
	IntrinsicGET_COMMAND                        // GET_COMMAND
	IntrinsicGET_COMMAND_ARGUMENT               // GET_COMMAND_ARGUMENT
	IntrinsicGET_ENVIRONMENT_VARIABLE           // GET_ENVIRONMENT_VARIABLE
	fortran2003End
)

// Fortran 2008 Intrinsics
const (
	fortran2008Start      Intrinsic = iota + fortran2003End
	IntrinsicACOSH                  // ACOSH
	IntrinsicASINH                  // ASINH
	IntrinsicATANH                  // ATANH
	IntrinsicBESSEL_J0              // BESSEL_J0
	IntrinsicBESSEL_J1              // BESSEL_J1
	IntrinsicBESSEL_JN              // BESSEL_JN
	IntrinsicBESSEL_Y0              // BESSEL_Y0
	IntrinsicBESSEL_Y1              // BESSEL_Y1
	IntrinsicBESSEL_YN              // BESSEL_YN
	IntrinsicERF                    // ERF
	IntrinsicERFC                   // ERFC
	IntrinsicERFC_SCALED            // ERFC_SCALED
	IntrinsicGAMMA                  // GAMMA
	IntrinsicLOG_GAMMA              // LOG_GAMMA
	IntrinsicHYPOT                  // HYPOT
	IntrinsicNORM2                  // NORM2
	IntrinsicPARITY                 // PARITY
	IntrinsicFINDLOC                // FINDLOC
	IntrinsicSTORAGE_SIZE           // STORAGE_SIZE
	fortran2008End
)

func (intr Intrinsic) Version() (year int) {
	switch {
	case intr > fortran2008Start:
		year = 2008
	case intr > fortran2003Start:
		year = 2003
	case intr > fortran95Start:
		year = 95
	case intr > fortran77Start:
		year = 77
	case intr > fortran66Start:
		year = 66
	default:
		year = -1
	}
	return year
}

func (intr Intrinsic) IsValid() bool {
	return intr > fortran66Start && intr < fortran2008End && intr != fortran77Start &&
		intr != fortran90Start && intr != fortran95Start && intr != fortran2003Start && intr != fortran2008Start
}

func LookupIntrinsic(s string) Intrinsic {
	if len(s) < 2 || len(s) > 18 {
		return 0
	}
	// Hash uses uppercase, so convert input to uppercase for lookup
	intr := IntrinsicMap[ntrinsicHash(s)]
	if intr != 0 && strings.EqualFold(s, intr.String()) {
		return intr
	}
	return 0
}

func IsIntrinsic(s string) bool {
	return LookupIntrinsic(s) != 0
}

var IntrinsicMap [1 << 10]Intrinsic

func init() {
	for intr := fortran66Start + 1; intr < fortran2008End; intr++ {
		if !intr.IsValid() {
			continue
		} else if intr.Version() > 95 {
			break
		}
		name := intr.String()
		h := ntrinsicHash(name)
		if IntrinsicMap[h] != 0 {
			panic("imperfect hash")
		}
		IntrinsicMap[h] = intr
	}
}

func ntrinsicHash(s string) uint {
	h := uint(len(s)) * 13
	h *= uint(toUpper(s[0])) * 21
	h += uint(toUpper(s[1])) * 62
	h += uint(toUpper(s[len(s)-2])) * 25
	h += uint(toUpper(s[len(s)-1])) * 41
	return h & uint(len(IntrinsicMap)-1)
}
