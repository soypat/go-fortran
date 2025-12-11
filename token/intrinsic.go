package token

import "strings"

type intrinsic uint

const (
	intrinsicUndefined intrinsic = 0
)

// Fortran 66 function intrinsics (FORTRAN IV compatible)
const (
	fortran66Start intrinsic = iota + intrinsicUndefined + 1
	// Mathematical functions
	intrinsicABS   // ABS
	intrinsicMOD   // MOD
	intrinsicSIGN  // SIGN
	intrinsicDIM   // DIM
	intrinsicDPROD // DPROD
	// Trigonometric
	intrinsicSIN   // SIN
	intrinsicCOS   // COS
	intrinsicTAN   // TAN
	intrinsicASIN  // ASIN
	intrinsicACOS  // ACOS
	intrinsicATAN  // ATAN
	intrinsicATAN2 // ATAN2
	// Hyperbolic
	intrinsicSINH // SINH
	intrinsicCOSH // COSH
	intrinsicTANH // TANH
	// Exponential and logarithmic
	intrinsicEXP   // EXP
	intrinsicLOG   // LOG
	intrinsicLOG10 // LOG10
	intrinsicSQRT  // SQRT
	// Type conversion
	intrinsicINT   // INT
	intrinsicREAL  // REAL
	intrinsicDBLE  // DBLE
	intrinsicCMPLX // CMPLX
	intrinsicFLOAT // FLOAT
	intrinsicIFIX  // IFIX
	intrinsicSNGL  // SNGL
	// Truncation and rounding
	intrinsicAINT  // AINT
	intrinsicANINT // ANINT
	intrinsicNINT  // NINT
	// Min/Max
	intrinsicMAX   // MAX
	intrinsicMIN   // MIN
	intrinsicMAX0  // MAX0
	intrinsicMAX1  // MAX1
	intrinsicMIN0  // MIN0
	intrinsicMIN1  // MIN1
	intrinsicAMAX0 // AMAX0
	intrinsicAMAX1 // AMAX1
	intrinsicAMIN0 // AMIN0
	intrinsicAMIN1 // AMIN1
	intrinsicDMAX1 // DMAX1
	intrinsicDMIN1 // DMIN1
	// Complex conjugate and imaginary part
	intrinsicCONJG // CONJG
	intrinsicAIMAG // AIMAG
	// Specific names for type variants
	intrinsicIABS   // IABS
	intrinsicDABS   // DABS
	intrinsicCABS   // CABS
	intrinsicDSIN   // DSIN
	intrinsicDCOS   // DCOS
	intrinsicDTAN   // DTAN
	intrinsicDASIN  // DASIN
	intrinsicDACOS  // DACOS
	intrinsicDATAN  // DATAN
	intrinsicDATAN2 // DATAN2
	intrinsicDSINH  // DSINH
	intrinsicDCOSH  // DCOSH
	intrinsicDTANH  // DTANH
	intrinsicDEXP   // DEXP
	intrinsicDLOG   // DLOG
	intrinsicDLOG10 // DLOG10
	intrinsicDSQRT  // DSQRT
	intrinsicCSIN   // CSIN
	intrinsicCCOS   // CCOS
	intrinsicCEXP   // CEXP
	intrinsicCLOG   // CLOG
	intrinsicCSQRT  // CSQRT
	intrinsicIDIM   // IDIM
	intrinsicDDIM   // DDIM
	intrinsicIDINT  // IDINT
	intrinsicISIGN  // ISIGN
	intrinsicDSIGN  // DSIGN
	intrinsicDNINT  // DNINT
	intrinsicIDNINT // IDNINT
	intrinsicDIMAG  // DIMAG
	intrinsicDCONJG // DCONJG
	fortran66End
)

// Fortran 77 function intrinsics (added string handling)
const (
	fortran77Start intrinsic = iota + fortran66End
	// Character functions
	intrinsicCHAR  // CHAR
	intrinsicICHAR // ICHAR
	intrinsicLEN   // LEN
	intrinsicINDEX // INDEX
	// Lexical comparison
	intrinsicLGE // LGE
	intrinsicLGT // LGT
	intrinsicLLE // LLE
	intrinsicLLT // LLT
	fortran77End
)

// Fortran 90 function intrinsics
const (
	fortran90Start intrinsic = iota + fortran77End
	// Array reduction functions
	intrinsicSUM     // SUM
	intrinsicPRODUCT // PRODUCT
	intrinsicMAXVAL  // MAXVAL
	intrinsicMINVAL  // MINVAL
	intrinsicALL     // ALL
	intrinsicANY     // ANY
	intrinsicCOUNT   // COUNT
	// Array inquiry functions
	intrinsicSIZE      // SIZE
	intrinsicSHAPE     // SHAPE
	intrinsicLBOUND    // LBOUND
	intrinsicUBOUND    // UBOUND
	intrinsicALLOCATED // ALLOCATED
	// Array construction functions
	intrinsicRESHAPE // RESHAPE
	intrinsicSPREAD  // SPREAD
	intrinsicPACK    // PACK
	intrinsicUNPACK  // UNPACK
	intrinsicMERGE   // MERGE
	// Array manipulation functions
	intrinsicTRANSPOSE // TRANSPOSE
	intrinsicCSHIFT    // CSHIFT
	intrinsicEOSHIFT   // EOSHIFT
	// Array location functions
	intrinsicMAXLOC // MAXLOC
	intrinsicMINLOC // MINLOC
	// Matrix functions
	intrinsicMATMUL      // MATMUL
	intrinsicDOT_PRODUCT // DOT_PRODUCT
	// Bit manipulation functions
	intrinsicIAND   // IAND
	intrinsicIOR    // IOR
	intrinsicIEOR   // IEOR
	intrinsicNOT    // NOT
	intrinsicBTEST  // BTEST
	intrinsicIBSET  // IBSET
	intrinsicIBCLR  // IBCLR
	intrinsicIBITS  // IBITS
	intrinsicISHFT  // ISHFT
	intrinsicISHFTC // ISHFTC
	intrinsicMVBITS // MVBITS
	// Floating point inquiry
	intrinsicHUGE         // HUGE
	intrinsicTINY         // TINY
	intrinsicEPSILON      // EPSILON
	intrinsicPRECISION    // PRECISION
	intrinsicRANGE        // RANGE
	intrinsicRADIX        // RADIX
	intrinsicDIGITS       // DIGITS
	intrinsicBIT_SIZE     // BIT_SIZE
	intrinsicEXPONENT     // EXPONENT
	intrinsicFRACTION     // FRACTION
	intrinsicNEAREST      // NEAREST
	intrinsicRRSPACING    // RRSPACING
	intrinsicSPACING      // SPACING
	intrinsicSCALE        // SCALE
	intrinsicSET_EXPONENT // SET_EXPONENT
	// Kind functions
	intrinsicKIND               // KIND
	intrinsicSELECTED_INT_KIND  // SELECTED_INT_KIND
	intrinsicSELECTED_REAL_KIND // SELECTED_REAL_KIND
	// String functions
	intrinsicLEN_TRIM // LEN_TRIM
	intrinsicTRIM     // TRIM
	intrinsicADJUSTL  // ADJUSTL
	intrinsicADJUSTR  // ADJUSTR
	intrinsicREPEAT   // REPEAT
	intrinsicSCAN     // SCAN
	intrinsicVERIFY   // VERIFY
	// Pointer inquiry
	intrinsicASSOCIATED // ASSOCIATED
	// Argument presence
	intrinsicPRESENT // PRESENT
	// Transfer and conversion
	intrinsicTRANSFER // TRANSFER
	intrinsicLOGICAL  // LOGICAL
	// Miscellaneous
	intrinsicCEILING // CEILING
	intrinsicFLOOR   // FLOOR
	intrinsicMODULO  // MODULO
	intrinsicNULL    // NULL
	fortran90End
)

// Fortran 95 intrinsics
const (
	fortran95Start    intrinsic = iota + fortran90End
	intrinsicCPU_TIME           // CPU_TIME
	fortran95End
)

// Fortran 2003 intrinsics
const (
	fortran2003Start                  intrinsic = iota + fortran95End
	intrinsicMOVE_ALLOC                         // MOVE_ALLOC
	intrinsicIS_IOSTAT_END                      // IS_IOSTAT_END
	intrinsicIS_IOSTAT_EOR                      // IS_IOSTAT_EOR
	intrinsicNEW_LINE                           // NEW_LINE
	intrinsicCOMMAND_ARGUMENT_COUNT             // COMMAND_ARGUMENT_COUNT
	intrinsicGET_COMMAND                        // GET_COMMAND
	intrinsicGET_COMMAND_ARGUMENT               // GET_COMMAND_ARGUMENT
	intrinsicGET_ENVIRONMENT_VARIABLE           // GET_ENVIRONMENT_VARIABLE
	fortran2003End
)

// Fortran 2008 intrinsics
const (
	fortran2008Start      intrinsic = iota + fortran2003End
	intrinsicACOSH                  // ACOSH
	intrinsicASINH                  // ASINH
	intrinsicATANH                  // ATANH
	intrinsicBESSEL_J0              // BESSEL_J0
	intrinsicBESSEL_J1              // BESSEL_J1
	intrinsicBESSEL_JN              // BESSEL_JN
	intrinsicBESSEL_Y0              // BESSEL_Y0
	intrinsicBESSEL_Y1              // BESSEL_Y1
	intrinsicBESSEL_YN              // BESSEL_YN
	intrinsicERF                    // ERF
	intrinsicERFC                   // ERFC
	intrinsicERFC_SCALED            // ERFC_SCALED
	intrinsicGAMMA                  // GAMMA
	intrinsicLOG_GAMMA              // LOG_GAMMA
	intrinsicHYPOT                  // HYPOT
	intrinsicNORM2                  // NORM2
	intrinsicPARITY                 // PARITY
	intrinsicFINDLOC                // FINDLOC
	intrinsicSTORAGE_SIZE           // STORAGE_SIZE
	fortran2008End
)

func (intr intrinsic) Version() (year int) {
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

func (intr intrinsic) IsValid() bool {
	return intr > fortran66Start && intr < fortran2008End && intr != fortran77Start &&
		intr != fortran90Start && intr != fortran95Start && intr != fortran2003Start && intr != fortran2008Start
}

func LookupIntrinsic(s string) intrinsic {
	if len(s) < 2 || len(s) > 18 {
		return 0
	}
	intr := intrinsicMap[intrinsicHash(s)]
	if intr != 0 && strings.EqualFold(s, intr.String()) {
		return intr
	}
	return 0
}

func IsIntrinsic(s string) bool {
	return LookupIntrinsic(s) != 0
}

var intrinsicMap [1 << 10]intrinsic

func init() {
	for intr := fortran66Start + 1; intr < fortran2008End; intr++ {
		if !intr.IsValid() {
			continue
		} else if intr.Version() > 77 {
			break
		}
		name := intr.String()
		h := intrinsicHash(name)
		if intrinsicMap[h] != 0 {
			panic("imperfect hash")
		}
		intrinsicMap[h] = intr
	}
}

func intrinsicHash(s string) uint {
	h := uint(len(s)) * 13
	h *= uint(s[0]) * 21
	h += uint(s[1]) * 62
	h += uint(s[len(s)-2]) * 25
	h += uint(s[len(s)-1]) * 41
	return h & uint(len(intrinsicMap)-1)
}
