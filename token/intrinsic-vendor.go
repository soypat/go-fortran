package token

import (
	"strconv"
	"strings"
)

type VendorIntrinsic int

const (
	vendorUndefined VendorIntrinsic = iota // undefined

	// Common vendor intrinsics (shared across multiple compilers)
	vendorCommonStart
	VendorSYSTEM // SYSTEM
	VendorMALLOC // MALLOC
	VendorFREE   // FREE
	VendorLOC    // LOC
	VendorISNAN  // ISNAN
	VendorSIZEOF // SIZEOF
	// Command-line argument handling (legacy)
	VendorIARGC  // IARGC
	VendorGETARG // GETARG
	VendorNARGS  // NARGS
	// Type conversions
	VendorDFLOAT // DFLOAT
	VendorDCMPLX // DCMPLX
	VendorDREAL  // DREAL
	// Random numbers
	VendorRAND  // RAND
	VendorSRAND // SRAND
	VendorIRAND // IRAND
	// Bit operations (legacy names)
	VendorLSHIFT // LSHIFT
	VendorRSHIFT // RSHIFT
	// Degree-based trigonometric
	VendorSIND   // SIND
	VendorCOSD   // COSD
	VendorTAND   // TAND
	VendorASIND  // ASIND
	VendorACOSD  // ACOSD
	VendorATAND  // ATAND
	VendorATAN2D // ATAN2D
	vendorCommonEnd

	// Intel-specific intrinsics
	vendorIntelStart VendorIntrinsic = iota + vendorCommonEnd
	VendorQCMPLX                     // QCMPLX
	VendorQEXT                       // QEXT
	VendorQFLOAT                     // QFLOAT
	VendorQREAL                      // QREAL
	// String-to-value scanning
	VendorDNUM // DNUM
	VendorINUM // INUM
	VendorJNUM // JNUM
	VendorKNUM // KNUM
	VendorQNUM // QNUM
	VendorRNUM // RNUM
	// Bit manipulation
	VendorIBCHNG // IBCHNG
	VendorISHA   // ISHA
	VendorISHC   // ISHC
	VendorISHL   // ISHL
	VendorIXOR   // IXOR
	// Query functions
	VendorILEN         // ILEN
	VendorMCLOCK       // MCLOCK
	VendorSECNDS       // SECNDS
	VendorCACHESIZE    // CACHESIZE
	VendorEOF          // EOF
	VendorFP_CLASS     // FP_CLASS
	VendorINT_PTR_KIND // INT_PTR_KIND
	// Address functions
	VendorBADDRESS // BADDRESS
	VendorIADDR    // IADDR
	// Random
	VendorRAN  // RAN
	VendorRANF // RANF
	// Cotan
	VendorCOTAND // COTAND
	vendorIntelEnd

	// GNU-specific intrinsics
	vendorGNUStart VendorIntrinsic = iota + vendorIntelEnd
	// Time functions
	VendorETIME   // ETIME
	VendorDTIME   // DTIME
	VendorDSECNDS // DSECNDS
	VendorTIME    // TIME
	VendorTIME8   // TIME8
	VendorCTIME   // CTIME
	VendorFDATE   // FDATE
	VendorGMTIME  // GMTIME
	VendorLTIME   // LTIME
	VendorIDATE   // IDATE
	VendorITIME   // ITIME
	// File operations
	VendorGETCWD // GETCWD
	VendorCHDIR  // CHDIR
	VendorRENAME // RENAME
	VendorUNLINK // UNLINK
	VendorLINK   // LINK
	VendorSYMLNK // SYMLNK
	VendorACCESS // ACCESS
	VendorCHMOD  // CHMOD
	VendorSTAT   // STAT
	VendorLSTAT  // LSTAT
	VendorFSTAT  // FSTAT
	// File I/O
	VendorFSEEK // FSEEK
	VendorFTELL // FTELL
	VendorFNUM  // FNUM
	VendorFGET  // FGET
	VendorFGETC // FGETC
	VendorFPUT  // FPUT
	VendorFPUTC // FPUTC
	VendorFLUSH // FLUSH
	// System information
	VendorHOSTNM // HOSTNM
	VendorGETLOG // GETLOG
	VendorGETPID // GETPID
	VendorGETUID // GETUID
	VendorGETGID // GETGID
	VendorGETENV // GETENV
	VendorPUTENV // PUTENV
	VendorISATTY // ISATTY
	VendorTTYNAM // TTYNAM
	// Process control
	VendorALARM     // ALARM
	VendorSIGNAL    // SIGNAL
	VendorKILL      // KILL
	VendorSLEEP     // SLEEP
	VendorABORT     // ABORT
	VendorEXIT      // EXIT
	VendorBACKTRACE // BACKTRACE
	// Error handling
	VendorPERROR // PERROR
	VendorIERRNO // IERRNO
	VendorGERROR // GERROR
	// Miscellaneous
	VendorUMASK  // UMASK
	VendorLNBLNK // LNBLNK
	VendorCOTAN  // COTAN
	VendorQSORT  // QSORT
	vendorGNUEnd

	// PGI/NVIDIA-specific intrinsics
	vendorPGIStart VendorIntrinsic = iota + vendorGNUEnd
	// Bitwise operations (function form)
	VendorAND   // AND
	VendorOR    // OR
	VendorXOR   // XOR
	VendorCOMPL // COMPL
	VendorEQV   // EQV
	VendorNEQV  // NEQV
	// Type conversions
	VendorZEXT  // ZEXT
	VendorIZEXT // IZEXT
	VendorINT8  // INT8
	VendorJINT  // JINT
	VendorJNINT // JNINT
	VendorKNINT // KNINT
	// Shift
	VendorSHIFT // SHIFT
	vendorPGIEnd
)

// Vendor returns the vendor/origin of the intrinsic.
func (vi VendorIntrinsic) Vendor() string {
	switch {
	case vi > vendorPGIStart && vi < vendorPGIEnd:
		return "PGI"
	case vi > vendorGNUStart && vi < vendorGNUEnd:
		return "GNU"
	case vi > vendorIntelStart && vi < vendorIntelEnd:
		return "Intel"
	case vi > vendorCommonStart && vi < vendorCommonEnd:
		return "Common"
	default:
		return ""
	}
}

// IsValid returns true if the VendorIntrinsic is a valid vendor intrinsic.
func (vi VendorIntrinsic) IsValid() bool {
	return (vi > vendorCommonStart && vi < vendorCommonEnd) ||
		(vi > vendorIntelStart && vi < vendorIntelEnd) ||
		(vi > vendorGNUStart && vi < vendorGNUEnd) ||
		(vi > vendorPGIStart && vi < vendorPGIEnd)
}

// LookupVendorIntrinsic returns the VendorIntrinsic for the given name, or 0 if not found.
func LookupVendorIntrinsic(s string) VendorIntrinsic {
	if len(s) < 2 || len(s) > 14 {
		return 0
	}
	vi := VendorIntrinsicMap[vendorIntrinsicHash(s)]
	if vi != 0 && strings.EqualFold(s, vi.String()) {
		return vi
	}
	return 0
}

// IsVendorIntrinsic returns true if the given name is a vendor intrinsic.
func IsVendorIntrinsic(s string) bool {
	return LookupVendorIntrinsic(s) != 0
}

// VendorIntrinsicMap is the hash map for vendor intrinsic lookup.
var VendorIntrinsicMap [1 << 10]VendorIntrinsic

func init() {
	for vi := vendorCommonStart + 1; vi < vendorPGIEnd; vi++ {
		if !vi.IsValid() {
			continue
		}
		name := vi.String()
		h := vendorIntrinsicHash(name)
		if VendorIntrinsicMap[h] != 0 {
			panic("imperfect vendor intrinsic hash: " + name + " collides with " + VendorIntrinsicMap[h].String() + " " + strconv.Itoa(int(h)))
			// println("IMPERFECT HASH")
		}
		VendorIntrinsicMap[h] = vi
	}
}

func vendorIntrinsicHash(s string) uint {
	// Perfect hash found by TestFindPerfectHashVendorIntrinsics
	// s := [6]byte{toUpper(ss[0]), toUpper(ss[1]), toUpper(ss[2]),
	// 	toUpper(ss[len(ss)-3]), toUpper(ss[len(ss)-2]), toUpper(ss[len(ss)-1])}
	h := uint(len(s))
	h += uint(toUpper(s[0])) * 57
	h *= uint(toUpper(s[1])) * 49
	h *= uint(toUpper(s[len(s)-2])) * 8
	h += uint(toUpper(s[len(s)-1])) * 43
	if len(s) > 2 {
		h += uint(toUpper(s[2])) * 58
		h *= uint(toUpper(s[len(s)-3])) * 47
	}
	return h & uint(len(VendorIntrinsicMap)-1)
}
