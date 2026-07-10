package fortio

// IOStat represents Fortran IOSTAT values.
// Standard: 0=success, <0=end conditions, >0=errors.
type IOStat int32

// Standard IOSTAT values per ISO/IEC 1539.
const (
	IOStatOK  IOStat = 0  // Successful completion
	IOStatEOF IOStat = -1 // End of file
	IOStatEOR IOStat = -2 // End of record (non-advancing I/O)
)

// Implementation-specific error codes (positive values).
const (
	IOStatErrFileNotFound     IOStat = 1
	IOStatErrPermissionDenied IOStat = 2
	IOStatErrFileExists       IOStat = 3
	IOStatErrFormatMismatch   IOStat = 4
	IOStatErrConversion       IOStat = 5
	IOStatErrInvalidUnit      IOStat = 6
	IOStatErrNotConnected     IOStat = 7
	IOStatErrRecordTooLong    IOStat = 8
	IOStatErrEndfile          IOStat = 9
	IOStatErrInternal         IOStat = 99
)

// IsOK returns true if the operation succeeded.
func (s IOStat) IsOK() bool { return s == IOStatOK }

// IsEOF returns true if end-of-file was reached.
// Equivalent to IS_IOSTAT_END intrinsic.
func (s IOStat) IsEOF() bool { return s == IOStatEOF }

// IsEOR returns true if end-of-record was reached (non-advancing I/O).
// Equivalent to IS_IOSTAT_EOR intrinsic.
func (s IOStat) IsEOR() bool { return s == IOStatEOR }

// IsEndCondition returns true for any end condition (EOF or EOR).
func (s IOStat) IsEndCondition() bool { return s < 0 }

// IsError returns true if an error occurred.
func (s IOStat) IsError() bool { return s > 0 }
