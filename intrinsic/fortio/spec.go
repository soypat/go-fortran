package fortio

import "github.com/soypat/go-fortran/intrinsic"

// OpenSpec holds all OPEN statement specifiers.
type OpenSpec struct {
	// UNIT number to connect. Required.
	UNIT int32
	// FILE name. Required for non-scratch files.
	FILE string
	// STATUS specifies file existence handling.
	STATUS FileStatus
	// ACCESS method (sequential, direct, stream).
	ACCESS AccessMode
	// FORM specifies record format (formatted, unformatted).
	FORM FormMode
	// RECL specifies record length in bytes. Required for direct access.
	RECL int32
	// POSITION specifies initial file position.
	POSITION PositionMode
	// ACTION specifies allowed I/O operations.
	ACTION ActionMode
	// BLANK specifies blank interpretation in numeric input.
	BLANK BlankMode
	// DELIM specifies character delimiter for list-directed output.
	DELIM DelimMode
	// PAD specifies padding for short formatted input records.
	PAD PadMode
	// DECIMAL specifies decimal separator character.
	DECIMAL DecimalMode
	// ROUND specifies rounding mode for I/O conversions.
	ROUND RoundMode
	// SIGN specifies plus sign display mode.
	SIGN SignMode
	// ENCODING specifies character encoding.
	ENCODING EncodingMode
	// ASYNCHRONOUS specifies asynchronous I/O mode.
	ASYNCHRONOUS AsyncMode

	// NEWUNIT receives auto-assigned unit number (F2008+).
	// If non-nil, unit is auto-assigned and stored here.
	NEWUNIT *int32
	// IOSTAT receives the I/O status code.
	// 0=success, <0=end condition, >0=error.
	IOSTAT *int32
	// IOMSG receives the error message string.
	IOMSG *intrinsic.CharacterArray
}

// CloseSpec holds all CLOSE statement specifiers.
type CloseSpec struct {
	// UNIT number to disconnect. Required.
	UNIT int32
	// STATUS specifies file disposition after close.
	STATUS CloseStatus
	// IOSTAT receives the I/O status code.
	IOSTAT *int32
	// IOMSG receives the error message string.
	IOMSG *intrinsic.CharacterArray
}

// IOSpec holds common READ/WRITE statement specifiers.
type IOSpec struct {
	// UNIT number for I/O operation.
	UNIT int32
	// FMT is the format specifier.
	FMT *Format
	// REC specifies record number for direct access.
	REC int32
	// ADVANCE specifies record advancement mode.
	ADVANCE AdvanceMode
	// DECIMAL specifies decimal separator (overrides OPEN setting).
	DECIMAL DecimalMode
	// ROUND specifies rounding mode (overrides OPEN setting).
	ROUND RoundMode
	// SIGN specifies plus sign display (overrides OPEN setting).
	SIGN SignMode
	// BLANK specifies blank interpretation for READ (overrides OPEN setting).
	BLANK BlankMode
	// PAD specifies padding for READ (overrides OPEN setting).
	PAD PadMode
	// DELIM specifies character delimiter for list-directed WRITE (overrides OPEN setting).
	DELIM DelimMode
	// ASYNCHRONOUS specifies asynchronous operation mode.
	ASYNCHRONOUS AsyncMode
	// POS specifies stream position for stream access.
	POS int64

	// ID receives async operation identifier.
	ID *int32
	// SIZE receives characters read/written (non-advancing).
	SIZE *int32
	// IOSTAT receives the I/O status code.
	IOSTAT *int32
	// IOMSG receives the error message string.
	IOMSG *intrinsic.CharacterArray
}

// InquireSpec holds INQUIRE statement input specifiers.
type InquireSpec struct {
	// UNIT number to query. Mutually exclusive with FILE.
	UNIT int32
	// FILE name to query. Mutually exclusive with UNIT.
	FILE string
	// ByFile indicates whether to query by file name (true) or unit (false).
	ByFile bool
}

// InquireResult holds all INQUIRE statement output specifiers.
type InquireResult struct {
	// EXIST is true if the file or unit exists.
	EXIST bool
	// OPENED is true if the unit is currently connected.
	OPENED bool
	// NUMBER is the unit number; -1 if not connected.
	NUMBER int32
	// NAMED is true if the file has a name (not scratch).
	NAMED bool
	// NAME is the file name if named.
	NAME string

	// ACCESS mode of connected unit.
	ACCESS AccessMode
	// FORM mode of connected unit.
	FORM FormMode
	// POSITION mode of connected unit.
	POSITION PositionMode
	// ACTION mode of connected unit.
	ACTION ActionMode
	// BLANK mode of connected unit.
	BLANK BlankMode
	// DELIM mode of connected unit.
	DELIM DelimMode
	// PAD mode of connected unit.
	PAD PadMode
	// DECIMAL mode of connected unit.
	DECIMAL DecimalMode
	// ROUND mode of connected unit.
	ROUND RoundMode
	// SIGN mode of connected unit.
	SIGN SignMode
	// ENCODING mode of connected unit.
	ENCODING EncodingMode
	// ASYNCHRONOUS mode of connected unit.
	ASYNCHRONOUS AsyncMode

	// SEQUENTIAL indicates if sequential access is allowed.
	SEQUENTIAL YesNoUnknown
	// DIRECT indicates if direct access is allowed.
	DIRECT YesNoUnknown
	// STREAM indicates if stream access is allowed.
	STREAM YesNoUnknown
	// FORMATTED indicates if formatted I/O is allowed.
	FORMATTED YesNoUnknown
	// UNFORMATTED indicates if unformatted I/O is allowed.
	UNFORMATTED YesNoUnknown
	// READ indicates if read operations are allowed.
	READ YesNoUnknown
	// WRITE indicates if write operations are allowed.
	WRITE YesNoUnknown
	// READWRITE indicates if read/write operations are allowed.
	READWRITE YesNoUnknown

	// RECL is the record length; 0 if not connected or not applicable.
	RECL int32
	// NEXTREC is the next record number for direct access.
	NEXTREC int32
	// POS is the current stream position.
	POS int64
	// SIZE is the file size in storage units.
	SIZE int64

	// PENDING is true if async operations are pending.
	PENDING bool
	// ID is the pending async operation identifier.
	ID int32

	// IOSTAT is the status code from the inquiry.
	IOSTAT IOStat
	// IOMSG is the error message if any.
	IOMSG string
}
