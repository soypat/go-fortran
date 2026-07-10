// Package fortio provides Fortran I/O types and operations.
package fortio

//go:generate stringer -type=FileStatus,AccessMode,FormMode,PositionMode,ActionMode,BlankMode,DelimMode,PadMode,DecimalMode,RoundMode,SignMode,AdvanceMode,CloseStatus,AsyncMode,EncodingMode,YesNoUnknown -linecomment -output stringers.go .

// FileStatus represents the STATUS= specifier for OPEN statements.
type FileStatus uint8

const (
	// Processor-dependent behavior; typically opens if exists, creates if not.
	StatusUNKNOWN FileStatus = iota // UNKNOWN
	// File must already exist; error if not found.
	StatusOLD // OLD
	// File must not exist; error if found; creates new file.
	StatusNEW // NEW
	// If file exists, delete and create new; if not, create new.
	StatusREPLACE // REPLACE
	// Create temporary file; automatically deleted on CLOSE or program end.
	StatusSCRATCH // SCRATCH
)

// AccessMode represents the ACCESS= specifier for OPEN statements.
type AccessMode uint8

const (
	// Records accessed in order. Default access mode.
	AccessSEQUENTIAL AccessMode = iota // SEQUENTIAL
	// Random access by record number. Requires RECL= specifier.
	AccessDIRECT // DIRECT
	// Byte-stream access with no record structure. Use POS= for positioning. (F2003+)
	AccessSTREAM // STREAM
)

// FormMode represents the FORM= specifier for OPEN statements.
type FormMode uint8

const (
	// Text/readable records with format control. Default for sequential access.
	FormFORMATTED FormMode = iota // FORMATTED
	// Binary records without interpretation. Default for direct access.
	FormUNFORMATTED // UNFORMATTED
)

// PositionMode represents the POSITION= specifier for OPEN statements.
type PositionMode uint8

const (
	// Maintain current position when reconnecting. Default.
	PositionASIS PositionMode = iota // ASIS
	// Position at beginning of file.
	PositionREWIND // REWIND
	// Position after last record for appending.
	PositionAPPEND // APPEND
)

// ActionMode represents the ACTION= specifier for OPEN statements.
type ActionMode uint8

const (
	// Both read and write operations allowed. Default.
	ActionREADWRITE ActionMode = iota // READWRITE
	// Read-only access; cannot write to file.
	ActionREAD // READ
	// Write-only access; BACKSPACE prohibited.
	ActionWRITE // WRITE
)

// BlankMode represents the BLANK= specifier for OPEN/READ statements.
type BlankMode uint8

const (
	// Blanks ignored in numeric input fields. Default.
	BlankNULL BlankMode = iota // NULL
	// Blanks treated as zeros in numeric input fields.
	BlankZERO // ZERO
)

// DelimMode represents the DELIM= specifier for OPEN statements.
type DelimMode uint8

const (
	// No delimiter for character constants in list-directed output. Default.
	DelimNONE DelimMode = iota // NONE
	// Use apostrophe (') as delimiter for character constants.
	DelimAPOSTROPHE // APOSTROPHE
	// Use quote (") as delimiter for character constants.
	DelimQUOTE // QUOTE
)

// PadMode represents the PAD= specifier for OPEN/READ statements.
type PadMode uint8

const (
	// Pad short formatted input records with blanks. Default.
	PadYES PadMode = iota // YES
	// Error on short formatted input records.
	PadNO // NO
)

// DecimalMode represents the DECIMAL= specifier (F2003+).
type DecimalMode uint8

const (
	// Use period (.) as decimal separator. Default.
	DecimalPOINT DecimalMode = iota // POINT
	// Use comma (,) as decimal separator. European style.
	DecimalCOMMA // COMMA
)

// RoundMode represents the ROUND= specifier (F2003+).
type RoundMode uint8

const (
	// Processor-defined rounding behavior. Default.
	RoundPROCESSOR_DEFINED RoundMode = iota // PROCESSOR_DEFINED
	// Round to nearest representable value.
	RoundNEAREST // NEAREST
	// Round toward positive infinity.
	RoundUP // UP
	// Round toward negative infinity.
	RoundDOWN // DOWN
	// Round toward zero.
	RoundZERO // ZERO
	// Compatible rounding for I/O conversion.
	RoundCOMPATIBLE // COMPATIBLE
)

// SignMode represents the SIGN= specifier (F2003+).
type SignMode uint8

const (
	// Processor-defined sign display. Default.
	SignPROCESSOR_DEFINED SignMode = iota // PROCESSOR_DEFINED
	// Always display plus sign for positive values.
	SignPLUS // PLUS
	// Never display plus sign for positive values.
	SignSUPPRESS // SUPPRESS
)

// AdvanceMode represents the ADVANCE= specifier for READ/WRITE.
type AdvanceMode uint8

const (
	// Advance to next record after I/O operation. Default.
	AdvanceYES AdvanceMode = iota // YES
	// Non-advancing I/O; stay within current record.
	AdvanceNO // NO
)

// CloseStatus represents the STATUS= specifier for CLOSE statements.
type CloseStatus uint8

const (
	// Retain file after close. Default for named files.
	CloseKEEP CloseStatus = iota // KEEP
	// Delete file after close. Default for scratch files.
	CloseDELETE // DELETE
)

// AsyncMode represents the ASYNCHRONOUS= specifier (F2003+).
type AsyncMode uint8

const (
	// Synchronous I/O operations. Default.
	AsyncNO AsyncMode = iota // NO
	// Enable asynchronous I/O operations. Requires WAIT statement.
	AsyncYES // YES
)

// EncodingMode represents the ENCODING= specifier (F2003+).
type EncodingMode uint8

const (
	// Default character encoding.
	EncodingDEFAULT EncodingMode = iota // DEFAULT
	// UTF-8 character encoding.
	EncodingUTF8 // UTF-8
)

// YesNoUnknown represents tri-state inquiry results from INQUIRE statements.
type YesNoUnknown uint8

const (
	// Capability is unknown or cannot be determined.
	InquiryUNKNOWN YesNoUnknown = iota // UNKNOWN
	// Capability is available or condition is true.
	InquiryYES // YES
	// Capability is not available or condition is false.
	InquiryNO // NO
)
