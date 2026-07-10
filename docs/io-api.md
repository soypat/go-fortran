# Fortran I/O Specification Reference

This document describes Fortran I/O semantics as defined in the ISO/IEC 1539 Fortran standards (F77, F90, F95, F2003, F2008, F2018).

---

## I/O Statements Overview

| Statement | Purpose | Introduced |
|-----------|---------|------------|
| OPEN | Connect file to unit | F77 |
| CLOSE | Disconnect unit from file | F77 |
| READ | Input data | F77 |
| WRITE | Output data | F77 |
| PRINT | Output to default unit | F77 |
| INQUIRE | Query file/unit properties | F77 |
| REWIND | Position at beginning | F77 |
| BACKSPACE | Position before previous record | F77 |
| ENDFILE | Write end-of-file marker | F77 |
| FLUSH | Force buffered data to file | F2003 |
| WAIT | Wait for async I/O completion | F2003 |

---

## OPEN Statement

### Syntax
```fortran
OPEN([UNIT=] u, slist)
```

### Specifiers

| Specifier | Values | Type | Default | Notes |
|-----------|--------|------|---------|-------|
| UNIT | 1+ or expression | INTEGER | Required | Unit number to connect |
| FILE | filename or '*' | CHARACTER | Processor-dependent | External file name |
| STATUS | 'OLD', 'NEW', 'REPLACE', 'SCRATCH', 'UNKNOWN' | CHARACTER | 'UNKNOWN' | File existence handling |
| ACCESS | 'SEQUENTIAL', 'DIRECT', 'STREAM' | CHARACTER | 'SEQUENTIAL' | Access method |
| FORM | 'FORMATTED', 'UNFORMATTED' | CHARACTER | 'FORMATTED' (seq), 'UNFORMATTED' (direct) | Record format |
| RECL | positive integer | INTEGER | Required for DIRECT | Record length in bytes |
| POSITION | 'REWIND', 'APPEND', 'ASIS' | CHARACTER | 'ASIS' | Initial position (F90+) |
| ACTION | 'READ', 'WRITE', 'READWRITE' | CHARACTER | Processor-dependent | Allowed operations (F90+) |
| BLANK | 'NULL', 'ZERO' | CHARACTER | 'NULL' | Blank interpretation in numeric input |
| DELIM | 'APOSTROPHE', 'QUOTE', 'NONE' | CHARACTER | 'NONE' | Character delimiter for list-directed (F90+) |
| PAD | 'YES', 'NO' | CHARACTER | 'YES' | Pad short formatted records (F90+) |
| ROUND | 'UP', 'DOWN', 'ZERO', 'NEAREST', 'COMPATIBLE', 'PROCESSOR_DEFINED' | CHARACTER | 'PROCESSOR_DEFINED' | Rounding mode (F2003+) |
| SIGN | 'PLUS', 'SUPPRESS', 'PROCESSOR_DEFINED' | CHARACTER | 'PROCESSOR_DEFINED' | Plus sign display (F2003+) |
| DECIMAL | 'POINT', 'COMMA' | CHARACTER | 'POINT' | Decimal separator (F2003+) |
| ENCODING | 'UTF-8', 'DEFAULT' | CHARACTER | 'DEFAULT' | Character encoding (F2003+) |
| ASYNCHRONOUS | 'YES', 'NO' | CHARACTER | 'NO' | Enable async I/O (F2003+) |
| NEWUNIT | integer variable | INTEGER | N/A | Auto-assign unit number (F2008+) |
| IOSTAT | integer variable | INTEGER | N/A | Status code output |
| IOMSG | character variable | CHARACTER | N/A | Error message output (F2003+) |
| ERR | statement label | INTEGER | N/A | Error branch target |

### STATUS Values

| Value | Meaning |
|-------|---------|
| 'OLD' | File must already exist; error if not found |
| 'NEW' | File must not exist; error if found; creates new file |
| 'REPLACE' | If file exists, delete and create new; if not, create new |
| 'SCRATCH' | Create temporary file; deleted on CLOSE or program end |
| 'UNKNOWN' | Processor-dependent behavior (typically: open if exists, create if not) |

### ACCESS Modes

| Value | Description |
|-------|-------------|
| 'SEQUENTIAL' | Records accessed in order. Default. |
| 'DIRECT' | Random access by record number. Requires RECL=. All records same fixed length. |
| 'STREAM' | Byte-stream access. No record structure. Use POS= for positioning. (F2003+) |

### Constraints

- If `ACCESS='DIRECT'`, then `RECL=` is required
- If `BLANK=`, `DECIMAL=`, `PAD=`, or `ROUND=` specified, must use `FORM='FORMATTED'`
- If `ACTION='READ'`, cannot write to file
- `ENCODING=` only valid for `FORM='FORMATTED'`
- `STATUS='SCRATCH'` files must have `STATUS='DELETE'` on close (or are auto-deleted)

---

## CLOSE Statement

### Syntax
```fortran
CLOSE([UNIT=] u [, STATUS=sta] [, IOSTAT=ios] [, IOMSG=msg] [, ERR=s])
```

### Specifiers

| Specifier | Values | Type | Default |
|-----------|--------|------|---------|
| UNIT | integer | INTEGER | Required |
| STATUS | 'KEEP', 'DELETE' | CHARACTER | 'KEEP' (named), 'DELETE' (scratch) |
| IOSTAT | integer variable | INTEGER | N/A |
| IOMSG | character variable | CHARACTER | N/A |
| ERR | statement label | INTEGER | N/A |

### STATUS Values

| Value | Meaning |
|-------|---------|
| 'KEEP' | Retain file after close. Default for named files. |
| 'DELETE' | Delete file after close. Default for scratch files. |

Cannot specify `STATUS='KEEP'` for scratch files.

---

## READ Statement

### Syntax (Sequential Formatted)
```fortran
READ([UNIT=] u, [FMT=] fmt [, specifiers]) [io-list]
```

### Syntax (Direct Access)
```fortran
READ([UNIT=] u, REC=n [, specifiers]) [io-list]
```

### Specifiers

| Specifier | Values | Type | Notes |
|-----------|--------|------|-------|
| UNIT | integer, *, or internal file | INTEGER | Default: 5 (stdin) |
| FMT | label, character expr, or * | CHARACTER/INTEGER | * = list-directed |
| NML | namelist group name | - | Mutually exclusive with FMT |
| REC | positive integer | INTEGER | Record number for direct access |
| IOSTAT | integer variable | INTEGER | 0=success, <0=EOF, >0=error |
| IOMSG | character variable | CHARACTER | Error message (F2003+) |
| ERR | statement label | INTEGER | Error branch |
| END | statement label | INTEGER | EOF branch (sequential only) |
| EOR | statement label | INTEGER | End-of-record branch (non-advancing only) |
| ADVANCE | 'YES', 'NO' | CHARACTER | Record advancement (default: 'YES') |
| SIZE | integer variable | INTEGER | Characters read (non-advancing only) |
| BLANK | 'NULL', 'ZERO' | CHARACTER | Override OPEN setting |
| PAD | 'YES', 'NO' | CHARACTER | Override OPEN setting |
| DECIMAL | 'POINT', 'COMMA' | CHARACTER | Override OPEN setting (F2003+) |
| ROUND | rounding mode | CHARACTER | Override OPEN setting (F2003+) |
| ASYNCHRONOUS | 'YES', 'NO' | CHARACTER | Async operation (F2003+) |
| ID | integer variable | INTEGER | Async operation ID (F2003+) |
| POS | integer | INTEGER | Stream position (F2003+) |

### Non-Advancing I/O Constraints

- `ADVANCE='NO'` requires explicit format (not `*` or `NML`)
- If `ADVANCE='NO'`, can specify `SIZE=` to get character count
- If `ADVANCE='NO'`, can specify `EOR=` for end-of-record branch
- Cannot use `END=` with non-advancing I/O

---

## WRITE Statement

### Syntax
```fortran
WRITE([UNIT=] u, [FMT=] fmt [, specifiers]) [io-list]
```

### Specifiers

Same as READ, except:
- No `END=` (only for input)
- No `EOR=` (only for input with non-advancing)
- `SIZE=` returns characters written in non-advancing mode
- `DELIM=` can override OPEN setting for list-directed output

---

## PRINT Statement

### Syntax
```fortran
PRINT fmt [, io-list]
```

Equivalent to `WRITE(*, fmt) io-list`. Always uses default output unit (typically 6).

---

## INQUIRE Statement

### Syntax (By Unit)
```fortran
INQUIRE(UNIT=u, spec-list)
```

### Syntax (By File)
```fortran
INQUIRE(FILE=filename, spec-list)
```

Cannot specify both UNIT= and FILE= in same statement.

### Output Specifiers

| Specifier | Type | Value Returned |
|-----------|------|----------------|
| EXIST | LOGICAL | .TRUE. if file/unit exists |
| OPENED | LOGICAL | .TRUE. if unit is connected |
| NUMBER | INTEGER | Unit number; -1 if not connected |
| NAMED | LOGICAL | .TRUE. if file is named |
| NAME | CHARACTER | Filename (if named) |
| ACCESS | CHARACTER | 'SEQUENTIAL', 'DIRECT', 'STREAM', or 'UNDEFINED' |
| SEQUENTIAL | CHARACTER | 'YES', 'NO', or 'UNKNOWN' |
| DIRECT | CHARACTER | 'YES', 'NO', or 'UNKNOWN' |
| STREAM | CHARACTER | 'YES', 'NO', or 'UNKNOWN' (F2003+) |
| FORM | CHARACTER | 'FORMATTED', 'UNFORMATTED', or 'UNDEFINED' |
| FORMATTED | CHARACTER | 'YES', 'NO', or 'UNKNOWN' |
| UNFORMATTED | CHARACTER | 'YES', 'NO', or 'UNKNOWN' |
| RECL | INTEGER | Record length; 0 if not connected |
| NEXTREC | INTEGER | Next record number (direct access) |
| BLANK | CHARACTER | 'NULL' or 'ZERO' |
| POSITION | CHARACTER | 'REWIND', 'APPEND', 'ASIS', or 'UNDEFINED' |
| ACTION | CHARACTER | 'READ', 'WRITE', 'READWRITE', or 'UNDEFINED' |
| READ | CHARACTER | 'YES', 'NO', or 'UNKNOWN' |
| WRITE | CHARACTER | 'YES', 'NO', or 'UNKNOWN' |
| READWRITE | CHARACTER | 'YES', 'NO', or 'UNKNOWN' |
| DELIM | CHARACTER | 'APOSTROPHE', 'QUOTE', 'NONE', or 'UNDEFINED' |
| PAD | CHARACTER | 'YES' or 'NO' |
| ASYNCHRONOUS | CHARACTER | 'YES' or 'NO' (F2003+) |
| DECIMAL | CHARACTER | 'POINT' or 'COMMA' (F2003+) |
| ENCODING | CHARACTER | 'UTF-8' or 'DEFAULT' (F2003+) |
| ROUND | CHARACTER | Rounding mode (F2003+) |
| SIGN | CHARACTER | Sign mode (F2003+) |
| PENDING | LOGICAL | .TRUE. if async operations pending (F2003+) |
| POS | INTEGER | Current position in stream (F2003+) |
| SIZE | INTEGER | File size in file storage units (F2003+) |
| ID | INTEGER | Pending async operation ID (F2003+) |
| IOSTAT | INTEGER | Status code |
| IOMSG | CHARACTER | Error message (F2003+) |
| ERR | label | Error branch |

### IOLENGTH Form
```fortran
INQUIRE(IOLENGTH=len) io-list
```
Computes record length needed for unformatted direct-access I/O of the io-list.

---

## REWIND Statement

### Syntax
```fortran
REWIND([UNIT=] u [, IOSTAT=ios] [, IOMSG=msg] [, ERR=s])
```

Positions sequential file at beginning (before first record).

---

## BACKSPACE Statement

### Syntax
```fortran
BACKSPACE([UNIT=] u [, IOSTAT=ios] [, IOMSG=msg] [, ERR=s])
```

Positions sequential file before previous record. Cannot be used on stream files.

---

## ENDFILE Statement

### Syntax
```fortran
ENDFILE([UNIT=] u [, IOSTAT=ios] [, IOMSG=msg] [, ERR=s])
```

Writes end-of-file record and positions after it. For direct access, truncates file.

---

## FLUSH Statement (F2003+)

### Syntax
```fortran
FLUSH([UNIT=] u [, IOSTAT=ios] [, IOMSG=msg] [, ERR=s])
```

Forces buffered data to be physically written.

---

## WAIT Statement (F2003+)

### Syntax
```fortran
WAIT([UNIT=] u [, ID=id] [, DONE=done] [, IOSTAT=ios] [, IOMSG=msg] [, ERR=s])
```

Waits for completion of pending asynchronous I/O operations.

| Specifier | Type | Notes |
|-----------|------|-------|
| UNIT | INTEGER | Required |
| ID | INTEGER | Specific operation; if omitted, waits for all |
| DONE | LOGICAL | Poll completion without blocking |

---

## IOSTAT Values

| Value | Meaning |
|-------|---------|
| 0 | Successful completion |
| -1 | End of file (EOF) |
| -2 | End of record (EOR) with non-advancing I/O |
| > 0 | Error (specific values are processor-dependent) |

### Portable Constants (ISO_FORTRAN_ENV)

```fortran
USE ISO_FORTRAN_ENV
! IOSTAT_END - end of file value
! IOSTAT_EOR - end of record value
```

Intrinsic functions:
- `IS_IOSTAT_END(ios)` - returns .TRUE. if ios indicates EOF
- `IS_IOSTAT_EOR(ios)` - returns .TRUE. if ios indicates EOR

---

## Predefined Unit Numbers

| Unit | Purpose | Notes |
|------|---------|-------|
| 5 | Standard input (stdin) | Conventional, not standardized |
| 6 | Standard output (stdout) | Conventional, not standardized |
| 0 | Standard error (stderr) | Some implementations |
| * | Default unit | Context-dependent |

---

## FORMAT Descriptors

### Data Edit Descriptors

| Descriptor | Syntax | Purpose |
|------------|--------|---------|
| I | Iw, Iw.m | Integer |
| B | Bw, Bw.m | Binary integer (F90+) |
| O | Ow, Ow.m | Octal integer |
| Z | Zw, Zw.m | Hexadecimal integer |
| F | Fw.d | Fixed-point real |
| E | Ew.d, Ew.dEe | Exponential notation |
| EN | ENw.d, ENw.dEe | Engineering notation (F90+) |
| ES | ESw.d, ESw.dEe | Scientific notation (F90+) |
| D | Dw.d | Double precision exponential |
| G | Gw.d, Gw.dEe | General (auto-selects F or E) |
| L | Lw | Logical (T/F) |
| A | A, Aw | Character |
| DT | DT'name'(vlist) | Derived type (F2003+) |

Where:
- w = total field width
- m = minimum digits
- d = decimal places
- e = exponent digits

### Control Edit Descriptors

| Descriptor | Syntax | Purpose |
|------------|--------|---------|
| X | nX | Skip n positions |
| T | Tc | Tab to column c |
| TL | TLn | Tab left n positions |
| TR | TRn | Tab right n positions |
| / | / | New record |
| : | : | Terminate if no more items |
| S | S | Default sign mode |
| SP | SP | Always print plus sign |
| SS | SS | Suppress plus sign |
| P | kP | Scale factor for E/F |
| BN | BN | Blanks are null |
| BZ | BZ | Blanks are zero |
| DC | DC | Decimal comma (F2003+) |
| DP | DP | Decimal point (F2003+) |
| RU | RU | Round up (F2003+) |
| RD | RD | Round down (F2003+) |
| RZ | RZ | Round toward zero (F2003+) |
| RN | RN | Round to nearest (F2003+) |
| RC | RC | Compatible rounding (F2003+) |
| RP | RP | Processor-defined rounding (F2003+) |

### String Literals
```fortran
'text'  or  "text"
nHtext  (Hollerith - F77 only, obsolescent)
```

### Repeat Counts
```fortran
3I5        ! Three I5 fields
3(I5,F8.2) ! Three groups of (I5,F8.2)
```

### Unlimited Format
```fortran
*(I5,F8.2)  ! Repeat until io-list exhausted (F2008+)
```

---

## List-Directed I/O

When format is `*`:
- Input: Values separated by blanks, commas, or end-of-record
- Output: Processor-determined spacing and format
- Repeat counts: `n*value` means value repeated n times
- Null values: `,,` or `n*` means skip n values
- Character strings: Enclosed in quotes/apostrophes in input

---

## Namelist I/O

```fortran
NAMELIST /group_name/ var1, var2, ...

READ(unit, NML=group_name)
WRITE(unit, NML=group_name)
```

Input format:
```
&group_name
  var1 = value1,
  var2 = value2
/
```

---

## Internal Files

Character variables can be used as internal files:
```fortran
CHARACTER(LEN=80) :: buffer
WRITE(buffer, '(I5,F10.2)') i, x  ! Write to string
READ(buffer, '(I5,F10.2)') j, y   ! Read from string
```

Internal files:
- Always sequential access
- Always formatted
- No OPEN/CLOSE required
- No IOSTAT for EOF (use string length)

---

## Asynchronous I/O (F2003+)

```fortran
OPEN(10, FILE='data.bin', ASYNCHRONOUS='YES')
READ(10, ASYNCHRONOUS='YES', ID=req_id) buffer
! ... do other work ...
WAIT(10, ID=req_id)  ! or WAIT(10) for all
```

Constraints:
- Must specify `ASYNCHRONOUS='YES'` in OPEN
- Use `ID=` to track individual operations
- Variables in pending async I/O must not be accessed until WAIT completes

---

## Stream I/O (F2003+)

```fortran
OPEN(10, FILE='data.bin', ACCESS='STREAM', FORM='UNFORMATTED')
WRITE(10) value1, value2  ! Written at current position
READ(10, POS=100) value3  ! Read from byte position 100
INQUIRE(10, POS=current)  ! Get current position
```

- No record structure
- Position measured in file storage units (typically bytes)
- Can mix formatted and unformatted in same file

---

## Version History

| Version | Year | Key I/O Additions |
|---------|------|-------------------|
| F77 | 1978 | Core I/O: OPEN, CLOSE, READ, WRITE, PRINT, INQUIRE, positioning |
| F90 | 1991 | POSITION, ACTION, DELIM, PAD specifiers; non-advancing I/O |
| F95 | 1997 | Minor clarifications |
| F2003 | 2004 | Stream I/O, async I/O, FLUSH, WAIT, IOMSG, ENCODING, DECIMAL, ROUND, SIGN |
| F2008 | 2010 | NEWUNIT, unlimited format repeat |
| F2018 | 2018 | Clarifications and corrections |

---

## References

- ISO/IEC 1539-1:2018 (Fortran 2018 Standard)
- Oracle FORTRAN 77 Language Reference
- Intel Fortran Compiler Documentation
- GNU Fortran (gfortran) Manual
- Fortran Wiki (fortranwiki.org)
