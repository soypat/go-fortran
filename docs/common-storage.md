# Fortran COMMON Block Storage and Alignment Specification

This document details how Fortran COMMON blocks work at the memory level, including offsets, alignment, and the behavior with non-homogeneous data types.

---

## 1. The Fortran Standard: Storage Units

### Numeric Storage Unit
- **Definition**: A fixed unit of physical memory (typically 4 bytes)
- **Default mapping**: 1 numeric storage unit = INTEGER, REAL, LOGICAL
- **Double mapping**: 2 numeric storage units = DOUBLE PRECISION, COMPLEX

### Character Storage Unit
- **Definition**: 1 byte per character
- **The standard intentionally does NOT define a relationship between numeric and character storage units**

### Critical Point
The Fortran standard does **NOT** specify byte sizes - only "storage units". The actual byte size is implementation-defined. However, most modern compilers use:
- 1 numeric storage unit = 4 bytes
- 1 character storage unit = 1 byte

---

## 2. COMMON Block Memory Layout: The Standard View

### Sequential Contiguous Storage
According to **all Fortran standards**, COMMON blocks must be **contiguous in memory**. Variables are laid out in **declaration order** without gaps.

```fortran
COMMON /BLK/ A, B, C
```
Memory: `[A][B][C]` - no padding between them per the standard.

### The Standard Says NO Padding
From the Fortran 77/90/95 standards:
> "COMMONs are supposed to be contiguous in memory... An implementation that inserts padding between variables in COMMON is in violation of the Fortran standards."

---

## 3. Reality: Compiler Alignment Behavior

### The Problem
Many compilers (especially for RISC architectures) **violate the standard** by inserting padding for performance reasons. Unaligned memory access can be:
- Slow (2-10x penalty on some CPUs)
- Illegal (causes hardware fault on some architectures)

### Typical Compiler Behavior

| Data Type | Size (bytes) | Default Alignment | Alignment in COMMON |
|-----------|--------------|-------------------|---------------------|
| INTEGER*1 (int8) | 1 | 1 | 1 |
| INTEGER*2 (int16) | 2 | 2 | 2 |
| INTEGER*4 (int32) | 4 | 4 | 4 |
| INTEGER*8 (int64) | 8 | 8 | **4** (capped!) |
| REAL*4 | 4 | 4 | 4 |
| REAL*8 | 8 | 8 | **4** (capped!) |
| DOUBLE PRECISION | 8 | 8 | **4** (capped!) |

**Key insight**: In COMMON blocks, maximum alignment is typically **capped at 4 bytes**, not the natural alignment of the type!

---

## 4. Mixed-Type Example: int8, int64, int16, int32, int32

Consider `COMMON /BLK/ a, b, c, d, e` where:
- `a` = INTEGER*1 (1 byte)
- `b` = INTEGER*8 (8 bytes)
- `c` = INTEGER*2 (2 bytes)
- `d` = INTEGER*4 (4 bytes)
- `e` = INTEGER*4 (4 bytes)

### Scenario A: Standard-Compliant (NO PADDING)

```
Offset  0: a (1 byte)   [X]
Offset  1: b (8 bytes)  [XXXXXXXX]  <- UNALIGNED!
Offset  9: c (2 bytes)  [XX]        <- UNALIGNED!
Offset 11: d (4 bytes)  [XXXX]      <- UNALIGNED!
Offset 15: e (4 bytes)  [XXXX]      <- UNALIGNED!
Total: 19 bytes
```

Memory layout: `[a][bbbbbbbb][cc][dddd][eeee]`

This is what the **Fortran standard requires**, but:
- `b` at offset 1 is unaligned (should be at multiple of 8)
- `c` at offset 9 is unaligned (should be at multiple of 2)
- `d` at offset 11 is unaligned (should be at multiple of 4)
- `e` at offset 15 is unaligned (should be at multiple of 4)

### Scenario B: With Natural Alignment Padding

```
Offset  0: a (1 byte)   [X]
Offset  1: padding      [......7 bytes......]  <- align b to 8
Offset  8: b (8 bytes)  [XXXXXXXX]
Offset 16: c (2 bytes)  [XX]
Offset 18: padding      [.2 bytes.]  <- align d to 4
Offset 20: d (4 bytes)  [XXXX]
Offset 24: e (4 bytes)  [XXXX]
Total: 28 bytes
```

### Scenario C: COMMON-style 4-byte Max Alignment

```
Offset  0: a (1 byte)   [X]
Offset  1: padding      [.3 bytes.]  <- align b to 4 (not 8!)
Offset  4: b (8 bytes)  [XXXXXXXX]   <- still "unaligned" for 8-byte type
Offset 12: c (2 bytes)  [XX]
Offset 14: padding      [.2 bytes.]  <- align d to 4
Offset 16: d (4 bytes)  [XXXX]
Offset 20: e (4 bytes)  [XXXX]
Total: 24 bytes
```

---

## 5. Aliasing Different COMMON Declarations

**COMMON blocks are position-based, NOT name-based.** The variable names in different subroutines are just local aliases for positions in the shared memory block.

### Same Memory, Different Variable Names

```fortran
! In Subroutine A:
COMMON /BLK/ d1k, d2k, d3k    ! Maps: slot0=d1k, slot1=d2k, slot2=d3k

! In Subroutine B:
COMMON /BLK/ d3k, d2k, d1k    ! Maps: slot0=d3k, slot1=d2k, slot2=d1k
```

The COMMON block `/BLK/` is a **single contiguous memory region** of 12 bytes (3 REALs).

After Subroutine A sets values:
```
Memory: [1.0][2.0][3.0]
         ^    ^    ^
         |    |    |
Sub A:   d1k  d2k  d3k    (reads: d1k=1.0, d2k=2.0, d3k=3.0)
Sub B:   d3k  d2k  d1k    (reads: d1k=3.0, d2k=2.0, d3k=1.0)
```

**The variable names are just LOCAL ALIASES for byte positions in the shared memory!**

---

## 6. EQUIVALENCE Interaction

EQUIVALENCE overlays variables at the same memory location:

```fortran
COMMON /BLK/ d1k, d2k, d3k
DIMENSION delta(3)
EQUIVALENCE (d1k, delta)
```

This means:
- `delta(1)` shares memory with `d1k`
- `delta(2)` shares memory with `d2k`
- `delta(3)` shares memory with `d3k`

### EQUIVALENCE Restrictions with COMMON

1. **Cannot extend COMMON to the left**:
   ```fortran
   COMMON /X/ A
   REAL B(2)
   EQUIVALENCE (A, B(2))  ! ILLEGAL - would put B(1) before A
   ```

2. **CAN extend COMMON to the right**:
   ```fortran
   COMMON /X/ A
   REAL B(3)
   EQUIVALENCE (A, B(1))  ! LEGAL - B(2), B(3) extend past A
   ```

3. **Cannot merge two COMMON blocks**:
   ```fortran
   COMMON /X/ A
   COMMON /Y/ B
   EQUIVALENCE (A, B)  ! ILLEGAL
   ```

---

## 7. Compiler Flags for Alignment Control

### GNU gfortran
- `-fno-align-commons` (default): No padding, standard-compliant
- `-falign-commons`: Insert padding for natural alignment

### Intel Fortran
- `-align nocommons`: No padding
- `-align commons`: Align to natural boundaries (up to 4 bytes)
- `-align commons=8`: Align to 8-byte boundaries

### Oracle/Sun Fortran
- `-dalign`: Force 8-byte alignment
- Default: 4-byte max alignment in COMMON

---

## 8. Best Practices for Portable Code

### Order Variables by Decreasing Size
```fortran
! BAD:
COMMON /BLK/ i1, d8, i2, i4a, i4b
! Results in lots of padding or unaligned access

! GOOD:
COMMON /BLK/ d8, i4a, i4b, i2, i1
! Natural alignment, minimal/no padding
```

### Never Mix Character and Numeric
```fortran
! BAD - portability nightmare:
COMMON /BLK/ x, name, y
REAL x, y
CHARACTER*10 name

! GOOD - separate blocks:
COMMON /NUMS/ x, y
COMMON /CHARS/ name
```

---

## 9. Transpiler Implementation Options

For a transpiler targeting Go, there are several approaches:

### Option A: Standard-Compliant (Packed)
- No padding between variables
- Use Go's `unsafe` to read/write at exact byte offsets
- May cause performance issues with unaligned access

### Option B: Use Go Structs with Explicit Layout
- Use manual padding fields
- Match the most common Fortran compiler behavior (4-byte max in COMMON)

### Option C: Track Original Fortran Declaration Order
- Store COMMON block as raw `[]byte`
- Map variable access to byte offsets based on:
  - Declaration order in COMMON statement
  - Size of each variable
  - Alignment mode (packed vs aligned)

---

## Sources

- [Oracle FORTRAN 77 Size and Alignment](https://docs.oracle.com/cd/E19957-01/805-4939/c400041360f5/index.html)
- [Oracle FORTRAN 77 COMMON Statement](https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vn7v/index.html)
- [GNU g77 Aligned Data](https://gcc.gnu.org/onlinedocs/gcc-3.4.6/g77/Aligned-Data.html)
- [FORTRAN 77 COMMON Blocks Tutorial](https://www.obliquity.com/computer/fortran/common.html)
- [Intel Fortran Storage Units](https://www.cita.utoronto.ca/~merz/intel_f10b/main_for/mergedProjects/lref_for/source_files/pgcstuni.htm)
- [gfortran COMMON padding discussion](https://gcc.gnu.org/legacy-ml/fortran/2008-09/msg00231.html)
