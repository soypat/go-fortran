# Fortran Token Types: Non-String Literals and Identifiers

This document catalogs ALL Fortran 77/90 tokens for identifiers and non-string literals. Disambiguation will happen at FORMAT parsing level, not lexer level.

---

## 1. Integer Literals

| Format | Example | Notes |
|--------|---------|-------|
| Simple decimal | `123`, `0`, `42` | Basic integers |
| With kind specifier | `123_8`, `1_INT32` | Fortran 90 kind parameters |
| Negative (unary) | `-42` | Minus is separate token |

**Current Status**: Fully implemented in `lexer.go:369-389`

---

## 2. Real/Float Literals

| Format | Example | Precision |
|--------|---------|-----------|
| Simple decimal | `3.14`, `.5`, `1.` | Default |
| E exponent | `1.5E3`, `1E-4`, `.5E2` | Single precision |
| D exponent | `1.5D3`, `100.D0` | Double precision |
| Q exponent | `1.Q0`, `180.Q0` | Quad precision |
| With kind | `1.5_4`, `2.0_REAL64` | Explicit kind |

**Current Status**: Fully implemented in `lexer.go:611-682`

**Critical Disambiguation**: `1.E5` (float) vs `1.EQ.1` (comparison) handled via 2-char lookahead

---

## 3. BOZ Literals (Binary/Octal/Hex)

| Type | Formats | Example |
|------|---------|---------|
| Binary | `B'...'`, `b"..."` | `B'101'` = 5 |
| Octal | `O'...'`, `o"..."` | `O'777'` = 511 |
| Hex | `Z'...'`, `z"..."` | `Z'FF'` = 255 |
| Hex (alt) | `X'...'`, `x"..."` | `X'FF'` = 255 (some compilers) |

**Current Status**: Z, O, B implemented (`lexer.go:514-568`), X variant **NOT** implemented

**Note**: Currently converted to decimal IntLit, losing original representation

---

## 4. Complex Literals

| Format | Example |
|--------|---------|
| Pair notation | `(1.0, 2.0)`, `(a, b)` |

**Current Status**: Parser-level only (not a lexer token). Handled as `ParenExpr` with `Imag` field.

---

## 5. Logical Literals

| Format | Token |
|--------|-------|
| `.TRUE.` | `token.TRUE` |
| `.FALSE.` | `token.FALSE` |

**Current Status**: Fully implemented via `LookupDotOperator()`

---

## 6. Format Edit Descriptors - COMPLETE LIST

### Data Edit Descriptors (F77/F90)

| Descriptor | Purpose | Syntax | Example |
|------------|---------|--------|---------|
| **I** | Integer | `Iw`, `Iw.m` | `I3`, `I6.4` |
| **F** | Fixed-point real | `Fw.d` | `F10.2` |
| **E** | Exponential real | `Ew.d`, `Ew.dEe` | `E12.5`, `E12.5E3` |
| **ES** | Scientific (F90) | `ESw.d`, `ESw.dEe` | `ES12.5` |
| **EN** | Engineering (F90) | `ENw.d`, `ENw.dEe` | `EN15.6` |
| **D** | Double precision exp | `Dw.d`, `Dw.dEe` | `D20.7` |
| **G** | General (auto E/F) | `Gw.d`, `Gw.dEe` | `G15.8` |
| **A** | Character | `A`, `Aw` | `A`, `A8` |
| **L** | Logical | `Lw` | `L1` |
| **B** | Binary I/O | `Bw`, `Bw.m` | `B8` |
| **O** | Octal I/O | `Ow`, `Ow.m` | `O11` |
| **Z** | Hex I/O | `Zw`, `Zw.m` | `Z8` |

### Control Edit Descriptors (F77/F90)

| Descriptor | Purpose | Syntax |
|------------|---------|--------|
| **X** | Skip positions | `nX` |
| **T** | Tab to absolute column | `Tn` |
| **TL** | Tab left (relative) | `TLn` |
| **TR** | Tab right (relative) | `TRn` |
| **P** | Scale factor | `kP` |
| **/** | Record terminator | `/` |
| **:** | Conditional format terminator | `:` |
| **S** | Sign default | `S` |
| **SP** | Sign plus (always show +) | `SP` |
| **SS** | Sign suppress (no +) | `SS` |
| **SU** | Unsigned integer (F77 ext) | `SU` |
| **BN** | Blank null (ignore blanks) | `BN` |
| **BZ** | Blank zero (blanks = 0) | `BZ` |
| **H** | Hollerith | `nHchars` |
| **Q** | Remaining chars in record | `Q` |
| **R** | Radix control (F77 ext) | `nR`, `R` |
| **$** | Suppress carriage return | `$` |

### Fortran 2003+ Format Descriptors

| Descriptor | Purpose | Syntax |
|------------|---------|--------|
| **DC** | Decimal comma | `DC` |
| **DP** | Decimal point | `DP` |
| **RC** | Round compatible | `RC` |
| **RD** | Round down | `RD` |
| **RN** | Round nearest | `RN` |
| **RP** | Round processor-defined | `RP` |
| **RU** | Round up | `RU` |
| **RZ** | Round toward zero | `RZ` |
| **DT** | Derived type (F2003) | `DT'string'(v-list)` |
| **G0** | Minimal width (F2008) | `G0`, `G0.d` |

### Character String Edit Descriptors

| Syntax | Example |
|--------|---------|
| `'string'` | `'Result: '` |
| `"string"` | `"Value="` |

### Format with Repeat Counts

- `6I3` = repeat I3 six times
- `6ES12.5` = repeat ES12.5 six times
- `3(I3,F6.2)` = grouped repeat
- `*(format)` = unlimited repeat (F2008)

---

## 7. Hollerith Constants (Legacy)

| Format | Example | Meaning |
|--------|---------|---------|
| `nHchars` | `5HHELLO` | 5-character string "HELLO" |
| In DATA | `DATA A/4HTEST/` | Store chars in numeric var |

**Note**: Deprecated in F95+, but still widely supported. The number `n` must exactly match character count.

---

## 8. Identifiers

| Rule | Example |
|------|---------|
| Start with letter | `ABC`, `i`, `myVar` |
| Letters, digits, underscore | `ABC123`, `my_var`, `x1y2z3` |
| Case-insensitive | `ABC` = `abc` = `Abc` |
| Max 31 chars (F90) | F77 was 6 chars |

**Current Status**: Fully implemented in `lexer.go:505-512`

---

## 9. Dot Operators

### Built-in Operators

| Type | Operators |
|------|-----------|
| Comparison | `.EQ.`, `.NE.`, `.LT.`, `.LE.`, `.GT.`, `.GE.` |
| Logical | `.AND.`, `.OR.`, `.NOT.` |
| Equivalence | `.EQV.`, `.NEQV.` |
| Literals | `.TRUE.`, `.FALSE.` |

### User-Defined Operators (F90)

| Syntax | Example |
|--------|---------|
| `.name.` | `.CROSS.`, `.DOT.`, `.MYOP.` |

User-defined operators can be 1-63 letters between dots.

**Current Status**: Built-in operators implemented via `readDotOperator()` and `LookupDotOperator()`

---

## 10. Special Tokens That Look Like Literals

### Alternate Return Labels

| Syntax | Example | Context |
|--------|---------|---------|
| `*label` | `*100`, `*200` | CALL statement |

```fortran
CALL mysub(x, *100, *200)
```

### Statement Labels

| Syntax | Example | Context |
|--------|---------|---------|
| `nnnnn` | `100`, `99999` | Column 1-5 |

Labels are 1-5 digit integers at line start, distinct from integer literals.

---

## Complete Format Letter List (All Standards)

Single letters recognized in FORMAT context:
```
A B D E F G H I L O P Q R S T X Z
```

Two-letter combinations:
```
BN BZ DC DP DT EN ES G0 RC RD RN RP RU RZ SP SS SU TL TR
```

---

## Lexer Token Categories Summary

| Category | Token Type | Examples |
|----------|------------|----------|
| Integer | `IntLit` | `123`, `123_8`, `B'101'`, `Z'FF'` |
| Real | `FloatLit` | `3.14`, `1E3`, `1D0`, `1Q0` |
| Logical | `TRUE`, `FALSE` | `.TRUE.`, `.FALSE.` |
| Identifier | `Identifier` | `ABC`, `x1y2`, `my_var` |
| Format Spec | `FormatSpec` | `I3`, `F10.2`, `E12.5E3` |
| Dot Operator | Various | `.EQ.`, `.AND.`, `.CUSTOM.` |
| String | `StringLit` | `'hello'`, `"world"` |

---

## Ambiguous Tokens (Resolved at Parser Level)

| Token | Possible Interpretations |
|-------|-------------------------|
| `I3` | Identifier OR Format `Iw` |
| `E12` | Identifier OR Float 12.0 OR Format `Ew` |
| `6E2` | Float 600.0 OR Format repeat `6×Ew` |
| `6ES3` | Format only (ES not valid in numbers) |
| `D10` | Identifier OR Format `Dw` |
| `A8` | Identifier OR Format `Aw` |
| `T5` | Identifier OR Format Tab |
| `X` | Identifier OR Format skip |
| `Q` | Identifier OR Format remaining |
| `H` | Part of Hollerith OR identifier |

**Resolution**: Will be handled at FORMAT parsing level, NOT lexer level.

---

## Sources

- [Fortran Wiki - Edit descriptors](https://fortranwiki.org/fortran/show/Edit+descriptors)
- [Oracle FORTRAN 77 Format Specifiers](https://docs.oracle.com/cd/E19957-01/805-4939/z40007437a2e/index.html)
- [FORTRAN 77 FORMAT Edit Descriptors](https://www.obliquity.com/computer/fortran/format.html)
- [IBM DC/DP Decimal Editing](https://www.ibm.com/docs/en/xffbg/121.141?topic=descriptors-dc-dp-decimal-editing-fortran-2003)
- [IBM Round Editing RC/RD/RN/RP/RU/RZ](https://www.ibm.com/docs/en/openxl-fortran-aix/17.1.0?topic=ced-rc-rd-rn-rp-ru-rz-round-editing-fortran-2003)
- [Oracle Fortran 90 Kind Parameters](https://docs.oracle.com/cd/E19957-01/805-4939/z400073662be/index.html)
- [Typeless Constants](https://www.cenapad.unicamp.br/parque/manuais/Xlf/lr33.HTM)
