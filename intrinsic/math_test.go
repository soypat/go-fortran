package intrinsic

import (
	"math"
	"testing"
)

const epsilon = 1e-6

func almostEqual(a, b, epsilon float64) bool {
	return math.Abs(a-b) < epsilon
}

// Test ABS function
func TestABS(t *testing.T) {
	// Integer tests
	if ABS(-5) != 5 {
		t.Errorf("ABS(-5) = %d, expected 5", ABS(-5))
	}
	if ABS(int32(-10)) != 10 {
		t.Errorf("ABS(int32(-10)) = %d, expected 10", ABS(int32(-10)))
	}

	// Float tests
	if ABS(-3.14) != 3.14 {
		t.Errorf("ABS(-3.14) = %f, expected 3.14", ABS(-3.14))
	}
	if ABS(float32(-2.5)) != 2.5 {
		t.Errorf("ABS(float32(-2.5)) = %f, expected 2.5", ABS(float32(-2.5)))
	}
}

// Test SQRT function
func TestSQRT(t *testing.T) {
	tests := []struct {
		input    float64
		expected float64
	}{
		{4.0, 2.0},
		{9.0, 3.0},
		{16.0, 4.0},
		{2.0, math.Sqrt(2.0)},
	}

	for _, tt := range tests {
		result := SQRT(tt.input)
		if !almostEqual(result, tt.expected, epsilon) {
			t.Errorf("SQRT(%f) = %f, expected %f", tt.input, result, tt.expected)
		}
	}

	// Test with float32
	result32 := SQRT(float32(16.0))
	if result32 != 4.0 {
		t.Errorf("SQRT(float32(16.0)) = %f, expected 4.0", result32)
	}
}

// Test trigonometric functions
func TestTrig(t *testing.T) {
	// SIN(0) = 0
	if !almostEqual(float64(SIN(0.0)), 0.0, epsilon) {
		t.Errorf("SIN(0) = %f, expected 0", SIN(0.0))
	}

	// SIN(π/2) = 1
	if !almostEqual(float64(SIN(math.Pi/2)), 1.0, epsilon) {
		t.Errorf("SIN(π/2) = %f, expected 1", SIN(math.Pi/2))
	}

	// COS(0) = 1
	if !almostEqual(float64(COS(0.0)), 1.0, epsilon) {
		t.Errorf("COS(0) = %f, expected 1", COS(0.0))
	}

	// TAN(π/4) ≈ 1
	if !almostEqual(float64(TAN(math.Pi/4)), 1.0, epsilon) {
		t.Errorf("TAN(π/4) = %f, expected 1", TAN(math.Pi/4))
	}
}

// Test inverse trigonometric functions
func TestInverseTrig(t *testing.T) {
	// ASIN(0) = 0
	if !almostEqual(float64(ASIN(0.0)), 0.0, epsilon) {
		t.Errorf("ASIN(0) = %f, expected 0", ASIN(0.0))
	}

	// ASIN(1) = π/2
	if !almostEqual(float64(ASIN(1.0)), math.Pi/2, epsilon) {
		t.Errorf("ASIN(1) = %f, expected π/2", ASIN(1.0))
	}

	// ACOS(1) = 0
	if !almostEqual(float64(ACOS(1.0)), 0.0, epsilon) {
		t.Errorf("ACOS(1) = %f, expected 0", ACOS(1.0))
	}

	// ATAN(1) = π/4
	if !almostEqual(float64(ATAN(1.0)), math.Pi/4, epsilon) {
		t.Errorf("ATAN(1) = %f, expected π/4", ATAN(1.0))
	}
}

// Test ATAN2
func TestATAN2(t *testing.T) {
	// ATAN2(1, 1) = π/4
	result := ATAN2(1.0, 1.0)
	if !almostEqual(float64(result), math.Pi/4, epsilon) {
		t.Errorf("ATAN2(1, 1) = %f, expected π/4", result)
	}

	// ATAN2(0, 1) = 0
	result = ATAN2(0.0, 1.0)
	if !almostEqual(float64(result), 0.0, epsilon) {
		t.Errorf("ATAN2(0, 1) = %f, expected 0", result)
	}

	// ATAN2(1, 0) = π/2
	result = ATAN2(1.0, 0.0)
	if !almostEqual(float64(result), math.Pi/2, epsilon) {
		t.Errorf("ATAN2(1, 0) = %f, expected π/2", result)
	}
}

// Test exponential and logarithmic functions
func TestExpLog(t *testing.T) {
	// EXP(0) = 1
	if !almostEqual(float64(EXP(0.0)), 1.0, epsilon) {
		t.Errorf("EXP(0) = %f, expected 1", EXP(0.0))
	}

	// EXP(1) = e
	if !almostEqual(float64(EXP(1.0)), math.E, epsilon) {
		t.Errorf("EXP(1) = %f, expected e", EXP(1.0))
	}

	// LOG(1) = 0
	if !almostEqual(float64(LOG(1.0)), 0.0, epsilon) {
		t.Errorf("LOG(1) = %f, expected 0", LOG(1.0))
	}

	// LOG(e) = 1
	if !almostEqual(float64(LOG(math.E)), 1.0, epsilon) {
		t.Errorf("LOG(e) = %f, expected 1", LOG(math.E))
	}

	// LOG10(10) = 1
	if !almostEqual(float64(LOG10(10.0)), 1.0, epsilon) {
		t.Errorf("LOG10(10) = %f, expected 1", LOG10(10.0))
	}

	// LOG10(100) = 2
	if !almostEqual(float64(LOG10(100.0)), 2.0, epsilon) {
		t.Errorf("LOG10(100) = %f, expected 2", LOG10(100.0))
	}
}

// Test hyperbolic functions
func TestHyperbolic(t *testing.T) {
	// SINH(0) = 0
	if !almostEqual(float64(SINH(0.0)), 0.0, epsilon) {
		t.Errorf("SINH(0) = %f, expected 0", SINH(0.0))
	}

	// COSH(0) = 1
	if !almostEqual(float64(COSH(0.0)), 1.0, epsilon) {
		t.Errorf("COSH(0) = %f, expected 1", COSH(0.0))
	}

	// TANH(0) = 0
	if !almostEqual(float64(TANH(0.0)), 0.0, epsilon) {
		t.Errorf("TANH(0) = %f, expected 0", TANH(0.0))
	}
}

// Test MOD function
func TestMOD(t *testing.T) {
	// Integer MOD
	if MOD(11, 3) != 2 {
		t.Errorf("MOD(11, 3) = %d, expected 2", MOD(11, 3))
	}
	if MOD(int32(-11), int32(3)) != -2 {
		t.Errorf("MOD(-11, 3) = %d, expected -2", MOD(int32(-11), int32(3)))
	}

	// Float MOD
	result := MODREAL(11.5, 3.0)
	expected := 2.5
	if !almostEqual(float64(result), expected, epsilon) {
		t.Errorf("MODREAL(11.5, 3.0) = %f, expected %f", result, expected)
	}
}

// Test SIGN function
func TestSIGN(t *testing.T) {
	// SIGN(5, 2) = 5 (positive sign)
	if SIGN(5, 2) != 5 {
		t.Errorf("SIGN(5, 2) = %d, expected 5", SIGN(5, 2))
	}

	// SIGN(5, -2) = -5 (negative sign)
	if SIGN(5, -2) != -5 {
		t.Errorf("SIGN(5, -2) = %d, expected -5", SIGN(5, -2))
	}

	// SIGN(-5, 2) = 5 (positive sign)
	if SIGN(-5, 2) != 5 {
		t.Errorf("SIGN(-5, 2) = %d, expected 5", SIGN(-5, 2))
	}

	// Float version
	if SIGN(3.14, -1.0) != -3.14 {
		t.Errorf("SIGN(3.14, -1.0) = %f, expected -3.14", SIGN(3.14, -1.0))
	}
}

// Test MAX and MIN functions
func TestMAX_MIN(t *testing.T) {
	// MAX with integers
	if MAX(1, 2, 3, 4, 5) != 5 {
		t.Errorf("MAX(1, 2, 3, 4, 5) = %d, expected 5", MAX(1, 2, 3, 4, 5))
	}
	if MAX(int32(10), int32(5), int32(20)) != 20 {
		t.Errorf("MAX(10, 5, 20) = %d, expected 20", MAX(int32(10), int32(5), int32(20)))
	}

	// MIN with integers
	if MIN(1, 2, 3, 4, 5) != 1 {
		t.Errorf("MIN(1, 2, 3, 4, 5) = %d, expected 1", MIN(1, 2, 3, 4, 5))
	}

	// MAX with floats
	if MAX(1.5, 2.3, 0.7) != 2.3 {
		t.Errorf("MAX(1.5, 2.3, 0.7) = %f, expected 2.3", MAX(1.5, 2.3, 0.7))
	}

	// MIN with floats
	if MIN(1.5, 2.3, 0.7) != 0.7 {
		t.Errorf("MIN(1.5, 2.3, 0.7) = %f, expected 0.7", MIN(1.5, 2.3, 0.7))
	}

	// Convenience wrappers
	if MaxInt32(10, 20, 5) != 20 {
		t.Errorf("MaxInt32(10, 20, 5) = %d, expected 20", MaxInt32(10, 20, 5))
	}
	if MinInt32(10, 20, 5) != 5 {
		t.Errorf("MinInt32(10, 20, 5) = %d, expected 5", MinInt32(10, 20, 5))
	}
}

// Test FLOOR and CEILING
func TestFLOOR_CEILING(t *testing.T) {
	// FLOOR
	if FLOOR(3.7) != 3.0 {
		t.Errorf("FLOOR(3.7) = %f, expected 3.0", FLOOR(3.7))
	}
	if FLOOR(-3.7) != -4.0 {
		t.Errorf("FLOOR(-3.7) = %f, expected -4.0", FLOOR(-3.7))
	}

	// CEILING
	if CEILING(3.2) != 4.0 {
		t.Errorf("CEILING(3.2) = %f, expected 4.0", CEILING(3.2))
	}
	if CEILING(-3.2) != -3.0 {
		t.Errorf("CEILING(-3.2) = %f, expected -3.0", CEILING(-3.2))
	}
}

// Test NINT, AINT, and ANINT
func TestRounding(t *testing.T) {
	// NINT - nearest integer
	if NINT(3.4) != 3 {
		t.Errorf("NINT(3.4) = %d, expected 3", NINT(3.4))
	}
	if NINT(3.6) != 4 {
		t.Errorf("NINT(3.6) = %d, expected 4", NINT(3.6))
	}
	if NINT(-3.6) != -4 {
		t.Errorf("NINT(-3.6) = %d, expected -4", NINT(-3.6))
	}

	// AINT - truncate toward zero
	if AINT(3.7) != 3.0 {
		t.Errorf("AINT(3.7) = %f, expected 3.0", AINT(3.7))
	}
	if AINT(-3.7) != -3.0 {
		t.Errorf("AINT(-3.7) = %f, expected -3.0", AINT(-3.7))
	}

	// ANINT - nearest whole number
	if ANINT(3.4) != 3.0 {
		t.Errorf("ANINT(3.4) = %f, expected 3.0", ANINT(3.4))
	}
	if ANINT(3.6) != 4.0 {
		t.Errorf("ANINT(3.6) = %f, expected 4.0", ANINT(3.6))
	}
}

// Test DIM function
func TestDIM(t *testing.T) {
	// Positive difference
	if DIM(10, 3) != 7 {
		t.Errorf("DIM(10, 3) = %d, expected 7", DIM(10, 3))
	}

	// When a <= b, return 0
	if DIM(3, 10) != 0 {
		t.Errorf("DIM(3, 10) = %d, expected 0", DIM(3, 10))
	}

	// Float version
	if DIM(5.5, 2.0) != 3.5 {
		t.Errorf("DIM(5.5, 2.0) = %f, expected 3.5", DIM(5.5, 2.0))
	}
}

// Test DPROD function
func TestDPROD(t *testing.T) {
	result := DPROD(float32(2.0), float32(3.0))
	expected := 6.0
	if result != expected {
		t.Errorf("DPROD(2.0, 3.0) = %f, expected %f", result, expected)
	}

	// Verify it returns float64
	var _ float64 = DPROD(float32(1.0), float32(1.0))
}

// Benchmark tests
func BenchmarkSQRT(b *testing.B) {
	for i := 0; i < b.N; i++ {
		_ = SQRT(float64(i))
	}
}

func BenchmarkMAX(b *testing.B) {
	for i := 0; i < b.N; i++ {
		_ = MAX(int32(i), int32(i+1), int32(i+2), int32(i+3))
	}
}

func BenchmarkABS(b *testing.B) {
	for i := 0; i < b.N; i++ {
		_ = ABS(int32(-i))
	}
}
