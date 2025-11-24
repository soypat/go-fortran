package symbol

import (
	"strings"
	"testing"
)

// TestGetDefaultImplicitRules verifies the default F77/F90 implicit typing rules
func TestGetDefaultImplicitRules(t *testing.T) {
	rules := GetDefaultImplicitRules()

	// Check I-N are INTEGER
	for letter := 'I'; letter <= 'N'; letter++ {
		typeStr, _ := GetImplicitTypeForLetter(letter, rules)
		if typeStr != "INTEGER" {
			t.Errorf("Letter %c: expected INTEGER, got %s", letter, typeStr)
		}
	}

	// Check A-H are REAL
	for letter := 'A'; letter <= 'H'; letter++ {
		typeStr, _ := GetImplicitTypeForLetter(letter, rules)
		if typeStr != "REAL" {
			t.Errorf("Letter %c: expected REAL, got %s", letter, typeStr)
		}
	}

	// Check O-Z are REAL
	for letter := 'O'; letter <= 'Z'; letter++ {
		typeStr, _ := GetImplicitTypeForLetter(letter, rules)
		if typeStr != "REAL" {
			t.Errorf("Letter %c: expected REAL, got %s", letter, typeStr)
		}
	}
}

// TestGetImplicitTypeForLetter tests the letter-to-type mapping
func TestGetImplicitTypeForLetter(t *testing.T) {
	rules := GetDefaultImplicitRules()

	tests := []struct {
		letter   rune
		wantType string
	}{
		{'I', "INTEGER"},
		{'J', "INTEGER"},
		{'K', "INTEGER"},
		{'N', "INTEGER"},
		{'A', "REAL"},
		{'X', "REAL"},
		{'Z', "REAL"},
	}

	for _, tt := range tests {
		typeStr, _ := GetImplicitTypeForLetter(tt.letter, rules)
		if typeStr != tt.wantType {
			t.Errorf("GetImplicitTypeForLetter(%c): expected %s, got %s",
				tt.letter, tt.wantType, typeStr)
		}
	}
}

// TestGetImplicitTypeForLetterInvalidInput tests edge cases
func TestGetImplicitTypeForLetterInvalidInput(t *testing.T) {
	rules := GetDefaultImplicitRules()

	invalidLetters := []rune{'@', '[', '0', '9', ' '}

	for _, letter := range invalidLetters {
		typeStr, kind := GetImplicitTypeForLetter(letter, rules)
		if typeStr != "" || kind != 0 {
			t.Errorf("GetImplicitTypeForLetter(%c): expected empty type, got %s", letter, typeStr)
		}
	}
}

// TestApplyImplicitType tests applying implicit typing to identifiers
func TestApplyImplicitType(t *testing.T) {
	rules := GetDefaultImplicitRules()

	tests := []struct {
		name     string
		wantType string
		wantErr  bool
	}{
		{"i", "INTEGER", false},
		{"INDEX", "INTEGER", false},
		{"N", "INTEGER", false},
		{"ncount", "INTEGER", false},
		{"x", "REAL", false},
		{"ALPHA", "REAL", false},
		{"temp", "REAL", false},
		{"Z99", "REAL", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			resolvedType, err := ApplyImplicitType(tt.name, rules)

			if tt.wantErr {
				if err == nil {
					t.Errorf("ApplyImplicitType(%s): expected error, got nil", tt.name)
				}
				return
			}

			if err != nil {
				t.Errorf("ApplyImplicitType(%s): unexpected error: %v", tt.name, err)
				return
			}

			if resolvedType == nil {
				t.Errorf("ApplyImplicitType(%s): got nil type", tt.name)
				return
			}

			if resolvedType.BaseType != tt.wantType {
				t.Errorf("ApplyImplicitType(%s): expected type %s, got %s",
					tt.name, tt.wantType, resolvedType.BaseType)
			}
		})
	}
}

// TestApplyImplicitTypeCaseInsensitive verifies case-insensitive behavior
func TestApplyImplicitTypeCaseInsensitive(t *testing.T) {
	rules := GetDefaultImplicitRules()

	// These should all resolve to INTEGER (first letter is I)
	names := []string{"i", "I", "index", "INDEX", "Index"}

	for _, name := range names {
		resolvedType, err := ApplyImplicitType(name, rules)
		if err != nil {
			t.Errorf("ApplyImplicitType(%s): unexpected error: %v", name, err)
			continue
		}

		if resolvedType.BaseType != "INTEGER" {
			t.Errorf("ApplyImplicitType(%s): expected INTEGER, got %s",
				name, resolvedType.BaseType)
		}
	}
}

// TestApplyImplicitTypeWithIMPLICITNONE tests that IMPLICIT NONE prevents implicit typing
func TestApplyImplicitTypeWithIMPLICITNONE(t *testing.T) {
	rules := &ImplicitRules{IsNone: true}

	_, err := ApplyImplicitType("x", rules)
	if err == nil {
		t.Error("ApplyImplicitType with IMPLICIT NONE: expected error, got nil")
	}

	if !strings.Contains(err.Error(), "IMPLICIT NONE") {
		t.Errorf("Expected error to mention IMPLICIT NONE, got: %v", err)
	}
}

// TestApplyImplicitTypeWithCustomRules tests custom IMPLICIT rules
func TestApplyImplicitTypeWithCustomRules(t *testing.T) {
	// Custom rule: IMPLICIT REAL (A-Z)
	rules := &ImplicitRules{}
	for ch := 'A'; ch <= 'Z'; ch++ {
		rules.LetterTypes[ch-'A'] = "REAL"
	}

	// Even I-N should be REAL now
	tests := []struct {
		name     string
		wantType string
	}{
		{"i", "REAL"},
		{"index", "REAL"},
		{"x", "REAL"},
		{"alpha", "REAL"},
	}

	for _, tt := range tests {
		resolvedType, err := ApplyImplicitType(tt.name, rules)
		if err != nil {
			t.Errorf("ApplyImplicitType(%s): unexpected error: %v", tt.name, err)
			continue
		}

		if resolvedType.BaseType != tt.wantType {
			t.Errorf("ApplyImplicitType(%s): expected %s, got %s",
				tt.name, tt.wantType, resolvedType.BaseType)
		}
	}
}

// TestApplyImplicitTypeWithKind tests that KIND parameter is preserved
func TestApplyImplicitTypeWithKind(t *testing.T) {
	// Custom rule with KIND: IMPLICIT REAL*8 (A-H)
	rules := &ImplicitRules{}
	for ch := 'A'; ch <= 'H'; ch++ {
		rules.LetterTypes[ch-'A'] = "REAL"
		rules.LetterKinds[ch-'A'] = 8
	}

	// Test with variable 'alpha' which starts with A (in A-H range)
	resolvedType, err := ApplyImplicitType("alpha", rules)
	if err != nil {
		t.Fatalf("ApplyImplicitType(alpha): unexpected error: %v", err)
	}

	if resolvedType.BaseType != "REAL" {
		t.Errorf("Expected type REAL, got %s", resolvedType.BaseType)
	}

	if resolvedType.Kind != 8 {
		t.Errorf("Expected kind 8, got %d", resolvedType.Kind)
	}
}

// TestApplyImplicitTypeEmptyName tests error handling for empty names
func TestApplyImplicitTypeEmptyName(t *testing.T) {
	rules := GetDefaultImplicitRules()

	_, err := ApplyImplicitType("", rules)
	if err == nil {
		t.Error("ApplyImplicitType with empty name: expected error, got nil")
	}
}

// TestApplyImplicitTypeNonLetterStart tests error handling for non-letter identifiers
func TestApplyImplicitTypeNonLetterStart(t *testing.T) {
	rules := GetDefaultImplicitRules()

	invalidNames := []string{"0abc", "9x", "_var"}

	for _, name := range invalidNames {
		_, err := ApplyImplicitType(name, rules)
		if err == nil {
			t.Errorf("ApplyImplicitType(%s): expected error for non-letter start, got nil", name)
		}
	}
}

// TestValidateImplicitNone tests the IMPLICIT NONE validation function
func TestValidateImplicitNone(t *testing.T) {
	tests := []struct {
		name    string
		isNone  bool
		wantErr bool
	}{
		{"x", true, true},   // IMPLICIT NONE active - should error
		{"x", false, false}, // IMPLICIT typing active - should pass
		{"index", true, true},
		{"alpha", false, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			rules := &ImplicitRules{IsNone: tt.isNone}
			err := ValidateImplicitNone(tt.name, rules)

			if tt.wantErr && err == nil {
				t.Errorf("ValidateImplicitNone(%s) with IsNone=%v: expected error, got nil",
					tt.name, tt.isNone)
			}

			if !tt.wantErr && err != nil {
				t.Errorf("ValidateImplicitNone(%s) with IsNone=%v: unexpected error: %v",
					tt.name, tt.isNone, err)
			}
		})
	}
}
