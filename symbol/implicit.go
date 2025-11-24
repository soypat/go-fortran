package symbol

import (
	"fmt"
	"unicode"
)

// ApplyImplicitType determines the type for an identifier based on implicit typing rules.
// Returns the resolved type if implicit typing applies, or an error if the identifier
// is undeclared and IMPLICIT NONE is active.
func ApplyImplicitType(name string, rules *ImplicitRules) (*ResolvedType, error) {
	if name == "" {
		return nil, fmt.Errorf("cannot apply implicit type to empty name")
	}

	// Check if IMPLICIT NONE is active
	if rules.IsNone {
		return nil, fmt.Errorf("variable %s used without declaration (IMPLICIT NONE active)", name)
	}

	// Get first letter (case-insensitive)
	firstChar := rune(name[0])
	firstLetter := unicode.ToUpper(firstChar)

	// Validate it's a letter
	if firstLetter < 'A' || firstLetter > 'Z' {
		return nil, fmt.Errorf("identifier %s does not start with a letter", name)
	}

	// Look up implicit type for this letter
	typeStr, kind := GetImplicitTypeForLetter(firstLetter, rules)
	if typeStr == "" {
		return nil, fmt.Errorf("no implicit type defined for letter %c", firstLetter)
	}

	// Create resolved type
	resolvedType := &ResolvedType{
		BaseType: typeStr,
		Kind:     kind,
	}

	return resolvedType, nil
}

// GetImplicitTypeForLetter returns the type and kind for a given first letter
// based on the implicit typing rules. The letter should be uppercase A-Z.
func GetImplicitTypeForLetter(letter rune, rules *ImplicitRules) (typeStr string, kind int) {
	if letter < 'A' || letter > 'Z' {
		return "", 0
	}

	idx := letter - 'A'
	return rules.LetterTypes[idx], rules.LetterKinds[idx]
}

// ValidateImplicitNone checks if using an identifier would violate IMPLICIT NONE.
// Returns an error if IMPLICIT NONE is active, nil otherwise.
func ValidateImplicitNone(name string, rules *ImplicitRules) error {
	if rules.IsNone {
		return fmt.Errorf("variable %s used without declaration (IMPLICIT NONE active)", name)
	}
	return nil
}

// GetDefaultImplicitRules returns the default Fortran 77/90 implicit typing rules:
// I-N are INTEGER, A-H and O-Z are REAL.
func GetDefaultImplicitRules() *ImplicitRules {
	rules := &ImplicitRules{}

	// A-H: REAL
	for ch := 'A'; ch <= 'H'; ch++ {
		rules.LetterTypes[ch-'A'] = "REAL"
	}

	// I-N: INTEGER
	for ch := 'I'; ch <= 'N'; ch++ {
		rules.LetterTypes[ch-'A'] = "INTEGER"
	}

	// O-Z: REAL
	for ch := 'O'; ch <= 'Z'; ch++ {
		rules.LetterTypes[ch-'A'] = "REAL"
	}

	return rules
}
