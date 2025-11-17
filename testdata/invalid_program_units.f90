! Test cases for invalid program unit definitions.

PROGRAM ! ERROR "expected program name, got NewLine"

SUBROUTINE ! ERROR "expected subroutine name, got NewLine"

FUNCTION ! ERROR "expected function name, got NewLine"

MODULE ! ERROR "expected Identifier, got NewLine"

RECURSIVE ! ERROR "expected SUBROUTINE or FUNCTION after attributes"

