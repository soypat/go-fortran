! Test cases for invalid program unit definitions.

PROGRAM ! ERROR "expected program name"
END PROGRAM
SUBROUTINE ! ERROR "expected subroutine name"
END SUBROUTINE

FUNCTION ! ERROR "expected function name"
END FUNCTION

MODULE ! ERROR "expected module name"
END MODULE



