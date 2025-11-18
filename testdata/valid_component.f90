SUBROUTINE test
	  TYPE person
	    INTEGER :: age
	  END TYPE person
	  TYPE(person) :: p
	  p%age = 30
    END SUBROUTINE