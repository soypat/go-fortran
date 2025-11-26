MODULE abc
    TYPE, PUBLIC :: module_type
    INTEGER :: value
    REAL :: data
    END TYPE module_type
END MODULE

SUBROUTINE f77_test()
      DO 10 i = 1, 10
10    CONTINUE
    END SUBROUTINE