PROGRAM blocky
  INTEGER A, B, C
  COMMON /BLOCK1/ A, B, C
END PROGRAM blocky

BLOCK DATA constants
	COMMON /data/ x, y
	REAL :: x, y
	DATA x /1.0/, y /2.0/
	END BLOCK DATA

BLOCK DATA init
	END BLOCK DATA

