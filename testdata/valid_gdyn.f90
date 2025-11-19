PROGRAM GDYN2E
! &
! 
      COMMON/APHASE/NANT_sat
      DOUBLEPRECISION          :: DEFALT
      IF(LNORMP) CALL TITLE(IOUT15)
      IF(NPARC.LE.0.AND..NOT.LSTARC) GO TO 2000
      IF(LSTINR) WRITE(91) BIASP,BIAS,DYNEQ
      LEDIT_EXTRA(:) = .FALSE.
      IF(ISPACE-2) 500,1000,1500
       goto(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) ibeta
    1  continue
    2  continue
    4  continue
    5  continue
    7  continue
    8  continue
    9  continue
   11  continue
   12  continue
   13  continue
   14  continue
      IF(CARD(I:I).EQ.DIG(J:J)) THEN
      ENDIF
   WRITE(IOUT6,10109) TOTAL,DEL,(COVSCR(M,1),M=1,6)
END PROGRAM GDYN2E