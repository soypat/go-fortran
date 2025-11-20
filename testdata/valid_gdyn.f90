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
      IF(LSIMND) THEN
      CALL DIRALT(AA,II,&
! comment
     &           C3)
      ENDIF
      
      if (ALL( k==(/ 0 /) )) then
      end if
      ((k == (/ 0, 
     3  /)))
   if (k==1) then
   DO 3800 IT=1,5
      k = 0
 3800      CONTINUE
      endif
   DO 500 I=1,NPOINT
      DO 300 J=1,3
      VECTOR(J)=ECFSAT(J)-SVECT(I+JJ)
  300 END DO
      IF (IPNALB(I).EQ.1) THEN ! just short-wave radiation applies
         ACCEL=RAMS*APAREA*SFLUXSW(I)
      ELSEIF (IPNALB(I).EQ.2) THEN ! just long-wave radiation applies
         ACCEL=RAMS*APAREA*SFLUXLW(I)
      ELSE ! default: both
         ACCEL=RAMS*APAREA*SFLUX(I)
      ENDIF
      DO 400 J=1,3
      ACCSUM(J)=ACCSUM(J)+ACCEL*VECTOR(J)/DIST(I)
  400 END DO
  500 END DO
  READ(14,5000,END=500) CARD
  GO TO (1000,1300,1700,1900,2100,2300),MCALL
  IF(I2.GT.NDIMA) I2=I2-NDIMA
  IF=0  !apparently valid fortran
  IF (INDEX(TITLE(1,I),NAMES(I)).EQ.0) STOP = .TRUE.
END PROGRAM GDYN2E