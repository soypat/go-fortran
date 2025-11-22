PROGRAM GDYN2E
! &
!
      INTEGER ::  MAXDM1 =   6000000
      INTEGER :: ISTATAA
      use antphc_module
      POINTER (NPAA,AA(1)), (NPII,II(1)), (NPLL,LL(1))
      COMMON/TRQANG/DTQDAN(3,3,2)
      INCLUDE 'COMMON_DECL.inc'
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
   SELECT CASE (N)
   END SELECT
   READ ( IUNT14,81200,IOSTAT=IOS,END=60000)CDNAME,IFLD15,IA3, &
   &                                         IFLD78, FIELD
   if( ASAVE(isave)(1:1) .ne. ' ') k=1
   IF(1) RESULT(N)=1
   IF(LEXIST) OPEN(IUNT12,FILE='ftn12',STATUS='OLD',FORM='UNFORMATTED')
   BACKSPACE IUNT11
   READ(IUNT11) MAXDM1
   REWIND(IUNT11)
   CLOSE(IUNT11)
   STOP 69
   FORMAT(' ')
   ALLOCATE(IDWPT(NDIR))
   deallocate(IDWPT(NDIR))
   INQUIRE(FILE='ftn12', EXIST=LEXIST)
   DO 370 I1=1,6
      name = 'M1'' '
   370 CONTINUE
   POSPRT = REAL(NPERT, KIND=KIND(POSPRT))
   entry prthip(buffer,lu, filename, models )
   pointer(m) = k
   a=1.EQ.1
   ENDFILE IUNTMT
      data (ah_mean(i),i=1,55)/   &
     &+1.2517D+02, +8.503D-01, +6.936D-02, -6.760D+00, +1.771D-01,      &
     & +1.130D-02, +5.963D-01, +1.808D-02, +2.801D-03, -1.414D-03/
   dfac(1) = 1
   do i = 1,(2*n + 1)
      dfac(i+1) = dfac(i)*i
   end do
   if (func.lt.zero) return 1
   call ionlim (rd,pt,htrng(6),rlim1,rlim2,*200)
   CALL DSYSV('L',-1_INT32)
   ASSIGN 2000 TO IGOTO
   cycle satloop
END PROGRAM GDYN2E

SUBROUTINE PASYAW(NCARDS)
   10 READ(20,30,END=40) CARD
END SUBROUTINE

FUNCTION NMF_H()
   ! WRITE(A,1) I,(I3=1,3)
   ! END  =a(j)+d
   ! END FILE IUNTPF
   REWIND IUNTPF
   IUNTPF=IUNTPF+1
   TYPE='S1'
   ROW : DO J = 1,91   !NUMBER OF LATITUDE BANDS
   END DO ROW
   inquire( iolength = len ) date_plus_hour, vmf_array
   GO TO IGOTO,(500,2000)
!      if compiler does not accept "ASSIGN", use the lines below
!      IF( IGOTO == 500  ) go to 500
!      IF( IGOTO == 2000 ) go to 2000
   
END !


