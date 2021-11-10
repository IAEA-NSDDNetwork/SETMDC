      PROGRAM SETMDC
!
!***********************************************************************
!                                                                      *
!     PROGRAM SETMDC                                                   *
!                                                                      *
!     VERSIONS 1 AND 2 WERE FOR INTERNAL DEVELOPMENT.                  *
!     VERSION 3( 1) AS OF  7-DEC-77. FIRST DISTRIBUTION LEVEL.         *
!     VERSION 3( 2) AS OF  2-AUG-78. MAKE FILE 22 SEQOUT (DEC).        *
!     VERSION 3( 3) AS OF  7-FEB-80. ADD CLOSE + ERR= (DEC).           *
!     VERSION 4( 4) AS OF 16-JUN-81. USE DIALOG=LINE (DEC).            *
!     VERSION 5( 5) AS OF 11-AUG-82. DON'T CHANGE COLUMNS 73-80.       *
!     VERSION 6( 6) AS OF  6-DEC-84. CONVERT TO FORTRAN-77.            *
!     VERSION 6( 7) AS OF  6-DEC-84. ADD VMS AS MACHINE TYPE.          *
!     VERSION 6(10) AS OF 19-AUG-86. (VMS)OPEN STATEMENT.              *
!     VERSION 6(11) AS OF  2-DEC-86. ADD IbmPC MDC. NO BLANKS ADDED    *
!     VERSION 6(12) AS OF 30-SEP-87. Made selfcontained.               *
!     VERSION 6(13) AS OF  2-NOV-87. VMS mdc OPEN with READONLY for inp*
!     VERSION 6(14) AS OF    AUG-89. ANS code                          *
!     VERSION 6(15) AS OF 28-NOV-95. ALLOWED LOWER CASE MACHINE CODE   *
!     VERSION 6(16) AS OF 11-JAN-01. REVISED ENTRY OF INPUT            *
!     VERSION 6(17) AS OF 21-MAR-01. ALLOW F77 OR F90 SOURCE CODE      *
!     VERSION 6(18) AS OF 17-MAY-02. ALLOW F77 OR F95 SOURCE CODE      *
!                                                                      *
!      REFER ALL COMMENTS AND INQUIRIES TO                             *
!                                                                      *
!         NATIONAL NUCLEAR DATA CENTER                                 *
!         BUILDING 197D                                                *
!         BROOKHAVEN NATIONAL LABORATORY                               *
!         P.O. BOX 5000                                                *
!         UPTON, NY 11973-5000                                         *
!         USA                                                          *
!                                                                      *
!      TELEPHONE           631-344-2902                                *
!      E-MAIL              NNDC@BNL.GOV                                *
!                                                                      *
!***********************************************************************
!
!     THIS PROGRAM, AND ALL OTHER DISTRIBUTED SOFTWARE IN THIS PACKAGE,
!     HAS BEEN WRITTEN SO AS TO BE AS MACHINE INDEPENDENT AS POSSIBLE
!     THERE ARE FEATURES OF THE FORTRAN LANGUAGE WHICH ARE
!     NOT SUPPORTED BY ALL VENDORS. AS A RESULT, SOME SECTIONS OF THE
!     CODE HAD TO BE WRITTEN IN A MACHINE DEPENDENT MANNER. IN SUCH
!     CASES THE VARIANT CODES HAVE ALL BEEN INCLUDED WITH THE
!     INAPPROPRIATE CODE STORED AS COMMENTS. THE FORMAT OF THE MACHINE
!     DEPENDENT CODE SECTIONS IS DESCRIBED BELOW. PROGRAM SETMDC WILL
!     CONVERT PROGRAMS FROM ONE CONFIGURATION TO ANOTHER SO AS TO MAKE
!     MACHINE PORTABILITY AS SIMPLE AS POSSIBLE.
!
!     ALL MACHINE DEPENDENT CODE SECTIONS MUST HAVE THE SAME FORMAT.
!     EACH SECTION IS HEADED BY:
!
!         !+++MDC+++
!
!     AND TRAILED BY:
!
!         !---MDC---
!
!     CODE FOR ANY ONE MACHINE IS HEADED BY ONE OF:
!
!         !...VMS (VMS)
!         !...ANS (ANSI FORTRAN)
!         !...WIN (Fortran for Windows)
!         !...UNX (Fortran for UNIX)
!
!     THE PROGRAM RECOGNIZES ANY UNIQUE 3-LETTER CODE
!
!     CODE FOR ANY TWO OR MORE MACHINES IS HEADED BY A LIST OF THE
!     MACHINES, SEPARATED BY COMMAS, BLANKS AFTER COMMAS OPTIONAL.
!     FOR EXAMPLE:
!
!         !...VMS, WIN, ANS
!         !...ANS, VMS
!
!     ALL INAPPROPRIATE CODE IS SHIFTED RIGHT BY TWO COLUMNS AND IS
!     PRECEDED BY 'C/' IN COLUMNS 1 AND 2. THEREFORE, ALL MACHINE
!     DEPENDENT CODE SHOULD FIT IN 70 COLUMNS SO THAT SHIFTING WILL NOT
!     SHIFT OUT GOOD CODE.
!
!     FOR FORTRAN 77 FIXED FORMATTED CODE ALL "!"'S ARE REPLACED BY "C"
!       THE COMMENT FLAG IN FORTRAN 77
!
!     SEE PROGRAM CODE SETMDC FOR EXAMPLES.
!
!
      IMPLICIT NONE
!
!  LOCAL VARIABLES.
!
      CHARACTER(LEN=80)      FILE
      CHARACTER(LEN=3)       MACH,FORM
      INTEGER(KIND=4)        K,I
      CHARACTER(LEN=72)      CARD
      CHARACTER(LEN=8)       SEQ
      LOGICAL(KIND=4)        MDC
      LOGICAL(KIND=4)        CLEAR
      CHARACTER(LEN=72)      KARD
      CHARACTER(LEN=4)       CON1,CON2,CON3
      CHARACTER(LEN=2)       COM
!
!+++MDC+++
!...ANS
!/      INTEGER(KIND=4), PARAMETER :: IMDC = 0
!/      CHARACTER(LEN=*), PARAMETER :: TFMT = '(A)'
!...VMS
      INTEGER(KIND=4), PARAMETER :: IMDC = 1
      CHARACTER(LEN=*), PARAMETER :: TFMT = '(A,$)'
!...WIN
!/      INTEGER(KIND=4), PARAMETER :: IMDC = 2
!/      CHARACTER(LEN=*), PARAMETER :: TFMT = '(A,$)'
!... UNX
!/      INTEGER(KIND=4), PARAMETER :: IMDC = 3
!/      CHARACTER(LEN=*), PARAMETER :: TFMT = '(A,$)'
!---MDC---
      INTEGER(KIND=4), PARAMETER :: NIN = 21
      INTEGER(KIND=4), PARAMETER :: NOUT = 22
!
!  WRITE OUT PROGRAM HEADER.
!
      WRITE (UNIT=6, FMT=*)
      WRITE (UNIT=6, FMT=*)                                             &       
     &   'PROGRAM   S E T M D C   VERSION 6(18) AS OF MAY-02.'
!
!  OPEN INPUT AND OUTPUT FILES.
!
    5 WRITE (UNIT=6, FMT=*)
      WRITE (UNIT=6, FMT=TFMT) ' ENTER INPUT FILE NAME:        '
      READ  (UNIT=5, FMT='(A)') FILE
      OPEN  (UNIT=NIN,FILE=FILE,STATUS='OLD',ACTION='READ',ERR=100)
      WRITE (UNIT=6, FMT=TFMT) ' ENTER OUTPUT FILE NAME:       '
      READ  (UNIT=5, FMT='(A)') FILE
!+++MDC+++
!...VMS
      OPEN  (UNIT=NOUT, FILE=FILE, STATUS='NEW',                      &       
     &       CARRIAGECONTROL='LIST')
!...WIN, UNX
!/      OPEN  (UNIT=NOUT, FILE=FILE, STATUS='UNKNOWN',                    &       
!/     &       CARRIAGECONTROL='LIST')
!...ANS
!/      OPEN  (UNIT=NOUT, FILE=FILE, STATUS='UNKNOWN')
!---MDC---
!
!  OBTAIN COMPILER TYPE
!
      WRITE (UNIT=6, FMT=TFMT)                                          &       
     &                 ' COMPILER (ANS, VMS, etc):     '
      READ  (UNIT=5, FMT='(A)') MACH
      CALL UPSTR(MACH)
!
!  OBTAIN FORTRAN STANDARD
!
      WRITE (UNIT=6, FMT=TFMT)                                          &       
     &                 ' SOURCE FORMAT (F77 OR F95):   '
      READ  (UNIT=5, FMT='(A)') FORM
      CALL UPSTR(FORM)
      WRITE (UNIT=6, FMT=*)
!
!     SET CONTROL PARAMETERS BASE OF SOURCE STANDARD
!
      IF(FORM.EQ.'F95') THEN
         CON1 = '!+++'
         CON2 = '!...'
         CON3 = '!---'
         COM = '!/'
      ELSE
         CON1 = 'C+++'
         CON2 = 'C...'
         CON3 = 'C---'
         COM = 'C/'
      END IF
!
!  SET CARD COUNT TO ZERO.
!
      K = 0
      MDC = .FALSE.
!
!  RE&D NEXT INPUT CARD AND PROCESS IT.
!
   10 READ  (UNIT=NIN, FMT='(2A)', END=20) CARD, SEQ
      K = K + 1
      IF (MDC) THEN
         IF (CARD(1:4) .EQ. CON3) THEN
            MDC = .FALSE.
         ELSE IF (CARD(1:4) .EQ. CON2) THEN
            CLEAR = .FALSE.
            IF (INDEX(CARD, MACH) .GT. 0) CLEAR = .TRUE.
         ELSE
            KARD = CARD
            IF (CLEAR .AND. CARD(1:2) .EQ. COM)                         &       
     &         CARD = KARD(3:72) // '  '
            IF (.NOT. CLEAR .AND. CARD(1:2) .NE. COM)                   &       
     &         CARD = COM // KARD(1:70)
         ENDIF
      ELSE
         IF (CARD(1:4) .EQ. CON1) MDC = .TRUE.
      ENDIF
!
!   if no sequence no. write only nonblank part of the code
!
      IF(SEQ.EQ.' ') THEN
         I = LEN_TRIM(CARD)
         IF(I.EQ.0) I=1
         WRITE(UNIT=NOUT,FMT='(A)') CARD(1:I)
      ELSE
         WRITE (UNIT=NOUT, FMT='(2A)') CARD, SEQ
      ENDIF
      GOTO 10
!
!  CLOSE FILES.
!
   20 CLOSE (UNIT=NIN)
      CLOSE (UNIT=NOUT)
!
!  WRITE OUT FILE STATISTICS.
!
      WRITE (UNIT=6, FMT=*) K, ' RECORDS HAVE BEEN PROCESSED.'
!
!  FOR INTERACTIVE MACHINES, REPEAT IF NEEDED.
!
      IF(IMDC.NE.0) GO TO 5
!
  100 STOP
      END
!
!***********************************************************************
!
      SUBROUTINE UPSTR(STRING)
!
!     ROUTINE TO CONVERT A STRING TO ALL UPPER CASE
!
      CHARACTER(LEN=*) STRING
!
      L = LEN(STRING)
      DO I=1,L
         IC = ICHAR(STRING(I:I))
         IF(IC.GT.96.AND.IC.LT.123)   STRING(I:I) = CHAR(IC-32)
      END DO
!
      RETURN
      END
