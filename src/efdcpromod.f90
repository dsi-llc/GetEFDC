    MODULE EFDCPROMOD
    ! ** AUTHOR: DH CHUNG

    USE GLOBALVARS
    USE INFOMOD

    IMPLICIT NONE

    CONTAINS

    SUBROUTINE EFDCPRO
    INTEGER(4)::NS,LL,I,J,L,K,K1,KK
    REAL(4)::ANG1,ANG2,ANG,XLNUTME,YLTUTMN,CCUE,CCVE,CCUN,CCVN,DZPC,MAXTHICK,DEPTHMIN,TMP,TMPCOR
    REAL(4),ALLOCATABLE:: THICK(:),DZC(:,:)
    REAL(4),ALLOCATABLE:: R1D(:)
    CHARACTER*80 TEXT

    HFREOUT = 0

    CALL SCANEFDC
    CALL VARALLOC

    LIJ = 1
    XCC = BLK
    YCC = BLK

    CALL INPUT      !GET ZB(IC,JC)

    LAYK   = MIN(KC,LAYK)
    LAYBED = MIN(KB,LAYBED)

    PRINT *,'*** READING LXLY.INP'
    OPEN(1,FILE=LXLYFILE,STATUS='OLD',ERR=998)
    CALL SKIPCOM(1, 'C')
    CUE=0
    CVE=0
    CUN=0
    CVN=0
    XMIN = 1E20
    YMIN = 1E20
    DO LL=1,LVC
        READ(1,*,ERR=999)I,J,XCC(LIJ(I,J)),YCC(LIJ(I,J)),CCUE,CCVE,CCUN,CCVN !,TMPVAL,TMPCOR
        L=LIJ(I,J)
        ANG1=ATAN2(CCUN,CCUE)
        ANG2=ATAN2(-CCVE,CCVN)
        ANG=0.5*(ANG1+ANG2)
        IF(SIGN(1.,ANG1).NE.SIGN(1.,ANG2))THEN
            IF(ABS(ANG1).GT.(1.57).OR.ABS(ANG2).GT.(1.57)) THEN  !(>pi/2)
                ! *** HANDLE THE DISCONTINUITY AT THE 180 DEGREES ANGLE
                ANG = ANG + ACOS(-1.0)
            ENDIF
        END IF
        CUE(L)= COS(ANG)
        CVE(L)=-SIN(ANG)
        CUN(L)= SIN(ANG)
        CVN(L)= COS(ANG)
        XMIN = MIN(XMIN,XCC(LIJ(I,J)))
        YMIN = MIN(YMIN,YCC(LIJ(I,J)))
    ENDDO
    XMAX = MAXVAL(XCC)+1
    YMAX = MAXVAL(YCC)+1
    CLOSE(1)

    KSZ = 1
    IF( IGRIDV > 0 )THEN
        ALLOCATE(R1D(KCM))
        R1D = 0.
        ! *** READ SGZ BOTTOM ACTIVE LAYER FOR SIGMA-ZED CELLS
        OPEN(1,FILE=SGZFILE,ACTION='READ',ERR=997)
        CALL SKIPCOM(1, 'C')
        if( IGRIDV == 1 )then
          DO LL=2,LA
            READ(1,*,END=1000) I, J, K
            L = LIJ(I,J)
            KSZ(L) = K  ! BED LAYER
          ENDDO
        else
          DO LL=2,LA
            READ(1,*,END=1000) I, J, K, (R1D(KK),KK = 1,KC)
            L = LIJ(I,J)
            KSZ(L) = K  ! BED LAYER
            DZPC = SUM(R1D(:))
            if( DZPC < 1.0 ) DZPC = 1.0
            DZCS(L,:) = R1D(:)/DZPC
          ENDDO
        endif
1000    CLOSE(1)
    ENDIF
    
    if( IGRIDV <= 1 )then
      ! **
      DZCS = 0
      DO L=2,LA
          DZPC = SUM(DZCK(KSZ(L):KC))
          DO K=KSZ(L),KC
              DZCS(L,K) = DZCK(K)/DZPC
          ENDDO
      ENDDO
    endif

    RETURN
997 PRINT*, ' **** OPEN ERROR: SGZFILE '
    PAUSE(1)
    STOP
998 PRINT*, ' **** OPEN ERROR: LXLY.INP '
    PAUSE(1)
    STOP
999 PRINT*, ' **** READ ERROR: LXLY.INP '
    PAUSE(1)
    STOP
    END SUBROUTINE

    SUBROUTINE SCANEFDC
    INTEGER(4)::N,NS,IMDXDY,KDUM,K,STAT,ITMP,I,ISO,J,LINE,LL,L,N1
    REAL(4)::DX,DY,DXYCVT,ZBRADJ,ZBRCVRT,HMIN,HADADJ,HCVRT,HDRY,HWET,RTMP
    CHARACTER(4)::NCRD
    CHARACTER*80 TEXT

    PRINT*,'*** READING EFDC.INP'
    OPEN(1,FILE=EFDCFILE,STATUS='OLD',ERR=998)

    NCRD='1A'
    CALL SEEK('C1A')
    READ(1,*,ERR=10,END=30)ITMP,ITMP,IGRIDV !,IS2TIM,ISHOUSATONIC

    !C2**  READ RESTART AND DIAGNOSTIC SWITCHES
    NCRD='C2'
    CALL SEEK('C2')
    !READ(1,*,IOSTAT=ISO) ISRESTI,ISRESTO,ISRESTR,ISGREGOR,ISLOG,ISDIVEX,ISNEGH,ISMMC,ISBAL,ICONTINUE,ISHOW
    READ(1,*,ERR=10,END=30) ISRESTI
    ! *** HANDLE BATHYMETRY ADJUSTMENTS
    !ISRESTIOPT = 0
    IF( ISRESTI == -1 )THEN
        !ISRESTIOPT = 1
        ISRESTI = 1
    ENDIF

    NCRD = 'C6'
    CALL SEEK('C6')
    DO N=0,8
        READ(1,*,ERR=10,END=30) ISTRAN(N),ITMP,ITMP,ITMP,ITMP,ITMP,ITMP,ITMP,ISCI(N)
    ENDDO

    NCRD = 'C9'
    CALL SEEK('C9')
    READ(1,*,ERR=10,END=30)IC,JC,LC,LVC,ISCLO,NDM,LDM,ISMASK,ISCONNECT
    IF(IC.GE.3)THEN
        ICM=IC+1
    ELSE
        PRINT*, 'IC MUST BE AT LEAST 3'
        PAUSE(1)
        STOP
    ENDIF
    IF(JC.GE.3)THEN
        JCM=JC+1
    ELSE
        PRINT*, 'IJ MUST BE AT LEAST 3'
        PAUSE(1)
        STOP
    ENDIF
    IF(LC.GE.3)THEN
        LCM=LC+1
    ELSE
        PRINT*, 'LC MUST BE AT LEAST 3'
        PAUSE(1)
        STOP
    ENDIF

    NCRD='C9A'
    CALL SEEK('C9A')
    READ(1,*,ERR=10,END=30)KC !,KSIG
    KC=ABS(KC)
    IF(KC.GE.1)THEN
        KCM=KC+1
    ELSE
        PRINT*,'KC MUST BE AT LEAST 1'
        PAUSE(1)
        STOP
    ENDIF

    KSM=KCM
    KS=MAX(KC-1,1)

    ALLOCATE(DZCK(KC))
    NCRD='C10'
    CALL SEEK('C10')
    DO K=1,KC
        READ(1,*,ERR=10,END=30)KDUM,DZCK(K)
    ENDDO

    NCRD = 'C11'
    CALL SEEK('C11')
    READ(1,*,ERR=10,END=30) DX,DY,DXYCVT,IMDXDY,ZBRADJ,ZBRCVRT,HMIN,HADADJ,HCVRT,HDRY,HWET,BELADJ,BELCVRT

    NCRD = 'C14'
    CALL SEEK('C14')
    READ(1,*,ERR=10,END=30) MTIDE,NWSER,NASER,ISGWIT,ISCHAN,ISWAVE

    NCRD='C16'
    CALL SEEK('C16')
    READ(1,*,ERR=10,END=30) NPBS,NPBW,NPBE,NPBN !,NPFOR,NPFORT,NPSER,PDGINIT

    NCRD = 'C22'
    N= SEEK_SCANLINE('C22','NDYE')
    IF(N.GT.0) THEN
        READ(1,*,ERR=10,END=30)NDYE,NTOX,NSED,NSND
    ELSE
        READ(1,*,ERR=10,END=30)NTOX,NSED,NSND
    ENDIF
    NTXM=MAX(1,NTOX)
    NSCM=MAX(1,NSED)
    NSNM=MAX(1,NSND)
    NSTM=MAX(3,NSCM+NSNM+NTXM)
    NSED2 = NSED
    
    IF(ISTRAN(8).GT.0)THEN
        LCMWQ=LCM
    ELSE
        LCMWQ=1
    ENDIF

    !C23*  READ VELOCITY, VOL SOUR/SINK, FLOW CONTROL, & WITHDRAW/RETURN DATA
    NCRD='C23'
    !CALL SEEK('C23')
    N= SEEK_SCANLINE('C23','NQCTLSER')
    IF(N.GT.0) THEN
        !READ(1,*,ERR=10,END=30) RTMP,RTMP,RTMP,NQCTL
        READ(1,*,ERR=10,END=30) ITMP,ITMP,ITMP,NQCTL,ITMP,ITMP,NQWR,ITMP,ITMP,NQCTLSER,NQCRULES
        !NQSIJ NQJPIJ   NQSER   NQCTL  NQCTLT  NHYDST    NQWR  NQWRSR   ISDIQ   NQCTLSER  NQCRULES
    ELSE
        READ(1,*,ERR=10,END=30) ITMP,ITMP,ITMP,NQCTL,ITMP,ITMP,NQWR,ITMP,ITMP
        !NQSIJ NQJPIJ   NQSER   NQCTL  NQCTLT  NHYDST    NQWR  NQWRSR   ISDIQ
    ENDIF
    ! *** SET KB AND CHECK FOR SEDZLJ USEAGE
    NSEDFLUME=0
    LSEDZLJ = .FALSE.
    IF(NSED.GT.0.OR.NSND.GT.0)THEN
        CALL SEEK('C36')
        READ(1,*,ERR=10,END=30)ISEDINT,ISEDBINT,ISEDWC,ISMUD,ISNDWC,ISEDVW,ISNDVW,KB,ISDTXBUG
        IF(KB.GE.1)THEN
            KBM=KB+1
        ELSE
            STOP 'KB MUST BE AT LEAST 1'
        ENDIF
        IF(ISTRAN(6)>0)THEN
            IF(ISEDWC==98)THEN
                LSEDZLJ = .TRUE.
                NSEDFLUME=1
            ELSEIF(ISEDWC==99)THEN
                LSEDZLJ = .TRUE.
                NSEDFLUME=2
            ENDIF
        ENDIF

        IF (ISTRAN(6)>0.OR.ISTRAN(7)>0) THEN
            NCRD = 'C36A'
            CALL SEEK('C36A')
            READ(1,*,ERR=10,END=30)ISBEDSTR
            NCRD = 'C37'
            !CALL SEEK('C37')
            TEXT= SEEKTEXT('C37')
            N= SCANLINE(TEXT,'C37','SEDSTART')
            N1= SCANLINE(TEXT,'C37','ISEDDT')
            IF(N.GT.0) THEN
                READ(1,*,ERR=10,END=30)ITMP,ITMP,IBMECH,IMORPH
            ELSEIF(N1.GT.0) THEN
                READ(1,*,ERR=10,END=30)ITMP,IBMECH,IMORPH
            ELSE
                READ(1,*,ERR=10,END=30)IBMECH,IMORPH
            ENDIF
        ENDIF

        ! *** LOOK FOR LEGACY APPROACH TO SET SEDZLJ, I.E. IWRSP(1)=98
        IF(.NOT.LSEDZLJ .AND. ISTRAN(6)>0)THEN
            ISDTXBUG=0
            NCRD = 'C40'
            CALL SEEK('C40')
            READ(1,*,ERR=10,END=30)ISDTXBUG
            IF(ISDTXBUG==98)THEN
                LSEDZLJ = .TRUE.
                NSEDFLUME=1
            ENDIF
        ENDIF
    ELSE
        KBM=1
    ENDIF

    NCRD = 'C42A'
    CALL SEEK('C42A')
    DO NS=1,NSND
        READ(1,*,ERR=10,END=30) ISBDLDBC
    ENDDO

    NCRD='46A'
    CALL SEEK('C46A')
    READ(1,*,ERR=10,END=30)ISICE !,ISICECOV,ISICETHK,NISER,ICETHKFUN,DYICEBEG,DYICEEND,DYICEM1,DYICEM2,RICETHKMAX,TEMPICE

    K=0
    EFDCVER = 730
    TBEDIT = 0
    DO N=1,1000
        READ(1,*,ERR=10,END=30) NCRD
        IF (NCRD(1:4)=='C46D'.AND.K==0) THEN
            K=1
        ELSEIF (NCRD(1:4)=='C46D'.AND.K==1) THEN
            READ(1,*,ERR=10,END=30) ITMP,(RTMP,I=1,6),TBEDIT
            EXIT
        ELSEIF(NCRD(1:3)=='C47') THEN
            EFDCVER = 720
            EXIT
        ENDIF
    ENDDO

    NCRD='67'
    CALL SEEK('C67')
    READ(1,*,ERR=10,END=30) ISPD  !,NPD,NPDRT,NWPD,ISLRPD,ILRPD1,ILRPD2,JLRPD1, JLRPD2, MLRPDRT,IPLRPD
    NPD = 0

    NCRD='71A'
    CALL SEEK('C71A')
    READ(1,*,ERR=10,END=30) ITMP,ISBEXP !,NPBPH

    NCRD = 'C88'
    CALL SEEK('C88')
    READ(1,*,ERR=10,END=30) HFREOUT

    CLOSE(1)

    ! ** MAPPING
    !IF(ISCONNECT.GE.1)THEN
    !  !PRINT *,'***   READING MAPPGNS.INP'
    !  OPEN(1,FILE=MAPGFILE,STATUS='UNKNOWN',ERR=999)
    !  DO I=1,8
    !    READ(1,*)
    !  ENDDO
    !  READ(1,*,IOSTAT=ISO) NPNSBP
    !  IF(ISO.GT.0) STOP 'READ ERROR FOR FILE MAPPGNS.INP'
    !  !DO NPNS=1,NPNSBP
    !  !  READ(1,*,IOSTAT=ISO) ISPNS(NPNS),JSPNS(NPNS),INPNS(NPNS),JNPNS(NPNS)
    !  !  IF(ISO.GT.0) STOP 'READ ERROR FOR FILE MAPPGNS.INP'
    !  !ENDDO
    !  CLOSE(1)
    !ENDIF

    ! **
    IF( ISGWIT == 2 )THEN
        WRITE(*,'(A)')'READING GWSER.INP'
        OPEN(1,FILE='gwser.inp',STATUS='UNKNOWN')
        ! *** SKIP OVER TITLE AND AND HEADER LINES
        CALL SKIPCOM(1, '*')
        READ(1,*) NGWSER
        CLOSE(1)
    ENDIF

    RETURN
10  WRITE(*,'(A)') 'EFDC.INP: ERROR READING '//NCRD
    PAUSE(1)
    STOP
30  WRITE(*,'(A)') 'UNEXPECTED END OF INPUT FILE'
    PAUSE(1)
    STOP
998 PRINT*, 'OPEN ERROR: EFDC.INP'
    PAUSE(1)
    STOP
999 PRINT*, 'OPEN ERROR: MAPGFILE.INP'
    PAUSE(1)
    STOP
    END SUBROUTINE

    SUBROUTINE VARALLOC
    ALLOCATE(IJCT(ICM,JCM))
    ALLOCATE(IL(LCM))
    ALLOCATE(JL(LCM))
    ALLOCATE(LNC(LCM),LSC(LCM),LEC(LCM),LWC(LCM))
    ALLOCATE(LNEC(LCM),LNWC(LCM),LSEC(LCM),LSWC(LCM))

    ALLOCATE(LIJ(ICM,JCM))
    ALLOCATE(LCT(LCM))

    ALLOCATE(CUE(LCM))
    ALLOCATE(CUN(LCM))
    ALLOCATE(CVE(LCM))
    ALLOCATE(CVN(LCM))
    ALLOCATE(XCC(LCM))
    ALLOCATE(YCC(LCM))
    ALLOCATE(BELV(LCM))
    ALLOCATE(BELVIC(LCM))
    
    !ALLOCATE(HP(LCM))
    ALLOCATE(HP0(LCM))

    ALLOCATE(RSSBCE(LCM))
    ALLOCATE(RSSBCN(LCM))
    ALLOCATE(RSSBCS(LCM))
    ALLOCATE(RSSBCW(LCM))

    !ALLOCATE(ISPNS(NPNSBP))
    !ALLOCATE(JSPNS(NPNSBP))
    !ALLOCATE(INPNS(NPNSBP))
    !ALLOCATE(JNPNS(NPNSBP))

    ALLOCATE(U(LCM,KCM))
    ALLOCATE(V(LCM,KCM))
    ALLOCATE(W(LCM,0:KCM))
    ALLOCATE(UK(LCM,KCM))
    ALLOCATE(VK(LCM,KCM))

    ALLOCATE(UA(LCM))
    ALLOCATE(VA(LCM))
    ALLOCATE(WA(LCM))

    !ALLOCATE(ZBOT(JC,IC),WL(JC,IC))
    ALLOCATE(WL(JC,IC))
    ALLOCATE(XCOR(LCM,5),YCOR(LCM,5),AREA(LCM))
    ALLOCATE(KSZ(LCM))
    ALLOCATE(DZCS(LCM,KCM))
    ALLOCATE(ARR(LCM))

    END SUBROUTINE

    SUBROUTINE INPUT
    INTEGER::L,IS,JCTMP,IACROSS,JACROSS,JT,JF,I,J,JLAST
    INTEGER(4)::ISO,K,IFIRST,ILAST,IT,LT,N
    INTEGER(4)::MASER(100),TCASER,IRELH
    INTEGER(4)::IASWRAD,M,NPNS,MVEGIJT,KBINPUT,ISALTYP,IBEDLAYU,IISTMP
    REAL(4)   ::TASER,PATM,TDRY,TWET,RAIN,EVAP,SOLSWR,CLOUD,TAASER
    REAL(4)   ::RAINCVT,EVAPCVT,SOLRCVT,CLDCVT,REVC,RCHC,SWRATNF,DABEDT,HTBED1,HTBED2,SWRATNS,FSWRATF
    REAL(4)   ::DXIJ,DYIJ,HIJ,BELVIJ

    CHARACTER ::ADUMMY*5
    CHARACTER*80 TEXT, STR*200

    ! *** READ CELL TYPES FROM FILES CELL.INP
    PRINT *,'*** READING CELL.INP'
    OPEN(1,FILE=CELLFILE,STATUS='UNKNOWN',ERR=995)
    CALL SKIPCOM(1, 'C')
    READ(1,'(A5,640I1)')ADUMMY
    READ(ADUMMY,*)JCTMP
    CLOSE(1)

    OPEN(1,FILE=CELLFILE,STATUS='UNKNOWN')
    CALL SKIPCOM(1, 'C')
    IF(JCTMP.NE.JC)THEN
        ! ***   READ OLD FILE FORMAT
        JACROSS=JC
        IF(JC.GT.640)JACROSS=640
        DO JT=1,JC,JACROSS
            JF=JT
            JLAST=JT+JACROSS-1
            IF(JLAST.GT.JC) JLAST=JC
            DO I=1,IC
                READ(1,'(640I1)',IOSTAT=ISO) (IJCT(I,J),J=JF,JLAST)
                IF(ISO.GT.0) STOP ' **** ERROR IN READING CELL.INP !'
            ENDDO
        ENDDO
    ELSE
        IF(IC.GT.640)THEN
            IACROSS=640
            DO IT=1,IC,IACROSS
                IFIRST=IT
                ILAST=IT+IACROSS-1
                IF(ILAST.GT.IC) ILAST=IC
                DO J=JC,1,-1
                    READ(1,'(A5,640I1)',IOSTAT=ISO)ADUMMY,(IJCT(I,J),I=IFIRST,ILAST)
                    IF(ISO.GT.0) STOP ' **** ERROR IN READING CELL.INP !'
                ENDDO
            ENDDO
        ELSE
            IFIRST=1
            ILAST=IC
            DO J=JC,1,-1
                READ(1,'(A5,640I1)',IOSTAT=ISO)ADUMMY,(IJCT(I,J),I=IFIRST,ILAST)
                IF(ISO.GT.0) STOP ' **** ERROR IN READING CELL.INP !'
            ENDDO
        ENDIF
    ENDIF
    CLOSE(1)

    IF (EFDCVER<730) THEN
        IF (ISTRAN(2)==1 .AND. NASER>0)THEN
            CALL GETASER
        ELSE
            TBEDIT = 0
        ENDIF
    ENDIF

    ! *** IF ISCONNECT GE 1, READ IN NORTH-SOUTH BOUNDARY CELLS FROM
    ! *** FILE MAPPGNS.INP TO SPECIFY A PERIODIC DOMAIN IN THE NORTH-SOUTH
    !IF(ISCONNECT.GE.1)THEN
    !  PRINT *,'***   READING MAPPGNS.INP'
    !  OPEN(1,FILE=MAPGFILE,STATUS='UNKNOWN',ERR=996)
    !  DO IS=1,8
    !    READ(1,*)
    !  ENDDO
    !  READ(1,*,IOSTAT=ISO) NPNSBP
    !  IF(ISO.GT.0) STOP 'READ ERROR FOR FILE MAPPGNS.INP'
    !  DO NPNS=1,NPNSBP
    !    READ(1,*,IOSTAT=ISO) ISPNS(NPNS),JSPNS(NPNS),INPNS(NPNS),JNPNS(NPNS)
    !    IF(ISO.GT.0) STOP 'READ ERROR FOR FILE MAPPGNS.INP'
    !  ENDDO
    !  CLOSE(1)
    !ENDIF

    ! *** IF ISCONNECT GE 2, READ IN EAST-WEST BOUNDARY CELLS FROM
    ! *** FILE MAPPGEW.INP TO SPECIFY A PERIODIC DOMAIN IN THE EAST-WEST DIRECTION
    IF( ISCONNECT >= 2 )THEN
        WRITE(*,'(A)')' *** READING MAPPGEW.INP'
        OPEN(1,FILE=MAPEWFILE,STATUS='UNKNOWN')

        ! *** SKIP OVER TITLE AND AND HEADER LINES
        DO IS=1,8
            READ(1,*)
        ENDDO
        READ(1,*,IOSTAT=ISO) NPEWBP
        IF( ISO > 0 ) STOP ' **** READ ERROR FOR FILE MAPPGEW.INP !'

        ALLOCATE(IWPEW(NPEWBP))
        ALLOCATE(JWPEW(NPEWBP))
        ALLOCATE(IEPEW(NPEWBP))
        ALLOCATE(JEPEW(NPEWBP))

        DO NP=1,NPEWBP
            READ(1,*,IOSTAT=ISO) IWPEW(NP),JWPEW(NP),IEPEW(NP),JEPEW(NP)
            IF( ISO > 0 ) STOP ' **** READ ERROR FOR FILE MAPPGEW.INP !'
        ENDDO
        CLOSE(1)
    ENDIF

    ! *** IF ISCONNECT GE 1, READ IN NORTH-SOUTH BOUNDARY CELLS FROM
    ! *** FILE MAPPGNS.INP TO SPECIFY A PERIODIC DOMAIN IN THE NORTH-SOUTH DIRECTION
    IF( ISCONNECT == 1 .OR. ISCONNECT == 3 )THEN
        WRITE(*,'(A)')' *** READING MAPPGNS.INP'
        OPEN(1,FILE=MAPNSFILE,STATUS='UNKNOWN')

        ! *** SKIP OVER TITLE AND AND HEADER LINES
        DO IS=1,8
            READ(1,*)
        ENDDO
        READ(1,*,IOSTAT=ISO) NPNSBP
        IF( ISO > 0 ) STOP ' **** READ ERROR FOR FILE MAPPGNS.INP !'

        ALLOCATE(ISPNS(NPNSBP))
        ALLOCATE(JSPNS(NPNSBP))
        ALLOCATE(INPNS(NPNSBP))
        ALLOCATE(JNPNS(NPNSBP))

        DO NP=1,NPNSBP
            READ(1,*,IOSTAT=ISO) ISPNS(NP),JSPNS(NP),INPNS(NP),JNPNS(NP)
            IF( ISO > 0 ) STOP ' **** READ ERROR FOR FILE MAPPGNS.INP !'
        ENDDO
        CLOSE(1)
    ENDIF

    CALL CELLMAP

    !IMN = MINVAL(IL(1:LA),IL>0)
    !JMN = MINVAL(JL(1:LA),JL>0)
    !IMX = MAXVAL(IL(1:LA))
    !JMX = MAXVAL(JL(1:LA))

22  FORMAT (A80)

    IF(LVC > 0)THEN
        PRINT *,'*** READING DXDY.INP'
        OPEN(1,FILE=DXDYFILE,STATUS='UNKNOWN',ERR=997)
        CALL SKIPCOM(1, 'C')
        DX =0
        DY =0

        DO LT=1,LVC
            READ(1,*,IOSTAT=ISO)I,J,DXIJ,DYIJ,HIJ,BELVIJ
            IF(ISO.GT.0) STOP ' **** READ ERROR FOR FILE DXDY.INP !'
            L=LIJ(I,J)
            BELV(L)=BELADJ + BELCVRT*BELVIJ
            BELVIC(L)=BELV(L)
            HP0(L) = HIJ
            DX = DX+DXIJ
            DY = DY+DYIJ
        ENDDO
        DX = DX/LVC
        DY = DY/LVC
    ENDIF

    ISGWIE=0
    IF(ISGWIT==1)THEN
        OPEN(1,FILE='gwater.inp',STATUS='UNKNOWN',ERR=998)
        CALL SKIPCOM(1,'*')
        READ(1,*) ISGWIE
        CLOSE(1)
    ENDIF
    !  ** SEDIMENT BED MECHANICAL INITIAL CONDITIONS

    ALLOCATE(BEDLINIT(LC,KB))
    BEDLINIT = 0.
    
    IISTMP=1
    IF( ISRESTI == 0) IISTMP=0
    IF( ISRESTI >= 1 .AND. ISCI(6) == 0) IISTMP=0
    IF( ISRESTI >= 1 .AND. ISCI(7) == 0) IISTMP=0
    IF( (ISTRAN(6)>0 .OR. ISTRAN(7) > 0 ) .AND. ISEDINT>=1 )THEN

        !  ** BED LAYER THICKNESS
        IF( IISTMP == 0 .AND. IBMECH >= 1 )THEN
            WRITE(*,'(A)')' *** READING BEDLAY.INP'
            OPEN(1,FILE='bedlay.inp',STATUS='UNKNOWN')

            ! ***   SKIP OVER TITLE AND AND HEADER LINES
            STR=READSTR(1)
            READ(1,*)IBEDLAYU,ISALTYP,KBINPUT
            IF( IBEDLAYU > 0 )THEN
                DO K=1,KB
                    DO L=2,LC-1
                        BEDLINIT(L,K)=0.0
                    ENDDO
                ENDDO
                IF( ISALTYP == 0 )THEN
                    DO L=2,LC-1
                        READ(1,*,IOSTAT=ISO) (BEDLINIT(L,K),K=1,KBINPUT)
                        IF( ISO > 0 ) STOP '  READ ERROR FOR FILE BEDLAY.INP'
                    ENDDO
                ELSE
                    DO L=2,LC-1
                        READ(1,*,IOSTAT=ISO)ADUMMY,ADUMMY,ADUMMY,(BEDLINIT(L,K),K=1,KBINPUT)
                        IF( ISO > 0 ) STOP '  READ ERROR FOR FILE BEDLAY.INP'
                    ENDDO
                ENDIF
            ENDIF
            CLOSE(1)
        ENDIF
    ENDIF
    IF(ISPD >= 2) CALL DRIFTERINP

    RETURN
995 PRINT*,' **** OPEN ERROR: CELL.INP !'
    PAUSE(1)
    STOP
996 PRINT*,' **** OPEN ERROR: MAPPGNS.INP !'
    PAUSE(1)
    STOP
997 PRINT*,' **** OPEN ERROR: DXDY.INP !'
    PAUSE(1)
    STOP
998 PRINT*,' **** OPEN ERROR: GWATER.INP !'
    PAUSE(1)
    STOP
    END SUBROUTINE

    SUBROUTINE CELLMAP
    INTEGER :: L,I,J,LCTT,NPN,LE,LW,LS,LN,NW,IGTMP,JGTMP,ICOMP,JCOMP,LTMP
    CHARACTER(200) :: STR

    ! **  SET 1D CELL INDEX SEQUENCE AND MAPPINGS
    L=2
    IL=0
    JL=0

    DO J=1,JC
        DO I=1,IC
            IF( IJCT(I,J) > 0 .AND. IJCT(I,J) < 9 )THEN
                IL(L)=I
                JL(L)=J
                LCT(L)=IJCT(I,J)
                LIJ(I,J)=L
                L=L+1
            ENDIF
        ENDDO
    ENDDO

    LA=L-1

    ! **  SET NORTH AND SOUTH CELL IDENTIFIER ARRAYS
    LNC(1)=LC
    LSC(1)=LC
    LNEC(1)=LC
    LNWC(1)=LC
    LSEC(1)=LC
    LSWC(1)=LC
    LNC(LC)=1
    LSC(LC)=1
    LNEC(LC)=1
    LNWC(LC)=1
    LSEC(LC)=1
    LSWC(LC)=1
    DO L=2,LA
        I=IL(L)
        J=JL(L)
        IF( IJCT(I,J+1) == 9 )THEN
            LNC(L)=LC
        ELSE
            LNC(L)=LIJ(I,J+1)
        ENDIF
        IF( IJCT(I,J-1) == 9 )THEN
            LSC(L)=LC
        ELSE
            LSC(L)=LIJ(I,J-1)
        ENDIF
        IF( IJCT(I+1,J+1) == 9 )THEN
            LNEC(L)=LC
        ELSE
            LNEC(L)=LIJ(I+1,J+1)
        ENDIF
        IF( IJCT(I-1,J+1) == 9 )THEN
            LNWC(L)=LC
        ELSE
            LNWC(L)=LIJ(I-1,J+1)
        ENDIF
        IF( IJCT(I+1,J-1) == 9 )THEN
            LSEC(L)=LC
        ELSE
            LSEC(L)=LIJ(I+1,J-1)
        ENDIF
        IF( IJCT(I-1,J-1) == 9 )THEN
            LSWC(L)=LC
        ELSE
            LSWC(L)=LIJ(I-1,J-1)
        ENDIF
    ENDDO

    ! *** CONNECT EAST-WEST CELLS
    LEC(1)=LC
    LWC(1)=LC
    LEC(LC)=1
    LWC(LC)=1
    DO L=2,LA
        I=IL(L)
        J=JL(L)
        IF( IJCT(I+1,J) == 9 )THEN
            LEC(L)=LC
        ELSE
            LEC(L)=LIJ(I+1,J)
        ENDIF
        IF( IJCT(I-1,J) == 9 )THEN
            LWC(L)=LC
        ELSE
            LWC(L)=LIJ(I-1,J)
        ENDIF
    ENDDO

    ! **  MODIFY EAST-WEST CELL MAPPING FOR PERIOD GRID IN E-W DIRECTION
    IF( ISCONNECT >= 2 )THEN
        DO NPN=1,NPEWBP
            L=LIJ(IWPEW(NPN),JWPEW(NPN))
            LWC(L)=LIJ(IEPEW(NPN),JEPEW(NPN))
            IF( IJCT(IEPEW(NPN),JEPEW(NPN)-1) == 9 )THEN
                LSWC(L)=LC
            ELSE
                LSWC(L)=LIJ(IEPEW(NPN),JEPEW(NPN)-1)
            ENDIF
            IF( IJCT(IEPEW(NPN),JEPEW(NPN)+1) == 9 )THEN
                LNWC(L)=LC
            ELSE
                LNWC(L)=LIJ(IEPEW(NPN),JEPEW(NPN)+1)
            ENDIF

            L=LIJ(IEPEW(NPN),JEPEW(NPN))
            LEC(L)=LIJ(IWPEW(NPN),JWPEW(NPN))
            IF( IJCT(IWPEW(NPN),JWPEW(NPN)-1) == 9 )THEN
                LSEC(L)=LC
            ELSE
                LSEC(L)=LIJ(IWPEW(NPN),JWPEW(NPN)-1)
            ENDIF
            IF( IJCT(IWPEW(NPN),JWPEW(NPN)+1) == 9 )THEN
                LNEC(L)=LC
            ELSE
                LNEC(L)=LIJ(IWPEW(NPN),JWPEW(NPN)+1)
            ENDIF
        ENDDO
    ENDIF

    ! **  MODIFY NORTH-SOUTH CELL MAPPING FOR PERIOD GRID IN N-S DIRECTION
    IF( ISCONNECT == 1 .OR. ISCONNECT == 3 )THEN
        DO NPN=1,NPNSBP
            LS=LIJ(ISPNS(NPN),JSPNS(NPN))
            LSC(LS)=LIJ(INPNS(NPN),JNPNS(NPN))
            IF( IJCT(INPNS(NPN)+1,JNPNS(NPN)) == 9 )THEN
                LSEC(LS)=LC
            ELSE
                LSEC(LS)=LIJ(INPNS(NPN)+1,JNPNS(NPN))
            ENDIF
            IF( IJCT(INPNS(NPN)-1,JNPNS(NPN)) == 9 )THEN
                LSWC(LS)=LC
            ELSE
                LSWC(LS)=LIJ(INPNS(NPN)-1,JNPNS(NPN))
            ENDIF
            LN=LIJ(INPNS(NPN),JNPNS(NPN))
            LNC(LN)=LIJ(ISPNS(NPN),JSPNS(NPN))
            IF( IJCT(ISPNS(NPN)+1,JSPNS(NPN)) == 9 )THEN
                LNEC(LN)=LC
            ELSE
                LNEC(LN)=LIJ(ISPNS(NPN)+1,JSPNS(NPN))
            ENDIF
            IF( IJCT(ISPNS(NPN)-1,JSPNS(NPN)) == 9 )THEN
                LNWC(LN)=LC
            ELSE
                LNWC(LN)=LIJ(ISPNS(NPN)-1,JSPNS(NPN))
            ENDIF
        ENDDO
    ENDIF

    END SUBROUTINE

    SUBROUTINE SEEK(TAG,STAT)
    INTEGER(4),INTENT(OUT),OPTIONAL::STAT
    CHARACTER TAG*(*)
    CHARACTER*80 TEXT
    INTEGER(4)::I,J,K,L,M

    IF (PRESENT(STAT)) STAT=1
    L=LEN(TAG)
    DO I=1,L
        J=ICHAR(TAG(I:I))
        IF(97.LE.J.AND.J.LE.122)THEN
            TAG(I:I)=CHAR(J-32)
        ENDIF
    ENDDO

    DO K=1,2
10      READ(1,'(A)',END=20)TEXT
        M=MAX(1,LEN_TRIM(TEXT))
        DO WHILE(M.GT.L.AND.TEXT(1:1).EQ.'')
            TEXT(1:M-1)=TEXT(2:M)
            TEXT(M:M)=' '
            M=M-1
        ENDDO
        IF(M.LT.L)GO TO 10
        DO I=1,M
            J=ICHAR(TEXT(I:I))
            IF(97.LE.J.AND.J.LE.122)THEN
                TEXT(I:I)=CHAR(J-32)
            ENDIF
        ENDDO
        IF(TEXT(1:L).NE.TAG)GO TO 10
        IF(TEXT(L+1:L+1).NE.' ')GO TO 10
    ENDDO
    RETURN
    20  WRITE(*,'(A,A,A)')'GROUP: ',TAG,' NOT FOUND BEFORE END OF FILE'
    END SUBROUTINE
    
    FUNCTION SEEKTEXT(TAG)  RESULT(TEXT)
        CHARACTER TAG*(*)
        CHARACTER*80 TEXT
        INTEGER(4)::I,J,K,L,M

        L=LEN(TAG)
        DO I=1,L
            J=ICHAR(TAG(I:I))
            IF(97.LE.J.AND.J.LE.122)THEN
                TAG(I:I)=CHAR(J-32)
            ENDIF
        ENDDO

        DO K=1,2
    10      READ(1,'(A)',END=20)TEXT
            M=MAX(1,LEN_TRIM(TEXT))
            DO WHILE(M.GT.L.AND.TEXT(1:1).EQ.'')
                TEXT(1:M-1)=TEXT(2:M)
                TEXT(M:M)=' '
                M=M-1
            ENDDO
            IF(M.LT.L)GO TO 10
            DO I=1,M
                J=ICHAR(TEXT(I:I))
                IF(97.LE.J.AND.J.LE.122)THEN
                    TEXT(I:I)=CHAR(J-32)
                ENDIF
            ENDDO
            IF(TEXT(1:L).NE.TAG)GO TO 10
            IF(TEXT(L+1:L+1).NE.' ')GO TO 10
        ENDDO
        RETURN
    20  WRITE(*,'(A,A,A)')'GROUP: ',TAG,' NOT FOUND BEFORE END OF FILE'
    END FUNCTION

    ! *** NDLuan Added-2018-12-04
    FUNCTION SEEK_SCANLINE(TAG1, TAG2)  RESULT(FOUND)
    CHARACTER TAG1*(*)
    CHARACTER TAG2*(*)
    CHARACTER*80 TEXT
    INTEGER(4)::I,J,K,L1,L2,M,FOUND

    L1=LEN(TAG1)
    DO I=1,L1
        J=ICHAR(TAG1(I:I))
        IF(97.LE.J.AND.J.LE.122)THEN
            TAG1(I:I)=CHAR(J-32)
        ENDIF
    ENDDO

    DO K=1,2
10      READ(1,'(A)',END=20)TEXT
        M=MAX(1,LEN_TRIM(TEXT))
        DO WHILE(M.GT.L1.AND.TEXT(1:1).EQ.'')
            TEXT(1:M-1)=TEXT(2:M)
            TEXT(M:M)=' '
            M=M-1
        ENDDO
        IF(M.LT.L1)GO TO 10
        DO I=1,M
            J=ICHAR(TEXT(I:I))
            IF(97.LE.J.AND.J.LE.122)THEN
                TEXT(I:I)=CHAR(J-32)
            ENDIF
        ENDDO
        IF(TEXT(1:L1).NE.TAG1)GO TO 10
        IF(TEXT(L1+1:L1+1).NE.' ')GO TO 10
    ENDDO
    FOUND=0
    L2=LEN(TAG2)

    DO I=1,L2
        J=ICHAR(TAG2(I:I))
        IF(97.LE.J.AND.J.LE.122)THEN
            TAG2(I:I)=CHAR(J-32)
        ENDIF
    ENDDO
    M=MAX(1,LEN_TRIM(TEXT))
    
    I = INDEX(TEXT,TAG2)
    IF (I.GT.0.AND.I.LE.M) THEN
        IF(TEXT(I:L2+I-1).EQ.TAG2) THEN
            FOUND=1
            RETURN
        ENDIF
    ENDIF

    RETURN
20  WRITE(*,'(A,A,A)')'GROUP: ',TAG1,' NOT FOUND BEFORE END OF FILE'
    END FUNCTION

    FUNCTION SCANLINE(TEXT,TAG1, TAG2)  RESULT(FOUND)
    CHARACTER TAG1*(*)
    CHARACTER TAG2*(*)
    CHARACTER TEXT*(*)
    INTEGER(4)::I,J,K,L1,L2,M,FOUND
    L1=LEN(TAG1)
    M=MAX(1,LEN_TRIM(TEXT))
    I = INDEX(TEXT,TAG1)
    IF (I.GT.0.AND.I.LE.M) THEN
        IF(TEXT(I:L1+I-1).EQ.TAG1) THEN
            FOUND=1
        ENDIF
    ENDIF
    IF( FOUND==1) THEN
        L2=LEN(TAG2)
        I = INDEX(TEXT,TAG2)
        IF (I.GT.0.AND.I.LE.M) THEN
            IF(TEXT(I:L2+I-1).EQ.TAG2) THEN
                FOUND=1
                RETURN
            ENDIF
        ENDIF
    ENDIF
    FOUND=0
    RETURN
    END FUNCTION

    SUBROUTINE GETASER
    INTEGER(4):: N,POS,LS,M
    INTEGER(4):: MASER,IRELH,IASERVER
    REAL(4)   :: VAL(10),TCASER,TAASER,RAINCVT,EVAPCVT,SOLRCVT,CLDCVT
    REAL(4)   :: DS_LONG,DS_LAT,WINDFA,WINDFB,WINDFC
    LOGICAL(4):: COMPUTESOLRAD,USESHADE
    CHARACTER(200)::STR

    OPEN(UNAS,FILE=ASERFILE,ACTION='READ',ERR=998)
    TBEDIT = 0

    ! ** ONLY FOR EFDC_70-71-72:
    CALL SKIPCOM(UNAS,'C')
    DO N=1,4
        READ(UNAS,'(A)') STR  ! READING EE LINES
    ENDDO
    CALL SKIPCOM(UNAS,'C')

    READ(UNAS,*,END=100,ERR=999) MASER,TCASER,TAASER,IRELH,RAINCVT,EVAPCVT,SOLRCVT,CLDCVT
    READ(UNAS,*,END=100,ERR=999) VAL

    TBEDIT = VAL(8)

100 CLOSE(UNAS)
    RETURN
998 PRINT*,' **** OPEN ERROR: ASERFILE !'
    PAUSE(1)
    STOP
999 STOP ' **** READING ASER.INP ERROR !'
    PAUSE(1)
    STOP
    END SUBROUTINE

    SUBROUTINE FREEMEM
    DEALLOCATE(IJCT )
    DEALLOCATE(IL )
    DEALLOCATE(JL )
    DEALLOCATE(LNC )
    DEALLOCATE(LSC )
    DEALLOCATE(LIJ )

    DEALLOCATE(CUE )
    DEALLOCATE(CUN )
    DEALLOCATE(CVE )
    DEALLOCATE(CVN )
    DEALLOCATE(BELV )
    DEALLOCATE(BELVIC )
    IF (ALLOCATED(HP))  DEALLOCATE(HP)
    IF (ALLOCATED(HP0)) DEALLOCATE(HP0)

    DEALLOCATE(RSSBCE )
    DEALLOCATE(RSSBCN )
    DEALLOCATE(RSSBCS )
    DEALLOCATE(RSSBCW )

    DEALLOCATE(ISPNS )
    DEALLOCATE(JSPNS )
    DEALLOCATE(INPNS )
    DEALLOCATE(JNPNS )

    DEALLOCATE(U)
    DEALLOCATE(V)
    DEALLOCATE(W)
    DEALLOCATE(UK)
    DEALLOCATE(VK)

    DEALLOCATE(XCC)
    DEALLOCATE(YCC)

    IF (ALLOCATED(HSIG)) DEALLOCATE(HSIG,DIR,RTP,RLS,EDIS)
    IF (ALLOCATED(SXX))  DEALLOCATE(SXX,SXY,SYY,WVENE)
    IF (ALLOCATED(XP))   DEALLOCATE(XP,YP,SXXP,SXYP,SYYP)
    IF (ALLOCATED(HSP))  DEALLOCATE(HSP,DIRP,RTPP,RLSP,EDISP)

    END SUBROUTINE

    SUBROUTINE DRIFTERINP
    ! ********************************************************************
    !READING INPUT DATA OF INITIAL LOCATIONS OF DRIFTERS
    !OUTPUT: NPD,XLA,YLA,ZLA,NP=1:NPD
    !        LA_BEGTI, LA_ENDTI, LA_FREQ,LANDT
    INTEGER    :: NP,N,K,NPN,IN,JN,IS,JS,ITMP
    REAL(RKD)  :: RANVAL,RTMP
    CHARACTER(200)  :: STR

    OPEN(ULOC,FILE=DRIFILE,ACTION='READ')
    CALL SKIPCOM(ULOC,'*')
    READ(ULOC,*) ! LA_ZCAL,LA_PRAN,LA_DIFOP,LA_HORDIF,LA_VERDIF,DEPOP,ADJVERP    ! AUG 2016: NEW STRUCTURE
    CALL SKIPCOM(ULOC,'*')
    READ(ULOC,*) LA_BEGTI0,LA_ENDTI0,LA_FREQ
    CALL SKIPCOM(ULOC,'*')
    READ(ULOC,*) NPD,NGRP
    !LA_FREQ = LA_FREQ/1440.                             !Output Frequency

    ! *** ALLOCATE THE DRIFTER ARRAYS
    ALLOCATE(XLA(NPD),YLA(NPD),ZLA(NPD),LLA(NPD))
    ALLOCATE(LA_BEGTI(NPD),LA_ENDTI(NPD),LA_GRP(NPD))
    ALLOCATE(ISOILSPI(NGRP),DDEN(NGRP),MOC(LA))
    ALLOCATE(DVOL(NPD))

    ! *** INIITALIZE THE ARRAYS (NPD)
    XLA=0.0
    YLA=0.0
    ZLA=0.0
    LLA = 0
    ZLA = 0
    MOC = 0

    CALL SKIPCOM(ULOC,'*')
    DO N=1,NGRP
        READ(ULOC,'(A)') STR
        READ(STR,*) K,ISOILSPI(K) !,GRPWS(K),BEDFIX(K)
        IF( ISOILSPI(K) > 0 )THEN
            READ(STR,*) K,ISOILSPI(K),RTMP,ITMP,RTMP,DDEN(K) !,DRAT(K),DTEM(K),DVAP(K),DVMO(K)
            !DRAT(K)=DRAT(K)/86400.
        ENDIF
    ENDDO

    CALL SKIPCOM(ULOC,'*')
    DO NP=1,NPD
        ! *** Read Elevations
        READ(ULOC,*,ERR=999) XLA(NP),YLA(NP),ZLA(NP),LA_BEGTI(NP),LA_ENDTI(NP),LA_GRP(NP)
        IF( LA_GRP(NP) < 1)LA_GRP(NP)=1
    ENDDO
    CLOSE(ULOC)

    LPTSNP = NINT((LA_ENDTI0-LA_BEGTI0)*1440._8/LA_FREQ) + 1

    RETURN
999 STOP 'DRIFTER.INP READING ERROR!'
    END SUBROUTINE

    END MODULE


