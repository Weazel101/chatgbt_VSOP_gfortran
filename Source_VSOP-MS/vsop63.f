      SUBROUTINE ADAGE(IRB,JNN,NT)                                      ADA   10
C                                                                       ADA   20
      INTEGER*2 LOC,NON0,KD                                             ADA   30
C                                                                       ADA   40
      COMMON /MATRIX/ A(600),LOC(600),NON0(120),KD(120)                 ADA   50
C                                                                       ADA   60
      COMMON /EQ/ XZERO(120),XZH(120),XTEMP(120),XNEW(120),B(120),D(120)ADA   70
     1 ,XOLD(120)                                                       ADA   80
C                                                                       ADA   90
      COMMON /FLEX/ FLUX,MMN,MOUT,INDEX,QXN,AXN,ERR,NDUMMY,MZERO,ITMAX, ADA  100
     1 ILMAX,IAMAX,IFMAX,IZMAX                                          ADA  110
C                                                                       ADA  120
      COMMON /FLUXN/ TOCAP(120),FISS(120),DIS(120),ILITE,IACT,IFP,ITOT, ADA  130
     1 NON,INPT                                                         ADA  140
C                                                                       ADA  150
      COMMON /OUT/ NUCL(120),TITLE(20),Q(120),FG(120),CUTOFF(7),POW,    ADA  160
     1 BURNUP,FLUXB,MSTAR,ALPHAN(120),SPONF(120),ABUND(500),BASIS(10),  ADA  170
     2 TCONST,TUNIT,ZHM                                                 ADA  180
C                                                                       ADA  190
CFZJ031                                                       28.05.04  ADA  200
      COMMON /VSDATA/ NRKONZ,KFISS,N26,DELT,NB10,KMAT,AKONZ(30),        ADA  210
     1 MATNR(30),NR(30),FLAX(33),THERMA(30),RESA(30),FASTV(30),         ADA  220
     2 THERMF(30),RESF(30),FASTF(30),FASTAB(30),SIGMA(33,30),           ADA  230
     3 SIGMF(33,30)                                                     ADA  240
C                                                                       ADA  250
      COMMON /VSDA4/ NISO(30),IGAM,NRGAM(30),IDORG(200),THEB10,RESB10,  ADA  260
     1 RFAST                                                            ADA  270
C                                                                       ADA  280
      COMMON /IPO/ IR                                                   ADA  290
C                                                                       ADA  300
      COMMON /ORIGEN/ DUM(104),N200C                                    ADA  310
C                                                                       ADA  320
      DIMENSION AT(100,1000),LOCT(100,1000),NON0T(30,1000),KDT(30,1000),ADA  330
     1 TOCAPT(30,1000),FISST(30,1000),DIST(30,1000),BT(30,1000),GR(2)   ADA  340
C                                                                       ADA  350
      CHARACTER*3 GR/'GRO','UP '/                                       ADA  360
C                                                                       ADA  370
C                                                                       ADA  380
      ITMAX = 30                                                        ADA  390
      ILMAX = 0                                                         ADA  400
      IAMAX = 30                                                        ADA  410
      IFMAX = 0                                                         ADA  420
      IZMAX = 100                                                       ADA  430
      ZHM = 0.                                                          ADA  440
      IGAM = 1                                                          ADA  450
      INPT = 0                                                          ADA  460
      NDUMMY = 0                                                        ADA  470
      MMN = 1                                                           ADA  480
      MOUT = 1                                                          ADA  490
      INDEX = 1                                                         ADA  500
      QXN = 0.001                                                       ADA  510
      AXN = -ALOG(QXN)                                                  ADA  520
      BURNUP = 0.0                                                      ADA  530
      FLUXB = 0.0                                                       ADA  540
      POW = 0.0                                                         ADA  550
      TMO = 0.0                                                         ADA  560
      MZERO = 21                                                        ADA  570
      DO 100 I=1,ITMAX                                                  ADA  580
        B(I) = 0.0                                                      ADA  590
  100 CONTINUE                                                          ADA  600
C                                                                       ADA  610
C     IACT:   NUMBER OF ACTINIDES AS DEFINED BY THE DECAY LIBRARY       ADA  620
C     N26:    NUMBER OF NEUTRON ENERGY GROUPS                           ADA  630
C     XZERO:  NUCLIDE CONCENTRATIONS AT THE START OF BURNUP             ADA  640
C     DELT:   LENGTH OF BURNUP PERIOD                                   ADA  650
C                                                                       ADA  660
      FLUX = 0.                                                         ADA  670
      DO 200 N=1,N26                                                    ADA  680
        FLUX = FLUX + FLAX(N)                                           ADA  690
  200 CONTINUE                                                          ADA  700
      TCONST = 86400.                                                   ADA  710
      IF(N200C .GT. 1000) GOTO 400                                      ADA  720
      IF(JNN .GT. 1) GOTO 420                                           ADA  730
C                                                                       ADA  740
      CALL VSORG(NT)                                                    ADA  750
C                                                                       ADA  760
      CALL NUDATA                                                       ADA  770
C                                                                       ADA  780
      DO 410 I=1,IZMAX                                                  ADA  790
        AT(I,IRB) = A(I)                                                ADA  800
        LOCT(I,IRB) = LOC(I)                                            ADA  810
  410 CONTINUE                                                          ADA  820
      DO 411 I=1,IAMAX                                                  ADA  830
        NON0T(I,IRB) = NON0(I)                                          ADA  840
        KDT(I,IRB) = KD(I)                                              ADA  850
        TOCAPT(I,IRB) = TOCAP(I)                                        ADA  860
        FISST(I,IRB) = FISS(I)                                          ADA  870
        DIST(I,IRB) = DIS(I)                                            ADA  880
        BT(I,IRB) = B(I)                                                ADA  890
  411 CONTINUE                                                          ADA  900
      GOTO 440                                                          ADA  910
  420 CONTINUE                                                          ADA  920
      DO 430 I=1,IZMAX                                                  ADA  930
        A(I) = AT(I,IRB)                                                ADA  940
        LOC(I) = LOCT(I,IRB)                                            ADA  950
  430 CONTINUE                                                          ADA  960
      DO 431 I=1,IAMAX                                                  ADA  970
        NON0(I) = NON0T(I,IRB)                                          ADA  980
        KD(I) = KDT(I,IRB)                                              ADA  990
        TOCAP(I) = TOCAPT(I,IRB)                                        ADA 1000
        FISS(I) = FISST(I,IRB)                                          ADA 1010
        DIS(I) = DIST(I,IRB)                                            ADA 1020
        B(I) = BT(I,IRB)                                                ADA 1030
  431 CONTINUE                                                          ADA 1040
      GOTO 440                                                          ADA 1050
  400 CONTINUE                                                          ADA 1060
C                                                                       ADA 1070
      CALL VSORG(NT)                                                    ADA 1080
C                                                                       ADA 1090
      CALL NUDATA                                                       ADA 1100
C                                                                       ADA 1110
  440 CONTINUE                                                          ADA 1120
      IF(IR .EQ. 0) GOTO 301                                            ADA 1130
      IF(JNN .GT. 1) GOTO 304                                           ADA 1140
      WRITE (NT,302) (GR(1),GR(2),J,J=1,N26)                            ADA 1150
      WRITE (NT,303)                                                    ADA 1160
  304 CONTINUE                                                          ADA 1170
      WRITE (NT,300) JNN,FLUX,(FLAX(N),N=1,N26)                         ADA 1180
  301 CONTINUE                                                          ADA 1190
      DO 340 I=1,IACT                                                   ADA 1200
        XTEMP(I) = XZERO(I)                                             ADA 1210
        XOLD(I) = XZERO(I)                                              ADA 1220
        XNEW(I) = 0.0                                                   ADA 1230
  340 CONTINUE                                                          ADA 1240
C                                                                       ADA 1250
      CALL FLUXO(DELT)                                                  ADA 1260
C                                                                       ADA 1270
      CALL DECAY(1,DELT,ITOT)                                           ADA 1280
C                                                                       ADA 1290
      CALL TERM(1,DELT,ITOT)                                            ADA 1300
C                                                                       ADA 1310
      CALL EQUIL(1,ITOT)                                                ADA 1320
C                                                                       ADA 1330
      RETURN                                                            ADA 1340
C                                                                       ADA 1350
  300 FORMAT (I4,E12.5,8E13.5/(16X,8E13.5))                             ADA 1360
  302 FORMAT (//' FLUXES :'//' JNN',5X,'TOTAL',6X,8(2A3,I2,5X)/(20X,    ADA 1370
     1 8(2A3,I2,5X)))                                                   ADA 1380
  303 FORMAT (/)                                                        ADA 1390
      END                                                               ADA 1400
      SUBROUTINE VSORG(NT)                                              SOR   10
C                                                                       SOR   20
      COMMON /NUKDAT/ DDLAM(30),IIU(30),FFB1(30),FFP(30),FFP1(30),      SOR   30
     1 FFT(30),FFA(30),FFSF(30),FFNG1(30),FFN2N1(30),QMEV(30)           SOR   40
C                                                                       SOR   50
CFZJ031                                                       28.05.04  SOR   60
      COMMON /VSDATA/ NRKONZ,KFISS,N26,DELT,NB10,KMAT,AKONZ(30),        SOR   70
     1 MATNR(30),NR(30),FLAX(33),THERMA(30),RESA(30),FASTV(30),         SOR   80
     2 THERMF(30),RESF(30),FASTF(30),FASTAB(30),SIGMA(33,30),           SOR   90
     3 SIGMF(33,30),SIGMN2(33,30),FSTN2N(30)                            SOR  100
C                                                                       SOR  110
      COMMON /FLEX/ FLUX                                                SOR  120
C                                                                       SOR  130
      COMMON /FLUXN/ TOCAP(120),FISS(120),DIS(120),ILITE,IACT,IFP,ITOT, SOR  140
     1 NON,INPT                                                         SOR  150
C                                                                       SOR  160
      COMMON /VSDA4/ NISO(30),IGAM,NRGAM(30),IDORG(200),THEB10,RESB10,  SOR  170
     1 RFAST                                                            SOR  180
C                                                                       SOR  190
      COMMON /IPO/ IR                                                   SOR  200
C                                                                       SOR  210
C                                                                       SOR  220
C     CONSTRUCT 1-GROUP CROSS SECTIONS                                  SOR  230
C                                                                       SOR  240
      IF(FLUX .LE. 0.) GOTO 415                                         SOR  250
      DO 400 NUC=1,IACT                                                 SOR  260
        RESF(NUC) = 0.                                                  SOR  270
        RESA(NUC) = 0.                                                  SOR  280
        FSTN2N(NUC) = 0.                                                SOR  290
        DO 397 IG=1,N26                                                 SOR  300
          RESF(NUC) = RESF(NUC) + SIGMF(IG,NUC) * FLAX(IG)              SOR  310
          RESA(NUC) = RESA(NUC) + SIGMA(IG,NUC) * FLAX(IG)              SOR  320
          FSTN2N(NUC) = FSTN2N(NUC) + SIGMN2(IG,NUC) * FLAX(IG)         SOR  330
  397   CONTINUE                                                        SOR  340
        RESF(NUC) = RESF(NUC) / FLUX                                    SOR  350
        RESA(NUC) = RESA(NUC) / FLUX                                    SOR  360
        FSTN2N(NUC) = FSTN2N(NUC) / FLUX                                SOR  370
        IF(IR .EQ. 0) GOTO 400                                          SOR  380
        IF(NUC .EQ. 1) WRITE (NT,100)                                   SOR  390
        WRITE (NT,101) NUC,NRGAM(NUC),RESF(NUC),RESA(NUC),FSTN2N(NUC)   SOR  400
  400 CONTINUE                                                          SOR  410
  415 CONTINUE                                                          SOR  420
      RETURN                                                            SOR  430
C                                                                       SOR  440
  100 FORMAT (//' ISOTOP-NO.',2X,'GAM-NO.',5X,'SIG-F',8X,'SIG-A',7X,'SIGSOR  450
     1-N2N'/)                                                           SOR  460
  101 FORMAT (1X,I6,I11,E15.5,2E13.5)                                   SOR  470
      END                                                               SOR  480
      SUBROUTINE NUDATA                                                 UDA   10
C                                                                       UDA   20
      INTEGER*2 LOC,NON0,KD,KAP(120),MMAX(120),ELE(99),STA(2),NAME(3)   UDA   30
C                                                                       UDA   40
      DIMENSION COEFF(7,120),NPROD(7,120),CAPT(6),NUCAL(6)              UDA   50
C                                                                       UDA   60
      COMMON /LABEL/ ELE,STA                                            UDA   70
C                                                                       UDA   80
      COMMON /FLEX/ FLUX,MMN,MOUT,INDEX,QXN,AXN,ERR,NOBLND,MZERO,ITMAX, UDA   90
     1 ILMAX,IAMAX,IFMAX,IZMAX                                          UDA  100
C                                                                       UDA  110
      COMMON /FLUXN/ TOCAP(120),FISS(120),DIS(120),ILITE,IACT,IFP,ITOT, UDA  120
     1 NON                                                              UDA  130
C                                                                       UDA  140
      COMMON /OUT/ NUCL(120),TITLE(20),Q(120),FG(120),CUTOFF(7),POW,    UDA  150
     1 BURNUP,FLUXB,MSTAR,ALPHAN(120),SPONF(120)                        UDA  160
C                                                                       UDA  170
      COMMON /MATRIX/ A(600),LOC(600),NON0(120),KD(120)                 UDA  180
C                                                                       UDA  190
      COMMON /NUKDAT/ DDLAM(30),IIU(30),FFB1(30),FFP(30),FFP1(30),      UDA  200
     1 FFT(30),FFA(30),FFSF(30),FFNG1(30),FFN2N1(30),QMEV(30)           UDA  210
C                                                                       UDA  220
CFZJ031                                                       28.05.04  UDA  230
      COMMON /VSDATA/ NRKONZ,KFISS,N26,DELT,NB10,KMAT,AKONZ(30),        UDA  240
     1 MATNR(30),NR(30),FLAX(33),THERMA(30),RESA(30),FASTV(30),         UDA  250
     2 THERMF(30),RESF(30),FASTF(30),FASTAB(30),SIGMA(33,30),           UDA  260
     3 SIGMF(33,30),SIGMN2(33,30),FSTN2N(30)                            UDA  270
C                                                                       UDA  280
      COMMON /VSDA4/ NISO(30),IGAM,NRGAM(30),IDORG(200),THEB10,RESB10,  UDA  290
     1 RFAST                                                            UDA  300
C                                                                       UDA  310
      COMMON /DECAYT/ DECY(30)                                          UDA  320
C                                                                       UDA  330
      COMMON /IPO/ IR                                                   UDA  340
C                                                                       UDA  350
      EQUIVALENCE(A1,DLAM)                                              UDA  360
C                                                                       UDA  370
      DATA NUCAL/-20030,-10000,10,11,-10,-9/                            UDA  380
C                                                                       UDA  390
C                                                                       UDA  400
C     PROGRAM TO COMPUTE A MATRIX (TRANSITION MATRIX) FROM NUCLEAR DATA UDA  410
C                                                                       UDA  420
C     ERR = TRUNCATION ERROR LIMIT                                      UDA  430
C                                                                       UDA  440
C     GET DECAY DATA                                                    UDA  450
C                                                                       UDA  460
      IF(IACT .GT. IAMAX) WRITE (6,9043) IAMAX,IACT                     UDA  470
      IF(IR .GT. 1) WRITE (6,9024)                                      UDA  480
      DO 445 I=1,IACT                                                   UDA  490
        DLAM = DDLAM(I)                                                 UDA  500
        IU = IIU(I)                                                     UDA  510
        FB1 = FFB1(I)                                                   UDA  520
        FP = FFP(I)                                                     UDA  530
        FP1 = FFP1(I)                                                   UDA  540
        FT = FFT(I)                                                     UDA  550
        FA = FFA(I)                                                     UDA  560
        FSF = FFSF(I)                                                   UDA  570
        FNG1 = FFNG1(I)                                                 UDA  580
        FN2N1 = FFN2N1(I)                                               UDA  590
        M = 0                                                           UDA  600
        NUCLI = NUCL(I)                                                 UDA  610
        IF(NUCLI .EQ. 0) GOTO 450                                       UDA  620
C                                                                       UDA  630
        CALL HALF(A1,IU)                                                UDA  640
C                                                                       UDA  650
        CALL NOAH(NUCLI,NAME)                                           UDA  660
C                                                                       UDA  670
        DECY(I) = A1                                                    UDA  680
        SIGNG = 0.                                                      UDA  690
        SIGF = 0.                                                       UDA  700
        SIGN2N = 0.                                                     UDA  710
        SIGN3N = 0.                                                     UDA  720
C                                                                       UDA  730
C     GET CROSS SECTIONS                                                UDA  740
C                                                                       UDA  750
        SIGAB = RESA(I)                                                 UDA  760
        SIGF = RESF(I)                                                  UDA  770
        SIGN2N = FSTN2N(I)                                              UDA  780
        SIGNG = SIGAB - SIGF - SIGN2N - SIGN3N                          UDA  790
        IF(SIGNG .LT. 0.) SIGNG = 0.                                    UDA  800
        IF(IR .GT. 1) WRITE (6,9026) NAME,DLAM,FB1,FP,FP1,FT,FA,FSF,    UDA  810
     1   SIGNG,FNG1,SIGF,SIGN2N,SIGN3N                                  UDA  820
C                                                                       UDA  830
C     TEST RADIOACTIVITY                                                UDA  840
C                                                                       UDA  850
        IF(A1 .LT. ERR) GOTO 380                                        UDA  860
        ABETA = 1.0                                                     UDA  870
C                                                                       UDA  880
C     TEST POSITRON EMISSION                                            UDA  890
C                                                                       UDA  900
        IF(FP .LT. ERR) GOTO 350                                        UDA  910
        ABETA = ABETA - FP                                              UDA  920
        M = M + 1                                                       UDA  930
        COEFF(M,I) = FP * A1                                            UDA  940
        NPROD(M,I) = NUCLI - 10000                                      UDA  950
C                                                                       UDA  960
C     POSITRON EMISSION TO EXCITED STATE                                UDA  970
C                                                                       UDA  980
        IF(FP1 .LT. ERR) GOTO 350                                       UDA  990
        M = M + 1                                                       UDA 1000
        COEFF(M,I) = FP1 * COEFF(M-1,I)                                 UDA 1010
        NPROD(M,I) = NPROD(M-1,I) + 1                                   UDA 1020
        COEFF(M-1,I) = COEFF(M-1,I) - COEFF(M,I)                        UDA 1030
C                                                                       UDA 1040
C     ISOMERIC TRANSITION                                               UDA 1050
C                                                                       UDA 1060
  350   IF(FT .LT. ERR) GOTO 360                                        UDA 1070
        M = M + 1                                                       UDA 1080
        COEFF(M,I) = FT * A1                                            UDA 1090
        NPROD(M,I) = NUCLI                                              UDA 1100
        ABETA = ABETA - FT                                              UDA 1110
C                                                                       UDA 1120
C     ALPHA EMISSION                                                    UDA 1130
C                                                                       UDA 1140
  360   IF(FA .LT. ERR) GOTO 370                                        UDA 1150
        M = M + 1                                                       UDA 1160
        COEFF(M,I) = FA * A1                                            UDA 1170
        NPROD(M,I) = NUCLI - 20040                                      UDA 1180
        M = M + 1                                                       UDA 1190
        COEFF(M,I) = COEFF(M-1,I)                                       UDA 1200
        NPROD(M,I) = 20040                                              UDA 1210
        ABETA = ABETA - FA                                              UDA 1220
C                                                                       UDA 1230
C     BETA DECAY                                                        UDA 1240
C                                                                       UDA 1250
  370   IF(ABETA .LT. 1.E-4) GOTO 380                                   UDA 1260
        M = M + 1                                                       UDA 1270
        COEFF(M,I) = ABETA * A1                                         UDA 1280
        NPROD(M,I) = NUCLI + 10000                                      UDA 1290
        IF(FB1 .LT. ERR) GOTO 380                                       UDA 1300
        M = M + 1                                                       UDA 1310
        COEFF(M,I) = COEFF(M-1,I) * FB1                                 UDA 1320
        COEFF(M-1,I) = COEFF(M-1,I) - COEFF(M,I)                        UDA 1330
        NPROD(M,I) = NPROD(M-1,I) + 1                                   UDA 1340
C                                                                       UDA 1350
C     NEUTRON CAPTURE CROSS SECTIONS                                    UDA 1360
C                                                                       UDA 1370
  380   KAP(I) = M                                                      UDA 1380
        DO 390 K=1,6                                                    UDA 1390
          CAPT(K) = 0.0                                                 UDA 1400
  390   CONTINUE                                                        UDA 1410
        CAPT(2) = SIGNG * FNG1                                          UDA 1420
        CAPT(1) = SIGNG - CAPT(2)                                       UDA 1430
        CAPT(4) = SIGN2N * FN2N1                                        UDA 1440
        CAPT(3) = SIGN2N - CAPT(4)                                      UDA 1450
        FISS(I) = SIGF                                                  UDA 1460
        TOCAP(I) = 0.0                                                  UDA 1470
        DO 410 K=1,4                                                    UDA 1480
          CAPKI = CAPT(K)                                               UDA 1490
          IF(CAPKI .LT. ERR) GOTO 410                                   UDA 1500
          M = M + 1                                                     UDA 1510
          TOCAP(I) = TOCAP(I) + CAPKI                                   UDA 1520
          COEFF(M,I) = CAPKI                                            UDA 1530
          NPROD(M,I) = NUCLI + NUCAL(K+2)                               UDA 1540
  410   CONTINUE                                                        UDA 1550
        TOCAP(I) = TOCAP(I) + FISS(I)                                   UDA 1560
C                                                                       UDA 1570
C     N-3N CROSS SECTION                                                UDA 1580
C                                                                       UDA 1590
        A17 = SIGN3N                                                    UDA 1600
        IF(A17 .LT. ERR) GOTO 420                                       UDA 1610
        M = M + 1                                                       UDA 1620
        COEFF(M,I) = A17                                                UDA 1630
        NPROD(M,I) = NUCLI - 20                                         UDA 1640
        TOCAP(I) = TOCAP(I) + A17                                       UDA 1650
  420   IF(MOD(NUCLI,10) .EQ. 0) GOTO 440                               UDA 1660
        DO 430 K=1,M                                                    UDA 1670
          NPROD(K,I) = NPROD(K,I) - 1                                   UDA 1680
  430   CONTINUE                                                        UDA 1690
  440   MMAX(I) = M                                                     UDA 1700
        IF(M .GT. 7) WRITE (6,9039) M                                   UDA 1710
        DIS(I) = A1                                                     UDA 1720
  445 CONTINUE                                                          UDA 1730
  450 IL = 0                                                            UDA 1740
C                                                                       UDA 1750
C     ALL DATA ON NUCLIDES HAS BEEN READ, BEGIN TO COMPUTE MATRIX COEFF UDA 1760
C                                                                       UDA 1770
      IF(IFP .GT. IFMAX) WRITE (6,9044) IFMAX,IFP                       UDA 1780
      IF(ITOT .GT. ITMAX) WRITE (6,9045) ITMAX,ITOT                     UDA 1790
      NON = 0                                                           UDA 1800
      DO 700 K=1,ITOT                                                   UDA 1810
        NON0(K) = 0                                                     UDA 1820
  700 CONTINUE                                                          UDA 1830
C                                                                       UDA 1840
C     NON ZERO MATRIX ELEMENTS FOR THE ACTINIDES                        UDA 1850
C                                                                       UDA 1860
      IF(IACT .LT. 1) GOTO 820                                          UDA 1870
      IO = ILITE + 1                                                    UDA 1880
      I1 = ILITE + IACT                                                 UDA 1890
      DO 810 I=IO,I1                                                    UDA 1900
        NUCLI = NUCL(I)                                                 UDA 1910
        DO 780 J=IO,I1                                                  UDA 1920
          MAX = KAP(J)                                                  UDA 1930
          IF(MAX .LT. 1) GOTO 780                                       UDA 1940
          DO 770 M=1,MAX                                                UDA 1950
            IF(NUCLI .NE. NPROD(M,J)) GOTO 770                          UDA 1960
            NON0(I) = NON0(I) + 1                                       UDA 1970
            NON = NON + 1                                               UDA 1980
            IF(NON .GT. IZMAX) WRITE (6,9041) IZMAX,NON,NUCL(I)         UDA 1990
            A(NON) = COEFF(M,J)                                         UDA 2000
            JT = J                                                      UDA 2010
            LOC(NON) = JT                                               UDA 2020
  770     CONTINUE                                                      UDA 2030
  780   CONTINUE                                                        UDA 2040
        KD(I) = NON0(I)                                                 UDA 2050
        DO 800 J=IO,I1                                                  UDA 2060
          M1 = KAP(J) + 1                                               UDA 2070
          M2 = MMAX(J)                                                  UDA 2080
          IF(M2 .LT. M1) GOTO 800                                       UDA 2090
          DO 790 M=M1,M2                                                UDA 2100
            IF(NUCLI .NE. NPROD(M,J)) GOTO 790                          UDA 2110
            NON0(I) = NON0(I) + 1                                       UDA 2120
            NON = NON + 1                                               UDA 2130
            IF(NON .GT. IZMAX) WRITE (6,9041) IZMAX,NON,NUCL(I)         UDA 2140
            A(NON) = COEFF(M,J)                                         UDA 2150
            JT = J                                                      UDA 2160
            LOC(NON) = JT                                               UDA 2170
  790     CONTINUE                                                      UDA 2180
  800   CONTINUE                                                        UDA 2190
  810 CONTINUE                                                          UDA 2200
  820 CONTINUE                                                          UDA 2210
C                                                                       UDA 2220
C     TEMPORARILY WRITE OUT MATRIX ELEMENTS                             UDA 2230
C                                                                       UDA 2240
      IF(IR .LE. 1) GOTO 911                                            UDA 2250
      WRITE (6,9029)                                                    UDA 2260
      N = 0                                                             UDA 2270
      DO 910 I=1,ITOT                                                   UDA 2280
        NUM = NON0(I)                                                   UDA 2290
        IF(NUM .LE. 0) GOTO 910                                         UDA 2300
        N1 = N + NUM                                                    UDA 2310
        N = N + 1                                                       UDA 2320
        WRITE (6,9028)I,DIS(I),TOCAP(I),(A(K),LOC(K),K=N,N1)            UDA 2330
        N = N1                                                          UDA 2340
  910 CONTINUE                                                          UDA 2350
  911 CONTINUE                                                          UDA 2360
C                                                                       UDA 2370
C     ALL MATRIX ELEMENTS ARE NOW COMPUTED                              UDA 2380
C     BEGIN TRANSIENT SOLUTION                                          UDA 2390
C                                                                       UDA 2400
      DO 920 I=2,ITOT                                                   UDA 2410
        NON0(I) = NON0(I) + NON0(I-1)                                   UDA 2420
        KD(I) = KD(I) + NON0(I-1)                                       UDA 2430
  920 CONTINUE                                                          UDA 2440
      RETURN                                                            UDA 2450
C                                                                       UDA 2460
 9024 FORMAT (1H0,32X,'ACTINIDES AND THEIR DAUGHTERS'//'  NUCL    DLAM  UDA 2470
     1  FB1   FP    FP1   FT   FA    FSF E+6  SIGNG   FNG21    SIGF    SUDA 2480
     2IGN2N    SIGN3N')                                                 UDA 2490
 9026 FORMAT (1H ,A2,I3,A1,1PE9.2,5(0PF6.3),6PF9.1,1PE9.2,0PF6.3,       UDA 2500
     1 3(1PE9.2))                                                       UDA 2510
 9028 FORMAT (I5,2X,1PE10.3,3X,E10.3,5(2X,E10.3,3X,I5)/(30X,5(2X,E10.3, UDA 2520
     1 3X,I5)))                                                         UDA 2530
 9029 FORMAT ('1NON-ZERO MATRIX ELEMENTS AND THEIR LOCATIONS'/'  I      UDA 2540
     1DIS(I)       CAP(I)      A(I,J)       J    A(I,J)       J      A(IUDA 2550
     2,J)       J      A(I,J)       J      A(I,J)       J  ')           UDA 2560
 9039 FORMAT ('0  WARNING, M OUT OF RANGE IN NUDATA, =' ,I5)            UDA 2570
 9041 FORMAT ('0 NON HAS EXCEEDED',I5,', EQUAL TO ',2I6)                UDA 2580
 9043 FORMAT ('0 WARNING-NO OF ACTINIDES EXCEEDS',I4,', EQUAL TO ',I4)  UDA 2590
 9044 FORMAT ('0 WARNING-NO OF FISSION PRODUCTS EXCEEDS',I4,', EQUAL TO UDA 2600
     1',I4)                                                             UDA 2610
 9045 FORMAT ('0 WARNING-NO OF NUCLIDES EXCEEDS',I5,', EQUAL TO ',I4)   UDA 2620
      END                                                               UDA 2630
      SUBROUTINE HALF(A,I)                                              HAL   10
C                                                                       HAL   20
C     SUBROUTINE HALF CONVERTS HALF-LIFE TO DECAY CONSTANT (1/SEC)      HAL   30
C                                                                       HAL   40
      DIMENSION C(9)                                                    HAL   50
C                                                                       HAL   60
      DATA C/6.9315E-01,1.1552E-02,1.9254E-04,8.0226E-06,2.1965E-08,0.0,HAL   70
     1 2.1965E-11,2.1965E-14,2.1965E-17/                                HAL   80
C                                                                       HAL   90
C                                                                       HAL  100
      IF(A .GT. 0.0) GOTO 10                                            HAL  110
      IF(I .EQ. 6) GOTO 20                                              HAL  120
      A = 9.99                                                          HAL  130
      RETURN                                                            HAL  140
   10 A = C(I) / A                                                      HAL  150
      RETURN                                                            HAL  160
   20 A = 0.0                                                           HAL  170
      RETURN                                                            HAL  180
      END                                                               HAL  190
      SUBROUTINE NOAH(NUCLI,NAME)                                       NOA   10
C                                                                       NOA   20
C     SUBROUTINE NOAH CONVERTS SIX DIGIT IDENTIFIER TO ALPHAMERIC SYMBOLNOA   30
C                                                                       NOA   40
      INTEGER*2 NAME(3),ELE(99),STA(2)                                  NOA   50
C                                                                       NOA   60
      COMMON /LABEL/ ELE,STA                                            NOA   70
C                                                                       NOA   80
C                                                                       NOA   90
      IS = MOD(NUCLI,10) + 1                                            NOA  100
      NZ = NUCLI / 10000                                                NOA  110
      MW = NUCLI / 10 - NZ * 1000                                       NOA  120
      NAME(1) = ELE(NZ)                                                 NOA  130
      NAME(2) = MW                                                      NOA  140
      NAME(3) = STA(IS)                                                 NOA  150
      RETURN                                                            NOA  160
      END                                                               NOA  170
      SUBROUTINE FLUXO(DELT)                                            LUX   10
C                                                                       LUX   20
      INTEGER*2 LOC,NON0,KD                                             LUX   30
C                                                                       LUX   40
      LOGICAL*1 LONG                                                    LUX   50
C                                                                       LUX   60
      REAL XDOT(120)                                                    LUX   70
C                                                                       LUX   80
      DIMENSION XTEM(120)                                               LUX   90
C                                                                       LUX  100
      COMMON /DEBUGG/ AP(600)                                           LUX  110
C                                                                       LUX  120
      COMMON /SERIES/ XP(120),XPAR(120),LONG(120)                       LUX  130
C                                                                       LUX  140
      COMMON /FLEX/ FLUX,MMN,MOUT,INDEX,QXN,AXN,ERR,NOBLND,MZERO,ITMAX, LUX  150
     1 ILMAX,IAMAX,IFMAX,IZMAX                                          LUX  160
C                                                                       LUX  170
      COMMON /FLUXN/ TOCAP(120),FISS(120),DIS(120),ILITE,IACT,IFP,ITOT, LUX  180
     1 NON,INPT                                                         LUX  190
C                                                                       LUX  200
      COMMON /EQ/ XZERO(120),XZH(120),XTEMP(120),XNEW(120),B(120),D(120)LUX  210
     1 ,XOLD(120)                                                       LUX  220
C                                                                       LUX  230
      COMMON /MATRIX/ A(600),LOC(600),NON0(120),KD(120)                 LUX  240
C                                                                       LUX  250
      EQUIVALENCE(XDOT(1),XPAR(1)),(XTEM(1),AP(1))                      LUX  260
C                                                                       LUX  270
C                                                                       LUX  280
      M = 1                                                             LUX  290
C                                                                       LUX  300
C     MULTIPLY CROSS SECTIONS AND MATRIX ELEMENTS BY FLUX               LUX  310
C                                                                       LUX  320
      DO 10 J=1,ITOT                                                    LUX  330
        XTEM(J) = XTEMP(J)                                              LUX  340
   10 CONTINUE                                                          LUX  350
      FISSR = 0.0                                                       LUX  360
      IF(IACT .LT. 1) GOTO 30                                           LUX  370
      DO 20 I=1,IACT                                                    LUX  380
        L = I + ILITE                                                   LUX  390
        FISSR = FISSR + XTEMP(L) * FISS(I)                              LUX  400
   20 CONTINUE                                                          LUX  410
   30 CONTINUE                                                          LUX  420
      IF(FLUX .LT. ERR) FLUX = 1.                                       LUX  430
      FLUX = FLUX * 1.E-24                                              LUX  440
C                                                                       LUX  450
C     MULTIPLY CROSS SECTIONS BY FLUX                                   LUX  460
C                                                                       LUX  470
      N = 0                                                             LUX  480
      DO 70 I=1,ITOT                                                    LUX  490
        TOCAP(I) = TOCAP(I) * FLUX                                      LUX  500
        D(I) = -(DIS(I)+TOCAP(I))                                       LUX  510
        N = KD(I)                                                       LUX  520
        NDIFF = NON0(I) - KD(I)                                         LUX  530
        IF(NDIFF .EQ. 0) GOTO 70                                        LUX  540
        DO 60 L=1,NDIFF                                                 LUX  550
          N = N + 1                                                     LUX  560
          A(N) = A(N) * FLUX                                            LUX  570
   60   CONTINUE                                                        LUX  580
   70 CONTINUE                                                          LUX  590
C                                                                       LUX  600
C     COMPUTE CONCENTRATIONS AT THE BEGINNING OF THE INTERVAL           LUX  610
C                                                                       LUX  620
   90 IF(IACT .LT. 1) RETURN                                            LUX  630
      NB = 0                                                            LUX  640
      BIG = 0.0                                                         LUX  650
      IA = ILITE                                                        LUX  660
      DO 130 I=1,IACT                                                   LUX  670
        IA = IA + 1                                                     LUX  680
        DI = -D(IA)                                                     LUX  690
        IF(DI*DELT .LE. AXN) GOTO 120                                   LUX  700
        NZ = NON0(IA) - NB                                              LUX  710
        XNW = B(IA)                                                     LUX  720
        IF(NZ .LT. 1) GOTO 120                                          LUX  730
        DO 100 K=1,NZ                                                   LUX  740
          NB = NB + 1                                                   LUX  750
          J = LOC(NB)                                                   LUX  760
          XNW = XNW + A(NB) * XTEM(J)                                   LUX  770
  100   CONTINUE                                                        LUX  780
        XNW = XNW / DI                                                  LUX  790
        IF(XNW .LT. ERR) GOTO 110                                       LUX  800
        ARG = ABS((XNW-XTEM(IA))/XNW)                                   LUX  810
        IF(ARG .GT. BIG) BIG = ARG                                      LUX  820
  110   XTEM(IA) = XNW                                                  LUX  830
  120   NB = NON0(IA)                                                   LUX  840
  130 CONTINUE                                                          LUX  850
      IF(BIG .GT. QXN) GOTO 90                                          LUX  860
C                                                                       LUX  870
C     COMPUTE FIRST DERIVATIVE FOR TAYLOR SERIES EXPANSION              LUX  880
C                                                                       LUX  890
      FDOT = 0.0                                                        LUX  900
      L = 0                                                             LUX  910
      NUM = 0                                                           LUX  920
      DO 160 I=1,IACT                                                   LUX  930
        L = L + 1                                                       LUX  940
        NX = NON0(L) - NUM                                              LUX  950
        XDOTI = D(L) * XTEM(L)                                          LUX  960
        IF(NX .LT. 1) GOTO 155                                          LUX  970
        DO 150 N=1,NX                                                   LUX  980
          NUM = NUM + 1                                                 LUX  990
          J = LOC(NUM)                                                  LUX 1000
          XDOTI = XDOTI + A(NUM) * XTEM(J)                              LUX 1010
  150   CONTINUE                                                        LUX 1020
  155   XDOT(L) = XDOTI                                                 LUX 1030
  160 CONTINUE                                                          LUX 1040
      RETURN                                                            LUX 1050
      END                                                               LUX 1060
      SUBROUTINE DECAY(M,T,ITOT)                                        DEC   10
C                                                                       DEC   20
C     DECAY TREATS SHORT-LIVED ISOTOPES AT BEGINNING OF CHAINS USING    DEC   30
C     BATEMAN EQUATIONS                                                 DEC   40
C                                                                       DEC   50
      LOGICAL*1 LONG                                                    DEC   60
C                                                                       DEC   70
      REAL*8 BATE                                                       DEC   80
C                                                                       DEC   90
      INTEGER*2 LOC,NON0,KD,NQ,NQU,NQUEUE                               DEC  100
C                                                                       DEC  110
      COMMON /SERIES/ XP(120),XPAR(120),LONG(120)                       DEC  120
C                                                                       DEC  130
      COMMON /FLEX/ FLUX,MMN,MOUT,INDEX,QXN,AXN,ERR,NOBLND,MZERO,ITMAX, DEC  140
     1 ILMAX,IAMAX,IFMAX,IZMAX                                          DEC  150
C                                                                       DEC  160
      COMMON /EQ/ XZERO(120),XZH(120),XTEMP(120),XNEW(120),B(120),D(120)DEC  170
     1 ,XOLD(120)                                                       DEC  180
C                                                                       DEC  190
      COMMON /MATRIX/ A(600),LOC(600),NON0(120),KD(120)                 DEC  200
C                                                                       DEC  210
      COMMON /TERMD/ DD(100),DXP(100),QUEUE(50),NQU(50),NQUEUE(50),     DEC  220
     1 NQ(120)                                                          DEC  230
C                                                                       DEC  240
 9000 FORMAT ('1',4I5,E12.5)                                            DEC  250
C                                                                       DEC  260
C                                                                       DEC  270
      PRO1 = 0.                                                         DEC  280
      DO 10 I=1,ITOT                                                    DEC  290
        XPAR(I) = 0.0                                                   DEC  300
        LONG(I) = .FALSE.                                               DEC  310
        XPI = 0.0                                                       DEC  320
        DT = D(I) * T                                                   DEC  330
        IF(DT .LT. -37.) GOTO 11                                        DEC  340
        IF(ABS(DT) .LE. AXN) LONG(I) = .TRUE.                           DEC  350
        XPI = EXP(DT)                                                   DEC  360
   11   XP(I) = XPI                                                     DEC  370
   10 CONTINUE                                                          DEC  380
      NUL = 1                                                           DEC  390
      DO 160 L=1,ITOT                                                   DEC  400
        XTEM = 0.0                                                      DEC  410
        DL = -D(L)                                                      DEC  420
        NUM = NON0(L)                                                   DEC  430
        IF(M .GT. MMN .OR. M .EQ. MZERO) NUM = KD(L)                    DEC  440
        IF(NUM .LT. NUL) GOTO 150                                       DEC  450
        DO 140 N=NUL,NUM                                                DEC  460
          J = LOC(N)                                                    DEC  470
          DJ = -D(J)                                                    DEC  480
          IF(LONG(J)) GOTO 140                                          DEC  490
C                                                                       DEC  500
C     USE THIS FORM FOR TWO NEARLY EQUAL HALF-LIVES                     DEC  510
C                                                                       DEC  520
          IF(ABS(DL/DJ-1.0) .LE. 1.0E-5) XTEM = XTEM + XTEMP(J) * A(N) *DEC  530
     1     XP(J) * T                                                    DEC  540
C         UTEMP = XTEMP(J)                                              DEC  550
C         UFLOW = 1.0E+30 * XTEMP(J) * A(N) * (XP(J)-XP(L)) / (DL-DJ)   DEC  560
C         UFLOW = ABS(UFLOW)                                            DEC  570
C         IF(UFLOW .LT. 1.0) XTEMP(J) = 0.                              DEC  580
          IF(XTEMP(J) .LT. ERR) XTEMP(J) = 0.                           DEC  590
          IF(ABS(DL/DJ-1.0) .GT. 1.0E-5) XTEM = XTEM + XTEMP(J) * A(N) *DEC  600
     1     (XP(J)-XP(L)) / (DL-DJ)                                      DEC  610
C         XTEMP(J) = UTEMP                                              DEC  620
          QUE = A(N) / DJ                                               DEC  630
          NQ(L) = 0                                                     DEC  640
          NQ(J) = L                                                     DEC  650
          NSAVE = 0                                                     DEC  660
   20     NUX = NON0(J)                                                 DEC  670
          IF(M .GT. MMN .OR. M .EQ. MZERO) NUX = KD(J)                  DEC  680
          NUF = 1                                                       DEC  690
          IF(J .GT. 1) NUF = NON0(J-1) + 1                              DEC  700
          IF(NUF .GT. NUX) GOTO 130                                     DEC  710
          DO 120 K=NUF,NUX                                              DEC  720
            J1 = LOC(K)                                                 DEC  730
            IF(LONG(J1)) GOTO 120                                       DEC  740
            KP = J                                                      DEC  750
   30       IF(J1 .EQ. NQ(KP)) GOTO 120                                 DEC  760
            KP = NQ(KP)                                                 DEC  770
            IF(KP .NE. 0) GOTO 30                                       DEC  780
            DJ = -D(J1)                                                 DEC  790
            AKDJQ = A(K) / DJ * QUE                                     DEC  800
            IF(AKDJQ .LE. 1.0E-06) GOTO 120                             DEC  810
            NQ(J1) = J                                                  DEC  820
            I = 1                                                       DEC  830
            KP = J1                                                     DEC  840
   40       DD(I) = -D(KP)                                              DEC  850
            DXP(I) = XP(KP)                                             DEC  860
            KP = NQ(KP)                                                 DEC  870
            IF(KP .EQ. 0) GOTO 50                                       DEC  880
            I = I + 1                                                   DEC  890
            IF(I .LE. 100) GOTO 40                                      DEC  900
            WRITE (6,9000) M,L,J1,J,AKDJQ                               DEC  910
            GOTO 130                                                    DEC  920
   50       BATE = 0.D0                                                 DEC  930
            I1 = I - 1                                                  DEC  940
            XPL = XP(L)                                                 DEC  950
C                                                                       DEC  960
C     D R VONDY FORM OF BATEMAN EQUATIONS -- ORNL-TM-361                DEC  970
C                                                                       DEC  980
            DO 100 KB=1,I1                                              DEC  990
              XPJ = DXP(KB)                                             DEC 1000
              IF(XPL+XPJ .LT. ERR) GOTO 100                             DEC 1010
              DK = DD(KB)                                               DEC 1020
              PROD = (DL/DK-1.0)                                        DEC 1030
              DKR = PROD                                                DEC 1040
              IF(ABS(PROD) .GT. 1.E-4) GOTO 60                          DEC 1050
              PROD = T * DK * XPJ * (1.0-0.5*(DL-DK)*T)                 DEC 1060
              GOTO 70                                                   DEC 1070
   60         PROD = (XPJ-XPL) / PROD                                   DEC 1080
              PRO1 = XPJ / DKR                                          DEC 1090
   70         PI = 1.0                                                  DEC 1100
              S1 = 2. / (DK*T)                                          DEC 1110
              DO 90 JK=1,I1                                             DEC 1120
                IF(JK .EQ. KB) GOTO 90                                  DEC 1130
                S = 1.0 - DK / DD(JK)                                   DEC 1140
                IF(ABS(S) .GT. 1.E-4) GOTO 80                           DEC 1150
C                                                                       DEC 1160
C     USE THIS FORM FOR TWO NEARLY EQUAL HALF-LIVES                     DEC 1170
C                                                                       DEC 1180
                IF(ABS(DKR) .GT. 1.0E-4) PROD = PRO1                    DEC 1190
                S = S1                                                  DEC 1200
   80           PI = PI * S                                             DEC 1210
                IF(ABS(PI) .GT. 1.E25) GOTO 100                         DEC 1220
   90         CONTINUE                                                  DEC 1230
              BATE = BATE + PROD / PI                                   DEC 1240
  100       CONTINUE                                                    DEC 1250
            IF(BATE .LT. 0.D0) BATE = 0.D0                              DEC 1260
            XTEM = XTEM + XTEMP(J1) * AKDJQ * BATE                      DEC 1270
            IF(NSAVE .GE. 50) GOTO 120                                  DEC 1280
            NSAVE = NSAVE + 1                                           DEC 1290
            NQUEUE(NSAVE) = J1                                          DEC 1300
            QUEUE(NSAVE) = AKDJQ                                        DEC 1310
            NQU(NSAVE) = J                                              DEC 1320
  120     CONTINUE                                                      DEC 1330
  130     IF(NSAVE .LE. 0) GOTO 140                                     DEC 1340
          J = NQUEUE(NSAVE)                                             DEC 1350
          QUE = QUEUE(NSAVE)                                            DEC 1360
          NQ(J) = NQU(NSAVE)                                            DEC 1370
          NSAVE = NSAVE - 1                                             DEC 1380
          GOTO 20                                                       DEC 1390
  140   CONTINUE                                                        DEC 1400
        IF(LONG(L)) XPAR(L) = XTEM / XP(L)                              DEC 1410
  150   NUL = NON0(L) + 1                                               DEC 1420
C       UXP = XP(L)                                                     DEC 1430
C       UFLOW = 1.0E+60 * XTEMP(L) * XP(L)                              DEC 1440
C       UFLOW = ABS(UFLOW)                                              DEC 1450
C       IF(UFLOW .LT. 1.0) XP(L) = 0.                                   DEC 1460
        IF(.NOT. LONG(L)) XNEW(L) = XTEM + XTEMP(L) * XP(L)             DEC 1470
C       XP(L) = UXP                                                     DEC 1480
  160 CONTINUE                                                          DEC 1490
      DO 170 I=1,ITOT                                                   DEC 1500
        IF(LONG(I)) XTEMP(I) = XTEMP(I) + XPAR(I)                       DEC 1510
        IF(.NOT. LONG(I)) XTEMP(I) = 0.0                                DEC 1520
  170 CONTINUE                                                          DEC 1530
      RETURN                                                            DEC 1540
      END                                                               DEC 1550
      SUBROUTINE TERM(M,T,ITOT)                                         ERM   10
C                                                                       ERM   20
C     TERM ADDS ONE TERM TO EACH ELEMENT OF THE SOLUTION VECTOR         ERM   30
C     CSUM(J) IS THE CURRENT APPROXIMATION TO XNEW(J)                   ERM   40
C     CIM0(J) IS THE VECTOR CONTAINING THE LAST TERM ADDED TO EACH      ERM   50
C     ELEMENT OF CSUM(J)                                                ERM   60
C     CIMN(J) IS THE VECTOR CONTAINING 1/TON TIMES THE NEW TERM TO BE   ERM   70
C     ADDED TO CSUM(J)                                                  ERM   80
C     CIMN(J) IS GENERATED FROM CIM0(J) BY A RECURSION RELATION:        ERM   90
C      CIMN(J)= SUM OVER L OF (AP(J,L)*CIM0(L))                         ERM  100
C     AP(I,J) IS THE REDUCED TRANSITION MATRIX FOR THE LONG-LIVED       ERM  110
C     NUCLIDES                                                          ERM  120
C                                                                       ERM  130
      LOGICAL*1 LONG                                                    ERM  140
C                                                                       ERM  150
      INTEGER*2 LOC,NON0,KD,LOCP(600),NONP(120),NQ,NQU,NQUEUE           ERM  160
C                                                                       ERM  170
      REAL*8 BATE,BATM,CIMN(120),CSUM(120),CIMNI                        ERM  180
C                                                                       ERM  190
      DIMENSION AP(600),CIMB(120),CIM0(120),QUB(50)                     ERM  200
C                                                                       ERM  210
      COMMON /SERIES/ XP(120),XPAR(120),LONG(120)                       ERM  220
C                                                                       ERM  230
      COMMON /FLEX/ FLUX,MMN,MOUT,INDEX,QXN,AXN,ERR,NOBLND,MZERO,ITMAX, ERM  240
     1 ILMAX,IAMAX,IFMAX,IZMAX                                          ERM  250
C                                                                       ERM  260
      COMMON /EQ/ XZERO(120),XZH(120),XTEMP(120),XNEW(120),B(120),D(120)ERM  270
     1 ,XOLD(120)                                                       ERM  280
C                                                                       ERM  290
      COMMON /MATRIX/ A(600),LOC(600),NON0(120),KD(120)                 ERM  300
C                                                                       ERM  310
      COMMON /DEBUGG/ AP                                                ERM  320
C                                                                       ERM  330
      COMMON /TERMD/ DD(100),DXP(100),QUEUE(50),NQU(50),NQUEUE(50),     ERM  340
     1 NQ(120)                                                          ERM  350
C                                                                       ERM  360
 9000 FORMAT ('1TOO LONG A QUEUE HAS BEEN FORMED IN TERM',4I5,E12.5)    ERM  370
 9001 FORMAT ('1BATE IS NEGATIVE IN TERM. THERE ARE MORE THAN TWO SHORT-ERM  380
     1LIVED NUCLIDES IN A CHAIN WITH NEARLY EQUAL DIAGONAL ELEMENTS'/' LERM  390
     2,IM,BATE,BATM = ',2I5,2(1PE12.5))                                 ERM  400
 9002 FORMAT ('0MAXIMUM ERROR GT 0.002, =',F10.6,', TRACE = ',F10.4,' NLERM  410
     1ARGE = ',I6)                                                      ERM  420
C                                                                       ERM  430
C                                                                       ERM  440
      NUL = 0                                                           ERM  450
      NN = 0                                                            ERM  460
      PRO1 = 0.                                                         ERM  470
C                                                                       ERM  480
C     FIRST CONSTRUCT REDUCED TRANSITION MATRIX FOR LONG-LIVED ISOTOPES ERM  490
C                                                                       ERM  500
      DO 220 L=1,ITOT                                                   ERM  510
        IF(.NOT. LONG(L)) GOTO 210                                      ERM  520
        NUM = NON0(L)                                                   ERM  530
        IF(M .GT. MMN .OR. M .EQ. MZERO) NUM = KD(L)                    ERM  540
        CIMB(L) = B(L)                                                  ERM  550
        IF(NUM .LE. NUL) GOTO 210                                       ERM  560
        NS = NN + 1                                                     ERM  570
        N = NUL                                                         ERM  580
        NL = NUM - NUL                                                  ERM  590
        DO 200 N1=1,NL                                                  ERM  600
          N = N + 1                                                     ERM  610
          J = LOC(N)                                                    ERM  620
          DJ = -D(J)                                                    ERM  630
C                                                                       ERM  640
C     THIS IS A TEST TO SEE IF ONE OF THE ASSYMPTOTIC SOLUTIONS APPLIES ERM  650
C                                                                       ERM  660
          IF(.NOT. LONG(J)) GOTO 10                                     ERM  670
          NN = NN + 1                                                   ERM  680
          AP(NN) = A(N)                                                 ERM  690
          LOCP(NN) = J                                                  ERM  700
          GOTO 200                                                      ERM  710
C                                                                       ERM  720
C     GOING BACK UP THE CHAIN TO FIND A PARENT WHICH IS NOT IN          ERM  730
C     EQUILIBRIUM                                                       ERM  740
C                                                                       ERM  750
   10     NSAVE = 0                                                     ERM  760
          QUE = A(N) / DJ                                               ERM  770
          DRB = 1.0                                                     ERM  780
          CIMB(L) = CIMB(L) + QUE * B(J)                                ERM  790
          NQ(L) = 0                                                     ERM  800
          NQ(J) = L                                                     ERM  810
   20     NUX = NON0(J)                                                 ERM  820
          IF(M .GT. MMN .OR. M .EQ. MZERO) NUX = KD(J)                  ERM  830
          NUF = 0                                                       ERM  840
          IF(J .GT. 1) NUF = NON0(J-1)                                  ERM  850
          NX = NUX - NUF                                                ERM  860
          IF(NX .LT. 1) GOTO 190                                        ERM  870
          K = NUF                                                       ERM  880
          DO 180 KK=1,NX                                                ERM  890
            K = K + 1                                                   ERM  900
            J1 = LOC(K)                                                 ERM  910
            DJ = -D(J1)                                                 ERM  920
            KP = J                                                      ERM  930
   30       IF(J1 .EQ. NQ(KP)) GOTO 180                                 ERM  940
            KP = NQ(KP)                                                 ERM  950
            IF(KP .NE. 0) GOTO 30                                       ERM  960
            AKDJQ = QUE * A(K) / DJ                                     ERM  970
            IF(.NOT. LONG(J1)) GOTO 160                                 ERM  980
            TRM = 1.0 - XP(J1)                                          ERM  990
            IF(TRM .LT. 1.0E-6) GOTO 120                                ERM 1000
            NQ(J1) = J                                                  ERM 1010
            I = 1                                                       ERM 1020
            KP = J1                                                     ERM 1030
   40       DD(I) = -D(KP)                                              ERM 1040
            DXP(I) = XP(KP)                                             ERM 1050
            KP = NQ(KP)                                                 ERM 1060
            IF(KP .EQ. 0) GOTO 50                                       ERM 1070
            I = I + 1                                                   ERM 1080
            IF(I .LE. 100) GOTO 40                                      ERM 1090
C                                                                       ERM 1100
C     IF QUEUE OF SHORT-LIVED NUCLIDES EXCEEDS 100 ISOTOPES, TERMINATE  ERM 1110
C     CHAIN AND WRITE MESSAGE                                           ERM 1120
C                                                                       ERM 1130
            WRITE (6,9000) M,L,J1,J,AKDJQ                               ERM 1140
            GOTO 190                                                    ERM 1150
   50       BATM = 0.D0                                                 ERM 1160
            IM = I - 1                                                  ERM 1170
            DO 110 I=2,IM                                               ERM 1180
              DL = DD(I)                                                ERM 1190
              XPL = DXP(I)                                              ERM 1200
              BATE = 0.D0                                               ERM 1210
              I1 = I - 1                                                ERM 1220
C                                                                       ERM 1230
C     D R VONDY FORM OF BATEMAN EQUATIONS -- ORNL-TM-361                ERM 1240
C                                                                       ERM 1250
              DO 100 KB=1,I1                                            ERM 1260
                XPJ = DXP(KB)                                           ERM 1270
                IF(XPL+XPJ .LT. ERR) GOTO 100                           ERM 1280
                DK = DD(KB)                                             ERM 1290
                PROD = (DL/DK-1.0)                                      ERM 1300
                DKR = PROD                                              ERM 1310
                IF(ABS(PROD) .GT. 1.E-4) GOTO 60                        ERM 1320
C                                                                       ERM 1330
C     USE THIS FORM FOR TWO NEARLY EQUAL HALF-LIVES                     ERM 1340
C                                                                       ERM 1350
                PROD = T * DK * XPJ * (1.0-0.5*(DL-DK)*T)               ERM 1360
                GOTO 70                                                 ERM 1370
   60           PROD = (XPJ-XPL) / PROD                                 ERM 1380
                PRO1 = XPJ / DKR                                        ERM 1390
   70           PI = 1.0                                                ERM 1400
                S1 = 2. / (DK*T)                                        ERM 1410
                DO 90 JK=1,I1                                           ERM 1420
                  IF(JK .EQ. KB) GOTO 90                                ERM 1430
                  S = 1.0 - DK / DD(JK)                                 ERM 1440
                  IF(ABS(S) .GT. 1.E-4) GOTO 80                         ERM 1450
                  IF(ABS(DKR) .GT. 1.0E-4) PROD = PRO1                  ERM 1460
                  S = S1                                                ERM 1470
   80             PI = PI * S                                           ERM 1480
                  IF(ABS(PI) .GT. 1.E25) GOTO 100                       ERM 1490
   90           CONTINUE                                                ERM 1500
                BATE = BATE + PROD / PI                                 ERM 1510
  100         CONTINUE                                                  ERM 1520
C                                                                       ERM 1530
C     IF SUMMATION IS NEGATIVE, SET EQUAL TO ZERO AND PRINT MESSAGE     ERM 1540
C                                                                       ERM 1550
              IF(BATE .LT. 0.D0) WRITE (6,9001) L,IM,BATE,BATM          ERM 1560
C                                                                       ERM 1570
              IF(BATE .LT. 0.D0) BATE = 0.D0                            ERM 1580
              BATM = BATM + BATE                                        ERM 1590
  110       CONTINUE                                                    ERM 1600
            DRA = AKDJQ * DJ * (TRM-BATM) / TRM                         ERM 1610
            GOTO 130                                                    ERM 1620
  120       DRA = AKDJQ * AMAX1(DRB,0.0) * DJ                           ERM 1630
  130       IF(NS .GT. NN) GOTO 150                                     ERM 1640
            DO 140 LJ=NS,NN                                             ERM 1650
              IF(LOCP(LJ) .NE. J1) GOTO 140                             ERM 1660
              AP(LJ) = AP(LJ) + DRA                                     ERM 1670
              GOTO 180                                                  ERM 1680
  140       CONTINUE                                                    ERM 1690
  150       NN = NN + 1                                                 ERM 1700
            AP(NN) = DRA                                                ERM 1710
            LOCP(NN) = J1                                               ERM 1720
            GOTO 180                                                    ERM 1730
  160       IF(AKDJQ .LE. 1.0E-06) GOTO 180                             ERM 1740
            IF(NSAVE .GE. 50) GOTO 180                                  ERM 1750
            NSAVE = NSAVE + 1                                           ERM 1760
            NQUEUE(NSAVE) = J1                                          ERM 1770
            QUEUE(NSAVE) = AKDJQ                                        ERM 1780
            NQU(NSAVE) = J                                              ERM 1790
            QUB(NSAVE) = DRB - 1. / (DJ*T)                              ERM 1800
  180     CONTINUE                                                      ERM 1810
  190     IF(NSAVE .LE. 0) GOTO 200                                     ERM 1820
          J = NQUEUE(NSAVE)                                             ERM 1830
          QUE = QUEUE(NSAVE)                                            ERM 1840
          NQ(J) = NQU(NSAVE)                                            ERM 1850
          DRB = QUB(NSAVE)                                              ERM 1860
          CIMB(L) = CIMB(L) + QUE * B(J) * AMAX1(DRB,0.0)               ERM 1870
          NSAVE = NSAVE - 1                                             ERM 1880
          GOTO 20                                                       ERM 1890
  200   CONTINUE                                                        ERM 1900
  210   NUL = NON0(L)                                                   ERM 1910
        NONP(L) = NN                                                    ERM 1920
  220 CONTINUE                                                          ERM 1930
C                                                                       ERM 1940
C     FIND NORM OF MATRIX AND ESTIMATE ERROR AS DESCRIBED IN LAPIDUS    ERM 1950
C     AND LUUS, OPTIMAL CONTROL OF ENGINEERING PROCESSES BLAISDELL 1967 ERM 1960
C     FIND THE MINIMUM OF THE MAXIMUM ROW SUM AND THE MAXIMUM COLUMN SUMERM 1970
C                                                                       ERM 1980
      ASUM = 0.0                                                        ERM 1990
      ASUMJ = 0.0                                                       ERM 2000
      NUL = 1                                                           ERM 2010
      DO 250 I=1,ITOT                                                   ERM 2020
        IF(.NOT. LONG(I)) GOTO 249                                      ERM 2030
        DI = -D(I) * T                                                  ERM 2040
        AJ = DI                                                         ERM 2050
        NUM = NONP(I)                                                   ERM 2060
        IF(NUL .GT. NUM) GOTO 240                                       ERM 2070
        DO 230 N=NUL,NUM                                                ERM 2080
          AJ = AJ + AP(N) * T                                           ERM 2090
  230   CONTINUE                                                        ERM 2100
  240   AI = DI + DI                                                    ERM 2110
        IF(AI .GT. ASUM) ASUM = AI                                      ERM 2120
        IF(AJ .GT. ASUMJ) ASUMJ = AJ                                    ERM 2130
  249   NUL = NONP(I) + 1                                               ERM 2140
  250 CONTINUE                                                          ERM 2150
      IF(ASUMJ .LT. ASUM) ASUM = ASUMJ                                  ERM 2160
C                                                                       ERM 2170
C     USE ASUM TO DECIDE HOW MANY TERMS ARE REQUIRED AND ESTIMATE ERROR ERM 2180
C                                                                       ERM 2190
      NLARGE = 3.5 * ASUM  + 5.                                         ERM 2200
      XLARGE = NLARGE                                                   ERM 2210
      ERR1 = EXP(ASUM) * (ASUM*2.71828/XLARGE)**NLARGE / SQRT(6.2832*   ERM 2220
     1 XLARGE)                                                          ERM 2230
      IF(ERR1 .GT. 2.E-3) WRITE (6,9002) ERR1,ASUM,NLARGE               ERM 2240
C                                                                       ERM 2250
C     NEXT GENERATE MATRIX EXPONENTIAL SOLUTION                         ERM 2260
C                                                                       ERM 2270
      DO 260 I=1,ITOT                                                   ERM 2280
        CSUM(I) = XTEMP(I)                                              ERM 2290
        CIMN(I) = XTEMP(I)                                              ERM 2300
  260 CONTINUE                                                          ERM 2310
      DO 310 NT=1,NLARGE                                                ERM 2320
        DO 270 I=1,ITOT                                                 ERM 2330
          CIM0(I) = CIMN(I)                                             ERM 2340
  270   CONTINUE                                                        ERM 2350
        TON = T / NT                                                    ERM 2360
        NUL = 1                                                         ERM 2370
        DO 300 I=1,ITOT                                                 ERM 2380
          IF(.NOT. LONG(I)) GOTO 299                                    ERM 2390
          NUM = NONP(I)                                                 ERM 2400
          CIMNI = 0.0                                                   ERM 2410
          IF(NT .EQ. 1) CIMNI = CIMB(I)                                 ERM 2420
          IF(NUL .GT. NUM) GOTO 290                                     ERM 2430
          DO 280 N=NUL,NUM                                              ERM 2440
            J = LOCP(N)                                                 ERM 2450
            CIMNI = CIMNI + AP(N) * CIM0(J)                             ERM 2460
  280     CONTINUE                                                      ERM 2470
  290     CIMNI = CIMNI + D(I) * CIM0(I)                                ERM 2480
          CIMNI = TON * CIMNI                                           ERM 2490
          IF(DABS(CIMNI) .LE. ERR) CIMNI = 0.0                          ERM 2500
          CIMN(I) = CIMNI                                               ERM 2510
          CSUM(I) = CSUM(I) + CIMNI                                     ERM 2520
  299     NUL = NONP(I) + 1                                             ERM 2530
  300   CONTINUE                                                        ERM 2540
  310 CONTINUE                                                          ERM 2550
      DO 320 I=1,ITOT                                                   ERM 2560
        IF(LONG(I)) XNEW(I) = CSUM(I)                                   ERM 2570
  320 CONTINUE                                                          ERM 2580
      RETURN                                                            ERM 2590
      END                                                               ERM 2600
      SUBROUTINE EQUIL(M,ITOT)                                          EQU   10
C                                                                       EQU   20
C     EQUIL PUTS SHORT-LIVED DAUGHTERS IN EQUILIBRIUM WITH PARENTS      EQU   30
C     EQUIL USES GAUSS-SEIDEL ITERATION TO GENERATE STEADY STATE        EQU   40
C     CONCENTRATIONS                                                    EQU   50
C                                                                       EQU   60
      LOGICAL*1 LONG                                                    EQU   70
C                                                                       EQU   80
      INTEGER*2 LOC,NON0,KD                                             EQU   90
C                                                                       EQU  100
      COMMON /EQ/ XZERO(120),XZH(120),XTEMP(120),XNEW(120),B(120),D(120)EQU  110
     1 ,XOLD(120)                                                       EQU  120
C                                                                       EQU  130
      COMMON /MATRIX/ A(600),LOC(600),NON0(120),KD(120)                 EQU  140
C                                                                       EQU  150
      COMMON /FLEX/ FLUX,MMN,MOUT,INDEX,QXN,AXN,ERR,NOBLND,MZERO,ITMAX, EQU  160
     1 ILMAX,IAMAX,IFMAX,IZMAX                                          EQU  170
C                                                                       EQU  180
      COMMON /SERIES/ XP(120),XPAR(120),LONG(120)                       EQU  190
C                                                                       EQU  200
 9000 FORMAT (' GAUSS SEIDEL ITERATION DID NOT CONVERGE IN EQUIL')      EQU  210
C                                                                       EQU  220
C                                                                       EQU  230
      DO 10 I=1,ITOT                                                    EQU  240
        XPAR(I) = 0.0                                                   EQU  250
        IF(.NOT. LONG(I)) GOTO 10                                       EQU  260
        XTEMP(I) = XTEMP(I) * XP(I)                                     EQU  270
        XPAR(I) = AMAX1(XNEW(I)-XTEMP(I),0.0)                           EQU  280
   10 CONTINUE                                                          EQU  290
      ITER = 1                                                          EQU  300
   20 N = 0                                                             EQU  310
      BIG = 0.0                                                         EQU  320
      DO 60 I=1,ITOT                                                    EQU  330
        NUM = NON0(I) - N                                               EQU  340
        DI = -D(I)                                                      EQU  350
        IF(LONG(I)) GOTO 50                                             EQU  360
        XNW = B(I)                                                      EQU  370
        IF(M .GT. MMN .OR. M .EQ. MZERO) NUM = KD(I) - N                EQU  380
        IF(NUM .EQ. 0) GOTO 31                                          EQU  390
        DO 30 K=1,NUM                                                   EQU  400
          N = N + 1                                                     EQU  410
          J = LOC(N)                                                    EQU  420
          DJ = -D(J)                                                    EQU  430
          XJ = XPAR(J)                                                  EQU  440
          IF(LONG(J)) XJ = XJ + XTEMP(J) / (1.0-DJ/DI)                  EQU  450
          XNW = XNW + A(N) * XJ                                         EQU  460
   30   CONTINUE                                                        EQU  470
   31   XNW = XNW / DI                                                  EQU  480
        IF(XNW .LT. ERR) GOTO 40                                        EQU  490
        ARG = ABS((XNW-XPAR(I))/XNW)                                    EQU  500
        IF(ARG .GT. BIG) BIG = ARG                                      EQU  510
   40   XPAR(I) = XNW                                                   EQU  520
   50   N = NON0(I)                                                     EQU  530
   60 CONTINUE                                                          EQU  540
      IF(BIG .LT. QXN) GOTO 70                                          EQU  550
      ITER = ITER + 1                                                   EQU  560
      IF(ITER .LT. 100) GOTO 20                                         EQU  570
      WRITE (6,9000)                                                    EQU  580
      STOP                                                              EQU  590
   70 DO 80 I=1,ITOT                                                    EQU  600
        IF(.NOT. LONG(I)) XNEW(I) = XNEW(I) + XPAR(I)                   EQU  610
   80 CONTINUE                                                          EQU  620
      RETURN                                                            EQU  630
      END                                                               EQU  640