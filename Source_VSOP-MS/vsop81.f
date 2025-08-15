      SUBROUTINE LIFE(MEDUL,M200,M50)                                   LIF   10
C                                                                       LIF   20
C     PREPARE LIBRARY FOR THE DECAY POWER SUBROUTINE "NAKURE/NACHW"     LIF   30
C     CONTENT OF LIBRARY: COMPLETE POWER HISTORY OF ALL IN-CORE BATCHES LIF   40
C                         AND OF ALL STORAGE/COMPOUND STORAGE BOXES     LIF   50
C                                                                       LIF   60
      COMMON /LIF/ NBOX,MT,L0,N200,KT0,M60(5),N61,N9,K0,TG(50),         LIF   70
     1 DELDAY(800),KT1,LT0                                              LIF   80
C                                                                       LIF   90
CFZJ048 enlarged dimension                                    11.04.07  LIF  100
      DIMENSION A(20000000)                                             LIF  110
C                                                                       LIF  120
C                                                                       LIF  130
      CALL WATCH(ENDE)                                                  LIF  140
C                                                                       LIF  150
      B = ENDE                                                          LIF  160
C                                                                       LIF  170
CARD  LF1                                                               LIF  180
C                                                                       LIF  190
      READ (5,10) KMAX,LMAX,MTMAX                                       LIF  200
C                                                                       LIF  210
CFZJ048 enlarged dimension                                    11.04.07  LIF  220
      IA = 20000000                                                     LIF  230
      M250 = M200 + M50                                                 LIF  240
      N50 = MAX0(M50,50)                                                LIF  250
      N100 = LMAX / MEDUL                                               LIF  260
      IF(MEDUL .LE. 1) N100 = 1                                         LIF  270
      N100 = N100 + MTMAX + 1                                           LIF  280
      N100 = MAX0(N100,N50)                                             LIF  290
      LMAX50 = MAX0(N50,LMAX)                                           LIF  300
      N10 = 10                                                          LIF  310
      K1 = 1                                                            LIF  320
      K2 = K1 + (N10*LMAX50)                                            LIF  330
      K3 = K2 + (N10*M200*LMAX)                                         LIF  340
      K4 = K3 + (N10*M200*N100)                                         LIF  350
      K5 = K4 + (N10*M250*N50)                                          LIF  360
      K6 = K5 + (M200*KMAX)                                             LIF  370
      K7 = K6 + M200                                                    LIF  380
      K8 = K7 + M200                                                    LIF  390
      K9 = K8 + LMAX                                                    LIF  400
      K10 = K9 + LMAX                                                   LIF  410
      K11 = K10 + LMAX                                                  LIF  420
      K12 = K11 + LMAX                                                  LIF  430
      K13 = K12 + KMAX                                                  LIF  440
      K14 = K13 + KMAX                                                  LIF  450
      K15 = K14 + KMAX                                                  LIF  460
      K16 = K15 + N50                                                   LIF  470
      K17 = K16 + N50                                                   LIF  480
      K18 = K17 + N50                                                   LIF  490
      K19 = K18 + N50                                                   LIF  500
      K20 = K19 + M200                                                  LIF  510
      K21 = K20 + N50                                                   LIF  520
      K22 = K21 + N50                                                   LIF  530
      K23 = K22 + N50                                                   LIF  540
      K24 = K23 + N50                                                   LIF  550
      K25 = K24 + N50 - K1                                              LIF  560
C                                                                       LIF  570
      CALL DATEL                                                        LIF  580
C                                                                       LIF  590
      WRITE (6,19) K25,IA                                               LIF  600
      IF(K25 .LE. IA) GOTO 1                                            LIF  610
      WRITE (6,20) K25,IA                                               LIF  620
      STOP                                                              LIF  630
    1 CONTINUE                                                          LIF  640
C                                                                       LIF  650
      CALL STORY(A(K1),A(K2),A(K3),A(K4),A(K5),A(K6),A(K7),A(K8),A(K9), LIF  660
     1 A(K10),A(K11),A(K12),A(K13),A(K14),A(K15),A(K16),A(K17),A(K18),  LIF  670
     2 A(K19),N10,N50,M200,M250,KMAX,LMAX,MTMAX,LMAX50,N100,A(K20),     LIF  680
     3 A(K21),A(K22),A(K23),A(K24))                                     LIF  690
C                                                                       LIF  700
      CALL WATCH(ENDE)                                                  LIF  710
C                                                                       LIF  720
      CPU = ENDE - B                                                    LIF  730
      WRITE (6,30) CPU/60.                                              LIF  740
CFZJ013 RETURN instead of STOP                                03.12.03  LIF  750
      RETURN                                                            LIF  760
C                                                                       LIF  770
   10 FORMAT (3I6)                                                      LIF  780
   19 FORMAT (/' SELECTED DIMENSION  =',I8,' WORDS (LIMITS ARE SET TO MALIF  790
     1X. =',I8,' )'/)                                                   LIF  800
   20 FORMAT (' *** GIVEN DIMENSION OF ',I8,' WORDS IS HIGHER THAN MAXIMLIF  810
     1UM OF',I8,' WORDS.'/'     CHANGES OF IA AND OF FIELD A IN *LIFE* ALIF  820
     2RE NECESSARY. ***')                                               LIF  830
   30 FORMAT (///' "LIFE-HISTOGRAM": EXECUTION-TIME:',F10.3,' MIN.')    LIF  840
      END                                                               LIF  850
      SUBROUTINE DATEL                                                  TEL   10
C                                                                       TEL   20
C     DATE + TITLE                                                      TEL   30
C                                                                       TEL   40
      CHARACTER*2 DATG(4)                                               TEL   50
C                                                                       TEL   60
      CHARACTER*4 DATH                                                  TEL   70
C                                                                       TEL   80
      CHARACTER*8 DATF                                                  TEL   90
C                                                                       TEL  100
      EQUIVALENCE(DATG(1),DATF),(DATH,DATF)                             TEL  110
C                                                                       TEL  120
C                                                                       TEL  130
      CALL DATE_AND_TIME(DATF)                                          TEL  140
C                                                                       TEL  150
      READ (UNIT=DATH,FMT=101) JAHR                                     TEL  160
      READ (UNIT=DATG(3),FMT=100) MON                                   TEL  170
      READ (UNIT=DATG(4),FMT=100) ITAG                                  TEL  180
      WRITE (6,10)                                                      TEL  190
      WRITE (6,20)                                                      TEL  200
      WRITE (6,1003) ITAG,MON,JAHR                                      TEL  210
      WRITE (6,30)                                                      TEL  220
      RETURN                                                            TEL  230
C                                                                       TEL  240
   10 FORMAT ('1'///38X,'***',14X,'***',5X,12('*'),5X,12('*')/38X,'***',TEL  250
     1 14X,'***',5X,12('*'),5X,12('*')/38X,'***',14X,'***',5X,'***',14X,TEL  260
     2 '***'/38X,'***',14X,'***',5X,'***',14X,'***'/38X,'***',14X,'***',TEL  270
     3 5X,'***',14X,'***'/38X,'***',14X,'***',5X,9('*'),8X,9('*')/38X,'*TEL  280
     4**',14X,'***',5X,9('*'),8X,9('*')/38X,'***',14X,'***',5X,'***',14XTEL  290
     5 ,'***'/38X,'***',14X,'***',5X,'***',14X,'***'/38X,'***',14X,'***'TEL  300
     6 ,5X,'***',14X,'***'/38X,12('*'),5X,'***',5X,'***',14X,12('*')/38XTEL  310
     7 ,12('*'),5X,'***',5X,'***',14X,12('*'))                          TEL  320
   20 FORMAT (//////36X,'**',5X,'**',7X,7('*'),8X,7('*'),7X,8('*')/36X,'TEL  330
     1**',5X,'**',3(6X,'**',5X,'**')/36X,'**',5X,'**',6X,'**',13X,'**', TEL  340
     2 5X,'**',6X,8('*')/37X,'**',3X,'**',8X,7('*'),7X,'**',5X,'**',6X,'TEL  350
     3**'/38X,'**',1X,'**',4X,'**',2X,'*',6X,'**',2(2X,'**'),5X,'**',   TEL  360
     4 2(2X,'**'),9X,'**'/39X,'***',5X,'**',3X,7('*'),3X,'**',3X,7('*'),TEL  370
     5 3X,'**',2X,'**',9X,'**')                                         TEL  380
   30 FORMAT (//////35X,'  JAN. 2012',29X,'REPORT: V.S.O.P.(99/11)'/83X,TEL  390
     1 'JUEL - 4348'/35X,'  JUNE 2010',29X,'REPORT: V.S.O.P.(99/09)'/83XTEL  391
     2 ,'JUEL - 4326'/83X,'SECTION 4.4.9')                              TEL  400
  100 FORMAT (4I2)                                                      TEL  410
  101 FORMAT (I4)                                                       TEL  420
 1003 FORMAT (/////58X,'( ',I2,'.',I2,'.',I4,' )')                      TEL  430
      END                                                               TEL  440
      SUBROUTINE STORY(Z,ZFL,ZG,ZGR,NRY,NOPOW,VOL,JPR15,JN,POWER,KK,    STO   10
     1 IPR15,TBURN,LL,IBOX,ISTOB,IVORSB,IVORT,MG,N10,N50,M200,M250,KMAX,STO   20
     2 LMAX,MTMAX,LMAX50,N100,VVORT,TVORT,JSTB,MTM,VCOM)                STO   30
C                                                                       STO   40
CFZJ053                                                       19.07.07  STO   50
      DIMENSION Z(N10,LMAX50),ZFL(N10,M200,LMAX),ZG(N10,M200,N100),     STO   60
     1 ZGR(N10,M250,N50),NRY(M200,KMAX),NOPOW(M200),VOL(M200),          STO   70
     2 JPR15(LMAX),JN(LMAX),POWER(LMAX),KK(LMAX),IPR15(KMAX),TBURN(KMAX)STO   80
     3 ,LL(KMAX),IBOX(N50),ISTOB(N50),IVORSB(N50),IVORT(N50),MG(M200),  STO   90
     4 VVORT(N50),TVORT(N50),JSTB(N50),MTM(N50),VCOM(N50),FP(5)         STO  100
C                                                                       STO  110
      COMMON /UIL/ L,K,JNN                                              STO  120
C                                                                       STO  130
      COMMON /LIF/ NBOX,MT,L0,N200,KT0,M60(5),N61,N9,K0,TG(50),         STO  140
     1 DELDAY(800),KT1,LT0,ICOMPA,ICOMPE,IOUT,TOOP                      STO  150
C                                                                       STO  160
CFZJ053                                                       19.07.07  STO  170
      COMMON /FIW/ FIWISO(5)                                            STO  180
C                                                                       STO  190
  101 FORMAT (/' CYCLES MADE OUT OF EQUILIBRIUM CYCLE NO.',I4,' ARE GENESTO  200
     1RATED',I4,'-TIMES WITH',I4,' TIME STEPS.')                        STO  210
  102 FORMAT (/' CHANGE OF RELATIVE POWER TO COMPOSITION POWER,',I5,' STSTO  220
     1ORAGE BOXES ARE AVAILABLE')                                       STO  230
  103 FORMAT (/' HISTORY OF LIFE BACKWARDS FROM CYCLE',I4,' AND TIME STESTO  240
     1P',I4)                                                            STO  250
  109 FORMAT (' BATCH',I5,' COMES FROM',I5)                             STO  260
  110 FORMAT (' HISTORY OF CP-STORAGE BOX',I5,' ADDED. TOTAL LENGHT IS =STO  270
     1',I5,' TIME STEPS.')                                              STO  280
  111 FORMAT (' TOTAL COARSE HISTORY OF LIFE OF BATCH',I4,' IS GENERATEDSTO  290
     1 '/)                                                              STO  300
  112 FORMAT (/' DECAY-HEAT LIBRARY N61 IS WRITTEN. FOLLOWING TOTAL OUTPSTO  310
     1UT FOR BATCH IR =',I4,' -',I4,' (COARSE)')                        STO  320
  113 FORMAT (2I4,11E11.4)                                              STO  330
  114 FORMAT (/'  IR   M     TG         ZG          ZG         ZG       STO  340
     1  ZG         ZG         ZG         ZG         ZG         ZG       STO  350
     2  ZG'/12X,'(M+1)   (N10,IR,M)  (N9,IR,M)   (1,IR,M)   (2,IR,M)   (STO  360
     33,IR,M)   (4,IR,M)   (7,IR,M)   (5,IR,M)   (6,IR,M)   (8,IR,M)'/) STO  370
C                                                                       STO  380
CFZJ053                                                       19.07.07  STO  390
  115 FORMAT (//' BATCH      TIME    TIME-INTER-   POWER                STO  400
     1   POWER FRACTIONS                         CAPTURES/FISSION RATE  STO  410
     2 BURNUP'/23X,' VAL       (WATT)       U3         U5        PU9    STO  420
     3    PU1      FISSILE-   IN FISSIONABLE MAT.'/ 88X,'MATERIAL      TSTO  430
     4H         U8')                                                    STO  440
  120 FORMAT (/' KT0 (=',I4,') IS HIGHER THAN KMAX (=',I4,'), ==> STOP. STO  450
     1ENLARGE KMAX IN INPUT.')                                          STO  460
  200 FORMAT (/' KT0  =',I5/' KT1  =',I5/' KT2  =',I5/' KT3  =',I5/' KT4STO  470
     1  =',I5/' KT5  =',I5/' IZY  =',I5/' KMAX =',I5/' LLL  =',I5/' LMAXSTO  480
     2 =',I5)                                                           STO  490
  201 FORMAT (/' GENERATION OF LIFE-LIBRARY FROM VSOP-CYCLE',I4,' (INCLUSTO  500
     1DED) UP TO CYCLE',I4,' (INCLUDED)')                               STO  510
  300 FORMAT (9I6)                                                      STO  520
  400 FORMAT (/1X,I2,'.SET OF N60 IS READ, ITEST = ',I2)                STO  530
C                                                                       STO  540
C                                                                       STO  550
      ISA = 0                                                           STO  560
      DO 60 I=2,5                                                       STO  570
        M60(I) = 0                                                      STO  580
   60 CONTINUE                                                          STO  590
      M60(1) = 60                                                       STO  600
C                                                                       STO  610
CARD LF2                                                                STO  620
C                                                                       STO  630
      READ (5,300) IOUT,ICOMPA,ICOMPE,LT0,KT5                           STO  640
C                                                                       STO  650
      N61 = 61                                                          STO  660
      OPEN(N61,FORM='UNFORMATTED',FILE='nakure')                        STO  670
      KT1 = 0                                                           STO  680
      IF(KT5 .GT. 0) KT1 = -KT5                                         STO  690
      N9 = 9                                                            STO  700
      KT2 = 0                                                           STO  710
      KT3 = 1                                                           STO  720
      KT0 = 0                                                           STO  730
      KT4 = 0                                                           STO  740
      IF(KT1 .GE. 0) GOTO 1                                             STO  750
      KT1 = IABS(KT1)                                                   STO  760
      KT2 = 1 + KT1                                                     STO  770
      KT4 = 1                                                           STO  780
      IF(KT1 .EQ. 1) KT2 = 0                                            STO  790
    1 CONTINUE                                                          STO  800
      IZY = -1                                                          STO  810
      JJN = 0                                                           STO  820
      I1 = 1                                                            STO  830
      N60 = M60(I1)                                                     STO  840
      REWIND N60                                                        STO  850
    2 CONTINUE                                                          STO  860
      ISA = ISA + 1                                                     STO  870
      READ (N60,END=6) ITEST                                            STO  880
      IF(IOUT .EQ. 2) WRITE (6,400) ISA,ITEST                           STO  890
      IF(ITEST) 3,6,5                                                   STO  900
    3 CONTINUE                                                          STO  910
      IF(IZY .GE. KT1) GOTO 4                                           STO  920
      K = 0                                                             STO  930
      L = 1                                                             STO  940
    4 CONTINUE                                                          STO  950
      K = K + 1                                                         STO  960
      JJN = 0                                                           STO  970
      READ (N60) IPR15(K),N200,TBURN(K),NBOX,(NRY(IR,K),NOPOW(IR),      STO  980
     1 VOL(IR),DUM,IR=1,N200),(IBOX(I),ISTOB(I),I=1,NBOX)               STO  990
      IF(K .GT. 1) DELDAY(L-1) = TBURN(K) - DELDAY(L-1) * (JN(L-1)-1)   STO 1000
      IZY = IPR15(K) + 2                                                STO 1010
      IF(IZY .EQ. KT2) GOTO 7                                           STO 1020
      IF(IZY .EQ. KT3) GOTO 7                                           STO 1030
      GOTO 2                                                            STO 1040
    5 CONTINUE                                                          STO 1050
      JJN = JJN + 1                                                     STO 1060
      JNN = JJN                                                         STO 1070
      LL(K) = L                                                         STO 1080
      KK(L) = K                                                         STO 1090
      READ (N60) JPR15(L),JN(L),POWER(L),DELDAY(L),(ZFL(N9,IR,L),       STO 1100
     1 ZFL(8,IR,L),(ZFL(I,IR,L),I=1,7),IR=1,N200)                       STO 1110
      L = L + 1                                                         STO 1120
      GOTO 2                                                            STO 1130
    6 CONTINUE                                                          STO 1140
      I1 = I1 + 1                                                       STO 1150
      IF(M60(I1) .LE. 0) GOTO 7                                         STO 1160
      N60 = M60(I1)                                                     STO 1170
      OPEN(N60,FORM='UNFORMATTED')                                      STO 1180
      GOTO 2                                                            STO 1190
    7 CONTINUE                                                          STO 1200
      IF(KT0 .EQ. 0) KT0 = K - 1                                        STO 1210
      LLL = L - 1                                                       STO 1220
      IF(KT0 .LE. KMAX) GOTO 8                                          STO 1230
      WRITE (6,120) KT0,KMAX                                            STO 1240
      STOP                                                              STO 1250
    8 CONTINUE                                                          STO 1260
C                                                                       STO 1270
C     TEST OUTPUT                                                       STO 1280
C                                                                       STO 1290
      IF(IOUT .EQ. 2) WRITE (6,200) KT0,KT1,KT2,KT3,KT4,KT5,IZY,KMAX,LLLSTO 1300
     1 ,LMAX                                                            STO 1310
      KT11 = IPR15(1) + 2                                               STO 1320
      KT31 = KT3 - 1                                                    STO 1330
      IF(KT31 .LE. 0) KT31 = IZY - 1                                    STO 1340
      IF(KT4 .GT. 0) KT11 = KT31                                        STO 1350
      WRITE (6,201) KT31,KT11                                           STO 1360
      IF(KT4 .EQ. 0) GOTO 9                                             STO 1370
C                                                                       STO 1380
C     CONSTRUCT POWER HISTOGRAM OUT OF ONE EQUILIBRIUM CYCLE ONLY       STO 1390
C                                                                       STO 1400
      L = 0                                                             STO 1410
      KMAXP = KMAX + 1                                                  STO 1420
      DO 20 K=2,KMAXP                                                   STO 1430
        KALT = K - 2                                                    STO 1440
        IF(K .EQ. 2) KALT = KT0                                         STO 1450
        IPR15(K-1) = IPR15(KALT)                                        STO 1460
        TBURN(K-1) = TBURN(KALT)                                        STO 1470
        DO 11 IR=1,N200                                                 STO 1480
          NRY(IR,K-1) = NRY(IR,KALT)                                    STO 1490
   11   CONTINUE                                                        STO 1500
        DO 13 J=1,JNN                                                   STO 1510
          L = L + 1                                                     STO 1520
          L1 = L - 1                                                    STO 1530
          IF(K .EQ. 2) L1 = LLL - JNN + J                               STO 1540
          KK(L) = K - 1                                                 STO 1550
          JPR15(L) = JPR15(L1)                                          STO 1560
          JN(L) = JN(L1)                                                STO 1570
          POWER(L) = POWER(L1)                                          STO 1580
          DELDAY(L) = DELDAY(L1)                                        STO 1590
          DO 12 IR=1,N200                                               STO 1600
            DO 12 I=1,N9                                                STO 1610
              ZFL(I,IR,L) = ZFL(I,IR,L1)                                STO 1620
   12     CONTINUE                                                      STO 1630
   13   CONTINUE                                                        STO 1640
        LL(K-1) = L                                                     STO 1650
   20 CONTINUE                                                          STO 1660
      KT0 = KMAX                                                        STO 1670
      LLL = LL(KT0)                                                     STO 1680
      WRITE (6,101) KT31,KMAX,LLL                                       STO 1690
    9 CONTINUE                                                          STO 1700
C                                                                       STO 1710
C     PREPARATIONS                                                      STO 1720
C                                                                       STO 1730
      LLL0 = LLL                                                        STO 1740
      IF(LT0 .GT. 0) LLL0 = LL(KT0-1) + LT0                             STO 1750
      WRITE (6,103) KT0,LLL0                                            STO 1760
      VVL = 0.                                                          STO 1770
      DO 10 IR=1,N200                                                   STO 1780
        IF(NOPOW(IR) .LE. 0) VVL = VVL + VOL(IR)                        STO 1790
   10 CONTINUE                                                          STO 1800
      DO 21 L=1,LLL                                                     STO 1810
        DO 21 IR=1,N200                                                 STO 1820
          DO 19 I=1,4                                                   STO 1830
            IF(NOPOW(IR) .EQ. 0) ZFL(I,IR,L) = ZFL(I,IR,L) * ZFL(7,IR,L)STO 1840
   19     CONTINUE                                                      STO 1850
C                                                                       STO 1860
C     POWER OF COMPOSITIONS IN WATT                                     STO 1870
C                                                                       STO 1880
          ZFL(N9,IR,L) = ZFL(N9,IR,L) * POWER(L) * VOL(IR) / VVL        STO 1890
   21 CONTINUE                                                          STO 1900
      ISTO = 0                                                          STO 1910
      DO 22 I=1,NBOX                                                    STO 1920
        ISTO = MAX0(ISTO,ISTOB(I))                                      STO 1930
   22 CONTINUE                                                          STO 1940
      WRITE (6,102) NBOX                                                STO 1950
C                                                                       STO 1960
      CALL GROBT(MTMAX,TN)                                              STO 1970
C                                                                       STO 1980
      L0 = LL(KT0)                                                      STO 1990
      K0 = KK(L0)                                                       STO 2000
C                                                                       STO 2010
      IF(ISTO .GT. 0) CALL STOBOX(ZFL,ZG,ZGR,NRY,VOL,KK,LL,IBOX,ISTOB,  STO 2020
     1 IVORSB,IVORT,N10,N50,M200,M250,KMAX,LMAX,LMAX50,Z,N100,VVORT,    STO 2030
     2 TVORT,JSTB,MTM,VCOM)                                             STO 2040
C                                                                       STO 2050
C     PREPARATION OF POWER HISTOGRAM FOR EACH BATCH                     STO 2060
C                                                                       STO 2070
      DO 31 M=1,MT                                                      STO 2080
        DO 31 IR=1,N200                                                 STO 2090
          DO 31 I=1,N10                                                 STO 2100
            ZG(I,IR,M) = 0.                                             STO 2110
   31 CONTINUE                                                          STO 2120
      IS = 0                                                            STO 2130
      DO 40 IR=1,N200                                                   STO 2140
        IF(NOPOW(IR) .GT. 0) GOTO 40                                    STO 2150
        IS = IS + 1                                                     STO 2160
        LM = LLL0                                                       STO 2170
        LF = 0                                                          STO 2180
        IRM = IR                                                        STO 2190
   26   CONTINUE                                                        STO 2200
        K = KK(LM)                                                      STO 2210
        LF = LF + 1                                                     STO 2220
        DO 27 I=1,N9                                                    STO 2230
          Z(I,LF) = ZFL(I,IRM,LM)                                       STO 2240
   27   CONTINUE                                                        STO 2250
        Z(N10,LF) = DELDAY(LM)                                          STO 2260
        LM = LM - 1                                                     STO 2270
        IF(LM .LE. 0) GOTO 25                                           STO 2280
        IF(K .EQ. KK(LM)) GOTO 26                                       STO 2290
        IRM = NRY(IRM,K)                                                STO 2300
        IF(IOUT .GE. 1) WRITE (6,109) IR,IRM                            STO 2310
        IF(IRM.GT. 0 .AND. IRM .LE. N200) GOTO 26                       STO 2320
   23   CONTINUE                                                        STO 2330
        IF(IRM .EQ. 0) GOTO 25                                          STO 2340
C                                                                       STO 2350
C     CONSTRUCT COMPLETE POWER HISTOGRAM OUT OF HISTOGRAMS OF STORAGE   STO 2360
C     BOXES                                                             STO 2370
C                                                                       STO 2380
        IBX = IRM - N200                                                STO 2390
        MTXM = IVORT(IBX)                                               STO 2400
        VOLVER = VOL(IR) / VCOM(IBX)                                    STO 2410
        DO 28 M=1,MTXM                                                  STO 2420
          LF = LF + 1                                                   STO 2430
          DO 24 I=1,N10                                                 STO 2440
            Z(I,LF) = ZGR(I,IBX,M)                                      STO 2450
   24     CONTINUE                                                      STO 2460
          Z(N9,LF) = Z(N9,LF) * VOLVER                                  STO 2470
   28   CONTINUE                                                        STO 2480
        IRM = IVORSB(IBX)                                               STO 2490
        IF(IOUT .GE. 1) WRITE (6,110) IBX,LF                            STO 2500
        GOTO 23                                                         STO 2510
   25   CONTINUE                                                        STO 2520
C                                                                       STO 2530
C     COARSE HISTOGRAM                                                  STO 2540
C                                                                       STO 2550
        CALL AVGRB1(Z,ZG,N10,LMAX50,M200,N50,IR,LF,MMX,N100,TN)         STO 2560
C                                                                       STO 2570
        DO 33 M=1,MMX                                                   STO 2580
          DO 33 I=1,N10                                                 STO 2590
            ZG(I,IS,M) = ZG(I,IR,M)                                     STO 2600
   33   CONTINUE                                                        STO 2610
        MG(IS) = MMX                                                    STO 2620
        IF(IOUT .GE. 1) WRITE (6,111) IR                                STO 2630
   40 CONTINUE                                                          STO 2640
      REWIND N61                                                        STO 2650
CFZJ053                                                       19.07.07  STO 2660
      DO 73 IR=1,IS                                                     STO 2670
        DO 72 M=1,MG(IR)                                                STO 2680
          SUM = 0.                                                      STO 2690
          DO 70 I=1,4                                                   STO 2700
            Q = 1. / FIWISO(I)                                          STO 2710
            FP(I) = ZG(I,IR,M) * Q                                      STO 2720
            SUM = SUM + FP(I)                                           STO 2730
   70     CONTINUE                                                      STO 2740
          FP(5) = (1.-ZG(1,IR,M)-ZG(2,IR,M)-ZG(3,IR,M)-ZG(4,IR,M)) /    STO 2750
     1     FIWISO(5)                                                    STO 2760
          SUM = SUM + FP(5)                                             STO 2770
          SUM1 = 0.                                                     STO 2780
          DO 71 I=1,4                                                   STO 2790
            ZG(I,IR,M) = FP(I) / SUM                                    STO 2800
            SUM1 = SUM1 + ZG(I,IR,M)                                    STO 2810
   71     CONTINUE                                                      STO 2820
          ZG(7,IR,M) = SUM1                                             STO 2830
   72   CONTINUE                                                        STO 2840
   73 CONTINUE                                                          STO 2850
      DO 41 IR=1,IS                                                     STO 2860
        MMT = MG(IR)                                                    STO 2870
        WRITE (N61) MMT,ZG(8,IR,1),(ZG(N10,IR,M),ZG(N9,IR,M),(ZG(I,IR,M)STO 2880
     1   ,I=1,7),M=1,MMT)                                               STO 2890
   41 CONTINUE                                                          STO 2900
      IF(ICOMPA .EQ. 0) ICOMPA = 1                                      STO 2910
      IF(ICOMPE .EQ. 0) ICOMPE = IS                                     STO 2920
      WRITE (6,112) ICOMPA,ICOMPE                                       STO 2930
      WRITE (6,115)                                                     STO 2940
      WRITE (6,114)                                                     STO 2950
      DO 50 IR=ICOMPA,ICOMPE                                            STO 2960
        MTT = MG(IR)                                                    STO 2970
        DO 50 M=1,MTT                                                   STO 2980
          WRITE (6,113) IR,M,TG(M+1),ZG(N10,IR,M),ZG(N9,IR,M),          STO 2990
     1     (ZG(I,IR,M),I=1,4),ZG(7,IR,M),(ZG(I,IR,M),I=5,6),ZG(8,IR,M)  STO 3000
   50 CONTINUE                                                          STO 3010
      RETURN                                                            STO 3020
      END                                                               STO 3030
      SUBROUTINE GROBT(M,TN)                                            OBT   10
C                                                                       OBT   20
C     UNIFORM COARSE TIME-SCALE                                         OBT   30
C                                                                       OBT   40
      COMMON /LIF/ NBOX,MT,L0,N200,KT0,M60(5),N61,N9,K0,TG(50),         OBT   50
     1 DELDAY(800),KT1,LT0,ICOMPA,ICOMPE,IOUT,TOOP                      OBT   60
C                                                                       OBT   70
      EQUIVALENCE(MT,N1)                                                OBT   80
C                                                                       OBT   90
   10 FORMAT (4E12.5)                                                   OBT  100
   20 FORMAT (/' FOR CALCULATING THE UNIFORM COARSE TIME-SCALE',I4,' ITEOBT  110
     1RATIONS ARE CARRIED OUT')                                         OBT  120
  103 FORMAT (/' COARSE DELTAS OF UNIFORM COARSE TIME-SCALE:'/5(10E12.5/OBT  130
     1 ))                                                               OBT  140
  104 FORMAT (/' UNIFORM COARSE TIME-SCALE:'/5(10E12.5/))               OBT  150
C                                                                       OBT  160
C                                                                       OBT  170
      TT(X) = (((1.+X)**N)-1.) / X                                      OBT  180
C                                                                       OBT  190
CARD LF3                                                                OBT  200
C                                                                       OBT  210
      READ (5,10) TN,DT,TOOP,TEPS                                       OBT  220
C                                                                       OBT  230
      IF(TEPS .LE. 0.) TEPS = 0.1                                       OBT  240
      N = M                                                             OBT  250
      TD = TN / DT                                                      OBT  260
      E0 = ((TD-N)*2.) / N / (N-1)                                      OBT  270
      T0 = TT(E0)                                                       OBT  280
      E1 = E0 / 2.                                                      OBT  290
      T1 = TT(E1)                                                       OBT  300
      DO 1 I=1,500                                                      OBT  310
        I0 = I                                                          OBT  320
        E = E1 + (E0-E1) * (TD-T1) / (T0-T1)                            OBT  330
        T0 = T1                                                         OBT  340
        E0 = E1                                                         OBT  350
        E1 = E                                                          OBT  360
        T1 = TT(E)                                                      OBT  370
        IF(ABS(TD-T1) .LE. TEPS) GOTO 2                                 OBT  380
    1 CONTINUE                                                          OBT  390
    2 CONTINUE                                                          OBT  400
      WRITE (6,20) I0                                                   OBT  410
C                                                                       OBT  420
C     COARSE DELTAS                                                     OBT  430
C                                                                       OBT  440
      N1 = N + 1                                                        OBT  450
      TG(1) = 0.                                                        OBT  460
      DO 3 I=1,N                                                        OBT  470
        TG(I+1) = DT * ((1.+E1)**(I-1))                                 OBT  480
    3 CONTINUE                                                          OBT  490
      WRITE (6,103) (TG(I),I=2,N1)                                      OBT  500
C                                                                       OBT  510
C     ADD COARSE DELTAS TO TOTAL TIME TG                                OBT  520
C                                                                       OBT  530
      DO 4 I=2,N1                                                       OBT  540
        TG(I) = TG(I-1) + TG(I)                                         OBT  550
    4 CONTINUE                                                          OBT  560
      WRITE (6,104) (TG(I),I=1,N1)                                      OBT  570
      RETURN                                                            OBT  580
      END                                                               OBT  590
      SUBROUTINE STOBOX(ZFL,ZG,ZGR,NRY,VOL,KK,LL,IBOX,ISTOB,IVORSB,IVORTTOB   10
     1 ,N10,N50,M200,M250,KMAX,LMAX,LMAX50,Z,N100,VVORT,TVORT,JSTB,MTM, TOB   20
     2 VCOM)                                                            TOB   30
C                                                                       TOB   40
C     POWER HISTOGRAM OF THE BOXES                                      TOB   50
C                                                                       TOB   60
      DIMENSION ZFL(N10,M200,LMAX),ZG(N10,M200,N100),ZGR(N10,M250,N50), TOB   70
     1 NRY(M200,KMAX),VOL(M200),KK(LMAX),LL(KMAX),IBOX(N50),ISTOB(N50), TOB   80
     2 IVORSB(N50),IVORT(N50),VVORT(N50),TVORT(N50),JSTB(N50),          TOB   90
     3 Z(N10,LMAX50),MTM(N50),VCOM(N50)                                 TOB  100
C                                                                       TOB  110
      COMMON /LIF/ NBOX,MT,L0,N200,KT0,M60(5),N61,N9,K0,TG(50),         TOB  120
     1 DELDAY(800),KT1,LT0,ICOMPA,ICOMPE,IOUT,TOOP                      TOB  130
C                                                                       TOB  140
  104 FORMAT (I5,2(5X,E12.5))                                           TOB  150
  105 FORMAT (/' STORAGE BOX HISTORY',I3,' STARTS FROM BATCH',I5)       TOB  160
  106 FORMAT (/' CP-STORAGE BOXES: 1. OR. LOWEST =',I3,' LAST OR LARGESTTOB  170
     1 =',I3)                                                           TOB  180
  107 FORMAT ( ' COARSE HISTORY OF STORAGE BOX',I3,' IS GENERATED')     TOB  190
  108 FORMAT (/' COARSE HISTORY OF CP-STORAGE BOX',I3,' IS GENERATED, TITOB  200
     1ME INTERVALS =',I4)                                               TOB  210
  109 FORMAT (/' OUT-OF-PILE-TIME IN CP-STORAGE BOXES =',E12.5,' DAYS.')TOB  220
  110 FORMAT (3I6,E12.5)                                                TOB  230
  111 FORMAT (/)                                                        TOB  240
  112 FORMAT (2I6,2E12.5)                                               TOB  250
C                                                                       TOB  260
C                                                                       TOB  270
      WRITE (6,109) TOOP                                                TOB  280
      DO 10 IB=1,NBOX                                                   TOB  290
        IF(ISTOB(IB) .LE. 0) GOTO 10                                    TOB  300
        TT = 0.                                                         TOB  310
        LM = L0                                                         TOB  320
        IRM = ISTOB(IB)                                                 TOB  330
        LF = 0                                                          TOB  340
    1   CONTINUE                                                        TOB  350
        LF = LF + 1                                                     TOB  360
        K = KK(LM)                                                      TOB  370
        IF(LF .EQ. 1) VVORT(IB) = VOL(IRM)                              TOB  380
        DO 2 I=1,N9                                                     TOB  390
          ZG(I,IB,LF) = 0.                                              TOB  400
    2   CONTINUE                                                        TOB  410
        DLDY = TOOP                                                     TOB  420
        IF(LF .LE. 1 .AND. TOOP .GT. 0.) GOTO 4                         TOB  430
        DO 3 I=1,N9                                                     TOB  440
          ZG(I,IB,LF) = ZFL(I,IRM,LM)                                   TOB  450
    3   CONTINUE                                                        TOB  460
        DLDY = DELDAY(LM)                                               TOB  470
        LM = LM - 1                                                     TOB  480
    4   CONTINUE                                                        TOB  490
        ZG(N10,IB,LF) = DLDY                                            TOB  500
        TT = TT + DLDY                                                  TOB  510
CFZJ019 Exit loop after LM=0                                  22.12.03  TOB  520
        IF(LM .LE. 0) GOTO 6                                            TOB  530
        IF(K .EQ. KK(LM)) GOTO 1                                        TOB  540
    6   CONTINUE                                                        TOB  550
        IRM = NRY(IRM,K)                                                TOB  560
        IF(IRM .GT. 0 .AND. IRM .LE. N200) GOTO 1                       TOB  570
        IVORSB(IB) = 0                                                  TOB  580
        IVORT(IB) = LF                                                  TOB  590
        TVORT(IB) = TT                                                  TOB  600
        IF(IRM .GT. 0) IVORSB(IB) = IRM                                 TOB  610
        IF(IOUT .GE. 1) WRITE (6,105) IB,IVORSB(IB)                     TOB  620
        IF(IOUT .EQ. 2) WRITE (6,110) IB,K,IVORT(IB),TVORT(IB)          TOB  630
        DO 5 N=1,LF                                                     TOB  640
          IF(IOUT .EQ. 2) WRITE (6,104) N,ZG(N10,IB,N),ZG(N9,IB,N)      TOB  650
    5   CONTINUE                                                        TOB  660
   10 CONTINUE                                                          TOB  670
C                                                                       TOB  680
C     PREPARATION OF COMPOUND STORAGE BOXES                             TOB  690
C                                                                       TOB  700
      ICBU = 1000                                                       TOB  710
      ICBO = 0                                                          TOB  720
      DO 11 IB=1,NBOX                                                   TOB  730
        IF(IBOX(IB) .LE. 0) GOTO 11                                     TOB  740
        J = IBOX(IB)                                                    TOB  750
        ICBU = MIN0(ICBU,J)                                             TOB  760
        ICBO = MAX0(ICBO,J)                                             TOB  770
   11 CONTINUE                                                          TOB  780
      IF(IOUT .GE. 1) WRITE (6,106) ICBU,ICBO                           TOB  790
   12 CONTINUE                                                          TOB  800
C                                                                       TOB  810
C     CONSTRUCT UNIFORM RESIDENCE TIMES FOR THE HISTOGRAMS OF THE       TOB  820
C     STORAGE BOXES CONTAINED IN ONE COMPOUND STORAGE BOX               TOB  830
C                                ---                                    TOB  840
C                                                                       TOB  850
      JJ = 0                                                            TOB  860
      TMIT = 0.                                                         TOB  870
      VMIT = 0.                                                         TOB  880
      IF(IOUT .EQ. 2) WRITE (6,111)                                     TOB  890
      DO 13 IB=1,NBOX                                                   TOB  900
        IF(IBOX(IB) .NE. ICBU) GOTO 13                                  TOB  910
        JJ = JJ + 1                                                     TOB  920
        JSTB(JJ) = IB                                                   TOB  930
        TMIT = TMIT + TVORT(IB) * VVORT(IB)                             TOB  940
        VMIT = VMIT + VVORT(IB)                                         TOB  950
        IF(IOUT .EQ. 2) WRITE (6,112) IB,JJ,TVORT(IB),VVORT(IB)         TOB  960
   13 CONTINUE                                                          TOB  970
      VCOM(ICBU) = VMIT                                                 TOB  980
      IF(JJ .LE. 0) GOTO 33                                             TOB  990
      TMIT = TMIT / VMIT                                                TOB 1000
      WRITE (6,111)                                                     TOB 1010
      MTMY = 0                                                          TOB 1020
      DO 30 J=1,JJ                                                      TOB 1030
        IB = JSTB(J)                                                    TOB 1040
        LMX = IVORT(IB)                                                 TOB 1050
        IF(IVORSB(IB) .EQ. 0) GOTO 14                                   TOB 1060
C                                                                       TOB 1070
C     ADD THE HISTOGRAM OF COMPOUND STORAGE BOX "IVORSB(IB)"            TOB 1080
C                                                                       TOB 1090
        ICBX = IVORSB(IB) - N200                                        TOB 1100
        LMY = LMX + IVORT(ICBX)                                         TOB 1110
        LMX1 = LMX + 1                                                  TOB 1120
        DO 114 L=LMX1,LMY                                               TOB 1130
          L1 = L - LMX                                                  TOB 1140
          DO 113 N=1,N10                                                TOB 1150
            FAK = 1.                                                    TOB 1160
            IF(N .EQ. N9) FAK = VVORT(IB) / VCOM(ICBX)                  TOB 1170
            ZG(N,IB,L) = ZGR(N,ICBX,L1) * FAK                           TOB 1180
  113     CONTINUE                                                      TOB 1190
  114   CONTINUE                                                        TOB 1200
        LMX = LMY                                                       TOB 1210
   14   CONTINUE                                                        TOB 1220
        DO 15 NN=1,N10                                                  TOB 1230
          DO 15 L=1,LMX                                                 TOB 1240
            Z(NN,L) = ZG(NN,IB,L)                                       TOB 1250
   15   CONTINUE                                                        TOB 1260
C                                                                       TOB 1270
C     COARSE TIME-SCALE FOR THESE STORAGE BOXES BELONGING TO            TOB 1280
C                                                                       TOB 1290
        CALL AVGRB2(Z,ZGR,N10,LMAX50,M250,N50,IB,LMX,MTMX)              TOB 1300
C                                                                       TOB 1310
        MTM(IB) = MTMX                                                  TOB 1320
        MTMY = MAX0(MTMY,MTMX)                                          TOB 1330
        IF(IOUT .GE. 1) WRITE (6,107) IB                                TOB 1340
   30 CONTINUE                                                          TOB 1350
C                                                                       TOB 1360
C     COARSE HISTOGRAM OF COMPOUND STORAGE BOX                          TOB 1370
C                                                                       TOB 1380
      DO 31 M=1,MTMY                                                    TOB 1390
        DO 31 I=1,N10                                                   TOB 1400
          ZGR(I,ICBU,M) = 0.                                            TOB 1410
   31 CONTINUE                                                          TOB 1420
      DO 43 M=1,MTMY                                                    TOB 1430
        QQ = 0.                                                         TOB 1440
        DO 42 J=1,JJ                                                    TOB 1450
          IB = JSTB(J)                                                  TOB 1460
          IF(M .GT. MTM(IB)) GOTO 42                                    TOB 1470
          QQ = QQ + ZGR(N9,IB,M)                                        TOB 1480
   42   CONTINUE                                                        TOB 1490
        DO 41 J=1,JJ                                                    TOB 1500
          IB = JSTB(J)                                                  TOB 1510
          IF(M .GT. MTM(IB)) GOTO 41                                    TOB 1520
          QQQ = 0.                                                      TOB 1530
          IF(QQ .GT. 0.) QQQ = ZGR(N9,IB,M) / QQ                        TOB 1540
          DO 40 I=1,8                                                   TOB 1550
            FAK = QQQ                                                   TOB 1560
            IF(I .EQ. 5 .OR. I .EQ. 6) FAK = QQQ * ZGR(7,IB,M)          TOB 1570
            IF(I .EQ. 8) FAK = VVORT(IB) / VMIT                         TOB 1580
            ZGR(I,ICBU,M) = ZGR(I,ICBU,M) + ZGR(I,IB,M) * FAK           TOB 1590
   40     CONTINUE                                                      TOB 1600
          ZGR(N10,ICBU,M) = ZGR(N10,IB,M)                               TOB 1610
   41   CONTINUE                                                        TOB 1620
        ZGR(N9,ICBU,M) = QQ                                             TOB 1630
        ZG7 = ZGR(7,ICBU,M)                                             TOB 1640
        DO 44 I=5,6                                                     TOB 1650
          IF(ZG7 .GT. 0.) ZGR(I,ICBU,M) = ZGR(I,ICBU,M) / ZG7           TOB 1660
   44   CONTINUE                                                        TOB 1670
   43 CONTINUE                                                          TOB 1680
      IVORT(ICBU) = MTMY                                                TOB 1690
      IVORSB(ICBU) = 0                                                  TOB 1700
      IF(IOUT .GE. 1) WRITE (6,108) ICBU,MTMY                           TOB 1710
      ICBU = ICBU + 1                                                   TOB 1720
      IF(ICBU .LE. ICBO) GOTO 12                                        TOB 1730
   33 CONTINUE                                                          TOB 1740
      RETURN                                                            TOB 1750
      END                                                               TOB 1760
      SUBROUTINE AVGRB1(B,C,N10,N3,N4,N5,NR,NLF,MTMX,N100,TN)           VGR   10
C                                                                       VGR   20
C     COARSE TIME-SCALE FOR BATCHES AND BOXES                           VGR   30
C                                                                       VGR   40
      DIMENSION B(N10,N3),C(N10,N4,N100),TF(1000)                       VGR   50
C                                                                       VGR   60
      COMMON /LIF/ NBOX,MT,L0,N200,KT0,M60(5),N61,N9,K0,TG(50),         VGR   70
     1 DELDAY(800),KT1,LT0                                              VGR   80
C                                                                       VGR   90
C                                                                       VGR  100
      N8 = N9 - 1                                                       VGR  110
      TF(1) = 0.                                                        VGR  120
      DO 1 I=1,NLF                                                      VGR  130
        TF(I+1) = TF(I) + B(N10,I)                                      VGR  140
    1 CONTINUE                                                          VGR  150
      M = 1                                                             VGR  160
      L = 1                                                             VGR  170
      TFF = 0.                                                          VGR  180
      MFF = NLF + MT                                                    VGR  190
      DO 10 J=1,MFF                                                     VGR  200
        MTMX = M                                                        VGR  210
        IF(L .LE. NLF) GOTO 5                                           VGR  220
        TFF1 = TG(M+1)                                                  VGR  230
        TFL1 = TFF1                                                     VGR  240
        DLFF = TFF1 - TFF                                               VGR  250
        C(N10,NR,M) = C(N10,NR,M) + DLFF                                VGR  260
        GOTO 6                                                          VGR  270
    5   CONTINUE                                                        VGR  280
        TFL1 = TF(L+1)                                                  VGR  290
        TFF1 = AMIN1(TF(L+1),TG(M+1))                                   VGR  300
        DLFF = TFF1 - TFF                                               VGR  310
        DT = TF(L+1) - TF(L)                                            VGR  320
        DET = 0.                                                        VGR  330
        IF(DT .GT. 0.) DET = DLFF / DT                                  VGR  340
        DO 2 I=1,N10                                                    VGR  350
          Z0 = B(N10,L)                                                 VGR  360
          Z1 = B(N9,L)                                                  VGR  370
          IF(I .GE. N9) Z1 = 1.                                         VGR  380
          IF(I .GE. N10) Z0 = 1.                                        VGR  390
          C(I,NR,M) = C(I,NR,M) + B(I,L) * Z0 * Z1 * DET                VGR  400
    2   CONTINUE                                                        VGR  410
    6   CONTINUE                                                        VGR  420
        IF(TFF1 .LT. TG(M+1)) GOTO 4                                    VGR  430
        C9 = C(N9,NR,M)                                                 VGR  440
        C99 = 0.                                                        VGR  450
        IF(C9 .GT. 0.) C99 = 1. / C9                                    VGR  460
        DO 3 I=1,N8                                                     VGR  470
          C(I,NR,M) = C(I,NR,M) * C99                                   VGR  480
    3   CONTINUE                                                        VGR  490
        C(N9,NR,M) = C9 / C(N10,NR,M)                                   VGR  500
        M = M + 1                                                       VGR  510
    4   CONTINUE                                                        VGR  520
        IF(TFF1 .GE. TFL1) L = L + 1                                    VGR  530
        TFF = TFF1                                                      VGR  540
        IF(ABS((TG(M)-TN)/TN) .LE. 0.001) GOTO 11                       VGR  550
        IF(MTMX .NE. M .AND. L.GT. NLF) GOTO 11                         VGR  560
        IF(M .GT. MT) GOTO 11                                           VGR  570
   10 CONTINUE                                                          VGR  580
   11 CONTINUE                                                          VGR  590
      RETURN                                                            VGR  600
      END                                                               VGR  610
      SUBROUTINE AVGRB2(B,C,N10,N3,N4,N5,NR,NLF,MTMX)                   GRB   10
C                                                                       GRB   20
C     COARSE TIME-SCALE FOR BATCHES AND BOXES                           GRB   30
C                                                                       GRB   40
      DIMENSION B(N10,N3),C(N10,N4,N5),TF(1000)                         GRB   50
C                                                                       GRB   60
      COMMON /LIF/ NBOX,MT,L0,N200,KT0,M60(5),N61,N9,K0,TG(50),         GRB   70
     1 DELDAY(800),KT1,LT0                                              GRB   80
C                                                                       GRB   90
C                                                                       GRB  100
      N8 = N9 - 1                                                       GRB  110
      TF(1) = 0.                                                        GRB  120
      DO 1 I=1,NLF                                                      GRB  130
        TF(I+1) = TF(I) + B(N10,I)                                      GRB  140
    1 CONTINUE                                                          GRB  150
      M = 1                                                             GRB  160
      L = 1                                                             GRB  170
      TFF = 0.                                                          GRB  180
      MFF = NLF + MT                                                    GRB  190
      DO 10 J=1,MFF                                                     GRB  200
        MTMX = M                                                        GRB  210
        IF(L .LE. NLF) GOTO 5                                           GRB  220
        TFF1 = TG(M+1)                                                  GRB  230
        TFL1 = TFF1                                                     GRB  240
        DLFF = TFF1 - TFF                                               GRB  250
        C(N10,NR,M) = C(N10,NR,M) + DLFF                                GRB  260
        GOTO 6                                                          GRB  270
    5   CONTINUE                                                        GRB  280
        TFL1 = TF(L+1)                                                  GRB  290
        TFF1 = AMIN1(TF(L+1),TG(M+1))                                   GRB  300
        DLFF = TFF1 - TFF                                               GRB  310
        DT = TF(L+1) - TF(L)                                            GRB  320
        DET = 0.                                                        GRB  330
        IF(DT .GT. 0.) DET = DLFF / DT                                  GRB  340
        DO 2 I=1,N10                                                    GRB  350
          Z0 = B(N10,L)                                                 GRB  360
          Z1 = B(N9,L)                                                  GRB  370
          IF(I .GE. N9) Z1 = 1.                                         GRB  380
          IF(I .GE. N10) Z0 = 1.                                        GRB  390
          C(I,NR,M) = C(I,NR,M) + B(I,L) * Z0 * Z1 * DET                GRB  400
    2   CONTINUE                                                        GRB  410
    6   CONTINUE                                                        GRB  420
        IF(TFF1 .LT. TG(M+1)) GOTO 4                                    GRB  430
        C9 = C(N9,NR,M)                                                 GRB  440
        C99 = 0.                                                        GRB  450
        IF(C9 .GT. 0.) C99 = 1. / C9                                    GRB  460
        DO 3 I=1,N8                                                     GRB  470
          C(I,NR,M) = C(I,NR,M) * C99                                   GRB  480
    3   CONTINUE                                                        GRB  490
        C(N9,NR,M) = C9 / C(N10,NR,M)                                   GRB  500
        M = M + 1                                                       GRB  510
    4   CONTINUE                                                        GRB  520
        IF(TFF1 .GE. TFL1) L = L + 1                                    GRB  530
        TFF = TFF1                                                      GRB  540
        IF(MTMX .NE. M .AND. L.GT. NLF) GOTO 11                         GRB  550
        IF(M .GT. MT) GOTO 11                                           GRB  560
   10 CONTINUE                                                          GRB  570
   11 CONTINUE                                                          GRB  580
      RETURN                                                            GRB  590
      END                                                               GRB  600