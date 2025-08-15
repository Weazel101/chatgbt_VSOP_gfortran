      SUBROUTINE PRIMA(N200C,NXSC,KMAX,LMAX,IMAT,KFIS,NGRP)             RIM   10
C                                                                       RIM   20
C     MAIN FOR *PRIOR*                                                  RIM   30
C                                                                       RIM   40
C     PREPARING AN IRRADIATION HISTORY LIBRARY OF THE FUEL FOR THE      RIM   50
C     DEPLETION COMPUTER CODE ORIGEN-JUEL-II                            RIM   60
C                                                                       RIM   70
C     SET-UP THE DIMENSION                                              RIM   80
C                                                                       RIM   90
      COMMON /VARPRI/ A(2000000)                                        RIM  100
C                                                                       RIM  110
   10 FORMAT (12I6)                                                     RIM  120
   20 FORMAT ('1'/' DIMENSIONS FOR THIS PROBLEM:'//' N200C:  NO. OF BATCRIM  130
     1HES IN THE CORE:        ',I6/' NXSC :  NO. OF SPECTRUM ZONES IN THRIM  140
     2E CORE: ',I6/' LXS  :  NO. OF SPECTRUM CALCULATIONS:      ',I6/' KRIM  150
     3MAX :  NO. OF BURNUP CYCLES:              ',I6/' LMAX :  TOTAL NO.RIM  160
     4 OF BURNUP TIME INTERVALS:',I6/' IMAT :  NO. OF ISOTOPES:         RIM  170
     5          ',I6/' KFIS :  NO. OF HEAVY METAL ISOTOPES:       ',I6/'RIM  180
     6 NGRP :  NO. OF ENERGY GROUPS:              ',I6/)                RIM  190
   30 FORMAT (/' THE DIMENSION RESERVED FOR DATA STORAGE IS ABOUT',I8,' RIM  200
     1WORDS TOO SMALL.'/' SET-UP THE DIMENSION *A* AND VARIABLE *IA* (INRIM  210
     2 PRIMA).  *** STOP ***')                                          RIM  220
   40 FORMAT (/' DIMENSION RESERVED FOR DATA STORAGE:',I10/' DIMENSION URIM  230
     1SED FOR THIS PROBLEM:    ',I10/' DIMENSION NOT USED:              RIM  240
     2   ',I10//)                                                       RIM  250
   50 FORMAT (///' "POWER HISTOGRAM": EXECUTION-TIME:',F10.3,' MIN.')   RIM  260
C                                                                       RIM  270
C                                                                       RIM  280
      CALL WATCH(ENDE)                                                  RIM  290
C                                                                       RIM  300
      B = ENDE                                                          RIM  310
C                                                                       RIM  320
      CALL DATEP                                                        RIM  330
C                                                                       RIM  340
CARD P                                                                  RIM  350
C                                                                       RIM  360
      READ (5,10) LXS,LT0,IOUT                                          RIM  370
C                                                                       RIM  380
      WRITE (6,20) N200C,NXSC,LXS,KMAX,LMAX,IMAT,KFIS,NGRP              RIM  390
      KMAX = KMAX + 1                                                   RIM  400
      IA = 2000000                                                      RIM  410
      LNN = LMAX * NGRP * N200C                                         RIM  420
      KN = KMAX * N200C                                                 RIM  430
      KIN = KMAX * IMAT * N200C                                         RIM  440
      LNNK = LXS * NXSC * NGRP * KFIS                                   RIM  450
      LNNI = LXS * NXSC * NGRP * IMAT                                   RIM  460
      LN = LMAX * N200C                                                 RIM  470
      IN = IMAT * N200C                                                 RIM  480
      K1 = 1                                                            RIM  490
      K2 = K1 + IMAT                                                    RIM  500
      K3 = K2 + N200C                                                   RIM  510
      K4 = K3 + LNN                                                     RIM  520
      K5 = K4 + LMAX                                                    RIM  530
      K6 = K5 + LMAX                                                    RIM  540
      K7 = K6 + LMAX                                                    RIM  550
      K8 = K7 + LMAX                                                    RIM  560
      K9 = K8 + KN                                                      RIM  570
      K10 = K9 + KN                                                     RIM  580
      K11 = K10 + KIN                                                   RIM  590
      K12 = K11 + KMAX                                                  RIM  600
      K13 = K12 + LNNK                                                  RIM  610
      K14 = K13 + LNNI                                                  RIM  620
      K15 = K14 + LNN                                                   RIM  630
      K16 = K15 + LN                                                    RIM  640
      K17 = K16 + LN                                                    RIM  650
      K18 = K17 + LN                                                    RIM  660
      K19 = K18 + IN                                                    RIM  670
      K20 = K19 + N200C                                                 RIM  680
      K21 = K20 + LNNK - K1                                             RIM  690
      KNU = IA - K21                                                    RIM  700
      IF(KNU) 1,2,2                                                     RIM  710
    1 CONTINUE                                                          RIM  720
      IKNU = IABS(KNU)                                                  RIM  730
      WRITE (6,30) IKNU                                                 RIM  740
      STOP                                                              RIM  750
    2 CONTINUE                                                          RIM  760
      WRITE (6,40) IA,K21,KNU                                           RIM  770
C                                                                       RIM  780
      CALL PRIOR(KFIS,LMAX,NGRP,N200C,KMAX,IMAT,LXS,NXSC,A(K1),A(K2),   RIM  790
     1 A(K3),A(K4),A(K5),A(K6),A(K7),A(K8),A(K9),A(K10),A(K11),A(K12),  RIM  800
     2 A(K13),A(K14),A(K15),A(K16),A(K17),A(K18),A(K19),A(K20),LT0,IOUT)RIM  810
C                                                                       RIM  820
      CALL WATCH(ENDE)                                                  RIM  830
C                                                                       RIM  840
      CPU = ENDE - B                                                    RIM  850
      WRITE (6,50) CPU/60.                                              RIM  860
      STOP                                                              RIM  870
      END                                                               RIM  880
      SUBROUTINE DATEP                                                  TEP   10
C                                                                       TEP   20
C     DATE + TITLE                                                      TEP   30
C                                                                       TEP   40
      CHARACTER*2 DATG(4)                                               TEP   50
C                                                                       TEP   60
      CHARACTER*4 DATH                                                  TEP   70
C                                                                       TEP   80
      CHARACTER*8 DATF                                                  TEP   90
C                                                                       TEP  100
      EQUIVALENCE(DATG(1),DATF),(DATH,DATF)                             TEP  110
C                                                                       TEP  120
C                                                                       TEP  130
      CALL DATE_AND_TIME(DATF)                                          TEP  140
C                                                                       TEP  150
      READ (DATH,101) JAHR                                              TEP  160
      READ (DATG(3),100) MON                                            TEP  170
      READ (DATG(4),100) ITAG                                           TEP  180
      WRITE (6,10)                                                      TEP  190
      WRITE (6,20)                                                      TEP  200
      WRITE (6,1003) ITAG,MON,JAHR                                      TEP  210
      WRITE (6,30)                                                      TEP  220
      RETURN                                                            TEP  230
C                                                                       TEP  240
   10 FORMAT ('1'///30X,11('*'),6X,11('*'),6X,'***',6X,10('*'),6X,      TEP  250
     1 11('*')/30X,12('*'),5X,12('*'),5X,'***',5X,12('*'),5X,12('*')/30XTEP  260
     2 ,'***',6X,'***',5X,'***',6X,'***',5X,'***',5X,'****',4X,'****',5XTEP  270
     3 ,'***',6X,'***'/30X,'***',7X,'**',5X,'***',7X,'**',5X,'***',5X,'*TEP  280
     4**',6X,'***',5X,'***',7X,'**'/30X,'***',6X,'***',5X,'***',6X,'***'TEP  290
     5 ,5X,'***',5X,'***',6X,'***',5X,'***',6X,'***'/30X,11('*'),6X,    TEP  300
     6 11('*'),6X,'***',5X,'***',6X,'***',5X,11('*')/30X,10('*'),7X,    TEP  310
     7 10('*'),7X,'***',5X,'***',6X,'***',5X,10('*')/30X,'***',14X,'***'TEP  320
     8 ,2X,'***',9X,'***',5X,'***',6X,'***',5X,'***',2X,'***'/30X,'***',TEP  330
     9 14X,'***',3X,'***',8X,'***',5X,'***',6X,'***',5X,'***',3X,'***'/ TEP  340
     X 30X,'***',14X,'***',4X,'***',7X,'***',5X,'****',4X,'****',5X,'***TEP  350
     Y',4X,'***'/30X,'***',14X,'***',5X,'***',6X,'***',5X,12('*'),5X,'**TEP  360
     Z*',5X,'***'/30X,'***',14X,'***',6X,'***',5X,'***',6X,10('*'),6X,'*TEP  370
     Z**',6X,'***')                                                     TEP  380
   20 FORMAT (//////36X,'**',5X,'**',7X,7('*'),8X,7('*'),7X,8('*')/36X,'TEP  390
     1**',5X,'**',3(6X,'**',5X,'**')/36X,'**',5X,'**',6X,'**',13X,'**', TEP  400
     2 5X,'**',6X,8('*')/37X,'**',3X,'**',8X,7('*'),7X,'**',5X,'**',6X,'TEP  410
     3**'/38X,'**',1X,'**',4X,'**',2X,'*',6X,'**',2(2X,'**'),5X,'**',   TEP  420
     4 2(2X,'**'),9X,'**'/39X,'***',5X,'**',3X,7('*'),3X,'**',3X,7('*'),TEP  430
     5 3X,'**',2X,'**',9X,'**')                                         TEP  440
   30 FORMAT (//////29X,'  JAN. 2012',41X,'REPORT: V.S.O.P.(99/11)'/89X,TEP  450
     1 'JUEL - 4348'/29X,'  JUNE 2010',41X,'REPORT: V.S.O.P.(99/09)'/89XTEP  451
     2 ,'JUEL - 4326'/89X,'SECTION 4.4.10')                             TEP  460
  100 FORMAT (4I2)                                                      TEP  470
  101 FORMAT (I4)                                                       TEP  480
 1003 FORMAT (/////58X,'( ',I2,'.',I2,'.',I4,' )')                      TEP  490
      END                                                               TEP  500
      SUBROUTINE PRIOR(KFIS,LMAX,NGRP,N200C,KMAX,IMAT,LXS,NXSC,MATNR,   RIO   10
     1 NHOT,FLX,IP15,JNZ,ISP,DELD,VOL,NRY,DEN,IP15K,FSGMA,ASGMA,FLUSS,  RIO   20
     2 DELT,NRSPEK,NRHOT,AAKONZ,LSTMAX,SGMN2N,LT0,IOUT)                 RIO   30
C                                                                       RIO   40
      COMMON /UIP/ N26,KMAT,N200,KFISS,LST,NXS,ISMAX,KST,AVO,ISPK,DELDAYRIO   50
     1 ,IPR15,JN                                                        RIO   60
C                                                                       RIO   70
      DIMENSION MATNR(IMAT),NHOT(N200C),FLX(LMAX,NGRP,N200C),IP15(LMAX),RIO   80
     1 JNZ(LMAX),ISP(LMAX),DELD(LMAX),VOL(KMAX,N200C),NRY(KMAX,N200C),  RIO   90
     2 DEN(KMAX,IMAT,N200C),IP15K(KMAX),FSGMA(LXS,NXSC,NGRP,KFIS),      RIO  100
     3 ASGMA(LXS,NXSC,NGRP,IMAT),FLUSS(NGRP,LMAX,N200C),DELT(LMAX,N200C)RIO  110
     4 ,NRSPEK(LMAX,N200C),NRHOT(LMAX,N200C),AAKONZ(IMAT,N200C),        RIO  120
     5 LSTMAX(N200C),SGMN2N(LXS,NXSC,NGRP,KFIS),M60(5)                  RIO  130
C                                                                       RIO  140
      CHARACTER*4 GR(2)/' GRO','UP  '/,ST/'----'/                       RIO  150
C                                                                       RIO  160
 1020 FORMAT (/' THE DIMENSION INPUT ITEM *LXS* IS TOO SMALL. CORRECT ONRIO  170
     1 CARD P.')                                                        RIO  180
 1100 FORMAT (1H )                                                      RIO  190
 1110 FORMAT (/' BATCH',I4,' COMES FROM',I10)                           RIO  200
 1115 FORMAT ('           BEFORE THAT FROM',I4)                         RIO  210
 1120 FORMAT (///' CROSS SECTION SET AT THE DIFFERENT TIME STEPS:'//' BARIO  220
     1TCH | TIME STEPS:')                                               RIO  230
 1125 FORMAT (7X,'|',30I4)                                              RIO  240
 1126 FORMAT (7X,'|',30A4)                                              RIO  250
 1130 FORMAT (I5,'  |',30I4)                                            RIO  260
 1135 FORMAT (I5,'  |')                                                 RIO  270
 1140 FORMAT (//' TOTAL NO. OF CROSS SECTION SETS TO BE READ IN ORIGEN-JRIO  280
     1UEL-II =',I4)                                                     RIO  290
 1150 FORMAT (///' BATCH  TIME  X-SECT.   DELDAY     NEUTRON FLUXES:'/' RIO  300
     1       STEP    SET      (D)      ',8(2A4,I1,3X)/)                 RIO  310
 1160 FORMAT (I5,I6,I7,2X,9E12.5)                                       RIO  320
 1170 FORMAT (//' IRRADIATION HISTORY OF THE FUEL STORED ON DATA SET "orRIO  330
     1igen"')                                                           RIO  340
 1180 FORMAT (/' FOLLOWING IRRADIATION HISTORY FROM VSOP CYCLE',I4,' TIMRIO  350
     1E STEP',I3/'                                 TO VSOP CYCLE',I4,' TRIO  360
     2IME STEP',I3/)                                                    RIO  370
 1190 FORMAT (12I6)                                                     RIO  380
 1200 FORMAT (6E12.5)                                                   RIO  390
 1210 FORMAT (E12.5,I6)                                                 RIO  400
C                                                                       RIO  410
C                                                                       RIO  420
      L = 0                                                             RIO  430
      K = 0                                                             RIO  440
      KT3 = 0                                                           RIO  450
      N61 = 67                                                          RIO  460
      OPEN(N61,FILE='origen')                                           RIO  470
      DO 1000 I=1,5                                                     RIO  480
        M60(I) = 0                                                      RIO  490
 1000 CONTINUE                                                          RIO  500
      I1 = 1                                                            RIO  510
      N60 = 39                                                          RIO  520
      REWIND N60                                                        RIO  530
    1 CONTINUE                                                          RIO  540
      READ (N60,END=80) ITOR                                            RIO  550
      BACKSPACE N60                                                     RIO  560
      IF(ITOR .NE. 6) READ (N60) ITOR,IPR15,JN,NXS,ISPK,DELDAY          RIO  570
      GOTO(10,20,30,40,50,60,70),ITOR                                   RIO  580
   10 CONTINUE                                                          RIO  590
      IS = ISPK                                                         RIO  600
      IF(ISPK .LE. LXS) GOTO 11                                         RIO  610
      WRITE (6,1020)                                                    RIO  620
      STOP                                                              RIO  630
   11 CONTINUE                                                          RIO  640
      DO 13 J=1,NXS                                                     RIO  650
        READ (N60) ((FSGMA(IS,J,I,N),I=1,N26),N=1,KFISS)                RIO  660
        READ (N60) ((ASGMA(IS,J,I,N),I=1,N26),N=1,KMAT)                 RIO  670
        READ (N60) ((SGMN2N(IS,J,I,N),I=1,N26),N=1,KFISS)               RIO  680
   13 CONTINUE                                                          RIO  690
      GOTO 1                                                            RIO  700
   20 CONTINUE                                                          RIO  710
      L = L + 1                                                         RIO  720
      READ (N60) ((FLX(L,I,IR),I=1,N26),IR=1,N200)                      RIO  730
      IP15(L) = IPR15 + 1                                               RIO  740
      JNZ(L) = JN                                                       RIO  750
      ISP(L) = ISPK                                                     RIO  760
      DELD(L) = DELDAY                                                  RIO  770
      IF(KT3 .GT. 0 .AND. IP15(L) .GT. KT3) GOTO 22                     RIO  780
      IF(LT0 .GT. 0 .AND. JNZ(L) .GT. LT0) GOTO 22                      RIO  790
      LST = L                                                           RIO  800
      KST = K                                                           RIO  810
   22 CONTINUE                                                          RIO  820
      GOTO 1                                                            RIO  830
   30 CONTINUE                                                          RIO  840
      K = K + 1                                                         RIO  850
      DO 31 IR=1,N200                                                   RIO  860
        READ (N60) VOL(K,IR),NRY(K,IR),(DEN(K,I,IR),I=1,KMAT)           RIO  870
        IF(K .EQ. 1) NRY(K,IR) = 0                                      RIO  880
   31 CONTINUE                                                          RIO  890
      IP15K(K) = IPR15                                                  RIO  900
      GOTO 1                                                            RIO  910
   40 CONTINUE                                                          RIO  920
      BACKSPACE N60                                                     RIO  930
      READ (N60) ITOR,KMAT,KFISS,N200,N26,AVO                           RIO  940
      WRITE (N61,1190) KMAT,KFISS,N200,N26                              RIO  950
      GOTO 1                                                            RIO  960
   50 CONTINUE                                                          RIO  970
      BACKSPACE N60                                                     RIO  980
      READ (N60) ITOR,(MATNR(I),I=1,KMAT)                               RIO  990
      WRITE (N61,1190) (MATNR(I),I=1,KMAT)                              RIO 1000
      GOTO 1                                                            RIO 1010
   60 CONTINUE                                                          RIO 1020
      READ (N60) ITOR,(NHOT(IR),IR=1,N200)                              RIO 1030
      GOTO 1                                                            RIO 1040
   70 CONTINUE                                                          RIO 1050
      DELD(L) = DELDAY                                                  RIO 1060
      GOTO 1                                                            RIO 1070
   80 CONTINUE                                                          RIO 1080
      REWIND N60                                                        RIO 1090
      I1 = I1 + 1                                                       RIO 1100
      IF(M60(I1) .LE. 0) GOTO 90                                        RIO 1110
      N60 = M60(I1)                                                     RIO 1120
      OPEN(N60,FORM='UNFORMATTED')                                      RIO 1130
      GOTO 1                                                            RIO 1140
   90 CONTINUE                                                          RIO 1150
      WRITE (6,1180) IP15(1),JNZ(1),IP15(LST),JNZ(LST)                  RIO 1160
      ISMIN = 1000                                                      RIO 1170
      DO 97 IR=1,N200                                                   RIO 1180
        I0 = 0                                                          RIO 1190
        IS = IR                                                         RIO 1200
        LM = LST                                                        RIO 1210
        KM = KST                                                        RIO 1220
   91   CONTINUE                                                        RIO 1230
        DO 92 I=1,N26                                                   RIO 1240
          FLUSS(I,LM,IR) = FLX(LM,I,IS)                                 RIO 1250
   92   CONTINUE                                                        RIO 1260
        DELT(LM,IR) = DELD(LM)                                          RIO 1270
        ISPL = ISP(LM)                                                  RIO 1280
        ISMIN = MIN0(ISMIN,ISPL)                                        RIO 1290
        NRSPEK(LM,IR) = ISPL                                            RIO 1300
        NRHOT(LM,IR) = NHOT(IS)                                         RIO 1310
        IF(JNZ(LM) .NE. 1) GOTO 96                                      RIO 1320
        NM = NRY(KM,IS)                                                 RIO 1330
        IF(I0 .EQ. 0) WRITE (6,1110) IR,NM                              RIO 1340
        IF(I0 .GT. 0) WRITE (6,1115) NM                                 RIO 1350
        I0 = I0 + 1                                                     RIO 1360
        IF(NM .GT. 0 .AND. NM .LE. N200) GOTO 95                        RIO 1370
        DO 93 N=1,KMAT                                                  RIO 1380
          AAKONZ(N,IR) = DEN(KM,N,IS) * VOL(KM,IS) / AVO                RIO 1390
   93   CONTINUE                                                        RIO 1400
        LDEL = LST - LM + 1                                             RIO 1410
        LSTMAX(IR) = LDEL                                               RIO 1420
        ISMAX = 0                                                       RIO 1430
        DO 94 L=1,LDEL                                                  RIO 1440
          L0 = LM + L - 1                                               RIO 1450
          DELT(L,IR) = DELT(L0,IR)                                      RIO 1460
          ISMAX = MAX0(ISMAX,NRSPEK(L0,IR))                             RIO 1470
          NRSPEK(L,IR) = NRSPEK(L0,IR)                                  RIO 1480
          NRHOT(L,IR) = NRHOT(L0,IR)                                    RIO 1490
          DO 94 I=1,N26                                                 RIO 1500
            FLUSS(I,L,IR) = FLUSS(I,L0,IR)                              RIO 1510
   94   CONTINUE                                                        RIO 1520
        GOTO 97                                                         RIO 1530
   95   CONTINUE                                                        RIO 1540
        IS = NM                                                         RIO 1550
        KM = KM - 1                                                     RIO 1560
   96   CONTINUE                                                        RIO 1570
        LM = LM - 1                                                     RIO 1580
        GOTO 91                                                         RIO 1590
   97 CONTINUE                                                          RIO 1600
      LMA = 0                                                           RIO 1610
      DO 100 IR=1,N200                                                  RIO 1620
        LDEL = LSTMAX(IR)                                               RIO 1630
        LMA = MAX0(LMA,LDEL)                                            RIO 1640
        DO 100 L=1,LDEL                                                 RIO 1650
          NRSPEK(L,IR) = (NRSPEK(L,IR)-ISMIN) * NXS + NRHOT(L,IR)       RIO 1660
  100 CONTINUE                                                          RIO 1670
      WRITE (6,1120)                                                    RIO 1680
      M = 0                                                             RIO 1690
      M1 = 1                                                            RIO 1700
  102 CONTINUE                                                          RIO 1710
      WRITE (6,1100)                                                    RIO 1720
      I0 = 0                                                            RIO 1730
      M = M + 1                                                         RIO 1740
      M30 = M * 30                                                      RIO 1750
      LW = 0                                                            RIO 1760
      LMI = MIN0(LMA,M30)                                               RIO 1770
      DO 101 IR=1,N200                                                  RIO 1780
        LDEL = LSTMAX(IR)                                               RIO 1790
        LDEO = LDEL                                                     RIO 1800
        IF(LDEO .GT. M30) LDEO = M30                                    RIO 1810
        IF(LDEL .GT. LDEO) LW = LW + 1                                  RIO 1820
        IF(I0 .EQ. 0) WRITE (6,1125) (LL,LL=M1,LMI)                     RIO 1830
        IF(I0 .EQ. 0) WRITE (6,1126) (ST,I=M1,LMI)                      RIO 1840
        IF(M1 .LE. LDEL) WRITE (6,1130) IR,(NRSPEK(LL,IR),LL=M1,LDEO)   RIO 1850
        IF(M1 .GT. LDEL) WRITE (6,1135) IR                              RIO 1860
        I0 = I0 + 1                                                     RIO 1870
  101 CONTINUE                                                          RIO 1880
      M1 = M30 + 1                                                      RIO 1890
      IF(LW .GT. 0) GOTO 102                                            RIO 1900
      ISPEKT = (ISMAX-ISMIN+1) * NXS                                    RIO 1910
      WRITE (N61,1190) ISPEKT                                           RIO 1920
      DO 110 IS=ISMIN,ISMAX                                             RIO 1930
        DO 110 J=1,NXS                                                  RIO 1940
          WRITE (N61,1200) ((FSGMA(IS,J,I,N),I=1,N26),N=1,KFISS)        RIO 1950
          WRITE (N61,1200) ((ASGMA(IS,J,I,N),I=1,N26),N=1,KMAT)         RIO 1960
          WRITE (N61,1200) ((SGMN2N(IS,J,I,N),I=1,N26),N=1,KFISS)       RIO 1970
  110 CONTINUE                                                          RIO 1980
      WRITE (6,1140) ISPEKT                                             RIO 1990
      DO 120 IR=1,N200                                                  RIO 2000
        LDEL = LSTMAX(IR)                                               RIO 2010
        WRITE (N61,1190) LDEL                                           RIO 2020
        DO 121 L=1,LDEL                                                 RIO 2030
          WRITE (N61,1210) DELT(L,IR),NRSPEK(L,IR)                      RIO 2040
          WRITE (N61,1200) (FLUSS(I,L,IR), I=1,N26)                     RIO 2050
  121   CONTINUE                                                        RIO 2060
  120 CONTINUE                                                          RIO 2070
      WRITE (N61,1200) ((AAKONZ(N,IR),N=1,KMAT),IR=1,N200)              RIO 2080
      IF(IOUT .LE. 0) GOTO 140                                          RIO 2090
      WRITE (6,1150) (GR(1),GR(2),I,I=1,N26)                            RIO 2100
      WRITE (6,1100)                                                    RIO 2110
      DO 130 IR=1,N200                                                  RIO 2120
        LDEL = LSTMAX(IR)                                               RIO 2130
        DO 130 L=1,LDEL                                                 RIO 2140
          WRITE (6,1160) IR,L,NRSPEK(L,IR),DELT(L,IR),(FLUSS(I,L,IR),I=1RIO 2150
     1     ,N26)                                                        RIO 2160
  130 CONTINUE                                                          RIO 2170
  140 CONTINUE                                                          RIO 2180
      WRITE (6,1170)                                                    RIO 2190
      RETURN                                                            RIO 2200
      END                                                               RIO 2210