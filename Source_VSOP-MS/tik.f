      SUBROUTINE TEMPKU                                                 TEM   10
C                                                                       TEM   20
      DIMENSION WL(2,201),POW(2),TL(N200),RK(N200)                      TEM   30
C                                                                       TEM   40
      COMMON /TDWL/ R1,R2,PQ,DELTAT,TU,TO,NSCH,KTEM(2),LFAD(2),         TEM   50
     1 TSTUE(2,25),DSTUE(2,10),WLSTUE(2,25,10),KKT,TF(201),WRIT,R0,     TEM   60
     2 VOT(101,201),VOTOT,IVO,IA,STANZ                                  TEM   70
C                                                                       TEM   80
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,JM1,KM,DKUG      TEM   90
C                                                                       TEM  100
      COMMON /ZEUG/ ITM3,IFZW,CRLSTG,IAX,IRA,MRZ,MERZ(5),RADI(50),      TEM  110
     1 TQ(5,50,20),SD(60,50,30),AB(20),DENKGL,NRUN,R3,R4,PIE,HCOR,ISPALTTEM  120
     2 ,JIC,BRENKU(50),IB(19),RR,KRUN,POWBAL,TR2,FADO,TT,TDR,WL0,WL2,   TEM  130
     3 TDRR2,TINTE,WIFI,IBU,N6,TG,TM,TGAS,FRACKU,ISUBRG                 TEM  140
C                                                                       TEM  150
      COMMON /FORALL/ IRAGRO,JINPO1,UU,VV,ZGROB(500),POGROB(500),IJ,    TEM  160
     1 FIM(100),IFZ,IWO,LLL(30),CASE(18),EXPO(15),IFZR,IREP,LLLL,LTAB,  TEM  170
     2 RADX(50),EPSIL,ZFEIN(60),MSPALT,NTYPEN                           TEM  180
C                                                                       TEM  190
      COMMON /RVON/ RR2,RR5,R03,RR3                                     TEM  200
C                                                                       TEM  210
      COMMON /SPECTI/ ITIK(10)                                          TEM  220
C                                                                       TEM  230
      COMMON /KOMP1/ KMAX                                               TEM  240
C                                                                       TEM  250
      COMMON /LAMRZS/ TKSTUE(25),WKSTUE(25,10),KTF,LFA,LFA1,LFB         TEM  260
C                                                                       TEM  270
      COMMON /HET1/ F                                                   TEM  280
C                                                                       TEM  290
      COMMON /EPTI/ EPSST,ZEITNW                                        TEM  300
C                                                                       TEM  310
CFZJ055                                                       25.09.07  TEM  320
C                                                                       TEM  330
      COMMON /TRANS/ IFINST,INTVAL                                      TEM  340
C                                                                       TEM  350
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             TEM  360
C                                                                       TEM  370
      COMMON /COUPL/ IPRINT                                             TEM  380
C                                                                       TEM  390
      DIMENSION IFLT(KMAZ)                                              TEM  400
C                                                                       TEM  410
  100 FORMAT (12I6)                                                     TEM  420
  101 FORMAT (6E12.5)                                                   TEM  430
  102 FORMAT (5E12.5)                                                   TEM  440
  200 FORMAT (/5X,3E12.5,I5/)                                           TEM  450
  201 FORMAT (5X,I5,4E12.5)                                             TEM  460
  202 FORMAT (10E12.5)                                                  TEM  470
  203 FORMAT ('1')                                                      TEM  480
  204 FORMAT (' ****',2I5,3E12.5,'****')                                TEM  490
  205 FORMAT (//' AVERAGE TEMPERATURE IN FUEL MATRIX: ',E12.5//)        TEM  500
  400 FORMAT (2I6,5E12.5)                                               TEM  510
C                                                                       TEM  520
C                                                                       TEM  530
      ENTRY TEMPK0(IFLT)                                                TEM  540
C                                                                       TEM  550
      N5 = 5                                                            TEM  560
      N6 = 6                                                            TEM  570
      R1 = R3                                                           TEM  580
      R2 = R4                                                           TEM  590
      IVO = 1                                                           TEM  600
      IBURN = 101                                                       TEM  610
      K4 = 0                                                            TEM  620
      DO0 = 0.                                                          TEM  630
      DO1 = 0.                                                          TEM  640
      IFL0 = 0                                                          TEM  650
C                                                                       TEM  660
CARD TX20                                                               TEM  670
C                                                                       TEM  680
      IF(ITIK(1) .LE. 1) READ (N5,102) DELTAT,TU,TO,WRIT,R0             TEM  690
C                                                                       TEM  700
      ISTANZ = 0                                                        TEM  710
      IF(A0 .LE. 0.) A0 = 8.                                            TEM  720
      STANZ = FLOAT(ISTANZ)                                             TEM  730
      IF(WRIT .LT. 0.) GOTO 41                                          TEM  740
      NSCH = 1                                                          TEM  750
      KTEM(1) = 15                                                      TEM  760
      KTE = KTEM(1)                                                     TEM  770
      LFAD (1) = 5                                                      TEM  780
      QZ = 0.                                                           TEM  790
      DO 44 K=1,KTE                                                     TEM  800
        IF(K .LE. 10) QZ = QZ + 100.                                    TEM  810
        IF(K .GT. 10) QZ = QZ + 300.                                    TEM  820
        TSTUE(1,K) = QZ                                                 TEM  830
   44 CONTINUE                                                          TEM  840
      DSTUE(1,1) = 0.                                                   TEM  850
      DSTUE(1,2) = 0.44E+21                                             TEM  860
      DSTUE(1,3) = 1.67E+21                                             TEM  870
      DSTUE(1,4) = 2.98E+21                                             TEM  880
      DSTUE(1,5) = 6.09E+21                                             TEM  890
      WLSTUE(1,1,1) = 0.4940                                            TEM  900
      WLSTUE(1,2,1) = 0.4460                                            TEM  910
      WLSTUE(1,3,1) = 0.4115                                            TEM  920
      WLSTUE(1,4,1) = 0.3790                                            TEM  930
      WLSTUE(1,5,1) = 0.3555                                            TEM  940
      WLSTUE(1,6,1) = 0.3300                                            TEM  950
      WLSTUE(1,7,1) = 0.3100                                            TEM  960
      WLSTUE(1,8,1) = 0.2875                                            TEM  970
      WLSTUE(1,9,1) = 0.2740                                            TEM  980
      WLSTUE(1,10,1) = 0.2625                                           TEM  990
      WLSTUE(1,11,1) = 0.235                                            TEM 1000
      WLSTUE(1,12,1) = 0.210                                            TEM 1010
      WLSTUE(1,13,1) = 0.185                                            TEM 1020
      WLSTUE(1,14,1) = 0.165                                            TEM 1030
      WLSTUE(1,15,1) = 0.150                                            TEM 1040
      WLSTUE(1,1,2) = 0.3100                                            TEM 1050
      WLSTUE(1,2,2) = 0.3060                                            TEM 1060
      WLSTUE(1,3,2) = 0.2900                                            TEM 1070
      WLSTUE(1,4,2) = 0.2740                                            TEM 1080
      WLSTUE(1,5,2) = 0.2635                                            TEM 1090
      WLSTUE(1,6,2) = 0.2525                                            TEM 1100
      WLSTUE(1,7,2) = 0.2445                                            TEM 1110
      WLSTUE(1,8,2) = 0.2335                                            TEM 1120
      WLSTUE(1,9,2) = 0.2260                                            TEM 1130
      WLSTUE(1,10,2) = 0.2190                                           TEM 1140
      WLSTUE(1,11,2) = 0.204                                            TEM 1150
      WLSTUE(1,12,2) = 0.189                                            TEM 1160
      WLSTUE(1,13,2) = 0.175                                            TEM 1170
      WLSTUE(1,14,2) = 0.160                                            TEM 1180
      WLSTUE(1,15,2) = 0.145                                            TEM 1190
      WLSTUE(1,1,3) = 0.2025                                            TEM 1200
      WLSTUE(1,2,3) = 0.2160                                            TEM 1210
      WLSTUE(1,3,3) = 0.2170                                            TEM 1220
      WLSTUE(1,4,3) = 0.2135                                            TEM 1230
      WLSTUE(1,5,3) = 0.2090                                            TEM 1240
      WLSTUE(1,6,3) = 0.2015                                            TEM 1250
      WLSTUE(1,7,3) = 0.1945                                            TEM 1260
      WLSTUE(1,8,3) = 0.1895                                            TEM 1270
      WLSTUE(1,9,3) = 0.1845                                            TEM 1280
      WLSTUE(1,10,3) = 0.1815                                           TEM 1290
      WLSTUE(1,11,3) = 0.173                                            TEM 1300
      WLSTUE(1,12,3) = 0.165                                            TEM 1310
      WLSTUE(1,13,3) = 0.157                                            TEM 1320
      WLSTUE(1,14,3) = 0.148                                            TEM 1330
      WLSTUE(1,15,3) = 0.140                                            TEM 1340
      WLSTUE(1,1,4) = 0.1510                                            TEM 1350
      WLSTUE(1,2,4) = 0.1660                                            TEM 1360
      WLSTUE(1,3,4) = 0.1710                                            TEM 1370
      WLSTUE(1,4,4) = 0.1740                                            TEM 1380
      WLSTUE(1,5,4) = 0.1735                                            TEM 1390
      WLSTUE(1,6,4) = 0.1725                                            TEM 1400
      WLSTUE(1,7,4) = 0.1720                                            TEM 1410
      WLSTUE(1,8,4) = 0.1715                                            TEM 1420
      WLSTUE(1,9,4) = 0.1710                                            TEM 1430
      WLSTUE(1,10,4) = 0.1705                                           TEM 1440
      WLSTUE(1,11,4) = 0.163                                            TEM 1450
      WLSTUE(1,12,4) = 0.156                                            TEM 1460
      WLSTUE(1,13,4) = 0.149                                            TEM 1470
      WLSTUE(1,14,4) = 0.142                                            TEM 1480
      WLSTUE(1,15,4) = 0.135                                            TEM 1490
      WLSTUE(1,1,5) = 0.1211                                            TEM 1500
      WLSTUE(1,2,5) = 0.1359                                            TEM 1510
      WLSTUE(1,3,5) = 0.1465                                            TEM 1520
      WLSTUE(1,4,5) = 0.1500                                            TEM 1530
      WLSTUE(1,5,5) = 0.1506                                            TEM 1540
      WLSTUE(1,6,5) = 0.1486                                            TEM 1550
      WLSTUE(1,7,5) = 0.1472                                            TEM 1560
      WLSTUE(1,8,5) = 0.1423                                            TEM 1570
      WLSTUE(1,9,5) = 0.1415                                            TEM 1580
      WLSTUE(1,10,5) = 0.1408                                           TEM 1590
      WLSTUE(1,11,5) = 0.139                                            TEM 1600
      WLSTUE(1,12,5) = 0.137                                            TEM 1610
      WLSTUE(1,13,5) = 0.135                                            TEM 1620
      WLSTUE(1,14,5) = 0.133                                            TEM 1630
      WLSTUE(1,15,5) = 0.131                                            TEM 1640
   41 CONTINUE                                                          TEM 1650
      IF(WRIT .GE. 0.) GOTO 42                                          TEM 1660
C                                                                       TEM 1670
CARD TX21                                                               TEM 1680
C                                                                       TEM 1690
      IF(ITIK(1) .LE. 1) READ (N5,100) NSCH,(KTEM(N),N=1,NSCH),(LFAD(N),TEM 1700
     1 N=1,NSCH)                                                        TEM 1710
C                                                                       TEM 1720
   42 CONTINUE                                                          TEM 1730
      STAN = STANZ                                                      TEM 1740
      LFA1 = 10                                                         TEM 1750
      DO 1 N=1,NSCH                                                     TEM 1760
        KTE = KTEM(N)                                                   TEM 1770
        LFA = LFAD(N)                                                   TEM 1780
        IF(WRIT .GE. 0.) GOTO 43                                        TEM 1790
        IF(ITIK(1) .GT. 1) GOTO 43                                      TEM 1800
C                                                                       TEM 1810
CARD TX22                                                               TEM 1820
C                                                                       TEM 1830
        READ (N5,101) (TSTUE(N,K),K=1,KTE)                              TEM 1840
C                                                                       TEM 1850
CARD TX23                                                               TEM 1860
C                                                                       TEM 1870
        READ (N5,101) (DSTUE(N,L),L=1,LFA)                              TEM 1880
C                                                                       TEM 1890
CARD TX24                                                               TEM 1900
C                                                                       TEM 1910
        DO 22 L=1,LFA                                                   TEM 1920
C                                                                       TEM 1930
          READ (N5,101) (WLSTUE(N,K,L),K=1,KTE)                         TEM 1940
C                                                                       TEM 1950
   22   CONTINUE                                                        TEM 1960
   43   CONTINUE                                                        TEM 1970
        LFB = LFA                                                       TEM 1980
        IF(LFA .LE. 1) GOTO 1                                           TEM 1990
        LFB = LFA + 1                                                   TEM 2000
        DSTUE(N,LFB) = 3. * DSTUE(N,LFA) - 2. * DSTUE(N,LFA-1)          TEM 2010
        DO 30 K=1,KTE                                                   TEM 2020
          WLSTUE(N,K,LFB) = WLSTUE(N,K,LFA) * 2. - WLSTUE(N,K,LFA-1)    TEM 2030
   30   CONTINUE                                                        TEM 2040
    1 CONTINUE                                                          TEM 2050
      KKT = IFIX((TO-TU)/DELTAT)                                        TEM 2060
      KKT1 = KKT + 1                                                    TEM 2070
      DO 2 I=1,KKT1                                                     TEM 2080
        TF(I) = TU + (FLOAT(I-1)*DELTAT)                                TEM 2090
    2 CONTINUE                                                          TEM 2100
      ISUBRG = 0                                                        TEM 2110
      DO 301 K=1,KMAX                                                   TEM 2120
        IGERW = IFLT(K)                                                 TEM 2130
        IF(IGERW .EQ. 25) GOTO 302                                      TEM 2140
        IF(IGERW .EQ. 26) GOTO 302                                      TEM 2150
        IF(IGERW .EQ. 27) GOTO 302                                      TEM 2160
  301 CONTINUE                                                          TEM 2170
      GOTO 399                                                          TEM 2180
  302 CONTINUE                                                          TEM 2190
      N = NSCH                                                          TEM 2200
      KTF = 25                                                          TEM 2210
      DO 303 K=1,KTF                                                    TEM 2220
        IF(K .GT. KTE) GOTO 304                                         TEM 2230
        QZ = TSTUE(N,K)                                                 TEM 2240
        GOTO 305                                                        TEM 2250
  304   CONTINUE                                                        TEM 2260
        QZ = QZ + 100.                                                  TEM 2270
  305   TKSTUE(K) = QZ                                                  TEM 2280
  303 CONTINUE                                                          TEM 2290
C                                                                       TEM 2300
C     LAMBDA-EFFEKTIV NACH ROBOLD BZW. ZEHNER-SCHLUENDER                TEM 2310
C                                                                       TEM 2320
      PR = 0.                                                           TEM 2330
      CS = 5.67E-4                                                      TEM 2340
      DK = 6.                                                           TEM 2350
      FF = F                                                            TEM 2360
      PB = 1.                                                           TEM 2370
      DO 309 L=1,LFB                                                    TEM 2380
        DO 309 K=1,KTF                                                  TEM 2390
          IF(K .LE. KTE) WF = WLSTUE(N,K,L)                             TEM 2400
          TK = TKSTUE(K) + 273.                                         TEM 2410
C                                                                       TEM 2420
          EPSG = GREPS(TK)                                              TEM 2430
C                                                                       TEM 2440
          IF(EPSST .GT. 0.) EPSG = EPSST                                TEM 2450
          TKE = .71 * (1.-2.E-4*PB)                                     TEM 2460
          TK1 = TK**TKE                                                 TEM 2470
          XLGAS = 2.682E-3 * TK1 * (1.+1.123E-3*PB) / 100.              TEM 2480
C                                                                       TEM 2490
          XL = ROBOLD(CS*1.E-8,EPSG,DK,FF,TK,WF,XLGAS,PR)               TEM 2500
C                                                                       TEM 2510
          XM = ZEHSL(CS,EPSG,DK,FF,TK,WF,XLGAS,PR)                      TEM 2520
C                                                                       TEM 2530
          IF(IGERW .EQ. 26) XL = XM                                     TEM 2540
          IF(IGERW .EQ. 27) XL = AMAX1(XL,XM)                           TEM 2550
          TP = AMAX1(250.,TKSTUE(K))                                    TEM 2560
          TP1 = TP - 150.                                               TEM 2570
          TP1 = TP1**1.29                                               TEM 2580
          P = 1.9E-5 * TP1                                              TEM 2590
          IF(INTVAL .LE. 1 .AND. IPRINT .EQ. 2) WRITE (6,400) L,K,      TEM 2600
     1     DSTUE(N,L),TKSTUE(K),WF,XL,P                                 TEM 2610
          WKSTUE(K,L) = XL                                              TEM 2620
  309 CONTINUE                                                          TEM 2630
  399 CONTINUE                                                          TEM 2640
      RETURN                                                            TEM 2650
C                                                                       TEM 2660
C     WAERMELEITFAEHIGKEIT FUER DIE GEGEBENE FAST DOSIS *FADO* IN DEN   TEM 2670
C     TEMPERATURSTUETZSTELLEN                                           TEM 2680
C                                                                       TEM 2690
      ENTRY TEMPK1(JFIRST,N200,TL,RK)                                   TEM 2700
C                                                                       TEM 2710
      IF(JFIRST .GT. 0) GOTO 25                                         TEM 2720
      ISUBRG = ISUBRG + 1                                               TEM 2730
      DO 24 I=1,201                                                     TEM 2740
        DO 24 J=1,IBURN                                                 TEM 2750
          VOT(J,I) = 0.                                                 TEM 2760
   24 CONTINUE                                                          TEM 2770
      JFIRST = 1                                                        TEM 2780
   25 CONTINUE                                                          TEM 2790
      IF(IBU .GT. 0) IVO = IBU                                          TEM 2800
      WIFI = WIFI * 4. * PI / 3.                                        TEM 2810
      IF (WRIT .GE. 1.) WRITE (N6,200) POWBAL,TR2,FADO,KKT              TEM 2820
   26 CONTINUE                                                          TEM 2830
      DO 8 N=1,NSCH                                                     TEM 2840
        KTE = KTEM(N)                                                   TEM 2850
        LFA = LFAD(N)                                                   TEM 2860
        LFB = LFA                                                       TEM 2870
        IF(LFA .GT. 1) LFB = LFB + 1                                    TEM 2880
        IF(FADO .LT. DSTUE(N,LFA)) GOTO 4                               TEM 2890
        DO 3 K=1,KTE                                                    TEM 2900
          WLSTUE(N,K,LFA1) = WLSTUE(N,K,LFA)                            TEM 2910
    3   CONTINUE                                                        TEM 2920
        GOTO 7                                                          TEM 2930
    4   CONTINUE                                                        TEM 2940
        DO 6 K=1,KTE                                                    TEM 2950
          DO 5 L=1,LFB                                                  TEM 2960
            ZGROB(L) = DSTUE(N,L)                                       TEM 2970
            POGROB(L) = WLSTUE(N,K,L)                                   TEM 2980
    5     CONTINUE                                                      TEM 2990
          IJ = LFB                                                      TEM 3000
          UU = FADO                                                     TEM 3010
C                                                                       TEM 3020
          CALL LAGRAS                                                   TEM 3030
C                                                                       TEM 3040
          WLSTUE(N,K,LFA1) = VV                                         TEM 3050
    6   CONTINUE                                                        TEM 3060
    7   CONTINUE                                                        TEM 3070
        IF (WRIT .GE. 1.) WRITE (N6,202) FADO,(WLSTUE(N,K,LFA1),K=1,KTE)TEM 3080
    8 CONTINUE                                                          TEM 3090
      IF(K4 .GT. 0) GOTO 27                                             TEM 3100
C                                                                       TEM 3110
C     FEINE INTERVALLE IN WL (OLD TIK-LSD-VERSION)                      TEM 3120
C                                                                       TEM 3130
      WL2 = 0.                                                          TEM 3140
      DO 14 N=1,NSCH                                                    TEM 3150
        KTE = KTEM(N)                                                   TEM 3160
        DO 9 K=1,KTE                                                    TEM 3170
          ZGROB(K) = TSTUE(N,K)                                         TEM 3180
          POGROB(K) = WLSTUE(N,K,LFA1)                                  TEM 3190
    9   CONTINUE                                                        TEM 3200
        IFIRST = 0                                                      TEM 3210
        IA = 0                                                          TEM 3220
        VV = ZGROB(1)                                                   TEM 3230
        DO 23 I=1,KKT                                                   TEM 3240
          IF(TF(I+1) .LT. TR2) GOTO 23                                  TEM 3250
          IF(IA .EQ. 0) IA = I                                          TEM 3260
          IF(TF(I) .GE. ZGROB(1)) GOTO 10                               TEM 3270
          WL(N,I) = POGROB(1)                                           TEM 3280
          GOTO 13                                                       TEM 3290
   10     CONTINUE                                                      TEM 3300
          IF(TF(I+1) .LE. ZGROB(KTE)) GOTO 11                           TEM 3310
          WL(N,I) = POGROB(KTE)                                         TEM 3320
          GOTO 13                                                       TEM 3330
   11     CONTINUE                                                      TEM 3340
          IF(IFIRST .GT. 0) GOTO 12                                     TEM 3350
          TFI1 = TF(I)                                                  TEM 3360
          IJ = KTE                                                      TEM 3370
          UU = TFI1                                                     TEM 3380
C                                                                       TEM 3390
          CALL LAGRAS                                                   TEM 3400
C                                                                       TEM 3410
          IFIRST = 1                                                    TEM 3420
   12     CONTINUE                                                      TEM 3430
          W0 = VV                                                       TEM 3440
          TFI1 = TF(I+1)                                                TEM 3450
          IJ = KTE                                                      TEM 3460
          UU = TFI1                                                     TEM 3470
C                                                                       TEM 3480
          CALL LAGRAS                                                   TEM 3490
C                                                                       TEM 3500
          WL(N,I) = (W0+VV) / 2.                                        TEM 3510
   13     CONTINUE                                                      TEM 3520
          IF(WL2 .EQ. 0.) WL2 = WL(N,I)                                 TEM 3530
   23   CONTINUE                                                        TEM 3540
   14 CONTINUE                                                          TEM 3550
      IF(NSCH .EQ. 2) GOTO 16                                           TEM 3560
      DO 15 I=1,KKT                                                     TEM 3570
        WL(2,I) = WL(1,I)                                               TEM 3580
   15 CONTINUE                                                          TEM 3590
   16 CONTINUE                                                          TEM 3600
C                                                                       TEM 3610
C     JETZT DIE INTEGRATION                                             TEM 3620
C                                                                       TEM 3630
      R03 = R0**3                                                       TEM 3640
      R13 = R1**3                                                       TEM 3650
      IF(POWBAL .LT. 1.E-10) POWBAL = 1.E-10                            TEM 3660
      POW(2) = POWBAL / (4.*PI)                                         TEM 3670
      POW(1) = POW(2) / (2.*(R13-R03))                                  TEM 3680
      RM2 = RR**2.                                                      TEM 3690
      R12 = R1**2.                                                      TEM 3700
      RM1 = AMAX1(R1,RR)                                                TEM 3710
      N = 2                                                             TEM 3720
      RR2 = 0.                                                          TEM 3730
      RR1 = R2                                                          TEM 3740
      TT1 = TR2                                                         TEM 3750
      IIT1 = 1                                                          TEM 3760
      IIT = 1                                                           TEM 3770
      RK(IIT) = R2                                                      TEM 3780
      TL(IIT) = TR2                                                     TEM 3790
      I = IA                                                            TEM 3800
  119 CONTINUE                                                          TEM 3810
      IM1 = 0                                                           TEM 3820
      TT2 = TF(I+1)                                                     TEM 3830
      IF(N .EQ. 1) GOTO 17                                              TEM 3840
      IF(TF(I+1) .LE. TR2) GOTO 19                                      TEM 3850
      RR2 = 1. / RR1 + WL(N,I) * (TT2-TT1) / POW(N)                     TEM 3860
      RR2 = 1. / RR2                                                    TEM 3870
      RR3 = RR2                                                         TEM 3880
      RR4 = RR3                                                         TEM 3890
      RRE2 = RR2**2.                                                    TEM 3900
      IF(RR2 .GT. 0.) TDR = -POW(N) / (WL(N,I)*RRE2)                    TEM 3910
      IF(RR2 .GT. RM1) GOTO 18                                          TEM 3920
      RR2 = RM1                                                         TEM 3930
      TT2 = TT1 + POW(N) * (1./RR2-1./RR1) / WL(N,I)                    TEM 3940
      RRE2 = RR2**2.                                                    TEM 3950
      TDR = -POW(N) / (WL(N,I)*RRE2)                                    TEM 3960
      RR3 = RR2                                                         TEM 3970
      N = 1                                                             TEM 3980
      TDRR2 = ABS(TDR)                                                  TEM 3990
      TINTE = TT2                                                       TEM 4000
      RR11 = RR1                                                        TEM 4010
      RR22 = RR2                                                        TEM 4020
      RR1 = R12                                                         TEM 4030
      RR2 = RR2**2.                                                     TEM 4040
      IF(RR2 .EQ. RM2) GOTO 20                                          TEM 4050
      IF(R0 .LE. 0.) GOTO 40                                            TEM 4060
      RR1 = RR1 + 2. * R03 / RR11                                       TEM 4070
      RR2 = RR2 + 2. * R03 / RR22                                       TEM 4080
      RR  = AMAX1(R0,RR)                                                TEM 4090
      RRE = RR**2.                                                      TEM 4100
      RM2 = RRE + 2. * R03 / RR                                         TEM 4110
   40 CONTINUE                                                          TEM 4120
      IF(RR3 .GT. RR4) IM1 = -1                                         TEM 4130
      RR5 = R1                                                          TEM 4140
      IF(RR3 .EQ. R1) GOTO 18                                           TEM 4150
   17 CONTINUE                                                          TEM 4160
      RR2 = RR1 + WL(N,I) * (TT1-TT2) / POW(N)                          TEM 4170
      RR53 = RR5**3.                                                    TEM 4180
      IF(RR2 .LE. RM2) GOTO 21                                          TEM 4190
      RR3 = SQRT(RR2)                                                   TEM 4200
C                                                                       TEM 4210
      IF(R0 .GT. 0.) CALL RVONT                                         TEM 4220
C                                                                       TEM 4230
      RR33 = RR3**3.                                                    TEM 4240
      VOT(IVO,I) = VOT(IVO,I) + (WIFI*(RR53 - RR33))                    TEM 4250
      IF(WRIT .GE. 1.) WRITE (N6,204) IVO,I,RR5,RR3,VOT(IVO,I)          TEM 4260
      TDR = 2. * POW(N) * RR3 / WL(N,I)                                 TEM 4270
      IF(R0 .GT. 0.) TDR = 0.                                           TEM 4280
      GOTO 18                                                           TEM 4290
   21 TT2 = TT1 + POW(N) * (RR1-RM2) / WL(N,I)                          TEM 4300
      RRE3 = RR**3.                                                     TEM 4310
      VOT(IVO,I) = VOT(IVO,I) + (WIFI*(RR53 - RRE3))                    TEM 4320
      TDR = 2. * POW(N) * RR / WL(N,I)                                  TEM 4330
      IF(R0 .GT. 0.) TDR = 0.                                           TEM 4340
      RR3 = RR                                                          TEM 4350
      GOTO 20                                                           TEM 4360
   18 CONTINUE                                                          TEM 4370
      RR1 = RR2                                                         TEM 4380
      TT1 = TT2                                                         TEM 4390
      IF (WRIT .GE. 1.) WRITE (N6,201) I,RR3,TT2,WL(N,I),TDR            TEM 4400
      IIT = IIT + 1                                                     TEM 4410
      RK(IIT) = RR3                                                     TEM 4420
      TL(IIT) = TT2                                                     TEM 4430
      IF(RK(IIT) .EQ. R1) IIT1 = IIT                                    TEM 4440
   19 CONTINUE                                                          TEM 4450
      IF(I .GE. KKT) GOTO 20                                            TEM 4460
      I = I + 1 + IM1                                                   TEM 4470
      RR5 = RR3                                                         TEM 4480
      GOTO 119                                                          TEM 4490
   20 CONTINUE                                                          TEM 4500
      IF (WRIT .GE. 1.) WRITE (N6,201) I,RR3,TT2,WL(N,I),TDR            TEM 4510
      IIT = IIT + 1                                                     TEM 4520
      RK(IIT) = RR3                                                     TEM 4530
      TL(IIT) = TT2                                                     TEM 4540
      VG = 0.                                                           TEM 4550
      VM = 0.                                                           TEM 4560
      TG = 0.                                                           TEM 4570
      TM = 0.                                                           TEM 4580
      IIT = IIT - 1                                                     TEM 4590
      DO 33 J=1,IIT                                                     TEM 4600
        RK0 = RK(J)**3.                                                 TEM 4610
        RK1 = RK(J+1)**3.                                               TEM 4620
        VV = RK0 - RK1                                                  TEM 4630
        VG = VG + VV                                                    TEM 4640
        TKV = VV * (TL(J)+TL(J+1)) / 2.                                 TEM 4650
        TG = TG + TKV                                                   TEM 4660
        IF(J .LT. IIT1) GOTO 33                                         TEM 4670
        VM = VM + VV                                                    TEM 4680
        TM = TM + TKV                                                   TEM 4690
   33 CONTINUE                                                          TEM 4700
      IF(RK(IIT+1) .LE. 0.) GOTO 31                                     TEM 4710
      VV = RK(IIT+1)**3.                                                TEM 4720
      VG = VG + VV                                                      TEM 4730
      TG = TG + VV * TL(IIT+1)                                          TEM 4740
   31 CONTINUE                                                          TEM 4750
      TG = (TG/VG) * FRACKU + (1.-FRACKU) * TGAS                        TEM 4760
      IF(VM .NE. 0.) TM = TM / VM                                       TEM 4770
      WL0 = WL(N,I)                                                     TEM 4780
      TT = TT2                                                          TEM 4790
      RETURN                                                            TEM 4800
C                                                                       TEM 4810
      ENTRY TEMPK4(TN,FDO,TDLAM,IFL1)                                   TEM 4820
C                                                                       TEM 4830
      IF(FDO .LE. 0.) FDO = 1.E18                                       TEM 4840
      DLFD = (FDO-DO0) / FDO                                            TEM 4850
      DLFD = ABS(DLFD)                                                  TEM 4860
      IF(IFL1 .NE. IFL0) GOTO 34                                        TEM 4870
      IF(DLFD .LT. 0.03) GOTO 29                                        TEM 4880
   34 CONTINUE                                                          TEM 4890
      K4 = 1                                                            TEM 4900
      FADO = FDO                                                        TEM 4910
      GOTO 26                                                           TEM 4920
   27 CONTINUE                                                          TEM 4930
      K4 = 0                                                            TEM 4940
      DO0 = FDO                                                         TEM 4950
      N = 1                                                             TEM 4960
      DO 28 L=1,KTE                                                     TEM 4970
        ZGROB(L) = TSTUE(N,L)                                           TEM 4980
        POGROB(L) = WLSTUE(N,L,LFA1)                                    TEM 4990
   28 CONTINUE                                                          TEM 5000
      IJ = KTE                                                          TEM 5010
   29 CONTINUE                                                          TEM 5020
      UU = TN                                                           TEM 5030
C                                                                       TEM 5040
      CALL LAGRAS                                                       TEM 5050
C                                                                       TEM 5060
      TDLAM = VV                                                        TEM 5070
      IFL0 = IFL1                                                       TEM 5080
      RETURN                                                            TEM 5090
C                                                                       TEM 5100
      ENTRY TEMPK5(TN,FDO,TDLAM,IFL1)                                   TEM 5110
C                                                                       TEM 5120
      IF(FDO .LE. 0.) FDO = 1.E18                                       TEM 5130
      DLFD = (FDO-DO1) / FDO                                            TEM 5140
      DLFD = ABS(DLFD)                                                  TEM 5150
      IF(IFL1 .NE. IFL0) GOTO 35                                        TEM 5160
      IF(DLFD .LT. 0.03) GOTO 39                                        TEM 5170
   35 CONTINUE                                                          TEM 5180
      FADO = FDO                                                        TEM 5190
      N = NSCH                                                          TEM 5200
      IF(FADO .LT. DSTUE(N,LFA)) GOTO 74                                TEM 5210
      DO 73 K=1,KTF                                                     TEM 5220
        WKSTUE(K,LFA1) = WKSTUE(K,LFA)                                  TEM 5230
   73 CONTINUE                                                          TEM 5240
      GOTO 77                                                           TEM 5250
   74 CONTINUE                                                          TEM 5260
      DO 76 K=1,KTF                                                     TEM 5270
        DO 75 L=1,LFB                                                   TEM 5280
          ZGROB(L) = DSTUE(N,L)                                         TEM 5290
          POGROB(L) = WKSTUE(K,L)                                       TEM 5300
   75   CONTINUE                                                        TEM 5310
        IJ = LFB                                                        TEM 5320
        UU = FADO                                                       TEM 5330
C                                                                       TEM 5340
        CALL LAGRAS                                                     TEM 5350
C                                                                       TEM 5360
        WKSTUE(K,LFA1) = VV                                             TEM 5370
   76 CONTINUE                                                          TEM 5380
   77 CONTINUE                                                          TEM 5390
      IF (WRIT .GE. 1.) WRITE (N6,202) FADO,(WKSTUE(K,LFA1),K=1,KTF)    TEM 5400
      DO1 = FDO                                                         TEM 5410
      DO 38 L=1,KTF                                                     TEM 5420
        ZGROB(L) = TKSTUE(L)                                            TEM 5430
        POGROB(L) = WKSTUE(L,LFA1)                                      TEM 5440
   38 CONTINUE                                                          TEM 5450
   39 CONTINUE                                                          TEM 5460
      IJ = KTF                                                          TEM 5470
      UU = TN                                                           TEM 5480
C                                                                       TEM 5490
      CALL LAGRAS                                                       TEM 5500
C                                                                       TEM 5510
      TDLAM = VV                                                        TEM 5520
      IFL0 = IFL1                                                       TEM 5530
      RETURN                                                            TEM 5540
      END                                                               TEM 5550
      FUNCTION GREPS(T)                                                 GRE   10
C                                                                       GRE   20
      REAL*4 ZG(10),PG(10)                                              GRE   30
C                                                                       GRE   40
      DATA ZG/3.E+2,5.E+2,7.E+2,9.E+2,1.1E+3,1.3E+3,1.5E+3,1.7E+3,1.9E+3GRE   50
     1 ,1.E+10/,IJ/10/,PG/.7,.78,.85,.88,.88,.90,.89,.89,.88,.88/       GRE   60
C                                                                       GRE   70
C                                                                       GRE   80
      CALL LAGRGW(IJ,ZG,PG,T,E)                                         GRE   90
C                                                                       GRE  100
      GREPS = E                                                         GRE  110
C                                                                       GRE  120
      RETURN                                                            GRE  130
      END                                                               GRE  140
      SUBROUTINE LAGRGW(IJ,ZGROB,POGROB,UU,VV)                          GRG   10
C                                                                       GRG   20
C     INTERPOLATION NACH LAGRANGE                                       GRG   30
C                                                                       GRG   40
CFZJ055                                                       25.09.07  GRG   50
C                                                                       GRG   60
      REAL*4 ZGROB(IJ),POGROB(IJ)                                       GRG   70
C                                                                       GRG   80
      REAL*8 A1,A2,A3,A,B,C,D,B1,B2,B3,C1,C2,C3,D1,D2,D3,FX1,FX2,FX3,FX4GRG   90
C                                                                       GRG  100
C                                                                       GRG  110
      IF(IJ .LE. 2) GOTO 9                                              GRG  120
      IF(ZGROB(IJ)-UU) 9,1,1                                            GRG  130
    1 CONTINUE                                                          GRG  140
      DO 2 II=1,IJ                                                      GRG  150
        IP = II                                                         GRG  160
        IF(ZGROB(IP)-UU) 2,3,4                                          GRG  170
    2 CONTINUE                                                          GRG  180
    3 VV = POGROB(IP)                                                   GRG  190
      RETURN                                                            GRG  200
    4 IP = IP - 1                                                       GRG  210
      IF(IP-1) 10,5,6                                                   GRG  220
    5 IP = IP + 1                                                       GRG  230
      GOTO 8                                                            GRG  240
    6 IF(IJ-IP-2) 7,8,8                                                 GRG  250
    7 IP = IP - 1                                                       GRG  260
C                                                                       GRG  270
C     FAKTOREN FUER DEN ZAEHLER                                         GRG  280
C                                                                       GRG  290
    8 A = UU - ZGROB(IP-1)                                              GRG  300
      B = UU - ZGROB(IP)                                                GRG  310
      C = UU - ZGROB(IP+1)                                              GRG  320
      D = UU - ZGROB(IP+2)                                              GRG  330
C                                                                       GRG  340
C     FAKTOREN FUER DEN NENNER                                          GRG  350
C                                                                       GRG  360
      A1 = ZGROB(IP-1) - ZGROB(IP)                                      GRG  370
      A2 = ZGROB(IP-1) - ZGROB(IP+1)                                    GRG  380
      A3 = ZGROB(IP-1) - ZGROB(IP+2)                                    GRG  390
      B1 = -A1                                                          GRG  400
      B2 = ZGROB(IP) - ZGROB(IP+1)                                      GRG  410
      B3 = ZGROB(IP) - ZGROB(IP+2)                                      GRG  420
      C1 = -A2                                                          GRG  430
      C2 = -B2                                                          GRG  440
      C3 = ZGROB(IP+1) - ZGROB(IP+2)                                    GRG  450
      D1 = -A3                                                          GRG  460
      D2 = -B3                                                          GRG  470
      D3 = -C3                                                          GRG  480
      FX1 = (B*C*D*POGROB(IP-1)) / (A1*A2*A3)                           GRG  490
      FX2 = (A*C*D*POGROB(IP)) / (B1*B2*B3)                             GRG  500
      FX3 = (A*B*D*POGROB(IP+1)) / (C1*C2*C3)                           GRG  510
      FX4 = (A*B*C*POGROB(IP+2)) / (D1*D2*D3)                           GRG  520
      VV = FX1 + FX2 + FX3 + FX4                                        GRG  530
      RETURN                                                            GRG  540
C                                                                       GRG  550
C     LINEARE EXTRAPOLATION                                             GRG  560
C                                                                       GRG  570
    9 IP = IJ                                                           GRG  580
      VV = POGROB(IP-1) + (POGROB(IP)-POGROB(IP-1)) / (ZGROB(IP)-       GRG  590
     1 ZGROB(IP-1)) * (UU-ZGROB(IP-1))                                  GRG  600
      RETURN                                                            GRG  610
   10 VV = (POGROB(2)*(ZGROB(1)-UU)-(POGROB(1)*(ZGROB(2)-UU))) /        GRG  620
     1 (ZGROB(1)-ZGROB(2))                                              GRG  630
      RETURN                                                            GRG  640
      END                                                               GRG  650
      FUNCTION ZEHSL(CS,EPSG,DK,FF,TK0,XLKUG,XLGAS,PR)                  ZEH   10
C                                                                       ZEH   20
C     EFFEKTIVE WAERMELEITUNG NACH ZEHNER-SCHLUENDER (WA-EA12)          ZEH   30
C                                                                       ZEH   40
      AS = XLKUG / XLGAS                                                ZEH   50
C                                                                       ZEH   60
C     STRAHLUNGLSANTEIL                                                 ZEH   70
C                                                                       ZEH   80
      TK3 = TK0 / 100.                                                  ZEH   90
      TK3 = TK3**3.                                                     ZEH  100
      XLS = .04 * CS * DK / (2./EPSG-1.) * TK3                          ZEH  110
      AR = XLS / XLGAS                                                  ZEH  120
      DE = 1.D1 / 9.E0                                                  ZEH  130
      FE = FF / (1.E0-FF)                                               ZEH  140
      FE = FE**DE                                                       ZEH  150
      B = 1.25 * FE                                                     ZEH  160
      AN = 1. + (AR-B) / AS                                             ZEH  170
      SQFF = SQRT(FF)                                                   ZEH  180
      XLL = 2.E0 / AN * ((AS+AR-1.)*B/AS/AN/AN*ALOG((AS+AR)/B)-(B-1.)/  ZEH  190
     1 AN+(B+1.)/2./B*(AR-B)) * XLGAS                                   ZEH  200
C                                                                       ZEH  210
C     EFFEKTIVE LEITF. OHNE STROEMUNG                                   ZEH  220
C                                                                       ZEH  230
      ZEHSL = (1.D0-SQFF) * (XLGAS+(1.-FF)*XLS) + SQFF * XLL            ZEH  240
C                                                                       ZEH  250
      RETURN                                                            ZEH  260
      END                                                               ZEH  270
      FUNCTION ROBOLD(SIG,EPSG,DK,FF,TK,XLKUG,XLGAS,PR)                 ROB   10
C                                                                       ROB   20
      DATA PI/3.14159/                                                  ROB   30
C                                                                       ROB   40
C                                                                       ROB   50
      DP = .523 / FF * DK                                               ROB   60
      DFL = DP / (1.+PI)                                                ROB   70
      DSO = DP * PI / (1.+PI)                                           ROB   80
C                                                                       ROB   90
      B = BROB(EPSG,1.-FF)                                              ROB  100
C                                                                       ROB  110
      PSI = (2.*B+EPSG*(1.-B)) / (2.-EPSG) / (1.-B)                     ROB  120
      PSI0 = EPSG / (2.-EPSG)                                           ROB  130
      CHI = PSI0 / PSI                                                  ROB  140
C                                                                       ROB  150
      B0 = BROB(0,1.-FF)                                                ROB  160
C                                                                       ROB  170
      DELT0 = (PSI*(1.-B0)-B0) / PSI0 / (1.-B0)                         ROB  180
      TK3 = TK**3.                                                      ROB  190
C                                                                       ROB  200
      ROBOLD = 4. * PSI * SIG * TK3 * DP * (1.-CHI*DELT0/(1.+XLKUG/DSO/ ROB  210
     1 (4*PSI0*SIG*TK3+XLGAS/DFL))) + XLGAS * DP / DFL * (1.-DELT0/(1.+ ROB  220
     2 XLKUG/DSO/(4*PSI0*SIG*TK3+XLGAS/DFL)))                           ROB  230
C                                                                       ROB  240
      RETURN                                                            ROB  250
      END                                                               ROB  260
      FUNCTION BROB(E,P)                                                BRO   10
C                                                                       BRO   20
      REAL*4 ZG(6),PG(6,2),PH(2),B(2)                                   BRO   30
C                                                                       BRO   40
      DATA ZG/0.,.2,.4,.6,.8,1./,IJ/6/,IH/2/,PH/.26,.39/,PG/.0528,.0337,BRO   50
     1 .0246,.0206,.0179,.0152,.0849,.0595,.0476,.0397,.0357,.0325/     BRO   60
C                                                                       BRO   70
C                                                                       BRO   80
      B(1) = 1                                                          BRO   90
      B(2) = 1                                                          BRO  100
C                                                                       BRO  110
      IF(P .NE. PH(2)) CALL LAGRGW(IJ,ZG,PG(1,1),E,B(1))                BRO  120
C                                                                       BRO  130
      IF(P .NE. PH(1)) CALL LAGRGW(IJ,ZG,PG(1,2),E,B(2))                BRO  140
C                                                                       BRO  150
      CALL LAGRGW(IH,PH,B,P,B0)                                         BRO  160
C                                                                       BRO  170
      BROB = B0                                                         BRO  180
C                                                                       BRO  190
      RETURN                                                            BRO  200
      END                                                               BRO  210
      SUBROUTINE RVONT                                                  RVO   10
C                                                                       RVO   20
C     ITERIEREN DER RADIEN FUER KUGELN MIT BRENNSTOFFREIER INNENZONE    RVO   30
C                                                                       RVO   40
      COMMON /RVON/ RR2,RR5,R03,RR3                                     RVO   50
C                                                                       RVO   60
   10 FORMAT (50X,5('*'),' NO CONVERGENCY ',5('*'))                     RVO   70
C                                                                       RVO   80
C                                                                       RVO   90
      RKONV = 0.0001                                                    RVO  100
      R1 = RR5                                                          RVO  110
      DO 1 I=1,300                                                      RVO  120
        R12 = R1**2.                                                    RVO  130
        FR1 = RR2 - R12 - 2. * R03 / R1                                 RVO  140
        IF((ABS(FR1)/RR2) .LT. RKONV) GOTO 2                            RVO  150
        R1 = R1 + FR1 * 0.5 / (R1-R03/R12)                              RVO  160
    1 CONTINUE                                                          RVO  170
      WRITE (6,10)                                                      RVO  180
    2 RR3  = R1                                                         RVO  190
      RETURN                                                            RVO  200
      END                                                               RVO  210
      SUBROUTINE REGTEM(NRN,N200,NXS,VL,BUR,DOS,TEML,PF,TFU,TMO,SR,NDR, REG   10
     1 MIX,AGEFAC,VRP,VREG)                                             REG   20
C                                                                       REG   30
CFZJ042                                                       09.09.05  REG   40
C                                                                       REG   50
C     LIFE HISTORY PER REGION                                           REG   60
C                                                                       REG   70
      COMMON /ZEUG/ ITM3,IFZW,CRLSTG,IAX,IRA,MRZ,MERZ(5),RADI(50),      REG   80
     1 TQ(5,50,20),SD(60,50,30),AB(20),DENKGL,NRUN,R3,R4,PIE,HCOR,ISPALTREG   90
     2 ,JIC,BRENKU(50),IB(19),RR,KRUN,POWBAL,TR2,FADO,TT,TDR,WL0,WL2,   REG  100
     3 TDRR2,TINTE,WIFI,IBU,N6,TG,TM,TGAS,FRACKU,ISUBRG,DELDAY          REG  110
C                                                                       REG  120
      COMMON /BLOTIK/ N197,HCORE,NRAD,POWER,IX,JZ,ISP,NLIB,LAYER,DELZ,  REG  130
     1 TIN,TOUT,LAY(20),RAD(20),JR(20),RZ(2,50),IZ(2,50),Q(50,50,2),    REG  140
     2 A(3,25,25)                                                       REG  150
C                                                                       REG  160
      COMMON /SPECTI/ ITIK(10)                                          REG  170
C                                                                       REG  180
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      REG  190
C                                                                       REG  200
      COMMON /SPEZI/ FF                                                 REG  210
C                                                                       REG  220
      COMMON /KONTHX/ FALAST,NEU                                        REG  230
C                                                                       REG  240
CFZJ006 enlarged dimensions common QVAR                       28.11.03  REG  250
      COMMON /QVAR/ DUM(11),TE(4,300)                                   REG  260
C                                                                       REG  270
CFZJ042                                                       09.09.05  REG  280
CFZJ048 enlarged dimension                                    11.04.07  REG  290
      COMMON /VARDIT/ B(5000000)                                        REG  300
C                                                                       REG  310
CFZJ042                                                       09.09.05  REG  320
      COMMON /ADDRT/ KX(240),KY(240),LZ(240),NENDPT                     REG  330
C                                                                       REG  340
CFZJ055                                                       25.09.07  REG  350
C                                                                       REG  360
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             REG  370
C                                                                       REG  380
      COMMON /TRANS/ IFINST,INTVAL                                      REG  390
C                                                                       REG  400
      COMMON /STA/ IST,SB,TEX,JNS,RMI,RMA,TKI,TKA,TC                    REG  410
C                                                                       REG  420
CFZJ008 enlarged dimension common STAF                        28.11.03  REG  430
      COMMON /STAF/ TF,TFC(300)                                         REG  440
C                                                                       REG  450
CFZJ042                                                       09.09.05  REG  460
      COMMON /COUPL/ IPRINT                                             REG  470
C                                                                       REG  480
      COMMON /VRT/ MIXM                                                 REG  490
C                                                                       REG  500
CFZJ042                                                       09.09.05  REG  510
      DIMENSION VL(N200),BUR(N200),DOS(N200),TEML(3,N200),PF(15,N200),  REG  520
     1 TFU(NXS),TMO(NXS),SR(10,N200),MIX(NDR),AGEFAC(N200),VRP(NDR),    REG  530
     2 VREG(NDR)                                                        REG  540
C                                                                       REG  550
      EQUIVALENCE(LZ(18),ITK),(LZ(19),IRK)                              REG  560
C                                                                       REG  570
    1 FORMAT ('1',13X,'LIFE HISTORY PER REGION (',I2,'.BATCH)'/14X,34('=REG  580
     1')/' REGION      VOLUME       DOSIS     BURNUP   POWER      POWER'REG  590
     2 /'             (CM**3)      (NVT)     (MWD/T) KW/BALL  BATCH/REGIREG  600
     3ON'/)                                                             REG  610
    2 FORMAT (I4,4X,2E13.5,F8.0,F7.2,F10.2)                             REG  620
    3 FORMAT (/' AVERAGE VALUES (BATCH 1 - ',I2,'):')                   REG  630
    5 FORMAT (4X,I3,F13.0,F10.0,E14.5,8X,F4.2)                          REG  640
    6 FORMAT (14X,'GRAPHIT     FUEL     (CM**3)'/)                      REG  650
   10 FORMAT (/3X,'1 -',I3,' :  ',F7.0,F10.0,E14.5/)                    REG  660
   23 FORMAT (/'   REGION       T-CELSIUS IN        VOLUME     FRACTION REG  670
     1FUEL ELEMENTS')                                                   REG  680
C                                                                       REG  690
C                                                                       REG  700
      IF(NRN .GT. 1) GOTO 7                                             REG  710
CFZJ042                                                       09.09.05  REG  720
      DO 8 J=1,10                                                       REG  730
        DO 8 K=1,N200                                                   REG  740
          SR(J,K) = 0.                                                  REG  750
    8 CONTINUE                                                          REG  760
    7 CONTINUE                                                          REG  770
      DENKGL = FF                                                       REG  780
      R43 = R4**3.                                                      REG  790
      POKG = 4. * PI * R43 / 3.                                         REG  800
      IBS = 0                                                           REG  810
CFZJ042                                                       09.09.05  REG  820
      DO 9 L=1,LAYER                                                    REG  830
        IS = IBS + NRN                                                  REG  840
        IBS = IBS + MIX(L)                                              REG  850
        IF(NRN .GT. MIX(L)) GOTO 9                                      REG  860
        SR(1,L) = VL(IS)                                                REG  870
        SR(2,L) = DOS(IS)                                               REG  880
        SR(3,L) = BUR(IS)                                               REG  890
        SR(10,L) = 1.                                                   REG  900
        SR(4,L) = TEML(3,L) * PF(NRN,L) * POKG                          REG  910
        SR(5,L) = TEML(1,L)                                             REG  920
        SR(6,L) = TEML(1,L) + (TEML(2,L)-TEML(1,L)) * PF(NRN,L)         REG  930
    9 CONTINUE                                                          REG  940
CFZJ042                                                       09.09.05  REG  950
      IF(IPRINT .GE. -2 .AND. IFINST .EQ. 0) WRITE (N6,1) NRN           REG  960
      ZKGL = DENKGL * 3. / (4.*PI*R43)                                  REG  970
      IF(NRN .GT. 1) GOTO 14                                            REG  980
      JFIRST = 0                                                        REG  990
   14 CONTINUE                                                          REG 1000
      IBS = 0                                                           REG 1010
CFZJ042                                                       09.09.05  REG 1020
      DO 15 K=1,LAYER                                                   REG 1030
        IS = IBS + NRN                                                  REG 1040
        IBS = IBS + MIX(K)                                              REG 1050
        IF(NRN .GT. MIX(K)) GOTO 15                                     REG 1060
        IBU = 0                                                         REG 1070
        FADO = SR(2,K)                                                  REG 1080
        POWBAL = SR(4,K)                                                REG 1090
        TGAS = SR(5,K)                                                  REG 1100
        TR2 = SR(6,K)                                                   REG 1110
        FRACKU = SR(10,K)                                               REG 1120
        WIFI = SR(1,K) * ZKGL * FRACKU                                  REG 1130
C                                                                       REG 1140
        CALL TEMPK1(JFIRST,N200,B(KX(ITK)),B(KX(IRK)))                  REG 1150
C                                                                       REG 1160
        SR(4,K) = SR(4,K) / 1000.                                       REG 1170
        SR(7,K) = TT                                                    REG 1180
        SR(8,K) = TEML(1,K)                                             REG 1190
        SR(9,K) = TEML(2,K)                                             REG 1200
        IF(IPRINT .GE. -2 .AND. IFINST .EQ. 0) WRITE (N6,2) K,(SR(I,K), REG 1210
     1   I=1,4),AGEFAC(IS)                                              REG 1220
   15 CONTINUE                                                          REG 1230
CFZJ042                                                       09.09.05  REG 1240
      DO 17 K=1,LAYER                                                   REG 1250
        TMO(K) = SR(8,K)                                                REG 1260
        TFU(K) = SR(9,K)                                                REG 1270
   17 CONTINUE                                                          REG 1280
      IF(NRN .NE. MIXM) GOTO 18                                         REG 1290
CFZJ042                                                       09.09.05  REG 1300
      IF(INTVAL .GT. 1 .OR. IPRINT .LT. -2) GOTO 19                     REG 1310
      WRITE (N6,3) NRN                                                  REG 1320
      WRITE (N6,23)                                                     REG 1330
      WRITE (N6,6)                                                      REG 1340
   19 CONTINUE                                                          REG 1350
      TGV = 0.                                                          REG 1360
CFZJ042                                                       09.09.05  REG 1370
      VVM = 0.                                                          REG 1380
      VVF = 0.                                                          REG 1390
      TMV = 0.                                                          REG 1400
CFZJ042                                                       09.09.05  REG 1410
      DO 21 I=1,LAYER                                                   REG 1420
        FRACBE = VRP(I) / VREG(I)                                       REG 1430
        IF(FRACBE .LE. 0.) TFU(I) = 0.                                  REG 1440
        IF(INTVAL .LE. 1 .AND. IPRINT .GE. -2) WRITE(N6,5) I,TMO(I),    REG 1450
     1  TFU(I),VREG(I),FRACBE                                           REG 1460
        VVM = VVM + VREG(I)                                             REG 1470
        VVF = VVF + VRP(I)                                              REG 1480
        TGV = TGV + TMO(I) * VREG(I)                                    REG 1490
        TMV = TMV + TFU(I) * VRP(I)                                     REG 1500
   21 CONTINUE                                                          REG 1510
CFZJ042                                                       09.09.05  REG 1520
      TGV = TGV / VVM                                                   REG 1530
      TMV = TMV / VVF                                                   REG 1540
      IF(INTVAL .LE. 1 .AND. IPRINT .GE. -2) WRITE (N6,10) LAYER,TGV,TMVREG 1550
     1 ,VVM                                                             REG 1560
      IT1 = ITIK(1) + 1                                                 REG 1570
      IF(INTVAL .GT. 1) GOTO 30                                         REG 1580
      TC = TGV                                                          REG 1590
      TF = TMV                                                          REG 1600
   30 CONTINUE                                                          REG 1610
      TE(3,IT1) = TGV                                                   REG 1620
      TFC(IT1) = TMV                                                    REG 1630
   18 CONTINUE                                                          REG 1640
      RETURN                                                            REG 1650
      END                                                               REG 1660