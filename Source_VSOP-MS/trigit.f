      SUBROUTINE TRIGIT(MODE,MODE2,NLP,I3D)                             TRI   10
C                                                                       TRI   20
C     (3D-GEOMETRIE)                                                    TRI   30
C                                                                       TRI   40
C     BATCHES PRO LAYER > 1                                             TRI   50
C                                                                       TRI   60
CFZJ055                                                       25.09.07  TRI   70
C                                                                       TRI   80
      COMMON /TRIVSO/ NTRI(2),NTRI1(9999),NTRI2(9999),NTRI3(9999),      TRI   90
     1 NTRI4(9999),VTRI,VTRI1(9999),VTRI2(9999),VTRI3(9999),VTRI4(9999),TRI  100
     2 NTRI5(9999)                                                      TRI  110
C                                                                       TRI  120
      REAL VPART(9999),VOLREG(9999),VFV(9999),VFC(9999),DX(100),DY(100),TRI  130
     1 DZ(100),X(0:100),Y(0:100),Z(0:100)                               TRI  140
C                                                                       TRI  150
      INTEGER LAY3(100,100,100),NOPOW(9999),NRG(9999),XJV(9999),        TRI  160
     1 XIV(9999),MX(100),MY(100),MZ(100),MMX(9999)                      TRI  170
C                                                                       TRI  180
      CHARACTER*4 MODE,MODE2                                            TRI  190
C                                                                       TRI  200
   10 FORMAT (15I5)                                                     TRI  210
   20 FORMAT (6(I3,F9.3))                                               TRI  220
   30 FORMAT ('1'/' COARSE MESH IN X-Y-Z DIR.:',I4,' X',I4,' X',I4,' (='TRI  230
     1 ,I6,') WITH',I5,' COMPOSITIONS (IN CORE:',I5,')'/)               TRI  240
   40 FORMAT (///' MESH POINTS + DELTAS IN X-DIRECTION:'/)              TRI  250
  500 FORMAT (///' MESH POINTS + DELTAS IN PHI-DIRECTION:'/)            TRI  260
   50 FORMAT (10(I3,F9.4))                                              TRI  270
   60 FORMAT (///' MESH POINTS + DELTAS IN Y-DIRECTION:'/)              TRI  280
   70 FORMAT (///' MESH POINTS + DELTAS IN Z-DIRECTION:'/)              TRI  290
   80 FORMAT (////' COMPOSITION ID. NUMBERS  FOR ALL PLANES, ROWS AND COTRI  300
     1LUMNS:')                                                          TRI  310
   90 FORMAT (//' PLANE:',I4/)                                          TRI  320
  100 FORMAT (24I5)                                                     TRI  330
  110 FORMAT (////' CORE HEIGHT:',E12.5)                                TRI  340
  120 FORMAT (///' CORE VOLUME:',E12.5,' CCM',10X,'TOTAL VOLUME:',E12.5,TRI  350
     1 ' CCM')                                                          TRI  360
  130 FORMAT (////'   NO  VSOP-REGION CIT.COMP.  REGION/COMP.VOLUME  RELTRI  370
     1ATIVE PART OF VOLUME'/'    I    XIV(I)     XJV(I)           CCM   TRI  380
     2          VSOP       CITATION  '/)                                TRI  390
  140 FORMAT (I5,5X,I4,7X,I4,7X,E12.5,4X,E12.5,1X,E12.5)                TRI  400
  160 FORMAT (4I3,I5)                                                   TRI  410
  170 FORMAT ( /// ' INPUT-UNIT "geom" FOR CITATION GENERATED.')        TRI  420
  180 FORMAT (6(I3,F9.4))                                               TRI  430
  190 FORMAT (///' X-VALUES IN ASCENDING SEQUENCE:'/)                   TRI  440
  200 FORMAT (10E12.5)                                                  TRI  450
  210 FORMAT (///' Y-VALUES IN ASCENDING SEQUENCE:'/)                   TRI  460
  220 FORMAT (6E12.5)                                                   TRI  470
  221 FORMAT (E12.5,12X,E12.5)                                          TRI  480
  240 FORMAT (///' Z-VALUES IN ASCENDING SEQUENCE:'/)                   TRI  490
  250 FORMAT (/)                                                        TRI  500
  260 FORMAT (///'  REGIONS TOTAL:',I6,'     BATCHES TOTAL:',I6)        TRI  510
  270 FORMAT (///'  REGION    BATCHES IN REGION'/)                      TRI  520
  280 FORMAT (I6,5X,20I5)                                               TRI  530
  290 FORMAT ( ///' "TRIGIT": EXECUTION TIME:',F10.3,' MIN.')           TRI  540
  501 FORMAT (///' PHI-VALUES IN ASCENDING SEQUENCE:'/)                 TRI  550
  502 FORMAT (///' MESH POINTS + DELTAS IN R-DIRECTION:'/)              TRI  560
  503 FORMAT (///' R-VALUES IN ASCENDING SEQUENCE:'/)                   TRI  570
  504 FORMAT ('1'/' COARSE MESH IN PHI-R-Z-DIR.:',I4,' X',I4,' X',I4,' (TRI  580
     1=',I6,') WITH',I5,' COMPOSITIONS (IN CORE:',I5,')'/)              TRI  590
  505 FORMAT (1H )                                                      TRI  600
C                                                                       TRI  610
C                                                                       TRI  620
      CALL WATCH(ENDE)                                                  TRI  630
C                                                                       TRI  640
      A = ENDE                                                          TRI  650
      DO 111 I=0,100                                                    TRI  660
        X(I) = 0.                                                       TRI  670
        Y(I) = 0.                                                       TRI  680
        Z(I) = 0.                                                       TRI  690
  111 CONTINUE                                                          TRI  700
C                                                                       TRI  710
      CALL DATET                                                        TRI  720
C                                                                       TRI  730
CARD TR-1                                                               TRI  740
C                                                                       TRI  750
      READ (5,10) MYX,IPL,ICORE                                         TRI  760
C                                                                       TRI  770
      NGEOM = 37                                                        TRI  780
      IF(IPL .GT. 0) OPEN(77,FILE='trigplot')                           TRI  790
      IF(MODE .EQ. MODE2) OPEN(NGEOM,FILE='geom')                       TRI  800
      NA = 1                                                            TRI  810
    1 CONTINUE                                                          TRI  820
      NE = NA + 5                                                       TRI  830
C                                                                       TRI  840
CARD(S) TR-2                                                            TRI  850
C                                                                       TRI  860
      READ (5,20) (MX(I),DX(I),I=NA,NE)                                 TRI  870
C                                                                       TRI  880
      DO 2 I=NA,NE                                                      TRI  890
        IF(DX(I) .LE. 0.) GOTO 2                                        TRI  900
        IMX = I                                                         TRI  910
    2 CONTINUE                                                          TRI  920
      NA = NE + 1                                                       TRI  930
      IF(IMX .EQ. NE) GOTO 1                                            TRI  940
      IF(I3D .LT. 2) GOTO 49                                            TRI  950
      DO 53 I=1,IMX                                                     TRI  960
        DX(I) = DX(I) * 3.14159 / 180.                                  TRI  970
   53 CONTINUE                                                          TRI  980
   49 CONTINUE                                                          TRI  990
      X(0) = 0.                                                         TRI 1000
      DO 23 I=1,IMX                                                     TRI 1010
        X(I) = X(I-1) + DX(I)                                           TRI 1020
   23 CONTINUE                                                          TRI 1030
      NA = 1                                                            TRI 1040
    3 CONTINUE                                                          TRI 1050
      NE = NA + 5                                                       TRI 1060
C                                                                       TRI 1070
CARD(S) TR-3                                                            TRI 1080
C                                                                       TRI 1090
      READ (5,20) (MY(J),DY(J),J=NA,NE)                                 TRI 1100
C                                                                       TRI 1110
      DO 4 J=NA,NE                                                      TRI 1120
        IF(DY(J) .LE. 0.) GOTO 4                                        TRI 1130
        JMY = J                                                         TRI 1140
    4 CONTINUE                                                          TRI 1150
      NA = NE + 1                                                       TRI 1160
      IF(JMY .EQ. NE) GOTO 3                                            TRI 1170
      Y(0) = 0.                                                         TRI 1180
      DO 24 J=1,JMY                                                     TRI 1190
        Y(J) = Y(J-1) + DY(J)                                           TRI 1200
   24 CONTINUE                                                          TRI 1210
      NA = 1                                                            TRI 1220
      HCORE = 0.                                                        TRI 1230
    5 CONTINUE                                                          TRI 1240
      NE = NA + 5                                                       TRI 1250
C                                                                       TRI 1260
CARD(S) TR-4                                                            TRI 1270
C                                                                       TRI 1280
      READ (5,20) (MZ(K),DZ(K),K=NA,NE)                                 TRI 1290
C                                                                       TRI 1300
      DO 6 K=NA,NE                                                      TRI 1310
        IF(DZ(K) .LT. 0.) HCORE = HCORE + ABS(DZ(K))                    TRI 1320
        DZ(K) = ABS(DZ(K))                                              TRI 1330
        IF(DZ(K) .LE. 0.) GOTO 6                                        TRI 1340
        KMZ = K                                                         TRI 1350
    6 CONTINUE                                                          TRI 1360
      NA = NE + 1                                                       TRI 1370
      IF(KMZ .EQ. NE) GOTO 5                                            TRI 1380
      Z(0) = 0.                                                         TRI 1390
      DO 25 K=1,KMZ                                                     TRI 1400
        Z(K) = Z(K-1) + DZ(K)                                           TRI 1410
   25 CONTINUE                                                          TRI 1420
      N200 = 0                                                          TRI 1430
      NNUL = 0                                                          TRI 1440
      DO 7 K=1,KMZ                                                      TRI 1450
        DO 7 J=1,JMY                                                    TRI 1460
C                                                                       TRI 1470
CARD(S) TR-5                                                            TRI 1480
C                                                                       TRI 1490
          READ (5,10) (LAY3(I,J,K),I=1,IMX)                             TRI 1500
C                                                                       TRI 1510
          DO 7 I=1,IMX                                                  TRI 1520
            N200 = MAX0(N200,LAY3(I,J,K))                               TRI 1530
            IF(LAY3(I,J,K) .LE. 0) NNUL = 1                             TRI 1540
    7 CONTINUE                                                          TRI 1550
CFZJ055                                                       25.09.07  TRI 1560
      N200C = N200                                                      TRI 1570
      IF(NNUL .EQ. 0) GOTO 22                                           TRI 1580
      NLP = 0                                                           TRI 1590
      DO 19 K=1,KMZ                                                     TRI 1600
        IF(NLP .GT. 0) GOTO 17                                          TRI 1610
        DO 16 J=1,JMY                                                   TRI 1620
          DO 16 I=1,IMX                                                 TRI 1630
            NLP = MAX0(NLP,LAY3(I,J,K))                                 TRI 1640
   16   CONTINUE                                                        TRI 1650
        IF(NLP .EQ. 0) GOTO 19                                          TRI 1660
   17   CONTINUE                                                        TRI 1670
        DO 18 J=1,JMY                                                   TRI 1680
          DO 18 I=1,IMX                                                 TRI 1690
            IF(LAY3(I,J,K) .EQ. 0) LAY3(I,J,K) = LAY3(I,J,K-1) + NLP    TRI 1700
            N200C = MAX0(N200C,LAY3(I,J,K))                             TRI 1710
   18   CONTINUE                                                        TRI 1720
   19 CONTINUE                                                          TRI 1730
      DO 21 K=1,KMZ                                                     TRI 1740
        DO 21 J=1,JMY                                                   TRI 1750
          DO 21 I=1,IMX                                                 TRI 1760
            IF(LAY3(I,J,K) .LT. 0) LAY3(I,J,K) = IABS(LAY3(I,J,K)) +    TRI 1770
     1       N200C                                                      TRI 1780
            N200 = MAX0(N200,LAY3(I,J,K))                               TRI 1790
   21 CONTINUE                                                          TRI 1800
   22 CONTINUE                                                          TRI 1810
      IF(MYX .LE. 0) MYX = 1                                            TRI 1820
      VREL = 1. / MYX                                                   TRI 1830
      NDR = N200                                                        TRI 1840
      N200 = N200C * (MYX-1) + N200                                     TRI 1850
      IB = 0                                                            TRI 1860
      DO 8 I=1,NDR                                                      TRI 1870
        XIV(I) = I                                                      TRI 1880
        XJV(I) = I                                                      TRI 1890
        VFV(I) = 1.                                                     TRI 1900
        VFC(I) = 1.                                                     TRI 1910
        VOLREG(I) = 0.                                                  TRI 1920
        IF(I .GT. N200C) GOTO 35                                        TRI 1930
        MMX(I) = MYX                                                    TRI 1940
        DO 33 J=1,MYX                                                   TRI 1950
          IB = IB + 1                                                   TRI 1960
          VPART(IB) = VREL                                              TRI 1970
          NOPOW(IB) = 0                                                 TRI 1980
          NRG(IB) = I                                                   TRI 1990
   33   CONTINUE                                                        TRI 2000
        GOTO 8                                                          TRI 2010
   35   CONTINUE                                                        TRI 2020
        MMX(I) = 1                                                      TRI 2030
        IB = IB + 1                                                     TRI 2040
        VPART(IB) = 1.                                                  TRI 2050
        NOPOW(IB) = 1                                                   TRI 2060
        NRG(IB) = I                                                     TRI 2070
    8 CONTINUE                                                          TRI 2080
      VOLC = 0.                                                         TRI 2090
      VOLG = 0.                                                         TRI 2100
      DO 9 K=1,KMZ                                                      TRI 2110
        DO 9 J=1,JMY                                                    TRI 2120
          DO 9 I=1,IMX                                                  TRI 2130
            M = LAY3(I,J,K)                                             TRI 2140
            IF(I3D .EQ. 1) DDD = DX(I) * DY(J) * DZ(K)                  TRI 2150
            IF(I3D .EQ. 2) DDD = 0.5 * DX(I) * (Y(J)**2.-Y(J-1)**2.) *  TRI 2160
     1       DZ(K)                                                      TRI 2170
            VOLREG(M) = VOLREG(M) + DDD                                 TRI 2180
            VOLG = VOLG + DDD                                           TRI 2190
            IF(M .LE. N200C) VOLC = VOLC + DDD                          TRI 2200
    9 CONTINUE                                                          TRI 2210
      MXYZ = IMX * JMY * KMZ                                            TRI 2220
C                                                                       TRI 2230
C     OUTPUT                                                            TRI 2240
C                                                                       TRI 2250
      IF(I3D .EQ. 1) WRITE (6,30) IMX,JMY,KMZ,MXYZ,NDR,N200C            TRI 2260
      IF(I3D .EQ. 2) WRITE (6,504) IMX,JMY,KMZ,MXYZ,NDR,N200C           TRI 2270
      IF(I3D .EQ. 1) WRITE (6,40)                                       TRI 2280
      IF(I3D .EQ. 2) WRITE (6,500)                                      TRI 2290
      WRITE (6,50) (MX(I),DX(I),I=1,IMX)                                TRI 2300
      IF(I3D .EQ. 1) WRITE (6,190)                                      TRI 2310
      IF(I3D .EQ. 2) WRITE (6,501)                                      TRI 2320
      WRITE (6,200) (X(I),I=0,IMX)                                      TRI 2330
      IF(I3D .EQ. 1) WRITE (6,60)                                       TRI 2340
      IF(I3D .EQ. 2) WRITE (6,502)                                      TRI 2350
      WRITE (6,50) (MY(J),DY(J),J=1,JMY)                                TRI 2360
      IF(I3D .EQ. 1) WRITE (6,210)                                      TRI 2370
      IF(I3D .EQ. 2) WRITE (6,503)                                      TRI 2380
      WRITE (6,200) (Y(J),J=0,JMY)                                      TRI 2390
      WRITE (6,70)                                                      TRI 2400
      WRITE (6,50) (MZ(K),DZ(K),K=1,KMZ)                                TRI 2410
      WRITE (6,240)                                                     TRI 2420
      WRITE (6,200) (Z(K),K=0,KMZ)                                      TRI 2430
      WRITE (6,80)                                                      TRI 2440
      DO 11 K=1,KMZ                                                     TRI 2450
        WRITE (6,90) K                                                  TRI 2460
        IIX = IMX                                                       TRI 2470
        IF(IMX .GT. 24) IIX = 24                                        TRI 2480
        DO 61 J=1,JMY                                                   TRI 2490
          WRITE (6,100) (LAY3(I,J,K),I=1,IIX)                           TRI 2500
   61   CONTINUE                                                        TRI 2510
        IF(IIX .EQ. IMX) GOTO 11                                        TRI 2520
        IIX = IIX + 1                                                   TRI 2530
        GOTO 52                                                         TRI 2540
   51   CONTINUE                                                        TRI 2550
        IIX = MIX + 1                                                   TRI 2560
   52   CONTINUE                                                        TRI 2570
        MIX = IIX + 23                                                  TRI 2580
        WRITE (6,250)                                                   TRI 2590
        IF(IMX .LT. MIX) MIX = IMX                                      TRI 2600
        DO 62 J=1,JMY                                                   TRI 2610
          WRITE (6,100) (LAY3(I,J,K),I=IIX,MIX)                         TRI 2620
   62   CONTINUE                                                        TRI 2630
        IF(IMX .GT. MIX) GOTO 51                                        TRI 2640
   11 CONTINUE                                                          TRI 2650
      WRITE (6,260) NDR,IB                                              TRI 2660
      IF(IB .EQ. NDR) GOTO 48                                           TRI 2670
      WRITE (6,270)                                                     TRI 2680
      MXE = 0                                                           TRI 2690
      MXA = 1                                                           TRI 2700
      DO 38 K=1,NDR                                                     TRI 2710
        IF(K .GT. N200C) GOTO 37                                        TRI 2720
        MXE = MXA + MYX - 1                                             TRI 2730
        WRITE (6,280) K,(I,I=MXA,MXE)                                   TRI 2740
        MXA = MXE + 1                                                   TRI 2750
        GOTO 38                                                         TRI 2760
   37   CONTINUE                                                        TRI 2770
        MXE = MXE + 1                                                   TRI 2780
        WRITE (6,280) K,MXE                                             TRI 2790
   38 CONTINUE                                                          TRI 2800
   48 CONTINUE                                                          TRI 2810
      WRITE (6,110) HCORE                                               TRI 2820
      WRITE (6,120) VOLC,VOLG                                           TRI 2830
      WRITE (6,130)                                                     TRI 2840
      DO 12 I=1,NDR                                                     TRI 2850
        WRITE (6,140) I,XIV(I),XJV(I),VOLREG(I),VFV(I),VFC(I)           TRI 2860
   12 CONTINUE                                                          TRI 2870
C                                                                       TRI 2880
C     PLOTDATEN VON EBENE "IPL" / DARSTELLUNG (ICORE) WIE GERECHNET (=0)TRI 2890
C     TEILCORE AUF GANZCORE ERWEITERN (=2,4)                            TRI 2900
C                                                                       TRI 2910
      IF(I3D .EQ. 2) ICORE = 0                                          TRI 2920
      IF(IPL .LE. 0) GOTO 39                                            TRI 2930
      JJMY = JMY                                                        TRI 2940
      IIMX = IMX                                                        TRI 2950
      IF(ICORE .LE. 0) GOTO 47                                          TRI 2960
      ICORE = ICORE / 2                                                 TRI 2970
      GOTO(44,41),ICORE                                                 TRI 2980
   41 CONTINUE                                                          TRI 2990
      DO 42 J=1,JMY                                                     TRI 3000
        Y(J+JMY) = Y(J) + Y(JMY)                                        TRI 3010
        DO 42 I=1,IMX                                                   TRI 3020
          LAY3(I,J+JMY,IPL) = LAY3(I,J,IPL)                             TRI 3030
   42 CONTINUE                                                          TRI 3040
      DO 26 J=1,JMY-1                                                   TRI 3050
        Y(JMY-J) = Y(JMY+1-J) - DY(J)                                   TRI 3060
   26 CONTINUE                                                          TRI 3070
      JMD = JMY * 2 + 1                                                 TRI 3080
      DO 43 J=1,JMY                                                     TRI 3090
        DO 43 I=1,IMX                                                   TRI 3100
          LAY3(I,J,IPL) = LAY3(I,JMD-J,IPL)                             TRI 3110
   43 CONTINUE                                                          TRI 3120
      JJMY = JMD - 1                                                    TRI 3130
   44 CONTINUE                                                          TRI 3140
      DO 45 I=1,IMX                                                     TRI 3150
        X(I+IMX) = X(I) + X(IMX)                                        TRI 3160
        DO 45 J=1,JMY                                                   TRI 3170
          LAY3(I+IMX,J,IPL) = LAY3(I,J,IPL)                             TRI 3180
   45 CONTINUE                                                          TRI 3190
      DO 27 I=1,IMX-1                                                   TRI 3200
        X(IMX-I) = X(IMX+1-I) - DX(I)                                   TRI 3210
   27 CONTINUE                                                          TRI 3220
      IMD = IMX * 2 + 1                                                 TRI 3230
      DO 46 I=1,IMX                                                     TRI 3240
        DO 46 J=1,JMY                                                   TRI 3250
          LAY3(I,J,IPL) = LAY3(IMD-I,J,IPL)                             TRI 3260
   46 CONTINUE                                                          TRI 3270
      IIMX = IMD - 1                                                    TRI 3280
   47 CONTINUE                                                          TRI 3290
      IF(I3D .EQ. 1) GOTO 601                                           TRI 3300
      DO 400 I=0,IMX                                                    TRI 3310
        DO 800 J=0,JMY                                                  TRI 3320
          XX = Y(J) * COS(X(I))                                         TRI 3330
          YY = Y(J) * SIN(X(I))                                         TRI 3340
          WRITE (77,220) XX,YY                                          TRI 3350
  800   CONTINUE                                                        TRI 3360
        WRITE (77,505)                                                  TRI 3370
  400 CONTINUE                                                          TRI 3380
      DO 600 J=0,JMY                                                    TRI 3390
        DO 700 I=0,IMX-1                                                TRI 3400
          DELX = (X(I+1)-X(I)) / MX(I+1)                                TRI 3410
          DO 900 II=0,MX(I+1)                                           TRI 3420
            XX = Y(J) * COS(X(I)+II*DELX)                               TRI 3430
            YY = Y(J) * SIN(X(I)+II*DELX)                               TRI 3440
            WRITE (77,221) XX,YY                                        TRI 3450
  900     CONTINUE                                                      TRI 3460
  700   CONTINUE                                                        TRI 3470
        WRITE (77,505)                                                  TRI 3480
  600 CONTINUE                                                          TRI 3490
      GOTO 39                                                           TRI 3500
  601 CONTINUE                                                          TRI 3510
      DO 31 I=1,IIMX                                                    TRI 3520
        WRITE (77,220) X(I),X(I)                                        TRI 3530
        WRITE (77,220) Y(0),Y(JMY)                                      TRI 3540
   31 CONTINUE                                                          TRI 3550
      DO 32 J=1,JJMY                                                    TRI 3560
        WRITE (77,220) X(0),X(IMX)                                      TRI 3570
        WRITE (77,220) Y(J),Y(J)                                        TRI 3580
   32 CONTINUE                                                          TRI 3590
      IL = 0                                                            TRI 3600
      DO 34 J=1,JJMY                                                    TRI 3610
        DO 34 I=1,IIMX                                                  TRI 3620
          L = LAY3(I,J,IPL)                                             TRI 3630
          IF(L .EQ. LAY3(I+1,J,IPL)) GOTO 34                            TRI 3640
          WRITE (77,220) X(I),X(I)                                      TRI 3650
          WRITE (77,220) Y(J-1),Y(J)                                    TRI 3660
          IL = IL + 1                                                   TRI 3670
   34 CONTINUE                                                          TRI 3680
      DO 36 I=1,IIMX                                                    TRI 3690
        DO 36 J=1,JJMY                                                  TRI 3700
          L = LAY3(I,J,IPL)                                             TRI 3710
          IF(L .EQ. LAY3(I,J+1,IPL)) GOTO 36                            TRI 3720
          WRITE (77,220) X(I-1),X(I)                                    TRI 3730
          WRITE (77,220) Y(J),Y(J)                                      TRI 3740
          IL = IL + 1                                                   TRI 3750
   36 CONTINUE                                                          TRI 3760
   39 CONTINUE                                                          TRI 3770
C                                                                       TRI 3780
C     VSOP-VALUES                                                       TRI 3790
C                                                                       TRI 3800
      NTRI(1) = N200                                                    TRI 3810
      NTRI(2) = NDR                                                     TRI 3820
      VTRI = HCORE                                                      TRI 3830
      DO 300 J=1,N200                                                   TRI 3840
        VTRI1(J) = VPART(J)                                             TRI 3850
        NTRI1(J) = NRG(J)                                               TRI 3860
        NTRI2(J) = NOPOW(J)                                             TRI 3870
  300 CONTINUE                                                          TRI 3880
      DO 301 J=1,NDR                                                    TRI 3890
        VTRI2(J) = VOLREG(J)                                            TRI 3900
        NTRI5(J) = MMX(J)                                               TRI 3910
        NTRI3(J) = XJV(J)                                               TRI 3920
        NTRI4(J) = XIV(J)                                               TRI 3930
        VTRI3(J) = VFV(J)                                               TRI 3940
        VTRI4(J) = VFC(J)                                               TRI 3950
  301 CONTINUE                                                          TRI 3960
C                                                                       TRI 3970
C     CITATION-LIBRARY:                                                 TRI 3980
C     X-Y-Z - GEOMETRIEDATEN (SECTION 004)                              TRI 3990
C     COMPOSITION-BELEGUNG   (SECTION 005)                              TRI 4000
C                                                                       TRI 4010
      IOPT = 4                                                          TRI 4020
      WRITE (NGEOM,160) IOPT,IMX,JMY,KMZ,N200C                          TRI 4030
      WRITE (NGEOM,180) (MX(I),DX(I),I=1,IMX)                           TRI 4040
      WRITE (NGEOM,180) (MY(J),DY(J),J=1,JMY)                           TRI 4050
      WRITE (NGEOM,180) (MZ(K),DZ(K),K=1,KMZ)                           TRI 4060
      IOPT = 5                                                          TRI 4070
      WRITE (NGEOM,160) IOPT                                            TRI 4080
      DO 14 K=1,KMZ                                                     TRI 4090
        DO 14 J=1,JMY                                                   TRI 4100
          WRITE (NGEOM,10) (LAY3(I,J,K),I=1,IMX)                        TRI 4110
   14 CONTINUE                                                          TRI 4120
      WRITE (6,170)                                                     TRI 4130
      REWIND NGEOM                                                      TRI 4140
C                                                                       TRI 4150
      CALL WATCH(ENDE)                                                  TRI 4160
C                                                                       TRI 4170
      CPU = ENDE - A                                                    TRI 4180
      WRITE (6,290) CPU/60.                                             TRI 4190
      RETURN                                                            TRI 4200
      END                                                               TRI 4210
      SUBROUTINE DATET                                                  ATE   10
C                                                                       ATE   20
C     DATUMANGABE + TITELSEITE                                          ATE   30
C                                                                       ATE   40
      CHARACTER*2 DATG(4)                                               ATE   50
      CHARACTER*4 DATH                                                  ATE   60
      CHARACTER*8 DATF                                                  ATE   70
C                                                                       ATE   80
      EQUIVALENCE(DATG(1),DATF),(DATH,DATF)                             ATE   90
C                                                                       ATE  100
C                                                                       ATE  110
      CALL DATE_AND_TIME(DATF)                                          ATE  120
C                                                                       ATE  130
      READ (UNIT=DATH,FMT=101) JAHR                                     ATE  140
      READ (UNIT=DATG(3),FMT=100) MON                                   ATE  150
      READ (UNIT=DATG(4),FMT=100) ITAG                                  ATE  160
C                                                                       ATE  170
      WRITE (6,10)                                                      ATE  180
      WRITE (6,20)                                                      ATE  190
      WRITE (6,1003) ITAG,MON,JAHR                                      ATE  200
      WRITE (6,30)                                                      ATE  210
      JAHR = 0                                                          ATE  220
      ITAG = 0                                                          ATE  230
      RETURN                                                            ATE  240
C                                                                       ATE  250
   10 FORMAT ('1'///25X,13('*'),5X,11('*'),6X,'***',6X,10('*'),6X,'***',ATE  260
     1 5X,13('*')/25X,13('*'),5X,12('*'),5X,'***',5X,12('*'),5X,'***',5XATE  270
     2 ,13('*')/30X,'***',10X,'***',6X,'***',5X,'***',5X,'****',4X,'****ATE  280
     3',5X,'***',10X,'***'/30X,'***',10X,'***',7X,'**',5X,'***',5X,'***'ATE  290
     4 ,6X,'***',5X,'***',10X,'***'/30X,'***',10X,'***',6X,'***',5X,'***ATE  300
     5',5X,'***',14X,'***',10X,'***'/30X,'***',10X,11('*'),6X,'***',5X, ATE  310
     6 '***',14X,'***',10X,'***'/30X,'***',10X,11('*'),6X,'***',5X,'***'ATE  320
     7 ,2X,7('*'),5X,'***',10X,'***'/30X,'***',10X,'***',2X,'***',9X,'**ATE  330
     8*',5X,'***',2X,7('*'),5X,'***',10X,'***'/30X,'***',10X,'***',3X,'*ATE  340
     9**',8X,'***',5X,'***',6X,'***',5X,'***',10X,'***'/30X,'***',10X,'*ATE  350
     X**',4X,'***',7X,'***',5X,'****',4X,'****',5X,'***',10X,'***'/30X, ATE  360
     Y '***',10X,'***',5X,'***',6X,'***',5X,12('*'),5X,'***',10X,'***'/ ATE  370
     Z 30X,'***',10X,'***',6X,'***',5X,'***',6X,10('*'),6X,'***',10X,   ATE  380
     Z '***')                                                           ATE  390
   20 FORMAT (//////36X,'**',5X,'**',7X,7('*'),8X,7('*'),7X,8('*')/36X, ATE  400
     1 '**',5X,'**',3(6X,'**',5X,'**')/36X,'**',5X,'**',6X,'**',13X,'**'ATE  410
     2 ,5X,'**',6X,8('*')/37X,'**',3X,'**',8X,7('*'),7X,'**',5X,'**',6X,ATE  420
     3 '**'/38X,'**',1X,'**',4X,'**',2X,'*',6X,'**',2(2X,'**'),5X,'**', ATE  430
     4 2(2X,'**'),9X,'**'/39X,'***',5X,'**',3X,7('*'),3X,'**',3X,7('*'),ATE  440
     5 3X,'**',2X,'**',9X,'**')                                         ATE  450
   30 FORMAT (//////24X,'  JAN. 2012',52X,'REPORT: V.S.O.P.(99/11)'/95X,ATE  460
     1 'JUEL - 4348'/24X,'  JUNE 2010',52X,'REPORT: V.S.O.P.(99/09)'/95XATE  461
     2 ,'JUEL - 4326'/95X,'SECTION 4.2.2')                              ATE  470
  100 FORMAT (4I2)                                                      ATE  480
  101 FORMAT (I4)                                                       ATE  490
 1003 FORMAT (/////58X,'( ',I2,'.',I2,'.',I4,' )')                      ATE  500
      END                                                               ATE  510