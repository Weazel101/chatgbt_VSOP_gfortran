      SUBROUTINE BIRGIT(MODE,MODE2,ITTT)                                BIR   10
C                                                                       BIR   20
C     GITTERAUSLEGUNG VSOP/CITATION BZW. THERMIX + FLIESSKURVEN IM CORE BIR   30
C                                                                       BIR   40
C     (EIN STETER DANK AN FRL. BIRGIT GESKES)                           BIR   50
C                                                                       BIR   60
C                                                                       BIR   70
C     ERWEITERUNG: MAX. LAYER PRO KANAL              =  101             BIR   80
C                  MAX. VSOP-LAYER                   = 1515             BIR   90
C                  MAX. CITATION-COMP.               = 2000             BIR  100
C                  MAX. DELTAS GROB, RADIAL + RADIEN =  100             BIR  110
C                  MAX. DELTAS GROB, AXIAL + HOEHEN  =  200             BIR  120
C                  MAX. CORE-DELTAS GROB, RADIAL     =   50             BIR  130
C                  MAX. CORE-DELTAS GROB, AXIAL      =  100             BIR  140
C         ---->    ECHTER INNENKONUS (WIE AUSSEN)                       BIR  150
C         ---->    REFLEKTORBEREICHE (-1) AUCH INNEN                    BIR  160
C         >>>>>    LAYER-VOLUMINA EINES KANALS VARIIEREN (PER INPUT)    BIR  170
C                                                                       BIR  180
CFZJ048 enlarged dimension                                    11.04.07  BIR  190
      REAL XWE(0:15,15),YWE(0:15,15),VOLU(15),RAD(20),VEKA(0:15),       BIR  200
     1 RC(0:100),ZC(0:200),GRZKOT(25000),GRRKOT(10000),WEKA(0:15),      BIR  210
     2 VPART(2000),VOLREG(1515),YYY(1515),DR(100),DZ(200),VFV(4000),    BIR  220
     3 VFC(4000),VFT(4000),ZPL(0:100),DGR(50),DGZ(100),DELR(100),       BIR  230
     4 DELZ(200)                                                        BIR  240
C                                                                       BIR  250
CFZJ048 enlarged dimension                                    11.04.07  BIR  260
      INTEGER LAYER(20),ICOPL(20),NOPOW(2000),NRG(2000),KLAY(1515),     BIR  270
     1 KANDEF(20),XJV(4000),XIV(4000),IOP(100),NOP(200),KANTYP(15),     BIR  280
     2 MIX(1515),NOPOWR(1515),NOPOWK(4000),MPR(100),MPZ(200)            BIR  290
C                                                                       BIR  300
      COMMON /FORALB/ UU,VV,XWE,YWE,IJR(0:15),N5,N6,N7,IJ,N8            BIR  310
C                                                                       BIR  320
      COMMON /CORB/ HCORE,KAN(15),NKURVE,H4,R1,H3,R2,KANAL              BIR  330
C                                                                       BIR  340
      COMMON /GEOB/ MR(100),MZ(200),LAYVC(100,200),MGR(50),MGZ(100),    BIR  350
     1 ICOR(100),LAYC(100,200),JCOR(15),JI,JA,JI0,RKONUS,ZKONUS,RC,ZC,  BIR  360
     2 NCOR1,NCOR2,IMA,RKONI,ZKONI                                      BIR  370
C                                                                       BIR  380
      COMMON /VOLUB/ VGE,VOLU,VEKA,XPRUEF,IX,WEKA,KANDEF                BIR  390
C                                                                       BIR  400
      COMMON /KONSB/ PI,EPSY,YYY,IWRITE,ZPL,MDIREC,IPUT                 BIR  410
C                                                                       BIR  420
      COMMON /VSOB/ KLAY,IPL,NVSOP,ICOPL,VOLREG,N20,KSCH,IMX,XJV,XIV,VFVBIR  430
     1 ,VFC,KROT                                                        BIR  440
C                                                                       BIR  450
      COMMON /MAXIB/ IMAX,NMAX,DR,DZ,NGEOM                              BIR  460
C                                                                       BIR  470
      COMMON /TERMIB/ NTHX,VFT                                          BIR  480
C                                                                       BIR  490
      COMMON /IBEB/ IRL,IRR,ITO,IBO                                     BIR  500
C                                                                       BIR  510
      COMMON /NEB/ IOP,NOP,KANTYP,NCASE,RINTHX,DGR,DGZ,KONUS,KANALC,    BIR  520
     1 NMAXC,CIZET0                                                     BIR  530
C                                                                       BIR  540
      COMMON /DB/ DELLAY(15),DLAY(15,101)                               BIR  550
C                                                                       BIR  560
      COMMON /NOPO/ NOPOWR,NOPOWK                                       BIR  570
C                                                                       BIR  580
      COMMON /BIRVSO/ NBIR(4),NBIR1(20),VBIR(2000),NBIR2(2000),         BIR  590
     1 NBIR3(2000),VBIR1(1515),NBIR4,NBIR5(2000),NBIR6(2000),VBIR2(2000)BIR  600
     2 ,VBIR3(2000),VBIR4,NBIR7,VBIR5(20),VBIR6(2),NBIR8,VBIR7(101),    BIR  610
     3 NBIR9(1515),NBIR10(2000)                                         BIR  620
C                                                                       BIR  630
      COMMON /GRID/ DELR,DELZ,MPR,MPZ,ITOT,NTOT,N1,N2                   BIR  640
C                                                                       BIR  650
      CHARACTER*4 MODE,MODE2                                            BIR  660
C                                                                       BIR  670
   49 FORMAT (///' INPUT-UNIT "geom" FOR CITATION GENERATED')           BIR  680
   59 FORMAT (///' INPUT-UNIT "geom" FOR THERMIX GENERATED.')           BIR  690
  100 FORMAT (1X,' TOTAL-INTEGRAL CORE    : ',E12.5)                    BIR  700
  221 FORMAT (//)                                                       BIR  710
  222 FORMAT (1X,'INTEGRAL ON CHANNEL',I3,'  : ',E12.5,'  PROPORTION OF BIR  720
     1VOLUME = ',E12.5)                                                 BIR  730
  333 FORMAT (///1X,'VOLUME OF CORE AMOUNTS TO ',E14.7,' M**3  (DIRECT CBIR  740
     1ALCULATION FROM GEOMETRY DATA)'/)                                 BIR  750
  900 FORMAT (24I3)                                                     BIR  760
  901 FORMAT (6(I3,F9.3))                                               BIR  770
  903 FORMAT (//'       NEW COARSE MESH POINTS FOR CHANNEL-CURVE',I3,':'BIR  780
     1 )                                                                BIR  790
  904 FORMAT (/' ORD.:',10E12.5/ 6X,5E12.5)                             BIR  800
  905 FORMAT (15I5)                                                     BIR  810
  906 FORMAT (6(F9.3,I3))                                               BIR  820
  907 FORMAT (3E12.5,I5)                                                BIR  830
  908 FORMAT (3(2I6,E12.5))                                             BIR  840
  909 FORMAT (6(F8.3,I3,I1))                                            BIR  850
  999 FORMAT (///' "BIRGIT"-EXECUTION TIME:', F10.3,' MINUTES')         BIR  860
C                                                                       BIR  870
C                                                                       BIR  880
      CALL WATCH(ENDE)                                                  BIR  890
C                                                                       BIR  900
      A = ENDE                                                          BIR  910
      PI = 3.14159                                                      BIR  920
      N5 = 5                                                            BIR  930
      N6 = 6                                                            BIR  940
      LC = 0                                                            BIR  950
      NTHX = 0                                                          BIR  960
      NVSOP = 25                                                        BIR  970
      NGEOM = 37                                                        BIR  980
      IWRITE = 0                                                        BIR  990
      MDIREC = 0                                                        BIR 1000
      DO 8 I=1,15                                                       BIR 1010
        DELLAY(I) = 0.                                                  BIR 1020
        DO 8 J=1,101                                                    BIR 1030
          DLAY(I,J) = 0.                                                BIR 1040
    8 CONTINUE                                                          BIR 1050
C                                                                       BIR 1060
      CALL DATEB                                                        BIR 1070
C                                                                       BIR 1080
C     INPUT + GITTER-ERSTELLUNG FUER CITATION / VSOP + ZUGEHOERIGE KOM- BIR 1090
C     POSITIONSBELEGUNGEN                                               BIR 1100
C                                                                       BIR 1110
    1 CONTINUE                                                          BIR 1120
C                                                                       BIR 1130
      CALL GITTER(LC)                                                   BIR 1140
C                                                                       BIR 1150
      IF(N8 .GT. 0) OPEN(UNIT=N8,FILE='birgexcl')                       BIR 1160
      IF(MODE .EQ. MODE2) OPEN(37,FILE='geom')                          BIR 1170
C                                                                       BIR 1180
C     BERECHNUNG DES GESAMTVOLUMENS MIT R**2 UND PI                     BIR 1190
C                                                                       BIR 1200
      V4 = 0.                                                           BIR 1210
      IF(KONUS .NE. 2) GOTO 7                                           BIR 1220
      R0 = RC(JI-1)                                                     BIR 1230
      R02 = R0 + RKONUS                                                 BIR 1240
      H02 = ZKONUS                                                      BIR 1250
      H03 = R0 * H02 / (R02-R0)                                         BIR 1260
      V4 = R02**2. / 3. * (H02+H03) - R0**2. / 3. * H03 - R0**2. * H02  BIR 1270
      V4 = V4 * PI                                                      BIR 1280
    7 CONTINUE                                                          BIR 1290
      H3 = 0.                                                           BIR 1300
      H2 = ZKONUS                                                       BIR 1310
      HCORE = ZC(NCOR2) - ZC(NCOR1-1)                                   BIR 1320
      R1 = RC(JA)                                                       BIR 1330
      R2 = R1 - RKONUS                                                  BIR 1340
      V0 = RC(JI-1)**2 * HCORE * PI + V4                                BIR 1350
      V00 = RC(JI0-1)**2 * HCORE * PI                                   BIR 1360
      V1 = R1**2 * (HCORE-H2) * PI                                      BIR 1370
      IF(RKONUS .NE. 0.) H3= (H2*R2) / (R1-R2)                          BIR 1380
      V2 = R1**2 / 3. * (H2+H3) * PI                                    BIR 1390
      V3 = R2**2 / 3. * H3 * PI                                         BIR 1400
      H4 = HCORE - H2                                                   BIR 1410
      VGE = (V1+V2-V3) - V0                                             BIR 1420
      VGE1 = VGE / 1000000.                                             BIR 1430
C                                                                       BIR 1440
C     VGE1 : DAS COREVOLUMEN IN M**3                                    BIR 1450
C                                                                       BIR 1460
      WRITE (N6,333) VGE1                                               BIR 1470
C                                                                       BIR 1480
C     VORBESETZEN VON HILFSGROESSEN                                     BIR 1490
C     XPRUEF: (IN SUB) ZUR UEBERPRUEFUNG DER BERECHNETEN INTEGRALE      BIR 1500
C     IX    : (IN SUB) ZAEHLER FUER DIE INTEGRALE                       BIR 1510
C     HILF  : (IN BIRGIT) ENTHAELT DIE AUFADDIERTEN INTEGRALE           BIR 1520
C                                                                       BIR 1530
      HILF = V00                                                        BIR 1540
      XPRUEF = 1.                                                       BIR 1550
C                                                                       BIR 1560
C     UEBERGABE AN SUBROUTINE NUMINTB BZW. LAGRASB                      BIR 1570
C                                                                       BIR 1580
      DO 200 IANZ=1,KANAL                                               BIR 1590
        IJ = IJR(IANZ)                                                  BIR 1600
        IX = IANZ                                                       BIR 1610
C                                                                       BIR 1620
        CALL KURVE(IANZ,V0)                                             BIR 1630
C                                                                       BIR 1640
        IF(KANDEF(IANZ) .NE. 0) GOTO 200                                BIR 1650
        WRITE (N6,903) IANZ                                             BIR 1660
        J = IJR(IANZ)                                                   BIR 1670
        WRITE (N6,904) (YWE(IANZ,I),I=1,J)                              BIR 1680
  200 CONTINUE                                                          BIR 1690
C                                                                       BIR 1700
      IF(N7 .GT. 0 .OR. N8 .GT. 0) CALL PLOTB                           BIR 1710
C                                                                       BIR 1720
      CV = 0.                                                           BIR 1730
      WRITE (N6,221)                                                    BIR 1740
      DO 231 IANZ=1,KANAL                                               BIR 1750
        VOLU(IANZ) = VOLU(IANZ) - HILF                                  BIR 1760
        K = KANDEF(IANZ)                                                BIR 1770
        IF(K .EQ. 1) WEKA(IANZ) = VOLU(IANZ) / VGE                      BIR 1780
        IF(K .EQ. 2 .OR. K .EQ. 3) WEKA(IANZ) = 1. - CV / VGE           BIR 1790
        IF(WEKA(IANZ) .GT. 0.) CV = CV + VOLU(IANZ)                     BIR 1800
        WRITE (N6,222) IANZ,VOLU(IANZ),WEKA(IANZ)                       BIR 1810
        HILF = HILF + VOLU(IANZ)                                        BIR 1820
  231 CONTINUE                                                          BIR 1830
      WRITE (N6,100) CV                                                 BIR 1840
C                                                                       BIR 1850
C     AUFRUF DER SUBROUTINE VOLT ZUR BERECHNUNG DER TEILVOLUMINA DER    BIR 1860
C     EINZELNEN KANAELE                                                 BIR 1870
C                                                                       BIR 1880
      CALL VOLT                                                         BIR 1890
C                                                                       BIR 1900
C     AUFRUF DER SUBROUTINE ROTGIT, DIE DAS GROBE GITTER ERSTELLT       BIR 1910
C                                                                       BIR 1920
      CALL ROTGIT                                                       BIR 1930
C                                                                       BIR 1940
C     AUFRUF DER SUBROUTINE GRUGIT, DIE DAS FEINE GITTER ERSTELLT       BIR 1950
C                                                                       BIR 1960
      CALL GRUGIT(GRZKOT,GRRKOT,LC)                                     BIR 1970
C                                                                       BIR 1980
      IF(NVSOP .EQ. 0) GOTO 40                                          BIR 1990
C                                                                       BIR 2000
C     ERSTELLEN DER GROESSEN FUER VSOP                                  BIR 2010
C                                                                       BIR 2020
      IA = 0                                                            BIR 2030
      IC = 0                                                            BIR 2040
      IL = 0                                                            BIR 2050
C                                                                       BIR 2060
C     ALLE CORE-KANAELE                                                 BIR 2070
C                                                                       BIR 2080
      DO 30 I=1,KANAL                                                   BIR 2090
        IF(WEKA(I) .LE. 0.) GOTO 30                                     BIR 2100
        IL = IL + 1                                                     BIR 2110
        LAYER(IL) = KAN(I)                                              BIR 2120
        DO 20 J=1,KAN(I)                                                BIR 2130
          IA = IA + 1                                                   BIR 2140
          MIX(IA) = ICOPL(I)                                            BIR 2150
          VOLREG(IA) = YYY(IA)                                          BIR 2160
          DO 10 K=1,ICOPL(I)                                            BIR 2170
            IC = IC + 1                                                 BIR 2180
            NOPOW(IC) = 0                                               BIR 2190
            NRG(IC) = IA                                                BIR 2200
            VPART(IC) = 1. / REAL(ICOPL(I))                             BIR 2210
   10     CONTINUE                                                      BIR 2220
   20   CONTINUE                                                        BIR 2230
   30 CONTINUE                                                          BIR 2240
C                                                                       BIR 2250
C     ALLE ZUM INNEREN BEREICH GEHOERIGEN REFLEKTOR-KANAELE             BIR 2260
C                                                                       BIR 2270
      DO 31 I=1,KANAL                                                   BIR 2280
        IF(WEKA(I) .GT. 0.) GOTO 31                                     BIR 2290
        IL = IL + 1                                                     BIR 2300
        LAYER(IL) = KAN(I)                                              BIR 2310
        DO 21 J=1,KAN(I)                                                BIR 2320
          IA = IA + 1                                                   BIR 2330
          MIX(IA) = ICOPL(I)                                            BIR 2340
          VOLREG(IA) = YYY(IA)                                          BIR 2350
          IC = IC + 1                                                   BIR 2360
          NOPOW(IC) = 2                                                 BIR 2370
          NRG(IC) = IA                                                  BIR 2380
          VPART(IC) = 1. / REAL(ICOPL(I))                               BIR 2390
   21   CONTINUE                                                        BIR 2400
   31 CONTINUE                                                          BIR 2410
C                                                                       BIR 2420
C     ZUSAMMENFASSUNG DES AEUSSEREN BEREICHES ZU EINEM LAYER            BIR 2430
C                                                                       BIR 2440
      LAYER(IL+1) = KSCH - IA                                           BIR 2450
      DO 32 I=IA+1,KSCH                                                 BIR 2460
        VOLREG(I) = YYY(I)                                              BIR 2470
        IC = IC + 1                                                     BIR 2480
        NOPOW(IC) = 1                                                   BIR 2490
        MIX(I) = 1                                                      BIR 2500
        NRG(IC) = I                                                     BIR 2510
        VPART(IC) = 1.                                                  BIR 2520
   32 CONTINUE                                                          BIR 2530
      N200 = IC                                                         BIR 2540
      NDR = KSCH                                                        BIR 2550
C                                                                       BIR 2560
C     RADIEN FUER V.S.O.P.                                              BIR 2570
C                                                                       BIR 2580
      JIN = 0                                                           BIR 2590
      IF(RKONI .GT. 0.) JIN = 1                                         BIR 2600
      NRAD = KANAL - JIN                                                BIR 2610
      DO 2 J=1,NRAD                                                     BIR 2620
        RAD(J) = YWE(J+JIN,1)                                           BIR 2630
    2 CONTINUE                                                          BIR 2640
      RI = YWE(JIN,1)                                                   BIR 2650
      RA = YWE(KANAL,1)                                                 BIR 2660
      JC = 0                                                            BIR 2670
    3 CONTINUE                                                          BIR 2680
      IF(JC .GE. IMAX) GOTO 6                                           BIR 2690
      JC = JC + 1                                                       BIR 2700
      IF(RC(JC) .LE. RA) GOTO 3                                         BIR 2710
      NRAD = NRAD + 1                                                   BIR 2720
      RAD(NRAD) = RC(JC)                                                BIR 2730
      GOTO 3                                                            BIR 2740
    6 CONTINUE                                                          BIR 2750
      NBIR(1) = N200                                                    BIR 2760
      NBIR(2) = N20                                                     BIR 2770
      NBIR(3) = NDR                                                     BIR 2780
      NBIR(4) = KROT                                                    BIR 2790
      DO 1000 I=1,N20                                                   BIR 2800
        NBIR1(I) = LAYER(I)                                             BIR 2810
 1000 CONTINUE                                                          BIR 2820
      DO 1001 I=1,N200                                                  BIR 2830
        VBIR(I) = VPART(I)                                              BIR 2840
        NBIR2(I) = NRG(I)                                               BIR 2850
        NBIR3(I) = NOPOW(I)                                             BIR 2860
 1001 CONTINUE                                                          BIR 2870
      DO 1002 I=1,NDR                                                   BIR 2880
        NBIR9(I) = MIX(I)                                               BIR 2890
        VBIR1(I) = VOLREG(I)                                            BIR 2900
 1002 CONTINUE                                                          BIR 2910
      DO 1006 I=1,KROT                                                  BIR 2920
        NBIR10(I) = NOPOWK(I)                                           BIR 2930
 1006 CONTINUE                                                          BIR 2940
      NBIR4 = IMX                                                       BIR 2950
      DO 1003 I=1,IMX                                                   BIR 2960
        NBIR5(I) = XJV(I)                                               BIR 2970
        NBIR6(I) = XIV(I)                                               BIR 2980
        VBIR2(I) = VFV(I)                                               BIR 2990
        VBIR3(I) = VFC(I)                                               BIR 3000
 1003 CONTINUE                                                          BIR 3010
      VBIR4 = HCORE                                                     BIR 3020
      NBIR7 = NRAD                                                      BIR 3030
      DO 1004 I=1,NRAD                                                  BIR 3040
        VBIR5(I) = RAD(I)                                               BIR 3050
 1004 CONTINUE                                                          BIR 3060
      VBIR6(1) = RI                                                     BIR 3070
      VBIR6(2) = CIZET0                                                 BIR 3080
      NBIR8 = NMAXC                                                     BIR 3090
      DO 1005 I=1,NMAXC                                                 BIR 3100
        VBIR7(I) = DLAY(1,I)                                            BIR 3110
 1005 CONTINUE                                                          BIR 3120
   40 CONTINUE                                                          BIR 3130
      IF(NTHX .GT. 0) GOTO 50                                           BIR 3140
C                                                                       BIR 3150
C     ERSTELLEN DER LIBRARY FUER CITATION                               BIR 3160
C     RADIALE + AXIALE GEOMETRIEDATEN (SECTION 004)                     BIR 3170
C                                                                       BIR 3180
      IOPT = 4                                                          BIR 3190
      IM1 = IMAX + 1                                                    BIR 3200
      WRITE (NGEOM,900) IOPT,IM1,NMAX                                   BIR 3210
      WRITE (NGEOM,901) (MR(I),DR(I),I=1,IMAX)                          BIR 3220
      WRITE (NGEOM,901) (MZ(N),DZ(N),N=1,NMAX)                          BIR 3230
C                                                                       BIR 3240
C     CITATION - LAYER-NUMMERN (SECTION 005)                            BIR 3250
C                                                                       BIR 3260
      IOPT = 5                                                          BIR 3270
      WRITE (NGEOM,900) IOPT,IMAX,NMAX                                  BIR 3280
      DO 902 N=1,NMAX                                                   BIR 3290
        WRITE (NGEOM,905) (LAYC(I,N),I=1,IMAX)                          BIR 3300
  902 CONTINUE                                                          BIR 3310
C                                                                       BIR 3320
C     MESH-POINTS AT CORE-REFLECTOR EDGES                               BIR 3330
C                                                                       BIR 3340
      WRITE (NGEOM,900) IRL,IRR,ITO,IBO                                 BIR 3350
      WRITE (N6,49)                                                     BIR 3360
   50 CONTINUE                                                          BIR 3370
      IF(ITTT .LE. 0) NCASE = 2                                         BIR 3380
      IF(NTHX .EQ. 0) GOTO 60                                           BIR 3390
C                                                                       BIR 3400
C     ERSTELLEN DER LIBRARY FUER THERMIX                                BIR 3410
C                                                                       BIR 3420
      WRITE (NGEOM,905) ITOT,NTOT,N1,NMAX,N2                            BIR 3430
      WRITE (NGEOM,906) (DELR(I),MPR(I),I=1,ITOT)                       BIR 3440
      WRITE (NGEOM,909) (DELZ(N),MPZ(N),NOP(N),N=1,NTOT)                BIR 3450
      WRITE (NGEOM,907) RC(JI-1),RC(JA),HCORE,IMX                       BIR 3460
      WRITE (NGEOM,908) (XJV(I),XIV(I),VFT(I),I=1,IMX)                  BIR 3470
      WRITE (N6,59)                                                     BIR 3480
      NTHX = 0                                                          BIR 3490
      NCASE = 0                                                         BIR 3500
   60 CONTINUE                                                          BIR 3510
      NVSOP = 0                                                         BIR 3520
      N7 = 0                                                            BIR 3530
      N8 = 0                                                            BIR 3540
      IF(NCASE .EQ. 2) NTHX = 43                                        BIR 3550
      IF(NTHX .GT. 0) GOTO 1                                            BIR 3560
C                                                                       BIR 3570
      CALL WATCH(ENDE)                                                  BIR 3580
C                                                                       BIR 3590
      CPU = ENDE - A                                                    BIR 3600
      WRITE (N6,999) CPU/60.                                            BIR 3610
      REWIND NGEOM                                                      BIR 3620
      RETURN                                                            BIR 3630
      END                                                               BIR 3640
      SUBROUTINE DATEB                                                  DAT   10
C                                                                       DAT   20
C     DATE + HEAD TITLE                                                 DAT   30
C                                                                       DAT   40
      CHARACTER*2 DATG(4)                                               DAT   50
      CHARACTER*4 DATH                                                  DAT   60
      CHARACTER*8 DATF                                                  DAT   70
C                                                                       DAT   80
      EQUIVALENCE(DATG(1),DATF),(DATH,DATF)                             DAT   90
C                                                                       DAT  100
      CALL DATE_AND_TIME(DATF)                                          DAT  110
C                                                                       DAT  120
      READ (UNIT=DATH,FMT=101) JAHR                                     DAT  130
      READ (UNIT=DATG(3),FMT=100) MON                                   DAT  140
      READ (UNIT=DATG(4),FMT=100) ITAG                                  DAT  150
      WRITE (6,10)                                                      DAT  160
      WRITE (6,20)                                                      DAT  170
      WRITE (6,1003) ITAG,MON,JAHR                                      DAT  180
      WRITE (6,30)                                                      DAT  190
      WRITE (6,40)                                                      DAT  200
      RETURN                                                            DAT  210
C                                                                       DAT  220
   10 FORMAT ('1'///25X,11('*'),6X,'***',5X,11('*'),7X,10('*'),6X,'***',DAT  230
     1 5X,13('*')/25X,12('*'),5X,'***',5X,12('*'),5X,12('*'),5X,'***',5XDAT  240
     2 ,13('*')/25X,'***',6X,'***',5X,'***',5X,'***',6X,'***',5X,'****',DAT  250
     3 4X,'****',5X,'***',10X,'***'/25X,'***',7X,'**',5X,'***',5X,'***',DAT  260
     4 7X,'**',5X,'***',6X,'***',5X,'***',10X,'***'/25X,'***',6X,'***', DAT  270
     5 5X,'***',5X,'***',6X,'***',5X,'***',14X,'***',10X,'***'/25X,     DAT  280
     6 11('*'),6X,'***',5X,11('*'),6X,'***',14X,'***',10X,'***'/25X,    DAT  290
     7 11('*'),6X,'***',5X,11('*'),6X,'***',2X,7('*'),5X,'***',10X,'***'DAT  300
     8 /25X,'***',6X,'***',5X,'***',5X,'***',2X,'***',9X,'***',2X,7('*')DAT  310
     9 ,5X,'***',10X,'***'/25X,'***',7X,'**',5X,'***',5X,'***',3X,'***',DAT  320
     X 8X,'***',6X,'***',5X,'***',10X,'***'/25X,'***',6X,'***',5X,'***',DAT  330
     Y 5X,'***',4X,'***',7X,'****',4X,'****',5X,'***',10X,'***'/25X,    DAT  340
     Z 12('*'),5X,'***',5X,'***',5X,'***',6X,12('*'),5X,'***',10X,'***'/DAT  350
     Z 25X,11('*'),6X,'***',5X,'***',6X,'***',6X,10('*'),6X,'***',10X,'*DAT  360
     Z**')                                                              DAT  370
   20 FORMAT (//////36X,'**',5X,'**',7X,7('*'),8X,7('*'),7X,8('*')/36X, DAT  380
     1 '**',5X,'**',3(6X,'**',5X,'**')/36X,'**',5X,'**',6X,'**',13X,'**'DAT  390
     2 ,5X,'**',6X,8('*')/37X,'**',3X,'**',8X,7('*'),7X,'**',5X,'**',6X,DAT  400
     3 '**'/38X,'**',1X,'**',4X,'**',2X,'*',6X,'**',2(2X,'**'),5X,'**', DAT  410
     4 2(2X,'**'),9X,'**'/39X,'***',5X,'**',3X,7('*'),3X,'**',3X,7('*'),DAT  420
     5 3X,'**',2X,'**',9X,'**')                                         DAT  430
   30 FORMAT (//////25X,' JAN. 12',53X,'REPORT: V.S.O.P.(99/11)'/94X,'JUDAT  440
     1EL- 4348'/25X,' JUNE 10',53X,'REPORT: V.S.O.P.(99/09)'/94X,'JUEL -DAT  450
     2 4326'/94X,'SECTION 4.2.1')                                       DAT  451
   40 FORMAT ('1')                                                      DAT  460
  100 FORMAT (4I2)                                                      DAT  470
  101 FORMAT (I4)                                                       DAT  480
 1003 FORMAT (/////58X,'( ',I2,'.',I2,'.',I4,' )')                      DAT  490
      END                                                               DAT  500
      SUBROUTINE GITTER(LC)                                             GIT   10
C                                                                       GIT   20
C     ERSTELLUNG DER GITTER-AUSLEGUNGEN FUER CITATION BZW. THERMIX UND  GIT   30
C     VSOP                                                              GIT   40
C     JEWEILS ZUGEHOERIGE KOMPOSITIONSBELEGUNGEN                        GIT   50
C     STUETZSTELLEN-VORBEREITUNG FUER ALLE KURVEN (EINSCHL. DER FLIESS- GIT   60
C     KURVEN IM CORE                                                    GIT   70
C                                                                       GIT   80
CFZJ048 enlarged dimension                                    11.04.07  GIT   90
CFZJ049                                                       23.04.07  GIT  100
      REAL XWE(0:15,15),YWE(0:15,15),VOLU(15),YYY(1515),VEKA(0:15),     GIT  110
     1 DR(100),DZ(200),DGR(50),DGZ(100),RC(0:100),ZC(0:200),VOLREG(1515)GIT  120
     2 ,VFT(4000),ZPL(0:100),WEKA(0:15),R(50),DELF(50),RR(100),DDGR(50) GIT  130
C                                                                       GIT  140
CFZJ048 enlarged dimension                                    11.04.07  GIT  150
CFZJ049                                                       23.04.07  GIT  160
      INTEGER ICOPL(20),KLAY(1515),KANDEF(20),KLA(15),LAYV(100,200),    GIT  170
     1 NKN(1515),IOP(100),NOP(200),KANTYP(15),KANDET(20),NOPOWR(1515),  GIT  180
     2 NOPOWK(4000),MMGR(50)                                            GIT  190
C                                                                       GIT  200
      COMMON /FORALB/ UU,VV,XWE,YWE,IJR(0:15),N5,N6,N7,IJ               GIT  210
C                                                                       GIT  220
      COMMON /CORB/ HCORE,KAN(15),NKURVE,H4,R1,H3,R2,KANAL              GIT  230
C                                                                       GIT  240
      COMMON /GEOB/ MR(100),MZ(200),LAYVC(100,200),MGR(50),MGZ(100),    GIT  250
     1 ICOR(100),LAYC(100,200),JCOR(15),JI,JA,JI0,RKONUS,ZKONUS,RC,ZC,  GIT  260
     2 NCOR1,NCOR2,IMA,RKONI,ZKONI                                      GIT  270
C                                                                       GIT  280
      COMMON /VOLUB/ VGE,VOLU,VEKA,XPRUEF,IX,WEKA,KANDEF                GIT  290
C                                                                       GIT  300
      COMMON /VSOB/ KLAY,IPL,NVSOP,ICOPL,VOLREG,N20,KSCH                GIT  310
C                                                                       GIT  320
      COMMON /MAXIB/ IMAX,NMAX,DR,DZ,NGEOM                              GIT  330
C                                                                       GIT  340
      COMMON /VARDIB/ IZFEIN,JRFEIN,EP                                  GIT  350
C                                                                       GIT  360
      COMMON /KONSB/ PI,EPSY,YYY,IWRITE,ZPL,MDIREC,IPUT                 GIT  370
C                                                                       GIT  380
      COMMON /TERMIB/ NTHX,VFT                                          GIT  390
C                                                                       GIT  400
      COMMON /IBEB/ IRL,IRR,ITO,IBO                                     GIT  410
C                                                                       GIT  420
      COMMON /DB/ DELLAY(15),DLAY(15,101)                               GIT  430
C                                                                       GIT  440
      COMMON /NEB/ IOP,NOP,KANTYP,NCASE,RINTHX,DGR,DGZ,KONUS,KANALC,    GIT  450
     1 NMAXC                                                            GIT  460
C                                                                       GIT  470
      COMMON /UB/ KD,IMAXG,NMAXG,IL,RG,ZG,IWRC,IWRT,KC                  GIT  480
C                                                                       GIT  490
      COMMON /NOPO/ NOPOWR,NOPOWK                                       GIT  500
C                                                                       GIT  510
CFZJ057                                                       07.11.07  GIT  520
      COMMON /NXXC/ NCP(200)                                            GIT  530
C                                                                       GIT  540
  104 FORMAT (//' COMPOSITION ID. NUMBERS IN EXTERNAL PART OF COARSE-MESGIT  550
     1H GRID:'/' (CORE-REGION = 0)'/)                                   GIT  560
  106 FORMAT (///' RADII OF CITATION/THERMIX IN ASCENDING SEQUENCE:'/)  GIT  570
  107 FORMAT (10E12.5)                                                  GIT  580
  108 FORMAT (///' HEIGHTS OF CITATION/THERMIX FROM TOP TO BOTTOM:'/)   GIT  590
  109 FORMAT (///' MAX. CITATION- OR THERMIX-COMPOSITIONS:',I5,'  <---- GIT  600
     1(FOR CITATION THE NEW DIMENSION IS <= 2000)')                     GIT  610
  110 FORMAT (//' FINAL COMPOSITION ID. NUMBERS OF CITATION- OR THERMIX-GIT  620
     1COMPOSITIONS:'/)                                                  GIT  630
  111 FORMAT (///' CHARACTERISTIC OF RADIAL CHANNELS:'//3(30I4/))       GIT  640
  113 FORMAT (///' INNER AND OUTER REFLECTOR CHANNELS ADJACENT TO THE COGIT  650
     1RE:'// 10I4)                                                      GIT  660
  114 FORMAT (/// I8,'.CHANNEL-CURVE (INPUT DATA):'//' ABS.:',10E12.5/6XGIT  670
     1 ,5E12.5)                                                         GIT  680
  115 FORMAT (/ ' ORD.:',10E12.5/6X,5E12.5)                             GIT  690
  117 FORMAT (/// ' VSOP-REGIONS (WITHOUT EXTERNAL PART):')             GIT  700
  118 FORMAT (35X,55('='))                                              GIT  710
  119 FORMAT (/// ' CITATION/THERMIX-DELTAS (RADIAL) + MESH-POINTS:'/)  GIT  720
  120 FORMAT (10(I3,F9.3))                                              GIT  730
  121 FORMAT (/// ' CITATION/THERMIX-DELTAS (AXIAL) + MESH-POINTS:'/)   GIT  740
  123 FORMAT (35X,'VOLUME-MATRIX FOR VSOP/CITATION (',I6,' X',I6,' POINTGIT  750
     1S)')                                                              GIT  760
  124 FORMAT (///35X,'VOLUME-MATRIX FOR VSOP/ THERMIX (',I6,' X',I6,' POGIT  770
     1INTS)')                                                           GIT  780
  125 FORMAT (/' CHANNEL:',I5,14I7)                                     GIT  790
  126 FORMAT (I14,14I7)                                                 GIT  800
  127 FORMAT (/)                                                        GIT  810
  128 FORMAT (///' MAX. VSOP-BATCHES        :',I4 /' (IN CORE (INCLUDINGGIT  820
     1 CONE):',I4,' )')                                                 GIT  830
  129 FORMAT (//' FINAL ID. NUMBERS OF VSOP-BATCHES (EXTERNAL PART)  -  GIT  840
     1ANALOGOUS TO CITATION-COMPOSITIONS:'/)                            GIT  850
  130 FORMAT (///' REGION   BATCHES IN THE REGION'/)                    GIT  860
  131 FORMAT (I5,5X,25I4)                                               GIT  870
  140 FORMAT (24I5)                                                     GIT  880
  555 FORMAT (///' MESH-POINTS AT CORE-REFLECTOR EDGES (FOR PRINTOUT OF GIT  890
     1NEUTRON FLUX AND DOSIS)'//' TOP:',I4,' / BOTTOM:',I4,' / INSIDE:',GIT  900
     2 I4,' / OUTSIDE:',I4)                                             GIT  910
C                                                                       GIT  920
C                                                                       GIT  930
      IF(NTHX .GT. 0) GOTO 500                                          GIT  940
      RKONUS = 0.                                                       GIT  950
      ZKONUS = 0.                                                       GIT  960
  500 CONTINUE                                                          GIT  970
      DO 900 I=0,15                                                     GIT  980
        DO 900 J=1,15                                                   GIT  990
          XWE(I,J) = 0.                                                 GIT 1000
          YWE(I,J) = 0.                                                 GIT 1010
  900 CONTINUE                                                          GIT 1020
      DO 901 J=0,100                                                    GIT 1030
        RC(J) = 0.                                                      GIT 1040
  901 CONTINUE                                                          GIT 1050
      DO 902 J=0,200                                                    GIT 1060
        ZC(J) = 0.                                                      GIT 1070
  902 CONTINUE                                                          GIT 1080
      IVSOPC = 0                                                        GIT 1090
C                                                                       GIT 1100
      CALL INPUT                                                        GIT 1110
C                                                                       GIT 1120
      IF(NTHX .EQ. 0) WRITE (N6,123) IZFEIN,JRFEIN                      GIT 1130
      IF(NTHX .GT. 0) WRITE (N6,124) IZFEIN,JRFEIN                      GIT 1140
      WRITE (N6,118)                                                    GIT 1150
      DO 1 K=1,KANAL                                                    GIT 1160
        IF(NTHX .GT. 0) GOTO 43                                         GIT 1170
C                                                                       GIT 1180
C     ANZAHL VSOP-COMPOSITIONS IM CORE (+ GGF. KONUS)                   GIT 1190
C                                                                       GIT 1200
        IVSOPC = IVSOPC + KAN(K) * ICOPL(K)                             GIT 1210
   43   CONTINUE                                                        GIT 1220
        IF(KANTYP(K) .EQ. 0 .AND. VEKA(K) .GT. 0.) KD = 0               GIT 1230
        IF(KANTYP(K) .EQ. 0 .AND. VEKA(K) .EQ. 0.) KD = 1               GIT 1240
        IF(KANTYP(K) .EQ. 1 .AND. RKONUS .EQ. 0.) KD = 2                GIT 1250
        IF(KANTYP(K) .EQ. 1 .AND. RKONUS .GT. 0.) KD = 3                GIT 1260
        IF(KANTYP(K) .EQ. -1 .AND. KONUS .EQ. 1) KD = 4                 GIT 1270
        IF(KANTYP(K) .EQ. -1 .AND. KONUS .EQ. 2) KD = 6                 GIT 1280
        IF(KANTYP(K) .EQ. -2 .AND. KONUS .EQ. 2) KD = 4                 GIT 1290
        KANDEF(K) = KD                                                  GIT 1300
        IF(KD .EQ. 2 .OR. KD .EQ. 3) KANDEF(K) = 1                      GIT 1310
        IF(NTHX .EQ. 0) KANDET(K) = KANDEF(K)                           GIT 1320
        IF(NTHX .GT. 0) KANDEF(K) = KANDET(K)                           GIT 1330
        IF(KD .EQ. 3) IJR(K) = -1                                       GIT 1340
        IF(KD .EQ. 4) IJR(K) = -2                                       GIT 1350
        IF(KD .EQ. 6) IJR(K) = -3                                       GIT 1360
    1 CONTINUE                                                          GIT 1370
      N20 = KANAL + 1                                                   GIT 1380
      NKURVE = KANAL + 1                                                GIT 1390
C                                                                       GIT 1400
C     BENENNUNG DER VSOP-LAYERS (IM CORE MIT 1 BEGINNEND)               GIT 1410
C                                                                       GIT 1420
      KANALS = 0                                                        GIT 1430
      LY = 0                                                            GIT 1440
      DO 202 K=1,KANAL                                                  GIT 1450
        DO 201 KN=1,KAN(K)                                              GIT 1460
          KANALS = KANALS + 1                                           GIT 1470
          NKN(KANALS) = K                                               GIT 1480
          IF(KANDEF(K) .LE. 1) THEN                                     GIT 1490
           LY = LY + 1                                                  GIT 1500
           KLAY(KANALS) = LY                                            GIT 1510
          ELSE                                                          GIT 1520
           KLAY(KANALS) = 0                                             GIT 1530
          END IF                                                        GIT 1540
  201   CONTINUE                                                        GIT 1550
  202 CONTINUE                                                          GIT 1560
      DO 203 KL=1,KANALS                                                GIT 1570
        IF(KLAY(KL) .NE. 0) GOTO 203                                    GIT 1580
        LY = LY + 1                                                     GIT 1590
        KLAY(KL) = LY                                                   GIT 1600
  203 CONTINUE                                                          GIT 1610
      WRITE (N6,117)                                                    GIT 1620
      MKAN = 0                                                          GIT 1630
      DO 50 K=1,KANAL                                                   GIT 1640
        KANK = KAN(K)                                                   GIT 1650
        MKAN = MAX0(MKAN,KANK)                                          GIT 1660
   50 CONTINUE                                                          GIT 1670
      WRITE (N6,125) (K,K=1,KANAL)                                      GIT 1680
      WRITE (N6,127)                                                    GIT 1690
      DO 52 K=1,MKAN                                                    GIT 1700
        DO 51 I=1,KANAL                                                 GIT 1710
          KLA(I) = 0                                                    GIT 1720
          DO 63 KL=1,KANALS                                             GIT 1730
            IF(NKN(KL) .NE. I) GOTO 63                                  GIT 1740
            KLA(I) = KLAY(KL)                                           GIT 1750
            NKN(KL) = -NKN(KL)                                          GIT 1760
            GOTO 51                                                     GIT 1770
   63     CONTINUE                                                      GIT 1780
   51   CONTINUE                                                        GIT 1790
        WRITE (N6,126) (KLA(I),I=1,KANAL)                               GIT 1800
   52 CONTINUE                                                          GIT 1810
      IRL = 0                                                           GIT 1820
      DO 333 I=1,IMAX                                                   GIT 1830
        IF(IOP(I) .EQ. 0) GOTO 334                                      GIT 1840
        IRL = IRL + MR(I)                                               GIT 1850
  333 CONTINUE                                                          GIT 1860
  334 CONTINUE                                                          GIT 1870
      ITO = 0                                                           GIT 1880
      ITH = 0                                                           GIT 1890
CFZJ057                                                       07.11.07  GIT 1900
      DO 444 N=1,NMAX                                                   GIT 1910
        IF(NTHX .EQ. 0) NXX = NCP(N)                                    GIT 1920
        IF(NTHX .GT. 0) NXX = NOP(N)                                    GIT 1930
        IF(NXX .EQ. 0 .OR. MZ(N) .LT. 0) GOTO 445                       GIT 1940
        ITO = ITO + MZ(N)                                               GIT 1950
  444 CONTINUE                                                          GIT 1960
  445 CONTINUE                                                          GIT 1970
      IF(MZ(N) .LT. 0) ITH = IABS(MZ(N))                                GIT 1980
      MZ(N) = IABS(MZ(N))                                               GIT 1990
C                                                                       GIT 2000
C     ZUGEHOERIGE KOMPOSITIONSBELEGUNG (AEUSSERER TEIL)                 GIT 2010
C                                                                       GIT 2020
      WRITE (N6,104)                                                    GIT 2030
      DO 6 N=1,NMAX                                                     GIT 2040
        WRITE (N6,140) (LAYVC(I,N),I=1,IMAX)                            GIT 2050
    6 CONTINUE                                                          GIT 2060
      IPL = 0                                                           GIT 2070
      DO 207 N=1,NMAX                                                   GIT 2080
        DO 206 I=1,IMAX                                                 GIT 2090
          IF(LAYVC(I,N) .NE. 0) GOTO 206                                GIT 2100
          IPL = IPL + 1                                                 GIT 2110
          ZPL(IPL) = DZ(N)                                              GIT 2120
          GOTO 207                                                      GIT 2130
  206   CONTINUE                                                        GIT 2140
  207 CONTINUE                                                          GIT 2150
CFZJ056                                                       06.11.07  GIT 2160
      IRR = 0                                                           GIT 2170
CFZJ049                                                       23.04.07  GIT 2180
      DO 8 I=1,KANALC                                                   GIT 2190
        IRR = IRR + MGR(I)                                              GIT 2200
        IMAXG = I                                                       GIT 2210
        MMGR(I) = MGR(I)                                                GIT 2220
        DDGR(I) = DGR(I)                                                GIT 2230
    8 CONTINUE                                                          GIT 2240
      IF(NTHX .EQ. 0) GOTO 56                                           GIT 2250
      JMAXG = IMAXG                                                     GIT 2260
CFZJ049                                                       23.04.07  GIT 2270
      J = 1                                                             GIT 2280
      JZ = 0                                                            GIT 2290
      R(1) = RINTHX                                                     GIT 2300
      RR(1) = R(1)                                                      GIT 2310
      DO 55 I=1,IMAXG                                                   GIT 2320
        R(I+1) = R(I) + DDGR(I)                                         GIT 2330
        DELF(I) = (R(I+1)**2.-R(I)**2.) / FLOAT(MMGR(I))                GIT 2340
        DO 54 N=J,J+MMGR(I)-1                                           GIT 2350
          JZ = JZ + 1                                                   GIT 2360
          RR(N+1) = SQRT(RR(N)**2.+DELF(I))                             GIT 2370
          DGR(N) = RR(N+1) - RR(N)                                      GIT 2380
          MGR(N) = 1                                                    GIT 2390
   54   CONTINUE                                                        GIT 2400
        J = JZ + 1                                                      GIT 2410
   55 CONTINUE                                                          GIT 2420
   56 CONTINUE                                                          GIT 2430
      IRR = IRR + IRL + 1                                               GIT 2440
      IBO = 0                                                           GIT 2450
      DO 10 N=1,NMAXC                                                   GIT 2460
        IBO = IBO + MGZ(N)                                              GIT 2470
        NMAXG = N                                                       GIT 2480
   10 CONTINUE                                                          GIT 2490
      IF(NTHX .EQ. 0) GOTO 60                                           GIT 2500
      MMAXG = NMAXG                                                     GIT 2510
      DO 59 I=NMAXG,1,-1                                                GIT 2520
        MGZI = MGZ(I)                                                   GIT 2530
        MGZI1 = MGZI - 1                                                GIT 2540
        IF(MGZI .LE. 1) GOTO 59                                         GIT 2550
        DGZI = DGZ(I) / FLOAT(MGZI)                                     GIT 2560
        DO 57 J=MMAXG,I,-1                                              GIT 2570
          MGZ(J+MGZI1) = MGZ(J)                                         GIT 2580
          DGZ(J+MGZI1) = DGZ(J)                                         GIT 2590
   57   CONTINUE                                                        GIT 2600
        DO 58 J=I,I+MGZI1                                               GIT 2610
          MGZ(J) = 1                                                    GIT 2620
          DGZ(J) = DGZI                                                 GIT 2630
   58   CONTINUE                                                        GIT 2640
        MMAXG = MMAXG + MGZI1                                           GIT 2650
   59 CONTINUE                                                          GIT 2660
   60 CONTINUE                                                          GIT 2670
      IBO = IBO + ITO + ITH + 1                                         GIT 2680
C                                                                       GIT 2690
C     EINFUEGEN DES CORE-GITTERS FUER CITATION IN DIE GITTER-STRUKTUR   GIT 2700
C     DES AEUSSEREN TEILES (RADIAL)                                     GIT 2710
C                                                                       GIT 2720
      EPS = 0.01                                                        GIT 2730
      RB = 0.                                                           GIT 2740
      II = 0                                                            GIT 2750
      I = 0                                                             GIT 2760
      IF(NTHX .EQ. 0) GOTO 11                                           GIT 2770
      RC(I) = RINTHX                                                    GIT 2780
      DO 501 IT=1,IMAX                                                  GIT 2790
        MR(IT) = MGR(IT)                                                GIT 2800
        DR(IT) = DGR(IT)                                                GIT 2810
  501 CONTINUE                                                          GIT 2820
      GOTO 16                                                           GIT 2830
   11 CONTINUE                                                          GIT 2840
      I = I + 1                                                         GIT 2850
      IF(I .GT. IMAX) GOTO 16                                           GIT 2860
      IF(II .LE. 0) RG = RB                                             GIT 2870
      RB = RB + DR(I)                                                   GIT 2880
      IF(MR(I) .GT. 0) GOTO 11                                          GIT 2890
      III = 0                                                           GIT 2900
   12 CONTINUE                                                          GIT 2910
      II = II + 1                                                       GIT 2920
      RG = RG + DGR(II)                                                 GIT 2930
      III = III + 1                                                     GIT 2940
      IF(III .LE. 1) GOTO 15                                            GIT 2950
      DO 14,J=IMAX,I,-1                                                 GIT 2960
        DO 13 N=1,NMAX                                                  GIT 2970
          LAYVC(J+1,N) = LAYVC(J,N)                                     GIT 2980
   13   CONTINUE                                                        GIT 2990
   14 CONTINUE                                                          GIT 3000
      I = I + 1                                                         GIT 3010
      DO 214 J=IMAX,I,-1                                                GIT 3020
        DR(J+1) = DR(J)                                                 GIT 3030
        MR(J+1) = MR(J)                                                 GIT 3040
  214 CONTINUE                                                          GIT 3050
      IMAX = IMAX + 1                                                   GIT 3060
   15 CONTINUE                                                          GIT 3070
      DR(I) = DGR(II)                                                   GIT 3080
      MR(I) = MGR(II)                                                   GIT 3090
      IF(RG+EPS-RB) 12,11,11                                            GIT 3100
   16 CONTINUE                                                          GIT 3110
      DO 37 I=1,IMAX                                                    GIT 3120
        RC(I) = RC(I-1) + DR(I)                                         GIT 3130
   37 CONTINUE                                                          GIT 3140
C                                                                       GIT 3150
C     EINFUEGEN DES CORE-GITTERS FUER CITATION IN DIE GITTER-STRUKTUR   GIT 3160
C     DES AEUSSEREN TEILES (AXIAL)                                      GIT 3170
C                                                                       GIT 3180
      ZB = 0.                                                           GIT 3190
      NN = 0                                                            GIT 3200
      N = 0                                                             GIT 3210
      ZC(N) = 0.                                                        GIT 3220
      IF(NTHX .EQ. 0) GOTO 17                                           GIT 3230
      DO 502 NT=1,NMAX                                                  GIT 3240
        MZ(NT) = MGZ(NT)                                                GIT 3250
        DZ(NT) = DGZ(NT)                                                GIT 3260
  502 CONTINUE                                                          GIT 3270
      GOTO 22                                                           GIT 3280
   17 CONTINUE                                                          GIT 3290
      N = N + 1                                                         GIT 3300
      IF(N .GT. NMAX) GOTO 22                                           GIT 3310
      IF(NN .LE. 0) ZG = ZB                                             GIT 3320
      ZB = ZB + DZ(N)                                                   GIT 3330
      IF(MZ(N) .GT. 0) GOTO 17                                          GIT 3340
      NNN = 0                                                           GIT 3350
   18 CONTINUE                                                          GIT 3360
      NN = NN + 1                                                       GIT 3370
      ZG = ZG + DGZ(NN)                                                 GIT 3380
      NNN = NNN + 1                                                     GIT 3390
      IF(NNN .LE. 1) GOTO 21                                            GIT 3400
      DO 20,M=NMAX,N,-1                                                 GIT 3410
        DO 19 I=1,IMAX                                                  GIT 3420
          LAYVC(I,M+1) = LAYVC(I,M)                                     GIT 3430
   19   CONTINUE                                                        GIT 3440
   20 CONTINUE                                                          GIT 3450
      N = N + 1                                                         GIT 3460
      DO 220,M=NMAX,N,-1                                                GIT 3470
        DZ(M+1) = DZ(M)                                                 GIT 3480
        MZ(M+1) = MZ(M)                                                 GIT 3490
  220 CONTINUE                                                          GIT 3500
      NMAX = NMAX + 1                                                   GIT 3510
   21 CONTINUE                                                          GIT 3520
      DZ(N) = DGZ(NN)                                                   GIT 3530
      MZ(N) = MGZ(NN)                                                   GIT 3540
      IF(ZG+EPS-ZB) 18,17,17                                            GIT 3550
   22 CONTINUE                                                          GIT 3560
      DO 38 N=1,NMAX                                                    GIT 3570
        ZC(N) = ZC(N-1) + DZ(N)                                         GIT 3580
   38 CONTINUE                                                          GIT 3590
      IM1 = 1                                                           GIT 3600
      IM2 = IMAX                                                        GIT 3610
      IF(IMAX .GT. 24) IM2 = 24                                         GIT 3620
   80 CONTINUE                                                          GIT 3630
C     DO 23 N=1,NMAX                                                    GIT 3640
C       WRITE (N6,140) (LAYVC(I,N),I=IM1,IM2)                           GIT 3650
C  23 CONTINUE                                                          GIT 3660
      IF(IM2 .EQ. IMAX) GOTO 81                                         GIT 3670
      IM1 = IM2 + 1                                                     GIT 3680
      IM2 = IM2 + 24                                                    GIT 3690
      IF(IMAX .LT. IM2) IM2 = IMAX                                      GIT 3700
C     WRITE (6,127)                                                     GIT 3710
      GOTO 80                                                           GIT 3720
   81 CONTINUE                                                          GIT 3730
      WRITE (N6,119)                                                    GIT 3740
      WRITE (N6,120) (MR(I),DR(I),I=1,IMAX)                             GIT 3750
      WRITE (N6,106)                                                    GIT 3760
      WRITE (N6,107) (RC(I),I=0,IMAX)                                   GIT 3770
      WRITE (N6,121)                                                    GIT 3780
      WRITE (N6,120) (MZ(I),DZ(I),I=1,NMAX)                             GIT 3790
      WRITE (N6,108)                                                    GIT 3800
      WRITE (N6,107) (ZC(N),N=0,NMAX)                                   GIT 3810
      IF(NTHX .EQ. 0) WRITE (N6,555) ITO,IBO,IRL,IRR                    GIT 3820
C                                                                       GIT 3830
C     KOMPOSITIONSBELEGUNG DES CITATION-COREBEREICHES UND EVTL.ZUSAETZL.GIT 3840
C     RAD. REFL. (BEGINNEND MIT 1), DANACH NAHTLOSER ANSCHLUSS DER KOM- GIT 3850
C     POSITIONEN DES AEUSSEREN BEREICHES + CHARAKTERISTIK DER EINZELNEN GIT 3860
C     KANAELE DURCH *ICOR*                                              GIT 3870
C                                                                       GIT 3880
      LCMI = 1000                                                       GIT 3890
      LCMA = 0                                                          GIT 3900
      LC = 1                                                            GIT 3910
      NCOR1 = NMAX                                                      GIT 3920
      NCOR2 = 0                                                         GIT 3930
      DO 27 I=1,IMAX                                                    GIT 3940
        ICOR(I) = 0                                                     GIT 3950
        L0 = 1000                                                       GIT 3960
        DO 26 N=1,NMAX                                                  GIT 3970
          L = LAYVC(I,N)                                                GIT 3980
          L0 = MIN0(L0,L)                                               GIT 3990
          LCMA = MAX0(LCMA,L)                                           GIT 4000
          IF(L .GT. 0) GOTO 24                                          GIT 4010
          LAYC(I,N) = LC                                                GIT 4020
          LAYV(I,N) = 0                                                 GIT 4030
          LC = LC + 1                                                   GIT 4040
          NCOR1 = MIN0(NCOR1,N)                                         GIT 4050
          NCOR2 = MAX0(NCOR2,N)                                         GIT 4060
          ICOR(I) = I                                                   GIT 4070
          IF(L .LT. 0) ICOR(I) = -I                                     GIT 4080
          GOTO 25                                                       GIT 4090
   24     CONTINUE                                                      GIT 4100
          LCMI = MIN0(LCMI,L)                                           GIT 4110
   25     CONTINUE                                                      GIT 4120
   26   CONTINUE                                                        GIT 4130
   27 CONTINUE                                                          GIT 4140
      IF(LCMI .EQ. 1000) LCMI = 1                                       GIT 4150
      LC = LC - LCMI                                                    GIT 4160
      LCMAX = LCMA + LC                                                 GIT 4170
      LVMAX = IVSOPC + LCMA                                             GIT 4180
      DO 29 I=1,IMAX                                                    GIT 4190
        DO 28 N=1,NMAX                                                  GIT 4200
          L = LAYVC(I,N)                                                GIT 4210
          IF(L .LE. 0) GOTO 28                                          GIT 4220
          LAYC(I,N) = L + LC                                            GIT 4230
          LAYV(I,N) = L + IVSOPC                                        GIT 4240
   28   CONTINUE                                                        GIT 4250
   29 CONTINUE                                                          GIT 4260
      WRITE (N6,109) LCMAX                                              GIT 4270
      WRITE (N6,110)                                                    GIT 4280
      IM1 = 1                                                           GIT 4290
      IM2 = IMAX                                                        GIT 4300
      IF(IMAX .GT. 24) IM2 = 24                                         GIT 4310
   82 CONTINUE                                                          GIT 4320
      DO 30 N=1,NMAX                                                    GIT 4330
        WRITE (N6,140) (LAYC(I,N),I=IM1,IM2)                            GIT 4340
   30 CONTINUE                                                          GIT 4350
      IF(IM2 .EQ. IMAX) GOTO 83                                         GIT 4360
      IM1 = IM2 + 1                                                     GIT 4370
      IM2 = IM2 + 24                                                    GIT 4380
      IF(IMAX .LT. IM2) IM2 = IMAX                                      GIT 4390
      WRITE (6,127)                                                     GIT 4400
      GOTO 82                                                           GIT 4410
   83 CONTINUE                                                          GIT 4420
      IF(IVSOPC .EQ. 0) GOTO 45                                         GIT 4430
C                                                                       GIT 4440
C     DARSTELLUNG DER VSOP-COMPOSITIONS (AEUSSERER TEIL) ANALOG DER     GIT 4450
C     CITATION-LAYER                                                    GIT 4460
C                                                                       GIT 4470
      WRITE (N6,128) LVMAX,IVSOPC                                       GIT 4480
      WRITE (N6,129)                                                    GIT 4490
      IM1 = 1                                                           GIT 4500
      IM2 = IMAX                                                        GIT 4510
      IF(IMAX .GT. 24) IM2 = 24                                         GIT 4520
   84 CONTINUE                                                          GIT 4530
      DO 44 N=1,NMAX                                                    GIT 4540
        WRITE (N6,140) (LAYV(I,N),I=IM1,IM2)                            GIT 4550
   44 CONTINUE                                                          GIT 4560
      IF(IM2 .EQ. IMAX) GOTO 85                                         GIT 4570
      IM1 = IM2 + 1                                                     GIT 4580
      IM2 = IM2 + 24                                                    GIT 4590
      IF(IMAX .LT. IM2) IM2 = IMAX                                      GIT 4600
      WRITE (6,127)                                                     GIT 4610
      GO TO 84                                                          GIT 4620
   85 CONTINUE                                                          GIT 4630
      IB0 = 1                                                           GIT 4640
      IB1 = 0                                                           GIT 4650
      K0 = 0                                                            GIT 4660
      WRITE (N6,130)                                                    GIT 4670
      DO 47 K=1,KANALS                                                  GIT 4680
        K0 = K0 + 1                                                     GIT 4690
        DO 46 L=1,KANALS                                                GIT 4700
          IF(KLAY(L) .NE. K0) GOTO 46                                   GIT 4710
          KC = IABS(NKN(L))                                             GIT 4720
          GOTO 64                                                       GIT 4730
   46   CONTINUE                                                        GIT 4740
   64   CONTINUE                                                        GIT 4750
        IB1 = IB1 + ICOPL(KC)                                           GIT 4760
        WRITE (N6,131) K0,(IB,IB=IB0,IB1)                               GIT 4770
        NOPOWR(K0) = 0                                                  GIT 4780
        IL = K0 + 1                                                     GIT 4790
        IB0 = IB1 + 1                                                   GIT 4800
   47 CONTINUE                                                          GIT 4810
      IF(KONUS .EQ. 0) GOTO 49                                          GIT 4820
      DO 42 K=1,KONUS                                                   GIT 4830
        NOPOWR(K0+1-K) = 1                                              GIT 4840
   42 CONTINUE                                                          GIT 4850
   49 CONTINUE                                                          GIT 4860
      DO 48 ILA=IB0,LVMAX                                               GIT 4870
        WRITE (N6,131) IL,ILA                                           GIT 4880
        NOPOWR(IL) = 1                                                  GIT 4890
        IL = IL + 1                                                     GIT 4900
   48 CONTINUE                                                          GIT 4910
   45 CONTINUE                                                          GIT 4920
      IF(IPUT .GT. 0) WRITE (N6,111) (ICOR(I),I=1,IMAX)                 GIT 4930
C                                                                       GIT 4940
C     HERAUSFILTERN DER (UNABHAENGIG VOM AEUSSEREN BEREICH) NICHT ZUM COGIT 4950
C     GEHOERENDEN KANAELE BZW. DER INNEREN UND AEUSSEREN KANALBEGRENZUNGGIT 4960
C                                                                       GIT 4970
      JJ = 1                                                            GIT 4980
      JI = 0                                                            GIT 4990
      JI0 = 0                                                           GIT 5000
      DO 32 I=1,IMAX                                                    GIT 5010
        IF(JI0 .LE. 0 .AND. ICOR(I) .NE. 0) JI0 = I                     GIT 5020
        IF(ICOR(I) .GT. 0) JA = ICOR(I)                                 GIT 5030
        IF(JI .LE. 0 .AND. JA .GT. 0) JI = JA                           GIT 5040
        IF(ICOR(I) .EQ. 0) GOTO 32                                      GIT 5050
        IF(JJ .NE. 1) GOTO 31                                           GIT 5060
        JCOR(JJ) = I - 1                                                GIT 5070
        JJ = JJ + 1                                                     GIT 5080
   31   CONTINUE                                                        GIT 5090
        JCOR(JJ) = I                                                    GIT 5100
        IF(I .EQ. IMAX) GOTO 32                                         GIT 5110
        IF(ICOR(I) .LT. 0 .OR. ICOR(I+1) .LT. 0) JJ = JJ + 1            GIT 5120
   32 CONTINUE                                                          GIT 5130
      IF(IPUT .GT. 0) WRITE (N6,113) (JCOR(J),J=1,JJ)                   GIT 5140
C                                                                       GIT 5150
C     DEFINIEREN DER STUETZSTELLEN PRO KURVE (BZW. VERVOLLSTAENDIGEN DERGIT 5160
C     EINGELESENEN STUETZSTELLEN-ANGABEN)                               GIT 5170
C                                                                       GIT 5180
      ID = 0                                                            GIT 5190
      JJ = 1                                                            GIT 5200
      IMI = JCOR(JJ)                                                    GIT 5210
      IJR(ID) = 2                                                       GIT 5220
      XWE(ID,1) = ZC(NCOR1-1)                                           GIT 5230
      XWE(ID,2) = ZC(NCOR2)                                             GIT 5240
      YWE(ID,1) = RC(IMI)                                               GIT 5250
      YWE(ID,2) = RC(IMI)                                               GIT 5260
      WRITE (N6,114) ID,(XWE(ID,J),J=1,2)                               GIT 5270
      WRITE (N6,115) (YWE(ID,J),J=1,2)                                  GIT 5280
      IMA = 0                                                           GIT 5290
      DO 36 K=1,KANAL                                                   GIT 5300
        IJ = IJR(K)                                                     GIT 5310
        IF(IJ .LE. 0) GOTO 33                                           GIT 5320
        DO 39 J=1,IJ                                                    GIT 5330
          XWE(K,J) = XWE(K,J) + ZC(NCOR1-1)                             GIT 5340
          YWE(K,J) = YWE(K,J) + RC(IMI)                                 GIT 5350
   39   CONTINUE                                                        GIT 5360
        IF(IJ .GT. 1) GOTO 35                                           GIT 5370
        IJR(K) = 2                                                      GIT 5380
        XWE(K,2) = ZC(NCOR2)                                            GIT 5390
        YWE(K,2) = YWE(K,1)                                             GIT 5400
        GOTO 35                                                         GIT 5410
   33   CONTINUE                                                        GIT 5420
        IF(IJR(K) .NE. -2 .AND. IJR(K) .NE. -3) JJ = JJ + 1             GIT 5430
        IMI = JCOR(JJ)                                                  GIT 5440
        XWE(K,1) = ZC(NCOR1-1)                                          GIT 5450
        XWE(K,2) = ZC(NCOR2)                                            GIT 5460
        YWE(K,1) = RC(IMI)                                              GIT 5470
        YWE(K,2) = RC(IMI)                                              GIT 5480
        IF(IJR(K) .EQ.-1) GOTO 34                                       GIT 5490
        IF(IJR(K) .EQ.-3) GOTO 71                                       GIT 5500
        IJR(K) = 2                                                      GIT 5510
        GOTO 35                                                         GIT 5520
   34   CONTINUE                                                        GIT 5530
        IJR(K) = 3                                                      GIT 5540
        XWE(K,2) = ZC(NCOR2) - ZKONUS                                   GIT 5550
        XWE(K,3) = ZC(NCOR2)                                            GIT 5560
        YWE(K,3) = RC(IMI) - RKONUS                                     GIT 5570
        GOTO 35                                                         GIT 5580
   71   CONTINUE                                                        GIT 5590
        IJR(K) = 3                                                      GIT 5600
        XWE(K,2) = ZC(NCOR2) - ZKONUS                                   GIT 5610
        XWE(K,3) = ZC(NCOR2)                                            GIT 5620
        YWE(K,3) = RC(IMI) + RKONUS                                     GIT 5630
   35   CONTINUE                                                        GIT 5640
        IJ = IJR(K)                                                     GIT 5650
        WRITE (N6,114) K,(XWE(K,J),J=1,IJ)                              GIT 5660
        WRITE (N6,115) (YWE(K,J),J=1,IJ)                                GIT 5670
        IMA = MAX0(IMA,IMI)                                             GIT 5680
   36 CONTINUE                                                          GIT 5690
      RETURN                                                            GIT 5700
      END                                                               GIT 5710
      SUBROUTINE INPUT                                                  INP   10
C                                                                       INP   20
C     SAEMTLICHER INPUT (BI-KARTEN)                                     INP   30
C                                                                       INP   40
CFZJ048 enlarged dimension                                    11.04.07  INP   50
      REAL XWE(0:15,15),YWE(0:15,15),VOLU(15),YYY(1515),VEKA(0:15),     INP   60
     1 DR(100),DZ(200),DGR(50),DGZ(100),RC(0:100),ZC(0:200),VOLREG(1515)INP   70
     2 ,VFT(4000),ZPL(0:100),WEKA(0:15),YWT(0:15,15),XWT(0:15,15),      INP   80
     3 VEKAT(0:15),DELR(100),DELZ(200)                                  INP   90
C                                                                       INP  100
      INTEGER ICOPL(20),KLAY(1515),KANDEF(20),IOP(100),NOP(200),        INP  110
     1 KANTYP(15),IJT(0:15),MPR(100),MPZ(200)                           INP  120
C                                                                       INP  130
      COMMON /FORALB/ UU,VV,XWE,YWE,IJR(0:15),N5,N6,N7,IJ,N8            INP  140
C                                                                       INP  150
      COMMON /CORB/ HCORE,KAN(15),NKURVE,H4,R1,H3,R2,KANAL              INP  160
C                                                                       INP  170
      COMMON /GEOB/ MR(100),MZ(200),LAYVC(100,200),MGR(50),MGZ(100),    INP  180
     1 ICOR(100),LAYC(100,200),JCOR(15),JI,JA,JI0,RKONUS,ZKONUS,RC,ZC,  INP  190
     2 NCOR1,NCOR2,IMA,RKONI,ZKONI                                      INP  200
C                                                                       INP  210
      COMMON /VOLUB/ VGE,VOLU,VEKA,XPRUEF,IX,WEKA,KANDEF                INP  220
C                                                                       INP  230
      COMMON /VSOB/ KLAY,IPL,NVSOP,ICOPL,VOLREG,N20,KSCH                INP  240
C                                                                       INP  250
      COMMON /MAXIB/ IMAX,NMAX,DR,DZ,NGEOM                              INP  260
C                                                                       INP  270
      COMMON /VARDIB/ IZFEIN,JRFEIN,EP                                  INP  280
C                                                                       INP  290
      COMMON /KONSB/ PI,EPSY,YYY,IWRITE,ZPL,MDIREC,IPUT                 INP  300
C                                                                       INP  310
      COMMON /DB/ DELLAY(15),DLAY(15,101)                               INP  320
C                                                                       INP  330
      COMMON /TERMIB/ NTHX,VFT                                          INP  340
C                                                                       INP  350
      COMMON /IBEB/ IRL,IRR,ITO,IBO                                     INP  360
C                                                                       INP  370
      COMMON /NEB/ IOP,NOP,KANTYP,NCASE,RINTHX,DGR,DGZ,KONUS,KANALC,    INP  380
     1 NMAXC,CIZET0                                                     INP  390
C                                                                       INP  400
      COMMON /GRID/ DELR,DELZ,MPR,MPZ,ITOT,NTOT,N1,N2                   INP  410
C                                                                       INP  420
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             INP  430
C                                                                       INP  440
CFZJ057                                                       07.11.07  INP  450
      COMMON /NXXC/ NCP(200)                                            INP  460
C                                                                       INP  470
   10 FORMAT (5I6)                                                      INP  480
   30 FORMAT (I6,E12.5,I6)                                              INP  490
   40 FORMAT (24I3)                                                     INP  500
   50 FORMAT (3I6,3E12.5)                                               INP  510
   60 FORMAT (I12,E12.5)                                                INP  520
   70 FORMAT (6E12.5)                                                   INP  530
C                                                                       INP  540
C                                                                       INP  550
      IF(NTHX .GT. 0) GOTO 14                                           INP  560
      CIZET0 = 0.                                                       INP  570
      N7 = 0                                                            INP  580
      N8 = 0                                                            INP  590
C                                                                       INP  600
CARD BI1                                                                INP  610
C                                                                       INP  620
      READ (N5,10) NCASE,IPUT,IPLOT,KANAL                               INP  630
C                                                                       INP  640
      IF(NCASE .EQ. 0) MDIREC = 1                                       INP  650
      IF(IPLOT .GT. 0) N8 = 76                                          INP  660
      DO 1 I=1,KANAL                                                    INP  670
        DELLAY(I) = 0.                                                  INP  680
C                                                                       INP  690
CARD BI2                                                                INP  700
C                                                                       INP  710
        READ (N5,10) KANTYP(I),KAN(I),ICOPL(I)                          INP  720
C                                                                       INP  730
        IF(NCASE .GT. 0) GOTO 18                                        INP  740
        DELLAY(I) = -1.                                                 INP  750
        IF(I .EQ. 1) DELLAY(I) = 1.                                     INP  760
   18   CONTINUE                                                        INP  770
        IF(KAN(I) .EQ. 0) KAN(I) = 1                                    INP  780
        IF(ICOPL(I) .EQ. 0) ICOPL(I) = 1                                INP  790
    1 CONTINUE                                                          INP  800
      I = 0                                                             INP  810
      II = 0                                                            INP  820
    2 CONTINUE                                                          INP  830
      I = I + 1                                                         INP  840
C                                                                       INP  850
CARD BI3                                                                INP  860
C                                                                       INP  870
      READ (N5,30) IOP(I),DELR(I),MPR(I)                                INP  880
C                                                                       INP  890
      IF(IOP(I) .EQ. 2 .OR. IOP(I) .LT. 0) GOTO 41                      INP  900
      II = II + 1                                                       INP  910
      DR(II) = DELR(I)                                                  INP  920
      MR(II) = MPR(I)                                                   INP  930
   41 CONTINUE                                                          INP  940
      IF(IOP(I) .GE. 0) GOTO 2                                          INP  950
      IMAX = II                                                         INP  960
      ITOT = I - 1                                                      INP  970
      N = 0                                                             INP  980
      NN = 0                                                            INP  990
    3 CONTINUE                                                          INP 1000
      N = N + 1                                                         INP 1010
C                                                                       INP 1020
CARD BI4                                                                INP 1030
C                                                                       INP 1040
      READ (N5,30) NOP(N),DELZ(N),MPZ(N)                                INP 1050
C                                                                       INP 1060
      IF(NOP(N) .EQ. 2 .OR. NOP(N) .LT. 0) GOTO 42                      INP 1070
      NN = NN + 1                                                       INP 1080
      DZ(NN) = DELZ(N)                                                  INP 1090
      MZ(NN) = MPZ(N)                                                   INP 1100
      NCP(NN) = NOP(N)                                                  INP 1110
   42 CONTINUE                                                          INP 1120
      IF(NOP(N) .GE. 0) GOTO 3                                          INP 1130
      NMAX = NN                                                         INP 1140
      NTOT = N - 1                                                      INP 1150
      KENN = 0                                                          INP 1160
      DO 4 N=1,NMAX                                                     INP 1170
C                                                                       INP 1180
CARD BI5                                                                INP 1190
C                                                                       INP 1200
        READ (N5,40) (LAYVC(I,N),I=1,IMAX)                              INP 1210
C                                                                       INP 1220
    4 CONTINUE                                                          INP 1230
      N = 0                                                             INP 1240
      RINTHX = 0.                                                       INP 1250
      JRFEIN = 0                                                        INP 1260
      DO 5 I=1,IMAX                                                     INP 1270
        IF(IOP(I) .EQ. 1 .AND. KENN .EQ. 0) RINTHX = RINTHX + DR(I)     INP 1280
        IF(IOP(I) .EQ. 1) GOTO 5                                        INP 1290
        KENN = 1                                                        INP 1300
        N = N + 1                                                       INP 1310
        MGR(N) = MR(I)                                                  INP 1320
        JRFEIN = JRFEIN + MGR(N)                                        INP 1330
        DGR(N) = DR(I)                                                  INP 1340
    5 CONTINUE                                                          INP 1350
      KANALC = N                                                        INP 1360
      I = 0                                                             INP 1370
      IZFEIN = 0                                                        INP 1380
      DO 6 N=1,NMAX                                                     INP 1390
        IF(NCP(N) .EQ. 1) GOTO 6                                        INP 1400
        I = I + 1                                                       INP 1410
        IF(I .EQ. 1) ICI = N - 1                                        INP 1420
        MGZ(I) = MZ(N)                                                  INP 1430
        IZFEIN = IZFEIN + IABS(MGZ(I))                                  INP 1440
        DGZ(I) = DZ(N)                                                  INP 1450
        IF(NCASE .EQ. 0) DLAY(1,I) = DGZ(I)                             INP 1460
    6 CONTINUE                                                          INP 1470
      NMAXC = I                                                         INP 1480
      DO 19 N=1,ICI                                                     INP 1490
        CIZET0 = CIZET0 + DZ(N)                                         INP 1500
   19 CONTINUE                                                          INP 1510
      DO 11 K=1,KANAL-1                                                 INP 1520
        YWE(K,1) = YWE(K-1,1) + DGR(K)                                  INP 1530
        XWE(K,1) = 0.                                                   INP 1540
        IJR(K) = 1                                                      INP 1550
   11 CONTINUE                                                          INP 1560
      JRFEIN = JRFEIN * 100                                             INP 1570
      IZFEIN = IZFEIN * 100                                             INP 1580
      EP = 0.1                                                          INP 1590
      EPSY = 1.E-6                                                      INP 1600
      KONUS = 0                                                         INP 1610
      IF(NCASE .EQ. 0) GOTO 13                                          INP 1620
C                                                                       INP 1630
CARD BI6                                                                INP 1640
C                                                                       INP 1650
      READ (N5,50) KONUS,IZFEIN,JRFEIN,EPSY,RKONUS,ZKONUS               INP 1660
C                                                                       INP 1670
      EP = 0.1                                                          INP 1680
      DO 7 I=1,KANAL                                                    INP 1690
C                                                                       INP 1700
CARD BI7                                                                INP 1710
C                                                                       INP 1720
        READ (N5,60) IJR(I),VEKA(I)                                     INP 1730
C                                                                       INP 1740
CARD BI8                                                                INP 1750
C                                                                       INP 1760
        IF(IJR(I) .GT. 0) READ (N5,70) (XWE(I,J),J=1,IJR(I))            INP 1770
C                                                                       INP 1780
        IF(IJR(I) .GE. 0) GOTO 9                                        INP 1790
        IJR(I) = IJR(I-1)                                               INP 1800
        DO 8 J=1,IJR(I)                                                 INP 1810
          XWE(I,J) = XWE(I-1,J)                                         INP 1820
    8   CONTINUE                                                        INP 1830
    9   CONTINUE                                                        INP 1840
C                                                                       INP 1850
CARD BI9                                                                INP 1860
C                                                                       INP 1870
        IF(IJR(I) .GT. 0) READ (N5,70) (YWE(I,J),J=1,IJR(I))            INP 1880
C                                                                       INP 1890
    7 CONTINUE                                                          INP 1900
      IF(KONUS .EQ. 0) GOTO 13                                          INP 1910
      KANAL = KANAL + 1                                                 INP 1920
      KANTYP(KANAL) = -1                                                INP 1930
      KAN(KANAL) = 1                                                    INP 1940
      ICOPL(KANAL) = 1                                                  INP 1950
      IJR(KANAL) = 0                                                    INP 1960
      VEKA(KANAL) = 0.                                                  INP 1970
      IF(KONUS .LT. 2) GOTO 13                                          INP 1980
      RKONI = RKONUS                                                    INP 1990
      DO 12 I=KANAL,1,-1                                                INP 2000
        KAN(I+1) = KAN(I)                                               INP 2010
        ICOPL(I+1) = ICOPL(I)                                           INP 2020
        IJR(I+1) = IJR(I)                                               INP 2030
        VEKA(I+1) = VEKA(I)                                             INP 2040
        KANTYP(I+1) = KANTYP(I)                                         INP 2050
        DO 21 J=1,IJR(I)                                                INP 2060
          XWE(I+1,J) = XWE(I,J)                                         INP 2070
          YWE(I+1,J) = YWE(I,J)                                         INP 2080
   21   CONTINUE                                                        INP 2090
   12 CONTINUE                                                          INP 2100
      KANTYP(1) = -1                                                    INP 2110
      KAN(1) = 1                                                        INP 2120
      ICOPL(1) = 1                                                      INP 2130
      VEKA(1) = 0.                                                      INP 2140
      DO 24 J=1,IJR(1)                                                  INP 2150
        XWE(1,J) = 0.                                                   INP 2160
        YWE(1,J) = 0.                                                   INP 2170
   24 CONTINUE                                                          INP 2180
      IJR(1) = 0                                                        INP 2190
      KANAL = KANAL + 1                                                 INP 2200
      KANTYP(KANAL) = -2                                                INP 2210
   13 CONTINUE                                                          INP 2220
      DO 31 I=1,KANAL                                                   INP 2230
        VEKAT(I) = VEKA(I)                                              INP 2240
        IJT(I) = IJR(I)                                                 INP 2250
        DO 22 J=1,IJR(I)                                                INP 2260
          XWT(I,J) = XWE(I,J)                                           INP 2270
          YWT(I,J) = YWE(I,J)                                           INP 2280
   22   CONTINUE                                                        INP 2290
   31 CONTINUE                                                          INP 2300
      RETURN                                                            INP 2310
   14 CONTINUE                                                          INP 2320
      II = 0                                                            INP 2330
      IMAZ = 0                                                          INP 2340
      IMAX = 0                                                          INP 2350
      DO 15 I=1,ITOT                                                    INP 2360
        IMAZ = IMAZ + MPR(I)                                            INP 2370
        IF(IOP(I) .GT. 0) GOTO 15                                       INP 2380
        II = II + 1                                                     INP 2390
        DGR(II) = DELR(I)                                               INP 2400
        MGR(II) = MPR(I)                                                INP 2410
        IMAX = IMAX + MGR(II)                                           INP 2420
   15 CONTINUE                                                          INP 2430
C                                                                       INP 2440
C     DIMENSIONS FOR THERMIX                                            INP 2450
C                                                                       INP 2460
C                                                                       INP 2470
      DO 32 I=1,KANAL                                                   INP 2480
        VEKA(I) = VEKAT(I)                                              INP 2490
        IJR(I) = IJT(I)                                                 INP 2500
        DO 23 J=1,IJR(I)                                                INP 2510
          XWE(I,J) = XWT(I,J)                                           INP 2520
          YWE(I,J) = YWT(I,J)                                           INP 2530
   23   CONTINUE                                                        INP 2540
   32 CONTINUE                                                          INP 2550
      KANALC = II                                                       INP 2560
      NN = 0                                                            INP 2570
      NMAZ = 0                                                          INP 2580
      NMAX = 0                                                          INP 2590
      N1 = 0                                                            INP 2600
      N2 = 0                                                            INP 2610
      IKENN = 0                                                         INP 2620
      DO 16 N=1,NTOT                                                    INP 2630
        NMAZ = NMAZ + IABS(MPZ(N))                                      INP 2640
        IF(NOP(N) .GT. 0 .AND. IKENN .EQ. 0) N1 = N1 + IABS(MPZ(N))     INP 2650
        IF(NOP(N) .EQ. 1 .AND. IKENN .EQ. 0) N2 = N2 + IABS(MPZ(N))     INP 2660
        IF(NOP(N) .NE. 0) GOTO 16                                       INP 2670
        NN = NN + 1                                                     INP 2680
        DGZ(NN) = DELZ(N)                                               INP 2690
        MGZ(NN) = MPZ(N)                                                INP 2700
        NMAX = NMAX + MGZ(NN)                                           INP 2710
        IKENN = 1                                                       INP 2720
   16 CONTINUE                                                          INP 2730
C                                                                       INP 2740
C     DIMENSIONS FOR THERMIX/KONVEK                                     INP 2750
C                                                                       INP 2760
      NMAZ = MAX0(NMAZ,IMAZ) + 2                                        INP 2770
      IMAZ = NMAZ                                                       INP 2780
      NCO = MAX0(NMAX,IMAX) + 2                                         INP 2790
      ICO = NCO                                                         INP 2800
      KONN = NMAZ                                                       INP 2810
      KONI = KONN                                                       INP 2820
C                                                                       INP 2830
      NMAXC = NN                                                        INP 2840
      DO 17 N=1,NMAX                                                    INP 2850
        DO 17 I=1,IMAX                                                  INP 2860
          LAYVC(I,N) = 0                                                INP 2870
   17 CONTINUE                                                          INP 2880
      RETURN                                                            INP 2890
      END                                                               INP 2900
      SUBROUTINE LAGRASB(ZGROB,POGROB)                                  LAG   10
C                                                                       LAG   20
C     SUBROUTINE LAGRASB: INTERPOLIERT NACH VORGEGEBENEN STUETZSTELLEN  LAG   30
C                                                                       LAG   40
C     INTERPOLATION NACH LAGRANGE                                       LAG   50
C                                                                       LAG   60
      REAL*8 A1,A2,A3,A,B,C,D,B1,B2,B3,C1,C2,C3,D1,D2,D3,FX1,FX2,FX3,FX4LAG   70
C                                                                       LAG   80
      REAL ZGROB(100),POGROB(100),XWE(0:15,15),YWE(0:15,15)             LAG   90
C                                                                       LAG  100
      COMMON /FORALB/ UU,VV,XWE,YWE,IJR(0:15),N5,N6,N7,IJ               LAG  110
C                                                                       LAG  120
      COMMON /UB/ DUM(9),IP                                             LAG  130
C                                                                       LAG  140
C                                                                       LAG  150
      IF(ZGROB(IJ)-UU) 9,1,1                                            LAG  160
    1 CONTINUE                                                          LAG  170
      DO 2 II=1,IJ                                                      LAG  180
        IP = II                                                         LAG  190
CFZJ032                                                        14.7.04  LAG  200
        IZGROB = IFIX(ZGROB(IP))                                        LAG  210
        RZGROB = IZGROB                                                 LAG  220
        IF(RZGROB-UU) 2,3,4                                             LAG  230
    2 CONTINUE                                                          LAG  240
    3 VV = POGROB(IP)                                                   LAG  250
      RETURN                                                            LAG  260
    4 CONTINUE                                                          LAG  270
      IF(IJ .LT. 4) GOTO 11                                             LAG  280
      IP = IP - 1                                                       LAG  290
      IF(IP-1) 10,5,6                                                   LAG  300
    5 IP = IP + 1                                                       LAG  310
      GOTO 8                                                            LAG  320
    6 IF(IJ-IP-2) 7,8,8                                                 LAG  330
    7 IP = IP - 1                                                       LAG  340
C                                                                       LAG  350
C     FAKTOREN FUER DEN ZAEHLER                                         LAG  360
C                                                                       LAG  370
    8 A = UU - ZGROB(IP-1)                                              LAG  380
      B = UU - ZGROB(IP)                                                LAG  390
      C = UU - ZGROB(IP+1)                                              LAG  400
      D = UU - ZGROB(IP+2)                                              LAG  410
C                                                                       LAG  420
C     FAKTOREN FUER DEN NENNER                                          LAG  430
C                                                                       LAG  440
      A1 = ZGROB(IP-1) - ZGROB(IP)                                      LAG  450
      A2 = ZGROB(IP-1) - ZGROB(IP+1)                                    LAG  460
      A3 = ZGROB(IP-1) - ZGROB(IP+2)                                    LAG  470
      B1 = -A1                                                          LAG  480
      B2 = ZGROB(IP) - ZGROB(IP+1)                                      LAG  490
      B3 = ZGROB(IP) - ZGROB(IP+2)                                      LAG  500
      C1 = -A2                                                          LAG  510
      C2 = -B2                                                          LAG  520
      C3 = ZGROB(IP+1) - ZGROB(IP+2)                                    LAG  530
      D1 = -A3                                                          LAG  540
      D2 = -B3                                                          LAG  550
      D3 = -C3                                                          LAG  560
      FX1 = (B*C*D*POGROB(IP-1)) / (A1*A2*A3)                           LAG  570
      FX2 = (A*C*D*POGROB(IP)) / (B1*B2*B3)                             LAG  580
      FX3 = (A*B*D*POGROB(IP+1)) / (C1*C2*C3)                           LAG  590
      FX4 = (A*B*C*POGROB(IP+2)) / (D1*D2*D3)                           LAG  600
      VV = FX1 + FX2 + FX3 + FX4                                        LAG  610
      RETURN                                                            LAG  620
C                                                                       LAG  630
C     LINEARE EXTRAPOLATION                                             LAG  640
C                                                                       LAG  650
    9 IP = IJ                                                           LAG  660
   11 CONTINUE                                                          LAG  670
      VV = POGROB(IP-1) + (POGROB(IP)-POGROB(IP-1)) / (ZGROB(IP)-ZGROB( LAG  680
     1 IP-1)) * (UU-ZGROB(IP-1))                                        LAG  690
      RETURN                                                            LAG  700
   10 VV = (POGROB(2)*(ZGROB(1)-UU)-(POGROB(1)*(ZGROB(2)-UU))) / (ZGROB(LAG  710
     1 1)-ZGROB(2))                                                     LAG  720
      RETURN                                                            LAG  730
      END                                                               LAG  740
      SUBROUTINE LAGRAD(ZGROB,POGROB)                                   AGR   10
C                                                                       AGR   20
C     SUBROUTINE LAGRAD: INTERPOLIERT NACH VORGEGEBENEN STUETZSTELLEN   AGR   30
C     (ALLES DOPPELT GENAU)                                             AGR   40
C                                                                       AGR   50
C     INTERPOLATION NACH LAGRANGE                                       AGR   60
C                                                                       AGR   70
C                                                                       AGR   80
      REAL*8 A1,A2,A3,A,B,C,D,B1,B2,B3,C1,C2,C3,D1,D2,D3,FX1,FX2,FX3,FX4AGR   90
     1 ,UU,VV,ZGROB(100),POGROB(100)                                    AGR  100
C                                                                       AGR  110
      REAL XWE(0:15,15),YWE(0:15,15)                                    AGR  120
C                                                                       AGR  130
      COMMON /FORALB/ US,VS,XWE,YWE,IJR(0:15),N5,N6,N7,IJ               AGR  140
C                                                                       AGR  150
      COMMON /UVB/ UU,VV                                                AGR  160
C                                                                       AGR  170
      COMMON /UB/ DUM(9),IP                                             AGR  180
C                                                                       AGR  190
C                                                                       AGR  200
      IF(ZGROB(IJ)-UU) 9,1,1                                            AGR  210
    1 CONTINUE                                                          AGR  220
      DO 2 II=1,IJ                                                      AGR  230
        IP = II                                                         AGR  240
        IF(ZGROB(IP)-UU) 2,3,4                                          AGR  250
    2 CONTINUE                                                          AGR  260
    3 VV = POGROB(IP)                                                   AGR  270
      RETURN                                                            AGR  280
    4 CONTINUE                                                          AGR  290
      IF(IJ .LT. 4) GOTO 11                                             AGR  300
      IP = IP - 1                                                       AGR  310
      IF(IP-1) 10,5,6                                                   AGR  320
    5 IP = IP + 1                                                       AGR  330
      GOTO 8                                                            AGR  340
    6 IF(IJ-IP-2) 7,8,8                                                 AGR  350
    7 IP = IP - 1                                                       AGR  360
C                                                                       AGR  370
C     FAKTOREN FUER DEN ZAEHLER                                         AGR  380
C                                                                       AGR  390
    8 A = UU - ZGROB(IP-1)                                              AGR  400
      B = UU - ZGROB(IP)                                                AGR  410
      C = UU - ZGROB(IP+1)                                              AGR  420
      D = UU - ZGROB(IP+2)                                              AGR  430
C                                                                       AGR  440
C     FAKTOREN FUER DEN NENNER                                          AGR  450
C                                                                       AGR  460
      A1 = ZGROB(IP-1) - ZGROB(IP)                                      AGR  470
      A2 = ZGROB(IP-1) - ZGROB(IP+1)                                    AGR  480
      A3 = ZGROB(IP-1) - ZGROB(IP+2)                                    AGR  490
      B1 = -A1                                                          AGR  500
      B2 = ZGROB(IP) - ZGROB(IP+1)                                      AGR  510
      B3 = ZGROB(IP) - ZGROB(IP+2)                                      AGR  520
      C1 = -A2                                                          AGR  530
      C2 = -B2                                                          AGR  540
      C3 = ZGROB(IP+1) - ZGROB(IP+2)                                    AGR  550
      D1 = -A3                                                          AGR  560
      D2 = -B3                                                          AGR  570
      D3 = -C3                                                          AGR  580
      FX1 = (B*C*D*POGROB(IP-1)) / (A1*A2*A3)                           AGR  590
      FX2 = (A*C*D*POGROB(IP)) / (B1*B2*B3)                             AGR  600
      FX3 = (A*B*D*POGROB(IP+1)) / (C1*C2*C3)                           AGR  610
      FX4 = (A*B*C*POGROB(IP+2)) / (D1*D2*D3)                           AGR  620
      VV = FX1 + FX2 + FX3 + FX4                                        AGR  630
      RETURN                                                            AGR  640
C                                                                       AGR  650
C     LINEARE EXTRAPOLATION                                             AGR  660
C                                                                       AGR  670
    9 IP = IJ                                                           AGR  680
   11 CONTINUE                                                          AGR  690
      VV = POGROB(IP-1) + (POGROB(IP)-POGROB(IP-1)) / (ZGROB(IP)-ZGROB( AGR  700
     1 IP-1)) * (UU-ZGROB(IP-1))                                        AGR  710
      RETURN                                                            AGR  720
   10 VV = (POGROB(2)*(ZGROB(1)-UU)-(POGROB(1)*(ZGROB(2)-UU))) / (ZGROB(AGR  730
     1 1)-ZGROB(2))                                                     AGR  740
      RETURN                                                            AGR  750
      END                                                               AGR  760
      SUBROUTINE PLOTB                                                  PLO   10
C                                                                       PLO   20
C     SUBROUTINE PLOTB : BESCHREIBT UNIT N7 MIT PLOT-DATEN              PLO   30
C                                                                       PLO   40
      REAL ZGROB(100),POGROB(100),XWE(0:15,15),YWE(0:15,15),RC(0:100),  PLO   50
     1 ZC(0:200),YFUN(200),XFUN(200)                                    PLO   60
C                                                                       PLO   70
      COMMON /FORALB/ UU,VV,XWE,YWE,IJR(0:15),N5,N6,N7,IJ,N8            PLO   80
C                                                                       PLO   90
      COMMON /CORB/ HCORE,KAN(15),NKURVE,H4,R1,H3,R2,KANAL              PLO  100
C                                                                       PLO  110
      COMMON /GEOB/ MR(100),MZ(200),LAYVC(100,200),MGR(50),MGZ(100),    PLO  120
     1 ICOR(100),LAYC(100,200),JCOR(15),JI,JA,JI0,RKONUS,ZKONUS,RC,ZC,  PLO  130
     2 NCOR1,NCOR2,IMA                                                  PLO  140
C                                                                       PLO  150
      COMMON /MAXIB/ IMAX,NMAX                                          PLO  160
C                                                                       PLO  170
      CHARACTER*11 BLANC/'           '/                                 PLO  180
C                                                                       PLO  190
  101 FORMAT (6E12.5)                                                   PLO  200
  102 FORMAT (A11)                                                      PLO  210
C                                                                       PLO  220
C                                                                       PLO  230
      IPA = ZC(NCOR1-1)                                                 PLO  240
      IPE = (IFIX(HCORE)/10) * 10                                       PLO  250
      IPE = IPE + IPA                                                   PLO  260
      DO 1000 K=0,KANAL                                                 PLO  270
        IJ = IJR(K)                                                     PLO  280
        DO 10 I=1,IJ                                                    PLO  290
          ZGROB(I) = XWE(K,I)                                           PLO  300
          POGROB(I) = YWE(K,I)                                          PLO  310
   10   CONTINUE                                                        PLO  320
        IZAEHL = 0                                                      PLO  330
        DO 300 L=IPA,IPE,10                                             PLO  340
          UU = L                                                        PLO  350
C                                                                       PLO  360
          CALL LAGRASB(ZGROB,POGROB)                                    PLO  370
C                                                                       PLO  380
          IZAEHL = IZAEHL + 1                                           PLO  390
          YFUN(IZAEHL) = VV                                             PLO  400
          XFUN(IZAEHL) = UU                                             PLO  410
  300   CONTINUE                                                        PLO  420
        UU = HCORE + ZC(NCOR1-1)                                        PLO  430
C                                                                       PLO  440
        CALL LAGRASB(ZGROB,POGROB)                                      PLO  450
C                                                                       PLO  460
        IZAEHL = IZAEHL + 1                                             PLO  470
        YFUN(IZAEHL) = VV                                               PLO  480
        XFUN(IZAEHL) = UU                                               PLO  490
        IDIF = IZAEHL / 6                                               PLO  500
        BI = FLOAT(IDIF) - FLOAT(IZAEHL) / 6.                           PLO  510
        DO 400 IX=1,IDIF                                                PLO  520
          IF(N7 .GT. 0) WRITE (N7,101) (XFUN(KA),KA=(IX-1)*6+1,IX*6)    PLO  530
  400   CONTINUE                                                        PLO  540
        IF(BI .EQ. 0.) GOTO 401                                         PLO  550
        IF(N7 .GT. 0) WRITE (N7,101) (XFUN(LA),LA=IDIF*6+1,IZAEHL)      PLO  560
  401   CONTINUE                                                        PLO  570
        DO 500 IY=1,IDIF                                                PLO  580
          IF(N7 .GT. 0) WRITE (N7,101) (YFUN(KA),KA=(IY-1)*6+1,IY*6)    PLO  590
  500   CONTINUE                                                        PLO  600
        IF(BI .EQ. 0.) GOTO 501                                         PLO  610
        IF(N7 .GT. 0) WRITE (N7,101) (YFUN(LA),LA=IDIF*6+1,IZAEHL)      PLO  620
  501   CONTINUE                                                        PLO  630
        IF(N8 .EQ. 0 .OR. K .EQ. 0) GOTO 1000                           PLO  640
        DO 600 IE=1,IZAEHL                                              PLO  650
          WRITE (N8,101) YFUN(IE),ZC(NMAX)-XFUN(IE)                     PLO  660
  600   CONTINUE                                                        PLO  670
        WRITE (N8,102) BLANC                                            PLO  680
 1000 CONTINUE                                                          PLO  690
      RETURN                                                            PLO  700
      END                                                               PLO  710
      SUBROUTINE NUMINTB(CNUMI,X1,IANZ)                                 NUM   10
C                                                                       NUM   20
C     SUBROUTINE NUMINTB : NUMERISCHE INTEGRATION                       NUM   30
C                                                                       NUM   40
      IMPLICIT REAL*8 (A-H,O-Z)                                         NUM   50
C                                                                       NUM   60
      REAL UU,VV,ZGROB(100),POGROB(100),XWE(0:15,15),YWE(0:15,15),X1    NUM   70
C                                                                       NUM   80
      COMMON /FORALB/ UU,VV,XWE,YWE,IJR(0:15),N5,N6,N7,IJ               NUM   90
C                                                                       NUM  100
      DIMENSION Y(5),X(6),IK(4)                                         NUM  110
C                                                                       NUM  120
   12 FORMAT (' MORE THAN 600 SIMPSON-ITERATIONS FOR ONE INTEGRAL')     NUM  130
C                                                                       NUM  140
C     NUMERISCHE INTEGRATION                                            NUM  150
C                                                                       NUM  160
      DO 101 I=1,IJ                                                     NUM  170
        ZGROB(I) = XWE(IANZ,I)                                          NUM  180
        POGROB(I) = YWE(IANZ,I)                                         NUM  190
  101 CONTINUE                                                          NUM  200
      CNUMI = 0.D0                                                      NUM  210
      L = 1                                                             NUM  220
      X(6) = DBLE(X1)                                                   NUM  230
      X(1) = DBLE(ZGROB(1))                                             NUM  240
      FSIMP = 0.                                                        NUM  250
      EPSI1 = 1.D-6                                                     NUM  260
      EPSI2 = EPSI1  * 1.0D-2                                           NUM  270
      SCHRIT = 0.2 * (X(6)-X(1))                                        NUM  280
      DX1 = 0.                                                          NUM  290
      DO 2 I=1,4                                                        NUM  300
        IK(I) = 0                                                       NUM  310
    2 CONTINUE                                                          NUM  320
    1 CONTINUE                                                          NUM  330
      FPROV = 0.                                                        NUM  340
C                                                                       NUM  350
      CALL SIMPB(X,Y,FSIMP,EPSI1,EPSI2,SCHRIT,L,IA,IS,DX1,IK,F1SIM,     NUM  360
     1 FPROV)                                                           NUM  370
C                                                                       NUM  380
      GOTO(10,3),L                                                      NUM  390
    3 CONTINUE                                                          NUM  400
C                                                                       NUM  410
C     BERECHNUNG DES INTEGRANDEN                                        NUM  420
C                                                                       NUM  430
      DO 9 I=IA,5,IS                                                    NUM  440
        UU = SNGL(X(I))                                                 NUM  450
C                                                                       NUM  460
        CALL LAGRASB(ZGROB,POGROB)                                      NUM  470
C                                                                       NUM  480
        Y(I) = DBLE(VV)**2.                                             NUM  490
    9 CONTINUE                                                          NUM  500
      IK(4) = IK(4) + 1                                                 NUM  510
      IF(IK(4)-600) 1,1,11                                              NUM  520
   10 CNUMI = FSIMP                                                     NUM  530
      RETURN                                                            NUM  540
   11 WRITE (N6,12)                                                     NUM  550
      RETURN                                                            NUM  560
      END                                                               NUM  570
      SUBROUTINE NUMIND(CNUMI,X1,IANZ)                                  UMI   10
C                                                                       UMI   20
C     SUBROUTINE NUMIND : NUMERISCHE INTEGRATION                        UMI   30
C     (INTERPOLATIONSWERTE DOPPELT GENAU)                               UMI   40
C                                                                       UMI   50
      IMPLICIT REAL*8 (A-H,O-Z)                                         UMI   60
C                                                                       UMI   70
      REAL US,VS,XWE(0:15,15),YWE(0:15,15)                              UMI   80
C                                                                       UMI   90
      COMMON /FORALB/ US,VS,XWE,YWE,IJR(0:15),N5,N6,N7,IJ               UMI  100
C                                                                       UMI  110
      COMMON /UVB/ UU,VV                                                UMI  120
C                                                                       UMI  130
      DIMENSION ZGROB(100),POGROB(100),Y(5),X(6),IK(4)                  UMI  140
C                                                                       UMI  150
   12 FORMAT (' MORE THAN 600 SIMPSON-ITERATIONS FOR ONE INTEGRAL')     UMI  160
C                                                                       UMI  170
C     NUMERISCHE INTEGRATION                                            UMI  180
C                                                                       UMI  190
      DO 101 I=1,IJ                                                     UMI  200
        ZGROB(I) = DBLE(XWE(IANZ,I))                                    UMI  210
        POGROB(I) = DBLE(YWE(IANZ,I))                                   UMI  220
  101 CONTINUE                                                          UMI  230
      CNUMI = 0.D0                                                      UMI  240
      L = 1                                                             UMI  250
      X(6) = X1                                                         UMI  260
      X(1) = ZGROB(1)                                                   UMI  270
      FSIMP = 0.                                                        UMI  280
      EPSI1 = 1.D-8                                                     UMI  290
      EPSI2 = EPSI1 * 1.0D-2                                            UMI  300
      SCHRIT = 0.2 * (X(6)-X(1))                                        UMI  310
      DX1 = 0.                                                          UMI  320
      DO 2 I=1,4                                                        UMI  330
        IK(I) = 0                                                       UMI  340
    2 CONTINUE                                                          UMI  350
    1 CONTINUE                                                          UMI  360
      FPROV = 0.                                                        UMI  370
C                                                                       UMI  380
      CALL SIMPB(X,Y,FSIMP,EPSI1,EPSI2,SCHRIT,L,IA,IS,DX1,IK,F1SIM,     UMI  390
     1 FPROV)                                                           UMI  400
C                                                                       UMI  410
      GOTO(10,3),L                                                      UMI  420
    3 CONTINUE                                                          UMI  430
C                                                                       UMI  440
C     BERECHNUNG DES INTEGRANDEN                                        UMI  450
C                                                                       UMI  460
      DO 9 I=IA,5,IS                                                    UMI  470
        UU = X(I)                                                       UMI  480
C                                                                       UMI  490
        CALL LAGRAD(ZGROB,POGROB)                                       UMI  500
C                                                                       UMI  510
        Y(I) = VV**2.                                                   UMI  520
    9 CONTINUE                                                          UMI  530
      IK(4) = IK(4) + 1                                                 UMI  540
      IF(IK(4)-600) 1,1,11                                              UMI  550
   10 CNUMI = FSIMP                                                     UMI  560
      RETURN                                                            UMI  570
   11 WRITE (N6,12)                                                     UMI  580
      RETURN                                                            UMI  590
      END                                                               UMI  600
      SUBROUTINE SIMPB(X,Y,FSIMP,EPSI1,EPSI2,SCHRIT,L,IA,IS,DX1,IK,F1SIMSIM   10
     1 ,FPROV)                                                          SIM   20
C                                                                       SIM   30
C     SIMPSON INTEGRATION                                               SIM   40
C                                                                       SIM   50
C     X      = STUETZPUNKT                                              SIM   60
C     Y      = ZUGEHOERIGER FUNKTIONSWERT                               SIM   70
C     SCHRIT = SCHRITTWEITE                                             SIM   80
C     EPSI1  = MAX. RELATIVER FEHLER                                    SIM   90
C     EPSI2  = MIN. RELATIVER FEHLER                                    SIM  100
C     FSIMP  = INTEGRALWERT MIT SCHRITTWEITE DX/2                       SIM  110
C                                                                       SIM  120
      IMPLICIT REAL*8 (A-H,O-Z)                                         SIM  130
C                                                                       SIM  140
      REAL UU,VV,XWE(0:15,15),YWE(0:15,15)                              SIM  150
C                                                                       SIM  160
      COMMON /FORALB/ UU,VV,XWE,YWE,IJR(0:15),N5,N6,N7,IJ               SIM  170
C                                                                       SIM  180
CFZJ055                                                       25.09.07  SIM  190
C                                                                       SIM  200
      DIMENSION X(6),Y(5),IK(4)                                         SIM  210
C                                                                       SIM  220
   31 FORMAT (' MAGNIFICATION OF ITERATION-STEP IN SIMP-INTEGRATION DX='SIM  230
     1 ,D12.6,', EPSI1=',D12.6,' BY FACTOR 10')                         SIM  240
C                                                                       SIM  250
C                                                                       SIM  260
      DX = DX1                                                          SIM  270
      MM = IK(1)                                                        SIM  280
      KSI = IK(2)                                                       SIM  290
      LAUF = IK(3)                                                      SIM  300
      K = KSI                                                           SIM  310
      GOTO(111,2),L                                                     SIM  320
  111 L = 2                                                             SIM  330
      K = 1                                                             SIM  340
      LAUF = 1                                                          SIM  350
      FSIMP = 0.                                                        SIM  360
      F1SIM = 0.                                                        SIM  370
      DX = SCHRIT                                                       SIM  380
      MM = 1                                                            SIM  390
      VFAKT = 1.                                                        SIM  400
   14 GOTO(114,214),MM                                                  SIM  410
  214 MM = 1                                                            SIM  420
      EPSI1 = EPSI1 / VFAKT                                             SIM  430
      EPSI2 = EPSI2 / VFAKT                                             SIM  440
      VFAKT = 1.                                                        SIM  450
  114 IA = 1                                                            SIM  460
      IS = 1                                                            SIM  470
      DO 5 I=1,2                                                        SIM  480
        X(I+1) = X(I) + DX                                              SIM  490
        X(I+3) = X(I) + DX / 2.                                         SIM  500
    5 CONTINUE                                                          SIM  510
   88 KSI = K                                                           SIM  520
      DX1 = DX                                                          SIM  530
      IK(1) = MM                                                        SIM  540
      IK(2) = KSI                                                       SIM  550
      IK(3) = LAUF                                                      SIM  560
      IK(4) = IK(4) + 1                                                 SIM  570
      IF(IK(4) .LE. 1) DX0 = X(6) - X(1)                                SIM  580
      RETURN                                                            SIM  590
    2 D1 = DX / 3.                                                      SIM  600
      D2 = D1 / 2.                                                      SIM  610
      A1 = Y(1) + 4. * Y(2) + Y(3)                                      SIM  620
      A2 = A1 - Y(2) - Y(2) + 4. * (Y(4)+Y(5))                          SIM  630
      F1 = D1 * A1                                                      SIM  640
      F2 = D2 * A2                                                      SIM  650
      ABW = 0.                                                          SIM  660
      IF(F1 .EQ. F2) GOTO 30                                            SIM  670
      F12 = DMAX1(DABS(F1),DABS(F2),DABS(F2+FSIMP),1.0D-7,DABS(FPROV*DX)SIM  680
     1 )                                                                SIM  690
      ABW = DABS((F1-F2)/F12)                                           SIM  700
   30 CONTINUE                                                          SIM  710
      IF(ABW-EPSI1) 10,11,11                                            SIM  720
   11 DX = DX / 2.                                                      SIM  730
      IF(DABS(DX)/2. .GT. 1.0D-11) GOTO 113                             SIM  740
      DX = DX * 2.                                                      SIM  750
      WRITE (N6,31) DX,EPSI1                                            SIM  760
      EPSI1 = EPSI1 * 10.                                               SIM  770
      EPSI2 = EPSI2 * 10.                                               SIM  780
      MM = 2                                                            SIM  790
      VFAKT = VFAKT * 10.                                               SIM  800
      GOTO 30                                                           SIM  810
  113 K = 2                                                             SIM  820
      X(3) = X(2)                                                       SIM  830
      Y(3) = Y(2)                                                       SIM  840
      X(2) = X(4)                                                       SIM  850
      Y(2) = Y(4)                                                       SIM  860
      X(4) = X(1) + DX / 2.                                             SIM  870
      X(5) = X(2) + DX / 2.                                             SIM  880
      IA = 4                                                            SIM  890
      IS = 1                                                            SIM  900
      GOTO 88                                                           SIM  910
   10 IF(ABW-EPSI2) 12,12,13                                            SIM  920
   12 GOTO(1,13),K                                                      SIM  930
    1 DX = 2. * DX                                                      SIM  940
      IF((X(6)-X(3))/(DX*(1.+1.0D-14)) .LE. 1.0)  GOTO 9                SIM  950
      X(4) = X(2)                                                       SIM  960
      Y(4) = Y(2)                                                       SIM  970
      X(2) = X(3)                                                       SIM  980
      Y(2) = Y(3)                                                       SIM  990
      X(3) = X(2) + DX                                                  SIM 1000
      X(5) = X(2) + DX / 2.                                             SIM 1010
      IA = 3                                                            SIM 1020
      IS = 2                                                            SIM 1030
      GOTO 88                                                           SIM 1040
    9 K = 2                                                             SIM 1050
      DX = (X(6)-X(1)) / 2.                                             SIM 1060
      GOTO 14                                                           SIM 1070
   13 K = 1                                                             SIM 1080
      GOTO(130,230),LAUF                                                SIM 1090
  130 LAUF = 2                                                          SIM 1100
      SCHRIT = DX                                                       SIM 1110
  230 FSIMP = FSIMP + F2                                                SIM 1120
      F1SIM = F1SIM + F1                                                SIM 1130
      X(1) = X(3)                                                       SIM 1140
      R = X(6) - X(1)                                                   SIM 1150
      IF(R/(2.0*DX) .GT. 1.0) GOTO 14                                   SIM 1160
      RREL = DABS(R/DX0)                                                SIM 1170
      IF(RREL .LT. 1.0D-14) GOTO 17                                     SIM 1180
      DX = R / 2.                                                       SIM 1190
      GOTO 14                                                           SIM 1200
   17 L = 1                                                             SIM 1210
      GOTO 88                                                           SIM 1220
      END                                                               SIM 1230
      SUBROUTINE SUB(FLAG,V0)                                           SUB   10
C                                                                       SUB   20
C     SUBROUTINE SUB : VERGLEICHT DIE BERECHNETEN VOLUMEN DER KANAELE   SUB   30
C                      DEN ORGINALVOLUMEN UND AENDERT ENTSPRECHEND DIE  SUB   40
C                      VORGEGEBENEN STUETZSTELLEN                       SUB   50
C                                                                       SUB   60
      REAL VS(15),VOLU(15),VEKA(0:15),WEKA(0:15)                        SUB   70
C                                                                       SUB   80
      COMMON /VOLUB/ VGE,VOLU,VEKA,XPRUEF,IX,WEKA                       SUB   90
C                                                                       SUB  100
      COMMON /KONSB/ PI,EPSY                                            SUB  110
C                                                                       SUB  120
C                                                                       SUB  130
      IF(WEKA(IX) .LE. 0.) GOTO 1                                       SUB  140
      VS(IX) = VEKA(IX) * VGE + V0                                      SUB  150
      XPRUEF = VS(IX) / VOLU(IX)                                        SUB  160
      IF((ABS(XPRUEF-1.)) .GT. EPSY) GOTO 2                             SUB  170
    1 FLAG = 2.                                                         SUB  180
C                                                                       SUB  190
C     GEHE ZUM NAECHSTEN KANAL                                          SUB  200
C                                                                       SUB  210
      IX = IX + 1                                                       SUB  220
      VEKA(IX) = VEKA(IX) + VEKA(IX-1)                                  SUB  230
    2 CONTINUE                                                          SUB  240
      RETURN                                                            SUB  250
      END                                                               SUB  260
      SUBROUTINE KURVE(IANZ,V0)                                         KUR   10
C                                                                       KUR   20
C     SUBROUTINE KURVE : GLEICHT DIE EINGELESENEN STUETZSTELLEN SO AN,  KUR   30
C     DASS DIE VOLUMEN RICHTIG SIND                                     KUR   40
C                                                                       KUR   50
      REAL XWE(0:15,15),YWE(0:15,15),VOLU(15),VEKA(0:15),WEKA(0:15)     KUR   60
C                                                                       KUR   70
      REAL*8 CNUMI                                                      KUR   80
C                                                                       KUR   90
      INTEGER KANDEF(20)                                                KUR  100
C                                                                       KUR  110
      COMMON /FORALB/ UU,VV,XWE,YWE,IJR(0:15),N5,N6,N7,IJ               KUR  120
C                                                                       KUR  130
      COMMON /CORB/ HCORE,KAN(15),NKURVE,H4,R1,H3,R2,KANAL              KUR  140
C                                                                       KUR  150
      COMMON /VOLUB/ VGE,VOLU,VEKA,XPRUEF,IX,WEKA,KANDEF                KUR  160
C                                                                       KUR  170
      COMMON /KONSB/ PI,EPSY                                            KUR  180
C                                                                       KUR  190
C                                                                       KUR  200
      FLAG = 1.                                                         KUR  210
  111 CONTINUE                                                          KUR  220
      IJ = IJ                                                           KUR  230
      DO 200 I=1,IJ                                                     KUR  240
        YWE(IANZ,I) = YWE(IANZ,I) * SQRT(XPRUEF)                        KUR  250
  200 CONTINUE                                                          KUR  260
      IF(IANZ .NE. 1) GOTO 202                                          KUR  270
      DO 201 I=1,KANAL                                                  KUR  280
        WEKA(I) = VEKA(I)                                               KUR  290
  201 CONTINUE                                                          KUR  300
  202 CONTINUE                                                          KUR  310
      X1 = XWE(IANZ,IJ)                                                 KUR  320
C                                                                       KUR  330
      CALL NUMINTB(CNUMI,X1,IANZ)                                       KUR  340
C                                                                       KUR  350
      VOLU(IANZ) = SNGL(CNUMI) * PI                                     KUR  360
      VEK = 0.                                                          KUR  370
      IF(IANZ .GT. 1) VEK = VEKA(IANZ-1)                                KUR  380
      IF(KANDEF(IANZ) .EQ. 1) VEKA(IANZ) = (VOLU(IANZ)-VEK) / VGE       KUR  390
C                                                                       KUR  400
      CALL SUB(FLAG,V0)                                                 KUR  410
C                                                                       KUR  420
      IF(FLAG.LE.1.) GOTO 111                                           KUR  430
      XPRUEF = 1.                                                       KUR  440
      RETURN                                                            KUR  450
      END                                                               KUR  460
      SUBROUTINE VOLT                                                   VOL   10
C                                                                       VOL   20
C     SUBROUTINE VOLT: BERECHNET DIE TEILVOLUMEN DER EINZELNEN KANALE   VOL   30
C                      MIT DEM UNTERPROGRAMM NUMINT                     VOL   40
C                                                                       VOL   50
      REAL ZGROB(100),POGROB(100),XWE(0:15,15),YWE(0:15,15),VOLU(15),   VOL   60
     1 VEKA(0:15),RC(0:100),ZC(0:200),YYY(1515),YYS(100),PGROB2(100),   VOL   70
     2 ZKOT(15,101),WEKA(0:15),DGR(50),DGZ(100)                         VOL   80
C                                                                       VOL   90
      INTEGER IOP(100),NOP(200),KANTYP(15)                              VOL  100
C                                                                       VOL  110
      REAL*8 CNUMI,DY1S,DX0,DY0,DX1,DY1,DVERG,DYY1,DDY10,DX2,DX3,DABC,  VOL  120
     1 VLU,VOLUT(15),VOLD(15),DDX10                                     VOL  130
C                                                                       VOL  140
      COMMON /FORALB/ UU,VV,XWE,YWE,IJR(0:15),N5,N6,N7,IJ,N8            VOL  150
C                                                                       VOL  160
      COMMON /CORB/ HCORE,KAN(15),NKURVE,H4,R1,H3,R2,KANAL              VOL  170
C                                                                       VOL  180
      COMMON /GEOB/ MR(100),MZ(200),LAYVC(100,200),MGR(50),MGZ(100),    VOL  190
     1 ICOR(100),LAYC(100,200),JCOR(15),JI,JA,JI0,RKONUS,ZKONUS,RC,ZC,  VOL  200
     2 NCOR1,NCOR2,IMA                                                  VOL  210
C                                                                       VOL  220
      COMMON /VOLUB/ VGE,VOLU,VEKA,XPRUEF,IX,WEKA                       VOL  230
C                                                                       VOL  240
      COMMON /KONSB/ PI,EPSY,YYY                                        VOL  250
C                                                                       VOL  260
      COMMON /GTGRUB/ ZKOT                                              VOL  270
C                                                                       VOL  280
      COMMON /DB/ DELLAY(15),DLAY(15,101)                               VOL  290
C                                                                       VOL  300
      COMMON /NEB/ IOP,NOP,KANTYP,NCASE,RINTHX,DGR,DGZ,KONUS,KANALC,    VOL  310
     1 NMAXC                                                            VOL  320
C                                                                       VOL  330
      COMMON /MAXIB/ IMAX,NMAX                                          VOL  340
C                                                                       VOL  350
      COMMON /UB/ DUM(10),IYS                                           VOL  360
C                                                                       VOL  370
      CHARACTER*11 BLANC/'           '/                                 VOL  380
C                                                                       VOL  390
   12 FORMAT (1X,I3,6X,I3,4X,F10.3,G12.5,G14.4)                         VOL  400
   13 FORMAT (56X,E13.5)                                                VOL  410
   15 FORMAT (5X,'TOTAL VOLUME OF CORE =',E12.5)                        VOL  420
  122 FORMAT (2E12.5)                                                   VOL  430
  125 FORMAT (E12.5,1X,A11,E12.5)                                       VOL  440
  201 FORMAT (///' CHANNEL  REGION  BOT.LIMIT     VOLUME       ACCURACY VOL  450
     1    VOLUME OF CHANNEL'/'                     CM          CM**3'/) VOL  460
  202 FORMAT (A11)                                                      VOL  470
C                                                                       VOL  480
C                                                                       VOL  490
      DO 1 I=1,1515                                                     VOL  500
        IF(I .LE. 100) YYS(I) = 0.                                      VOL  510
        YYY(I) = 0.                                                     VOL  520
    1 CONTINUE                                                          VOL  530
      IY = 0                                                            VOL  540
      YCS = 0.                                                          VOL  550
      WRITE (N6,201)                                                    VOL  560
      IC = 0                                                            VOL  570
      ICO = 0                                                           VOL  580
      IK = 1                                                            VOL  590
      DO 1000 IANZ=1,KANAL                                              VOL  600
        IF(DELLAY(IANZ) .GE. 0.) GOTO 73                                VOL  610
        DO 72 I=1,KAN(IANZ)                                             VOL  620
          DLAY(IANZ,I) = DLAY(IANZ-1,I)                                 VOL  630
   72   CONTINUE                                                        VOL  640
   73   CONTINUE                                                        VOL  650
        DY1S = 0.D0                                                     VOL  660
        Y2S = 0.                                                        VOL  670
        DO 20 IXT=1,KAN(IANZ)                                           VOL  680
          IZ = 0                                                        VOL  690
          VOLD(IANZ) = DBLE(VOLU(IANZ))                                 VOL  700
          VLU = VOLD(IANZ) / (KAN(IANZ)*DBLE(PI))                       VOL  710
          VOLUT(IANZ) = VLU * IXT                                       VOL  720
          DX0 = DBLE(ZC(NCOR1-1))                                       VOL  730
          DY0 = 0.D0                                                    VOL  740
          DX1 = DBLE(HCORE) / KAN(IANZ) * IXT + DX0                     VOL  750
   10     CONTINUE                                                      VOL  760
          IZ = IZ + 1                                                   VOL  770
          IJ = IJR(IANZ)                                                VOL  780
C                                                                       VOL  790
          CALL NUMIND(CNUMI,DX1,IANZ)                                   VOL  800
C                                                                       VOL  810
          DY1 = CNUMI                                                   VOL  820
          DVERG = DY1                                                   VOL  830
          IJ = IJR(IANZ-1)                                              VOL  840
C                                                                       VOL  850
          CALL NUMIND(CNUMI,DX1,IANZ-1)                                 VOL  860
C                                                                       VOL  870
          DY1 = DVERG - CNUMI                                           VOL  880
          DYY1 = (DY1-DY1S) * DBLE(PI)                                  VOL  890
          DABC = (VOLUT(IANZ)-DY1) / VLU                                VOL  900
          IF(DABS(DABC) .LE. DBLE(EPSY)) GOTO 30                        VOL  910
          DDY10 = DY1 - DY0                                             VOL  920
          IF(DDY10 .NE. 0.D0) GOTO 11                                   VOL  930
          WRITE (N6,'(A)') ' NO MORE CONVERGENCE IN CALCULATION IN THIS VOL  940
     1PART OF VOLUME'                                                   VOL  950
          GOTO 30                                                       VOL  960
   11     CONTINUE                                                      VOL  970
          IF(IZ .LT. 100) GOTO 14                                       VOL  980
          WRITE (N6,'(A)') ' NO MORE CONVERGENCE IN CALCULATION IN THIS VOL  990
     1PART OF VOLUME AFTER 100 ITERATIONS'                              VOL 1000
          GOTO 30                                                       VOL 1010
   14     CONTINUE                                                      VOL 1020
          DDX10 = DX1 - DX0                                             VOL 1030
          DX2 = DX1 + (DDX10/DDY10) * (VOLUT(IANZ)-DY1)                 VOL 1040
          DX3 = DBLE(ZC(NCOR2)) * (1.D0+1.D-2/IZ)                       VOL 1050
          DX2 = DMIN1(DX2,DX3)                                          VOL 1060
          DX0 = DX1                                                     VOL 1070
          DX1 = DX2                                                     VOL 1080
          DY0 = DY1                                                     VOL 1090
          GOTO 10                                                       VOL 1100
   30     CONTINUE                                                      VOL 1110
          X1 = SNGL(DX1)                                                VOL 1120
          YY1 = SNGL(DYY1)                                              VOL 1130
          ABC = SNGL(DABC)                                              VOL 1140
          YPU1 = R1                                                     VOL 1150
          IJ = IJR(IANZ)                                                VOL 1160
          DO 55 I=1,IJ                                                  VOL 1170
            ZGROB(I) = XWE(IANZ,I)                                      VOL 1180
            POGROB(I) = YWE(IANZ,I)                                     VOL 1190
   55     CONTINUE                                                      VOL 1200
          UU = X1                                                       VOL 1210
C                                                                       VOL 1220
          CALL LAGRASB(ZGROB,POGROB)                                    VOL 1230
C                                                                       VOL 1240
          YPU1 = VV                                                     VOL 1250
          IJ = IJR(IANZ-1)                                              VOL 1260
          DO 66 I=1,IJ                                                  VOL 1270
            PGROB2(I) = YWE(IANZ-1,I)                                   VOL 1280
            ZGROB(I) = XWE(IANZ-1,I)                                    VOL 1290
   66     CONTINUE                                                      VOL 1300
          UU = X1                                                       VOL 1310
C                                                                       VOL 1320
          CALL LAGRASB(ZGROB,PGROB2)                                    VOL 1330
C                                                                       VOL 1340
          YPU2 = VV                                                     VOL 1350
          IF(IXT .EQ. KAN(IANZ)) X1 = ZC(NCOR2)                         VOL 1360
          ZKOT(IANZ,IXT) = X1                                           VOL 1370
          IF(DELLAY(IANZ) .EQ. 0.) GOTO 75                              VOL 1380
          VHC = DLAY(IANZ,IXT) / HCORE                                  VOL 1390
          YY1 = VOLU(IANZ) * VHC                                        VOL 1400
          ABC = 0.                                                      VOL 1410
          ZKOT(IANZ,IXT) = 0.                                           VOL 1420
          IF(IXT .NE. 1) GOTO 76                                        VOL 1430
          ZKOT(IANZ,IXT) = DLAY(IANZ,IXT) + DX0                         VOL 1440
          GOTO 77                                                       VOL 1450
   76     CONTINUE                                                      VOL 1460
          ZKOT(IANZ,IXT) = ZKOT(IANZ,IXT-1) + DLAY(IANZ,IXT)            VOL 1470
   77     X1 = ZKOT(IANZ,IXT)                                           VOL 1480
   75     CONTINUE                                                      VOL 1490
          IY = IY + 1                                                   VOL 1500
          YYY(IY) = YY1                                                 VOL 1510
          WRITE (N6,12) IANZ,IXT,ZKOT(IANZ,IXT),YY1,ABC                 VOL 1520
          IF(N7 .GT. 0) WRITE (N7,122) X1,X1                            VOL 1530
          IF(N7 .GT. 0) WRITE (N7,122) YPU1,YPU2                        VOL 1540
          IF(N8 .EQ. 0) GOTO 123                                        VOL 1550
          WRITE (N8,125) YPU1,BLANC,ZC(NMAX)-X1                         VOL 1560
          WRITE (N8,125) YPU2,BLANC,ZC(NMAX)-X1                         VOL 1570
          WRITE (N8,202) BLANC                                          VOL 1580
  123     CONTINUE                                                      VOL 1590
          DY1S = DY1                                                    VOL 1600
          Y2S = Y2S + YY1                                               VOL 1610
   20   CONTINUE                                                        VOL 1620
        WRITE (N6,13) Y2S                                               VOL 1630
        IF(WEKA(IANZ) .GT. 0.) GOTO 3                                   VOL 1640
        IF(ICO .GT. 0) GOTO 1000                                        VOL 1650
        DO 2 II=IK,IY                                                   VOL 1660
          IC = IC + 1                                                   VOL 1670
          YYS(IC) = YYY(II)                                             VOL 1680
    2   CONTINUE                                                        VOL 1690
        IK = IY + 1                                                     VOL 1700
        GOTO 999                                                        VOL 1710
    3   CONTINUE                                                        VOL 1720
        YCS = YCS + Y2S                                                 VOL 1730
        ICO = 1                                                         VOL 1740
  999   CONTINUE                                                        VOL 1750
        IYS = IY                                                        VOL 1760
 1000 CONTINUE                                                          VOL 1770
      IF(IC .LE. 0) GOTO 6                                              VOL 1780
      IY = IYS - IC                                                     VOL 1790
      DO 4 I=1,IY                                                       VOL 1800
        YYY(I) = YYY(I+IC)                                              VOL 1810
    4 CONTINUE                                                          VOL 1820
      DO 5 I=1,IC                                                       VOL 1830
        YYY(IY+I) = YYS(I)                                              VOL 1840
    5 CONTINUE                                                          VOL 1850
    6 CONTINUE                                                          VOL 1860
      WRITE (N6,15) YCS                                                 VOL 1870
      IF(NCOR1-1 .EQ. 0) GOTO 1001                                      VOL 1880
      IF(N7 .GT. 0) WRITE (N7,122) ZC(NCOR1-1),ZC(NCOR1-1)              VOL 1890
      IF(N7 .GT. 0) WRITE (N7,122) RC(JI0-1),RC(IMA)                    VOL 1900
      IF(N8 .EQ. 0) GOTO 1001                                           VOL 1910
      WRITE (N8,125) RC(JI0-1),BLANC,ZC(NMAX)-ZC(NCOR1-1)               VOL 1920
      WRITE (N8,125) RC(IMA),BLANC,ZC(NMAX)-ZC(NCOR1-1)                 VOL 1930
      WRITE (N8,202) BLANC                                              VOL 1940
 1001 CONTINUE                                                          VOL 1950
      RETURN                                                            VOL 1960
      END                                                               VOL 1970
      SUBROUTINE ROTGIT                                                 ROT   10
C                                                                       ROT   20
C     SUBROUTINE ROTGIT: ERSTELLT EIN GROBES GITTER, DAS DEN FLIESSKUR- ROT   30
C                        VEN UEBERLAGERT WIRD                           ROT   40
C                                                                       ROT   50
      REAL RC(0:100),ZC(0:200),ZROT(0:100),RROT(0:50),XWE(0:15,15),     ROT   60
     1 YWE(0:15,15)                                                     ROT   70
C                                                                       ROT   80
      COMMON /FORALB/ UU,VV,XWE,YWE,IJR(0:15),N5,N6,N7,IJ,N8            ROT   90
C                                                                       ROT  100
      COMMON /GEOB/ MR(100),MZ(200),LAYVC(100,200),MGR(50),MGZ(100),    ROT  110
     1 ICOR(100),LAYC(100,200),JCOR(15),JI,JA,JI0,RKONUS,ZKONUS,RC,ZC,  ROT  120
     2 NCOR1,NCOR2,IMA                                                  ROT  130
C                                                                       ROT  140
      COMMON /GITROB/ JZROT,IRROT,ZROT,RROT                             ROT  150
C                                                                       ROT  160
      COMMON /MAXIB/ IMAX,NMAX                                          ROT  170
C                                                                       ROT  180
      CHARACTER*11 BLANC/'           '/                                 ROT  190
C                                                                       ROT  200
  120 FORMAT (2E12.5)                                                   ROT  210
  121 FORMAT (A11)                                                      ROT  220
  125 FORMAT (E12.5,2(1X,A11),E12.5)                                    ROT  230
C                                                                       ROT  240
C                                                                       ROT  250
      NCO1 = NCOR1 - 1                                                  ROT  260
      JZROT = NCOR2 - NCO1                                              ROT  270
      ICO1 = JI0 - 1                                                    ROT  280
      IRROT = IMA - ICO1                                                ROT  290
      DO 10 J=0,JZROT                                                   ROT  300
        ZROT(J)= ZC(J+NCO1)                                             ROT  310
        IF(N7 .GT. 0) WRITE (N7,120) ZROT(J),ZROT(J)                    ROT  320
        IF(N7 .GT. 0) WRITE (N7,120) RC(ICO1),RC(IMA)                   ROT  330
        IF(N8 .EQ. 0) GOTO 10                                           ROT  340
        WRITE (N8,125) RC(ICO1),BLANC,BLANC,ZC(NMAX)-ZROT(J)            ROT  350
        WRITE (N8,125) RC(IMA),BLANC,BLANC,ZC(NMAX)-ZROT(J)             ROT  360
        WRITE (N8,121) BLANC                                            ROT  370
   10 CONTINUE                                                          ROT  380
      DO 20 I=0,IRROT                                                   ROT  390
        RROT(I)= RC(I+ICO1)                                             ROT  400
        IF(N7 .GT. 0) WRITE (N7,120) ZC(NCO1),ZC(NCOR2)                 ROT  410
        IF(N7 .GT. 0) WRITE (N7,120) RROT(I),RROT(I)                    ROT  420
        IF(N8 .EQ. 0) GOTO 20                                           ROT  430
        WRITE (N8,125) RROT(I),BLANC,BLANC,ZC(NMAX)-ZC(NCO1)            ROT  440
        WRITE (N8,125) RROT(I),BLANC,BLANC,ZC(NMAX)-ZC(NCOR2)           ROT  450
        WRITE (N8,121) BLANC                                            ROT  460
   20 CONTINUE                                                          ROT  470
      END                                                               ROT  480
      SUBROUTINE GRUGIT(GRZKOT,GRRKOT,LC)                               GRU   10
C                                                                       GRU   20
C     SUBROUTINE GRUGIT: ERSTELLT EIN FEINES GITTER, DAS DEN FLIESSKUR- GRU   30
C                        VEN UEBERLAGERT WIRD                           GRU   40
C                                                                       GRU   50
C                                                                       GRU   60
C     V A R I A B L E N B E S C H R E I B U N G :                       GRU   70
C                                                                       GRU   80
C     GRZKOT : Z-KOORDINATE DES FEINEN GITTERS(MITTELPUNKT)             GRU   90
C     GRRKOT : R-KOORDINATE DES FEINEN GITTERS(MITTELPUNKT)             GRU  100
C     RKOSCH : R-KOORDINATE DES ELEMENTES DER BETR. FLIESSKURVE         GRU  110
C     V      : MATRIX AUF DER DIE HAEUFIGKEIT DER FEINEN ELEMENTE IN DENGRU  120
C              SCHWARZEN BZW ROTEN GITTERELEMENTEN STEHT                GRU  130
C     KHILF  : HILFSGROESSE AUF DER DIE AKTUELLE ANZAHL DER ELEMENTE DERGRU  140
C              FLIESSKURVEN STEHT                                       GRU  150
C     KANALS : NUMMER DES SCHWARZEN ELEMENTES IN DEM EIN ELEMENT DES    GRU  160
C              FEINEN GITTERS LIEGT                                     GRU  170
C     KANALR : NUMMER DES ROTEN ELEMENTES IN DEM EIN ELEMENT DES FEINEN GRU  180
C              GITTERS LIEGT                                            GRU  190
C                                                                       GRU  200
C                                                                       GRU  210
CFZJ048 enlarged dimension                                    11.04.07  GRU  220
      REAL ZGROB(100),POGROB(100),XWE(0:15,15),YWE(0:15,15),RKOSCH(0:15)GRU  230
     1 ,RC(0:100),VFV(4000),VFC(4000),ZC(0:200),ZKOT(15,101),ZROT(0:100)GRU  240
     2 ,RROT(0:50),VCI(2000),ZPL(0:100),GRZKOT(IZFEIN),GRRKOT(JRFEIN),  GRU  250
     3 YYY(1515),VOLREG(1515),VFT(4000),VT(0:4000),VS(0:1515),DDYY(1515)GRU  260
     4 ,RF(0:200),ZF(0:400)                                             GRU  270
C                                                                       GRU  280
CFZJ048 enlarged dimension                                    11.04.07  GRU  290
      INTEGER KHILF(15),XJV(4000),XIV(4000),VVOL(4000),MT(4000),NT(1515)GRU  300
     1 ,ICOPL(20),KLAY(1515),V,NOPOWR(1515),NOPOWK(4000)                GRU  310
C                                                                       GRU  320
      COMMON /FORALB/ UU,VV,XWE,YWE,IJR(0:15),N5,N6,N7,IJ,N8            GRU  330
C                                                                       GRU  340
      COMMON /CORB/ HCORE,KAN(15),NKURVE,H4,R1,H3,R2,KANAL              GRU  350
C                                                                       GRU  360
      COMMON /GEOB/ MR(100),MZ(200),LAYVC(100,200),MGR(50),MGZ(100),    GRU  370
     1 ICOR(100),LAYC(100,200),JCOR(15),JI,JA,JI0,RKONUS,ZKONUS,RC,ZC,  GRU  380
     2 NCOR1,NCOR2,IMA                                                  GRU  390
C                                                                       GRU  400
      COMMON /KONSB/ PI,EPSY,YYY,IWRITE,ZPL,MDIREC,IPUT                 GRU  410
C                                                                       GRU  420
      COMMON /GTGRUB/ ZKOT                                              GRU  430
C                                                                       GRU  440
      COMMON /GITROB/ JZROT,IRROT,ZROT,RROT                             GRU  450
C                                                                       GRU  460
      COMMON /VSOB/ KLAY,IPL,NVSOP,ICOPL,VOLREG,N20,KSCH,IMX,XJV,XIV,VFVGRU  470
     1 ,VFC,KROT                                                        GRU  480
C                                                                       GRU  490
      COMMON /VARDIB/ IZFEIN,JRFEIN,EP                                  GRU  500
C                                                                       GRU  510
      COMMON /MAXIB/ IMAX,NMAX                                          GRU  520
C                                                                       GRU  530
      COMMON /TERMIB/ NTHX,VFT                                          GRU  540
C                                                                       GRU  550
CFZJ048 enlarged dimension                                    11.04.07  GRU  560
      COMMON /VB/ V(0:4000,0:1515)                                      GRU  570
C                                                                       GRU  580
      COMMON /UB/ DUM(11),L1,KANALI                                     GRU  590
C                                                                       GRU  600
      COMMON /NOPO/ NOPOWR,NOPOWK                                       GRU  610
C                                                                       GRU  620
      CHARACTER*11 BLANC/'           '/                                 GRU  630
C                                                                       GRU  640
      CHARACTER*1 CITCO(2)/'C',' '/                                     GRU  650
C                                                                       GRU  660
  101 FORMAT (///' TEST OUTPUT V(J,I):')                                GRU  670
  102 FORMAT (19('=')/)                                                 GRU  680
  103 FORMAT ('                                ---> I = 1,KSCH (VSOP)'/)GRU  690
  104 FORMAT (' J = 1,KROT (CITATION)'/' |'/' V')                       GRU  700
  111 FORMAT ('+',' ',I9,15I8/  100(' ',I10,15I8/))                     GRU  710
  112 FORMAT (/100(' ',I10,15I8/))                                      GRU  720
  550 FORMAT (2E12.5)                                                   GRU  730
  551 FORMAT (E12.5,3(1X,A11),E12.5)                                    GRU  740
  560 FORMAT (/' NUMBER OF COMPOSITION-PLOT-CURVES OF EXTERNAL PART AMOUGRU  750
     1NTS TO',I5)                                                       GRU  760
  570 FORMAT (/' NUMBER OF FINE-MESH PLOT-CURVES (OVER ALL) AMOUNTS TO',GRU  770
     1 I5)                                                              GRU  780
  580 FORMAT (A11)                                                      GRU  790
  710 FORMAT (///' DATA SET NO.',I4,' WRITTEN FOR V-MATRIX.')           GRU  800
  720 FORMAT (///' DATA SET NO.',I4,' READ FOR V-MATRIX.')              GRU  810
  771 FORMAT (I5,3X,I5,8X,I5,2X,A1,7X,I9,15X,E12.5,11X,E12.5)           GRU  820
  772 FORMAT (I5,3X,I5,8X,I5,10X,I9,15X,E12.5,11X,E12.5)                GRU  830
  912 FORMAT ('+',72X,'  |  IR =',I5,3X,I9,12X,E12.5)                   GRU  840
  915 FORMAT (' IS =',I5,3X,I9,12X,E12.5,E13.5,G13.5)                   GRU  850
  916 FORMAT (73X,'  |  IR =',I5,3X,I9,12X,E12.5)                       GRU  860
  917 FORMAT (///' VSOP - REGIONS:',62X,'CITATION- OR THERMIX - COMPOSITGRU  870
     1IONS:'/1X,15('='),62X,36('=')//'      REGION     NUMBER * MINI-VOLGRU  880
     2. = VOLUME    REF.-VOLUME    ACCURACY           COMP.      NUMBER GRU  890
     3* MINI-VOL. = VOLUME'/)                                           GRU  900
  918 FORMAT (///32X,'VOLUME-MATRIX ELEMENTS GREATER ZERO:'/32X,'=======GRU  910
     1============================='///'   NO. VSOP-REGION CIT/THX-COMP.GRU  920
     2  NUMBER OF MINI-VOL.   RELATIVE PORTION OF VOLUME-MATRIX ELEMENT GRU  930
     3IN'/'    I     XIV(I)      XJV(I)           VVOL(I)               GRU  940
     4VSOP-REGION           CIT/THX-COMP.'/)                            GRU  950
 1221 FORMAT (//' MINI-VOLUME: ',E12.5,'   TOTAL NUMBER:',I10,'   TOTAL GRU  960
     1VOLUME: ',E12.5)                                                  GRU  970
C                                                                       GRU  980
C     BERECHNUNG DER Z-KOORDINATEN DES FEINEN GITTERS                   GRU  990
C                                                                       GRU 1000
CFZJ048 enlarged dimension                                    11.04.07  GRU 1010
      DO 1 JM=0,1515                                                    GRU 1020
        VS(JM) = 0.                                                     GRU 1030
        DO 1 IM=0,4000                                                  GRU 1040
          V(IM,JM) = 0                                                  GRU 1050
          VT(IM) = 0.                                                   GRU 1060
    1 CONTINUE                                                          GRU 1070
      IF(IWRITE .LT. 0) GOTO 700                                        GRU 1080
      IF(MDIREC .EQ. 0) GOTO 99                                         GRU 1090
C                                                                       GRU 1100
      CALL DIRECT                                                       GRU 1110
C                                                                       GRU 1120
      GOTO 499                                                          GRU 1130
   99 CONTINUE                                                          GRU 1140
      DO 100 I=1,IZFEIN                                                 GRU 1150
        GRZKOT(I) = HCORE / (REAL(IZFEIN*2)) * (REAL(I*2)-EP) +         GRU 1160
     1   ZC(NCOR1-1)                                                    GRU 1170
  100 CONTINUE                                                          GRU 1180
C                                                                       GRU 1190
C     BERECHNUNG DER R-KOORDINATEN DES FEINEN GITTERS                   GRU 1200
C                                                                       GRU 1210
      R01 = (RC(JI0-1)/RC(IMA))**2                                      GRU 1220
      DO 200 I=1,JRFEIN                                                 GRU 1230
        GRRKOT(I) = SQRT((REAL(I*2)-EP)/REAL(JRFEIN*2)*(1.-R01)+R01) *  GRU 1240
     1   RC(IMA)                                                        GRU 1250
  200 CONTINUE                                                          GRU 1260
C                                                                       GRU 1270
C     BERECHNUNG DER ANZAHL DER FEINEN GITTERELEMENTE IN DEN TEILVOLUMI-GRU 1280
C     NA DER BEIDEN GROBEN UEBERLAGERTEN GITTER                         GRU 1290
C                                                                       GRU 1300
      KSCH = 0                                                          GRU 1310
      DO 10 I=1,IZFEIN                                                  GRU 1320
        KANALR = 0                                                      GRU 1330
        KANALS = 0                                                      GRU 1340
        DO 1011 IH=1,KANAL                                              GRU 1350
          KHILF(IH) = 0                                                 GRU 1360
 1011   CONTINUE                                                        GRU 1370
        DO 30 K=1,JZROT                                                 GRU 1380
C                                                                       GRU 1390
C     ABFRAGE AUF NUMMER DES ROTEN (CITATION) GITTERS IN Z-RICHTUNG     GRU 1400
C                                                                       GRU 1410
          IF(ZROT(K).LT.GRZKOT(I)) GOTO 30                              GRU 1420
          KANALI = K                                                    GRU 1430
          GOTO 40                                                       GRU 1440
   30   CONTINUE                                                        GRU 1450
   40   CONTINUE                                                        GRU 1460
        DO 32 IS=1,KANAL                                                GRU 1470
          DO 33 IX=1,KAN(IS)                                            GRU 1480
C                                                                       GRU 1490
C     ABFRAGE AUF NUMMER DES SCHWARZEN (VSOP) GITTERS IN Z-RICHTUNG     GRU 1500
C                                                                       GRU 1510
            IF(ZKOT(IS,IX) .LT. GRZKOT(I)) GOTO 33                      GRU 1520
            KHILF(IS) = IX                                              GRU 1530
            GOTO 43                                                     GRU 1540
   33     CONTINUE                                                      GRU 1550
   43     CONTINUE                                                      GRU 1560
          IJ = IJR(IS)                                                  GRU 1570
          DO 55 IK=1,IJ                                                 GRU 1580
            ZGROB(IK) = XWE(IS,IK)                                      GRU 1590
            POGROB(IK) = YWE(IS,IK)                                     GRU 1600
   55     CONTINUE                                                      GRU 1610
          UU = GRZKOT(I)                                                GRU 1620
C                                                                       GRU 1630
          CALL LAGRASB(ZGROB,POGROB)                                    GRU 1640
C                                                                       GRU 1650
          RKOSCH(IS) = VV                                               GRU 1660
   32   CONTINUE                                                        GRU 1670
        ISI = 1                                                         GRU 1680
        FLAG = 0.                                                       GRU 1690
        KROT = 0                                                        GRU 1700
        DO 20 J=1,JRFEIN                                                GRU 1710
C                                                                       GRU 1720
C     ABFRAGE AUF NUMMER DES SCHWARZEN (VSOP) GITTER IN R-RICHTUNG      GRU 1730
C                                                                       GRU 1740
   60     IF(J .EQ. JRFEIN .AND. RKOSCH(ISI) .EQ. GRRKOT(J)) THEN       GRU 1750
           KANALS = KANALS + KHILF(ISI)                                 GRU 1760
          ELSE IF(RKOSCH(ISI) .LT. GRRKOT(J)) THEN                      GRU 1770
           KANALS = 0                                                   GRU 1780
          DO 222 IZ=1,ISI                                               GRU 1790
            KANALS = KANALS + KAN(IZ)                                   GRU 1800
  222     CONTINUE                                                      GRU 1810
          ISI = ISI + 1                                                 GRU 1820
          FLAG = 1.                                                     GRU 1830
          GOTO 60                                                       GRU 1840
          ELSE                                                          GRU 1850
           KANALS = KANALS + KHILF(ISI)                                 GRU 1860
          END IF                                                        GRU 1870
          KANALR = KANALI                                               GRU 1880
          DO 50 KJ=1,IRROT                                              GRU 1890
C                                                                       GRU 1900
C     ABFRAGE AUF NUMMER DES ROTEN (CITATION) GITTERS IN R-RICHTUNG     GRU 1910
C                                                                       GRU 1920
            IF(RROT(KJ) .GE. GRRKOT(J)) GOTO 66                         GRU 1930
            KANALR = KANALR + JZROT                                     GRU 1940
            IF(KANALR .GT. (JZROT*IRROT)) GOTO 10                       GRU 1950
   50     CONTINUE                                                      GRU 1960
   66     KANALV = KLAY(KANALS)                                         GRU 1970
          V(KANALR,KANALV) = V(KANALR,KANALV) + 1                       GRU 1980
          KSCH = MAX0(KANALV,KSCH)                                      GRU 1990
          KROT = MAX0(KANALR,KROT)                                      GRU 2000
          IF(FLAG .LE. 0.) THEN                                         GRU 2010
           KANALS = 0                                                   GRU 2020
          ELSE                                                          GRU 2030
           KANALS = 0                                                   GRU 2040
           DO 333 IY=1,ISI-1                                            GRU 2050
             KANALS = KANALS + KAN(IY)                                  GRU 2060
  333      CONTINUE                                                     GRU 2070
          END IF                                                        GRU 2080
   20   CONTINUE                                                        GRU 2090
   10 CONTINUE                                                          GRU 2100
C                                                                       GRU 2110
C     FEINE TEILVOLUMENEINHEIT *ZVOL*                                   GRU 2120
C                                                                       GRU 2130
  499 CONTINUE                                                          GRU 2140
      ZVOL = PI * ((RC(IMA)**2-RC(JI0-1)**2)/FLOAT(JRFEIN)) *           GRU 2150
     1 ((ZC(NCOR2)- ZC(NCOR1-1))/FLOAT(IZFEIN))                         GRU 2160
      IF(MDIREC .LE. 0) GOTO 665                                        GRU 2170
      DO 664 I=1,IMX                                                    GRU 2180
        V(XJV(I),XIV(I)) = VFT(I) / ZVOL                                GRU 2190
  664 CONTINUE                                                          GRU 2200
  665 CONTINUE                                                          GRU 2210
C                                                                       GRU 2220
C     EINFUEGEN DES GEMEINSAMEN AEUSSEREN CITATION/VSOP-GITTERS IN DIE  GRU 2230
C     VOLUMEN-MATRIX                                                    GRU 2240
C                                                                       GRU 2250
      DO 500 K=1,2000                                                   GRU 2260
        VCI(K) = 0.                                                     GRU 2270
  500 CONTINUE                                                          GRU 2280
      KKROT = KROT                                                      GRU 2290
      DO 502 N=1,NMAX                                                   GRU 2300
        DO 501 I=1,IMAX                                                 GRU 2310
          IF(LAYC(I,N) .LE. KROT) GOTO 501                              GRU 2320
          VCIT = (RC(I)**2-RC(I-1)**2) * PI * (ZC(N)-ZC(N-1))           GRU 2330
          KANALR = LAYC(I,N)                                            GRU 2340
          KANALV = KSCH + KANALR - KROT                                 GRU 2350
          KKROT = MAX0(KKROT,KANALR)                                    GRU 2360
          KLAY(KANALV) = KANALV                                         GRU 2370
          VCI(KANALR) = VCI(KANALR) + VCIT                              GRU 2380
          V(KANALR,KANALV) = VCI(KANALR) / ZVOL                         GRU 2390
  501   CONTINUE                                                        GRU 2400
  502 CONTINUE                                                          GRU 2410
      KSCH = KSCH + KKROT - KROT                                        GRU 2420
      KROT = KKROT                                                      GRU 2430
      IF(IPUT .LE. 0) GOTO 105                                          GRU 2440
      WRITE (N6,101)                                                    GRU 2450
      WRITE (N6,102)                                                    GRU 2460
      WRITE (N6,103)                                                    GRU 2470
      WRITE (N6,104)                                                    GRU 2480
      DO 110 J=1,KROT                                                   GRU 2490
        IF(J .EQ. 1) WRITE (N6,111)(V(J,I),I=1,KSCH)                    GRU 2500
        IF(J .GT. 1) WRITE (N6,112)(V(J,I),I=1,KSCH)                    GRU 2510
  110 CONTINUE                                                          GRU 2520
  105 CONTINUE                                                          GRU 2530
      IVOL = 0                                                          GRU 2540
      DO 413 IR=1,KROT                                                  GRU 2550
        MT(IR) = 0                                                      GRU 2560
        DO 415 IS=1,KSCH                                                GRU 2570
          IF(IR .EQ. 1) NT(IS) = 0                                      GRU 2580
          IVOL = V(IR,IS) + IVOL                                        GRU 2590
  415   CONTINUE                                                        GRU 2600
  413 CONTINUE                                                          GRU 2610
      GESVOL = REAL(IVOL) * ZVOL                                        GRU 2620
      WRITE (6,1221) ZVOL,IVOL,GESVOL                                   GRU 2630
      DO 910 IR=1,KROT                                                  GRU 2640
        DO 911 IS=1,KSCH                                                GRU 2650
          MT(IR) = MT(IR) + V(IR,IS)                                    GRU 2660
          NT(IS) = NT(IS) + V(IR,IS)                                    GRU 2670
  911   CONTINUE                                                        GRU 2680
        VT(IR) = FLOAT(MT(IR)) * ZVOL                                   GRU 2690
  910 CONTINUE                                                          GRU 2700
      DO 913 IS=1,KSCH                                                  GRU 2710
        VS(IS) = FLOAT(NT(IS)) * ZVOL                                   GRU 2720
        IF(YYY(IS) .EQ. 0.) YYY(IS) = VS(IS)                            GRU 2730
        DDYY(IS) = (VS(IS)-YYY(IS)) / VS(IS)                            GRU 2740
  913 CONTINUE                                                          GRU 2750
      WRITE (N6,917)                                                    GRU 2760
      KROSCH = MAX0(KROT,KSCH)                                          GRU 2770
      DO 914 IRIS=1,KROSCH                                              GRU 2780
        IF(IRIS .LE. KSCH) WRITE (N6,915) IRIS,NT(IRIS),VS(IRIS),       GRU 2790
     1   YYY(IRIS),DDYY(IRIS)                                           GRU 2800
        IF(IRIS .LE. KROT .AND. IRIS .LE. KSCH) WRITE (N6,912) IRIS,    GRU 2810
     1   MT(IRIS),VT(IRIS)                                              GRU 2820
        IF(IRIS .GT. KSCH) WRITE (N6,916) IRIS,MT(IRIS),VT(IRIS)        GRU 2830
  914 CONTINUE                                                          GRU 2840
      WRITE (N6,918)                                                    GRU 2850
      I=0                                                               GRU 2860
      DO 667 IR=1,KROT                                                  GRU 2870
        IF(NVSOP .GT. 0) NOPOWK(IR) = 2                                 GRU 2880
        DO 666 IS=1,KSCH                                                GRU 2890
          IF(V(IR,IS) .NE. 0) THEN                                      GRU 2900
           I = I + 1                                                    GRU 2910
           XJV(I) = IR                                                  GRU 2920
           XIV(I) = IS                                                  GRU 2930
           VVOL(I) = V(IR,IS)                                           GRU 2940
           IF(NVSOP .GT. 0 .AND. NOPOWR(IS) .EQ. 0) NOPOWK(IR) = 1      GRU 2950
           ICORCI = NOPOWK(IR)                                          GRU 2960
          ELSE                                                          GRU 2970
           GOTO 666                                                     GRU 2980
          END IF                                                        GRU 2990
          VFV(I) = REAL(VVOL(I)) / REAL(NT(IS))                         GRU 3000
          VFC(I) = REAL(VVOL(I)) / REAL(MT(IR))                         GRU 3010
          IF(NVSOP .GT. 0) WRITE (6,771) I,XIV(I),XJV(I),CITCO(ICORCI), GRU 3020
     1     VVOL(I),VFV(I),VFC(I)                                        GRU 3030
          IF(NVSOP .EQ. 0) WRITE (6,772) I,XIV(I),XJV(I),VVOL(I),VFV(I),GRU 3040
     1     VFC(I)                                                       GRU 3050
  666   CONTINUE                                                        GRU 3060
  667 CONTINUE                                                          GRU 3070
      IMX = I                                                           GRU 3080
C                                                                       GRU 3090
C     SCHREIBE V-MATRIX-LIBRARY                                         GRU 3100
C                                                                       GRU 3110
      IF(IWRITE .GT. 0) WRITE (IWRITE) IMX,KROT,KSCH,ZVOL,(XIV(I),XJV(I)GRU 3120
     1 ,VVOL(I),VFV(I),VFC(I), I=1,IMX),(YYY(IS), IS=1,KSCH)            GRU 3130
      IF(IWRITE .GT. 0) WRITE (N6,710) IWRITE                           GRU 3140
      GOTO 701                                                          GRU 3150
C                                                                       GRU 3160
C     LIES V-MATRIX-LIBRARY                                             GRU 3170
C                                                                       GRU 3180
  700 CONTINUE                                                          GRU 3190
      JWRITE = IABS(IWRITE)                                             GRU 3200
      READ (JWRITE) IMX,KROT,KSCH,ZVOL,(XIV(I),XJV(I),VVOL(I),VFV(I),   GRU 3210
     1 VFC(I),I=1,IMX),(YYY(IS),IS=1,KSCH)                              GRU 3220
      WRITE (N6,720) JWRITE                                             GRU 3230
  701 CONTINUE                                                          GRU 3240
      IF(NTHX .EQ. 0) GOTO 669                                          GRU 3250
      DO 668 J=1,IMX                                                    GRU 3260
        VFT(J) = VVOL(J) * ZVOL                                         GRU 3270
  668 CONTINUE                                                          GRU 3280
  669 CONTINUE                                                          GRU 3290
      IF(N7 .LE. 0 .AND. N8 .LE. 0) GOTO 600                            GRU 3300
C                                                                       GRU 3310
C     NEUE PLOT-AUFBEREITUNG DES AEUSSEREN TEILES:                      GRU 3320
C                            1.)  NACH KOMPOSITIONEN                    GRU 3330
C                            2.)  FEINMASCHEN-GITTER  (R + Z)           GRU 3340
C                                                                       GRU 3350
      IC = 0                                                            GRU 3360
      DO 523 N=1,NMAX                                                   GRU 3370
        L = LAYC(1,N)                                                   GRU 3380
        DO 523 I=1,IMAX                                                 GRU 3390
          IF(I .EQ. IMAX) GOTO 521                                      GRU 3400
          L1 = LAYC(I+1,N)                                              GRU 3410
          IF(L1 .EQ. L) GOTO 522                                        GRU 3420
          IF(L1 .LE. LC .AND. L .LE. LC) GOTO 522                       GRU 3430
  521     CONTINUE                                                      GRU 3440
          IC = IC + 1                                                   GRU 3450
          IF(N7 .GT. 0) WRITE (N7,550) ZC(N-1),ZC(N)                    GRU 3460
          IF(N7 .GT. 0) WRITE (N7,550) RC(I),RC(I)                      GRU 3470
          IF(N8 .EQ. 0) GOTO 522                                        GRU 3480
          WRITE (N8,551) RC(I),BLANC,BLANC,BLANC,ZC(NMAX)-ZC(N-1)       GRU 3490
          WRITE (N8,551) RC(I),BLANC,BLANC,BLANC,ZC(NMAX)-ZC(N)         GRU 3500
          WRITE (N8,580) BLANC                                          GRU 3510
  522     L = L1                                                        GRU 3520
  523 CONTINUE                                                          GRU 3530
      DO 526 I=1,IMAX                                                   GRU 3540
        L = LAYC(I,1)                                                   GRU 3550
        DO 526 N=1,NMAX                                                 GRU 3560
          IF(N .EQ. NMAX) GOTO 524                                      GRU 3570
          L1 = LAYC(I,N+1)                                              GRU 3580
          IF(L1 .EQ. L) GOTO 525                                        GRU 3590
          IF(L1 .LE. LC .AND. L .LE. LC) GOTO 525                       GRU 3600
  524     CONTINUE                                                      GRU 3610
          IC = IC + 1                                                   GRU 3620
          IF(N7 .GT. 0) WRITE (N7,550) ZC(N),ZC(N)                      GRU 3630
          IF(N7 .GT. 0) WRITE (N7,550) RC(I-1),RC(I)                    GRU 3640
          IF(N8 .EQ. 0) GOTO 525                                        GRU 3650
          WRITE (N8,551) RC(I-1),BLANC,BLANC,BLANC,ZC(NMAX)-ZC(N)       GRU 3660
          WRITE (N8,551) RC(I),BLANC,BLANC,BLANC,ZC(NMAX)-ZC(N)         GRU 3670
          WRITE (N8,580) BLANC                                          GRU 3680
  525     L = L1                                                        GRU 3690
  526 CONTINUE                                                          GRU 3700
      IF(N8 .EQ. 0) GOTO 900                                            GRU 3710
      IC = IC + 1                                                       GRU 3720
      WRITE (N8,551) RC(0),BLANC,BLANC,BLANC,ZC(NMAX)                   GRU 3730
      WRITE (N8,551) RC(IMAX),BLANC,BLANC,BLANC,ZC(NMAX)                GRU 3740
      WRITE (N8,580) BLANC                                              GRU 3750
  900 WRITE (N6,560) IC                                                 GRU 3760
      II = 1                                                            GRU 3770
      RF(0) = RC(0)                                                     GRU 3780
      DO 528 I=1,IMAX                                                   GRU 3790
        M = MR(I)                                                       GRU 3800
        DO 527 K=1,M                                                    GRU 3810
          RF(II) = RF(II-1) + (RC(I)-RC(I-1)) / M                       GRU 3820
          II = II + 1                                                   GRU 3830
  527   CONTINUE                                                        GRU 3840
  528 CONTINUE                                                          GRU 3850
      II = II - 1                                                       GRU 3860
      NN = 1                                                            GRU 3870
      ZF(0) = ZC(0)                                                     GRU 3880
      DO 530 N=1,NMAX                                                   GRU 3890
        M = MZ(N)                                                       GRU 3900
        DO 529 K=1,M                                                    GRU 3910
          ZF(NN) = ZF(NN-1) + (ZC(N)-ZC(N-1)) / M                       GRU 3920
          NN = NN + 1                                                   GRU 3930
  529   CONTINUE                                                        GRU 3940
  530 CONTINUE                                                          GRU 3950
      NN = NN - 1                                                       GRU 3960
      IC = 0                                                            GRU 3970
      DO 531 N=1,NN                                                     GRU 3980
        IC = IC + 1                                                     GRU 3990
        IF(N7 .GT. 0) WRITE (N7,550) ZF(N),ZF(N)                        GRU 4000
        IF(N7 .GT. 0) WRITE (N7,550) RF(0),RF(II)                       GRU 4010
  531 CONTINUE                                                          GRU 4020
      DO 532 I=1,II                                                     GRU 4030
        IC = IC + 1                                                     GRU 4040
        IF(N7 .GT. 0) WRITE (N7,550) ZF(0),ZF(NN)                       GRU 4050
        IF(N7 .GT. 0) WRITE (N7,550) RF(I),RF(I)                        GRU 4060
  532 CONTINUE                                                          GRU 4070
      WRITE (N6,570) IC                                                 GRU 4080
  600 CONTINUE                                                          GRU 4090
      END                                                               GRU 4100
      SUBROUTINE DIRECT                                                 DIR   10
C                                                                       DIR   20
C     VEREINFACHENDE VOLUMENMATRIX-BERECHNUNG AUS *THERMIX*             DIR   30
C     (NUR BEI SENKRECHTER BEGRENZUNG ALLER KANAELE MOEGLICH!)          DIR   40
C                                                                       DIR   50
CFZJ048 enlarged dimension                                    11.04.07  DIR   60
      REAL XWE(0:15,15),YWE(0:15,15),RC(0:100),ZC(0:200),VOLREG(1515),  DIR   70
     1 VFV(4000),VFC(4000),RV(21),RT(101),ZT(201),RVT(101),ZVT(201),    DIR   80
     2 VFT(4000)                                                        DIR   90
C                                                                       DIR  100
CFZJ048 enlarged dimension                                    11.04.07  DIR  110
      INTEGER ICOPL(20),KLAY(1515),XJV(4000),XIV(4000)                  DIR  120
C                                                                       DIR  130
      COMMON /FORALB/ UU,VV,XWE,YWE,IJR(0:15),N5,N6,N7,IJ               DIR  140
C                                                                       DIR  150
      COMMON /CORB/ HCORE,KAN(15),NKURVE,H4,R1,H3,R2,KANAL              DIR  160
C                                                                       DIR  170
      COMMON /GEOB/ MR(100),MZ(200),LAYVC(100,200),MGR(50),MGZ(100),    DIR  180
     1 ICOR(100),LAYC(100,200),JCOR(15),JI,JA,JI0,RKONUS,ZKONUS,RC,ZC,  DIR  190
     2 NCOR1,NCOR2,IMA                                                  DIR  200
C                                                                       DIR  210
      COMMON /VSOB/ KLAY,IPL,NVSOP,ICOPL,VOLREG,N20,KSCH,IMX,XJV,XIV,VFVDIR  220
     1 ,VFC,KROT                                                        DIR  230
C                                                                       DIR  240
      COMMON /MAXIB/ IMAX,NMAX                                          DIR  250
C                                                                       DIR  260
      COMMON /DB/ DELLAY(15),DLAY(15,101)                               DIR  270
C                                                                       DIR  280
      COMMON /TERMIB/ NTHX,VFT                                          DIR  290
C                                                                       DIR  300
  100 FORMAT (/' *** PROGRAM-INTERRUPION, BECAUSE VOLUME-MATRIX WILL BE DIR  310
     1TOO LARGE (MAXIMUM = 2000 ***'/)                                  DIR  320
C                                                                       DIR  330
C                                                                       DIR  340
      KDIM = 4000                                                       DIR  350
      PI = 3.141592654                                                  DIR  360
      E = 1.E-5                                                         DIR  370
C                                                                       DIR  380
C     R - INTERVALLE VSOP                                               DIR  390
C                                                                       DIR  400
      JV1 = NKURVE                                                      DIR  410
      DO 1 I=0,KANAL                                                    DIR  420
        RV(I+1) = YWE(I,1)                                              DIR  430
    1 CONTINUE                                                          DIR  440
C                                                                       DIR  450
C     R- UND Z - INTERVALLE FUER CITATION BZW. THERMIX                  DIR  460
C                                                                       DIR  470
      JT1 = 0                                                           DIR  480
      DO 2 I=0,IMAX                                                     DIR  490
        X = RC(I)                                                       DIR  500
        IF(RV(1) .EQ. 0.) GOTO 60                                       DIR  510
        IF(((RV(1)-X)/RV(1)) .GT. E) GOTO 2                             DIR  520
   60   CONTINUE                                                        DIR  530
        IF(((X-RV(JV1))/RV(JV1)) .GT. E) GOTO 2                         DIR  540
        JT1 = JT1 + 1                                                   DIR  550
        RT(JT1) = X                                                     DIR  560
    2 CONTINUE                                                          DIR  570
      NT1 = 0                                                           DIR  580
      HCO = HCORE                                                       DIR  590
      DO 3 N=0,NMAX                                                     DIR  600
        X = ZC(N) - ZC(NCOR1-1)                                         DIR  610
        IF(X .LT. 0.) GOTO 3                                            DIR  620
        IF(((X-HCO)/HCO) .GT. E) GOTO 3                                 DIR  630
        NT1 = NT1 + 1                                                   DIR  640
        ZT(NT1) = X                                                     DIR  650
    3 CONTINUE                                                          DIR  660
C                                                                       DIR  670
C     R + Z FUER VSOP- UND CITATION- BZW. THERMIX-GITTER                DIR  680
C                                                                       DIR  690
      X0 = 0.                                                           DIR  700
      X2 = RV(JV1)                                                      DIR  710
      JVT1 = 1                                                          DIR  720
      RVT(1) = RV(1)                                                    DIR  730
    4 CONTINUE                                                          DIR  740
      X1 = X2                                                           DIR  750
      DO 5 I=1,JV1                                                      DIR  760
        X = RV(I)                                                       DIR  770
        IF(X .EQ. 0.) GOTO 5                                            DIR  780
        IF(((X-X0)/X) .GT. E) X1 = AMIN1(X,X1)                          DIR  790
    5 CONTINUE                                                          DIR  800
      DO 6 I=1,JT1                                                      DIR  810
        X = RT(I)                                                       DIR  820
        IF(X .EQ. 0.) GOTO 6                                            DIR  830
        IF(((X-X0)/X) .GT. E) X1 = AMIN1(X,X1)                          DIR  840
    6 CONTINUE                                                          DIR  850
      JVT1 = JVT1 + 1                                                   DIR  860
      RVT(JVT1) = X1                                                    DIR  870
      X0 = X1                                                           DIR  880
      IF(((X2-X1)/X2) .GT. E) GOTO 4                                    DIR  890
      X0 = 0.                                                           DIR  900
      X2 = HCO                                                          DIR  910
      NVT1 = 1                                                          DIR  920
      ZVT(1) = 0.                                                       DIR  930
    7 CONTINUE                                                          DIR  940
      X1 = X2                                                           DIR  950
      DO 8 K=1,KANAL                                                    DIR  960
        DLA = 0.                                                        DIR  970
        NV1 = KAN(K) + 1                                                DIR  980
        HC = HCO / FLOAT(KAN(K))                                        DIR  990
        DO 8 N=1,NV1                                                    DIR 1000
          X = FLOAT(N-1) * HC                                           DIR 1010
          IF(DELLAY(K) .EQ. 0.) GOTO 50                                 DIR 1020
          IF(N .GT. 1) DLA = DLA + DLAY(K,N-1)                          DIR 1030
          X = DLA                                                       DIR 1040
   50     CONTINUE                                                      DIR 1050
          IF(X .EQ. 0.) GOTO 8                                          DIR 1060
          IF(((X-X0)/X) .GT. E) X1 = AMIN1(X,X1)                        DIR 1070
    8 CONTINUE                                                          DIR 1080
      DO 9 N=1,NT1                                                      DIR 1090
        X = ZT(N)                                                       DIR 1100
        IF(X .EQ. 0.) GOTO 9                                            DIR 1110
        IF(((X-X0)/X) .GT. E) X1 = AMIN1(X,X1)                          DIR 1120
    9 CONTINUE                                                          DIR 1130
      NVT1 = NVT1 + 1                                                   DIR 1140
      ZVT(NVT1) = X1                                                    DIR 1150
      X0 = X1                                                           DIR 1160
      IF(((X2-X1)/X2) .GT. E) GOTO 7                                    DIR 1170
C                                                                       DIR 1180
C     "VOLVT"-MATRIX DER VOLUMENZUORDNUNG VSOP-CITATION BZW. THERMIX    DIR 1190
C                                                                       DIR 1200
      IMX = 0                                                           DIR 1210
      NV0 = 0                                                           DIR 1220
      IVS0 = 1                                                          DIR 1230
      JJ = 2                                                            DIR 1240
      KSCH = 0                                                          DIR 1250
      KROT = 0                                                          DIR 1260
      DO 40 J=JJ,JVT1                                                   DIR 1270
        RR = RVT(J)**2 - RVT(J-1)**2                                    DIR 1280
        ITS = 1                                                         DIR 1290
        DO 11 I=2,JT1                                                   DIR 1300
          IF(ABS(RT(I)-RVT(J))/RT(I) .LE. E .OR. RT(I) .GT. RVT(J)) GOTODIR 1310
     1     12                                                           DIR 1320
          ITS = ITS + 1                                                 DIR 1330
   11   CONTINUE                                                        DIR 1340
   12   CONTINUE                                                        DIR 1350
        IVS = 1                                                         DIR 1360
        DO 13 I=2,JV1                                                   DIR 1370
          IF(ABS(RV(I)-RVT(J))/RV(I) .LE. E .OR. RV(I) .GT. RVT(J)) GOTODIR 1380
     1     14                                                           DIR 1390
          IVS = IVS + 1                                                 DIR 1400
   13   CONTINUE                                                        DIR 1410
   14   CONTINUE                                                        DIR 1420
        IF(IVS .GT. IVS0) NV0 = NV0 + KAN(IVS-1)                        DIR 1430
        IVS0 = IVS                                                      DIR 1440
        DO 30 M=2,NVT1                                                  DIR 1450
          W = (ZVT(M)-ZVT(M-1)) * RR * PI                               DIR 1460
          NT = (ITS-1) * (NT1-1) + 1                                    DIR 1470
          DO 21 N=2,NT1                                                 DIR 1480
            IF(ABS(ZT(N)-ZVT(M))/ZT(N) .LE. E .OR. ZT(N) .GT. ZVT(M))   DIR 1490
     1       GOTO 22                                                    DIR 1500
            NT = NT + 1                                                 DIR 1510
   21     CONTINUE                                                      DIR 1520
   22     CONTINUE                                                      DIR 1530
          NV1 = KAN(IVS) + 1                                            DIR 1540
          NV = NV0 + 1                                                  DIR 1550
          HC = HCO / FLOAT(KAN(IVS))                                    DIR 1560
          DLA = 0.                                                      DIR 1570
          DO 23 N=2,NV1                                                 DIR 1580
            X = HC * FLOAT(N-1)                                         DIR 1590
            IF(DELLAY(IVS) .EQ. 0.) GOTO 51                             DIR 1600
            DLA = DLA + DLAY(IVS,N-1)                                   DIR 1610
            X = DLA                                                     DIR 1620
   51       CONTINUE                                                    DIR 1630
            Y = ZVT(M)                                                  DIR 1640
            IF(ABS(X-Y)/X .LE. E .OR. X .GT. Y) GOTO 24                 DIR 1650
            NV = NV + 1                                                 DIR 1660
   23     CONTINUE                                                      DIR 1670
   24     CONTINUE                                                      DIR 1680
          IF(IMX .EQ. 0) GOTO 26                                        DIR 1690
          DO 25 K=1,IMX                                                 DIR 1700
            IF(XIV(K) .NE. NV) GOTO 25                                  DIR 1710
            IF(XJV(K) .NE. NT) GOTO 25                                  DIR 1720
            VFT(K) = VFT(K) + W                                         DIR 1730
            GOTO 28                                                     DIR 1740
   25     CONTINUE                                                      DIR 1750
   26     CONTINUE                                                      DIR 1760
          IMX = IMX + 1                                                 DIR 1770
          IF(IMX .LE. KDIM) GOTO 27                                     DIR 1780
          WRITE (6,100)                                                 DIR 1790
          STOP                                                          DIR 1800
   27     CONTINUE                                                      DIR 1810
          XIV(IMX) = NV                                                 DIR 1820
          XJV(IMX) = NT                                                 DIR 1830
          VFT(IMX) = W                                                  DIR 1840
          KSCH = MAX0(KSCH,NV)                                          DIR 1850
          KROT = MAX0(KROT,NT)                                          DIR 1860
   28     CONTINUE                                                      DIR 1870
   30   CONTINUE                                                        DIR 1880
   40 CONTINUE                                                          DIR 1890
      RETURN                                                            DIR 1900
      END                                                               DIR 1910