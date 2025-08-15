      SUBROUTINE CITA(ICI,SGA,SGTR,V1,FKEN,NFUMM,SS)                    CIT   10
C                                                                       CIT   20
C     ZUSAMMENSTELLEN DER CITATION-EINGABE                              CIT   30
C                                                                       CIT   40
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    CIT   50
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    CIT   60
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PICIT   70
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D             CIT   80
C                                                                       CIT   90
      COMMON /MU/ MU4,NCIT,NGEOM,I9,IOM,M34,M21                         CIT  100
C                                                                       CIT  110
CFZJ010 Increase dimensions in COMMON ANISO                   01.12.03  CIT  120
      COMMON /ANISO/ JH,IZONE(200),RDK(20,96),KK,M1(201,4),NANI,V2(20,3)CIT  130
C                                                                       CIT  140
CFZJ031                                                       28.05.04  CIT  150
CFZJ062                                                       04.05.11  CIT  160
      COMMON /ORIGEN/ LOB,NOR,VOR(100),ISPK,KFISS,N200C,NXSC,KGR,       CIT  170
     1 LOBN,IEZ,FSP(33)                                                 CIT  180
C                                                                       CIT  190
      COMMON /VARDIM/ A(8000000)                                        CIT  200
C                                                                       CIT  210
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       CIT  220
C                                                                       CIT  230
CFZJ056                                                       06.11.07  CIT  240
      COMMON /FSD/ NE,IOPTF,N1F(2),N2F(50),V1F(33),V2F(50),NFS          CIT  250
C                                                                       CIT  260
      COMMON /IFA/ FA0,FA1,FA2,IEND                                     CIT  270
C                                                                       CIT  280
      COMMON /BLOCK3/ IDUN(16),FIWATT,IDUM(467),SERCON,IDU(22),REACT(2),CIT  290
     1 JDUM(191),ZKFIND,JDU(1233),IVSP(30)                              CIT  300
C                                                                       CIT  310
      COMMON /ILRTB/ ITR,IBR,JRR,JIR                                    CIT  320
CFZJ039 increase dimensions of arrays DUM and DUN             03.01.05  CIT  330
      COMMON /ALSUB/ DUM(905),XMIS(6),DUN(830),SPARE(200)               CIT  340
C                                                                       CIT  350
      COMMON /IDIFKO/ INGC24                                            CIT  360
C                                                                       CIT  370
      COMMON /PDTX/ PDTHX                                               CIT  380
C                                                                       CIT  390
      COMMON /TRN200C/ N200TR                                           CIT  400
C                                                                       CIT  410
CFZJ057                                                       07.11.07  CIT  420
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300),FABC(10),REPC(10),NNECIT  430
     1 ,LISTEQ,NTIK                                                     CIT  440
C                                                                       CIT  450
      EQUIVALENCE(LI(3),LOUSI),(LI(4),LTOSI),(LI(5),LABSI),(LI(6),LFISI)CIT  460
     1 ,(LI(7),LXNU),(LI(10),LIREG),(LI(11),LRPHI),(LI(12),LNBUR),      CIT  470
     2 (LI(17),LSSLU),(LI(28),LNHOT),(LI(29),LVOL),(LI(30),LVPAR),      CIT  480
     3 (LI(31),LDEN),(LI(33),LFISM),(LI(48),LNRG),(LI(43),LXSIS),       CIT  490
     4 (LI(44),LXTOT),(LI(45),LXSIA),(LI(46),LXNFI),(LI(18),LCFRA),     CIT  500
     5 (LI(96),LCNU),(LI(20),LPOIL),(LI(23),LNPOI),(LI(40),LHM),        CIT  510
     6 (LI(41),LTHBU),(LI(51),LNOPO),(LI(62),LBUCK),(LI(66),LRPH),      CIT  520
     7 (LI(85),LBUK),(LI(101),LB),(LI(25),LZLTH),(LI(1),LIMAT),         CIT  530
     8 (LI(151),LAWT),(LI(152),LGS),(LI(153),LGM),(LI(179),LPHIN),      CIT  540
     9 (LI(180),LTRSI),(LI(181),LSCTR),(LI(182),LXSIG),(JTPE2,NS),      CIT  550
     X (LI(2),LNOPK)                                                    CIT  560
C                                                                       CIT  570
CFZJ011 Increase dimensions of arrays SGA, SGTR, FKEN         01.12.03  CIT  580
      DIMENSION N1(150),V(150),SGA(20,N26),SGTR(20,N26,N26),            CIT  590
     1 V1(201,N26,3),FKEN(20,N26),SS(N26,N200)                          CIT  600
C                                                                       CIT  610
      REAL*4 V0(6)/6*0./                                                CIT  620
C                                                                       CIT  630
      INTEGER N0(6)/6*0/                                                CIT  640
C                                                                       CIT  650
      CHARACTER*4 B(18)                                                 CIT  660
C                                                                       CIT  670
  100 FORMAT (2I6)                                                      CIT  680
  101 FORMAT (18A4)                                                     CIT  690
  102 FORMAT (24I3)                                                     CIT  700
  103 FORMAT (6E12.5)                                                   CIT  710
  104 FORMAT (6(I3,F9.3))                                               CIT  720
  105 FORMAT (3E12.5,I3)                                                CIT  730
  106 FORMAT (6(I3,E9.3))                                               CIT  740
  107 FORMAT (15I5)                                                     CIT  750
  108 FORMAT (6(I3,F9.4))                                               CIT  760
  109 FORMAT (18I4)                                                     CIT  770
  110 FORMAT (12I6)                                                     CIT  780
  111 FORMAT (15I3,I2,I4,7I3)                                           CIT  790
  112 FORMAT (4I3,I5)                                                   CIT  800
C                                                                       CIT  810
C                                                                       CIT  820
      IF(I3D .EQ. 0) MM1 = 2                                            CIT  830
      IF(I3D .GT. 0) MM1 = 3                                            CIT  840
      JTPE = 0                                                          CIT  850
      NSR = 66                                                          CIT  860
      NSU = NS                                                          CIT  870
      IF(JTPE7 .GT. 0) NSU = NSR                                        CIT  880
      IF(JTPE7 .EQ. 0 .AND. JTPE9 .GT. 0) JTPE = 1                      CIT  890
      IEND = 0                                                          CIT  900
      NCIT = 42                                                         CIT  910
      I9 = 4                                                            CIT  920
      M21 = 21                                                          CIT  930
      M34 = 54                                                          CIT  940
      NP1 = NFUMM                                                       CIT  950
      IF(NFUMM .EQ. 0) NP1 = NENDP + 1                                  CIT  960
      NP2 = NP1 + N200 * N26                                            CIT  970
      NP3 = NP1 + 2 * (N200*N26)                                        CIT  980
      NP5 = NP1 + 3 * (N200*N26)                                        CIT  990
      NP6 = NP1 + 4 * (N200*N26)                                        CIT 1000
      NP7 = NP1 + 5 * (N200*N26)                                        CIT 1010
      NP8 = NP1 + 6 * (N200*N26)                                        CIT 1020
      NP9 = NP8 + N200                                                  CIT 1030
      NP10 = NP9 + N200                                                 CIT 1040
      NEND10 = NP10 + N26                                               CIT 1050
      GOTO(200,300),ICI                                                 CIT 1060
  200 NGEOM = 37                                                        CIT 1070
      IF(MUHU(28) .GT. 0) GOTO 400                                      CIT 1080
      MUHU(I9) = MUHU(I9) + 1                                           CIT 1090
      IF(MUHU(I9) .GT. 1) GOTO 10                                       CIT 1100
      OPEN(35,FORM='UNFORMATTED',FILE='rstcit')                         CIT 1110
      OPEN(42,FILE='macsig')                                            CIT 1120
C                                                                       CIT 1130
C      TITLE CARD                                                       CIT 1140
C                                                                       CIT 1150
CARDS C 1                                                               CIT 1160
C                                                                       CIT 1170
      READ (NSU,101) (B(I),I=1,18)                                      CIT 1180
C                                                                       CIT 1190
      WRITE (NCIT,101) (B(I),I=1,18)                                    CIT 1200
      IF(JTPE .EQ. 1) WRITE (NSR,101) (B(I),I=1,18)                     CIT 1210
C                                                                       CIT 1220
C     SECTION 001: CONTROL OPTIONS,EDIT OPTIONS,ITERATION LIMIT ...     CIT 1230
C                                                                       CIT 1240
CARD C 2                                                                CIT 1250
C                                                                       CIT 1260
      READ (NSU,102) IOPT                                               CIT 1270
C                                                                       CIT 1280
      WRITE (NCIT,102) IOPT                                             CIT 1290
      IF(JTPE .EQ. 1) WRITE (NSR,102) IOPT                              CIT 1300
C                                                                       CIT 1310
CARDS C 3 / C 4 / C5                                                    CIT 1320
C                                                                       CIT 1330
      DO 2 J=1,3                                                        CIT 1340
        DO 500 I=1,24                                                   CIT 1350
          N1(I) = 0                                                     CIT 1360
  500   CONTINUE                                                        CIT 1370
        IF(J .NE. 1) GOTO 505                                           CIT 1380
        IF(JTPE7 .GT. 0) N1(2) = -1                                     CIT 1390
        N1(3) = 1                                                       CIT 1400
        N1(19) = 1                                                      CIT 1410
  505   CONTINUE                                                        CIT 1420
C                                                                       CIT 1430
        IF(J .EQ. 1) READ (NSU,102) N1(10),N1(15),N1(24)                CIT 1440
C                                                                       CIT 1450
        IF(J .EQ. 1 .AND. JTPE .EQ. 1) WRITE (NSR,102) N1(10),N1(15),   CIT 1460
     1   N1(24)                                                         CIT 1470
C                                                                       CIT 1480
        IF(J .EQ. 2) READ (NSU,102) (N1(I),I=3,6),(N1(I),I=9,10),N1(12) CIT 1490
C                                                                       CIT 1500
        IF(J .NE. 2) GOTO 150                                           CIT 1510
        N1(14) = N1(10)                                                 CIT 1520
        NN1 = 0                                                         CIT 1530
        NN14 = 0                                                        CIT 1540
        IF(PDTHX .GT. 0.) N1(14) = 2                                    CIT 1550
        IF(PDTHX .GT. 0.) N1(10) = 2                                    CIT 1560
        IF(N1(10) .EQ. 2) NN1 = 2                                       CIT 1570
        IF(N1(14) .EQ. 2) NN14 = 2                                      CIT 1580
        IF(NN14 .EQ. 2 .AND. PDTHX .EQ. 0.) OPEN(80,FILE='powform')     CIT 1590
        IF(NN1 .EQ. 2 .AND. PDTHX .EQ. 0.) OPEN(68,FILE='phiform')      CIT 1600
  150   CONTINUE                                                        CIT 1610
        IF(J .EQ. 2 .AND. JTPE .EQ. 1) WRITE (NSR,102) (N1(I),I=3,6),   CIT 1620
     1   (N1(I),I=9,10),N1(12),N1(14)                                   CIT 1630
C                                                                       CIT 1640
        IF(J .EQ. 3) READ (NSU,102) N1(1),(N1(I),I=19,20)               CIT 1650
C                                                                       CIT 1660
        IF(J .EQ. 3 .AND. JTPE .EQ. 1) WRITE (NSR,102) N1(1),(N1(I),I=19CIT 1670
     1   ,20)                                                           CIT 1680
        IF(J .EQ. 1 .AND. INGC24 .GT. 0) N1(24) = 0                     CIT 1690
        WRITE (NCIT,102) (N1(I),I=1,24)                                 CIT 1700
        IF(J .EQ. 1) NANI = N1(24)                                      CIT 1710
        IF(J .EQ. 1) NFS = N1(10)                                       CIT 1720
    2 CONTINUE                                                          CIT 1730
C                                                                       CIT 1740
CARD C 6                                                                CIT 1750
C                                                                       CIT 1760
      DO 501 I=1,6                                                      CIT 1770
        V(I) = 0.                                                       CIT 1780
  501 CONTINUE                                                          CIT 1790
C                                                                       CIT 1800
      READ (NSU,103) (V(I),I=1,2)                                       CIT 1810
C                                                                       CIT 1820
      IF(JTPE .EQ. 1) WRITE (NSR,103) (V(I),I=1,2)                      CIT 1830
      WRITE (NCIT,103) (V(I),I=1,6)                                     CIT 1840
C                                                                       CIT 1850
C     SECTION 003: DESCRIPTION OF NEUTRON FLUX PROBLEM                  CIT 1860
C                                                                       CIT 1870
CARD C 7                                                                CIT 1880
C                                                                       CIT 1890
      READ (NSU,102) IOPT                                               CIT 1900
C                                                                       CIT 1910
      WRITE (NCIT,102) IOPT                                             CIT 1920
      IF(JTPE .EQ. 1) WRITE (NSR,102) IOPT                              CIT 1930
C                                                                       CIT 1940
CARD C 8                                                                CIT 1950
C                                                                       CIT 1960
      DO 503 I=1,24                                                     CIT 1970
        N1(I) = 0                                                       CIT 1980
  503 CONTINUE                                                          CIT 1990
C                                                                       CIT 2000
      READ (NSU,110) (N1(I),I=11,20),N1(23)                             CIT 2010
C                                                                       CIT 2020
      IF(I3D .EQ. 0) N1(5) =  7                                         CIT 2030
      IF(I3D .EQ. 1) N1(5) = 11                                         CIT 2040
      IF(I3D .EQ. 2) N1(5) = 12                                         CIT 2050
      IF(JTPE .EQ. 1) WRITE (NSR,110) (N1(I),I=11,20),N1(23)            CIT 2060
      WRITE (NCIT,111) (N1(I),I=1,24)                                   CIT 2070
C                                                                       CIT 2080
CARD C 9                                                                CIT 2090
C                                                                       CIT 2100
      DO 504 I=3,6                                                      CIT 2110
        V(I) = 0.                                                       CIT 2120
  504 CONTINUE                                                          CIT 2130
C                                                                       CIT 2140
      READ (NSU,103) (V(I),I=1,2)                                       CIT 2150
C                                                                       CIT 2160
      IF(JTPE .EQ. 1) WRITE (NSR,103) (V(I),I=1,2)                      CIT 2170
      WRITE (NCIT,103) (V(I),I=1,6)                                     CIT 2180
C                                                                       CIT 2190
CARD C 10                                                               CIT 2200
C                                                                       CIT 2210
      READ (NSU,103) (V(I),I=1,2),V(6)                                  CIT 2220
C                                                                       CIT 2230
      V(3) = POWER * 1.E-6                                              CIT 2240
      V(4) = 1. / FIWATT                                                CIT 2250
      V(5) = 1.                                                         CIT 2260
      IF(JTPE .EQ. 1) WRITE (NSR,103) (V(I),I=1,2),V(6)                 CIT 2270
      WRITE (NCIT,103) (V(I),I=1,6)                                     CIT 2280
      IF(I3D) 30,30,35                                                  CIT 2290
   30 CONTINUE                                                          CIT 2300
C                                                                       CIT 2310
C     SECTION 004: GEOMETRIC MESH DESCRIPTION (2-D,R-Z) FROM UNIT NGEOM CIT 2320
C                                                                       CIT 2330
CARD C4-1                                                               CIT 2340
C                                                                       CIT 2350
      READ (NGEOM,102) IOPT,IRI,ND                                      CIT 2360
C                                                                       CIT 2370
      WRITE (NCIT,102) IOPT                                             CIT 2380
C                                                                       CIT 2390
CARDS C4-2 (RADIAL)                                                     CIT 2400
C                                                                       CIT 2410
      IR = IRI - 1                                                      CIT 2420
C                                                                       CIT 2430
      READ (NGEOM,104) (N1(I),V(I),I=1,IR)                              CIT 2440
C                                                                       CIT 2450
      WRITE (NCIT,104) (N1(I),V(I),I=1,IR)                              CIT 2460
      IF(MOD(IR,6) .EQ. 0) WRITE (NCIT,104) (N0(I),V0(I),I=1,6)         CIT 2470
C                                                                       CIT 2480
CARDS C4-2 (AXIAL)                                                      CIT 2490
C                                                                       CIT 2500
      READ (NGEOM,104) (N1(I),V(I),I=1,ND)                              CIT 2510
C                                                                       CIT 2520
      WRITE (NCIT,104) (N1(I),V(I),I=1,ND)                              CIT 2530
      IF(MOD(ND,6) .EQ. 0) WRITE (NCIT,104) (N0(I),V0(I),I=1,6)         CIT 2540
C                                                                       CIT 2550
C     SECTION 005: ZONE PLACEMENT (2-D,R-Z) FROM UNIT NGEOM             CIT 2560
C                                                                       CIT 2570
CARD C5-1                                                               CIT 2580
C                                                                       CIT 2590
      READ (NGEOM,102) IOPT,IR,N                                        CIT 2600
C                                                                       CIT 2610
      WRITE (NCIT,102) IOPT                                             CIT 2620
C                                                                       CIT 2630
CARDS C5-2                                                              CIT 2640
C                                                                       CIT 2650
      DO 4 I=1,N                                                        CIT 2660
C                                                                       CIT 2670
        READ (NGEOM,107) (N1(J),J=1,IR)                                 CIT 2680
C                                                                       CIT 2690
        WRITE (NCIT,107) (N1(J),J=1,IR)                                 CIT 2700
    4 CONTINUE                                                          CIT 2710
C                                                                       CIT 2720
C     MESH-POINTS AT CORE-REFLECTOR EDGES                               CIT 2730
C                                                                       CIT 2740
      READ (NGEOM,102) JIR,JRR,ITR,IBR                                  CIT 2750
      GOTO 40                                                           CIT 2760
   35 CONTINUE                                                          CIT 2770
C                                                                       CIT 2780
C     SECTION 004: GEOMETRIC MESH DESCRIPTION (3-D: X-Y-Z,THETA-R-Z)    CIT 2790
C                  FROM UNIT NGEOM                                      CIT 2800
C                                                                       CIT 2810
CARD C4-1                                                               CIT 2820
C                                                                       CIT 2830
      READ (NGEOM,112) IOPT,IMX,JMY,KMZ,N200TR                          CIT 2840
C                                                                       CIT 2850
      WRITE (NCIT,102) IOPT                                             CIT 2860
C                                                                       CIT 2870
CARDS C4-2 (X-RICHTUNG ODER THETA)                                      CIT 2880
C                                                                       CIT 2890
      READ (NGEOM,108) (N1(I),V(I),I=1,IMX)                             CIT 2900
C                                                                       CIT 2910
      WRITE (NCIT,108) (N1(I),V(I),I=1,IMX)                             CIT 2920
      IF(MOD(IMX,6) .EQ. 0) WRITE (NCIT,108) (N0(I),V0(I),I=1,6)        CIT 2930
C                                                                       CIT 2940
CCARDS C4-2 (Y-RICHTUNG ODER R)                                         CIT 2950
C                                                                       CIT 2960
      READ (NGEOM,108) (N1(I),V(I),I=1,JMY)                             CIT 2970
C                                                                       CIT 2980
      WRITE (NCIT,108) (N1(I),V(I),I=1,JMY)                             CIT 2990
      IF(MOD(JMY,6) .EQ. 0) WRITE (NCIT,108) (N0(I),V0(I),I=1,6)        CIT 3000
C                                                                       CIT 3010
CARDS C4-2 (Z-RICHTUNG)                                                 CIT 3020
C                                                                       CIT 3030
      READ (NGEOM,108) (N1(I),V(I),I=1,KMZ)                             CIT 3040
C                                                                       CIT 3050
      WRITE (NCIT,108) (N1(I),V(I),I=1,KMZ)                             CIT 3060
      IF(MOD(KMZ,6) .EQ. 0) WRITE (NCIT,108) (N0(I),V0(I),I=1,6)        CIT 3070
C                                                                       CIT 3080
C     SECTION 005: ZONE PLACEMENT (3-D,X-Y-Z) FROM UNIT NGEOM           CIT 3090
C                                                                       CIT 3100
CARD C5-1                                                               CIT 3110
C                                                                       CIT 3120
      READ (NGEOM,102) IOPT                                             CIT 3130
C                                                                       CIT 3140
      WRITE (NCIT,102) IOPT                                             CIT 3150
C                                                                       CIT 3160
CARDS C5-2                                                              CIT 3170
C                                                                       CIT 3180
      DO 36 K=1,KMZ                                                     CIT 3190
        DO 36 J=1,JMY                                                   CIT 3200
C                                                                       CIT 3210
          READ (NGEOM,107) (N1(I),I=1,IMX)                              CIT 3220
C                                                                       CIT 3230
          WRITE (NCIT,107) (N1(I),I=1,IMX)                              CIT 3240
   36 CONTINUE                                                          CIT 3250
   40 CONTINUE                                                          CIT 3260
      IF(NANI .NE. -1) GOTO 15                                          CIT 3270
C                                                                       CIT 3280
CARD C 11                                                               CIT 3290
C                                                                       CIT 3300
      READ (NSU,100) JH,KH                                              CIT 3310
      IF(JTPE .EQ. 1) WRITE (NSR,100) JH,KH                             CIT 3320
C                                                                       CIT 3330
CARD C 12                                                               CIT 3340
C                                                                       CIT 3350
      READ (NSU,109) (IZONE(J),M1(J,1),M1(J,2),J=1,JH)                  CIT 3360
      IF(JTPE .EQ. 1) WRITE (NSR,109) (IZONE(J),M1(J,1),M1(J,2),J=1,JH) CIT 3370
C                                                                       CIT 3380
      DO 17 K=1,KH                                                      CIT 3390
C                                                                       CIT 3400
CARD C 13                                                               CIT 3410
C                                                                       CIT 3420
        READ (NSU,103) (RDK(K,I),I=1,N26)                               CIT 3430
        IF(JTPE .EQ. 1) WRITE (NSR,103) (RDK(K,I),I=1,N26)              CIT 3440
C                                                                       CIT 3450
        DO 50 I=2,N26                                                   CIT 3460
          IF(RDK(K,I) .EQ. 0.) RDK(K,I) = RDK(K,I-1)                    CIT 3470
   50   CONTINUE                                                        CIT 3480
C                                                                       CIT 3490
CARD C 14                                                               CIT 3500
C                                                                       CIT 3510
        READ (NSU,103) (SGA(K,I),I=1,N26)                               CIT 3520
        IF(JTPE .EQ. 1) WRITE (NSR,103) (SGA(K,I),I=1,N26)              CIT 3530
C                                                                       CIT 3540
CARD C 15                                                               CIT 3550
C                                                                       CIT 3560
        DO 22 N=1,N26                                                   CIT 3570
          READ (NSU,103) (SGTR(K,N,I),I=1,N26)                          CIT 3580
          IF(JTPE .EQ. 1) WRITE (NSR,103) (SGTR(K,N,I),I=1,N26)         CIT 3590
   22   CONTINUE                                                        CIT 3600
C                                                                       CIT 3610
CARD C 16                                                               CIT 3620
C                                                                       CIT 3630
        READ (NSU,105) V2(K,1),V2(K,2),V2(K,3),IKEN                     CIT 3640
        IF(JTPE .EQ. 1) WRITE (NSR,105) V2(K,1),V2(K,2),V2(K,3),IKEN    CIT 3650
C                                                                       CIT 3660
        DO 21 J=1,N26                                                   CIT 3670
          FKEN(K,J) = 1.                                                CIT 3680
   21   CONTINUE                                                        CIT 3690
C                                                                       CIT 3700
CARD C 17                                                               CIT 3710
C                                                                       CIT 3720
        IF(IKEN .GT. 0) READ (NSU,103) (FKEN(K,I),I=1,N26)              CIT 3730
        IF(IKEN .GT. 0 .AND. JTPE .EQ. 1) WRITE (NSR,103) (FKEN(K,I),I=1CIT 3740
     1   ,N26)                                                          CIT 3750
C                                                                       CIT 3760
   17 CONTINUE                                                          CIT 3770
CFZJ009 Calculation of V1 moved to later                      01.12.03  CIT 3780
      KK = JH + 1                                                       CIT 3790
      M1(KK,1) = 0                                                      CIT 3800
   15 CONTINUE                                                          CIT 3810
      GOTO 20                                                           CIT 3820
   10 CONTINUE                                                          CIT 3830
      REWIND NCIT                                                       CIT 3840
      IF(MUHU(I9) .GT. 2) GOTO 11                                       CIT 3850
      READ (NCIT,101) B(1)                                              CIT 3860
   11 CONTINUE                                                          CIT 3870
      READ (NCIT,102) IOPT                                              CIT 3880
      READ (NCIT,102) (N1(I),I=1,72)                                    CIT 3890
      IF(PDTHX .GT. 0.) N1(38) = 2                                      CIT 3900
      IF(PDTHX .GT. 0.) N1(34) = 2                                      CIT 3910
CFZJ057                                                       07.11.07  CIT 3920
      IF(PDTHX .EQ. 0. .AND. NTIK .GT. 0) N1(38) = 0                    CIT 3930
      IF(PDTHX .EQ. 0. .AND. NTIK .GT. 0) N1(34) = 0                    CIT 3940
C                                                                       CIT 3950
CARD R32 (SAME AS CARD C 4 (REDEFINITION OF "EDIT OPTIONS"))            CIT 3960
C                                                                       CIT 3970
      IF(IVSP(11) .EQ. 2) READ (NS,102) (N1(I),I=27,30),(N1(I),I=33,34),CIT 3980
     1 N1(36)                                                           CIT 3990
C                                                                       CIT 4000
      N1(38) = N1(34)                                                   CIT 4010
      IF(IVSP(11) .EQ. 2) IVSP(11) = 1                                  CIT 4020
      IF(N1(34) .EQ. 2 .AND. PDTHX .EQ. 0.) OPEN(68,FILE='phiform')     CIT 4030
      IF(N1(38) .EQ. 2 .AND. PDTHX .EQ. 0.) OPEN(80,FILE='powform')     CIT 4040
      NN1 = 0                                                           CIT 4050
      NN14 = 0                                                          CIT 4060
      N1(2) = -1                                                        CIT 4070
      READ (NCIT,103) (V(I),I=1,6)                                      CIT 4080
      REWIND NCIT                                                       CIT 4090
      WRITE (NCIT,102) IOPT                                             CIT 4100
      WRITE (NCIT,102) (N1(I),I=1,72)                                   CIT 4110
      WRITE (NCIT,103) (V(I),I=1,6)                                     CIT 4120
C                                                                       CIT 4130
C     REDEFINITION OF "POWER" AND "1/FIWATT")                           CIT 4140
C                                                                       CIT 4150
      XMIS(3) = POWER * 1.E-6                                           CIT 4160
      SPARE(100) = XMIS(3)                                              CIT 4170
      XMIS(4) = 1. / FIWATT                                             CIT 4180
   20 CONTINUE                                                          CIT 4190
C                                                                       CIT 4200
C     SECTION 008: MACROSCOPIC CROSS SECTIONS, PREPARED IN SUBROUTINE   CIT 4210
C                  *MACROC*                                             CIT 4220
C                                                                       CIT 4230
CARD C8-1                                                               CIT 4240
C                                                                       CIT 4250
      IOPT = 8                                                          CIT 4260
C                                                                       CIT 4270
      WRITE (NCIT,102) IOPT                                             CIT 4280
C                                                                       CIT 4290
CARD C8-2                                                               CIT 4300
C                                                                       CIT 4310
      N1(1) = N26                                                       CIT 4320
      N1(2) = N26 - 1                                                   CIT 4330
      N1(3) = 0                                                         CIT 4340
C                                                                       CIT 4350
      WRITE (NCIT,102) (N1(I),I=1,3)                                    CIT 4360
C                                                                       CIT 4370
CARDS C8-3 / C8-4                                                       CIT 4380
C                                                                       CIT 4390
      CALL MACROC(A(KA(LOUSI)),KL(LOUSI),A(KA(LTOSI)),A(KA(LABSI)),     CIT 4400
     1 A(KA(LFISI)),KL(LFISI),A(KA(LXNU)),A(KA(LIREG)),A(KA(LRPHI)),    CIT 4410
     2 A(KA(LNBUR)),A(KA(LSSLU)),A(KA(LNHOT)),A(KA(LVOL)),A(KA(LVPAR)), CIT 4420
     3 A(KA(LDEN)),SS,A(KA(LFISM)),A(KA(LNRG)),SGA,SGTR,A(KA(LXSIS)),   CIT 4430
     4 A(KA(LXTOT)),A(KA(LXSIA)),A(KA(LXNFI)),A(KA(LCNU)),A(NP1),A(NP2),CIT 4440
     5 A(NP3),A(NP5),A(NP6),A(NP7),A(NP8),A(NP9),A(NP10),A(KA(LTRSI)),  CIT 4450
     6 KL(LTRSI),A(KA(LSCTR)),A(KA(LXSIG)),A(KA(LNOPK)))                CIT 4460
C                                                                       CIT 4470
CARD C8-5                                                               CIT 4480
C                                                                       CIT 4490
      FSPT = 0.                                                         CIT 4500
      DO 5 I=1,N26                                                      CIT 4510
        FSPT = FSPT + FSP(I)                                            CIT 4520
    5 CONTINUE                                                          CIT 4530
      IF(FSPT .EQ. 0.) FSP(1) = 1.                                      CIT 4540
      DO 6 I=1,N26                                                      CIT 4550
        V(I) = FSP(I)                                                   CIT 4560
    6 CONTINUE                                                          CIT 4570
C                                                                       CIT 4580
      WRITE (NCIT,103) (V(I),I=1,N26)                                   CIT 4590
C                                                                       CIT 4600
C     SECTION 026: FIXED SOURCE DATA                                    CIT 4610
C                                                                       CIT 4620
      IF(NFS .NE. -5) GOTO 13                                           CIT 4630
      IF(MUHU(I9) .GT. 1) GOTO 12                                       CIT 4640
C                                                                       CIT 4650
CARD C 18                                                               CIT 4660
C                                                                       CIT 4670
      READ (NSU,102) IOPTF                                              CIT 4680
C                                                                       CIT 4690
      IF(JTPE .EQ. 1) WRITE (NSR,102) IOPTF                             CIT 4700
C                                                                       CIT 4710
CARD C 19                                                               CIT 4720
C                                                                       CIT 4730
      READ (NSU,102) N1F(2)                                             CIT 4740
C                                                                       CIT 4750
      IF(JTPE .EQ. 1) WRITE (NSR,102) N1F(2)                            CIT 4760
      N1F(1) = -1                                                       CIT 4770
C                                                                       CIT 4780
CARD C 20                                                               CIT 4790
C                                                                       CIT 4800
      READ (NSU,103) (V1F(I),I=1,N26)                                   CIT 4810
C                                                                       CIT 4820
      IF(JTPE .EQ. 1) WRITE (NSR,103) (V1F(I),I=1,N26)                  CIT 4830
C                                                                       CIT 4840
CARD C 21                                                               CIT 4850
C                                                                       CIT 4860
      NA = 1                                                            CIT 4870
    9 CONTINUE                                                          CIT 4880
      NE = NA + 5                                                       CIT 4890
C                                                                       CIT 4900
      READ (NSU,106) (N2F(I),V2F(I),I=NA,NE)                            CIT 4910
C                                                                       CIT 4920
      IF(JTPE .EQ. 1) WRITE (NSU,106) (N2F(I),V2F(I),I=NA,NE)           CIT 4930
      NA = NE + 1                                                       CIT 4940
      IF(N2F(NE) .GT. 0) GOTO 9                                         CIT 4950
   12 CONTINUE                                                          CIT 4960
      WRITE (NCIT,102) IOPTF                                            CIT 4970
      WRITE (NCIT,102) (N1F(I),I=1,2)                                   CIT 4980
      WRITE (NCIT,103) (V1F(I),I=1,N26)                                 CIT 4990
      WRITE (NCIT,106) (N2F(I),V2F(I),I=1,NE)                           CIT 5000
   13 CONTINUE                                                          CIT 5010
C                                                                       CIT 5020
C     ABSCHLUSS CITATION-EINGABE                                        CIT 5030
C                                                                       CIT 5040
CARD C9-1                                                               CIT 5050
C                                                                       CIT 5060
      IOPT = 999                                                        CIT 5070
      WRITE (NCIT,102) IOPT                                             CIT 5080
      IF(NANI .NE. -1) GOTO 19                                          CIT 5090
CFZJ009 Calculation of V1 moved here                          01.12.03  CIT 5100
      DO 16 J=1,JH                                                      CIT 5110
        K = IZONE(J)                                                    CIT 5120
        DO 16 I=1,N26                                                   CIT 5130
          DO 16 N=1,MM1                                                 CIT 5140
            V1(J,I,N) = V2(K,N) * FKEN(K,I)                             CIT 5150
   16 CONTINUE                                                          CIT 5160
      DO 18 K=1,KK                                                      CIT 5170
        M1(K,3) = 1                                                     CIT 5180
        M1(K,4) = MM1                                                   CIT 5190
        WRITE (NCIT,109) (M1(K,I),I=1,4)                                CIT 5200
        IF(K .EQ. KK) GOTO 18                                           CIT 5210
        DO 14 J=1,MM1                                                   CIT 5220
          WRITE (NCIT,103) (V1(K,I,J),I=1,N26)                          CIT 5230
   14   CONTINUE                                                        CIT 5240
   18 CONTINUE                                                          CIT 5250
   19 CONTINUE                                                          CIT 5260
      REWIND NCIT                                                       CIT 5270
C                                                                       CIT 5280
C     AUFRUF CITATION                                                   CIT 5290
C                                                                       CIT 5300
      MU4 = MUHU(I9)                                                    CIT 5310
C                                                                       CIT 5320
      CALL MAICIT                                                       CIT 5330
C                                                                       CIT 5340
  400 CONTINUE                                                          CIT 5350
C                                                                       CIT 5360
      CALL DENOUT(A(KA(LIREG)),A(KA(LCFRA)),A(KA(LPOIL)),A(KA(LNPOI)),  CIT 5370
     1 A(KA(LVOL)),A(KA(LDEN)),A(KA(LHM)),A(KA(LTHBU)),A(KA(LIMAT)),    CIT 5380
     2 A(KA(LAWT)),A(KA(LGM)),A(KA(LGS)))                               CIT 5390
C                                                                       CIT 5400
      NP11 = NEND10                                                     CIT 5410
C                                                                       CIT 5420
      CALL MACRO2(SS,A(KA(LFISM)),A(KA(LNRG)),NEND10,NEND11,A(NP11))    CIT 5430
C                                                                       CIT 5440
      KMAT2 = KMAT + 2                                                  CIT 5450
      N261 = N26 + 1                                                    CIT 5460
      IF(N26 .LT. 4) N261 = 5                                           CIT 5470
      NP12 = NEND11                                                     CIT 5480
      NP13 = NP12 + N261                                                CIT 5490
      NP14 = NP13 + N26                                                 CIT 5500
      NP15 = NP13 + N26 * 2                                             CIT 5510
      NP16 = NP13 + N26 * 3                                             CIT 5520
      NP17 = NP13 + N26 * 4                                             CIT 5530
      NP18 = NP13 + N26 * 5                                             CIT 5540
      NP19 = NP13 + N26 * 6                                             CIT 5550
      NP20 = NP13 + N26 * 7                                             CIT 5560
      NP21 = NP13 + N26 * 8                                             CIT 5570
      NEND21 = NP21 + N26                                               CIT 5580
      N266 = N26 * 2 + 2                                                CIT 5590
      IF(ABS(ZKFIND-REACT(2)) .LT. SERCON) IEND = 1                     CIT 5600
C                                                                       CIT 5610
      CALL BALH(A(KA(LOUSI)),KL(LOUSI),A(KA(LTOSI)),A(KA(LABSI)),       CIT 5620
     1 A(KA(LFISI)),KL(LFISI),A(KA(LIREG)),A(KA(LNBUR)),A(KA(LSSLU)),   CIT 5630
     2 A(KA(LNHOT)),A(KA(LVOL)),A(KA(LDEN)),A(KA(LFISM)),A(KA(LNRG)),   CIT 5640
     3 A(KA(LNOPO)),A(KA(LBUCK)),A(KA(LRPH)),A(KA(LBUK)),A(KA(LB)),     CIT 5650
     4 A(NP12),A(NP13),A(NP14),A(NP15),A(NP16),A(NP17),A(NP18),A(NP19), CIT 5660
     5 A(NP20),A(NP21),NEND21,N266,KMAT2,N261,A(KA(LZLTH)),A(KA(LIMAT)),CIT 5670
     6 A(KA(LPHIN)),A(KA(LTRSI)),KL(LTRSI))                             CIT 5680
C                                                                       CIT 5690
      IF(I3D .EQ. 0 .AND. MUHU(28) .LE. 0) CALL RAFLUC                  CIT 5700
C                                                                       CIT 5710
      GOTO 99                                                           CIT 5720
  300 CONTINUE                                                          CIT 5730
C                                                                       CIT 5740
      CALL MACROC(A(KA(LOUSI)),KL(LOUSI),A(KA(LTOSI)),A(KA(LABSI)),     CIT 5750
     1 A(KA(LFISI)),KL(LFISI),A(KA(LXNU)),A(KA(LIREG)),A(KA(LRPHI)),    CIT 5760
     2 A(KA(LNBUR)),A(KA(LSSLU)),A(KA(LNHOT)),A(KA(LVOL)),A(KA(LVPAR)), CIT 5770
     3 A(KA(LDEN)),SS,A(KA(LFISM)),A(KA(LNRG)),SGA,SGTR,A(KA(LXSIS)),   CIT 5780
     4 A(KA(LXTOT)),A(KA(LXSIA)),A(KA(LXNFI)),A(KA(LCNU)),A(NP1),A(NP2),CIT 5790
     5 A(NP3),A(NP5),A(NP6),A(NP7),A(NP8),A(NP9),A(NP10),A(KA(LTRSI)),  CIT 5800
     6 KL(LTRSI),A(KA(LSCTR)),A(KA(LXSIG)),A(KA(LNOPK)))                CIT 5810
C                                                                       CIT 5820
      CALL MACRO3(A(KA(LRPHI)),A(KA(LVOL)),SS,A(KA(LFISM)),A(KA(LPHIN)))CIT 5830
C                                                                       CIT 5840
   99 RETURN                                                            CIT 5850
      END                                                               CIT 5860
      SUBROUTINE MACROC(OUSIG,LA0,TOSIG,ABSIG,FISIG,LF0,XNU,IREG,RPHIAV,ACR   10
     1 NBURN,SSLUMP,NHOT,VOL,VPART,DEN,SS,FISMAC,NRG,SGA,SGTR,XSIGS,    ACR   20
     2 XTOTL,XSIGA,XNFIS,CNU,DS,SINK,SIGNUF,ENU,XSIG,SCOU,GI,GX,SCAT,   ACR   30
     3 TRSIG,LT0,SCTR,XSIGTR,NOPOWK)                                    ACR   40
C                                                                       ACR   50
C     PREPARING THE MACROSCOPIC CROSS SECTIONS FOR CITATION             ACR   60
C                                                                       ACR   70
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    ACR   80
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    ACR   90
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIACR  100
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D,NLAYP,ITTT, ACR  110
     4 LIMT                                                             ACR  120
C                                                                       ACR  130
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1ACR  140
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         ACR  150
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    ACR  160
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)ACR  170
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  ACR  180
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    ACR  190
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         ACR  200
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,ACR  210
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            ACR  220
C                                                                       ACR  230
      COMMON /NORM/ PDUMMY,IMX,ICI(9999),IVS(9999),VFV(9999),VFC(9999)  ACR  240
C                                                                       ACR  250
      COMMON /MU/ MU4,NCIT,NGEOM,I9,IOM,M34                             ACR  260
C                                                                       ACR  270
CFZJ010 Increase dimensions in COMMON ANISO                   01.12.03  ACR  280
      COMMON /ANISO/ JH,IZONE(200),RDK(20,96),KK,M1(201,4),NANI,V2(20,3)ACR  290
C                                                                       ACR  300
CFZJ056                                                       06.11.07  ACR  310
      COMMON /FSD/ NE,IOPTF,N1F(2),N2F(50),V1F(33),V2F(50),NFS          ACR  320
C                                                                       ACR  330
      COMMON /FLUXN/ D(361),IACT                                        ACR  340
C                                                                       ACR  350
      COMMON /TRN200C/ N200TR                                           ACR  360
C                                                                       ACR  370
CFZJ011 Increase dimensions of arrays SGA, SGTR, FKEN         01.12.03  ACR  380
      DIMENSION OUSIG(LA0),TOSIG(LA0),ABSIG(LA0),FISIG(LF0),XNU(LF0),   ACR  390
     1 IREG(NDR),RPHIAV(N26,NDR),SSLUMP(N26,NDR),NHOT(N200),VOL(N200),  ACR  400
     2 VPART(N200),DEN(KMAT,N200),SS(N26,N200),FISMAC(N26,N200),        ACR  410
     3 NRG(N200),SGA(20,N26),SGTR(20,N26,N26),DS(N200,N26),             ACR  420
     4 SINK(N200,N26),SIGNUF(N200,N26),CNU(KROT,N26),ENU(N200,N26),     ACR  430
     5 XSIG(N200,N26),SCOU(N200,N26),XSIGS(KROT,N26),XTOTL(KROT,N26),   ACR  440
     6 XSIGA(KROT,N26),XNFIS(KROT,N26),GI(N200),GX(N200),SCAT(N26),     ACR  450
     7 SSS(N26,KROT),PHIN(N26),TRSIG(LT0),SCTR(NDR,LIMT),               ACR  460
     8 XSIGTR(KROT,LIMT),NOPOWK(KROT)                                   ACR  470
C                                                                       ACR  480
  102 FORMAT (6(1PE12.5))                                               ACR  490
  103 FORMAT (2I6,5(1PE12.5))                                           ACR  500
C                                                                       ACR  510
C                                                                       ACR  520
C     ERSTELLEN MAKROSKOP. WQ + SPALTRATE IN IR                         ACR  530
C                                                                       ACR  540
      ANUL = 0.                                                         ACR  550
      DO 20 M=1,N200                                                    ACR  560
        GI(M) = 0.                                                      ACR  570
        GX(M) = 0.                                                      ACR  580
        DO 20 I=1,N26                                                   ACR  590
          DS(M,I) = 0.                                                  ACR  600
          SINK(M,I) = 0.                                                ACR  610
          SIGNUF(M,I) = 0.                                              ACR  620
          SCOU(M,I) = 0.                                                ACR  630
          ENU(M,I) = 0.                                                 ACR  640
          XSIG(M,I) = 0.                                                ACR  650
          FISMAC(I,M) = 0.                                              ACR  660
   20 CONTINUE                                                          ACR  670
      DO 21 M=1,NDR                                                     ACR  680
        DO 21 I=1,LIMT                                                  ACR  690
          SCTR(M,I) = 0.                                                ACR  700
   21 CONTINUE                                                          ACR  710
      NRO = IACT + 2 + NO                                               ACR  720
      NRL = NRO + NLUM                                                  ACR  730
      DO 10 M=1,NDR                                                     ACR  740
        DO 11 IR=1,N200                                                 ACR  750
          IF(NRG(IR) .NE. M) GOTO 11                                    ACR  760
          DO 12 I=1,N26                                                 ACR  770
            SSFAC = 1.                                                  ACR  780
            DO 13 L=1,KMAT                                              ACR  790
              IL = ((NHOT(IR)-1)*KMAT+(L-1)) * N26                      ACR  800
              QLUM = 1.                                                 ACR  810
              IF(L .GT. NRO .AND. L .LE. NRL) QLUM = SSLUMP(I,IR)       ACR  820
              Q1 = DEN(L,IR) * SSFAC * QLUM                             ACR  830
              Q = Q1 * VPART(IR)                                        ACR  840
              IEL = I + IL                                              ACR  850
              IF(MUHU(2) .EQ. 1) GOTO 14                                ACR  860
              DS(M,I) = DS(M,I) + Q * TOSIG(IEL)                        ACR  870
              SINK(M,I) = SINK(M,I) + Q * ABSIG(IEL)                    ACR  880
              SCOU(M,I) = SCOU(M,I) + Q * OUSIG(IEL)                    ACR  890
   14         CONTINUE                                                  ACR  900
              IF(L .GT. IACT) GOTO 13                                   ACR  910
              LF = ((NHOT(IR)-1)*IACT+(L-1)) * N26                      ACR  920
              QQ = Q1 * FISIG(I+LF)                                     ACR  930
              QF = XNU(I+LF)                                            ACR  940
              IF(QF .GT. 0.) QF = QQ / QF                               ACR  950
              FISMAC(I,IR) = FISMAC(I,IR) + QF                          ACR  960
              SIGNUF(M,I) = SIGNUF(M,I) + QQ * VPART(IR)                ACR  970
              ENU(M,I) = ENU(M,I) + QF * VPART(IR)                      ACR  980
   13       CONTINUE                                                    ACR  990
   12     CONTINUE                                                      ACR 1000
          DO 16 I=1,LIMT                                                ACR 1010
            DO 17 L=1,KMAT                                              ACR 1020
              IL = ((NHOT(IR)-1)*KMAT+(L-1)) * LIMT                     ACR 1030
              QLUM = 1.                                                 ACR 1040
              IF(L .GT. NRO .AND. L .LE. NRL) QLUM = SSLUMP(I,IR)       ACR 1050
              Q1 = DEN(L,IR) * SSFAC * QLUM                             ACR 1060
              Q = Q1 * VPART(IR)                                        ACR 1070
              IEL = I + IL                                              ACR 1080
              IF(MUHU(2) .EQ. 1) GOTO 17                                ACR 1090
              SCTR(M,I) = SCTR(M,I) + Q * TRSIG(IEL)                    ACR 1100
   17       CONTINUE                                                    ACR 1110
   16     CONTINUE                                                      ACR 1120
   11   CONTINUE                                                        ACR 1130
   10 CONTINUE                                                          ACR 1140
      DO 110 N=1,KROT                                                   ACR 1150
        DO 109 I=1,N26                                                  ACR 1160
          XTOTL(N,I) = 0.                                               ACR 1170
          XSIGA(N,I) = 0.                                               ACR 1180
          XSIGS(N,I) = 0.                                               ACR 1190
          XNFIS(N,I) = 0.                                               ACR 1200
          CNU(N,I) = 0.                                                 ACR 1210
  109   CONTINUE                                                        ACR 1220
        DO 108 I=1,LIMT                                                 ACR 1230
          XSIGTR(N,I) = 0.                                              ACR 1240
  108   CONTINUE                                                        ACR 1250
  110 CONTINUE                                                          ACR 1260
      DO 111 K=1,IMX                                                    ACR 1270
        IC = ICI(K)                                                     ACR 1280
        IV = IVS(K)                                                     ACR 1290
        DO 112 I=1,N26                                                  ACR 1300
          XTOTL(IC,I) = XTOTL(IC,I) + VFC(K) * DS(IV,I)                 ACR 1310
          XSIGA(IC,I) = XSIGA(IC,I) + VFC(K) * SINK(IV,I)               ACR 1320
          XSIGS(IC,I) = XSIGS(IC,I) + VFC(K) * SCOU(IV,I)               ACR 1330
          XNFIS(IC,I) = XNFIS(IC,I) + VFC(K) * SIGNUF(IV,I)             ACR 1340
          CNU(IC,I) = CNU(IC,I) + VFC(K) * ENU(IV,I)                    ACR 1350
  112   CONTINUE                                                        ACR 1360
        DO 113 I=1,LIMT                                                 ACR 1370
          XSIGTR(IC,I) = XSIGTR(IC,I) + VFC(K) * SCTR(IV,I)             ACR 1380
  113   CONTINUE                                                        ACR 1390
  111 CONTINUE                                                          ACR 1400
      IF(MUHU(2) .EQ. 1) GOTO 99                                        ACR 1410
      IF(I3D .EQ. 0) GOTO 32                                            ACR 1420
      DO 33 I=1,N200TR                                                  ACR 1430
        NOPOWK(I) = 1                                                   ACR 1440
   33 CONTINUE                                                          ACR 1450
   32 CONTINUE                                                          ACR 1460
      DO 31 I=1,N26                                                     ACR 1470
        DO 30 M=1,KROT                                                  ACR 1480
          ST = XTOTL(M,I)                                               ACR 1490
          IF(XTOTL(M,I) .GT. 0.) XTOTL(M,I) = 1. / (3.*XTOTL(M,I))      ACR 1500
          IF(MUHU(3) .NE. 2) GOTO 30                                    ACR 1510
          IF(NOPOWK(M) .NE. 1) GOTO 30                                  ACR 1520
C                                                                       ACR 1530
          CALL STREAM(ST,STRM)                                          ACR 1540
C                                                                       ACR 1550
          XTOTL(M,I) = XTOTL(M,I) * STRM                                ACR 1560
   30   CONTINUE                                                        ACR 1570
   31 CONTINUE                                                          ACR 1580
      IF(NANI .NE. -1) GOTO 69                                          ACR 1590
C                                                                       ACR 1600
C     AENDERUNG DIFF.KONST./SIGMA-A/SIGMA-TR (Z.B. IN HOHLRAEUMEN)      ACR 1610
C                                                                       ACR 1620
      DO 66 J=1,JH                                                      ACR 1630
        K = IZONE(J)                                                    ACR 1640
        MA = M1(J,1)                                                    ACR 1650
        MB = M1(J,2)                                                    ACR 1660
        DO 66 M=MA,MB                                                   ACR 1670
          JJ = 1                                                        ACR 1680
          DO 66 I=1,N26                                                 ACR 1690
            IF(RDK(K,I) .LT. 0.) GOTO 65                                ACR 1700
            XSIGA(M,I) = SGA(K,I)                                       ACR 1710
            XNFIS(M,I) = 0.                                             ACR 1720
            CNU(M,I) = 0.                                               ACR 1730
            DO 64 N=I+1,N26                                             ACR 1740
              XSIGTR(M,JJ) = SGTR(K,I,N)                                ACR 1750
              JJ = JJ + 1                                               ACR 1760
   64       CONTINUE                                                    ACR 1770
   65       XTOTL(M,I) = ABS(RDK(K,I))                                  ACR 1780
   66 CONTINUE                                                          ACR 1790
   69 CONTINUE                                                          ACR 1800
C                                                                       ACR 1810
CARDS C8-3                                                              ACR 1820
C                                                                       ACR 1830
      DO 202 M=1,KROT                                                   ACR 1840
        JJ = 1                                                          ACR 1850
        DO 201 I=1,N26                                                  ACR 1860
          WRITE (NCIT,103) M,I,XTOTL(M,I),XSIGA(M,I),XNFIS(M,I),ANUL,   ACR 1870
     1     CNU(M,I)                                                     ACR 1880
C                                                                       ACR 1890
CARDS C8-4                                                              ACR 1900
C                                                                       ACR 1910
          DO 206 J=1,N26                                                ACR 1920
            SCAT(J) = 0.                                                ACR 1930
  206     CONTINUE                                                      ACR 1940
          IF(I .EQ. N26) GOTO 207                                       ACR 1950
          DO 205 J=I+1,N26                                              ACR 1960
            SCAT(J) = XSIGTR(M,JJ)                                      ACR 1970
            JJ = JJ + 1                                                 ACR 1980
  205     CONTINUE                                                      ACR 1990
  207     CONTINUE                                                      ACR 2000
          WRITE (NCIT,102) (SCAT(J),J=1,N26)                            ACR 2010
  201   CONTINUE                                                        ACR 2020
  202 CONTINUE                                                          ACR 2030
      M = 0                                                             ACR 2040
      WRITE (NCIT,103) M,M,(ANUL,I=1,5)                                 ACR 2050
      GO TO 99                                                          ACR 2060
C                                                                       ACR 2070
      ENTRY MACRO2(SS,FISMAC,NRG,NEND10,NEND11,SSS)                     ACR 2080
C                                                                       ACR 2090
C     LESEN CITATION-FLUESSE UND VERBUCHEN (+K-EFFECTIVE)               ACR 2100
C                                                                       ACR 2110
      NEND11 = NEND10 + N26 * KROT                                      ACR 2120
      REACT(1) = REACT(2)                                               ACR 2130
      IF(MUHU(28) .GT. 0) GOTO 99                                       ACR 2140
      READ (M34) REACT2,KMAX,MMAX,((SSS(IE,KR),IE=1,KMAX),KR=1,MMAX)    ACR 2150
      REACT(2) = REACT2                                                 ACR 2160
      DO 140 N=1,NDR                                                    ACR 2170
        DO 140 IE=1,N26                                                 ACR 2180
          FISMAC(IE,N) = 0.                                             ACR 2190
  140 CONTINUE                                                          ACR 2200
      DO 141 K=1,IMX                                                    ACR 2210
        IC = ICI(K)                                                     ACR 2220
        IV = IVS(K)                                                     ACR 2230
        DO 141 I=1,N26                                                  ACR 2240
          FISMAC(I,IV) = FISMAC(I,IV) + VFV(K) * SSS(I,IC)              ACR 2250
  141 CONTINUE                                                          ACR 2260
      DO 40 IR=1,N200                                                   ACR 2270
        KR = NRG(IR)                                                    ACR 2280
        DO 41 IE=1,N26                                                  ACR 2290
          SS(IE,IR) = FISMAC(IE,KR)                                     ACR 2300
   41   CONTINUE                                                        ACR 2310
   40 CONTINUE                                                          ACR 2320
      GO TO 99                                                          ACR 2330
C                                                                       ACR 2340
      ENTRY MACRO3(RPHIAV,VOL,SS,FISMAC,PHIN)                           ACR 2350
C                                                                       ACR 2360
C     NORMIERUNG DER FLUESSE                                            ACR 2370
C                                                                       ACR 2380
      IF(NFS .EQ. -5) GOTO 60                                           ACR 2390
      TOTAL = 0.                                                        ACR 2400
      DO 51 IR=1,N200                                                   ACR 2410
        FCI = 0.                                                        ACR 2420
        DO 50 IE=1,N26                                                  ACR 2430
          IF(MUHU(28) .GT. 0) SS(IE,IR) = PHIN(IE)                      ACR 2440
          FCI = FCI + FISMAC(IE,IR) * SS(IE,IR)                         ACR 2450
   50   CONTINUE                                                        ACR 2460
        TOTAL = TOTAL + FCI * VOL(IR)                                   ACR 2470
   51 CONTINUE                                                          ACR 2480
      XNORM = POWER * FIWATT / TOTAL                                    ACR 2490
      DO 52 IR=1,N200                                                   ACR 2500
        DO 52 IE=1,N26                                                  ACR 2510
          SS(IE,IR) = SS(IE,IR) * XNORM                                 ACR 2520
   52 CONTINUE                                                          ACR 2530
   60 CONTINUE                                                          ACR 2540
      DO 53 IE=1,N26                                                    ACR 2550
        DO 53 KR=1,NDR                                                  ACR 2560
          RPHIAV(IE,KR) = 1.E-24                                        ACR 2570
   53 CONTINUE                                                          ACR 2580
   99 CONTINUE                                                          ACR 2590
      RETURN                                                            ACR 2600
      END                                                               ACR 2610
      SUBROUTINE MAICIT                                                 MAI   10
C                                                                       MAI   20
      COMMON /MU/ MU4,NCIT,NGEOM,I9,IOM                                 MAI   30
C                                                                       MAI   40
      COMMON /COCI/ A(3000000)                                          MAI   50
C                                                                       MAI   60
C                                                                       MAI   70
C                                                                       MAI   80
      NBYTE = 3000000                                                   MAI   90
C                                                                       MAI  100
      CALL RESERV(A,NBYTE)                                              MAI  110
C                                                                       MAI  120
      RETURN                                                            MAI  130
      END                                                               MAI  140
      SUBROUTINE RESERV(A,NBYTE)                                        SER   10
C                                                                       SER   20
      COMMON /MU/ MU4                                                   SER   30
C                                                                       SER   40
      DIMENSION A(3000000)                                              SER   50
C                                                                       SER   60
C                                                                       SER   70
      IMAX = NBYTE                                                      SER   80
C                                                                       SER   90
      CALL INPT(A,IMAX)                                                 SER  100
C                                                                       SER  110
      RETURN                                                            SER  120
      END                                                               SER  130
      SUBROUTINE AJAXC                                                  JAX   10
C                                                                       JAX   20
      COMMON /ALSUB/ BL(21939)                                          JAX   30
C                                                                       JAX   40
      COMMON /AMESH/ BM(23130)                                          JAX   50
C                                                                       JAX   60
      COMMON /AFLUX/ BF(498)                                            JAX   70
C                                                                       JAX   80
      COMMON /ABURN/ BB(80522)                                          JAX   90
C                                                                       JAX  100
      COMMON /ASRCH/ BS(63)                                             JAX  110
C                                                                       JAX  120
      COMMON/CMARY/ BC(141)                                             JAX  130
C                                                                       JAX  140
C                                                                       JAX  150
      DO 101 N=1,21939                                                  JAX  160
        BL(N) = 0.0                                                     JAX  170
  101 CONTINUE                                                          JAX  180
      DO 102 N=1,23130                                                  JAX  190
        BM(N) = 0.0                                                     JAX  200
  102 CONTINUE                                                          JAX  210
      DO 103 N=1,498                                                    JAX  220
        BF(N) = 0.0                                                     JAX  230
  103 CONTINUE                                                          JAX  240
      DO 104 N=1,80522                                                  JAX  250
        BB(N) = 0.0                                                     JAX  260
  104 CONTINUE                                                          JAX  270
      DO 105 N=1,63                                                     JAX  280
        BS(N) = 0.0                                                     JAX  290
  105 CONTINUE                                                          JAX  300
      DO 106 N=1,141                                                    JAX  310
        BC(N) = 0.0                                                     JAX  320
  106 CONTINUE                                                          JAX  330
      RETURN                                                            JAX  340
      END                                                               JAX  350
      SUBROUTINE SETV                                                   ETV   10
C                                                                       ETV   20
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,ETV   30
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   ETV   40
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), ETV   50
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    ETV   60
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    ETV   70
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   ETV   80
     6 IXPUT(9999),XPUT(9999)                                           ETV   90
C                                                                       ETV  100
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  ETV  110
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), ETV  120
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)ETV  130
     3 ,PDK(211)                                                        ETV  140
C                                                                       ETV  150
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   ETV  160
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKETV  170
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    ETV  180
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  ETV  190
     4 ITMAX,ITIME,BET(211),DEL(211)                                    ETV  200
C                                                                       ETV  210
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           ETV  220
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    ETV  230
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),ETV  240
     3 NCLASS(9999),NDP(9999)                                           ETV  250
C                                                                       ETV  260
      COMMON /ASRCH/ BSRCH(30),XK1,XK2,XK3,XN1,XN2,XN3,DELK1,DELK2,DELK3ETV  270
     1 ,BATTY,DRV,TBF,GWC,EK2,RCCM,DNDK(5),NSC(5),NSCN,NXZ,NXN,NXM,NXS, ETV  280
     2 INIL,INIU,INID                                                   ETV  290
C                                                                       ETV  300
      COMMON /CMARY/ MEMARY,IMN,MNI,IJLMN,NMLJI,IY(50),AX(50),TITL(36)  ETV  310
C                                                                       ETV  320
      COMMON /MU/ MU4,NCIT,NGEOM,I9,IOM,M34,M21                         ETV  330
C                                                                       ETV  340
   10 FORMAT ('1'//26X,5('@'),5X,'@@@',3X,9('@'),6X,'@@@',6X,9('@'),3X, ETV  350
     1 '@@@',5X,5('@'),5X,'@@@',4X,'@@@'/25X,7('@'),4X,'@@@',3X,9('@'), ETV  360
     2 5X,5('@'),5X,9('@'),3X,'@@@',4X,7('@'),4X,'@@@@',3X,'@@@'/24X,'@@ETV  370
     3@',4X,'@@',3X,'@@@',6X,'@@@',7X,'@@@',1X,'@@@',7X,'@@@',6X,'@@@', ETV  380
     4 3X,'@@@',3X,'@@@',3X,5('@'),2X,'@@@'/24X,'@@@',9X,'@@@',6X,'@@@',ETV  390
     5 6X,'@@@',3X,'@@@',6X,'@@@',6X,'@@@',3X,'@@@',3X,'@@@',3X,6('@'), ETV  400
     6 1X,'@@@'/24X,'@@@',9X,'@@@',6X,'@@@',6X,9('@'),6X,'@@@',6X,'@@@',ETV  410
     7 3X,'@@@',3X,'@@@',3X,'@@@',1X,6('@')/24X,'@@@',4X,'@@',3X,'@@@', ETV  420
     8 6X,'@@@',6X,9('@'),6X,'@@@',6X,'@@@',3X,'@@@',3X,'@@@',3X,'@@@', ETV  430
     9 2X,5('@')/25X,7('@'),4X,'@@@',6X,'@@@',6X,'@@@',3X,'@@@',6X,'@@@'ETV  440
     X ,6X,'@@@',4X,7('@'),4X,'@@@',3X,'@@@@'/26X,5('@'),5X,'@@@',6X,'@@ETV  450
     Y@',6X,'@@@',3X,'@@@',6X,'@@@',6X,'@@@',5X,5('@'),5X,'@@@',4X,'@@@'ETV  460
     Z )                                                                ETV  470
   20 FORMAT (//////36X,'**',5X,'**',7X,7('*'),8X,7('*'),7X,8('*')/36X, ETV  480
     1 '**',5X,'**',3(6X,'**',5X,'**')/36X,'**',5X,'**',6X,'**',13X,'**'ETV  490
     2 ,5X,'**',6X,8('*')/37X,'**',3X,'**',8X,7('*'),7X,'**',5X,'**',6X,ETV  500
     3 '**'/38X,'**',1X,'**',4X,'**',2X,'*',6X,'**',2(2X,'**'),5X,'**',2ETV  510
     4 (2X,'**'),9X,'**'/39X,'***',5X,'**',3X,7('*'),3X,'**',3X,7('*'), ETV  520
     5 3X,'**',2X,'**',9X,'**')                                         ETV  530
   30 FORMAT (//////23X,'  JAN. 2012',53X,'REPORT: V.S.O.P.(99/11)'/96X,ETV  540
     1 'JUEL - 4348'/23X,'  JUNE 2010',53X,'REPORT: V.S.O.P.(99/09)'/96XETV  541
     2 ,'JUEL - 4326'/96X,'SECTION 4.4.6')                              ETV  550
C                                                                       ETV  560
C                                                                       ETV  570
      CALL AJAXC                                                        ETV  580
C                                                                       ETV  590
      IOIN = NCIT                                                       ETV  600
      IOUT = 6                                                          ETV  610
      I4 = 4                                                            ETV  620
      IOSIG = I4                                                        ETV  630
      IOFLX = M21                                                       ETV  640
      IO1 = 1                                                           ETV  650
      IO2 = 2                                                           ETV  660
      IO3 = 3                                                           ETV  670
      IO4 = 4                                                           ETV  680
      IO7 = 7                                                           ETV  690
      IX(77) = 22                                                       ETV  700
      IO10 = IX(77)                                                     ETV  710
      IX(78) = 23                                                       ETV  720
      IX(79) = I4                                                       ETV  730
      IX(80) = 35                                                       ETV  740
      IX(81) = 24                                                       ETV  750
      IX(82) = 26                                                       ETV  760
      IX(83) = 27                                                       ETV  770
      IX(84) = 28                                                       ETV  780
      IX(85) = 31                                                       ETV  790
      IX(86) = 32                                                       ETV  800
      IX(87) = I4                                                       ETV  810
      IX(88) = I4                                                       ETV  820
      IX(89) = I4                                                       ETV  830
      IX(90) = I4                                                       ETV  840
      IX(91) = I4                                                       ETV  850
      IX(92) = I4                                                       ETV  860
      IX(93) = 36                                                       ETV  870
      IX(94) = I4                                                       ETV  880
      IX(95) = I4                                                       ETV  890
      IX(96) = I4                                                       ETV  900
      IX(97) = I4                                                       ETV  910
      IX(137) = I4                                                      ETV  920
      IX(138) = I4                                                      ETV  930
      IX(139) = I4                                                      ETV  940
      IX(140) = I4                                                      ETV  950
      IX(141) = I4                                                      ETV  960
      N1 = 1000                                                         ETV  970
      N2 = 0                                                            ETV  980
      REWIND IO10                                                       ETV  990
      WRITE (IO10) N1,N2,N2,N2,N2                                       ETV 1000
      END FILE IO10                                                     ETV 1010
      REWIND IO10                                                       ETV 1020
      WRITE (IOUT,10)                                                   ETV 1030
      WRITE (IOUT,20)                                                   ETV 1040
      WRITE (IOUT,30)                                                   ETV 1050
      RETURN                                                            ETV 1060
      END                                                               ETV 1070
      SUBROUTINE BALH(OUSIG,LA0,TOSIG,ABSIG,FISIG,LF0,IREG,NBURN,SSLUMP,BAL   10
     1 NHOT,VOL,DEN,FISMAC,NRG,NOPOW,BUCKS,RPHI,BUK,B,AB,PK,SOUT,ZLEAK1,BAL   20
     2 DEE,SIN,ZLEAK2,SIGL2,AM,PM,NEND21,N266,KMAT2,N261,ZLTH,IMAT,PHIN,BAL   30
     3 TRSIG,LT0)                                                       BAL   40
C                                                                       BAL   50
C     NEUTRON BALANCE CALCULCATION AND PRINT                            BAL   60
C                                                                       BAL   70
C     AB(IE)        INTERNAL VARIABLE REFERING TO VOLUME AND FLUX       BAL   80
C                   AVERAGED MACROSCOPIC ABSORPTION OF GROUP (IE)       BAL   90
C     AM(IE)        INTERNAL VARIABLE REFERING TO VOLUME AND FLUX       BAL  100
C                   AVERAGED MACROSCOPIC ABSORPTION OF GROUP (IE)       BAL  110
C                   AND EACH MATERIAL                                   BAL  120
C     BUK(IE)       NORMAL BUCKLING COMPUTED FROM NEUTRON BALANCE       BAL  130
C     DEE(IE)       AVERAGE DIFUSSION COEFF                             BAL  140
C     ZLEAK1(IE)    INTERNAL VARIABLE REFERING TO AVERAGE TRANSVERSE    BAL  150
C                   LEAKAGE                                             BAL  160
C     ZLEAK2(IE)    INTERNAL VARIABLE REFERING TO AVERAGE NORMAL        BAL  170
C                   LEAKAGE                                             BAL  180
C     PK(IE)        INTERNAL VARIABLE REFERING TO AVERAGE NEUTRON       BAL  190
C                   PRODUCTION GROUP (IE)                               BAL  200
C     SIGL2(IE)     NORMAL LEAKAGE CROSS SECTION COMPUTED FROM          BAL  210
C                   NEUTRON BALANCE                                     BAL  220
C     PM(IE)        INTERNAL VARIABLE REFERING TO AVERAGE NEUTRON       BAL  230
C                   PRODUCTION GROUP (IE) FOR EACH MATERIAL             BAL  240
C     SIN(IE)       SOURCE IN EACH GROUP PER INITIAL SOURCE NEUTRON     BAL  250
C     SOUT(IE)      OUT SCATTERING PER SOURCE NEUTRON FOR EACH GROUP    BAL  260
C     PKT           TOTAL NEUTRON PRODUCTION (NU*SIGMAFISSION)          BAL  270
C     SUMA          TOTAL NEUTRON ABSORPTION                            BAL  280
C     SUMAB         TOTAL NEUTRON ABSORPTION PER SOURCE NEUTRON         BAL  290
C     SUML1         TOTAL TRANSVERSE LEAKAGE PER SOURCE NEUTRON         BAL  300
C     SUML2         TOTAL NORMAL LEAKAGE PER SOURCE NEUTRON             BAL  310
C     PT            TOTAL NEUTRON PRODUCTION /K   (NU*SIGMAFISSION/K)   BAL  320
C                                                                       BAL  330
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    BAL  340
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    BAL  350
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIBAL  360
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D,NLAYP,ITTT, BAL  370
     4 LIMT                                                             BAL  380
C                                                                       BAL  390
      EQUIVALENCE(JTPE3,NT)                                             BAL  400
C                                                                       BAL  410
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1BAL  420
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         BAL  430
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    BAL  440
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)BAL  450
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  BAL  460
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    BAL  470
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         BAL  480
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,BAL  490
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            BAL  500
C                                                                       BAL  510
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300),FABC(10),REPC(10),NNEBAL  520
     1 ,LISTEQ,NTIK,K2,NRC,NRL,NRO                                      BAL  530
C                                                                       BAL  540
      COMMON /NUCNAM/ T1(200),T2(200),GR(2),PU                          BAL  550
C                                                                       BAL  560
CFZJ006 enlarged dimensions common QVAR                       28.11.03  BAL  570
      COMMON /QVAR/ DUM(1513),ZLEKA,ABXEN                               BAL  580
C                                                                       BAL  590
CFZJ031                                                       28.05.04  BAL  600
CFZJ062                                                       04.05.11  BAL  610
      COMMON /ORIGEN/ LOB,NOR,VOR(100),ISPK,KFISS,N200C,NXSC,KGR,       BAL  620
     1 LOBN,IEZ,FSP(33),IDOP,JNEU(15),LMAT(15)                          BAL  630
C                                                                       BAL  640
      COMMON /VARDIM/ A(8000000)                                        BAL  650
C                                                                       BAL  660
      COMMON /IFA/ FA0,FA1,FA2,IEND                                     BAL  670
C                                                                       BAL  680
CFZJ055                                                       25.09.07  BAL  690
C                                                                       BAL  700
      COMMON /FLUXN/ D(361),IACT                                        BAL  710
C                                                                       BAL  720
      CHARACTER*4 T1,T2,GR,PU                                           BAL  730
C                                                                       BAL  740
      DIMENSION OUSIG(LA0),TOSIG(LA0),ABSIG(LA0),FISIG(LF0),IREG(NDR),  BAL  750
     1 NBURN(NDR),SSLUMP(N26,NDR),NHOT(N200),VOL(N200),DEN(KMAT,N200),  BAL  760
     2 FISMAC(N26,N200),NRG(N200),NOPOW(N200),BUCKS(N26,NXS),RPHI(N26), BAL  770
     3 BUK(N26),B(KMAT2,N266),AB(N261),PK(N26),SOUT(N26),ZLEAK1(N26),   BAL  780
     4 DEE(N26),SIN(N26),ZLEAK2(N26),SIGL2(N26),AM(N26),PM(N26),        BAL  790
     5 ZLTH(NXS),IMAT(KMAT),PHIN(N26),TRSIG(LT0),SGA(N26),SGS(N26),     BAL  800
     6 SGU(N26),SIG(N26),PHI(N26)                                       BAL  810
C                                                                       BAL  820
   10 FORMAT(//' TIME STEP',I3,45X,13H K-EFFECTIVE#,1PE12.5/////10X,36HNBAL  830
     1EUTRON BALANCE PER ONE LOST NEUTRON///52H GROUP  ABSORPTION  SCATTBAL  840
     1ERING    SOURCE     LEAKAGE/23X,3HOUT//90(I4,1PE14.4,3(1PE12.4)/))BAL  850
   11 FORMAT (11X,46H FRACTIONAL ABSORPTIONS (PER ONE LOST NEUTRON)//'  BAL  860
     1  MATERIAL'/' VSOP-NO  NUCL    ALL GROUPS     ',8(2A4,I2,2X)/(33X,BAL  870
     2 8(2A4,I2,2X)))                                                   BAL  880
   12 FORMAT (I5,3X,2A4,1PE12.4,3X,8(1PE12.4)/(31X,8(1PE12.4)))         BAL  890
   13 FORMAT (6H TOTAL,1PE12.4,1PE36.4///////)                          BAL  900
   14 FORMAT (3X,13('.'),1PE12.4,3X,3A4)                                BAL  910
   15 FORMAT (/)                                                        BAL  920
   80 FORMAT (' TIME STEP',I3,',K-EFF=',E12.5/' DATA PER LOST NEUTRON:'/BAL  930
     1 3X,'LEAKAGE......',1PE12.4/ 3X,'ABSORPTION...',1PE12.4)          BAL  940
  111 FORMAT (////11X,47H FRACTIONAL PRODUCTIONS (PER PRODUCED NEUTRON) BAL  950
     1 //'    MATERIAL'/' VSOP-NO  NUCL    ALL GROUPS     ',8(2A4,I2,2X)BAL  960
     2 /(33X,8(2A4,I2,2X)))                                             BAL  970
  437 FORMAT (/' TIME STEP',I3,',K-EFF=',E12.5/' DATA PER LOST NEUTRON:'BAL  980
     1 /3X,'ABSORPTION...',1PE12.4)                                     BAL  990
  438 FORMAT (//' TIME STEP',I3,45X,13H K-EFFECTIVE#,1PE12.5/////10X,36HBAL 1000
     1NEUTRON BALANCE PER ONE LOST NEUTRON///52H GROUP  ABSORPTION  SCATBAL 1010
     1TERING    SOURCE            /23X,3HOUT//90(I4,1PE14.4,2(1PE12.4)/)BAL 1020
     1 )                                                                BAL 1030
  441 FORMAT (6H TOTAL,1PE12.4///////)                                  BAL 1040
C                                                                       BAL 1050
C                                                                       BAL 1060
      X = 100.                                                          BAL 1070
      K13 = IACT * N26                                                  BAL 1080
      K26 = KMAT * N26                                                  BAL 1090
      PKT = 0.0                                                         BAL 1100
      SUMA = 0.0                                                        BAL 1110
      SUMAB = 0.0                                                       BAL 1120
      SUML1 = 0.0                                                       BAL 1130
      SUML2 = 0.0                                                       BAL 1140
      DO 442 IE=1,N26                                                   BAL 1150
        RPHI(IE) = 0.                                                   BAL 1160
        AB(IE) = 0.0                                                    BAL 1170
        PK(IE) = 0.0                                                    BAL 1180
        SOUT(IE) = 0.0                                                  BAL 1190
        SIN(IE) = 0.                                                    BAL 1200
        ZLEAK1(IE) = 0.0                                                BAL 1210
        DEE(IE) = 0.0                                                   BAL 1220
  442 CONTINUE                                                          BAL 1230
      DO 2 IE=1,N26                                                     BAL 1240
        IRRE1 = 1                                                       BAL 1250
        NN71 = N71                                                      BAL 1260
        IF(MUHU(28) .GT. 0) GOTO 400                                    BAL 1270
        DO 1 KR=1,NN71                                                  BAL 1280
          IF(KR .GT. 1) IRRE1 = IREG(KR-1) + 1                          BAL 1290
          IRRE2 = IREG(KR)                                              BAL 1300
          TOS = 0.                                                      BAL 1310
          DO 201 IR=IRRE1,IRRE2                                         BAL 1320
            RP = FISMAC(IE,KR) * VOL(IR)                                BAL 1330
            RPHI(IE) = RPHI(IE) + RP                                    BAL 1340
            DO 202 L=1,KMAT                                             BAL 1350
              IL = ((NHOT(IR)-1)*KMAT+(L-1)) * N26                      BAL 1360
              IM = ((NHOT(IR)-1)*IACT+(L-1)) * N26                      BAL 1370
              IT = IL * LIMT / N26                                      BAL 1380
              IEL = IE + IL                                             BAL 1390
              IEM = IE + IM                                             BAL 1400
              QLUM = 1.                                                 BAL 1410
              IF(L .GT. NRO .AND. L .LE. NRL) QLUM = SSLUMP(IE,IR)      BAL 1420
              Q = DEN(L,IR) * QLUM                                      BAL 1430
              AB(IE) = AB(IE) + RP * Q * ABSIG(IEL)                     BAL 1440
              IF(L .LE. IACT) PK(IE) = PK(IE) + RP * Q * FISIG(IEM)     BAL 1450
              SOUT(IE) = SOUT(IE) + RP * Q * OUSIG(IEL)                 BAL 1460
              TOS = TOS + RP * Q * TOSIG(IEL)                           BAL 1470
              IF(IE .EQ. 1) GOTO 204                                    BAL 1480
              N25 = N26 - 1                                             BAL 1490
              ITM = IT + IE - 1                                         BAL 1500
              DO 203 N=1,IE-1                                           BAL 1510
                SIN(IE) = SIN(IE) + FISMAC(N,KR) * VOL(IR) * Q *        BAL 1520
     1           TRSIG(ITM)                                             BAL 1530
                ITM = ITM + N25 - N                                     BAL 1540
  203         CONTINUE                                                  BAL 1550
  204         CONTINUE                                                  BAL 1560
  202       CONTINUE                                                    BAL 1570
  201     CONTINUE                                                      BAL 1580
          DEE(IE) = DEE(IE) + TOS                                       BAL 1590
    1   CONTINUE                                                        BAL 1600
        RPHI(IE) = RPHI(IE) / VOLUME                                    BAL 1610
        PKT = PKT + PK(IE)                                              BAL 1620
        SUMA = SUMA + AB(IE)                                            BAL 1630
        TOS = DEE(IE) / (RPHI(IE)*VOLUME)                               BAL 1640
        STRM = 1.                                                       BAL 1650
C                                                                       BAL 1660
        IF(MUHU(3) .EQ. 2) CALL STREAM(TOS,STRM)                        BAL 1670
C                                                                       BAL 1680
        DEE(IE) = STRM / (3.*TOS)                                       BAL 1690
    2 CONTINUE                                                          BAL 1700
      PT = PKT / REACT(2)                                               BAL 1710
      SIN(1) = FSP(1)                                                   BAL 1720
      DO 3 IE=1,N26                                                     BAL 1730
        AB(IE) = AB(IE) / PT                                            BAL 1740
        SOUT(IE) = SOUT(IE) / PT                                        BAL 1750
        IF(IE .GT. 1) SIN(IE) = SIN(IE) / PT + FSP(IE)                  BAL 1760
        ZLEAK2(IE) = SIN(IE) - AB(IE) - SOUT(IE)                        BAL 1770
        SUMAB = SUMAB + AB(IE)                                          BAL 1780
        SUML2 = SUML2 + ZLEAK2(IE)                                      BAL 1790
        SIGL2(IE) = ZLEAK2(IE) * PT / (RPHI(IE)*VOLUME)                 BAL 1800
        BUK(IE) = SIGL2(IE) / DEE(IE)                                   BAL 1810
    3 CONTINUE                                                          BAL 1820
      ZLEKA = SUML2                                                     BAL 1830
      SUML3 = SUML2                                                     BAL 1840
  400 CONTINUE                                                          BAL 1850
      NP22 = NEND21                                                     BAL 1860
      NP23 = NP22 + N26                                                 BAL 1870
      NP24 = NP22 + N26 * 2                                             BAL 1880
      NP25 = NP22 + N26 * 3                                             BAL 1890
      NP26 = NP22 + N26 * 4                                             BAL 1900
      NP27 = NP26 + N26 * NXS                                           BAL 1910
      NP28 = NP26 + N26 * NXS * 2                                       BAL 1920
      NP29 = NP26 + N26 * NXS * 3                                       BAL 1930
      NP30 = NP29 + NXS                                                 BAL 1940
      NP31 = NP29 + NXS * 2                                             BAL 1950
      NP32 = NP29 + NXS * 3                                             BAL 1960
      NP33 = NP32 + N26                                                 BAL 1970
      NP331 = NP33 + N26                                                BAL 1980
      NP332 = NP331 + N261                                              BAL 1990
      NP333 = NP332 + N26                                               BAL 2000
      NEND33 = NP333 + N26                                              BAL 2010
C                                                                       BAL 2020
      IF(MUHU(28) .LE. 0) CALL BALSP(PT,OUSIG,LA0,TOSIG,ABSIG,FISIG,LF0,BAL 2030
     1 SSLUMP,NHOT,VOL,DEN,FISMAC,NRG,NOPOW,BUCKS,RPHI,N261,A(NP22),PK, BAL 2040
     2 DEE,A(NP23),A(NP24),A(NP25),A(NP26),A(NP27),A(NP28),A(NP29),     BAL 2050
     3 A(NP30),A(NP31),A(NP32),A(NP33),A(NP331),A(NP332),A(NP333),ZLTH, BAL 2060
     4 PHIN,TRSIG,LT0)                                                  BAL 2070
C                                                                       BAL 2080
      IF(MUHU(28) .LE. 0) GOTO 440                                      BAL 2090
      DO 410 IE=1,N26                                                   BAL 2100
        SGA(IE) = 0.                                                    BAL 2110
        SGS(IE) = 0.                                                    BAL 2120
        SGU(IE) = 0.                                                    BAL 2130
        DO 415 L=1,KMAT                                                 BAL 2140
          IL = (L-1) * N26                                              BAL 2150
          IEL = IE + IL                                                 BAL 2160
          QLUM = 1.                                                     BAL 2170
          IF(L .GT. NRO .AND. L .LE. NRL) QLUM = SSLUMP(IE,1)           BAL 2180
          Q = DEN(L,1) * QLUM                                           BAL 2190
          SGA(IE) = SGA(IE) + Q * ABSIG(IEL)                            BAL 2200
          SGS(IE) = SGS(IE) + Q * OUSIG(IEL)                            BAL 2210
          IF(L .LE. IACT) SGU(IE) = SGU(IE) + Q * FISIG(IEL)            BAL 2220
  415   CONTINUE                                                        BAL 2230
  410 CONTINUE                                                          BAL 2240
      ISP = 1                                                           BAL 2250
C                                                                       BAL 2260
      CALL KINF(SGA,SGS,SGU,SIG,PHI,RKINF,PHIN,ISP,NHOT,DEN,TRSIG,LT0,  BAL 2270
     1 VOL,SIN)                                                         BAL 2280
C                                                                       BAL 2290
      REACT(2) = RKINF                                                  BAL 2300
      DO 430 IE=1,N26                                                   BAL 2310
        DO 425 L=1,KMAT                                                 BAL 2320
          IL = (L-1) * N26                                              BAL 2330
          IEL = IE + IL                                                 BAL 2340
          QLUM = 1.                                                     BAL 2350
          IF(L .GT. NRO .AND. L .LE. NRL) QLUM = SSLUMP(IE,1)           BAL 2360
          Q = DEN(L,1) * QLUM                                           BAL 2370
          AB(IE) = AB(IE) + PHI(IE) * Q * ABSIG(IEL)                    BAL 2380
          SOUT(IE) = SOUT(IE) + PHI(IE) * Q * OUSIG(IEL)                BAL 2390
          IF(L .LE. IACT) PK(IE) = PK(IE) + PHI(IE) * Q * FISIG(IEL)    BAL 2400
  425   CONTINUE                                                        BAL 2410
        PKT = PKT + PK(IE)                                              BAL 2420
  430 CONTINUE                                                          BAL 2430
      PT = PKT / REACT(2)                                               BAL 2440
      SIN(1) = FSP(1)                                                   BAL 2450
      DO 435 IE=1,N26                                                   BAL 2460
        ZLEAK2(IE) = 0.                                                 BAL 2470
        AB(IE) = AB(IE) / PT                                            BAL 2480
        SOUT(IE) = SOUT(IE) / PT                                        BAL 2490
        SUMAB = SUMAB + AB(IE)                                          BAL 2500
        IF(IE .GT. 1) SIN(IE) = SIN(IE) / PT + FSP(IE)                  BAL 2510
  435 CONTINUE                                                          BAL 2520
      ZLEKA = SUML2                                                     BAL 2530
      SUML3 = SUML2                                                     BAL 2540
      IF(ABS(ZKFIND-REACT(2)) .LT. SERCON) IEND = 1                     BAL 2550
      IF(IPRIN(3) .GE. 0) GOTO 436                                      BAL 2560
      WRITE (NT,437) JN,REACT(2),SUMAB                                  BAL 2570
      GOTO 91                                                           BAL 2580
  436 WRITE (NT,438) JN,REACT(2),(IE,AB(IE),SOUT(IE),SIN(IE),IE=1,N26)  BAL 2590
      WRITE (NT,441) SUMAB                                              BAL 2600
      GOTO 439                                                          BAL 2610
  440 CONTINUE                                                          BAL 2620
      IF(IPRIN(3) .GE. 0) GOTO 90                                       BAL 2630
      WRITE (NT,80) JN,REACT(2),SUML2,SUMAB                             BAL 2640
      GOTO 91                                                           BAL 2650
   90 CONTINUE                                                          BAL 2660
      WRITE (NT,10) JN,REACT(2),(IE,AB(IE),SOUT(IE),SIN(IE),ZLEAK2(IE), BAL 2670
     1 IE=1,N26)                                                        BAL 2680
      WRITE (NT,13) SUMAB,SUML2                                         BAL 2690
  439 WRITE (NT,11) (GR(1),GR(2),I,I=1,N26)                             BAL 2700
   91 CONTINUE                                                          BAL 2710
      IF(INZWX .EQ. 0) GOTO 8                                           BAL 2720
      PRO(2) = SUML1 * X                                                BAL 2730
      PRO(3) = SUML2 * X                                                BAL 2740
    8 NRO = IACT + 2 + NO                                               BAL 2750
      NRL = NRO + NLUM                                                  BAL 2760
      NRC = NRL + NC                                                    BAL 2770
      IF(JN .GT. 0) GOTO 51                                             BAL 2780
      DO 50 K=1,KMAT2                                                   BAL 2790
        DO 50 J=1,N266                                                  BAL 2800
          B(K,J) = 0.                                                   BAL 2810
   50 CONTINUE                                                          BAL 2820
   51 CONTINUE                                                          BAL 2830
      IP = 0                                                            BAL 2840
   99 CONTINUE                                                          BAL 2850
      DO 30 I=1,N261                                                    BAL 2860
        AB(I) = 0.                                                      BAL 2870
   30 CONTINUE                                                          BAL 2880
      NAB = 1                                                           BAL 2890
      IP = IP + 1                                                       BAL 2900
      IF(IP .EQ. 2 .AND. IPRIN(3) .GE. 0) WRITE (NT,111) (GR(1),GR(2),I,BAL 2910
     1 I=1,N26)                                                         BAL 2920
      IF(N26 .NE. 8) WRITE (NT,15)                                      BAL 2930
      DO 7 L=1,KMAT                                                     BAL 2940
        IF(IP .EQ. 2 .AND. L .GT. IACT) GOTO 98                         BAL 2950
        KIK = IMAT(L)                                                   BAL 2960
        IF(KIK .GT. 190) KIK = LMAT(KIK-190)                            BAL 2970
        L26 = (L-1) * N26 - K26                                         BAL 2980
        L13 = (L-1) * N26 - K13                                         BAL 2990
        AMT = 0.                                                        BAL 3000
        SUML2 = 0.0                                                     BAL 3010
        IF(L .EQ. IACT+1) NAB = 2                                       BAL 3020
        DO 6 IE=1,N26                                                   BAL 3030
          AM(IE) = 0.0                                                  BAL 3040
          PM(IE) = 0.0                                                  BAL 3050
          IRRE1 = 1                                                     BAL 3060
          DO 5 KR=1,NN71                                                BAL 3070
            IF(KR .GT. 1) IRRE1 = IREG(KR-1) + 1                        BAL 3080
            IRRE2 = IREG(KR)                                            BAL 3090
            DO 4 IR=IRRE1,IRRE2                                         BAL 3100
              IL = NHOT(IR) * K26 + L26                                 BAL 3110
              Q = DEN(L,IR)                                             BAL 3120
              IF(L .LE. NRO) GOTO 350                                   BAL 3130
              NAB = 3                                                   BAL 3140
              IF(L .GT. NRL) GOTO 320                                   BAL 3150
              Q = Q * SSLUMP(IE,IR)                                     BAL 3160
              GOTO 350                                                  BAL 3170
  320         CONTINUE                                                  BAL 3180
              NAB = 4                                                   BAL 3190
              IF(L .GT. NRC) GOTO 330                                   BAL 3200
              GOTO 350                                                  BAL 3210
  330         CONTINUE                                                  BAL 3220
              NAB = 5                                                   BAL 3230
  350         CONTINUE                                                  BAL 3240
              RPHAV = FISMAC(IE,KR)                                     BAL 3250
              IF(MUHU(28) .GT. 0) RPHAV = PHI(IE)                       BAL 3260
              DENKZ = Q * RPHAV                                         BAL 3270
              IF(MUHU(28) .LE. 0) DENKZ = DENKZ * VOL(IR)               BAL 3280
              AM(IE) = AM(IE) + DENKZ * ABSIG(IE+IL)                    BAL 3290
              IF(L .GT. IACT) GOTO 4                                    BAL 3300
              LF = NHOT(IR) * K13 + L13                                 BAL 3310
              PM(IE) = PM(IE) + DENKZ * FISIG(IE+LF)                    BAL 3320
    4       CONTINUE                                                    BAL 3330
    5     CONTINUE                                                      BAL 3340
          AM(IE) = AM(IE) / PT                                          BAL 3350
          PM(IE) = PM(IE) / PKT                                         BAL 3360
          AMT = AMT + AM(IE)                                            BAL 3370
          SUML2 = SUML2 + PM(IE)                                        BAL 3380
    6   CONTINUE                                                        BAL 3390
        IF(INZWX .EQ. 0) GOTO 9                                         BAL 3400
        IF(IP .EQ. 2) GOTO 55                                           BAL 3410
        IF(L .GT. IACT) GOTO 9                                          BAL 3420
        PRO(L+96) = AMT * X                                             BAL 3430
        PRO(L+126) = SUML2 * X                                          BAL 3440
    9   CONTINUE                                                        BAL 3450
        IF(L .EQ. IACT+1) PRO(19) = AMT * X                             BAL 3460
        IF(INZW(1) .GE. 0) GOTO 55                                      BAL 3470
        IF(L .GT. 1) GOTO 53                                            BAL 3480
        DO 52 IE=1,N26                                                  BAL 3490
          B(KMAT2,IE) = B(KMAT2,IE) + ZLEAK2(IE)                        BAL 3500
   52   CONTINUE                                                        BAL 3510
        B(KMAT2,N26+1) = B(KMAT2,N26+1) + SUML3                         BAL 3520
        B(KMAT+1,N26+2) = B(KMAT+1,N26+2) + 1.                          BAL 3530
   53   CONTINUE                                                        BAL 3540
        DO 54 IE=1,N26                                                  BAL 3550
          B(L,IE) = B(L,IE) + AM(IE)                                    BAL 3560
          B(L,IE+N26+1) = B(L,IE+N26+1) + PM(IE)                        BAL 3570
   54   CONTINUE                                                        BAL 3580
        B(L,N26+1) = B(L,N26+1) + AMT                                   BAL 3590
        B(L,N266) = B(L,N266) + SUML2                                   BAL 3600
   55   CONTINUE                                                        BAL 3610
        IF (IPRIN(3) .LT. 0) GOTO 92                                    BAL 3620
        IF(IP .EQ. 1) WRITE (NT,12) L,T1(KIK),T2(KIK),AMT,(AM(IE),IE=1, BAL 3630
     1   N26)                                                           BAL 3640
        IF(IP .EQ. 2) WRITE (NT,12) L,T1(KIK),T2(KIK),SUML2,(PM(IE),IE=1BAL 3650
     1   ,N26)                                                          BAL 3660
   92   CONTINUE                                                        BAL 3670
        IF(IP .EQ. 1) AB(NAB) = AB(NAB) + AMT                           BAL 3680
        IF(IP .EQ. 2) AB(NAB) = AB(NAB) + SUML2                         BAL 3690
        IF(INZWX .EQ. 0) GOTO 31                                        BAL 3700
        IF(IP .EQ. 1 .AND. L .EQ. IACT) PRO(20) = AB(NAB) * X           BAL 3710
        IF(L .EQ. NRO) PRO(21) = AB(NAB) * X                            BAL 3720
   31   IF(L .EQ. IACT .OR. L .EQ. NRO .OR. L .EQ. NRL .OR. L .EQ. NRC  BAL 3730
     1   .OR. L .EQ. KMAT) WRITE (NT,14) AB(NAB),PU,PU,PU               BAL 3740
        IF(L .EQ. IACT+1) ABXEN = AMT                                   BAL 3750
    7 CONTINUE                                                          BAL 3760
      IF(JSER .NE. 0) GOTO 34                                           BAL 3770
      IF(IEND .NE. 1) GOTO 34                                           BAL 3780
      IF(MUHU(22) .GT. 0) AB(4) = AB(4) + SUML3                         BAL 3790
      IF(JN) 34,32,33                                                   BAL 3800
   32 FA0 = AB(4)                                                       BAL 3810
      GOTO 34                                                           BAL 3820
   33 FA2 = FA1                                                         BAL 3830
      FA1 = FA0                                                         BAL 3840
      FA0 = AB(4)                                                       BAL 3850
   34 CONTINUE                                                          BAL 3860
      IF(INZWX .GT. 0) PRO(59) = PRO(100) + PRO(102) + PRO(112) +       BAL 3870
     1 PRO(114)                                                         BAL 3880
      DO 40 I=2,N261                                                    BAL 3890
        AB(1) = AB(1) + AB(I)                                           BAL 3900
   40 CONTINUE                                                          BAL 3910
      WRITE (NT,14) AB(1),PU,PU,PU                                      BAL 3920
      IF(IP .EQ. 1) GOTO 99                                             BAL 3930
   98 RETURN                                                            BAL 3940
      END                                                               BAL 3950
      SUBROUTINE BALSP(PT,OUSIG,LA0,TOSIG,ABSIG,FISIG,LF0,SSLUMP,NHOT,  ALS   10
     1 VOL,DEN,FISMAC,NRG,NOPOW,BUCKS,RPHI,N261,ZLEAK,PK,DEE,SGA,SGS,SGUALS   20
     2 ,SGAUN,SGUUN,SGSUN,REACUN,ZL3N,ZL3,SIG,PHI,AB,SIN,SOUT,ZLTH,PHIN,ALS   30
     3 TRSIG,LT0)                                                       ALS   40
C                                                                       ALS   50
C     BUCKLING-RUECKKOPPLUNG FUER ALLE SPEKTRUMZONEN                    ALS   60
C                                                                       ALS   70
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    ALS   80
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    ALS   90
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIALS  100
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D,NLAYP,ITTT, ALS  110
     4 LIMT                                                             ALS  120
C                                                                       ALS  130
      EQUIVALENCE(JTPE3,NT)                                             ALS  140
C                                                                       ALS  150
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1ALS  160
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         ALS  170
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    ALS  180
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)ALS  190
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  ALS  200
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    ALS  210
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         ALS  220
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,ALS  230
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            ALS  240
C                                                                       ALS  250
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300),FABC(10),REPC(10),NNEALS  260
     1 ,LISTEQ,NTIK,K2,NRC,NRL,NRO                                      ALS  270
C                                                                       ALS  280
CFZJ031                                                       28.05.04  ALS  290
CFZJ062                                                       04.05.11  ALS  300
      COMMON /ORIGEN/ LOB,NOR,VOR(100),ISPK,KFISS,N200C,NXSC,KGR,       ALS  310
     1 LOBN,IEZ,FSP(33)                                                 ALS  320
C                                                                       ALS  330
      COMMON /FLUXN/ D(361),IACT                                        ALS  340
C                                                                       ALS  350
CFZJ061                                                       29.3.11   ALS  360
CFZJ055                                                       25.09.07  ALS  370
C                                                                       ALS  380
      DIMENSION OUSIG(LA0),TOSIG(LA0),ABSIG(LA0),FISIG(LF0),            ALS  390
     1 SSLUMP(N26,NDR),NHOT(N200),VOL(N200),DEN(KMAT,N200),             ALS  400
     2 FISMAC(N26,N200),NRG(N200),NOPOW(N200),BUCKS(N26,NXS),RPHI(N26), ALS  410
     3 ZLEAK(N26),AB(N261),PK(N26),SOUT(N26),DEE(N26),SGA(N26),SGS(N26),ALS  420
     4 SGU(N26),SGAUN(N26,NXS),SGUUN(N26,NXS),SGSUN(N26,NXS),REACUN(NXS)ALS  430
     5 ,ZL3N(NXS),ZL3(NXS),SIG(N26),PHI(N26),SIN(N26),ZLTH(NXS),        ALS  440
     6 PHIN(N26),TRSIG(LT0),DUMMY(N26),RV(1500)                         ALS  450
C                                                                       ALS  460
  100 FORMAT (/' SPZ',3X,'LEAK.TOTAL',3X,'LEAKAGE GROUP 1 -',I2/)       ALS  470
  200 FORMAT (I4,9E13.5/(17X,8E13.5))                                   ALS  480
  201 FORMAT (I4,9E13.5/(4X,9E13.5))                                    ALS  490
  300 FORMAT (/' SPZ',7X,'ZL3',9X,'K-INF')                              ALS  500
  400 FORMAT (I4,2E13.5)                                                ALS  510
  500 FORMAT (/' SPZ',10X,'SPEC-ZONES - DB**2 GROUP 1 -',I2/)           ALS  520
C                                                                       ALS  530
C                                                                       ALS  540
      DO 50 N=1,NXS                                                     ALS  550
        PKT = 0.                                                        ALS  560
        ABT = 0.                                                        ALS  570
        ZLTOT = 0.                                                      ALS  580
        TOS3 = 0.                                                       ALS  590
        FLX3 = 0.                                                       ALS  600
        ZLTH(N) = 0.                                                    ALS  610
        RV(N) = 0.                                                      ALS  620
        DO 2 IE=1,N26                                                   ALS  630
          ZLEAK(IE) = 0.                                                ALS  640
          RPHI(IE) = 0.                                                 ALS  650
          AB(IE) = 0.                                                   ALS  660
          PK(IE) = 0.                                                   ALS  670
          SOUT(IE) = 0.                                                 ALS  680
          SIN(IE) = 0.                                                  ALS  690
          DEE(IE) = 0.                                                  ALS  700
          SGA(IE) = 0.                                                  ALS  710
          SGS(IE) = 0.                                                  ALS  720
          SGU(IE) = 0.                                                  ALS  730
          NOP = 0                                                       ALS  740
          DO 11 IR=1,N200                                               ALS  750
            IF(NHOT(IR) .NE. N) GOTO 11                                 ALS  760
CFZJ061                                                  29.3.11        ALS  770
            RV(N) = RV(N) + VOL(IR)                                     ALS  780
            KR = NRG(IR)                                                ALS  790
            RP = FISMAC(IE,KR) * VOL(IR)                                ALS  800
            RPHI(IE) = RPHI(IE) + RP                                    ALS  810
            DO 202 L=1,KMAT                                             ALS  820
              IL = ((NHOT(IR)-1)*KMAT+(L-1)) * N26                      ALS  830
              IM = ((NHOT(IR)-1)*IACT+(L-1)) * N26                      ALS  840
              IT = IL * LIMT / N26                                      ALS  850
              IEL = IE + IL                                             ALS  860
              IEM = IE + IM                                             ALS  870
              QLUM = 1.                                                 ALS  880
              IF(L .GT. NRO .AND. L .LE. NRL) QLUM = SSLUMP(IE,IR)      ALS  890
              Q = DEN(L,IR) * QLUM                                      ALS  900
              AB(IE) = AB(IE) + RP * Q * ABSIG(IEL)                     ALS  910
              IF(L .LE. IACT) PK(IE) = PK(IE) + RP * Q * FISIG(IEM)     ALS  920
              SOUT(IE) = SOUT(IE) + RP * Q * OUSIG(IEL)                 ALS  930
              DEE(IE) = DEE(IE) + RP * Q * TOSIG(IEL)                   ALS  940
              SGA(IE) = SGA(IE) + VOL(IR) * Q * ABSIG(IEL)              ALS  950
              IF(L .LE. IACT) SGU(IE) = SGU(IE) + VOL(IR) * Q *         ALS  960
     1         FISIG(IEM)                                               ALS  970
              SGS(IE) = SGS(IE) + VOL(IR) * Q * OUSIG(IEL)              ALS  980
              IF(IE .EQ. 1) GOTO 204                                    ALS  990
              N25 = N26 - 1                                             ALS 1000
              ITM = IT + IE - 1                                         ALS 1010
              DO 203 J=1,IE-1                                           ALS 1020
                SIN(IE) = SIN(IE) + FISMAC(J,KR) * VOL(IR) * Q *        ALS 1030
     1           TRSIG(ITM)                                             ALS 1040
                ITM = ITM + N25 - J                                     ALS 1050
  203         CONTINUE                                                  ALS 1060
  204         CONTINUE                                                  ALS 1070
  202       CONTINUE                                                    ALS 1080
            NOP = NOP + NOPOW(IR)                                       ALS 1090
   11     CONTINUE                                                      ALS 1100
          SGA(IE) = SGA(IE) / RV(N)                                     ALS 1110
          SGS(IE) = SGS(IE) / RV(N)                                     ALS 1120
          SGU(IE) = SGU(IE) / RV(N)                                     ALS 1130
          SGAUN(IE,N) = SGA(IE)                                         ALS 1140
          SGUUN(IE,N) = SGU(IE)                                         ALS 1150
          SGSUN(IE,N) = SGS(IE)                                         ALS 1160
          PKT = PKT + PK(IE)                                            ALS 1170
          ABT = ABT + AB(IE)                                            ALS 1180
          TOS = DEE(IE) / RPHI(IE)                                      ALS 1190
          STRM = 1.                                                     ALS 1200
C                                                                       ALS 1210
          IF(MUHU(3) .EQ. 2 .AND. NOP .EQ. 0) CALL STREAM(TOS,STRM)     ALS 1220
C                                                                       ALS 1230
          DEE(IE) = STRM / (3.*TOS)                                     ALS 1240
          IF(IE .EQ. N26) GOTO 2                                        ALS 1250
          TOS3 = TOS3 + (3.*TOS) * RPHI(IE) / STRM                      ALS 1260
          FLX3 = FLX3 + RPHI(IE)                                        ALS 1270
          D3 = FLX3 / TOS3                                              ALS 1280
    2   CONTINUE                                                        ALS 1290
        ISP = N                                                         ALS 1300
C                                                                       ALS 1310
        CALL KINF(SGA,SGS,SGU,SIG,PHI,RKINF,PHIN,ISP,NHOT,DEN,TRSIG,LT0,ALS 1320
     1   VOL,DUMMY)                                                     ALS 1330
C                                                                       ALS 1340
        REACUN(N) = RKINF                                               ALS 1350
        IF(IBUCK .LE. 0) RETURN                                         ALS 1360
        P = PKT / REACT(2)                                              ALS 1370
        SIN(1) = P * FSP(1)                                             ALS 1380
        DO 3 IE=1,N26                                                   ALS 1390
          IF(IE .GT. 1) SIN(IE) = SIN(IE) + FSP(IE) * P                 ALS 1400
          ZLEAK(IE) = SIN(IE) - AB(IE) - SOUT(IE)                       ALS 1410
CFZJ034                                                       31.08.04  ALS 1420
          BUCKS(IE,N) = ZLEAK(IE) / RPHI(IE)                            ALS 1430
          IF(IE .EQ. N26) ZLTH(N) = 2. * ZLEAK(IE) / RPHI(IE)           ALS 1440
          ZLEAK(IE) = ZLEAK(IE) / PT                                    ALS 1450
          ZLTOT = ZLTOT + ZLEAK(IE)                                     ALS 1460
          IF(IE .EQ. N26) GOTO 3                                        ALS 1470
          ZL3N(N) = ZLTOT                                               ALS 1480
          ZL3(N) = ZL3N(N) * PT                                         ALS 1490
    3   CONTINUE                                                        ALS 1500
        IF(IBUCK .NE. 2) GOTO 6                                         ALS 1510
CFZJ034                                                       31.08.04  ALS 1520
        BU3 = ZL3(N) / FLX3                                             ALS 1530
        DO 5 IE=1,N25                                                   ALS 1540
          BUCKS(IE,N) = BU3                                             ALS 1550
    5   CONTINUE                                                        ALS 1560
    6   CONTINUE                                                        ALS 1570
        IF(IPRIN(3) .LE. 0) GOTO 50                                     ALS 1580
        IF(N .EQ. 1) WRITE (NT,100) N26                                 ALS 1590
        WRITE (NT,200) N,ZLTOT,(ZLEAK(IE),IE=1,N26)                     ALS 1600
   50 CONTINUE                                                          ALS 1610
      IF(IPRIN(3) .LE. 0) GOTO 60                                       ALS 1620
      WRITE (NT,500) N26                                                ALS 1630
      DO 51 N=1,NXS                                                     ALS 1640
        WRITE (NT,201) N,(BUCKS(IE,N),IE=1,N26)                         ALS 1650
   51 CONTINUE                                                          ALS 1660
      WRITE (NT,300)                                                    ALS 1670
      DO 59 N=1,NXS                                                     ALS 1680
        WRITE (NT,400) N,ZL3N(N),REACUN(N)                              ALS 1690
   59 CONTINUE                                                          ALS 1700
   60 CONTINUE                                                          ALS 1710
      RETURN                                                            ALS 1720
      END                                                               ALS 1730
      SUBROUTINE INPT(A,MEMORY)                                         NPT   10
C                                                                       NPT   20
C    INPT*****CITATION - COMMENTS AND DEFINITIONS FOLLOW*****           NPT   30
C                                                                       NPT   40
C     REAL*8 STATEMENTS ARE USED TO ACHIEVE DOUBLE PRECISION. SEE       NPT   50
C     COMMENTS IN SUBROUTINES KRST AND CNIO FOR CHANGES TO ALLOCATE     NPT   60
C     STORAGE FOR MACHINES OTHER THAN IBM-360 SERIES WHICH ALLOWS USE   NPT   70
C     OF 4-BYTE OR 8-BYTE WORDS.*****                                   NPT   80
C                                                                       NPT   90
C     FOLLOWING ARE THE DEFINITIONS OF THE VARIABLE INTEGER             NPT  100
C     SUBSCRIPTS USED TO DIMENSION ADJUSTABLE DIMENSIONED VARIABLES.    NPT  110
C                                                                       NPT  120
C     IVX = IMAX - NUMBER OF ROWS IN THE MESH.                          NPT  130
C     JVX = JMAX - NUMBER OF COLUMNS IN THE MESH.                       NPT  140
C     KBVX = KBMAX - NUMBER OF PLANES IN THE MESH.                      NPT  150
C     KVX = KMAX - NUMBER OF ENERGY GROUPS.                             NPT  160
C     LVX = LMAX - NUMBER OF REGIONS.                                   NPT  170
C     MVX = MMAX - NUMBER OF ZONES.                                     NPT  180
C     NVX = NMAX - MAX. NUMBER OF NUCLIDES IN ANY ONE CROSS SECTION SET.NPT  190
C     NSETVX = NSETMX - NUMBER OF MICROSCOPIC CROSS SECTION SETS.       NPT  200
C     NVO = IVO - MAXIMUM NUMBER OF SUB-ZONE DENSITIES IN ANY ONE ZONE. NPT  210
C     MVZ = MAXIMUM(MVX,NSETVX + 2)                                     NPT  220
C     IVXP1 = IVX+1.                                                    NPT  230
C     JVXP1 = JVX+1.                                                    NPT  240
C     KBVXP1 = KBVX+1.                                                  NPT  250
C     JIVX = JVX*IVX.                                                   NPT  260
C     JIP1VX = JVX*IVXP1.                                               NPT  270
C     IVZ = IVX+1 FOR HEX GEOMETRY, = IVX FOR ALL OTHERS.               NPT  280
C     KVZ = 2*KVX FOR HEX GEOMETRY, = KVX FOR ALL OTHERS.               NPT  290
C     JP1IXZ = JVXP1*IVZ                                                NPT  300
C     IOVX = KVX IF NO I/O DURING ITERATION, = 1 OTHERWISE.             NPT  310
C     IOVZ = KVZ IF NO I/O DURING ITERATION, = 2 OTHERWISE.             NPT  320
C     NSPB = 0 FOR STATICS AND DEPLETION PROBLEMS,                      NPT  330
C          = 1 FOR FIXED SOURCE PROBLEMS,                               NPT  340
C          = 5 FOR XENON OSCILLATION ONLY,                              NPT  350
C          = 2 *(NO. DELAYED GROUPS)+5 FOR ALL OTHER DYNAMICS PROBLEMS. NPT  360
C     NSPA = 1 IF NSPB=0, = NSPB OTHERWISE.                             NPT  370
C     NCRP = IVX*JVX*KBVX IF NSPB.GT.0, = 1 OTHERWISE.*****             NPT  380
C     NBLOCK - UTILITY STORAGE. PRESENTLY = 2200.                       NPT  390
C                                                                       NPT  400
C     FOLLOWING ARE THE DEFINITIONS OF THE ADJUSTABLE DIMENSIONED       NPT  410
C     VARIABLES.                                                        NPT  420
C                                                                       NPT  430
C     AC(NVX) - NUCLIDE CAPTURE RATE.                                   NPT  440
C     AL(NVX) - ABSORPTION RATE.                                        NPT  450
C     AP(NVX) - PRODUCTION RATE.                                        NPT  460
C     AX(NVX) - FISSION RATE.                                           NPT  470
C     BAL(KVX,MVX,1) - 1/V LOSSES (BAL IS ALSO USED IN PERT AND BEYOND  NPT  480
C                      FOR TEMPORARY STORAGE)                           NPT  490
C     BAL(KVX,MVX,2) - ABSORPTIONS.                                     NPT  500
C     BAL(KVX,MVX,3) - PRODUCTIONS.                                     NPT  510
C     BAL(KVX,MVX,4) - POWER(WATTS).                                    NPT  520
C     BAL(KVX,MVX,5) - OUTSCATTER.                                      NPT  530
C     BAL(KVX,MVX,6) - INSCATTER.                                       NPT  540
C     BAL(KVX,MVX,7) - BUCKLING LOSSES.                                 NPT  550
C     BAL(KVX,MVX,8) - XENON LOSSES.                                    NPT  560
C     BBND(KVX) - INTERNAL ROD BOUNDARY CONDITION CONSTANTS.            NPT  570
C     BIEMS(KVX) - FIXED SOURCE DISTRIBUTION FUNCTION                   NPT  580
C     BIK(20,KVX) - AUXILIARY STORAGE.                                  NPT  590
C     BND(6,KVX) - EXTERNAL BOUNDARY CONDITION CONSTANTS.               NPT  600
C     B1(MVX,KVX) - ZONE AVERAGE FLUX (REAL*8)                          NPT  610
C     B2(MVX,KVX) - ZONE ABSORPTION + BUCKLING LOSSES                   NPT  620
C     B3(MVX,KVX) - ZONE PRODUCTIONS.                                   NPT  630
C     B4(MVX,KVX) - ZONE LOSSES TO SEARCH PARAMETER, ZONE FLUX          NPT  640
C     B5(MVX,KVX) - ZONE PRODUCTIONS IN SEARCH PARAMETER.               NPT  650
C     CM(IVO) - SUB-ZONE CONCENTRATIONS (1 ZONE).                       NPT  660
C     CN(IVO) - SAME.                                                   NPT  670
C     CO(NVX) - SMEARED CONCENTRATIONS (1 ZONE).                        NPT  680
C     CONC(NVX,MVX) - ZONE NUCLIDE CONCENTRATIONS.                      NPT  690
C     CP(NVX) - AVERAGE NUCLIDE CONCENTRATION OVER A TIME STEP (1 ZONE).NPT  700
C     DCONB(JVX,IVXP1,IOVX) - TOP, BOTTOM DIFF. EQU. CNST. 1- AND 2-D.  NPT  710
C     DCONBE(JIP1VX,KBVX,IOVX) - SAME, AND SAME STORAGE EXCEPT 3-D      NPT  720
C     DCONBK(JIVX,KBVXP1,IOVX) - FRONT, BACK DIFF. EQU. CNST. FOR 3-D.  NPT  730
C     DCONR(JVXP1,IVZ,IOVZ) - LEFT,RIGHT DIFF. EQU. CNST. 1- AND 2-D.   NPT  740
C     DCONRE(JP1IXZ,IVBX,IOVZ) - SAME, AND SAME STORAGE EXCEPT 3-D.     NPT  750
C     E1(LVX,KVX) - LOSS TO SEARCH PARAMETER - EQUATION CONSTANT.       NPT  760
C     F1(KVX,MVX) - MACROSCOPIC INSCATTER CROSS SECTION (SCATTER TO K). NPT  770
C     HOL(NVX,NSETVX,10) - H1-H6 AND A1-A4 PAGE 105-3 OF THE REPORT.    NPT  780
C     HOX(NVX,NSETVX,20) - FISSION YIELD,DELAYED NEUTRON DATA BY NUCLIDENPT  790
C     HOY(NVX,NSETVX,20) - TEMPORARY STORAGE FOR DELAYED NEUTRON DATA   NPT  800
C     MJJR(9999,NSETVX) - NUCLIDE ORDER NUMBERS (SUBSCRIPT IS REAL NO.).NPT  810
C     NCOMP(LVX) - ZONE NUMBER FOR EACH REGION.                         NPT  820
C     NFO(20,NSETVX) - REAL NUMBER OF FIRST NUCLIDE WHICH CAUSES YIELD. NPT  830
C     NIC(1000) - STORAGE OF NUCLIDES IN CHAINS FOR DEPLETION.          NPT  840
C     NJJR(NVX,NSETVX) - NUCLIDE REAL NUMBERS (SUBSCRIPT IS ORDER NO.). NPT  850
C     NNDI1(NVX,NSETVX) - N3 PAGE 105-3 OF THE REPORT.                  NPT  860
C     NNFO(20,NSETVX) - ORDER NUMBER OF FIRST NUCLIDE CAUSING YIELD.    NPT  870
C     NNXTRA(NVX,NSETVX) - N5 PAGE 105-3 OF THE REPORT.                 NPT  880
C     NRGN(JVX,IVX) - REGION NUMBER AT EACH MESH POINT, 1- AND 2-D.     NPT  890
C     NRGNE(JVX,IVX,KBVX) - SAME, AND SAME STORAGE EXCEPT FOR 3-D.      NPT  900
C     ONEOV(KVX,NSETVX) - MICROSCOPIC 1/V CROSS SECTION.                NPT  910
C     PTSA(JVX,IVX,IOVX)- TOTAL LOSS EQUATION CONSTANT FOR 1- AND 2-D.  NPT  920
C     PTSAE(JIVX,KBVX,IOVX)- SAME, AND SAME STORAGE EXCEPT FOR 3-D.     NPT  930
C     PVOL(LVX) - POINT VOLUME.                                         NPT  940
C     P1(JVX,IVX) - FLUX AT ITERATION N-1 FOR 1- AND 2-D.               NPT  950
C     P1E(JIVX,KBVX) - SAME ,AND SAME STORAGE FOR 3-D.                  NPT  960
C     P2(JVX,IVX,KVX) - FLUX FOR 1- AND 2-D (REAL*8).                   NPT  970
C     P2E(JIVX,KBVX,KVX) - SAME, AND SAME STORAGE EXCEPT 3-D (REAL*4).  NPT  980
C     QMI(MVX,10) - AUXILIARY STORAGE.                                  NPT  990
C     RATAT(NBLOCK) - UTILITY STORAGE.                                  NPT 1000
C     RATE(NVX,MVX,1) - NUCLIDE, ZONE ABSORPTIONS.                      NPT 1010
C     RATE(NVX,MVX,2) - CAPTURES.                                       NPT 1020
C     RATE(NVX,MVX,3) - PRODUCTIONS.                                    NPT 1030
C     RATE(NVX,MVX,4) - AMOUNT (KG.).                                   NPT 1040
C     RATE(NVX,MVX,5) - ETA.                                            NPT 1050
C     RATE(NVX,MVX,6) - FISSIONS.                                       NPT 1060
C     RATE(NVX,MVX,7) - AUXILIARY STORAGE.                              NPT 1070
C     RATE(NVX,MVX,8) - AUXILIARY STORAGE.                              NPT 1080
C     RATE(NVX,MVX,9) - AUXILIARY STORAGE.                              NPT 1090
C     RATE(NVX,MVX,10) - AUXILIARY STORAGE.                             NPT 1100
C     RATET(NVX,1) - TOTAL NUCLIDE ABSORPTIONS.                         NPT 1110
C     RATET(NVX,2) - CAPTURES.                                          NPT 1120
C     RATET(NVX,3) - PRODUCTIONS.                                       NPT 1130
C     RATET(NVX,4) - AMOUNT (KG.).                                      NPT 1140
C     RATET(NVX,5) - ETA.                                               NPT 1150
C     RATET(NVX,6) - FISSIONS.                                          NPT 1160
C     RVOL(LVX) - REGION VOLUME*****.                                   NPT 1170
C     SCAC(MAX((KVX,MVX,KVX),(KVX,NSETVX+2,KVX)))-INSCAT.EQU.CNST(KKTOK)NPT 1180
C     SCAT(JVX,IVX) - INSCATTER SOURCE FOR 1- AND 2-D (REAL*8).         NPT 1190
C     SCATE(JVX,IVX,KBVX) - SAME, AND SAME STORAGE EXCEPT 3-D (REAL*4). NPT 1200
C     SIG(KVX,MVX,1) - MACROSCOPIC DIFFUSION COEFFICIENT.               NPT 1210
C     SIG(KVX,MVX,2) - MACROSCOPIC REMOVAL CROSS SECTION.               NPT 1220
C     SIG(KVX,MVX,3) - MACROSCOPIC ABSORPTION CROSS SECTION.            NPT 1230
C     SIG(KVX,MVX,4) - MACROSCOPIC NU*(FISSION CROSS SECTION).          NPT 1240
C     SIG(KVX,MVX,5) - MACRO. ABSORP. CROSS SECTION OF SEARCH NUCLIDES. NPT 1250
C     SIG(KVX,MVX,6) - BUCKLING (B**2)                                  NPT 1260
C     SIG(KVX,MVX,7) - POWER PER UNIT FLUX.                             NPT 1270
C     SIG(KVX,MVX,8) - MACRO. NU*SIGF CROSS SECTION OF SEARCH NUCLIDES. NPT 1280
C     SIG(KVX,MVX,9) - DIFFUSION COEFFICIENT * BUCKLING (D*B**2)        NPT 1290
C     SIG(KVX,MVX,10) - SIGA + SIGR + D*B**2                            NPT 1300
C     SOUR(JVX,IVX) - FISSION SOURCE FOR 1- AND 2-D (REAL*8 ON IBM-360).NPT 1310
C     SOURE(JVX,IVX,KBVX) - SAME, AND SAME STORAGE EXCEPT 3-D (REAL*4). NPT 1320
C     SPAR(NCRP,NSPA) - FIXED SOURCE BY POINT                           NPT 1330
C     SSC(KVX,KVX) - MICRO. INSCATTER CROSS SECTION (TO K FROM KK).     NPT 1340
C     SS1(KVX,NVX,NSETVX) - MICROSCOPIC ABSORPTION CROSS SECTION.       NPT 1350
C     SS2(KVX,NVX,NSETVX) - MICROSCOPIC FISSION CROSS SECTION.          NPT 1360
C     SS3(KVX,NVX,NSETVX) - MICROSCOPIC TRANSPORT CROSS SECTION.        NPT 1370
C     SS4(KVX,NVX,NSETVX) - NU, NEUTRONS PER FISSION                    NPT 1380
C     SS5(KVX,NVX,NSETVX) - MICRO. SCATTER CROSS SECTION FROM K TO K+1. NPT 1390
C     UTIL(JVX,IVX) - AUXILLIARY STORAGE FOR 1- AND 2-D.                NPT 1400
C     UTILE(JIVX,KBVX) - SAME ,AND SAME STORAGE EXCEPT FOR 3-D.         NPT 1410
C     XI(KVX) - FISSION DISTRIBUTION FUNCTION.                          NPT 1420
C     XII(KVX) - XI(K)/(MULTIPLICATION FACTOR) (REAL*8).                NPT 1430
C     XIK(KVX) - AUXILIARY STORAGE.                                     NPT 1440
C     XIKP(KVX) - AUXILIARY STORAGE.                                    NPT 1450
C     XL(6,KVX) - EXTERNAL BOUNDARY LEAKAGE.                            NPT 1460
C     YD(IVO) - YIELD RATE.                                             NPT 1470
C     ZONEN(NVO) - SUB-ZONE CONCENTRATIONS (ONE ZONE AT A TIME).        NPT 1480
C                                                                       NPT 1490
C     STARTING ADDRESSES OF PRINCIPAL VARIABLES IN THE -A- ARRAY        NPT 1500
C                                                                       NPT 1510
C     K1     SS1, HOY                  K2     SS2                       NPT 1520
C     K3     SS3                       K4     SS4                       NPT 1530
C     K5     SS5                       K6     SSC                       NPT 1540
C     K7     ONEOV                     K8     HOL                       NPT 1550
C     K9     NJJR                      K10    MJJR                      NPT 1560
C     K11                              K12    NNDI1                     NPT 1570
C     K13    NNXTRA                    K14    SIG                       NPT 1580
C     K15    F1                        K16    XI                        NPT 1590
C     K17    CONC                      K18    ZONEN                     NPT 1600
C     K19    SOUR, SOURE, RATE         K20    SCAT, SCATE               NPT 1610
C     K21    B1                        K22                              NPT 1620
C     K23    SCAC                      K24    P2, P2E                   NPT 1630
C     K25                              K26                              NPT 1640
C     K27                              K28                              NPT 1650
C     K29    E1                        K30    HOX                       NPT 1660
C     K31    NFO                       K32    NNFO                      NPT 1670
C     K33    NIC                       K34    AC                        NPT 1680
C     K35                              K36    B2                        NPT 1690
C     K37    B3                        K38    B4                        NPT 1700
C     K39    B5                        K40 = K63  BAL                   NPT 1710
C     K41    P1, P1E, UTIL, UTILE      K42    (DUMMY OF UNIT LENGTH)    NPT 1720
C     K43    RATET                     K44    QMI                       NPT 1730
C     K45    BIEMS                     K46    XIK                       NPT 1740
C     K47    XIKP                      K48    BIK                       NPT 1750
C     K49    XII                       K50    BBND                      NPT 1760
C     K51    BND                       K52    XL                        NPT 1770
C     K53                              K54    AL                        NPT 1780
C     K55    AX                        K56    AP                        NPT 1790
C     K57    CP                        K58    CO                        NPT 1800
C     K59    CN                        K60    CM                        NPT 1810
C     K61    YD                        K62    SPAR                      NPT 1820
C     K63    PTSA, PTSAE               K64    DCONR, DCONRE             NPT 1830
C     K65    DCONB, DCONBE             K66    DCONBK                    NPT 1840
C     K67 = K63  RATAT                 K68    (DUMMY OF UNIT LENGTH)    NPT 1850
C     K69                              K70                              NPT 1860
C     KNRGN  NRGN, NRGNE               KNCOMP NCOMP                     NPT 1870
C     KPVOL  PVOL                      KRVOL  RVOL                      NPT 1880
C                                                                       NPT 1890
C     ORDER OF STORAGE                                                  NPT 1900
C                                                                       NPT 1910
C     K1 K2 K3 K4 K5 K6 K7 K8 K9 K10 K11 K12 K13 K14 K15 K16 K17 K18 K38NPT 1920
C                                                                       NPT 1930
C     K39 K19 K20 K23 K24 K41                                           NPT 1940
C         K30 K31 K32 K33 K34 K54 K55 K56 K57 K58 K59 K60 K61 (OVERLAID)NPT 1950
C                                                                       NPT 1960
C     K42 K43 -THRU- K51 K52 K62 K64 K65 K66 K63 K21 K29 K36 K37 K68    NPT 1970
C         K40 (OVERLAID)                                                NPT 1980
C         K67 (OVERLAID)                                                NPT 1990
C                                                                       NPT 2000
C     KNRGN KNCOMP KPVOL KRVOL                                          NPT 2010
C                                                                       NPT 2020
C     FOLLOWING ARE THE DEFINITIONS OF SOME OF THE FIXED DIMENSIONED    NPT 2030
C     VARIABLES IN NAMED COMMONS.                                       NPT 2040
C                                                                       NPT 2050
C     AVZPD(9999) - ZONE AVERAGE POWER DENSITY.                         NPT 2060
C     NCH(50) - LOCATION OF CHAIN DATA IN NIC(SEE BELOW).               NPT 2070
C     NCLASS(9999) - CLASS.                                             NPT 2080
C     NJJM(50) - FIRST NUC. IN SET THAT HAS FISSION YIELD.              NPT 2090
C     NJM(50) - NUMBER OF NUCLIDES.                                     NPT 2100
C     NJNQ(50) - NUMBER OF FISSILE NUCLIDES WHICH CAUSE FISSION.        NPT 2110
C     NSIG1(50) - TYPE OF DATA.                                         NPT 2120
C     NSIG2(50) - NUMBER OF NUCLIDES.                                   NPT 2130
C     NSIG3(50) - NUMBER OF GROUPS.                                     NPT 2140
C     NSIG4(50) - NUMBER OF GROUPS FOR DOWNSCATTER.                     NPT 2150
C     NSIG5(50) - NUMBER OF GROUPS FOR UPSCATTER.                       NPT 2160
C     NSIG6(50) - NUMBER OF GROUPS WHICH CAN SCATTER TO GROUP 1.        NPT 2170
C     NXODR(9999) - MICROSCOPIC CROSS SECTION SET ODER NUMBER.          NPT 2180
C     NXSET(9999) - REAL MICROSCOPIC CROSS SECTION SET NUMBER.          NPT 2190
C     NZON(9999) - NUMBER OF SUB-ZONES FOR EACH ZONE.                   NPT 2200
C     PDKB(211) - POWER DENSITY TRAVERSE FORE AND AFT.                  NPT 2210
C     PDI(211) - POWER DENSITY TRAVERSE ALONG ROWS.                     NPT 2220
C     PDJ(211) - POWER DENSITY TRAVERSE ALONG COLUMNS.                  NPT 2230
C     X(211) - DISTANCE TO FLUX POINTS, LEFT TO RIGHT.                  NPT 2240
C     XX(211) - DISTANCE TO INTERVAL INTERFACES, LEFT TO RIGHT.         NPT 2250
C     Y(211) - DISTANCE TO FLUX POINTS, TOP TO BOTTOM.                  NPT 2260
C     YY(211) - DISTANCE TO INTERVAL INTERFACES, TOP TO BOTTOM.         NPT 2270
C     ZONVOL(9999) - ZONE VOLUME.                                       NPT 2280
C     Z(211) - DISTANCE TO FLUX POINTS, FRONT TO BACK.                  NPT 2290
C     ZZ(211) - DISTANCE TO INTERVAL INTERFACES, FRONT TO BACK.         NPT 2300
C                                                                       NPT 2310
C     IX(  1)   1 IF IBM-360/75, 5 IF IBM-360/91                        NPT 2320
C     IX(  2)   NUMBER OF DEPLETION TIME STEPS                          NPT 2330
C     IX(  3)   NUMBER OF CYCLES (FUELINGS)                             NPT 2340
C     IX(  4)   END OF CYCLE INDICATOR                                  NPT 2350
C     IX(  5)   INDICATES TYPE OF SEARCH IN NEUTRONICS                  NPT 2360
C     IX(  6)   BEHAVIOR OF SEARCH PROBLEM, DIVERGING IF .GT. 0         NPT 2370
C     IX(  7)   IF .GT. 0 INDICATES NUMBER OF REGIONS CHANGED FROM      NPT 2380
C                PREVIOUS CASE                                          NPT 2390
C     IX(  8)   REPEAT CYCLE INDEX                                      NPT 2400
C     IX(  9)   REPEAT TIME STEP INDEX                                  NPT 2410
C     IX( 10)   POINT IN MESH FOR CHECKING ITERATIVE BEHAVIOR (I)       NPT 2420
C     IX( 11)   POINT IN MESH FOR CHECKING ITERATIVE BEHAVIOR (J)       NPT 2430
C     IX( 12)   POINT IN MESH FOR CHECKING ITERATIVE BEHAVIOR (KB)      NPT 2440
C     IX( 13)   POINT IN MESH FOR CHECKING ITERATIVE BEHAVIOR (GROUP)   NPT 2450
C     IX( 14)   COUNT ON NEUTRONICS PROBLEM                             NPT 2460
C     IX( 15)   EXTRAPOLATION IN TIME, SEE IEXTRP IN BURN               NPT 2470
C     IX( 16)   INDICATES AN INITIALIZATION PROBLEM IS DONE 1ST CYCLE   NPT 2480
C     IX( 17)   TYPE OF SEARCH PREVIOUS PROBLEM                         NPT 2490
C     IX( 18)   NUMBER OF DELAYED NEUTRON FAMILIES                      NPT 2500
C     IX( 19)   CYCLE NUMBER FROM RESTART TAPE                          NPT 2510
C     IX( 20)   1 EXCEPT NUMBER OF GROUPS WITH I/O OF CONSTANTS         NPT 2520
C     IX( 21)   INDICATOR ON UPDATE OF MACRO DATA IN NEUTRONICS         NPT 2530
C     IX( 22)   POINT FLUXES NOT ON RESTART TAPE IF .GT. 0              NPT 2540
C     IX( 23)   REQUESTS ADJOINT NEUTRONICS FOLLOWING REGULAR PROBLEM   NPT 2550
C     IX( 24)   0 - REGULAR, 1 - ADJOINT NEUTRONICS BEING DONE          NPT 2560
C     IX( 25)   1, 2, 3, INDICATING GEOMETRIC DIMENSIONS                NPT 2570
C     IX( 26)   VALUE OF NUAC(5) GEOMETRY OPTION                        NPT 2580
C     IX( 27)   A SUCCEEDING CASE IF .GT. 0                             NPT 2590
C     IX( 28)   NUMBER OF DOWN-SCATTER GROUPS                           NPT 2600
C     IX( 29)   NUMBER OF UP-SCATTER GROUPS                             NPT 2610
C     IX( 30)   TYPE OF CROSS SECTION DATA PROVIDED                     NPT 2620
C     IX( 31)   BOUNDARY CONDITIONS SUPPLIED IF .GT. 0                  NPT 2630
C     IX( 32)   FLUX WAS EXTRAPOLATED IN NEUTRONICS IF .GT. 0           NPT 2640
C     IX( 33)   OVERRELAXATION FACTOR WAS REDUCED IF .GT. 0             NPT 2650
C     IX( 34)   COUNT OF THE NUMBER OF SETS OF REGIONS IN CLASS INPUT   NPT 2660
C     IX( 35)   NEUTRONICS FLUX EXTRAPOLATION CYCLE                     NPT 2670
C     IX( 36)   GROUPS UP-SCATTER TO THE FIRST                          NPT 2680
C     IX( 37)   I/O INDICATOR BLOCK A(IX37) - A(IX38) IF .GT. 0         NPT 2690
C     IX( 38)   I/O INDICATOR BLOCK A(IX37) - A(IX38) IF .GT. 0         NPT 2700
C     IX( 39)   FORCES CONSTANT POINT FLUX INITIALIZATION IF .GT. 0     NPT 2710
C                EXCEPT ON RESTART                                      NPT 2720
C     IX( 40)   INDICATOR ON I/O DEVICE DATA (WRITE F1 IF .EQ. 0)       NPT 2730
C     IX( 41)   LAST TIME STEP IN CYCLE IF .GT. 0                       NPT 2740
C     IX( 42)   NUCLIDE INDEX IN PERTURBATION ROUTINE PURT              NPT 2750
C     IX( 43)   GROUP INDEX IN PERTURBATION ROUTINE PURT                NPT 2760
C     IX( 44)   SEARCH DATA FLAG (NSRH(4))                              NPT 2770
C     IX( 45)   SEARCH DATA FLAG (NSRH(5))                              NPT 2780
C     IX( 46)   SEARCH DATA FLAG (NSRH(6))                              NPT 2790
C     IX( 47)   SEARCH DATA FLAG (NSRH(7))                              NPT 2800
C     IX( 48)   SEARCH DATA FLAG (NSRH(8))                              NPT 2810
C     IX( 49)   SEARCH DATA FLAG (NSRH(9))                              NPT 2820
C     IX( 50)   FUEL MANAGEMENT EDIT FLAG (SEE THOSE ROUTINES)          NPT 2830
C     IX( 51)   FIRST ENTRY IN FISSLE TABLE                             NPT 2840
C     IX( 52)   FINAL ENTRY IN FISSLE TABLE                             NPT 2850
C     IX( 53)   FIRST ENTRY IN FERTILE TABLE                            NPT 2860
C     IX( 54)   FINAL ENTRY IN FERTILE TABLE                            NPT 2870
C     IX( 55)   FIRST ENTRY IN INTERMEDIATE TABLE                       NPT 2880
C     IX( 56)   FINAL ENTRY IN INTERMEDIATE TABLE                       NPT 2890
C     IX( 57)   FIRST ENTRY IN (OTHER) TABLE                            NPT 2900
C     IX( 58)   FINAL ENTRY IN (OTHER) TABLE                            NPT 2910
C     IX( 59)   FIRST ENTRY IN STRUCTURAL TABLE                         NPT 2920
C     IX( 60)   FINAL ENTRY IN STRUCTURAL TABLE                         NPT 2930
C     IX( 61)   FIRST ENTRY IN SPECIAL TABLE                            NPT 2940
C     IX( 62)   FINAL ENTRY IN SPECIAL TABLE                            NPT 2950
C     IX( 63)   FIRST ENTRY IN FISSION PRODUCT TABLE                    NPT 2960
C     IX( 64)   FINAL ENTRY IN FISSION PRODUCT TABLE                    NPT 2970
C     IX( 68)   GEOMETRY OPTION FROM PRECEEDING CASE                    NPT 2980
C     IX( 69)   DATA FROM PRECEEDING CASE (NGC(19))                     NPT 2990
C     IX( 70)   INDIRECT SEARCH PROBLEM BEHAVIOR FLAG STOP IF .NE. 0    NPT 3000
C     IX( 71)   INDICATES ADJOINT PROBLEM WITH I/O OF CONSTANTS IF .GT.0NPT 3010
C     IX( 72)   SPECIAL BOUNDARY CONDITIONS IF .GT. 0                   NPT 3020
C     IX( 73)   COUNTER ON ITERATIONS FOR INDIRECT SEARCH               NPT 3030
C     IX( 74)   INDICATOR ON CALCULATION OF SEARCH PROBLEM EIGENVALUE   NPT 3040
C     IX( 75)   INDIRECT SEARCH ITERATION CONVERGED IF .GT. 0           NPT 3050
C     IX( 76)   SECTION 002 INPUT WITH RESTART IF .GT. 0                NPT 3060
C     IX( 77)   I/O DEVICE (10)                                         NPT 3070
C     IX( 78)   I/O DEVICE (11)                                         NPT 3080
C     IX( 79)   I/O DEVICE (12)                                         NPT 3090
C     IX( 80)   I/O DEVICE (13)                                         NPT 3100
C     IX( 81)   I/O DEVICE (14)                                         NPT 3110
C     IX( 82)   I/O DEVICE (15)                                         NPT 3120
C     IX( 83)   I/O DEVICE (16)                                         NPT 3130
C     IX( 84)   I/O DEVICE (17)                                         NPT 3140
C     IX( 85)   I/O DEVICE (18)                                         NPT 3150
C     IX( 86)   I/O DEVICE (19)                                         NPT 3160
C     IX( 87)   I/O DEVICE (20)                                         NPT 3170
C     IX( 88)   I/O DEVICE (21)                                         NPT 3180
C     IX( 89)   I/O DEVICE (22)                                         NPT 3190
C     IX( 90)   I/O DEVICE (23)                                         NPT 3200
C     IX( 91)   I/O DEVICE (24)                                         NPT 3210
C     IX( 92)   I/O DEVICE (25)                                         NPT 3220
C     IX( 93)   I/O DEVICE (26)                                         NPT 3230
C     IX( 94)   I/O DEVICE (27)                                         NPT 3240
C     IX( 95)   I/O DEVICE (28)                                         NPT 3250
C     IX( 96)   I/O DEVICE (29)                                         NPT 3260
C     IX( 97)   I/O DEVICE (30)                                         NPT 3270
C     IX( 98)   CALCULATE THERMAL ENERGY IF .GT. 0 FOR EDIT             NPT 3280
C     IX( 99)   CALCULATING END-OF-CYCLE NEUTRONICS IF .GT. 0           NPT 3290
C     IX(101)-IX(124)   EDIT OPTIONS, SEE INPUT IEDG AND IXPUT          NPT 3300
C     IX(126)   INDIRECT SEARCH NOT CONVERGING IF .GT. 0                NPT 3310
C     IX(127)   REFERENCE CLOCK TIME FOR INDIRECT SEARCH                NPT 3320
C     IX(128)   SPECIAL TYPE OF DIRECT SEARCH PROBLEM                   NPT 3330
C     IX(129)   NUCLIDE CONCENTRATIONS UPDATED RECENTLY IF .GT. 0       NPT 3340
C     IX(130)   INDICATES INDIRECT SEARCH USING PREVIOUS ESTIMATE OF    NPT 3350
C                N-DK/DN IF .GT. 0                                      NPT 3360
C     IX(131)   INDICATES SECTION 026 WAS INPUT IF .GT. 0               NPT 3370
C     IX(132)   INDICATES FIXED SOURCE IS TO BE SUPPLIED FROM I/O DEVICENPT 3380
C                IF .GT. 0                                              NPT 3390
C     IX(133)   RESERVED                                                NPT 3400
C     IX(134)   INDICATES FLUX HAS BEEN SCALED BY 1.0E+30 OR 1.0E-30    NPT 3410
C                IF .GT. 0                                              NPT 3420
C     IX(135)   INDICATES FLUX TO BE WRITTEN FOR POSSIBLE EXTRAPOLATION NPT 3430
C                IF .GT. 0                                              NPT 3440
C     IX(136)   INDICATES MACROSCOPIC CROSS SECTIONS WERE INPUT FROM    NPT 3450
C                TAPE OR DISK                                           NPT 3460
C     IX(137)   I/O DEVICE (31)                                         NPT 3470
C     IX(138)   I/O DEVICE (32)                                         NPT 3480
C     IX(139)   I/O DEVICE (33)                                         NPT 3490
C     IX(140)   I/O DEVICE (34)                                         NPT 3500
C     IX(141)   I/O DEVICE (35)                                         NPT 3510
C     IX(142)   INDICATES SECTION 030 SUPPLIED IF .GT. 0                NPT 3520
C     IX(150)   INDICATOR FOR PUNCHING OR COLLSP. MACROS.               NPT 3530
C     IX(151)   INDICATES IF SECTION 038 HAS BEEN INPUT                 NPT 3540
C     IX(152)   SAVE NFLXA                                              NPT 3550
C     IX(153)   SAVE NFLXB                                              NPT 3560
C     IX(160)   RESTART DATA (ISK)                                      NPT 3570
C     IX(161)   RESTART DATA (IEXR)                                     NPT 3580
C     IX(162)   RESTART DATA (NI3)                                      NPT 3590
C     IX(163)   RESTART DATA (IC10)                                     NPT 3600
C     IX(164)   RESTART DATA (IC20)                                     NPT 3610
C     IX(165)   NUCLIDE REFERENCE FOR DAMAGE RATE MAP                   NPT 3620
C     IX(166)   CROSS SECTION CORRELATION IF .GT. 0, SUBZONES IF 2      NPT 3630
C     IX(167)   INDICATES THAT ONLY INPUT SECTION 000 WAS USED          NPT 3640
C     IX(168)   MAXIMUM NUMBER OF SUBZONES IN A ZONE                    NPT 3650
C     IX(169)   SET TO K64 IN CNIO, USED IN KNST                        NPT 3660
C     IX(170)   SET TO K21-1 IN CNIO, USED IN KNST                      NPT 3670
C     IX(171)   SET TO INTGR(29) IN CALR, USED IN CALR                  NPT 3680
C     IX(172)   INDICATES IF READING FLUXES FROM PREVIOUS CYCLE         NPT 3690
C     IX(190)   INPUT ROUTINES COMMUNICATION (NUO)                      NPT 3700
C     IX(191)   INPUT ROUTINES COMMUNICATION (NHEX)                     NPT 3710
C     IX(192)   INPUT ROUTINES COMMUNICATION (NRVX)                     NPT 3720
C     IX(193)   INPUT ROUTINES COMMUNICATION (IJKBX)                    NPT 3730
C     IX(196)   RESTART INDICATOR ON VARIABLES (IND)                    NPT 3740
C     IX(197)   RESTART INDICATOR ON VARIABLES (IBCUT)                  NPT 3750
C     IX(198)   RESTART INDICATOR ON VARIABLES (NTITE)                  NPT 3760
C     IX(199)   INDICATOR OF SUBZONE TREATMENT REQUIRING I/O IF GT. 0   NPT 3770
C     IX(200)   WE SUSPECT SOME CAD IS PLAYING WITH THIS                NPT 3780
C                                                                       NPT 3790
C     SPARE(  1)   CYCLE DEPLETION (EXPOSURE) TIME, SEC (CYED)          NPT 3800
C     SPARE(  2)   CYCLE DEPLETION (EXPOSURE) TIME, DAYS (BURN)         NPT 3810
C     SPARE(  3)   EXPOSURE TIME INTERVAL, 1ST STEP 1ST CYCLE, DAYS     NPT 3820
C     SPARE(  4)   EXPOSURE TIME INTERVAL, 2ND STEP 1ST CYCLE, DAYS     NPT 3830
C     SPARE(  5)   EXPOSURE TIME INTERVAL, 3RD+ STEP CYCLE 1, DAYS      NPT 3840
C     SPARE(  6)   EXPOSURE TIME INTERVAL, 2ND STEP CYCLE 2+, DAYS      NPT 3850
C     SPARE(  7)   EXPOSURE TIME INTERVAL, 3RD+ STEP CYCLE 2+, DAYS     NPT 3860
C     SPARE(  8)   EXPOSURE TIME INTERVAL, 3RD+ STEP CYCLE 2+, DAYS     NPT 3870
C     SPARE(  9)   CURRENT EXPOSURE TIME STEP, SEC (CYED)               NPT 3880
C     SPARE( 10)   PREVIOUS EXPOSURE TIME STEP, SEC (CYED)              NPT 3890
C     SPARE( 11)   BEFORE PREVIOUS EXPOSURE TIME STEP, SEC (CYED)       NPT 3900
C     SPARE( 12)   TOTAL DEPLETION TIME, DAYS (DTOR)                    NPT 3910
C     SPARE( 13)   CYCLE DEPLETION TIME START EXPOSURE, DAYS (BURN)     NPT 3920
C     SPARE( 14)   REFERENCE MULTIPLICATION FACTOR (MAIN)               NPT 3930
C     SPARE( 15)   PRESENT MULTIPLICATION FACTOR (CYED)                 NPT 3940
C     SPARE( 16)   PREVIOUS MULTIPLICATION FACTOR (CYED)                NPT 3950
C     SPARE( 17)   BEFORE PREVIOUS MULTIPLICATION FACTOR (CYED)         NPT 3960
C     SPARE( 18)   NEUTRONICS EIGENVALUE (NMBL)                         NPT 3970
C     SPARE( 19)   TIME STEP THERMAL ENERGY, MW-HRS (TABL)              NPT 3980
C     SPARE( 20)   SPARE(3) IN SEC, ACTIVE                              NPT 3990
C     SPARE( 21)   SPARE(4) IN SEC, ACTIVE                              NPT 4000
C     SPARE( 22)   SPARE(5) IN SEC, ACTIVE                              NPT 4010
C     SPARE( 23)   SPARE(6) IN SEC, ACTIVE                              NPT 4020
C     SPARE( 24)   SPARE(7) IN SEC, ACTIVE                              NPT 4030
C     SPARE( 25)   SPARE(8) IN SEC, ACTIVE                              NPT 4040
C     SPARE( 26)   CYCLE THERMAL ENERGY, CURRENT SUM, MW-HRS            NPT 4050
C     SPARE( 27)   NEUTRONICS EIGENVALUE FOR RECOVERY (NMBL)            NPT 4060
C     SPARE( 28)   SEARCH PROBLEM EIGENVALUE                            NPT 4070
C     SPARE( 29)   RUNNING PRODUCT OF SPARE(29), EIGENVALUE SUM         NPT 4080
C     SPARE( 31)   ERROR MODE EIGENVALUE, FLUX INCREASING               NPT 4090
C     SPARE( 32)   ERROR MODE EIGENVALUE, FLUX DECREASING               NPT 4100
C     SPARE( 33)   POINT FLUX CHANGE                                    NPT 4110
C     SPARE( 34)   FLUX EXTRAPOLATION FACTOR                            NPT 4120
C     SPARE( 35)   FLUX EXTRAPOLATION FACTOR AT A REFERENCE POINT       NPT 4130
C     SPARE( 39)   THIRD DIMENSION OVERRELAXATION FACTOR                NPT 4140
C     SPARE( 40)   REFERENCE INFORMATION FOR FLUX EXTRAPOLATION         NPT 4150
C     SPARE( 43)   POINT FLUX EXTRAPOLATION FACTOR, PAST ESTIMATE       NPT 4160
C     SPARE( 44)   REFERENCE POINT FLUX VALUE, PREVIOUS ITERATION       NPT 4170
C     SPARE( 44)   REFERENCE POINT FLUX DATA, LAST ITERATION            NPT 4180
C     SPARE( 45)   REFERENCE POINT FLUX DATA, BEFORE LAST ITERATION     NPT 4190
C     SPARE( 46)   DAMPENING FACTOR FOR SEARCH PROCEDURE                NPT 4200
C     SPARE( 48)   NEUTRON ABSORPTIONS IN SEARCH MATERIAL               NPT 4210
C     SPARE( 49)   NEUTRON PRODUCTIONS BY SEARCH MATERIAL               NPT 4220
C     SPARE( 50)   REFERENCE MULTIPLICATION FACTOR FOR SEARCH           NPT 4230
C     SPARE( 51)   LIMITING VALUE SEARCH PROBLEM EIGENVALUE             NPT 4240
C     SPARE( 52)   FRACTION LOSS RATE TO SEARCH MATERIAL                NPT 4250
C     SPARE( 53)   FRACTION LOSS RATE TO SEARCH MATERIAL, N             NPT 4260
C     SPARE( 54)   FRACTION LOSS RATE TO SEARCH MATERIAL, N-1           NPT 4270
C     SPARE( 55)   FRACTION LOSS RATE TO SEARCH MATERIAL, N-2           NPT 4280
C     SPARE( 56)   TOTAL NEUTRON LOSSES                                 NPT 4290
C     SPARE( 56)   REFERENCE FRACTION SEARCH LOSSES                     NPT 4300
C     SPARE( 57)   REFERENCE FRACTION SEARCH LOSSES                     NPT 4310
C     SPARE( 58)   MAXIMUM SEARCH NUCLIDE DENSITY                       NPT 4320
C     SPARE( 60)   SEARCH PROBLEM EIGENVALUE (DISH)                     NPT 4330
C     SPARE( 61)   SEARCH PROBLEM DATA                                  NPT 4340
C     SPARE( 62)   SEARCH PROBLEM DATA                                  NPT 4350
C     SPARE( 63)   SEARCH PROBLEM DATA                                  NPT 4360
C     SPARE( 64)   SEARCH PROBLEM DATA                                  NPT 4370
C     SPARE( 65)   SEARCH PROBLEM DATA                                  NPT 4380
C     SPARE( 74)   REFERENCE EDIT TIME FOR REACTION RATES, DAYS         NPT 4390
C     SPARE( 75)   REFERENCE EDIT TIME FOR MASS DATA, DAYS              NPT 4400
C     SPARE( 87)   FIXED SOURCE FRACTION, AS DELAYED NEUTRONS           NPT 4410
C     SPARE( 88)   SUM OF FIXED SOURCE                                  NPT 4420
C     SPARE( 89)   SCALING FACTOR FOR FIXED SOURCE PROBLEM              NPT 4430
C     SPARE( 91)   REFERENCE PERTURBATION DATA (POUT, KOUT)             NPT 4440
C     SPARE( 94)   RESERVED BY A LOCAL USER                             NPT 4450
C     SPARE( 95)   RESERVED BY A LOCAL USER                             NPT 4460
C     SPARE( 96)   RESERVED BY A LOCAL USER                             NPT 4470
C     SPARE( 97)   RESERVED BY A LOCAL USER                             NPT 4480
C     SPARE( 98)   ZONE FACTOR SUM (SIG(K,M,8) IN CNST, KNST)           NPT 4490
C     SPARE( 99)   POWER LEVEL FOR NEUTRONICS PROBLEM, FISSION MW       NPT 4500
C     SPARE(100)   POWER LEVEL FOR REACTOR, THERMAL MW                  NPT 4510
C     SPARE(160)   RESTART DATA (VOG1 IN EXTR)                          NPT 4520
C     SPARE(161)   RESTART DATA (RP(1) IN EXTR)                         NPT 4530
C     SPARE(162)   RESTART DATA (RP(2) IN EXTR)                         NPT 4540
C     SPARE(163)   RESTART DATA (RP(3) IN EXTR)                         NPT 4550
C     SPARE(164)   RESTART DATA (RP(4) IN EXTR)                         NPT 4560
C     SPARE(165)   RESTART DATA (RP(5) IN EXTR)                         NPT 4570
C     SPARE(166)   RESTART DATA (RP(6) IN EXTR)                         NPT 4580
C     SPARE(167)   RESTART DATA (RP(7) IN EXTR)                         NPT 4590
C     SPARE(168)   RESTART DATA (RP(8) IN EXTR)                         NPT 4600
C     SPARE(169)   RESTART DATA (RP(9) IN EXTR)                         NPT 4610
C     SPARE(170)   RESTART DATA (RP(10) IN EXTR)                        NPT 4620
C                                                                       NPT 4630
CINPT --002 ***CITATION*** INPUT ACCESSING ROUTINE                      NPT 4640
C                                                                       NPT 4650
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,NPT 4660
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   NPT 4670
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), NPT 4680
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    NPT 4690
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    NPT 4700
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   NPT 4710
     6 IXPUT(9999),XPUT(9999)                                           NPT 4720
C                                                                       NPT 4730
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  NPT 4740
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), NPT 4750
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)NPT 4760
     3 ,PDK(211)                                                        NPT 4770
C                                                                       NPT 4780
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   NPT 4790
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKNPT 4800
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    NPT 4810
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  NPT 4820
     4 ITMAX,ITIME,BET(211),DEL(211)                                    NPT 4830
C                                                                       NPT 4840
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           NPT 4850
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    NPT 4860
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),NPT 4870
     3 NCLASS(9999),NDP(9999)                                           NPT 4880
C                                                                       NPT 4890
      COMMON/AVDLM/IVDLM(1),IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1,   NPT 4900
     1 KBVXP1,NSETVX,NVO,IVO,IVZ,KVZ,NCRP,NSPA,N3DDIM,NBLOCK,JIVX,JIP1VXNPT 4910
     2 ,JP1IXZ,IOVX,IOVZ                                                NPT 4920
C                                                                       NPT 4930
      COMMON /AKADD/ KAY(1),K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13, NPT 4940
     1 K131,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,K28,NPT 4950
     2 K29,K30,K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,K41,K42,K43,K44, NPT 4960
     3 K45,K46,K47,K48,K49,K50,K51,K52,K53,K54,K55,K56,K57,K58,K59,K60, NPT 4970
     4 K61,K62,K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,K74,K75,K76, NPT 4980
     5 K77,K78,K79,K80,K81,K82,K83,K84,K85,K86,K87,K88,K89,K90,K91,K92, NPT 4990
     6 K93,K94,K95,K96,K97,K98,K99,K100,NDATA,KNRGN,KNCOMP,KPVOL,KRVOL, NPT 5000
     7 MEMVRY,MEMX                                                      NPT 5010
C                                                                       NPT 5020
      COMMON /ASRCH/ BSRCH(30),XK1,XK2,XK3,XN1,XN2,XN3,DELK1,DELK2,DELK3NPT 5030
     1 ,BATTY,DRV,TBF,GWC,EK2,RCCM,DNDK(5),NSC(5),NSCN,NXZ,NXN,NXM,NXS, NPT 5040
     2 INIL,INIU,INID                                                   NPT 5050
C                                                                       NPT 5060
      COMMON /CMARY/ MEMARY,IMN,MNI,IJLMN,NMLJI,IY(50),AX(50),TITL(36)  NPT 5070
C                                                                       NPT 5080
      COMMON /COOPD/ FLOTR(9999),INTGR(200)                             NPT 5090
C                                                                       NPT 5100
      COMMON /MU/ MU4                                                   NPT 5110
C                                                                       NPT 5120
CFZJ055                                                       25.09.07  NPT 5130
C                                                                       NPT 5140
      COMMON /BLOCK1/ IDUM(17),JTPE3                                    NPT 5150
C                                                                       NPT 5160
      EQUIVALENCE(JTPE3,NT)                                             NPT 5170
C                                                                       NPT 5180
      DIMENSION A(MEMORY),FLATR(8)                                      NPT 5190
C                                                                       NPT 5200
      REAL*8 FLATR,FLOTR                                                NPT 5210
C                                                                       NPT 5220
      CHARACTER*4 BLANK/'    '/,TITL,TITL1,TITL2                        NPT 5230
C                                                                       NPT 5240
      CHARACTER*5 DATE                                                  NPT 5250
C                                                                       NPT 5260
      DATA FLATR(1)/8HMANGDATA/,FLATR(2)/8HSCRATCH1/,FLATR(3)/8HHISTDATANPT 5270
     1 /,FLATR(4)/8HCYCLHIST/,FLATR(5)/8HMASSDATA/,FLATR(6)/8HSBZNDENS/,NPT 5280
     2 FLATR(7)/8HZONEDENS/,FLATR(8)/8HSCRATCH2/                        NPT 5290
C                                                                       NPT 5300
 1001 FORMAT (////15X,'************THIS JOB WAS RUN ON  ',A5,'  ON A WINNPT 5310
     1DOWS-PC OR A UNIX-COMPUTER************'/////)                     NPT 5320
 1002 FORMAT (18A4)                                                     NPT 5330
 1004 FORMAT (1H ,18A4)                                                 NPT 5340
 1005 FORMAT (1H0,'END OF CASE,                                         NPT 5350
     > "CITATION"-EXECUTION TIME WAS',F7.2,1H ,'MINUTES')               NPT 5360
 1006 FORMAT (1H ,'**************************************************', NPT 5370
     > 1H*,'************************************************************NPT 5380
     >')                                                                NPT 5390
C                                                                       NPT 5400
C                                                                       NPT 5410
      ENDE = 0.                                                         NPT 5420
      IO13 = IX(80)                                                     NPT 5430
      IF(MU4 .GT. 1) GOTO 115                                           NPT 5440
      DO 100 N=1,9999                                                   NPT 5450
        FLOTR(N) = 0.0                                                  NPT 5460
  100 CONTINUE                                                          NPT 5470
      DO 101 N=1,8                                                      NPT 5480
        FLOTR(N) = FLATR(N)                                             NPT 5490
  101 CONTINUE                                                          NPT 5500
      DO 102 N=1,200                                                    NPT 5510
        INTGR(N) = 0                                                    NPT 5520
  102 CONTINUE                                                          NPT 5530
      INTGR(1) = 8                                                      NPT 5540
      MEMVRY = MEMORY                                                   NPT 5550
C                                                                       NPT 5560
      CALL SETV                                                         NPT 5570
C                                                                       NPT 5580
      NSPA = 0                                                          NPT 5590
      IX(1) = 1                                                         NPT 5600
      IO13 = IX(80)                                                     NPT 5610
      NDATA = 0                                                         NPT 5620
      NTYP = 0                                                          NPT 5630
      INNO(18) = 0                                                      NPT 5640
      LVX = 0                                                           NPT 5650
  104 CONTINUE                                                          NPT 5660
      IF(MU4 .GE. 2) RETURN                                             NPT 5670
  115 CONTINUE                                                          NPT 5680
      ICPU1 = ICLOCK(0)                                                 NPT 5690
C                                                                       NPT 5700
      CALL WATCH(ENDE)                                                  NPT 5710
C                                                                       NPT 5720
      REAL1 = ENDE                                                      NPT 5730
      IX(69) = NGC(19)                                                  NPT 5740
      NTITE = 0                                                         NPT 5750
      IMEM = 0                                                          NPT 5760
C                                                                       NPT 5770
      CALL IDAY(DATE)                                                   NPT 5780
C                                                                       NPT 5790
      IF(MU4 .EQ. 1) WRITE (IOUT,1001) DATE                             NPT 5800
      IF(IX(27) .EQ. 0) GOTO 107                                        NPT 5810
      REWIND IO2                                                        NPT 5820
      NLIM = KNRGN + IVX * JVX * KBVX - 1                               NPT 5830
      WRITE (IO2) (A(N),N=KNRGN,NLIM)                                   NPT 5840
      END FILE IO2                                                      NPT 5850
      REWIND IO2                                                        NPT 5860
      IRV = 1                                                           NPT 5870
C                                                                       NPT 5880
      CALL BNSB(A(K50),A(K51),KVX,IRV)                                  NPT 5890
C                                                                       NPT 5900
  107 CONTINUE                                                          NPT 5910
      MM1VX = MMAX                                                      NPT 5920
      KM1VX = KMAX                                                      NPT 5930
      NM1VX = NMAX                                                      NPT 5940
      NSM1VX = NSETVX                                                   NPT 5950
      MM1AC5 = NUAC(5)                                                  NPT 5960
      IF(MU4 .LE. 1) READ (IOIN,1002) (TITL2(I),I=1,18)                 NPT 5970
      IF(MU4 .EQ. 1) WRITE (IOUT,1004) (TITL2(I),I=1,18)                NPT 5980
      DO 111 I=1,18                                                     NPT 5990
        TITL(I) = TITL2(I)                                              NPT 6000
        TITL1(I) = BLANK                                                NPT 6010
        II = I + 18                                                     NPT 6020
        TITL(II) = TITL1(I)                                             NPT 6030
  111 CONTINUE                                                          NPT 6040
C                                                                       NPT 6050
      CALL IPTM(A,MEMORY,NTITE,IMEM,NM1VX,NSM1VX,MM1AC5)                NPT 6060
C                                                                       NPT 6070
      IF(IX(167) .EQ. 0) GOTO 113                                       NPT 6080
      IX(167) = 0                                                       NPT 6090
      GOTO 104                                                          NPT 6100
  113 CONTINUE                                                          NPT 6110
      JIVX = JVX * IVX                                                  NPT 6120
      JIP1VX = JVX * IVXP1                                              NPT 6130
      JP1IXZ = JVXP1 * IVZ                                              NPT 6140
      MVZ = MAX0(MVX,NSETVX+2)                                          NPT 6150
C                                                                       NPT 6160
C     IX3738 IS DIMENSION OF I/O VARIABLE AIO                           NPT 6170
C                                                                       NPT 6180
      IX3738 = IX(38) - IX(37) + 1                                      NPT 6190
C                                                                       NPT 6200
C     THE NEXT STATMENT FORCES FLAT FLUX IF NOT RESTART                 NPT 6210
C                                                                       NPT 6220
      IF(NGC(2) .EQ. 0) IX(39) = 1                                      NPT 6230
      ISIZ = K41 - K24 + 1                                              NPT 6240
C                                                                       NPT 6250
      CALL CALR(A,A(K19),NTITE,NVX,MVX,IX3738,MVZ,A(K24),ISIZ)          NPT 6260
C                                                                       NPT 6270
      IF(NGC(3) .EQ. 0) GOTO 114                                        NPT 6280
      END FILE IO13                                                     NPT 6290
      REWIND IO13                                                       NPT 6300
  114 CONTINUE                                                          NPT 6310
      IF(NGC(3) .EQ. 0 .AND. NGC(2) .NE. 0) REWIND IO13                 NPT 6320
      IX(3) = 0                                                         NPT 6330
      ICPU2 = ICLOCK(0)                                                 NPT 6340
C                                                                       NPT 6350
      CALL WATCH(ENDE)                                                  NPT 6360
C                                                                       NPT 6370
      REAL2 = ENDE                                                      NPT 6380
      TOTREL = (REAL2-REAL1) / 60.                                      NPT 6390
      WRITE (NT,1005) TOTREL                                            NPT 6400
      WRITE (NT,1006)                                                   NPT 6410
      WRITE (NT,1006)                                                   NPT 6420
      IX(4) = 0                                                         NPT 6430
      IX(6) = 0                                                         NPT 6440
      IX(7) = 0                                                         NPT 6450
      IX(27) = 1                                                        NPT 6460
      MU4 = MU4 + 1                                                     NPT 6470
      GOTO 104                                                          NPT 6480
      END                                                               NPT 6490
      SUBROUTINE OUTC(A,CONC,BAL,B4,SIG,RATE,XL,XI,TX2,SCAC,NDX,MDX,KDX,OUT   10
     1 IX3738,INDTOR,MVZ,B5)                                            OUT   20
C                                                                       OUT   30
C     OUTC --136 ***CITATION*** PROBLEM EDIT CONTROL/ CF-CALR           OUT   40
C                ***VERKUERZT**                                         OUT   50
C     IF(INDTOR.EQ.0) INITILIZATION CALCULATION                         OUT   60
C                                                                       OUT   70
C     TX2 IN THE FOLLOWING LINE IS BIK IN CALR                          OUT   80
C                                                                       OUT   90
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,OUT  100
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   OUT  110
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), OUT  120
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    OUT  130
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    OUT  140
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   OUT  150
     6 IXPUT(9999),XPUT(9999)                                           OUT  160
C                                                                       OUT  170
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  OUT  180
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), OUT  190
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)OUT  200
     3 ,PDK(211)                                                        OUT  210
C                                                                       OUT  220
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   OUT  230
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKOUT  240
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    OUT  250
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  OUT  260
     4 ITMAX,ITIME,BET(211),DEL(211)                                    OUT  270
C                                                                       OUT  280
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           OUT  290
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    OUT  300
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),OUT  310
     3 NCLASS(9999),NDP(9999)                                           OUT  320
C                                                                       OUT  330
      COMMON /AKADD/ KAY(1),K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13, OUT  340
     1 K131,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,K28,OUT  350
     2 K29,K30,K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,K41,K42,K43,K44, OUT  360
     3 K45,K46,K47,K48,K49,K50,K51,K52,K53,K54,K55,K56,K57,K58,K59,K60, OUT  370
     4 K61,K62,K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,K74,K75,K76, OUT  380
     5 K77,K78,K79,K80,K81,K82,K83,K84,K85,K86,K87,K88,K89,K90,K91,K92, OUT  390
     6 K93,K94,K95,K96,K97,K98,K99,K100,NDATA,KNRGN,KNCOMP,KPVOL,KRVOL, OUT  400
     7 MEMVRY,MEMX                                                      OUT  410
C                                                                       OUT  420
      COMMON /AVDLM/ IVDLM(1),IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1, OUT  430
     1 KBVXP1,NSETVX,NVO,IVO,IVZ,KVZ,NCRP,NSPA,N3DDIM,NBLOCK,JIVX,JIP1VXOUT  440
     2 ,JP1IXZ,IOVX,IOVZ                                                OUT  450
C                                                                       OUT  460
      COMMON /MU/ MU4,NCIT,NGEOM,I9,IOM,M34                             OUT  470
C                                                                       OUT  480
CFZJ042                                                       09.09.05  OUT  490
CFZJ048 enlarged dimension                                    11.04.07  OUT  500
      COMMON /VARDIT/ B(5000000)                                        OUT  510
C                                                                       OUT  520
CFZJ042                                                       09.09.05  OUT  530
      COMMON /ADDRT/ KX(240),KY(240),LZ(240),NENDPT                     OUT  540
C                                                                       OUT  550
      DIMENSION A(1),CONC(NDX,MDX),BAL(KDX,MDX,8),B4(MDX,KDX),          OUT  560
     1 SIG(KDX,MDX,10),RATE(NDX,MDX,10),XL(6,KDX),XI(KDX),TX2(KDX,20),  OUT  570
     2 SCAC(KDX,MDX,KDX),B5(MDX,KDX),TX1(20)                            OUT  580
C                                                                       OUT  590
      EQUIVALENCE(LZ(88),IPDTH),(LZ(224),IFFTH)                         OUT  600
C                                                                       OUT  610
 1000 FORMAT (1H0)                                                      OUT  620
 1002 FORMAT (1H0,'GROSS NEUTRON BALANCE',1H ,'             '/1H0,'GRP  OUT  630
     1LFT LEAKAGE ',1H ,'TOP LEAKAGE  RIT LEAKAGE  BOT LEAKAGE ',1H ,'FNOUT  640
     2T LEAKAGE  BAK LEAKAGE',2H  ,'B**2 LOSSES    1/V LOSS   XENON LOSSOUT  650
     3')                                                                OUT  660
 1003 FORMAT (1H ,I2,1PE14.5,8E13.5)                                    OUT  670
 1004 FORMAT (1H0,'SUM',9(1PE13.5))                                     OUT  680
 1005 FORMAT (1H0/1H ,'GRP  ABSORPTIONS  OUT-SCATTER    SOURCE',5H     ,OUT  690
     1 'IN-SCATTER  TOTAL LOSSES  TOTAL GAINS                    ',8H   OUT  700
     2     ,'PROD/ABSORP')                                              OUT  710
 1006 FORMAT (1H ,I2,1PE14.5,5E13.5,E39.5)                              OUT  720
 1007 FORMAT (1H0/1H0,'GROUP NEUTRON BALANCE FOR EACH ZONE')            OUT  730
 1008 FORMAT (//1H0,'ZONE NUMBER',I4,'--',4H    ,'VOLUME',1PE13.5)      OUT  740
 1009 FORMAT (1H0,'GRP  ABSORPTIONS  OUT-SCATTER  B**2 LOSSES    1/V',1HOUT  750
     1 ,'LOSS   XENON LOSS   IN-SCATTER     SOURCE    POWER(WATTS)',2H  OUT  760
     2 ,'AVERAGE FLUX')                                                 OUT  770
 1010 FORMAT (1H ,I2,1PE14.5,8E13.5)                                    OUT  780
 1011 FORMAT (1H0,'SUM',9(1PE13.5)//)                                   OUT  790
 1012 FORMAT (1H0/1H0,'AVERAGE FLUXES BY ZONE AND GROUP')               OUT  800
 1013 FORMAT (1H0,'ZONE',I4,'--')                                       OUT  810
 1014 FORMAT (1H ,1PE14.5,8E13.5)                                       OUT  820
 1015 FORMAT (1H0/1H0,'ZONE AVERAGE POWER DENSITIES(WATTS/CC)')         OUT  830
 1016 FORMAT (1H0/1H0,'TOTAL LEAKAGE ',4X,'CORE LEAKAGE',4X,'REFLECTOR AOUT  840
     1BSORPTIONS')                                                      OUT  850
 1017 FORMAT (1H0,1PE12.5,6X,1PE12.5,6X,1PE12.5)                        OUT  860
 1077 FORMAT  (1H0,'***CAUTION*** COMPOSITION BALANCE DOES NOT CONTAIN FOUT  870
     1IXED SOURCE***')                                                  OUT  880
 1201 FORMAT (18A4)                                                     OUT  890
 1202 FORMAT (2I5,6(1PE10.3))                                           OUT  900
 1203 FORMAT (1H ,'GRP  PRODUCTIONS  PROD/ABSORP    LEAKAGE     BUCKLINGOUT  910
     >')                                                                OUT  920
C                                                                       OUT  930
C                                                                       OUT  940
      KNBAL = KVX                                                       OUT  950
      IF(NGC(19) .GT. 0) GOTO 101                                       OUT  960
      IF(INDTOR .NE. 0) GOTO 101                                        OUT  970
      REWIND IO1                                                        OUT  980
      DO 100 M=1,MMAX                                                   OUT  990
        NACT = NXSET(M)                                                 OUT 1000
        NSET = NXODR(NACT)                                              OUT 1010
        JM = NJM(NSET)                                                  OUT 1020
        WRITE (IO1) (CONC(N,M),N=1,JM)                                  OUT 1030
  100 CONTINUE                                                          OUT 1040
      END FILE IO1                                                      OUT 1050
      REWIND IO1                                                        OUT 1060
  101 CONTINUE                                                          OUT 1070
      NGMI = 0                                                          OUT 1080
      IF(NUAC(5) .GT. 10) NGMI = 1                                      OUT 1090
      RECPRK = SPARE(18)                                                OUT 1100
      DO 103 M=1,MMAX                                                   OUT 1110
        AVZPD(M) = 0.0                                                  OUT 1120
  103 CONTINUE                                                          OUT 1130
      DO 105 N=1,20                                                     OUT 1140
        TX1(N) = 0.0                                                    OUT 1150
        DO 104 K=1,KNBAL                                                OUT 1160
          TX2(K,N) = 0.0                                                OUT 1170
  104   CONTINUE                                                        OUT 1180
  105 CONTINUE                                                          OUT 1190
      ISH = IX(5)                                                       OUT 1200
      IF(IX(24) .GT. 0) ISH = IX(17)                                    OUT 1210
      DO 110 M=1,MMAX                                                   OUT 1220
        DO 109 K=1,KMAX                                                 OUT 1230
          BAL(K,M,8) = 0.0                                              OUT 1240
          BAL(K,M,1) = 0.0                                              OUT 1250
          V = B4(M,K) * ZONVOL(M)                                       OUT 1260
          BAL(K,M,2) = B5(M,K)                                          OUT 1270
          IF(ISH .EQ. 1 .AND. NSRH(11) .EQ. 0) BAL(K,M,2) = BAL(K,M,2) +OUT 1280
     1     V * SIG(K,M,5)                                               OUT 1290
          BAL(K,M,3) = V * SIG(K,M,4)                                   OUT 1300
          BAL(K,M,4) = V * SIG(K,M,7)                                   OUT 1310
          BAL(K,M,5) = V * SIG(K,M,2)                                   OUT 1320
          BAL(K,M,7) = V * SIG(K,M,9)                                   OUT 1330
          BAL(K,M,2) = BAL(K,M,2) - BAL(K,M,7)                          OUT 1340
C                                                                       OUT 1350
C*********SEARCH OPTIONS                                                OUT 1360
C                                                                       OUT 1370
          IF(ISH .EQ. -5) GOTO 107                                      OUT 1380
          IF(ISH) 106,107,107                                           OUT 1390
  106     BAL(K,M,1) = V * SIG(K,M,5) * SPARE(27)                       OUT 1400
          IF(ISH .NE. -2) GOTO 107                                      OUT 1410
          T9 = BAL(K,M,1)                                               OUT 1420
          BAL(K,M,1) = 0.0                                              OUT 1430
          IF(T9 .NE. 0.0) BAL(K,M,7) = T9                               OUT 1440
  107     CONTINUE                                                      OUT 1450
          T1 = 0.0                                                      OUT 1460
          DO 108 KK=1,KMAX                                              OUT 1470
            T1 = T1 + SCAC(KK,M,K) * B4(M,KK)                           OUT 1480
  108     CONTINUE                                                      OUT 1490
          BAL(K,M,6) = T1 * ZONVOL(M)                                   OUT 1500
  109   CONTINUE                                                        OUT 1510
  110 CONTINUE                                                          OUT 1520
      IF(NUAC(3) .GT. 0) GOTO 139                                       OUT 1530
C                                                                       OUT 1540
      CALL RQED(IX(105),IND)                                            OUT 1550
C                                                                       OUT 1560
      IF(IND .NE. 0) GOTO 124                                           OUT 1570
      WRITE (IOUT,1000)                                                 OUT 1580
      WRITE (IOUT,1000)                                                 OUT 1590
      WRITE (IOUT,1002)                                                 OUT 1600
      DO 114 K=1,KMAX                                                   OUT 1610
        DO 113 N=1,6                                                    OUT 1620
          TX1(N+13) = TX1(N+13) + XL(N,K)                               OUT 1630
          TX2(K,12) = TX2(K,12) + XL(N,K)                               OUT 1640
  113   CONTINUE                                                        OUT 1650
  114 CONTINUE                                                          OUT 1660
C                                                                       OUT 1670
C     AB-2,PR-3,FS-4,OS-5,IS-6,BSQ-7,XE-8,POS-9,SO-10,K-11,TOTLOSS-12,  OUT 1680
C     TOTGAIN-13                                                        OUT 1690
C                                                                       OUT 1700
      DO 117 K=1,KMAX                                                   OUT 1710
        DO 116 M=1,MMAX                                                 OUT 1720
          DO 115 N=1,8                                                  OUT 1730
            TX2(K,N) = TX2(K,N) + BAL(K,M,N)                            OUT 1740
            TX1(N) = TX1(N) + BAL(K,M,N)                                OUT 1750
  115     CONTINUE                                                      OUT 1760
  116   CONTINUE                                                        OUT 1770
        TX2(K,12) = TX2(K,2) + TX2(K,5) + TX2(K,7) + TX2(K,8) + TX2(K,1)OUT 1780
     1   + XL(1,K) + XL(2,K) + XL(3,K) + XL(4,K) + XL(5,K) + XL(6,K)    OUT 1790
        TX1(12) = TX1(12) + TX2(K,12)                                   OUT 1800
  117 CONTINUE                                                          OUT 1810
      DO 118 K=1,KMAX                                                   OUT 1820
        WRITE (IOUT,1003) K,(XL(N,K),N=1,6),TX2(K,7),TX2(K,1),TX2(K,8)  OUT 1830
  118 CONTINUE                                                          OUT 1840
      WRITE (IOUT,1004) (TX1(N),N=14,19),TX1(7),TX1(1),TX1(8)           OUT 1850
      WRITE (IOUT,1005)                                                 OUT 1860
      DO 123 K=1,KMAX                                                   OUT 1870
        TX2(K,10) = RECPRK * XI(K) * TX1(3)                             OUT 1880
        TX1(10) = TX1(10) + TX2(K,10)                                   OUT 1890
        TX2(K,13) = TX2(K,6) + TX2(K,10)                                OUT 1900
        TX1(13) = TX1(13) + TX2(K,13)                                   OUT 1910
        T1 = TX2(K,12) - TX2(K,6)                                       OUT 1920
        IF(T1) 119,120,119                                              OUT 1930
  119   TX2(K,11) = XI(K) * TX1(3) / T1                                 OUT 1940
  120   T2 = 0.0                                                        OUT 1950
        IF(TX2(K,2)) 121,122,121                                        OUT 1960
  121   T2 = TX2(K,3) / TX2(K,2)                                        OUT 1970
  122   WRITE (IOUT,1006) K,TX2(K,2),TX2(K,5),TX2(K,10),TX2(K,6),       OUT 1980
     1   TX2(K,12),TX2(K,13),T2                                         OUT 1990
  123 CONTINUE                                                          OUT 2000
      WRITE (IOUT,1004) TX1(2),TX1(5),TX1(10),TX1(6),TX1(12),TX1(13)    OUT 2010
      REFLAB = 0.0                                                      OUT 2020
      DO 180 K=1,KMAX                                                   OUT 2030
        DO 180 M=1,MMAX                                                 OUT 2040
          IF(BAL(K,M,4) .LT. 1.0) REFLAB = REFLAB + BAL(K,M,2)          OUT 2050
  180 CONTINUE                                                          OUT 2060
      TOTLEK = 0.0                                                      OUT 2070
      DO 181 N=1,6                                                      OUT 2080
        DO 181 K=1,KMAX                                                 OUT 2090
          TOTLEK = TOTLEK + XL(N,K)                                     OUT 2100
  181 CONTINUE                                                          OUT 2110
      CORLEK = TOTLEK + REFLAB                                          OUT 2120
      WRITE (IOUT,1016)                                                 OUT 2130
      WRITE (IOUT,1017) TOTLEK,CORLEK,REFLAB                            OUT 2140
      IF(MMAX .EQ. 1) GOTO 131                                          OUT 2150
  124 CONTINUE                                                          OUT 2160
C                                                                       OUT 2170
      CALL RQED(IX(106),IND)                                            OUT 2180
C                                                                       OUT 2190
      IF(IND .NE. 0) GOTO 131                                           OUT 2200
      WRITE (IOUT,1007)                                                 OUT 2210
      IF(NGC(10) .EQ. -5) WRITE (IOUT,1077)                             OUT 2220
      IPUNCH = 7                                                        OUT 2230
      IF(IEDG(24) .EQ. 1) WRITE (IPUNCH,1201) TITL1                     OUT 2240
      DO 130 M=1,MMAX                                                   OUT 2250
        DO 126 N=1,9                                                    OUT 2260
          TX1(N) = 0.0                                                  OUT 2270
  126   CONTINUE                                                        OUT 2280
        WRITE (IOUT,1008) M,ZONVOL(M)                                   OUT 2290
        WRITE (IOUT,1009)                                               OUT 2300
        T1 = 0.0                                                        OUT 2310
        DO 127 K=1,KMAX                                                 OUT 2320
          T1 = T1 + BAL(K,M,3)                                          OUT 2330
  127   CONTINUE                                                        OUT 2340
        T11 = 0.0                                                       OUT 2350
        T2 = 0.0                                                        OUT 2360
        DO 129 K=1,KMAX                                                 OUT 2370
          T11 = T11 + B4(M,K)                                           OUT 2380
          T3 = RECPRK * XI(K) * T1                                      OUT 2390
          T2 = T2 + T3                                                  OUT 2400
          POWER = BAL(K,M,4) * XMIS(4)                                  OUT 2410
          WRITE (IOUT,1010) K,BAL(K,M,2),BAL(K,M,5),BAL(K,M,7),         OUT 2420
     1     BAL(K,M,1),BAL(K,M,8),BAL(K,M,6),T3,POWER,B4(M,K)            OUT 2430
          DO 128 N=1,8                                                  OUT 2440
            TX1(N) = TX1(N) + BAL(K,M,N)                                OUT 2450
  128     CONTINUE                                                      OUT 2460
  129   CONTINUE                                                        OUT 2470
        POWER = TX1(4) * XMIS(4)                                        OUT 2480
        WRITE (IOUT,1011) TX1(2),TX1(5),TX1(7),TX1(1),TX1(8),TX1(6),T2, OUT 2490
     1   POWER,T11                                                      OUT 2500
        WRITE (IOUT,1203)                                               OUT 2510
        DO 201 K=1,KMAX                                                 OUT 2520
          T11 = 0.0                                                     OUT 2530
          IF(BAL(K,M,2) .NE. 0.0) T11 = BAL(K,M,3) / BAL(K,M,2)         OUT 2540
          SRR = BAL(K,M,6) + RECPRK * XI(K) * T1 - BAL(K,M,5) -         OUT 2550
     1     BAL(K,M,2) - BAL(K,M,1) - BAL(K,M,8)                         OUT 2560
          BRR = SIG(K,M,1) * B4(M,K) * ZONVOL(M)                        OUT 2570
          BBQQ = 0.0                                                    OUT 2580
          IF(BRR .GT. 0.0) BBQQ = SRR / BRR                             OUT 2590
          WRITE (IOUT,1010) K,BAL(K,M,3),T11,SRR,BBQQ                   OUT 2600
          IF(IEDG(24) .EQ. 1) WRITE (IPUNCH,1202) M,K,SIG(K,M,1),B4(M,K)OUT 2610
     1     ,BBQQ                                                        OUT 2620
  201   CONTINUE                                                        OUT 2630
        T12 = 0.0                                                       OUT 2640
        IF(TX1(2) .GT. 0.0) T12 = T1 / TX1(2)                           OUT 2650
        T13 = TX1(6) + T2 - TX1(5) - TX1(2)                             OUT 2660
        WRITE (IOUT,1011) T1,T12,T13                                    OUT 2670
  130 CONTINUE                                                          OUT 2680
  131 CONTINUE                                                          OUT 2690
      REWIND M34                                                        OUT 2700
      WRITE (M34) XKEF1,KMAX,MMAX,((B4(MI,KI),KI=1,KMAX),MI=1,MMAX)     OUT 2710
      REWIND M34                                                        OUT 2720
C                                                                       OUT 2730
      CALL RQED(IX(109),IND)                                            OUT 2740
C                                                                       OUT 2750
      IF(IND .NE. 0) GOTO 134                                           OUT 2760
      WRITE (IOUT,1012)                                                 OUT 2770
      DO 133 M=1,MMAX                                                   OUT 2780
        WRITE (IOUT,1013) M                                             OUT 2790
        WRITE (IOUT,1014) (B4(M,K),K=1,KMAX)                            OUT 2800
  133 CONTINUE                                                          OUT 2810
  134 CONTINUE                                                          OUT 2820
C                                                                       OUT 2830
      CALL RQED(IX(112),IND)                                            OUT 2840
C                                                                       OUT 2850
      IF(IND .NE. 0) GOTO 139                                           OUT 2860
      WRITE (IOUT,1015)                                                 OUT 2870
      DO 138 M=1,MMAX                                                   OUT 2880
        DO 137 K=1,KMAX                                                 OUT 2890
          IF(ZONVOL(M)) 136,137,136                                     OUT 2900
  136     CONTINUE                                                      OUT 2910
          AVZPD(M) = AVZPD(M) + BAL(K,M,4) / ZONVOL(M)                  OUT 2920
  137   CONTINUE                                                        OUT 2930
  138 CONTINUE                                                          OUT 2940
      WRITE (IOUT,1014) (AVZPD(M),M=1,MMAX)                             OUT 2950
  139 CONTINUE                                                          OUT 2960
      IF(IX(150) .LE. 0) GOTO 140                                       OUT 2970
C                                                                       OUT 2980
      CALL CMXS(SIG,SCAC,XI,BAL,A(K6),B4,ZONVOL,KVX,MVX)                OUT 2990
C                                                                       OUT 3000
  140 CONTINUE                                                          OUT 3010
C                                                                       OUT 3020
      CALL RQED(IX(110),IND)                                            OUT 3030
C                                                                       OUT 3040
      IF(IND .NE. 0) GOTO 144                                           OUT 3050
      IND = 0                                                           OUT 3060
      IF(NGMI .EQ. 1) GOTO 142                                          OUT 3070
C                                                                       OUT 3080
      CALL POUT(A(K24),A(K41),IND,A(K19),IVX,JVX,KBVX,KVX,B(KX(IPDTH)), OUT 3090
     1 B(KX(IFFTH)))                                                    OUT 3100
CFZJ023                                                       29.01.04  OUT 3110
C                                                                       OUT 3120
      GOTO 143                                                          OUT 3130
  142 CONTINUE                                                          OUT 3140
C                                                                       OUT 3150
      CALL KOUT(A(K24),A(K41),IND,A(K19),IVX,JVX,KBVX,KVX,JIVX)         OUT 3160
C                                                                       OUT 3170
  143 CONTINUE                                                          OUT 3180
  144 CONTINUE                                                          OUT 3190
      IF(IX(113) .EQ. 0 .AND. IX(114) .EQ. 0) GOTO 151                  OUT 3200
      IPRN = 0                                                          OUT 3210
      IF(NGMI .EQ. 1) GOTO 146                                          OUT 3220
C                                                                       OUT 3230
      CALL PDWT(A(K24),A(K41),A(KNRGN),A(KPVOL),IPRN,IVX,JVX,KVX,LVX,SIGOUT 3240
     1 ,A(KNCOMP),MVX)                                                  OUT 3250
C                                                                       OUT 3260
      GOTO 147                                                          OUT 3270
  146 CONTINUE                                                          OUT 3280
C                                                                       OUT 3290
      CALL KDWT(A(K24),A(K41),A(KNRGN),A(KPVOL),IPRN,IVX,JVX,KBVX,KVX,  OUT 3300
     1 LVX,JIVX,SIG,A(KNCOMP),MVX)                                      OUT 3310
C                                                                       OUT 3320
  147 CONTINUE                                                          OUT 3330
C                                                                       OUT 3340
      CALL RQED(IX(114),IND)                                            OUT 3350
C                                                                       OUT 3360
      IF(IND .NE. 0) GOTO 151                                           OUT 3370
      IND = 1                                                           OUT 3380
      IF(NGMI .EQ. 1) GOTO 149                                          OUT 3390
C                                                                       OUT 3400
CFZJ023                                                       29.01.04  OUT 3410
      CALL POUT(A(K24),A(K41),IND,A(K19),IVX,JVX,KBVX,KVX,B(KX(IPDTH)), OUT 3420
     1 B(KX(IFFTH)))                                                    OUT 3430
C                                                                       OUT 3440
      GOTO 150                                                          OUT 3450
  149 CONTINUE                                                          OUT 3460
C                                                                       OUT 3470
      CALL KOUT(A(K24),A(K41),IND,A(K19),IVX,JVX,KBVX,KVX,JIVX)         OUT 3480
C                                                                       OUT 3490
  150 CONTINUE                                                          OUT 3500
  151 CONTINUE                                                          OUT 3510
C                                                                       OUT 3520
      CALL RQED(IX(115),IND)                                            OUT 3530
C                                                                       OUT 3540
      IF(IND .NE. 0) GOTO 155                                           OUT 3550
      IF(IX(113) .NE. 0 .OR. IX(114) .NE. 0) GOTO 153                   OUT 3560
      IPRN = 1                                                          OUT 3570
      IF(NGMI .EQ. 1) GOTO 152                                          OUT 3580
C                                                                       OUT 3590
      CALL PDWT(A(K24),A(K41),A(KNRGN),A(KPVOL),IPRN,IVX,JVX,KVX,LVX,SIGOUT 3600
     1 ,A(KNCOMP),MVX)                                                  OUT 3610
C                                                                       OUT 3620
      GOTO 153                                                          OUT 3630
  152 CONTINUE                                                          OUT 3640
C                                                                       OUT 3650
      CALL KDWT(A(K24),A(K41),A(KNRGN),A(KPVOL),IPRN,IVX,JVX,KBVX,KVX,  OUT 3660
     1 LVX,JIVX,SIG,A(KNCOMP),MVX)                                      OUT 3670
C                                                                       OUT 3680
  153 CONTINUE                                                          OUT 3690
      IND = 8                                                           OUT 3700
      IF(NGMI .EQ. 1) GOTO 154                                          OUT 3710
C                                                                       OUT 3720
      CALL POUT(A(K24),A(K41),IND,A(K19),IVX,JVX,KBVX,KVX,B(KX(IPDTH)), OUT 3730
     1 B(KX(IFFTH)))                                                    OUT 3740
C                                                                       OUT 3750
      GOTO 155                                                          OUT 3760
  154 CONTINUE                                                          OUT 3770
C                                                                       OUT 3780
      CALL KOUT(A(K24),A(K41),IND,A(K19),IVX,JVX,KBVX,KVX,JIVX)         OUT 3790
C                                                                       OUT 3800
  155 CONTINUE                                                          OUT 3810
      IF(NGC(19) .GT. 0) GOTO 164                                       OUT 3820
C                                                                       OUT 3830
      CALL RQED(IX(116),IND)                                            OUT 3840
C                                                                       OUT 3850
      IF(IND .NE. 0) GOTO 159                                           OUT 3860
      IF(NGMI .EQ. 1) GOTO 157                                          OUT 3870
C                                                                       OUT 3880
      CALL NUDN(A(K7),A(KNCOMP),A(K24),A(KNRGN),A(K41),A(K19),IVX,JVX,  OUT 3890
     1 KBVX,KVX,LVX,MVX,NSETVX)                                         OUT 3900
C                                                                       OUT 3910
      GOTO 158                                                          OUT 3920
  157 CONTINUE                                                          OUT 3930
C                                                                       OUT 3940
      CALL KUDN(A(K7),A(KNCOMP),A(K24),A(KNRGN),A(K41),A(K19),IVX,JVX,  OUT 3950
     1 KBVX,KVX,LVX,MVX,NSETVX,JIVX)                                    OUT 3960
C                                                                       OUT 3970
  158 CONTINUE                                                          OUT 3980
  159 CONTINUE                                                          OUT 3990
      NND = 0                                                           OUT 4000
      IF(IX(2) .EQ. 0) GOTO 160                                         OUT 4010
C                                                                       OUT 4020
      CALL RQED(IX(116),IND)                                            OUT 4030
C                                                                       OUT 4040
      IF(IND .NE. 0) GOTO 163                                           OUT 4050
  160 CONTINUE                                                          OUT 4060
      IF(IX(117) .LE. 0) GOTO 163                                       OUT 4070
  161 CONTINUE                                                          OUT 4080
      IF(NGMI .EQ. 1) GOTO 162                                          OUT 4090
      GOTO 163                                                          OUT 4100
  162 CONTINUE                                                          OUT 4110
  163 CONTINUE                                                          OUT 4120
      IF(NND .GT. 0) GOTO 164                                           OUT 4130
C                                                                       OUT 4140
      CALL RQED(IX(119),IND)                                            OUT 4150
C                                                                       OUT 4160
      IF(IND .NE. 0) GOTO 164                                           OUT 4170
      NND = 1                                                           OUT 4180
      GOTO 161                                                          OUT 4190
  164 CONTINUE                                                          OUT 4200
      IF(NGC(13) .GT. 0) GOTO 166                                       OUT 4210
      IF(IX(24) .EQ. 0) GOTO 167                                        OUT 4220
      IX(24) = 0                                                        OUT 4230
  166 CONTINUE                                                          OUT 4240
      IX6364 = K63 - K64                                                OUT 4250
  167 CONTINUE                                                          OUT 4260
      IF(NGC(19) .GT. 0) GOTO 171                                       OUT 4270
      IF(IX(2) .EQ. 1) GOTO 168                                         OUT 4280
      IF(INDTOR .NE. 0) GOTO 169                                        OUT 4290
  168 CONTINUE                                                          OUT 4300
      IO1X16 = 0                                                        OUT 4310
      IF(IX(2) .EQ. 1) IO1X16 = 16                                      OUT 4320
  169 CONTINUE                                                          OUT 4330
      IO1X16 = 0                                                        OUT 4340
      IF(NGC(1) .NE. 0) GOTO 171                                        OUT 4350
C                                                                       OUT 4360
      CALL RQED(IX(102),IND)                                            OUT 4370
C                                                                       OUT 4380
  171 CONTINUE                                                          OUT 4390
      RETURN                                                            OUT 4400
      END                                                               OUT 4410
      SUBROUTINE CH6TTT(MAFIA,IMAT,KMAT30,KONID)                        H6T   10
C                                                                       H6T   20
C     TTTT: THIS PROGRAM PRODUCES A 30-GROUP THERMOS-LIBRARY OUT OF THE H6T   30
C     96-GROUP THERMALIZATION-LIBRARY.                                  H6T   40
C                                                                       H6T   50
      DIMENSION E(96),S(96),SIGA(96),DELTA(96),PHI(96),XA(96),XF(96),   H6T   60
     1 XS(96),SIGN(96),SIGTR(96),SIGS(96),SELS(97),DID(18),BEG(51),     H6T   70
     2 LBON(51),NDIV(96),VEC(5),TIN(50),XTR(96),FINU(96),EKVR(96,96),   H6T   80
     3 TIM(96,96),DIF(6,50),IDKER(2,20),NPHI(20),FLUSS(20,100),JJ(50),  H6T   90
     4 V(100),DELV(100),YA(30),YS(30),YF(30),YT(30),YP(30),DY(30),NG(30)H6T  100
     5 ,MG(30),YV(30),P(30,30),IMAT(KMAT),KONID(KMAT30,2),DUMM(96)      H6T  110
C                                                                       H6T  120
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    H6T  130
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    H6T  140
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIH6T  150
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 H6T  160
C                                                                       H6T  170
      COMMON /BLOCKT/ DELT,NOIT,TIN,SIGA,SIGN,SIGS,SIGTR,EKVR,PHI,E,BN, H6T  180
     1 SELS,DID,BEG,LBON,NDIV,DIF,TIM,FMIC,NEVR,NBG,NALPHA,NKER,MNBG,   H6T  190
     2 FINU,DELTA,KDIV,XA,XS,XF,XTR,S,T,UB,DFO,IDT,VEC                  H6T  200
C                                                                       H6T  210
      COMMON /TT/ JNTAPE                                                H6T  220
C                                                                       H6T  230
      COMMON /FLEX/ DUMMY(6),ERR                                        H6T  240
C                                                                       H6T  250
CFZJ055                                                       25.09.07  H6T  260
C                                                                       H6T  270
      CHARACTER*9 DSIDTP,THERM1/'therm1515'/,THERM2/'therm1516'/,THERM3 H6T  280
     1 /'therm1517'/,THERM4/'therm1518'/,THERM5/'therm1519'/            H6T  290
C                                                                       H6T  300
      EQUIVALENCE(JTPE2,N5),(JTPE3,N6),(JTPE4,NTH),(JTPE6,N9)           H6T  310
C                                                                       H6T  320
    7 FORMAT (1H1/1H0)                                                  H6T  330
    8 FORMAT (1H1,2X,1HI,2X,10(4HP(I,,I2,1H),5X))                       H6T  340
   15 FORMAT (10I5/10I5/10I5)                                           H6T  350
   54 FORMAT (//' ID-NO OF THERMALIZATION LIBRARY IN ERROR')            H6T  360
  155 FORMAT (A9,I3,4I6,E12.5)                                          H6T  370
  156 FORMAT (12I6)                                                     H6T  380
  351 FORMAT ('1 GROUP, FLUX-SETS, USED FOR CONDENSATION.'//' SET-NO.:',H6T  390
     1 8(4X,I6,5X))                                                     H6T  400
  352 FORMAT (I6,8E15.6)                                                H6T  410
  499 FORMAT (1H1,20X,5H  NO ,11X,6HENERGY,8X,8HDELTA(E),7X,5H  NO ,11X,H6T  420
     1 6HENERGY,8X,8HDELTA(E)/(20X,I5,5X,2E15.5,I10,5X,2E15.5))         H6T  430
  502 FORMAT (20X,'ERROR IN READING ABSORBERS,',2X,I5,' HAS BEEN READ') H6T  440
  504 FORMAT (1H1,18X,5HYV(I),10X,5HDY(I),7X,5HNG(I),10X,5HMG(I)//(5X,I2H6T  450
     1 ,4X,2E15.5,I10,5X,I10))                                          H6T  460
  505 FORMAT (1H ,10X,5HXA(I),10X,5HXS(I),10X,5H S(I),10X,6HXTR(I),10X, H6T  470
     1 5HXA(I),10X,5HXS(I),10X,5H S(I),10X,6HXTR(I) //(3X,I2,4E15.5,I3, H6T  480
     2 E12.5,3E15.5))                                                   H6T  490
  506 FORMAT (1H1,15X,5HYA(I),10X,5HYS(I),10X,5HYF(I),10X,5HYT(I)//(5X, H6T  500
     1 I2,4X,4E15.5))                                                   H6T  510
  507 FORMAT (2X,I2,1X,10E12.5)                                         H6T  520
  510 FORMAT (///// ' *** CONGRATULATIONS. HERE IS A WONDERFUL NEW THERMH6T  530
     1OS-LIBRARY WITH THE ID.NO.',I7,' ***'/)                           H6T  540
 1160 FORMAT ('0CONDENSATION WITH FLUX SET NO.',I6)                     H6T  550
 1301 FORMAT (11H1SCATTERER ,I4,1H,,18A4/14H0TEMPERATURE =,E14.7,11H DEGH6T  560
     1.KELVIN,33H     FREE ATOM SCATTERING SIGMA =,E14.7/)              H6T  570
 1302 FORMAT (11H0SCATTERER ,I4,1H,,18A4/14H0TEMPERATURE =,E14.7,11H DEGH6T  580
     1.KELVIN,33H     FREE ATOM SCATTERING SIGMA =,E14.7/)              H6T  590
 5001 FORMAT (I10)                                                      H6T  600
 5002 FORMAT (5E14.6)                                                   H6T  610
 5003 FORMAT (18A4)                                                     H6T  620
 5004 FORMAT (4I10)                                                     H6T  630
 5005 FORMAT (4E14.6)                                                   H6T  640
 7711 FORMAT (10H1ABSORBER ,I2,1H,,18A4/)                               H6T  650
 7712 FORMAT (10H0ABSORBER ,I3,1H,,18A4/)                               H6T  660
C                                                                       H6T  670
C                                                                       H6T  680
      REWIND N9                                                         H6T  690
      IX = 30                                                           H6T  700
      ITAUSN = 1000                                                     H6T  710
      IDKER(1,1) = 1                                                    H6T  720
      IZWEI = 2                                                         H6T  730
C                                                                       H6T  740
C     N5     INPUT UNIT NUMBER                                          H6T  750
C     N6     OUTPUT UNIT NUMBER                                         H6T  760
C     N9     UNIT NUMBER OF THE THERMAL LIBRARY                         H6T  770
C     NTH    UNIT NUMBER OF THE NEW CONDENSED THERMOS LIBRARY           H6T  780
C                                                                       H6T  790
      K1000 = 0                                                         H6T  800
      NEVR = 96                                                         H6T  810
      NEVRPR = NEVR - 1                                                 H6T  820
      DO 9  N=1,NEVR                                                    H6T  830
        FLUSS(1,N) = 0.                                                 H6T  840
    9 CONTINUE                                                          H6T  850
      DO 10 I=1,48                                                      H6T  860
        JJ(I) = I + 48                                                  H6T  870
   10 CONTINUE                                                          H6T  880
      READ (N9,5001) NTAPE                                              H6T  890
C                                                                       H6T  900
CARD TTTT3                                                              H6T  910
C                                                                       H6T  920
      READ (N5,155) DSIDTP,ITTTT,IEBE,ITUTEU,KERNE,ITOT,T               H6T  930
C                                                                       H6T  940
      IF(DSIDTP .EQ. THERM1) OPEN(NTH,FILE='Libraries\therm1515')       H6T  950
      IF(DSIDTP .EQ. THERM2) OPEN(NTH,FILE='Libraries\therm1516')       H6T  960
      IF(DSIDTP .EQ. THERM3) OPEN(NTH,FILE='Libraries\therm1517')       H6T  970
      IF(DSIDTP .EQ. THERM4) OPEN(NTH,FILE='Libraries\therm1518')       H6T  980
      IF(DSIDTP .EQ. THERM5) OPEN(NTH,FILE='Libraries\therm1519')       H6T  990
      IF(DSIDTP .EQ. THERM1) IDTP = 1515                                H6T 1000
      IF(DSIDTP .EQ. THERM2) IDTP = 1516                                H6T 1010
      IF(DSIDTP .EQ. THERM3) IDTP = 1517                                H6T 1020
      IF(DSIDTP .EQ. THERM4) IDTP = 1518                                H6T 1030
      IF(DSIDTP .EQ. THERM5) IDTP = 1519                                H6T 1040
      IF(KERNE .NE. ITAUSN) GOTO 12                                     H6T 1050
      KERNE = 1                                                         H6T 1060
      K1000 = 1                                                         H6T 1070
      GOTO 14                                                           H6T 1080
   12 CONTINUE                                                          H6T 1090
C                                                                       H6T 1100
CARD TTTT4                                                              H6T 1110
C                                                                       H6T 1120
      READ (N5,156) ((IDKER(I,J),I=1,2),J=1,KERNE)                      H6T 1130
C                                                                       H6T 1140
   14 CONTINUE                                                          H6T 1150
      IF(K1000 .EQ. 1) IDKER(2,1) = ITAUSN                              H6T 1160
      IF(JNTAPE-NTAPE) 53,5,53                                          H6T 1170
   53 WRITE (N6,54)                                                     H6T 1180
C                                                                       H6T 1190
      CALL EXIT                                                         H6T 1200
C                                                                       H6T 1210
    5 CONTINUE                                                          H6T 1220
C                                                                       H6T 1230
C     ZUORDNEN VON ISOTOPEN UND FLUSS-SAETZEN                           H6T 1240
C                                                                       H6T 1250
      LMAT = KMAT                                                       H6T 1260
      DO 1310 J=1,KERNE                                                 H6T 1270
        II = 0                                                          H6T 1280
        DO 1311 I=1,KMAT                                                H6T 1290
          IF(II .GT. 0) IMAT(I-1) = IMAT(I)                             H6T 1300
          IF(IMAT(I) .NE. IDKER(1,J)) GOTO 1311                         H6T 1310
          II = I                                                        H6T 1320
          LMAT = KMAT - 1                                               H6T 1330
 1311   CONTINUE                                                        H6T 1340
        KMAT = LMAT                                                     H6T 1350
 1310 CONTINUE                                                          H6T 1360
      LARGNR = 0                                                        H6T 1370
      IDT = 0                                                           H6T 1380
      DO 300 L=1,KMAT                                                   H6T 1390
        IDU = 100000                                                    H6T 1400
        DO 1300 I=1,KMAT                                                H6T 1410
          IF(IDT .GE. IMAT(I)) GOTO 1300                                H6T 1420
          IDU = MIN0(IDU,IMAT(I))                                       H6T 1430
          IF(IDU .EQ. IMAT(I)) II = I                                   H6T 1440
 1300   CONTINUE                                                        H6T 1450
        IDT = IMAT(II)                                                  H6T 1460
        KONID(L,1) = IMAT(II)                                           H6T 1470
        KONID(L,2) = IZWEI                                              H6T 1480
  300 CONTINUE                                                          H6T 1490
      IF(ITOT .GT. 0) KONID(1,1) = 0                                    H6T 1500
      KONZA = 0                                                         H6T 1510
      I2 = KMAT + KERNE                                                 H6T 1520
      IF(KERNE .EQ. 0) GOTO 301                                         H6T 1530
      DO 1312 J=1,KERNE                                                 H6T 1540
        KONID(J+KMAT,1) = IDKER(2,J)                                    H6T 1550
        KONID(J+KMAT,2) = 2                                             H6T 1560
 1312 CONTINUE                                                          H6T 1570
  301 CONTINUE                                                          H6T 1580
      KONABS = 0                                                        H6T 1590
      DO 302 I=1,I2                                                     H6T 1600
        IF(KONID(I,2) .EQ. 0) GOTO 303                                  H6T 1610
        KONZA = I                                                       H6T 1620
        IF(KONID(I,1) .GE. ITAUSN) GOTO 302                             H6T 1630
        KONABS = KONABS + 1                                             H6T 1640
        LARGNR = KONID(I,1)                                             H6T 1650
  302 CONTINUE                                                          H6T 1660
  303 CONTINUE                                                          H6T 1670
      INR = KONABS                                                      H6T 1680
      IF(KONID(1,1) .GT. 0) GOTO 201                                    H6T 1690
C                                                                       H6T 1700
C     ALLE ABSORBER                                                     H6T 1710
C                                                                       H6T 1720
      INR = 0                                                           H6T 1730
      READ (N9,5002) (E(I),I=1,NEVR)                                    H6T 1740
      DO 200 I=1,200                                                    H6T 1750
        READ (N9,5001) IDT                                              H6T 1760
        IF(IDT) 201,201,401                                             H6T 1770
  401   READ (N9,5003) (DUMM(IJ),IJ=1,18)                               H6T 1780
        READ (N9,5002) DUMM(1),DUMM(2)                                  H6T 1790
        DO 202 J=1,4                                                    H6T 1800
          READ (N9,5002) (DUMM(IJ),IJ=1,NEVR)                           H6T 1810
  202   CONTINUE                                                        H6T 1820
        READ (N9,5002) DUMM(1)                                          H6T 1830
        LARGNR = IDT                                                    H6T 1840
        INR = INR + 1                                                   H6T 1850
  200 CONTINUE                                                          H6T 1860
  201 CONTINUE                                                          H6T 1870
      REWIND N9                                                         H6T 1880
      READ (N9,5001) NTAPE                                              H6T 1890
      WRITE (NTH,5004) IDTP,LARGNR                                      H6T 1900
      WRITE (N6,7)                                                      H6T 1910
      READ (N9,5002) (E(I),I=1,NEVR)                                    H6T 1920
      DELTA(1) = (E(2)+E(1)) / 2.                                       H6T 1930
      DO 65 J=2,NEVRPR                                                  H6T 1940
        DELTA(J) = (E(J+1)-E(J-1)) / 2.                                 H6T 1950
   65 CONTINUE                                                          H6T 1960
      DELTA(NEVR) = E(NEVR) - E(NEVRPR)                                 H6T 1970
      WRITE (N6,499) (I,E(I),DELTA(I),JJ(I),E(I+48),DELTA(I+48),I=1,48) H6T 1980
      E02 = 1.0 / SQRT(0.0506)                                          H6T 1990
      E97 = E(96) + DELTA(96)                                           H6T 2000
      DO 11 I=1,NEVR                                                    H6T 2010
        EI1 = E97                                                       H6T 2020
        IF(I .LT. NEVR) EI1 = E(I+1)                                    H6T 2030
        EIP = SQRT(E(I)+EI1)                                            H6T 2040
        IF(I-1) 80,80,81                                                H6T 2050
   80   CONTINUE                                                        H6T 2060
        EIM = 0.0                                                       H6T 2070
        GOTO 82                                                         H6T 2080
   81   EIM = SQRT(E(I)+E(I-1))                                         H6T 2090
   82   DELV(I) = E02 * (EIP-EIM)                                       H6T 2100
        V(I) = 0.5 * E02 * (EIP+EIM)                                    H6T 2110
   11 CONTINUE                                                          H6T 2120
      IF(T .LE. 0.) GOTO 100                                            H6T 2130
      TQR = T / 293.4                                                   H6T 2140
      DO 13 I=1,NEVR                                                    H6T 2150
        VSQR = V(I) * V(I)                                              H6T 2160
        FLUSS(1,I) = VSQR * EXP(-VSQR/TQR)                              H6T 2170
   13 CONTINUE                                                          H6T 2180
  100 CONTINUE                                                          H6T 2190
C                                                                       H6T 2200
C     DIE FLUSS-SAETZE                                                  H6T 2210
C                                                                       H6T 2220
      NPHI(1) = 1                                                       H6T 2230
      NPHIA = 1                                                         H6T 2240
      NPHI(2) = 2                                                       H6T 2250
      NPHIA = 2                                                         H6T 2260
      DO 304 I=1,NEVR                                                   H6T 2270
        FLUSS(2,I) = PHI(I)                                             H6T 2280
  304 CONTINUE                                                          H6T 2290
      I2 = 0                                                            H6T 2300
  306 CONTINUE                                                          H6T 2310
      I1 = I2 + 1                                                       H6T 2320
      I2 = I1 + 7                                                       H6T 2330
      I2 = MIN0(I2,NPHIA)                                               H6T 2340
      WRITE (N6,351) (NPHI(I),I=I1,I2)                                  H6T 2350
      DO 308 N=1,NEVR                                                   H6T 2360
        WRITE (N6,352) N,(FLUSS(I,N),I=I1,I2)                           H6T 2370
        VDL = V(N) * DELV(N)                                            H6T 2380
        DO 307 I=I1,I2                                                  H6T 2390
          FLUSS(I,N) = FLUSS(I,N) * VDL                                 H6T 2400
  307   CONTINUE                                                        H6T 2410
  308 CONTINUE                                                          H6T 2420
      IF(NPHIA .GT. I2) GOTO 306                                        H6T 2430
C                                                                       H6T 2440
C     NG(I) UNTERE FEINE GRUPPE DER NEUEN GROBEN GRUPPE I               H6T 2450
C                                                                       H6T 2460
CARDS TTTT5                                                             H6T 2470
C                                                                       H6T 2480
      READ (N5,15) (NG(I),I=1,30)                                       H6T 2490
C                                                                       H6T 2500
      DO 16 I=1,29                                                      H6T 2510
        MG(I) = NG(I+1) - NG(I) - 1                                     H6T 2520
   16 CONTINUE                                                          H6T 2530
      MG(30) = NEVR - NG(30)                                            H6T 2540
      DO 2 J=1,30                                                       H6T 2550
        DY(J) = 0.0                                                     H6T 2560
        YV(J) = 0.0                                                     H6T 2570
        I1 = NG(J)                                                      H6T 2580
        I2 = NG(J) + MG(J)                                              H6T 2590
        DO 1 I=I1,I2                                                    H6T 2600
          YV(J) = YV(J) + V(I) * DELV(I)                                H6T 2610
          DY(J) = DY(J) + DELV(I)                                       H6T 2620
    1   CONTINUE                                                        H6T 2630
        YV(J) = YV(J) / DY(J)                                           H6T 2640
    2 CONTINUE                                                          H6T 2650
      WRITE (N6,504) (I,YV(I),DY(I),NG(I),MG(I),I=1,30)                 H6T 2660
      INO = 0                                                           H6T 2670
      WRITE (NTH,5005) (YV(I),DY(I),I=1,IX)                             H6T 2680
      WRITE (NTH,5004) (MG(I),I=1,IX)                                   H6T 2690
      NFL = 1                                                           H6T 2700
      IF(KONID(1,1) .GT. 0) GOTO 1130                                   H6T 2710
      DO 1128 K=1,NPHIA                                                 H6T 2720
        IF(NPHI(K) .EQ. KONID(1,2)) NFL = K                             H6T 2730
 1128 CONTINUE                                                          H6T 2740
      DO 1129 N=1,NEVR                                                  H6T 2750
        PHI(N) = FLUSS(NFL,N)                                           H6T 2760
 1129 CONTINUE                                                          H6T 2770
 1130 CONTINUE                                                          H6T 2780
   22 READ (N9,5001) IDT                                                H6T 2790
      READ (N9,5003) (DID(I),I=1,18)                                    H6T 2800
      READ (N9,5002) DFO,UB                                             H6T 2810
      READ (N9,5002) (XA(I),I=1,NEVR)                                   H6T 2820
      READ (N9,5002) (XS(I),I=1,NEVR)                                   H6T 2830
      READ (N9,5002) (XF(I),I=1,NEVR)                                   H6T 2840
      READ (N9,5002) (XTR(I),I=1,NEVR)                                  H6T 2850
      READ (N9,5002) ATOMGW                                             H6T 2860
      J = IDT                                                           H6T 2870
      IF(KONID(1,1) .EQ. 0) GOTO 130                                    H6T 2880
      IF(J .GT. LARGNR) GOTO 23                                         H6T 2890
      NFL = 0                                                           H6T 2900
      DO 1131 I=1,KONABS                                                H6T 2910
        IF(J .NE. KONID(I,1)) GOTO 1131                                 H6T 2920
        DO 1133 K=1,NPHIA                                               H6T 2930
          IF(NPHI(K) .EQ. KONID(I,2)) NFL = K                           H6T 2940
 1133   CONTINUE                                                        H6T 2950
 1131 CONTINUE                                                          H6T 2960
      IF(NFL .EQ. 0) GOTO 1123                                          H6T 2970
      DO 1132 N=1,NEVR                                                  H6T 2980
        PHI(N) = FLUSS(NFL,N)                                           H6T 2990
 1132 CONTINUE                                                          H6T 3000
  130 CONTINUE                                                          H6T 3010
      WRITE (N6,7712) J,(DID(I),I=1,18)                                 H6T 3020
      IF(ITTTT) 102,102,122                                             H6T 3030
  102 CONTINUE                                                          H6T 3040
      WRITE (N6,7711) J,(DID(I),I=1,18)                                 H6T 3050
      WRITE (N6,1160) NPHI(NFL)                                         H6T 3060
      WRITE (N6,505) (I,XA(I),XS(I),XF(I),XTR(I),JJ(I),XA(I+48),XS(I+48)H6T 3070
     1 ,XF(I+48),XTR(I+48),I=1,48)                                      H6T 3080
  122 CONTINUE                                                          H6T 3090
C                                                                       H6T 3100
C     KONDENSATION DER XA,XS,XF,XTR VON 96 ZU 30 GRUPPEN                H6T 3110
C                                                                       H6T 3120
      DO 21 J=1,30                                                      H6T 3130
        I1 = NG(J)                                                      H6T 3140
        I2 = NG(J) + MG(J)                                              H6T 3150
        YP(J) = 0.0                                                     H6T 3160
        YA(J) = 0.0                                                     H6T 3170
        YS(J) = 0.0                                                     H6T 3180
        YF(J) = 0.0                                                     H6T 3190
        YT(J) = 0.0                                                     H6T 3200
        DO 20 I=I1,I2                                                   H6T 3210
          YP(J) = YP(J) + PHI(I)                                        H6T 3220
          YA(J) = YA(J) + XA(I) * PHI(I)                                H6T 3230
          YS(J) = YS(J) + XS(I) * PHI(I)                                H6T 3240
          YF(J) = YF(J) + XF(I) * PHI(I)                                H6T 3250
          YT(J) = YT(J) + XTR(I) * PHI(I)                               H6T 3260
   20   CONTINUE                                                        H6T 3270
        YA(J) = YA(J) / YP(J)                                           H6T 3280
        YS(J) = YS(J) / YP(J)                                           H6T 3290
        YF(J) = YF(J) / YP(J)                                           H6T 3300
        YT(J) = YT(J) / YP(J)                                           H6T 3310
   21 CONTINUE                                                          H6T 3320
      IF(ITTTT) 103,103,123                                             H6T 3330
  103 CONTINUE                                                          H6T 3340
      WRITE (N6,506) (I,YA(I),YS(I),YF(I),YT(I),I=1,30)                 H6T 3350
  123 CONTINUE                                                          H6T 3360
      HXS = 0.                                                          H6T 3370
      IF(IDT .EQ. 6) HXS = 12.6                                         H6T 3380
      IF(IDT .EQ. 10) HXS = 11.5                                        H6T 3390
      IF(IDT .EQ. 12) HXS = 11.293                                      H6T 3400
      IF(IDT .EQ. 22) HXS = 9.957                                       H6T 3410
      IF(IDT .EQ. 24) HXS = 3.372                                       H6T 3420
      IF(IDT .EQ. 25) HXS = 1.348                                       H6T 3430
      IF(IDT .EQ. 26) HXS = 2.15                                        H6T 3440
      IF(IDT .EQ. 29) HXS = 4.547                                       H6T 3450
      WRITE (NTH,5001) IDT                                              H6T 3460
      WRITE (NTH,5003) (DID(I),I=1,18)                                  H6T 3470
      WRITE (NTH,5005) DFO,UB                                           H6T 3480
      WRITE (NTH,5005) (YA(I),I=1,30)                                   H6T 3490
      WRITE (NTH,5005) (YS(I),I=1,30)                                   H6T 3500
      WRITE (NTH,5005) (YF(I),I=1,30)                                   H6T 3510
      WRITE (NTH,5005) (YT(I),I=1,30)                                   H6T 3520
      WRITE (NTH,5005) ATOMGW,HXS                                       H6T 3530
      INO = INO + 1                                                     H6T 3540
 1123 CONTINUE                                                          H6T 3550
      IF(INO .LT. INR) GOTO 22                                          H6T 3560
   23 READ (N9,5001) IDT                                                H6T 3570
      IF(IDT) 1125,24,24                                                H6T 3580
   24 WRITE (N6,502) IDT                                                H6T 3590
      GOTO 23                                                           H6T 3600
C                                                                       H6T 3610
C     READ IN SCATTERS                                                  H6T 3620
C                                                                       H6T 3630
 1125 CONTINUE                                                          H6T 3640
      KONABS = KONABS + 1                                               H6T 3650
   25 READ (N9,5001) IDT                                                H6T 3660
      READ (N9,5003) (DID(I),I=1,18)                                    H6T 3670
      READ (N9,5002) T,FSX,UB                                           H6T 3680
      READ (N9,5002) (S(I),I=1,NEVR)                                    H6T 3690
      READ (N9,5002) (XA(I),I=1,NEVR)                                   H6T 3700
      READ (N9,5002) (XS(I),I=1,NEVR)                                   H6T 3710
      READ (N9,5002) ((TIM(I,J),I=1,NEVR),J=1,NEVR)                     H6T 3720
      READ (N9,5002) (XTR(I),I=1,NEVR)                                  H6T 3730
      READ (N9,5002) ATOMGW                                             H6T 3740
      NFL = IZWEI                                                       H6T 3750
      IF(IDKER(2,1) .EQ. ITAUSN) GOTO 2000                              H6T 3760
      NFL = 1                                                           H6T 3770
      IF(KONID(KONZA,1) .EQ. ITAUSN) GOTO 2000                          H6T 3780
      NFL = 0                                                           H6T 3790
      DO 2001 I=KONABS,KONZA                                            H6T 3800
        IF(KONID(I,1) .NE. IDT) GOTO 2001                               H6T 3810
        DO 2002 K=1,NPHIA                                               H6T 3820
          IF(NPHI(K) .EQ. KONID(I,2)) NFL = K                           H6T 3830
 2002   CONTINUE                                                        H6T 3840
 2001 CONTINUE                                                          H6T 3850
      IF(NFL .EQ. 0) GOTO 129                                           H6T 3860
 2000 CONTINUE                                                          H6T 3870
      KIM = IDT                                                         H6T 3880
      IF(ITTTT) 111,111,131                                             H6T 3890
  131 CONTINUE                                                          H6T 3900
      WRITE (N6,1302) KIM,(DID(I),I=1,18),T,FSX                         H6T 3910
  111 CONTINUE                                                          H6T 3920
      IF(IDT-IEBE) 151,151,150                                          H6T 3930
  150 ITTTT = 0                                                         H6T 3940
  151 CONTINUE                                                          H6T 3950
      IF(ITTTT) 104,104,124                                             H6T 3960
  104 CONTINUE                                                          H6T 3970
      WRITE (N6,1301) KIM,(DID(I),I=1,18),T,FSX                         H6T 3980
      WRITE (N6,1160) NPHI(NFL)                                         H6T 3990
      WRITE (N6,505) (I,XA(I),XS(I),S(I),XTR(I),JJ(I),XA(I+48),XS(I+48),H6T 4000
     1 S(I+48),XTR(I+48),I=1,48)                                        H6T 4010
  124 CONTINUE                                                          H6T 4020
      TQR = T / 293.6                                                   H6T 4030
      DO 26 I=1,NEVR                                                    H6T 4040
        VSQR = V(I) * V(I)                                              H6T 4050
        IF(NFL .GT. 1) GOTO 2005                                        H6T 4060
        FLUSS(1,I) = VSQR * EXP(-VSQR/TQR) * V(I) * DELV(I)             H6T 4070
   26 CONTINUE                                                          H6T 4080
 2005 CONTINUE                                                          H6T 4090
      DO 2006 I=1,NEVR                                                  H6T 4100
        PHI(I) = FLUSS(NFL,I)                                           H6T 4110
 2006 CONTINUE                                                          H6T 4120
      DO 29 J=1,30                                                      H6T 4130
        I1 = NG(J)                                                      H6T 4140
        I2 = NG(J) + MG(J)                                              H6T 4150
        YP(J) = 0.0                                                     H6T 4160
        YA(J) = 0.0                                                     H6T 4170
        YS(J) = 0.0                                                     H6T 4180
        YT(J) = 0.0                                                     H6T 4190
        DO 28 I=I1,I2                                                   H6T 4200
          YP(J) = YP(J) + PHI(I)                                        H6T 4210
          YS(J) = YS(J) + XS(I) * PHI(I)                                H6T 4220
          YA(J) = YA(J) + XA(I) * PHI(I)                                H6T 4230
          YT(J) = YT(J) + XTR(I) * PHI(I)                               H6T 4240
   28   CONTINUE                                                        H6T 4250
        YS(J) = YS(J) / YP(J)                                           H6T 4260
        YA(J) = YA(J) / YP(J)                                           H6T 4270
        YT(J) = YT(J) / YP(J)                                           H6T 4280
   29 CONTINUE                                                          H6T 4290
      IF(ITTTT) 106,106,126                                             H6T 4300
  106 CONTINUE                                                          H6T 4310
      YDUM = 0.0                                                        H6T 4320
      WRITE (N6,506) (I,YA(I),YS(I),YDUM,YT(I),I=1,30)                  H6T 4330
      IF(ITUTEU .EQ. 0) GOTO 126                                        H6T 4340
      J3 = 0                                                            H6T 4350
      J1 = 1                                                            H6T 4360
      J2 = 10                                                           H6T 4370
   37 WRITE (N6,8) (N,N=J1,J2)                                          H6T 4380
      DO 36 I=1,NEVR                                                    H6T 4390
        WRITE (N6,507) I,(TIM(I,J),J=J1,J2)                             H6T 4400
   36 CONTINUE                                                          H6T 4410
      IF(J3-3) 43,44,43                                                 H6T 4420
   43 J1 = J1 + 10                                                      H6T 4430
      J2 = J2 + 10                                                      H6T 4440
      IF(J2-NEVR) 37,38,38                                              H6T 4450
   38 J2 = NEVR                                                         H6T 4460
      J3 = 3                                                            H6T 4470
      GOTO 37                                                           H6T 4480
  126 CONTINUE                                                          H6T 4490
   44 DO 30 I=1,NEVR                                                    H6T 4500
        DO 30 J=1,NEVR                                                  H6T 4510
          IF(TIM(I,J) .LT. ERR) TIM(I,J) = 0.                           H6T 4520
          TIM(I,J) = TIM(I,J) * V(J) * 0.05060                          H6T 4530
   30 CONTINUE                                                          H6T 4540
      DO 32 L=1,30                                                      H6T 4550
        I1 = NG(L)                                                      H6T 4560
        I2 = NG(L) + MG(L)                                              H6T 4570
        DO 32 K=1,30                                                    H6T 4580
          J1 = NG(K)                                                    H6T 4590
          J2 = NG(K) + MG(K)                                            H6T 4600
          P(L,K) = 0.0                                                  H6T 4610
          DO 31 J=J1,J2                                                 H6T 4620
            DO 31 I=I1,I2                                               H6T 4630
              P(L,K) = P(L,K) + TIM(J,I) * PHI(J) * DELV(I)             H6T 4640
   31     CONTINUE                                                      H6T 4650
          P(L,K) = P(L,K) * DY(K) * YV(K) / (YP(K)*DY(L))               H6T 4660
   32 CONTINUE                                                          H6T 4670
      DO 70 J=1,30                                                      H6T 4680
        TEM = 0.0                                                       H6T 4690
        DO 71 I=1,30                                                    H6T 4700
          TEM = TEM + P(I,J) * DY(I)                                    H6T 4710
   71   CONTINUE                                                        H6T 4720
        P(J,J) = YV(J) * YS(J) - TEM / DY(J) + P(J,J)                   H6T 4730
   70 CONTINUE                                                          H6T 4740
      WRITE (NTH,5001) IDT                                              H6T 4750
      WRITE (NTH,5003) (DID(I),I=1,18)                                  H6T 4760
      WRITE (NTH,5005) T,FSX,UB                                         H6T 4770
      WRITE (NTH,5005) (YA(I),I=1,30)                                   H6T 4780
      WRITE (NTH,5005) (YS(I),I=1,30)                                   H6T 4790
      WRITE (NTH,5005) ((P(I,J),J=1,30),I=1,30)                         H6T 4800
      WRITE (NTH,5005) (YT(I),I=1,30)                                   H6T 4810
      WRITE (NTH,5005) ATOMGW                                           H6T 4820
      IF(ITTTT) 109,109,129                                             H6T 4830
  109 CONTINUE                                                          H6T 4840
      J3 = 0                                                            H6T 4850
      K1 = 1                                                            H6T 4860
      K2 = 10                                                           H6T 4870
   40 WRITE (N6,8) (N,N=K1,K2)                                          H6T 4880
      DO 39 L=1,30                                                      H6T 4890
        WRITE (N6,507) L,(P(L,K),K=K1,K2)                               H6T 4900
   39 CONTINUE                                                          H6T 4910
      IF(J3-3) 42,45,42                                                 H6T 4920
   42 K1 = K1 + 10                                                      H6T 4930
      K2 = K2 + 10                                                      H6T 4940
      IF(K2-30) 40,41,41                                                H6T 4950
   41 K2 = 30                                                           H6T 4960
      J3 = 3                                                            H6T 4970
      GOTO 40                                                           H6T 4980
   45 CONTINUE                                                          H6T 4990
  129 CONTINUE                                                          H6T 5000
      READ (N9,5001) IDT                                                H6T 5010
      IF(IDT) 34,34,35                                                  H6T 5020
   35 BACKSPACE N9                                                      H6T 5030
      GOTO 25                                                           H6T 5040
   34 WRITE (NTH,5001) IDT                                              H6T 5050
      END FILE NTH                                                      H6T 5060
      REWIND NTH                                                        H6T 5070
      WRITE (N6,510) IDTP                                               H6T 5080
      JTPE7 = -1                                                        H6T 5090
      MAFIA = 1                                                         H6T 5100
      RETURN                                                            H6T 5110
      END                                                               H6T 5120