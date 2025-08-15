      SUBROUTINE CH2(MAFIA,IMAT,NDES,TEMZUT,NHOTD,KDES,MGBN)            CH2   10
C                                                                       CH2   20
C     GAM                                                               CH2   30
C                                                                       CH2   40
C     A CODE TO CALCULATE MULTIGROUP CONSTANTS                          CH2   50
C     FLUX,CURRENT,THE FIRST 3 MOMENTS AND THE AGE MAY ALSO BE OBTAINED CH2   60
C                                                                       CH2   70
CFZJ035                                                       14.09.04  CH2   80
CFZJ063                                                       26.07.11  CH2   90
      DIMENSION D(108),B(108),SIGU(1),EN(250),GAMN(250),GAMGAM(250),    CH2  100
     1 L(250),GAMT(99),FAC3(99),FAC1(99),SIGZ(99),ZONE(9),TERM(9),      CH2  110
     2 AT(108),LP(108),T(1),AR(1),FACT(1),AL1(69),AFSS(69),AMAR(69),    CH2  120
     3 ADUM(5100),BST(69),CEG(33),LGBN(35),CGM(33),CTL(33),CRAM(33),    CH2  130
     4 CD(33),CSAR(33),CX(33),CNSQ(33),APHI(69),AJ(69),DB0(528),DB1(528)CH2  140
     5 ,DBN(528),DBIN(528),SIG(2,1),COEF1(1,99),BETA(1,50),GK(1,99),    CH2  150
     6 ZI(1,99),Z(2,1),SAR(9,68),ACS(4,2346),PLCK(2,69),GDJ(2,70),      CH2  160
     7 FTM(528),TAU(69),THRM(69),SECM(69),FIRM(69),ANSQ(68),LOL(4),LA(4)CH2  170
     8 ,LD(4),PSFS(70),FSN(18),RB0(33),RB1(33),RBIN(33),RBN(33),TTR(33),CH2  180
     9 TPB(33),CUDA(68),G(9,33,14),F(4158),MGBN(68),SIRA(68),NSKD0(NXS),CH2  190
     X IMAT(KMAT),NDES(NXS),TEMZUT(NXS),NHOTD(KMAT),KDES(NXS),          CH2  200
     Y INHALT(34,2)                                                     CH2  210
C                                                                       CH2  220
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    CH2  230
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    CH2  240
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PICH2  250
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 CH2  260
C                                                                       CH2  270
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), CH2  280
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10CH2  290
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11CH2  300
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         CH2  310
C                                                                       CH2  320
      COMMON /BLOCKG/ K,D,B,AU,F,JGM,SIG,SIGU,EN,GAMN,GAMGAM,L,GAMT,FAC3CH2  330
     1 ,FAC1,SIGZ,COEF1,BETA,GK,ZI,Z,ZONE,TERM,AT,SAR,LP,T,AR,FACT,Al1, CH2  340
     2 AFSS,AMAR,ACS,CEG,LGBN,CD,ANSQ,LOL,LA,LD,PSFS,PLCK,FSN,GDJ,RB0,  CH2  350
     3 RB1,RBIN,RBN,TTR,TPB,NOAG,NOBG,ADELL,MICR,NOI,MTP,WAG,TKN,ADUM,  CH2  360
     4 BXCX                                                             CH2  370
C                                                                       CH2  380
CFZJ035                                                       14.09.04  CH2  390
CFZJ063                                                        26.07.11 CH2  400
      COMMON /GRENZE/ NURTHM,THERGR,IDGAM,IDZUT,IDTHER,NGAM,IDESIN,JJGM,CH2  410
     1 MSTU,MGHUS,NNOBG,NZT(10,9),IZUT(10,2,10),SIRA,NIRPI,NID,NDA30    CH2  420
C                                                                       CH2  430
CFZJ031                                                       28.05.04  CH2  440
CFZJ062                                                       04.05.11  CH2  450
      COMMON /ORIGEN/ LOB,NOR,VOR(100),ISPK,KFISS,N200C,NXSC,KGR,       CH2  460
     1 LOBN,IEZ,FSP(33),IDOP,JNEU(15),LMAT(15),N34,JPROV(15)            CH2  470
C                                                                       CH2  480
CFZJ035                                                       14.09.04  CH2  490
CFZJ063                                                       26.07.11  CH2  500
      COMMON /RI/ SM(10,9,2),IZUT1(10,9,10),IZUT2(10,9,10),SMC          CH2  510
C                                                                       CH2  520
      COMMON /VARDIM/ A(8000000)                                        CH2  530
C                                                                       CH2  540
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       CH2  550
C                                                                       CH2  560
CFZJ055                                                       25.09.07  CH2  570
C                                                                       CH2  580
      COMMON /BUC/ BU                                                   CH2  590
C                                                                       CH2  600
CFZJ059                                                       04.11.09  CH2  610
      CHARACTER*4 CUD1(18),BCDW(18),BU(6,200)                           CH2  620
C                                                                       CH2  630
      EQUIVALENCE(JTPE2,M5),(JTPE3,M6),(JTPE10,M9),(G(1,1,1),F(1)),     CH2  640
     1 (BST(1),F(1)),(F(70),APHI(1)),(F(140),AJ(1)),(FIRM(1),F(210)),   CH2  650
     2 (F(280),SECM(1)),(F(350),THRM(1)),(F(420),TAU(1)),(CGM(1),F(500))CH2  660
     3 ,(CTL(1),F(540)),(F(580),CRAM(1)),(CSAR(1),F(620)),(F(660),CX(1))CH2  670
     4 ,(CNSQ(1),F(700)),(FTM(1),F(800)),(DB0(1),F(1350)),              CH2  680
     5 (DB1(1),F(1900)),(DBIN(1),F(2450)),(DBN(1),F(3000))              CH2  690
C                                                                       CH2  700
   33 FORMAT (' FOR NUCLID',I6,' THE FOLLOWING RESONANCE CROSS SECTION SCH2  710
     1ETS ARE'/' USED FOR TEMPERATURE AND BURNUP DEPENDENT INTERPOLATIONCH2  720
     2:'/' DESIGN NO:     CROSS SECTION SETS:')                         CH2  730
   34 FORMAT (71X,I2,E24.8)                                             CH2  740
   35 FORMAT (I6,8X,10I8)                                               CH2  750
   38 FORMAT (/1H ,32H EPITHERMAL SHORT LIBRARY - GAM:/)                CH2  760
   39 FORMAT (1H ,38H (EXPLICITE PRINTOUT HAS BEEN DROPPED)/)           CH2  770
   40 FORMAT (70X,'INPUT SET NO. (NSET):',I3,' (KDES =',I3,')')         CH2  780
   41 FORMAT (//70X,'BROAD ENERGY GROUPS:'//70X,'GROUP     GAM-GROUPS'/)CH2  790
   44 FORMAT (71X,I2,I10,'  -  ',I2)                                    CH2  800
   45 FORMAT (//)                                                       CH2  810
   47 FORMAT (1H )                                                      CH2  820
   74 FORMAT (2E12.5,I6)                                                CH2  830
   75 FORMAT (12I6)                                                     CH2  840
   76 FORMAT (6E12.5)                                                   CH2  850
   77 FORMAT (18X,5I6)                                                  CH2  860
  397 FORMAT (1H0/6H GROUP,23H          DELTA-U      )                  CH2  870
  399 FORMAT (1I4,1E24.8)                                               CH2  880
  512 FORMAT (/' THE WRONG DATA TAPE HAS BEEN LOADED.'/' ID-NO. ON UNIT CH2  890
     1= ',I6,'  ID-NO. FROM INPUT = ',I6/' NUMBER OF NUCLIDES IN VSOP (KCH2  900
     2MAT) = ',I6,' LIBRARY (NITL) = ',I6/)                             CH2  910
  525 FORMAT (1H0/55H  REVISED BROAD GROUP BOUNDARIES BY ENERGY AND LETHCH2  920
     1ARGY/1H /101H      DESIRED ENERGY          ACTUAL ENERGY USED     CH2  930
     2    DESIRED LETHARGY        ACTUAL LETHARGY USED/)                CH2  940
  551 FORMAT (70X,'GROUP     SELFSHIELDING FACTOR')                     CH2  950
CFZJ059                                                       04.11.09  CH2  960
  555 FORMAT (1H ,21H TAPE I.D. NUMBER IS ,I3,10X,18A4)                 CH2  970
  581 FORMAT (1E20.8,3E26.8)                                            CH2  980
  668 FORMAT (1H1)                                                      CH2  990
  998 FORMAT (1H /47H THE NUCLIDE DATA HAS NOT BEEN PROPERLY ORDERED)   CH2 1000
 7000 FORMAT (8I10)                                                     CH2 1010
 7001 FORMAT (18A4/(1P,5E14.6))                                         CH2 1020
CFZJ059                                                       04.11.09  CH2 1030
 7002 FORMAT (18A4)                                                     CH2 1040
 7003 FORMAT (1P,5E14.6)                                                CH2 1050
 7004 FORMAT (I10,1P,4E14.6/E14.6)                                      CH2 1060
 7005 FORMAT (1P,(3E14.6,I10))                                          CH2 1070
 8803 FORMAT (1H0/1H0/1H0/45H0THE SOURCE SPECTRUM USED FOR THIS PROBLEM CH2 1080
     1IS)                                                               CH2 1090
 8912 FORMAT (52H1THE SOURCE SPECTRUM HAS NOT BEEN SPECIFIED PROPERLY)  CH2 1100
 8813 FORMAT (24H0A UNIT SOURCE IN GROUP ,I2)                           CH2 1110
 8814 FORMAT (1H0,18A4)                                                 CH2 1120
 9000 FORMAT (///' SOURCE DISTRIBUTION IN BROAD GROUPS:'/)              CH2 1130
 9001 FORMAT (' BROAD GROUP:',I3,'  FIRST GAM-GROUP:',I3,'  FRACTION:', CH2 1140
     1 F8.5)                                                            CH2 1150
10001 FORMAT (//' SET NO.',I4,' STARTS ON NDA10 WITH RECORD NO.',I6//)  CH2 1160
C                                                                       CH2 1170
C                                                                       CH2 1180
      ADEN = 0.                                                         CH2 1190
      LT0 = 0                                                           CH2 1200
      MST = 0                                                           CH2 1210
      NDA30 = 30                                                        CH2 1220
      NGAM = NDA30                                                      CH2 1230
      IPRIN(7) = IPRIN(7) + 1                                           CH2 1240
      NIRPI = IPRIN(7)                                                  CH2 1250
      IF(NIRPI .GT. 1) GOTO 99                                          CH2 1260
      KD0 = 0                                                           CH2 1270
      DO 700 I=1,NXS                                                    CH2 1280
        NSKD0(I) = 0                                                    CH2 1290
        NDES(I) = 1                                                     CH2 1300
        KDES(I) = 1                                                     CH2 1310
  700 CONTINUE                                                          CH2 1320
   95 CONTINUE                                                          CH2 1330
      KD0 = KD0 + 1                                                     CH2 1340
      JSATZ = 2 + KD0                                                   CH2 1350
      IF(JAD10(JSATZ) .EQ. 0) JAD10(JSATZ) = JSUM10                     CH2 1360
      NXT10 = JAD10(JSATZ)                                              CH2 1370
      WRITE (M6,10001) JSATZ,NXT10                                      CH2 1380
      IF(KD0 .GT. 1 .AND. IPRIN(1) .LE. 1) GOTO 10                      CH2 1390
      WRITE (M6,668)                                                    CH2 1400
   10 CONTINUE                                                          CH2 1410
      IF(IDESIN .GT. 0) GOTO 702                                        CH2 1420
C                                                                       CH2 1430
CARD G1                                                                 CH2 1440
C                                                                       CH2 1450
      READ (M5,77) IDESIN,MSTU,MGHUS,NSSS,IPRSEL                        CH2 1460
C                                                                       CH2 1470
      IDGAM = 5115                                                      CH2 1480
      IDZUT = 180                                                       CH2 1490
      JJGM = 3                                                          CH2 1500
CFZJ035                                                       14.09.04  CH2 1510
CFZJ059                                                       04.11.09  CH2 1520
      OPEN(M9,FILE='Libraries\gam_b7')                                  CH2 1530
      IF(IDESIN .LE. 0) IDESIN = 1                                      CH2 1540
      NNOBG = N26                                                       CH2 1550
C                                                                       CH2 1560
CARD G2                                                                 CH2 1570
C                                                                       CH2 1580
      READ (M5,76) (TEMZUT(I),I=1,NXS)                                  CH2 1590
C                                                                       CH2 1600
CARD G3                                                                 CH2 1610
C                                                                       CH2 1620
      IF(IDESIN .GT. 1) READ (M5,75) (NDES(I),I=1,NXS)                  CH2 1630
C                                                                       CH2 1640
      NAE25 = 1                                                         CH2 1650
      READ (NDA30,REC=NAE25) KENNNR,MAXSAE,LIBIN,LIBSAE,NGGR            CH2 1660
C                                                                       CH2 1670
CFZJ035                                                       14.09.04  CH2 1680
CFZJ063                                                       26.07.11  CH2 1690
      DO 701 I=1,IDESIN                                                 CH2 1700
        DO 701 J=1,9                                                    CH2 1710
C                                                                       CH2 1720
CARD G4                                                                 CH2 1730
C                                                                       CH2 1740
          READ (M5,74) SM(I,J,1),SM(I,J,2),NZ                           CH2 1750
C                                                                       CH2 1760
CARD(S) G5                                                              CH2 1770
C                                                                       CH2 1780
          READ (M5,75) (IZUT1(I,J,M),M=1,NZ)                            CH2 1790
          READ (M5,75) (IZUT2(I,J,M),M=1,NZ)                            CH2 1800
C                                                                       CH2 1810
          NZT(I,J) = NZ                                                 CH2 1820
          DO 80 NN=1,2                                                  CH2 1830
            DO 79 LL=1,NZ                                               CH2 1840
              IF(NN .EQ. 1) IERR = IZUT1(I,J,LL)                        CH2 1850
              IF(NN .EQ. 2) IERR = IZUT2(I,J,LL)                        CH2 1860
              NAE25 = 2                                                 CH2 1870
              DO 78 L3=1,LIBSAE                                         CH2 1880
                READ (NDA30,REC=NAE25) ((INHALT(L4,K),L4=1,LIBIN),K=1,2)CH2 1890
                NAE25 = NAE25 + 1                                       CH2 1900
                DO 78 LLL=1,LIBIN                                       CH2 1910
                  IF(INHALT(LLL,1) .EQ. IERR .AND. IERR .GT. 0) GOTO 79 CH2 1920
   78         CONTINUE                                                  CH2 1930
              WRITE(6,*)' RESONANCE CROSS SECTIONS MISSING FOR ID-NO ', CH2 1940
     1         IERR                                                     CH2 1950
              STOP                                                      CH2 1960
   79       CONTINUE                                                    CH2 1970
   80     CONTINUE                                                      CH2 1980
  701 CONTINUE                                                          CH2 1990
  702 CONTINUE                                                          CH2 2000
      REWIND M9                                                         CH2 2010
      READ (M9,7000) NTID,NITL,NFS                                      CH2 2020
      NOID = IDGAM                                                      CH2 2030
      JGM = JJGM                                                        CH2 2040
      NOBG = NNOBG                                                      CH2 2050
      NOI = KMAT                                                        CH2 2060
      IF(NTID-NOID) 511,510,511                                         CH2 2070
  510 IF(NOI-NITL) 509,509,511                                          CH2 2080
  511 WRITE (M6,512) NTID,NOID,NOI,NITL                                 CH2 2090
      GOTO 955                                                          CH2 2100
  999 WRITE (M6,998)                                                    CH2 2110
C                                                                       CH2 2120
  955 CALL EXIT                                                         CH2 2130
C                                                                       CH2 2140
  509 IF(MSTU-NFS) 8911,8911,8910                                       CH2 2150
 8910 WRITE (M6,8912)                                                   CH2 2160
      GOTO 955                                                          CH2 2170
 8911 ADELL = 0.25                                                      CH2 2180
C                                                                       CH2 2190
      CALL INTERP(1,20.0,0.05,TABL,KD0)                                 CH2 2200
C                                                                       CH2 2210
      NOAG = NOBG - 1                                                   CH2 2220
C                                                                       CH2 2230
CARD G6                                                                 CH2 2240
C                                                                       CH2 2250
      IF(KD0 .EQ. 1) READ (M5,76) (CEG(I),I=2,NOBG)                     CH2 2260
C                                                                       CH2 2270
      THERGR = CEG(NOBG)                                                CH2 2280
C                                                                       CH2 2290
C     FIND GROUP BOUNDARIES IN OUR STRUCTURE                            CH2 2300
C                                                                       CH2 2310
      IF(CEG(NOBG)-0.414) 2005,2001,2001                                CH2 2320
 2005 CEG(NOBG) = 0.414                                                 CH2 2330
 2001 CONTINUE                                                          CH2 2340
      IF(KD0 .GT. 1) GOTO 1                                             CH2 2350
      WRITE (M6,525)                                                    CH2 2360
      CEG(1) = 1.0E7                                                    CH2 2370
      PEG = 0.0                                                         CH2 2380
      DO 29 K=1,NOBG                                                    CH2 2390
        IF(K-1) 505,505,506                                             CH2 2400
  506   PEG = ALOG(1.0E7/CEG(K))                                        CH2 2410
  505   LGBN(K) = PEG / ADELL + 1.5                                     CH2 2420
        REG = LGBN(K)                                                   CH2 2430
        RELU = (REG-1.0) / 4.0                                          CH2 2440
        RELE = 1.0E7 * EXP(-RELU)                                       CH2 2450
        WRITE (M6,581) CEG(K),RELE,PEG,RELU                             CH2 2460
   29 CONTINUE                                                          CH2 2470
      MGN26 = LGBN(N26)                                                 CH2 2480
      WRITE (M6,397)                                                    CH2 2490
      DO 398 K=1,NOAG                                                   CH2 2500
        TDEU = LGBN(K+1) - LGBN(K)                                      CH2 2510
        TDEU = TDEU * .25                                               CH2 2520
        WRITE (M6,399) K,TDEU                                           CH2 2530
  398 CONTINUE                                                          CH2 2540
      WRITE (M6,8803)                                                   CH2 2550
    1 CONTINUE                                                          CH2 2560
      IF(MSTU) 8801,8801,6                                              CH2 2570
 8801 CONTINUE                                                          CH2 2580
      DO 8802 I=1,68                                                    CH2 2590
        AFSS(I) = 0.0                                                   CH2 2600
 8802 CONTINUE                                                          CH2 2610
      AFSS(MGHUS) = 1.0                                                 CH2 2620
      MST = 1                                                           CH2 2630
      IF(KD0 .GT. 1 .AND. IPRIN(1) .LE. 1) GOTO 6                       CH2 2640
      WRITE (M6,8813) MGHUS                                             CH2 2650
    6 CONTINUE                                                          CH2 2660
      DO 8809 K=1,NFS                                                   CH2 2670
        IF(MST .GT. 0) GOTO 8807                                        CH2 2680
        IF(MSTU-K) 8806,8808,8806                                       CH2 2690
 8808   READ (M9,7001) (FSN(I),I=1,18),(AFSS(I),I=1,68)                 CH2 2700
        IF(KD0 .EQ. 1) WRITE (M6,8814) (FSN(I),I=1,18)                  CH2 2710
C                                                                       CH2 2720
C     CALCULATE SOURCE DISTRIBUTION IN BROAD GROUPS                     CH2 2730
C                                                                       CH2 2740
 8807   CONTINUE                                                        CH2 2750
        DO 4 I=1,N26                                                    CH2 2760
          FSP(I) = 0.                                                   CH2 2770
    4   CONTINUE                                                        CH2 2780
        FSPT = 0.                                                       CH2 2790
        DO 3 I=1,NOAG                                                   CH2 2800
          IFGO = LGBN(I)                                                CH2 2810
          IFGU = LGBN(I+1) - 1                                          CH2 2820
          DO 2 IFG=IFGO,IFGU                                            CH2 2830
            FSP(I) = FSP(I) + AFSS(IFG)                                 CH2 2840
    2     CONTINUE                                                      CH2 2850
          FSPT = FSPT + FSP(I)                                          CH2 2860
    3   CONTINUE                                                        CH2 2870
        FSP(N26) = AMAX1((1.-FSPT),0.)                                  CH2 2880
        IF(KD0 .GT. 1 .AND. IPRIN(1) .LE. 1) GOTO 8809                  CH2 2890
        WRITE (M6,9000)                                                 CH2 2900
        WRITE (M6,9001) (I,LGBN(I),FSP(I),I=1,N26)                      CH2 2910
        IF(MST .GT. 0) GOTO 8806                                        CH2 2920
        GOTO 8809                                                       CH2 2930
 8806   READ (M9,7001) (CUD1(I),I=1,18), (CUDA(I),I=1,68)               CH2 2940
        IF(MST .GT. 0) MST = 0                                          CH2 2950
 8809 CONTINUE                                                          CH2 2960
      DO 1010 I=1,68                                                    CH2 2970
        PLCK(1,I) = 0.0                                                 CH2 2980
        PLCK(2,I) = 0.0                                                 CH2 2990
        GDJ(1,I) = 0.0                                                  CH2 3000
        GDJ(2,I) = 0.0                                                  CH2 3010
        AMAR(I) = 0.0                                                   CH2 3020
        AL1(I) = 0.0                                                    CH2 3030
CFZJ063                                                       26.07.11  CH2 3040
        SAR(1,I) = 0.0                                                  CH2 3050
        SAR(2,I) = 0.0                                                  CH2 3060
        SAR(3,I) = 0.0                                                  CH2 3070
        SAR(4,I) = 0.0                                                  CH2 3080
        SAR(5,I) = 0.0                                                  CH2 3090
        SAR(6,I) = 0.0                                                  CH2 3100
        SAR(7,I) = 0.0                                                  CH2 3110
        SAR(8,I) = 0.0                                                  CH2 3120
        SAR(9,I) = 0.0                                                  CH2 3130
        ANSQ(I) = 0.0                                                   CH2 3140
 1010 CONTINUE                                                          CH2 3150
      PLCK(1,69) = 0.                                                   CH2 3160
      PLCK(2,69) = 0.                                                   CH2 3170
      DO 1011 I=1,2346                                                  CH2 3180
        ACS(1,I) = 0.0                                                  CH2 3190
        ACS(2,I) = 0.0                                                  CH2 3200
        ACS(3,I) = 0.0                                                  CH2 3210
        ACS(4,I) = 0.0                                                  CH2 3220
 1011 CONTINUE                                                          CH2 3230
      IF(KD0 .GT. 1 .AND. IPRIN(1) .LE. 1) GOTO 7                       CH2 3240
      WRITE (M6,668)                                                    CH2 3250
    7 CONTINUE                                                          CH2 3260
C                                                                       CH2 3270
C     READ-IN-AND-CALCULATE MACROSCOPIC CROSS-SECTIONS                  CH2 3280
C     THIS PRODUCES 4 TRIANGULAR ARRAYS AND 4 LISTS                     CH2 3290
C     NID = IDENTIFICATION NUMBER ON TAPE                               CH2 3300
C     LTOT = TOTAL LENGTH OF LIST FOR THIS NUCLIDE                      CH2 3310
C     IWF = ISOTOPE WHICH FISSIONS                                      CH2 3320
C     IWR = ISOTOPE WITH RESONANCES                                     CH2 3330
C     LOL(I) = LENGTH OF ACS(I) TABLE                                   CH2 3340
C     LA(I) = WIDTH ACROSS MATRIX                                       CH2 3350
C     LD(I) = LENGTH DOWN MATRIX                                        CH2 3360
C                                                                       CH2 3370
      WRITE (NDA10,REC=NXT10) NOBG,(CEG(I),I=1,NOBG),(LGBN(I),I=1,NOBG),CH2 3380
     1 (AFSS(I),I=1,68)                                                 CH2 3390
      NXT10 = NXT10 + 1                                                 CH2 3400
      IJIS = 0                                                          CH2 3410
      KIS = KMAT + 1                                                    CH2 3420
      NP1 = NENDP + 69                                                  CH2 3430
      NP2 = NP1 + NXS                                                   CH2 3440
      NP3 = NP2 + 15 * 68                                               CH2 3450
      NP4 = NP3 + KMAT                                                  CH2 3460
      NP5 = NP4 + KMAT                                                  CH2 3470
      NP6 = NP5 + KMAT                                                  CH2 3480
      NP7 = NP6 + KMAT * 6                                              CH2 3490
      NP8 = NP7 + 6                                                     CH2 3500
      MOB = 0                                                           CH2 3510
C                                                                       CH2 3520
      IF(NSSS .NE. 0) CALL SSFEPI(KDES,KD0,NSSS,MOB,MGBN,A(NP1),A(NP2), CH2 3530
     1 A(NP3),A(NP4),A(NP5),A(NP6),A(NP7),A(NP8),LT0,MOBG,NSKD0,MGN26,  CH2 3540
     2 LZ0,SSON,LOBG,1)                                                 CH2 3550
C                                                                       CH2 3560
      WRITE (M6,38)                                                     CH2 3570
      IF(NSSS .EQ. 0) GOTO 46                                           CH2 3580
      WRITE (M6,40) NSKD0(KD0),KD0                                      CH2 3590
      IF(KD0 .EQ. LT0) WRITE (M6,39)                                    CH2 3600
      IF(KD0 .EQ. LT0) GOTO 46                                          CH2 3610
      WRITE (M6,41)                                                     CH2 3620
      DO 43 J=1,MOBG                                                    CH2 3630
        IF(MOB .NE. 0) GOTO 42                                          CH2 3640
        L1 = LGBN(J)                                                    CH2 3650
        L2 = LGBN(J+1) - 1                                              CH2 3660
        GOTO 43                                                         CH2 3670
   42   CONTINUE                                                        CH2 3680
        L1 = MGBN(J)                                                    CH2 3690
        L2 = MGBN(J+1) - 1                                              CH2 3700
        WRITE (M6,44) J,L1,L2                                           CH2 3710
   43 CONTINUE                                                          CH2 3720
      WRITE (M6,45)                                                     CH2 3730
      IF(IPRSEL .GT. 0) WRITE (M6,551)                                  CH2 3740
   46 CONTINUE                                                          CH2 3750
      IF(IDOP .GT. 0) REWIND N34                                        CH2 3760
      JJ = 0                                                            CH2 3770
      DO 201 LZ=1,NOI                                                   CH2 3780
        IF(IJIS .GE. NITL) GOTO 3114                                    CH2 3790
        IF(KIS .LE. KMAT) GOTO 151                                      CH2 3800
CFZJ059                                                       04.11.09  CH2 3810
 3113   READ (M9,7002) (BCDW(I),I=1,18)                                 CH2 3820
        READ (M9,7000) NID,LTOT,IWA,IWF,IWR,(LOL(I),LA(I),LD(I),I=1,4)  CH2 3830
        READ (M9,7003) (ADUM(I),I=1,LTOT)                               CH2 3840
        NID0 = NID                                                      CH2 3850
        GOTO 3115                                                       CH2 3860
 3114   CONTINUE                                                        CH2 3870
        L0 = 0                                                          CH2 3880
   59   CONTINUE                                                        CH2 3890
        DO 60 N=1,IDOP                                                  CH2 3900
          IF(JPROV(N) .EQ. NID0) L0 = N                                 CH2 3910
   60   CONTINUE                                                        CH2 3920
        NID0 = NID0 + 1                                                 CH2 3930
        IF(L0 .EQ. 0) GOTO 59                                           CH2 3940
        REWIND N34                                                      CH2 3950
        DO 61 N=1,L0                                                    CH2 3960
CFZJ059                                                       04.11.09  CH2 3970
          READ (N34,7002) (BCDW(I),I=1,18)                              CH2 3980
          READ (N34,7000)NID,LTOT,IWA,IWF,IWR,(LOL(I),LA(I),LD(I),I=1,4)CH2 3990
          READ (N34,7003) (ADUM(I),I=1,LTOT)                            CH2 4000
   61   CONTINUE                                                        CH2 4010
 3115   CONTINUE                                                        CH2 4020
        IJIS = IJIS + 1                                                 CH2 4030
        KIS = 1                                                         CH2 4040
        IF(IWR-1) 151,153,153                                           CH2 4050
C                                                                       CH2 4060
C     READ NO. OF RESOLVED RES (LESS THAN 100) AND SIGMA-M AND SIGMA-P  CH2 4070
C                                                                       CH2 4080
  153   READ (M9,7004) NRGYS,SMAS,EC,DLEV,GAMNO,GAMGA                   CH2 4090
C                                                                       CH2 4100
C     READ RESONANCE  DATA                                              CH2 4110
C                                                                       CH2 4120
        READ(M9,7005)(EN(I),GAMN(I),GAMGAM(I),L(I),I=1,NRGYS)           CH2 4130
C                                                                       CH2 4140
C     CHECK TO SEE IF THIS NUCLIDE IS OF INTEREST                       CH2 4150
C                                                                       CH2 4160
  151   KIM = KIS                                                       CH2 4170
        DO 451 II=KIM,KMAT                                              CH2 4180
          I = II                                                        CH2 4190
          MANU = IMAT(I)                                                CH2 4200
          KIS = II + 1                                                  CH2 4210
          IF(NID-MANU) 451,25,451                                       CH2 4220
  451   CONTINUE                                                        CH2 4230
        IF(IJIS-NITL) 3113,1000,1000                                    CH2 4240
 1000   IF(IDOP .EQ. 0) GOTO 999                                        CH2 4250
        GOTO 3114                                                       CH2 4260
C                                                                       CH2 4270
C     THIS NUCLIDE IS OF INTEREST  -  READ IN CROSS-SECTION DATA        CH2 4280
C     DOES THIS ISOTOPE HAVE RESONANCES - IWR=0(NO) ,IWR=1(YES)         CH2 4290
C                                                                       CH2 4300
   25   CONTINUE                                                        CH2 4310
        IF(IDOP .EQ. 0) GOTO 28                                         CH2 4320
        DO 26 J=1,IDOP                                                  CH2 4330
          IF(LMAT(J) .NE. NID) GOTO 26                                  CH2 4340
CFZJ059                                                       04.11.09  CH2 4350
          WRITE (N34,7002) (BCDW(IN),IN=1,18)                           CH2 4360
          IWR = 0                                                       CH2 4370
          WRITE (N34,7000) JNEU(J),LTOT,IWA,IWF,IWR,(LOL(IN),LA(IN),    CH2 4380
     1     LD(IN),IN =1,4)                                              CH2 4390
          WRITE (N34,7003) (ADUM(IN),IN=1,LTOT)                         CH2 4400
          JJ = JJ + 1                                                   CH2 4410
          JPROV(JJ) = JNEU(J)                                           CH2 4420
   26   CONTINUE                                                        CH2 4430
   28   CONTINUE                                                        CH2 4440
        NHOTD(I) = LZ                                                   CH2 4450
CFZJ059                                                       04.11.09  CH2 4460
        BU(1,I) = BCDW(1)                                               CH2 4470
        BU(2,I) = BCDW(2)                                               CH2 4480
        BU(3,I) = BCDW(3)                                               CH2 4490
        BU(4,I) = BCDW(4)                                               CH2 4500
        BU(5,I) = BCDW(5)                                               CH2 4510
        BU(6,I) = BCDW(6)                                               CH2 4520
        SSON  = 0.                                                      CH2 4530
        LZ0 = LZ                                                        CH2 4540
C                                                                       CH2 4550
        IF(NSSS .GT. 0) CALL SSFEPI(KDES,KD0,NSSS,MOB,MGBN,A(NP1),A(NP2)CH2 4560
     1   ,A(NP3),A(NP4),A(NP5),A(NP6),A(NP7),A(NP8),LT0,MOBG,NSKD0,MGN26CH2 4570
     2   ,LZ0,SSON,LOBG,2)                                              CH2 4580
C                                                                       CH2 4590
        LOAG = LOBG - 1                                                 CH2 4600
CFZJ059                                                       04.11.09  CH2 4610
        IF(KD0 .NE. LT0) WRITE (M6,555) NID,(BCDW(I),I=1,18)            CH2 4620
        IF(SSON) 519,519,1001                                           CH2 4630
 1001   CONTINUE                                                        CH2 4640
        IF(KD0 .EQ. LT0 .OR. IPRSEL .EQ. 0) GOTO 8                      CH2 4650
        WRITE (M6,34) (I,PSFS(I),I=1,LOAG)                              CH2 4660
    8   CONTINUE                                                        CH2 4670
C                                                                       CH2 4680
C     SET-UP SELF-SHEILDING FACTORS FOR FINE GROUPS                     CH2 4690
C                                                                       CH2 4700
        M = 1                                                           CH2 4710
        NOTG = LOBG + 1                                                 CH2 4720
        DO 515 K=1,LOBG                                                 CH2 4730
          JK = NOTG - K                                                 CH2 4740
          DO 516 I=M,69                                                 CH2 4750
            JI = 70 - I                                                 CH2 4760
            IJ = I                                                      CH2 4770
            LGN = LGBN(JK)                                              CH2 4780
            IF(MOB .NE. 0) LGN = MGBN(JK)                               CH2 4790
            IF(JI-LGN) 513,514,514                                      CH2 4800
  514       PSFS(JI) = PSFS(JK)                                         CH2 4810
  516     CONTINUE                                                      CH2 4820
  513     M = IJ                                                        CH2 4830
  515   CONTINUE                                                        CH2 4840
        GOTO 518                                                        CH2 4850
  519   CONTINUE                                                        CH2 4860
        DO 520 I=1,68                                                   CH2 4870
          PSFS(I) = 1.0                                                 CH2 4880
  520   CONTINUE                                                        CH2 4890
  518   CONTINUE                                                        CH2 4900
        IF(KD0 .EQ. LT0) GOTO 157                                       CH2 4910
C                                                                       CH2 4920
C      REASONANCE INTEGRAL CALCULATION                                  CH2 4930
C                                                                       CH2 4940
CFZJ035                                                       14.09.04  CH2 4950
CFZJ059                                                       04.11.09  CH2 4960
CFZJ063                                                       26.07.11  CH2 4970
        IF(NID .NE. 6 .AND. NID .NE. 12 .AND. NID .NE. 15 .AND. NID .NE.CH2 4980
     1   17 .AND. NID .NE. 133 .AND. NID .NE. 178 .AND. NID .NE. 181    CH2 4990
     2  .AND. NID .NE. 182 .AND. NID .NE. 184) GOTO 157                 CH2 5000
        WRITE (M6,33) NID                                               CH2 5010
        J = 1                                                           CH2 5020
        IF(NID .EQ. 12) J = 2                                           CH2 5030
CFZJ035                                                       14.09.04  CH2 5040
CFZJ063                                                       26.07.11  CH2 5050
        IF(NID .EQ. 15) J = 3                                           CH2 5060
        IF(NID .EQ. 17) J = 4                                           CH2 5070
        IF(NID .EQ. 133) J = 5                                          CH2 5080
        IF(NID .EQ. 178) J = 6                                          CH2 5090
        IF(NID .EQ. 181) J = 7                                          CH2 5100
        IF(NID .EQ. 182) J = 8                                          CH2 5110
        IF(NID .EQ. 184) J = 9                                          CH2 5120
        DO 156 I=1,IDESIN                                               CH2 5130
          NZ = NZT(I,J)                                                 CH2 5140
          WRITE (M6,35) I,(IZUT1(I,J,M),M=1,NZ)                         CH2 5150
          WRITE (M6,35) I,(IZUT2(I,J,M),M=1,NZ)                         CH2 5160
          IF(I .EQ. IDESIN) WRITE (M6,47)                               CH2 5170
  156   CONTINUE                                                        CH2 5180
  157   CONTINUE                                                        CH2 5190
CFZJ059                                                       04.11.09  CH2 5200
        WRITE (NDA10,REC=NXT10) ADEN,SSON,(BCDW(I),I=1,18),(PSFS(I),I=1,CH2 5210
     1   68),NID,LTOT,IWA,IWF,IWR,(LOL(I),LA(I),LD(I),I=1,4)            CH2 5220
        NXT10 = NXT10 + 1                                               CH2 5230
C                                                                       CH2 5240
        CALL WRDA(IWRITE,NDA10,NXT10,L10,ADUM,LTOT)                     CH2 5250
C                                                                       CH2 5260
  201 CONTINUE                                                          CH2 5270
      IF(JSUM10 .LT. NXT10) JSUM10 = NXT10                              CH2 5280
      IF(KD0 .LT. NSSS) GOTO 95                                         CH2 5290
   99 CONTINUE                                                          CH2 5300
      MAFIA = 3                                                         CH2 5310
      RETURN                                                            CH2 5320
      END                                                               CH2 5330
      SUBROUTINE SSFEPI(KDES,KD0,NSSS,MOB,MGBN,NSET,SF,IDG,JT,LSC,ANT,  SSF   10
     1 ANTE0,ANTE,LT0,MOBG,NSKD0,MGN26,LZ0,SSON,LOBG,M)                 SSF   20
C                                                                       SSF   30
C     DIFFERENT SETS OF EPITHERMAL NEUTRON FLUX- AND/OR CROSS SECTION-  SSF   40
C     SELFSHIELDING FACTORS FOR DIFFERENT CELL ZONES IN BROAD ENERGY    SSF   50
C     GROUP STRUCTURE                                                   SSF   60
C                                                                       SSF   70
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    SSF   80
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    SSF   90
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PISSF  100
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 SSF  110
C                                                                       SSF  120
CFZJ035                                                       14.09.04  SSF  130
CFZJ063                                                       26.07.11  SSF  140
      COMMON /BLOCKG/ ADU(16746),PSFS                                   SSF  150
C                                                                       SSF  160
CFZJ035                                                       14.09.04  SSF  170
CFZJ063                                                       26.07.11  SSF  180
      COMMON /GRENZE/ NURTHM,THERGR,IDGAM,IDZUT,IDTHER,NGAM,IDESIN,JJGM,SSF  190
     1 MSTU,MGHUS,NNOBG,NZT(10,9),IZUT(10,2,10),SIRA,NIRPI,NID,NDA30    SSF  200
C                                                                       SSF  210
      COMMON /SFFLUX/ SFF                                               SSF  220
C                                                                       SSF  230
CFZJ055                                                       25.09.07  SSF  240
C                                                                       SSF  250
      EQUIVALENCE(JTPE2,M5),(JTPE3,M6)                                  SSF  260
C                                                                       SSF  270
      DIMENSION KDES(NXS),NSET(NXS),MGBN(68),SF(15,68),IDG(KMAT),       SSF  280
     1 JT(KMAT),LSC(KMAT),ANT(KMAT,6),ANTE0(6),ANTE(6),PSFS(70),SIRA(68)SSF  290
     2 ,NSKD0(NXS)                                                      SSF  300
C                                                                       SSF  310
  100 FORMAT (12I6)                                                     SSF  320
  101 FORMAT (6E12.5)                                                   SSF  330
  102 FORMAT (I6,2I2,6E10.4)                                            SSF  340
  200 FORMAT (/' SPECTRUM ZONE I     :',36I3)                           SSF  350
  201 FORMAT ( ' INPUT NSET(I)       :',36I3)                           SSF  360
  202 FORMAT ( ' INTERNAL SET KDES(I):',36I3/)                          SSF  370
C                                                                       SSF  380
C                                                                       SSF  390
C     SET OF SELFSHIELDING FACTORS IN SPECTRUM ZONE                     SSF  400
C                                                                       SSF  410
      GOTO(30,40),M                                                     SSF  420
   30 CONTINUE                                                          SSF  430
      IF(KD0 .GT. 1) GOTO 7                                             SSF  440
      IF(NSSS .LT. 0) GOTO 1                                            SSF  450
C                                                                       SSF  460
CARD G7                                                                 SSF  470
C                                                                       SSF  480
      READ (M5,100) (NSET(I),I=1,NXS)                                   SSF  490
C                                                                       SSF  500
      GOTO 3                                                            SSF  510
    1 CONTINUE                                                          SSF  520
      NSSS = 1                                                          SSF  530
      DO 2 I=1,NXS                                                      SSF  540
        NSET(I) = 1                                                     SSF  550
    2 CONTINUE                                                          SSF  560
    3 CONTINUE                                                          SSF  570
      NSKD0(1) = NSET(1)                                                SSF  580
C                                                                       SSF  590
C     INTERNAL SET IDENTIFICATION *KDES*                                SSF  600
C                                                                       SSF  610
      LS = 0                                                            SSF  620
      LT = 1                                                            SSF  630
      DO 5 I=1,NXS                                                      SSF  640
        IF(NSET(I) .EQ. 0) LS = LS + 1                                  SSF  650
        IF(I .EQ. 1) GOTO 18                                            SSF  660
        J0 = 0                                                          SSF  670
        IM1 = I - 1                                                     SSF  680
        DO 4 J=1,IM1                                                    SSF  690
          IF(NSET(J) .EQ. NSET(I)) J0 = J                               SSF  700
          IF(J0 .EQ. 0) GOTO 4                                          SSF  710
          KDES(I) = KDES(J0)                                            SSF  720
          GOTO 5                                                        SSF  730
    4   CONTINUE                                                        SSF  740
        LT = LT + 1                                                     SSF  750
        KDES(I) = LT                                                    SSF  760
        NSKD0(LT) = NSET(I)                                             SSF  770
   18   CONTINUE                                                        SSF  780
        IF(LS .EQ. 1 .AND. LT0 .EQ. 0) LT0 = LT                         SSF  790
    5 CONTINUE                                                          SSF  800
      IF(LS .GT. 0) NSSS = NSSS + 1                                     SSF  810
      NXO = NXS                                                         SSF  820
      IF(NXS .GT. 36) NXO = 36                                          SSF  830
      WRITE (M6,200) (I,I=1,NXO)                                        SSF  840
      WRITE (M6,201) (NSET(I),I=1,NXO)                                  SSF  850
      WRITE (M6,202) (KDES(I),I=1,NXO)                                  SSF  860
      IF(NXO .EQ. NXS) GOTO 6                                           SSF  870
      WRITE (M6,200) (I,I=37,NXS)                                       SSF  880
      WRITE (M6,201) (NSET(I),I=37,NXS)                                 SSF  890
      WRITE (M6,202) (KDES(I),I=37,NXS)                                 SSF  900
    6 CONTINUE                                                          SSF  910
    7 CONTINUE                                                          SSF  920
C                                                                       SSF  930
C     NUMBER OF ENERGY GROUPS, SUBSETS OF CROSS SECTION-SELFSHIELDING   SSF  940
C     FACTORS AND CELL ZONES FOR NEUTRON FLUX-SELFSHIELDING FACTORS     SSF  950
C                                                                       SSF  960
      IF(KD0 .EQ. LT0) GOTO 17                                          SSF  970
C                                                                       SSF  980
CARD G8                                                                 SSF  990
C                                                                       SSF 1000
      READ (M5,100) MOB,LSUB,NK                                         SSF 1010
C                                                                       SSF 1020
C     ENERGY GROUP STRUCTURE                                            SSF 1030
C                                                                       SSF 1040
      IF(MOB .LT. 0) GOTO 11                                            SSF 1050
      IF(MOB .GT. 0) GOTO 8                                             SSF 1060
      MOBG = N26 - 1                                                    SSF 1070
      GOTO 11                                                           SSF 1080
    8 CONTINUE                                                          SSF 1090
      MOBG = MOB                                                        SSF 1100
      IF(MOB .LT. 67) GOTO 10                                           SSF 1110
      DO 9 J=1,68                                                       SSF 1120
        MGBN(J) = J                                                     SSF 1130
    9 CONTINUE                                                          SSF 1140
      GOTO 11                                                           SSF 1150
   10 CONTINUE                                                          SSF 1160
C                                                                       SSF 1170
CARD(S) G9                                                              SSF 1180
C                                                                       SSF 1190
      READ (M5,100) (MGBN(J),J=1,MOBG)                                  SSF 1200
C                                                                       SSF 1210
      MGBN(MOBG+1) = MGN26                                              SSF 1220
   11 CONTINUE                                                          SSF 1230
C                                                                       SSF 1240
C     SF FOR EACH SET OF SELFSHIELDING FACTORS                          SSF 1250
C     CROSS SECTION-SELFSHIELDING FACTORS FOR EACH SUBSET (LSUB)        SSF 1260
C                                                                       SSF 1270
      IF(LSUB .LE. 0) GOTO 13                                           SSF 1280
      L1 = 1 + NK                                                       SSF 1290
      L2 = L1 + LSUB - 1                                                SSF 1300
C                                                                       SSF 1310
CARD(S) G10                                                             SSF 1320
C                                                                       SSF 1330
      DO 12 J=1,MOBG                                                    SSF 1340
C                                                                       SSF 1350
        READ (M5,101) (SF(L,J),L=L1,L2)                                 SSF 1360
C                                                                       SSF 1370
   12 CONTINUE                                                          SSF 1380
   13 CONTINUE                                                          SSF 1390
C                                                                       SSF 1400
C     NEUTRON FLUX-SELFSHIELDING FACTORS FOR EACH CELL ZONE (NK)        SSF 1410
C                                                                       SSF 1420
      MK = 1                                                            SSF 1430
      SFF = 1.                                                          SSF 1440
      IF(NK .LE. 0) GOTO 15                                             SSF 1450
      MK = NK                                                           SSF 1460
C                                                                       SSF 1470
CARD(S) G11                                                             SSF 1480
C                                                                       SSF 1490
      DO 14 J=1,MOBG                                                    SSF 1500
C                                                                       SSF 1510
        READ (M5,101) (SF(K,J),K=1,NK)                                  SSF 1520
C                                                                       SSF 1530
   14 CONTINUE                                                          SSF 1540
      SFF = SF(1,1)                                                     SSF 1550
   15 CONTINUE                                                          SSF 1560
C                                                                       SSF 1570
C     NUCLIDES AND FRACTIONS ASSIGNED TO THE CELL ZONES (NK)            SSF 1580
C                                                                       SSF 1590
      IS = 0                                                            SSF 1600
   16 CONTINUE                                                          SSF 1610
      IS = IS + 1                                                       SSF 1620
C                                                                       SSF 1630
CARD(S) G12                                                             SSF 1640
C                                                                       SSF 1650
      READ (M5,102) IDG(IS),JT(IS),LSC(IS),(ANT(IS,K),K=1,MK)           SSF 1660
C                                                                       SSF 1670
      IF(IDG(IS) .GT. 0) GOTO 16                                        SSF 1680
      ISM = IS                                                          SSF 1690
      IDG(IS) = IABS(IDG(IS))                                           SSF 1700
   17 CONTINUE                                                          SSF 1710
      RETURN                                                            SSF 1720
   40 CONTINUE                                                          SSF 1730
      IF(KD0 .EQ. LT0) GOTO 99                                          SSF 1740
      IF(LZ0 .GT. 1) GOTO 21                                            SSF 1750
      DO 20 K=1,MK                                                      SSF 1760
        ANTE0(K) = 0.                                                   SSF 1770
   20 CONTINUE                                                          SSF 1780
      LSC0 = 0                                                          SSF 1790
      IS = 1                                                            SSF 1800
      IT = 0                                                            SSF 1810
   21 CONTINUE                                                          SSF 1820
      SSON = 1.                                                         SSF 1830
      IF(IDG(IS) .GT. NID) GOTO 25                                      SSF 1840
      IF(IS .EQ. IT) GOTO 25                                            SSF 1850
      IT = IS                                                           SSF 1860
      IF(IS .LT. ISM) IS = IS + 1                                       SSF 1870
      IF(JT(IT) .GT. 0) GOTO 23                                         SSF 1880
      DO 22 K=1,MK                                                      SSF 1890
        ANTE0(K) = ANT(IT,K)                                            SSF 1900
   22 CONTINUE                                                          SSF 1910
      LSC0 = LSC(IT)                                                    SSF 1920
      GOTO 25                                                           SSF 1930
   23 CONTINUE                                                          SSF 1940
      DO 24 K=1,MK                                                      SSF 1950
        ANTE(K) = ANT(IT,K)                                             SSF 1960
   24 CONTINUE                                                          SSF 1970
      LSCE = LSC(IT)                                                    SSF 1980
      GOTO 27                                                           SSF 1990
   25 CONTINUE                                                          SSF 2000
      DO 26 K=1,MK                                                      SSF 2010
        ANTE(K) = ANTE0(K)                                              SSF 2020
   26 CONTINUE                                                          SSF 2030
      LSCE = LSC0                                                       SSF 2040
   27 CONTINUE                                                          SSF 2050
      DO 29 J=1,MOBG                                                    SSF 2060
        PSFS(J) = 0.                                                    SSF 2070
        DO 28 K=1,MK                                                    SSF 2080
          IF(ANTE(K) .LE. 0.) GOTO 28                                   SSF 2090
          PSFS(J) = PSFS(J) + SF(K,J) * ANTE(K)                         SSF 2100
   28   CONTINUE                                                        SSF 2110
        IF(PSFS(J) .LE. 0.) PSFS(J) = 1.                                SSF 2120
        IF(LSCE .GT. 0) PSFS(J) = PSFS(J) * SF(NK+LSCE,J)               SSF 2130
   29 CONTINUE                                                          SSF 2140
      LOBG = MOBG + 1                                                   SSF 2150
      PSFS(LOBG) = PSFS(MOBG)                                           SSF 2160
   99 CONTINUE                                                          SSF 2170
      RETURN                                                            SSF 2180
      END                                                               SSF 2190
      SUBROUTINE INTERP(IZ,EK,XI,TABL,KD0)                              NTE   10
C                                                                       NTE   20
C     CHANGES FOR INTERP  TO GET ANY F-TABLE IN DESIRED                 NTE   30
C                                                                       NTE   40
CFZJ035                                                       14.09.04  NTE   50
CFZJ063                                                       26.07.11  NTE   60
      DIMENSION D(108),B(108),SIGU(1),EN(250),GAMN(250),GAMGAM(250),    NTE   70
     1 P(250),GAMT(99),FAC3(99),FAC1(99),SIGZ(99),ZONE(9),TERM(9),      NTE   80
     2 AT(108),LP(108),T(1),AR(1),FACT(1),AL1(69),AFSS(69),AMAR(69),    NTE   90
     3 ADUM(5100),BST(69),CEG(33),LGBN(35),CGM(33),CTL(33),CRAM(33),    NTE  100
     4 CD(33),CSAR(33),CX(33),CNSQ(33),APHI(69),AJ(69),DB0(528),DB1(528)NTE  110
     5 ,DBN(528),DBIN(528),SIG(2,1),COEF1(1,99),BETA(1,50),GK(1,99),    NTE  120
     6 ZI(1,99),Z(2,1),SAR(9,68),ACS(4,2346),PLCK(2,69),GDJ(2,70),      NTE  130
     7 FTM(528),TAU(69),THRM(69),SECM(69),FIRM(69),ANSQ(68),LOL(4),LA(4)NTE  140
     8 ,LD(4),PSFS(70),FSN(18),RB0(33),RB1(33),RBIN(33),RBN(33),TTR(33),NTE  150
     9 TPB(33),Y(22),A(3),EKT(44),YJ(22),YL(13),DENOM(22,22),FJ(44,23), NTE  160
     X DENOML(13,13),DENOMJ(22,22),C(9,33,14),F(4158),CUDA(86)          NTE  170
C                                                                       NTE  180
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    NTE  190
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    NTE  200
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PINTE  210
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D             NTE  220
C                                                                       NTE  230
      COMMON /BLOCKG/ K,D,B,AU,F,JGM,SIG,SIGU,EN,GAMN,GAMGAM,P,GAMT,FAC3NTE  240
     1 ,FAC1,SIGZ,COEF1,BETA,GK,ZI,Z,ZONE,TERM,AT,SAR,LP,T,AR,FACT,Al1, NTE  250
     2 AFSS,AMAR,ACS,CEG,LGBN,CD,ANSQ,LOL,LA,LD,PSFS,PLCK,FSN,GDJ,RB0,  NTE  260
     3 RB1,RBIN,RBN,TTR,TPB,NOAG,NOBG,ADELL,MICR,NOI,MTP,WAG,TKN,ADUM,  NTE  270
     4 BXCX                                                             NTE  280
C                                                                       NTE  290
CFZJ055                                                       25.09.07  NTE  300
C                                                                       NTE  310
      EQUIVALENCE(C(1,1,1),F(1)),(JTPE3,M6),(JTPE10,M9),(BST(1),F(1)),  NTE  320
     1 (F(70),APHI(1)),(F(140),AJ(1)),(FIRM(1),F(210)),(F(280),SECM(1)),NTE  330
     2 (F(350),THRM(1)),(F(420),TAU(1)),(CGM(1),F(500)),(CTL(1),F(540)),NTE  340
     3 (F(580),CRAM(1)),(CSAR(1),F(620)),(F(660),CX(1)),(CNSQ(1),F(700))NTE  350
     4 ,(FTM(1),F(800)),(DB0(1),F(1350)),(DB1(1),F(1900)),              NTE  360
     5 (DBIN(1),F(2450)),(DBN(1),F(3000))                               NTE  370
C                                                                       NTE  380
 7003 FORMAT (1P,5E14.6)                                                NTE  390
C                                                                       NTE  400
C                                                                       NTE  410
      READ (M9,7003) ((FJ(I,J),I=1,44),J=1,23)                          NTE  420
      DO 202 I=1,3                                                      NTE  430
        READ (M9,7003) (((C(KZ,K,IXI),KZ=1,9),K=1,33),IXI=1,14)         NTE  440
  202 CONTINUE                                                          NTE  450
C                                                                       NTE  460
C     COMPUTE XI(I)                                                     NTE  470
C                                                                       NTE  480
      DO 1 L=1,5                                                        NTE  490
        EL = L                                                          NTE  500
        YJ(L) = 0.0 + (EL-1.0) * .005                                   NTE  510
    1 CONTINUE                                                          NTE  520
      DO 2 L=6,8                                                        NTE  530
        YJ(L) = YJ(L-1) + .01                                           NTE  540
    2 CONTINUE                                                          NTE  550
      DO 3 L=9,17                                                       NTE  560
        YJ(L) = YJ(L-1) + .05                                           NTE  570
    3 CONTINUE                                                          NTE  580
      DO 4 L=18,22                                                      NTE  590
        YJ(L) = YJ(L-1) + .1                                            NTE  600
    4 CONTINUE                                                          NTE  610
      DO 17 L=1,13                                                      NTE  620
        YL(L) = YJ(L+4)                                                 NTE  630
   17 CONTINUE                                                          NTE  640
C                                                                       NTE  650
C     COMPUTE K                                                         NTE  660
C                                                                       NTE  670
      DO 46 L=1,33                                                      NTE  680
        EL = L                                                          NTE  690
        EKT(L) = 4.0 + (EL-1.0) * 0.5                                   NTE  700
   46 CONTINUE                                                          NTE  710
      DO 47 L=34,44                                                     NTE  720
        FJ(L,23) = FJ(L,22)                                             NTE  730
        EKT(L) = EKT(L-1) + 1.0                                         NTE  740
   47 CONTINUE                                                          NTE  750
      DO 30 L=1,22                                                      NTE  760
        DO 30 J=1,22                                                    NTE  770
          IF(L-J) 31,30,31                                              NTE  780
   31     DENOMJ(L,J) = YJ(L) - YJ(J)                                   NTE  790
   30 CONTINUE                                                          NTE  800
      DO 32 L=1,13                                                      NTE  810
        DO 32 J=1,13                                                    NTE  820
          IF(L-J) 34,32,34                                              NTE  830
   34     DENOML(L,J) = YL(L) - YL(J)                                   NTE  840
   32 CONTINUE                                                          NTE  850
      RETURN                                                            NTE  860
      END                                                               NTE  870