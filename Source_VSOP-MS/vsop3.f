      SUBROUTINE CH3(MAFIA,IMAT,NDES,TEMZUT,TCELS,NHOTD,BUCKL)          CH3   10
C                                                                       CH3   20
C     THOR   (THERMALIZATION 3)                                         CH3   30
C                                                                       CH3   40
      DIMENSION E(96),S(96),SIGA(96),DELTA(96),GFLUX(96),PHI(96),XA(96),CH3   50
     1 XF(96),XS(96),SIGN(96),SIGTR(96),SIGS(96),SELS(97),DID(18),      CH3   60
     2 BEG(51),LBON(51),NDIV(96),ST(96),VEC(5),TIN(50),XTR(96),FINU(96),CH3   70
     3 BBS(98),EKVR(96,96),TIM(96,96),DIF(6,50),IDKER(2,10),SSTH(30,5), CH3   80
     4 SS96(96,5),EO(96),EG(30),IMAT(KMAT),NDES(NXS),TEMZUT(NXS),       CH3   90
     5 TCELS(5,NXS),NHOTD(KMAT),BUCKL(N26,5)                            CH3  100
C                                                                       CH3  110
      REAL SEL1(97)/97*0.0/,SEL2(97)/97*0.0/                            CH3  120
C                                                                       CH3  130
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    CH3  140
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    CH3  150
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PICH3  160
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D,NLAYP,ITTT  CH3  170
C                                                                       CH3  180
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), CH3  190
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10CH3  200
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11CH3  210
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         CH3  220
C                                                                       CH3  230
      COMMON /BLOCKT/ DELT,NOIT,TIN,SIGA,SIGN,SIGS,SIGTR,EKVR,PHI,E,BN, CH3  240
     1 SELS,DID,BEG,LBON,NDIV,DIF,TIM,FMIC,NEVR,NBG,NALPHA,NKER,MNBG,   CH3  250
     2 FINU,DELTA,KDIV,XA,XS,XF,XTR,S,T,UB,DFO,IDT,VEC                  CH3  260
C                                                                       CH3  270
CFZJ035                                                       14.09.04  CH3  280
CFZJ063                                                       26.07.11  CH3  290
      COMMON /GRENZE/ NURTHM,THERGR,IDGAM,IDZUT,IDTHER,NGAM,IDESIN,JJGM,CH3  300
     1 MSTU,MGHUS,NNOBG,NZT(10,9),IZUT(10,2,10),SIRA(68),NIRPI,NID,NDA30CH3  310
     2 ,MDUM                                                            CH3  320
C                                                                       CH3  330
      COMMON /TERFLU/ KANN,NUTTE,NEUSPK,NSAEFL,A39,B39,C39,A40,B40,C40, CH3  340
     1 AD39,AD40,AD39R,AD40R,SSPU39,SSPU40                              CH3  350
C                                                                       CH3  360
      COMMON /VARDIM/ A(8000000)                                        CH3  370
C                                                                       CH3  380
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       CH3  390
C                                                                       CH3  400
      COMMON /TT/ JNTAPE                                                CH3  410
C                                                                       CH3  420
CFZJ055                                                       25.09.07  CH3  430
C                                                                       CH3  440
      EQUIVALENCE(JTPE2,N5),(JTPE3,N6),(JTPE6,N9)                       CH3  450
C                                                                       CH3  460
    1 FORMAT (24H FINE  GROUP    ENERGIES/(I11,E15.7))                  CH3  470
    2 FORMAT (42H FINE GROUP    SOURCES  (LOW ENERGY FIRST)/(I11,E15.7))CH3  480
    7 FORMAT ('1')                                                      CH3  490
   12 FORMAT (6E12.8)                                                   CH3  500
   54 FORMAT (//' ID-NO OF THERMALIZATION LIBRARY IN ERROR')            CH3  510
  109 FORMAT ('1ON UNIT',I6,' IDNR IS NOT CORRECT, IDNR=',I6,' INSTAED OCH3  520
     1F 1001. NO SELFSHIELDING-'/' FACTORS ARE WRITTEN')                CH3  530
 1301 FORMAT (11H SCATTERER ,I2,1H ,18A4/14H TEMPERATURE =,E14.7,11H DEGCH3  540
     1.KELVIN,33H     FREE ATOM SCATTERING SIGMA =,E14.7)               CH3  550
 1510 FORMAT (12I6)                                                     CH3  560
 2171 FORMAT (6H0GROUP,69H    DESIRED ENERGY BOUNDARY               ACTUCH3  570
     1AL ENERGY BOUNDARY USED,16H        LETHARGY/)                     CH3  580
 2172 FORMAT (I5,E24.8,E41.8,E24.8)                                     CH3  590
 5001 FORMAT (I10)                                                      CH3  600
 5002 FORMAT (5E14.6)                                                   CH3  610
 5003 FORMAT (18A4)                                                     CH3  620
 7001 FORMAT (' SELFSHIELDING-FACTORS USED FOR SCATTERER ',I2,' WILL BE CH3  630
     1USED NOW')                                                        CH3  640
 7011 FORMAT (' SELFSHIELDING-FACTORS USED FOR ABSORBER ',I2,' WILL BE UCH3  650
     1SED NOW')                                                         CH3  660
 7151 FORMAT (38H0INPUT FLUX WILL BE USED FOR AVERAGING)                CH3  670
 7208 FORMAT (2H0A,E12.6,50H DEGREE KELVIN MAXWELLIAN FLUX GUESS WILL BECH3  680
     1 USED )                                                           CH3  690
 7211 FORMAT (73H0THE CONVERGED FLUXES OF THE LAST PROBLEM WILL BE USED CH3  700
     1AS THE FLUX GUESS )                                               CH3  710
 7701 FORMAT (11H THERE ARE ,I3,13H FINE GROUPS ,I2,24H ABSORBING NUCLIDCH3  720
     1ES AND ,I2,33H NUCLIDE(S) PROCESSING KERNEL(S)./14H0THERE WILL BE,CH3  730
     2 I2,14H BROAD GROUPS.)                                            CH3  740
 7702 FORMAT (55H0THE CONVERGENCE CRITERION FOR THE FLUX CALCULATION IS CH3  750
     1 ,E12.5)                                                          CH3  760
 7705 FORMAT (41H0THE INITIAL FLUX GUESSES WILL ALL BE ONE)             CH3  770
 7707 FORMAT (40H0INITIAL FLUX GUESSES WILL BE READ IN   )              CH3  780
 7711 FORMAT (10H ABSORBER ,I2,1H ,18A4)                                CH3  790
 7713 FORMAT ('0SELFSHIELDING-FACTORS IN BROAD GROUPS FOR ABSORBER ',I4,CH3  800
     1 '   ***   FROM INPUT   ***')                                     CH3  810
 7714 FORMAT ('0SELFSHIELDING-FACTORS IN BROAD GROUPS FOR SCATTERER',I4,CH3  820
     1 '   ***   FROM INPUT   ***')                                     CH3  830
 7715 FORMAT (6H GROUP,46H     SELF SHIELDING FACTOR  (LOW ENERGY FIRST)CH3  840
     1 /(I5,E24.8))                                                     CH3  850
 7717 FORMAT ('0SELFSHIELDING-FACTORS IN FINE GROUPS FOR ABSORBER ',I4, CH3  860
     1 '   ***   FROM INPUT   ***')                                     CH3  870
 7718 FORMAT ('0SELFSHIELDING-FACTORS IN FINE GROUPS FOR SCATTERER',I4, CH3  880
     1 '   ***   FROM INPUT   ***')                                     CH3  890
 7719 FORMAT ('  MU-BAR =',E16.8)                                       CH3  900
 8423 FORMAT (33H0A MAXWELLIAN FLUX CALCULATED AT ,E16.8,34H KELVIN WILLCH3  910
     1 BE USED FOR AVERAGING)                                           CH3  920
 8797 FORMAT ('1FOR READING THE SSF-DATA SET OF THERMOS AT LEAST FOR ONECH3  930
     1 ABSORBER *FOB* MUST BE <0.'/' THE PROGRAM SETS SELS(I)=1')       CH3  940
 8807 FORMAT ('1FOR ONE SECTOR -FOB=',I6,' ARE NO SELFSHIELDING-FACTORS CH3  950
     1DELIVERED BY THERMOS. THE PROGRAM SETS SELS=1')                   CH3  960
 8808 FORMAT ('096-GROUP-SELFSHIELDING-FACTORS FROM THERMOS-DATA SET SSFCH3  970
     1 =',I6///96(I6,5E14.5/))                                          CH3  980
 8809 FORMAT ('1ON DATA UNIT NSSFAK =',I6,' A DATA SET WITH NO. SSF =', CH3  990
     1 I6,' DOES NOT EXIST'//' THE SELFSHIELDING-FACTORS FOR 96 THERMAL CH3 1000
     2 GROUPS WILL BE = 1')                                             CH3 1010
 8819 FORMAT (' SELFSHIELDING-FACTORS IN 96 GROUPS OF THE ABOVE WRITTEN CH3 1020
     1DATA SET NO.',I6,'  SECTOR NO.',I6,'  WILL BE USED')              CH3 1030
 9002 FORMAT (51H0GROUP          BUCKLING TERM    (LOW ENERGY FIRST)//) CH3 1040
 9007 FORMAT (I5,11X,E13.6)                                             CH3 1050
 9102 FORMAT (48H READ THE INSTRUCTIONS,I TOLD YOU NOT TO SET W#0/27H0NOCH3 1060
     1W W WILL BE CHANGED TO 1)                                         CH3 1070
 9103 FORMAT (27H0ACCELERATION PARAMETER W# ,F10.5/1H0)                 CH3 1080
10001 FORMAT (//' ... SET ',I4,' STARTS ON NDA10 WITH RECORD NO.',I6//) CH3 1090
C                                                                       CH3 1100
C                                                                       CH3 1110
C     UNIT 2 IS USED FOR INTERMEDIATE STORAGE OF DATA TO BE AVERAGED    CH3 1120
C                                                                       CH3 1130
C     UNIT DESIGNATION                                                  CH3 1140
C                                                                       CH3 1150
C     N5     INPUT UNIT NUMBER                                          CH3 1160
C     N6     OUTPUT UNIT NUMBER                                         CH3 1170
C     N1     INTERMEDIATE STORAGE UNIT                                  CH3 1180
C     N2     INTERMEDIATE STORAGE UNIT                                  CH3 1190
C     N9     UNIT NUMBER OF THE THERMAL LIBRARY                         CH3 1200
C                                                                       CH3 1210
      NIRPI = IPRIN(7)                                                  CH3 1220
      IF(ITTT .LE. 0) GOTO 1270                                         CH3 1230
      OPEN(N9,FILE='Libraries\thermal')                                 CH3 1240
C                                                                       CH3 1250
C     THERMALIZATION - CODE                                             CH3 1260
C                                                                       CH3 1270
CARD TTTT1                                                              CH3 1280
C                                                                       CH3 1290
      READ (N5,1510) JNTAPE, NKER,((IDKER(J,I),J=1,2),I=1,NKER)         CH3 1300
C                                                                       CH3 1310
      JSATZ = 20 + NIRPI                                                CH3 1320
      IF(JAD10(JSATZ) .EQ. 0) JAD10(JSATZ) = JSUM10                     CH3 1330
      NXT10 = JAD10(JSATZ)                                              CH3 1340
      WRITE (6,10001) JSATZ,NXT10                                       CH3 1350
      REWIND N9                                                         CH3 1360
      READ (N9,5001) NTAPE                                              CH3 1370
      IF(JNTAPE-NTAPE) 53,5,53                                          CH3 1380
   53 WRITE (N6,54)                                                     CH3 1390
C                                                                       CH3 1400
      CALL EXIT                                                         CH3 1410
C                                                                       CH3 1420
    5 CONTINUE                                                          CH3 1430
      WRITE (N6,7)                                                      CH3 1440
      NBG = 1                                                           CH3 1450
      NFLG = 3                                                          CH3 1460
      NEVR = 96                                                         CH3 1470
      NALPHA = KMAT - NKER                                              CH3 1480
      HAT = 0.0                                                         CH3 1490
      BBS(1) = BUCKL(N26,NIRPI)                                         CH3 1500
C                                                                       CH3 1510
C     ENERGY BOUNDARY                                                   CH3 1520
C                                                                       CH3 1530
      BEG(1) = THERGR                                                   CH3 1540
C                                                                       CH3 1550
CARD TTTT2                                                              CH3 1560
C                                                                       CH3 1570
      READ (N5,12) TOM,EPSI,WAT                                         CH3 1580
C                                                                       CH3 1590
      SSOPT = 0.                                                        CH3 1600
      IF(WAT) 9100,9101,9100                                            CH3 1610
 9101 WRITE (N6,9102)                                                   CH3 1620
      WAT = 1.0                                                         CH3 1630
 9100 WRITE (N6,9103) WAT                                               CH3 1640
      HAT = 1.0 - WAT                                                   CH3 1650
      WRITE (N6,7701) NEVR,NALPHA,NKER,NBG                              CH3 1660
      GOTO(7693,7694,7207,7209,7111,8421),NFLG                          CH3 1670
 8421 WRITE (N6,8423) TOM                                               CH3 1680
      GOTO 9999                                                         CH3 1690
 7111 WRITE (N6,7151)                                                   CH3 1700
      GOTO 9999                                                         CH3 1710
 7693 WRITE (N6,7705)                                                   CH3 1720
      DO 3 J=1,96                                                       CH3 1730
        PHI(J) = 1.0                                                    CH3 1740
    3 CONTINUE                                                          CH3 1750
      GOTO 7196                                                         CH3 1760
 7694 WRITE (N6,7707)                                                   CH3 1770
      GOTO 7196                                                         CH3 1780
 7207 WRITE (N6,7208) TOM                                               CH3 1790
      GOTO 7196                                                         CH3 1800
 7209 WRITE (N6,7211)                                                   CH3 1810
 7196 WRITE (N6,7702) EPSI                                              CH3 1820
 9999 CONTINUE                                                          CH3 1830
C                                                                       CH3 1840
C     ZERO OUT SOME THINGS                                              CH3 1850
C                                                                       CH3 1860
      NSS1 = 0                                                          CH3 1870
      NSS2 = 0                                                          CH3 1880
      NSSFAK = 0                                                        CH3 1890
      DO 901 I=1,NEVR                                                   CH3 1900
        ST(I) = 0.0                                                     CH3 1910
        FINU(I) = 0.0                                                   CH3 1920
        SIGTR(I) = 0.0                                                  CH3 1930
        SIGN(I) = 0.0                                                   CH3 1940
        SIGS(I) = 0.0                                                   CH3 1950
        SIGA(I) = 0.0                                                   CH3 1960
  901 CONTINUE                                                          CH3 1970
      READ (N9,5002) (E(I),I=1,NEVR)                                    CH3 1980
      IF(IPRIN(1) .LT. 2) GOTO 3300                                     CH3 1990
      WRITE (N6,7)                                                      CH3 2000
      WRITE (N6,1) (I,E(I),I=1,48)                                      CH3 2010
      WRITE (N6,7)                                                      CH3 2020
      WRITE (N6,1) (I,E(I),I=49,NEVR)                                   CH3 2030
 3300 CONTINUE                                                          CH3 2040
      DO 3311 I=1,NBG                                                   CH3 2050
        K = NBG + 1 - I                                                 CH3 2060
        GFLUX(I) = BEG(K)                                               CH3 2070
 3311 CONTINUE                                                          CH3 2080
      DO 3314 I=1,NBG                                                   CH3 2090
        BEG(I) = GFLUX(I)                                               CH3 2100
 3314 CONTINUE                                                          CH3 2110
      LBON(1) = 1                                                       CH3 2120
      IF(BEG(NBG)-2.1) 2121,2121,2120                                   CH3 2130
 2120 BEG(NBG) = 2.1                                                    CH3 2140
 2121 JTS = 0                                                           CH3 2150
      DO 2130 I=1,NBG                                                   CH3 2160
 2127   JTS = JTS + 1                                                   CH3 2170
        IF(BEG(I)-E(JTS)) 2137,2147,2127                                CH3 2180
 2137   LL = ((BEG(I)-E(JTS-1))/(E(JTS)-E(JTS-1))+0.5)                  CH3 2190
        JTS = JTS - 1 + LL                                              CH3 2200
 2147   LBON(I+1) = JTS + 1                                             CH3 2210
 2130 CONTINUE                                                          CH3 2220
C                                                                       CH3 2230
C     PRINT-OUT DESIRED AND ACTUAL ENERGY BOUNDARIES                    CH3 2240
C                                                                       CH3 2250
      WRITE (N6,2171)                                                   CH3 2260
      DO 2173 K=1,NBG                                                   CH3 2270
        I = NBG + 1 - K                                                 CH3 2280
        J = LBON(I+1)                                                   CH3 2290
        UST = ALOG(1.0E7/E(J-1))                                        CH3 2300
        WRITE (N6,2172) K,BEG(I),E(J-1),UST                             CH3 2310
 2173 CONTINUE                                                          CH3 2320
      MNBG = 0                                                          CH3 2330
      IF(LBON(NBG+1)-NEVR-1) 2187,2186,2186                             CH3 2340
 2187 LBON(NBG+2) = NEVR + 1                                            CH3 2350
      MNBG = 1                                                          CH3 2360
 2186 MNBG = MNBG + NBG                                                 CH3 2370
C                                                                       CH3 2380
C     FIND WHERE DELTA-E IS THE SAME                                    CH3 2390
C                                                                       CH3 2400
      K = 1                                                             CH3 2410
      NRQ = NEVR - 2                                                    CH3 2420
      DO 2151 JQ=2,NRQ                                                  CH3 2430
        IF(ABS(2.0*E(JQ+1)-E(JQ)-E(JQ+2))-0.00001) 2151,2151,2156       CH3 2440
 2156   NDIV(K) = JQ + 1                                                CH3 2450
        K = K + 1                                                       CH3 2460
 2151 CONTINUE                                                          CH3 2470
      NDIV(K) = NEVR                                                    CH3 2480
      KDIV = K                                                          CH3 2490
      NEVRPR = NEVR - 1                                                 CH3 2500
      DO 1200 I=1,NEVR                                                  CH3 2510
        DO 1200 J=1,NEVR                                                CH3 2520
          EKVR(I,J) = 0.                                                CH3 2530
 1200 CONTINUE                                                          CH3 2540
      NEVRP = NEVR + 1                                                  CH3 2550
      NBGP = NBG + 1                                                    CH3 2560
      WRITE (N6,9002)                                                   CH3 2570
      I = 1                                                             CH3 2580
      WRITE (N6,9007) I,BBS(I)                                          CH3 2590
      BBS(MNBG) = BBS(NBG)                                              CH3 2600
      M = 1                                                             CH3 2610
      DO 9004 K=1,NBG                                                   CH3 2620
        JK = NBGP - K                                                   CH3 2630
        DO 9005 I=M,NEVRP                                               CH3 2640
          JI = NEVR + 2 - I                                             CH3 2650
          IF(JI-LBON(JK)) 9006,9008,9008                                CH3 2660
 9008     BBS(JI) = BBS(JK)                                             CH3 2670
 9005   CONTINUE                                                        CH3 2680
 9006   M = I                                                           CH3 2690
 9004 CONTINUE                                                          CH3 2700
      WRITE (N6,7)                                                      CH3 2710
      KCB = 0                                                           CH3 2720
      DELTA(1) = (E(2)+E(1)) / 2.                                       CH3 2730
      DO 65 J=2,NEVRPR                                                  CH3 2740
        DELTA(J) = (E(J+1)-E(J-1)) / 2.                                 CH3 2750
   65 CONTINUE                                                          CH3 2760
      DELTA(NEVR) = E(NEVR) - E(NEVRPR)                                 CH3 2770
      IF(SSOPT .EQ. 0.) GOTO 66                                         CH3 2780
C                                                                       CH3 2790
CARDS TERTT3 / TERTT4                                                   CH3 2800
C                                                                       CH3 2810
      READ (N5,12) A39,B39,C39,AD39R                                    CH3 2820
      READ (N5,12) A40,B40,C40,AD40R                                    CH3 2830
C                                                                       CH3 2840
      GOTO 67                                                           CH3 2850
   66 CONTINUE                                                          CH3 2860
      A39 = 1.                                                          CH3 2870
      A40 = 1.                                                          CH3 2880
      B39 = 0.                                                          CH3 2890
      B40 = 0.                                                          CH3 2900
      C39 = 0.                                                          CH3 2910
      C40 = 0.                                                          CH3 2920
      AD39R = 0.                                                        CH3 2930
      AD40R = 0.                                                        CH3 2940
   67 CONTINUE                                                          CH3 2950
      WRITE (NDA10,REC=NXT10) NEVR,NALPHA,NKER,NBG,MNBG,NRQ,TOM,WAT,HAT,CH3 2960
     1 EPSI,(E(I),I=1,NEVR),((IDKER(J,I),J=1,2),I=1,10),BEG(1),LBON(1), CH3 2970
     2 LBON(2),LBON(3),(NHOTD(I),I=1,KMAT),A39,B39,C39,A40,B40,C40,AD39RCH3 2980
     3 ,AD40R                                                           CH3 2990
      NXT10 = NXT10 + 1                                                 CH3 3000
C                                                                       CH3 3010
C     READ IN ABSORBERS                                                 CH3 3020
C                                                                       CH3 3030
      NUCL1 = 0                                                         CH3 3040
      NUCL2 = 0                                                         CH3 3050
      NNUCL = 0                                                         CH3 3060
      NNU = 0                                                           CH3 3070
      NS1 = 1                                                           CH3 3080
      IF(NALPHA) 5009,5009,11                                           CH3 3090
   11 DO 10 J=1,NALPHA                                                  CH3 3100
        KCB = KCB + 1                                                   CH3 3110
 1500 CONTINUE                                                          CH3 3120
        READ (N9,5001) IDT                                              CH3 3130
        READ (N9,5003) (DID(I),I=1,18)                                  CH3 3140
        READ (N9,5002) DFO,UB                                           CH3 3150
        READ (N9,5002) (XA(I),I=1,NEVR)                                 CH3 3160
        READ (N9,5002) (XS(I),I=1,NEVR)                                 CH3 3170
        READ (N9,5002) (XF(I),I=1,NEVR)                                 CH3 3180
        READ (N9,5002) (XTR(I),I=1,NEVR)                                CH3 3190
        READ (N9,5002) ATGEWI                                           CH3 3200
        DO 1501 I=1,NALPHA                                              CH3 3210
          MANU = IMAT(I)                                                CH3 3220
          IF(IDT-MANU) 1501,1502,1501                                   CH3 3230
 1501   CONTINUE                                                        CH3 3240
        GOTO 1500                                                       CH3 3250
 1502   CONTINUE                                                        CH3 3260
        WRITE (N6,7711) J,(DID(I),I=1,18)                               CH3 3270
        SSF = 0.                                                        CH3 3280
        FOB = 0.                                                        CH3 3290
        IF(NNUCL .GE. IDT .OR. NNU .EQ. 1) GOTO 44441                   CH3 3300
        IF(SSOPT .EQ. 0.) GOTO 1506                                     CH3 3310
C                                                                       CH3 3320
CARD TERTT5                                                             CH3 3330
C                                                                       CH3 3340
        READ (N5,1510) NNUCL,MSSF,NFOB,NS2                              CH3 3350
C                                                                       CH3 3360
        GOTO 1507                                                       CH3 3370
 1506   CONTINUE                                                        CH3 3380
        NNUCL = -4                                                      CH3 3390
        MSSF = 0                                                        CH3 3400
        NFOB = 0                                                        CH3 3410
        NS2 = 0                                                         CH3 3420
 1507   CONTINUE                                                        CH3 3430
        IF(.NOT. (MSSF .GT. 0 .AND. NFOB .GE. 0)) GOTO 3000             CH3 3440
        NUCL1 = IABS(NNUCL)                                             CH3 3450
        NUCSS = 0                                                       CH3 3460
        IM = 0                                                          CH3 3470
        DO 1515 I=1,NALPHA                                              CH3 3480
          IF(IMAT(I) .GT. NUCL1) GOTO 1515                              CH3 3490
          IM = IM + 1                                                   CH3 3500
 1515   CONTINUE                                                        CH3 3510
        IF(IM .LT. NALPHA) GOTO 1525                                    CH3 3520
        DO 1520 I=1,NKER                                                CH3 3530
          IM = I                                                        CH3 3540
          IF(IDKER(2,I) .EQ. NUCL1) GOTO 1522                           CH3 3550
 1520   CONTINUE                                                        CH3 3560
 1522   NUCSS = IM                                                      CH3 3570
        GOTO 1530                                                       CH3 3580
 1525   NUCL1 = IM                                                      CH3 3590
 1530   CONTINUE                                                        CH3 3600
        IF(NFOB .GT. 0) GOTO 2035                                       CH3 3610
C                                                                       CH3 3620
CARD TERTT6                                                             CH3 3630
C                                                                       CH3 3640
        READ (N5,12) (SEL1(I),I=1,NBG)                                  CH3 3650
C                                                                       CH3 3660
        IF(NUCSS .GT. 0) GOTO 2020                                      CH3 3670
        WRITE (N6,7713) NUCL1                                           CH3 3680
        GOTO 2022                                                       CH3 3690
 2020   WRITE (N6,7714) NUCSS                                           CH3 3700
 2022   CONTINUE                                                        CH3 3710
        WRITE (N6,7715) (I,SEL1(I),I=1,NBG)                             CH3 3720
        SEL1(NBGP) = SEL1(NBG)                                          CH3 3730
        M = 1                                                           CH3 3740
        DO 2041 K=1,NBG                                                 CH3 3750
          JK = NBGP - K                                                 CH3 3760
          DO 2042 I=M,NEVRP                                             CH3 3770
            JI = NEVR + 2 - I                                           CH3 3780
            IF(JI-LBON(JK)) 2043,2044,2044                              CH3 3790
 2044       SEL1(JI) = SEL1(JK)                                         CH3 3800
 2042     CONTINUE                                                      CH3 3810
 2043     M = I                                                         CH3 3820
 2041   CONTINUE                                                        CH3 3830
        GOTO 3000                                                       CH3 3840
 2035   CONTINUE                                                        CH3 3850
C                                                                       CH3 3860
        READ (N5,12) (SEL1(I),I=1,NEVR)                                 CH3 3870
C                                                                       CH3 3880
        KCB = 23                                                        CH3 3890
        WRITE (N6,7)                                                    CH3 3900
        IF(NUCSS .GT. 0) GOTO 2050                                      CH3 3910
        WRITE (N6,7717) NUCL1                                           CH3 3920
        GOTO 2052                                                       CH3 3930
 2050   WRITE (N6,7718) NUCSS                                           CH3 3940
 2052   CONTINUE                                                        CH3 3950
        WRITE (N6,7715) (I,SEL1(I),I=1,48)                              CH3 3960
        WRITE (N6,7)                                                    CH3 3970
        WRITE (N6,7715) (I,SEL1(I),I=49,NEVR)                           CH3 3980
 3000   CONTINUE                                                        CH3 3990
        IF(NS2 .GE. 0) GOTO 44440                                       CH3 4000
        NS1 = NS1 - 2                                                   CH3 4010
        IF(NS1 .EQ. -3) GOTO 44440                                      CH3 4020
        NSSF2 = MSSF                                                    CH3 4030
        NFOB2 = NFOB                                                    CH3 4040
        IF(NUCL1 .EQ. 0) GOTO 44440                                     CH3 4050
        NUCL2 = NUCL1                                                   CH3 4060
        DO 3100 I=1,NEVR                                                CH3 4070
          SEL2(I) = SEL1(I)                                             CH3 4080
 3100   CONTINUE                                                        CH3 4090
44440   CONTINUE                                                        CH3 4100
        IF(NNUCL .GT. 0) GOTO 44441                                     CH3 4110
        NNU = 1                                                         CH3 4120
        NNUCL = IABS(NNUCL)                                             CH3 4130
44441   CONTINUE                                                        CH3 4140
        IF(NNUCL .NE. IDT) GOTO 44442                                   CH3 4150
        SSF = FLOAT(MSSF)                                               CH3 4160
        FOB = FLOAT(NFOB)                                               CH3 4170
        IF(NUCL1 .EQ. 0) GOTO 33300                                     CH3 4180
        NUKLID = NUCL1                                                  CH3 4190
        DO 3200 I=1,NEVR                                                CH3 4200
          SELS(I) = SEL1(I)                                             CH3 4210
 3200   CONTINUE                                                        CH3 4220
33300   CONTINUE                                                        CH3 4230
        IF(NS1 .GT. -3) GOTO 44444                                      CH3 4240
        NS1 = NS1 + 2                                                   CH3 4250
        NSSF2 = MSSF                                                    CH3 4260
        NFOB2 = NFOB                                                    CH3 4270
        IF(NUCL1 .EQ. 0) GOTO 44444                                     CH3 4280
        NUCL2 = NUCL1                                                   CH3 4290
        DO 3400 I=1,NEVR                                                CH3 4300
          SEL2(I) = SEL1(I)                                             CH3 4310
 3400   CONTINUE                                                        CH3 4320
44444   CONTINUE                                                        CH3 4330
        GOTO 44443                                                      CH3 4340
44442   CONTINUE                                                        CH3 4350
        IF(NS1 .GT. 0) GOTO 44443                                       CH3 4360
        SSF = FLOAT(NSSF2)                                              CH3 4370
        FOB = FLOAT(NFOB2)                                              CH3 4380
        IF(NUCL1 .EQ. 0) GOTO 44443                                     CH3 4390
        NUKLID = NUCL2                                                  CH3 4400
        DO 3500 I=1,NEVR                                                CH3 4410
          SELS(I) = SEL2(I)                                             CH3 4420
 3500   CONTINUE                                                        CH3 4430
44443   CONTINUE                                                        CH3 4440
        IF(SSF) 2021,2021,2031                                          CH3 4450
 2031   IF(FOB .LT. 0) GOTO 8800                                        CH3 4460
        WRITE (N6,7011) NUKLID                                          CH3 4470
        GOTO 2071                                                       CH3 4480
C                                                                       CH3 4490
C     FLUSS-ABSENKUKGSFAKTOREN VON EINHEIT NSSFAK                       CH3 4500
C                                                                       CH3 4510
 8800   CONTINUE                                                        CH3 4520
        NSSF = IFIX(SSF)                                                CH3 4530
        IF(NSS2 .GT. 0) GOTO 8799                                       CH3 4540
        NSSFAK = JTPE8                                                  CH3 4550
        REWIND NSSFAK                                                   CH3 4560
        DO 8821 I=1,NEVR                                                CH3 4570
          DO 8821 M=1,5                                                 CH3 4580
            SS96(I,M) = 0.                                              CH3 4590
 8821   CONTINUE                                                        CH3 4600
        NSS2 = 1                                                        CH3 4610
 8799   CONTINUE                                                        CH3 4620
        IF(NSS1 .EQ. 1) GOTO 2021                                       CH3 4630
        IF(NSSFAK .EQ. 0) GOTO 8810                                     CH3 4640
        READ (NSSFAK) IDNR                                              CH3 4650
        IF(IDNR .EQ. 1001) GOTO 101                                     CH3 4660
        WRITE (N6,109) NSSFAK,IDNR                                      CH3 4670
        NSS1 = 1                                                        CH3 4680
        GOTO 2021                                                       CH3 4690
  101   CONTINUE                                                        CH3 4700
        DO 8801 ISAZ=1,160                                              CH3 4710
          READ (NSSFAK) NRSATZ                                          CH3 4720
          IF(NRSATZ .NE. NSSF) GOTO 8801                                CH3 4730
          IF(NRSATZ .LT. 0) GOTO 8812                                   CH3 4740
          BACKSPACE NSSFAK                                              CH3 4750
          READ (NSSFAK) NRSATZ,IX,MX,(EG(I),(SSTH(I,M),M=1,4),I=1,30)   CH3 4760
          GOTO 8802                                                     CH3 4770
 8801   CONTINUE                                                        CH3 4780
 8812   CONTINUE                                                        CH3 4790
        WRITE (N6,8809) NSSFAK,NSSF                                     CH3 4800
        NSS1 = 1                                                        CH3 4810
        GOTO 2021                                                       CH3 4820
 8802   CONTINUE                                                        CH3 4830
        ITH = 1                                                         CH3 4840
        DO 8803 I=1,NEVR                                                CH3 4850
          EO(I) = E(I) + DELTA(I) / 2.                                  CH3 4860
          IF(EO(I) .LE. EG(ITH)) GOTO 8804                              CH3 4870
          ITH = ITH + 1                                                 CH3 4880
          IF(ITH .LE. IX) GOTO 8804                                     CH3 4890
          ITH = IX                                                      CH3 4900
 8804     CONTINUE                                                      CH3 4910
          DO 8803 M=1,MX                                                CH3 4920
            SS96(I,M) = SSTH(ITH,M)                                     CH3 4930
 8803   CONTINUE                                                        CH3 4940
        WRITE (N6,8808) NSSF,(I,(SS96(I,M),M=1,5),I=1,NEVR)             CH3 4950
        NSSFAK = 0                                                      CH3 4960
 8810   CONTINUE                                                        CH3 4970
        MIXT = ABS(FOB)                                                 CH3 4980
        IF(MIXT .GT. MX) GOTO 8806                                      CH3 4990
        DO 8805 I=1,NEVR                                                CH3 5000
          SELS(I) = SS96(I,MIXT)                                        CH3 5010
 8805   CONTINUE                                                        CH3 5020
        WRITE (N6,8819) NSSF,MIXT                                       CH3 5030
        GOTO 2071                                                       CH3 5040
 8806   CONTINUE                                                        CH3 5050
        WRITE (N6,8807) MIXT                                            CH3 5060
 2021   DO 2025 I=1,NEVR                                                CH3 5070
          SELS(I) = 1.0                                                 CH3 5080
 2025   CONTINUE                                                        CH3 5090
 2071   CONTINUE                                                        CH3 5100
        ADEN = 0.0                                                      CH3 5110
        WRITE (NDA10,REC=NXT10) IDT,(DID(I),I=1,18),ADEN,UB,DFO,SSF,    CH3 5120
     1   (SELS(I),I=1,NEVR),(XA(I),I=1,NEVR),(XS(I),I=1,NEVR),(XF(I),I=1CH3 5130
     2   ,NEVR),(XTR(I),I=1,NEVR)                                       CH3 5140
        NXT10 = NXT10 + 1                                               CH3 5150
   10 CONTINUE                                                          CH3 5160
      IF(NUCSS .NE. 0) NUCL1 = NUCSS                                    CH3 5170
 5009 READ (N9,5001) IDT                                                CH3 5180
      IF(IDT) 5010,5009,5009                                            CH3 5190
C                                                                       CH3 5200
C     READ IN SCATTERS                                                  CH3 5210
C                                                                       CH3 5220
 5010 DO 1206 KIM=1,NKER                                                CH3 5230
        DFO = 0.0                                                       CH3 5240
 1503   CONTINUE                                                        CH3 5250
        READ (N9,5001) IDT                                              CH3 5260
        READ (N9,5003) (DID(I),I=1,18)                                  CH3 5270
        READ (N9,5002) T,FSX,UB                                         CH3 5280
        READ (N9,5002) (S(I),I=1,NEVR)                                  CH3 5290
        READ (N9,5002) (XA(I),I=1,NEVR)                                 CH3 5300
        READ (N9,5002) (XS(I),I=1,NEVR)                                 CH3 5310
        READ (N9,5002) ((TIM(I,J),I=1,NEVR),J=1,NEVR)                   CH3 5320
        READ (N9,5002) (XTR(I),I=1,NEVR)                                CH3 5330
        READ (N9,5002) ATGEWI                                           CH3 5340
        DO 1504 I=1,NKER                                                CH3 5350
          MANU = IDKER(2,I)                                             CH3 5360
          IF(IDT-MANU) 1504,1505,1504                                   CH3 5370
 1504   CONTINUE                                                        CH3 5380
        GOTO 1503                                                       CH3 5390
 1505   CONTINUE                                                        CH3 5400
        JI = NALPHA + I                                                 CH3 5410
        WRITE (N6,1301) KIM,(DID(I),I=1,18),T,FSX                       CH3 5420
        WRITE (N6,7719) UB                                              CH3 5430
        IF(IPRIN(1) .LT. 2) GOTO 1550                                   CH3 5440
        WRITE (N6,2) (I,S(I),I=1,44)                                    CH3 5450
        WRITE (N6,7)                                                    CH3 5460
        WRITE (N6,2) (I,S(I),I=45,NEVR)                                 CH3 5470
 1550   CONTINUE                                                        CH3 5480
        SSF = 0.                                                        CH3 5490
        FOB = 0.                                                        CH3 5500
        IF(NNUCL .GE. IDT .OR. NNU .EQ. 1) GOTO 44451                   CH3 5510
C                                                                       CH3 5520
        READ (N5,1510) NNUCL,MSSF,NFOB,NS2                              CH3 5530
C                                                                       CH3 5540
        IF(.NOT. (MSSF .GT. 0 .AND. NFOB .GE. 0)) GOTO 4400             CH3 5550
        NUCL1 = IABS(NNUCL)                                             CH3 5560
        DO 1570 I=1,NKER                                                CH3 5570
          IM = I                                                        CH3 5580
          IF(IDKER(2,I) .EQ. NUCL1) GOTO 1572                           CH3 5590
 1570   CONTINUE                                                        CH3 5600
 1572   NUCL1 = IM                                                      CH3 5610
        IF(NFOB .GT. 0) GOTO 4035                                       CH3 5620
C                                                                       CH3 5630
        READ (N5,12) (SEL1(I),I=1,NBG)                                  CH3 5640
C                                                                       CH3 5650
        WRITE (N6,7714) NUCL1                                           CH3 5660
        WRITE (N6,7715) (I,SEL1(I),I=1,NBG)                             CH3 5670
        SEL1(NBGP) = SEL1(NBG)                                          CH3 5680
        M = 1                                                           CH3 5690
        DO 4041 K=1,NBG                                                 CH3 5700
          JK = NBGP - K                                                 CH3 5710
          DO 4042 I=M,NEVRP                                             CH3 5720
            JI = NEVR + 2 - I                                           CH3 5730
            IF(JI-LBON(JK)) 4043,4044,4044                              CH3 5740
 4044       SEL1(JI) = SEL1(JK)                                         CH3 5750
 4042     CONTINUE                                                      CH3 5760
 4043     M = I                                                         CH3 5770
 4041   CONTINUE                                                        CH3 5780
        GOTO 4400                                                       CH3 5790
C                                                                       CH3 5800
 4035   READ (N5,12) (SEL1(I),I=1,NEVR)                                 CH3 5810
C                                                                       CH3 5820
        WRITE (N6,7)                                                    CH3 5830
        WRITE (N6,7718) NUCL1                                           CH3 5840
        WRITE (N6,7715) (I,SEL1(I),I=1,48)                              CH3 5850
        WRITE (N6,7)                                                    CH3 5860
        WRITE (N6,7715) (I,SEL1(I),I=49,NEVR)                           CH3 5870
 4400   CONTINUE                                                        CH3 5880
        IF(NS2 .GE. 0) GOTO 44450                                       CH3 5890
        NS1 = NS1 - 2                                                   CH3 5900
        IF(NS1 .EQ. -3) GOTO 44450                                      CH3 5910
        NSSF2 = MSSF                                                    CH3 5920
        NFOB2 = NFOB                                                    CH3 5930
        IF(NUCL1 .EQ. 0) GOTO 44450                                     CH3 5940
        NUCL2 = NUCL1                                                   CH3 5950
        DO 6100 I=1,NEVR                                                CH3 5960
          SEL2(I) = SEL1(I)                                             CH3 5970
 6100   CONTINUE                                                        CH3 5980
44450   CONTINUE                                                        CH3 5990
        IF(NNUCL .GT. 0) GOTO 44451                                     CH3 6000
        NNU = 1                                                         CH3 6010
        NNUCL = IABS(NNUCL)                                             CH3 6020
44451   CONTINUE                                                        CH3 6030
        IF(NNUCL .NE. IDT) GOTO 44452                                   CH3 6040
        SSF = FLOAT(MSSF)                                               CH3 6050
        FOB = FLOAT(NFOB)                                               CH3 6060
        IF(NUCL1 .EQ. 0) GOTO 33330                                     CH3 6070
        NUKLID = NUCL1                                                  CH3 6080
        DO 6200 I=1,NEVR                                                CH3 6090
          SELS(I) = SEL1(I)                                             CH3 6100
 6200   CONTINUE                                                        CH3 6110
33330   CONTINUE                                                        CH3 6120
        IF(NS1 .GT. -3) GOTO 44454                                      CH3 6130
        NS1 = NS1 + 2                                                   CH3 6140
        NSSF2 = MSSF                                                    CH3 6150
        NFOB2 = NFOB                                                    CH3 6160
        IF(NUCL1 .EQ. 0) GOTO 44454                                     CH3 6170
        NUCL2 = NUCL1                                                   CH3 6180
        DO 6300 I=1,NEVR                                                CH3 6190
          SEL2(I) = SEL1(I)                                             CH3 6200
 6300   CONTINUE                                                        CH3 6210
44454   CONTINUE                                                        CH3 6220
        GOTO 44453                                                      CH3 6230
44452   CONTINUE                                                        CH3 6240
        IF(NS1 .GT. 0) GOTO 44453                                       CH3 6250
        SSF = FLOAT(NSSF2)                                              CH3 6260
        FOB = FLOAT(NFOB2)                                              CH3 6270
        IF(NUCL1 .EQ. 0) GOTO 44453                                     CH3 6280
        NUKLID = NUCL2                                                  CH3 6290
        DO 6400 I=1,NEVR                                                CH3 6300
          SELS(I) = SEL2(I)                                             CH3 6310
 6400   CONTINUE                                                        CH3 6320
44453   CONTINUE                                                        CH3 6330
        IF(SSF) 4021,4021,4023                                          CH3 6340
 4023   IF(FOB .LT. 0.0) GOTO 8811                                      CH3 6350
        WRITE (N6,7001) NUKLID                                          CH3 6360
        GOTO 4071                                                       CH3 6370
 8811   CONTINUE                                                        CH3 6380
        NSSF = IFIX(SSF)                                                CH3 6390
        IF(NSS2 .GT. 0) GOTO 8798                                       CH3 6400
        WRITE (N6,8797)                                                 CH3 6410
        GOTO 4021                                                       CH3 6420
 8798   CONTINUE                                                        CH3 6430
        IF(NSS1 .EQ. 1) GOTO 4021                                       CH3 6440
        MIXT = ABS(FOB)                                                 CH3 6450
        IF(MIXT .GT. MX) GOTO 8816                                      CH3 6460
        DO 8815 I=1,NEVR                                                CH3 6470
          SELS(I) = SS96(I,MIXT)                                        CH3 6480
 8815   CONTINUE                                                        CH3 6490
        WRITE (N6,8819) NSSF,MIXT                                       CH3 6500
        GOTO 4071                                                       CH3 6510
 8816   CONTINUE                                                        CH3 6520
        WRITE (N6,8807) MIXT                                            CH3 6530
 4021   DO 4025 I=1,NEVR                                                CH3 6540
          SELS(I) = 1.0                                                 CH3 6550
 4025   CONTINUE                                                        CH3 6560
 4071   CONTINUE                                                        CH3 6570
        WRITE (NDA10,REC=NXT10) IDT,(DID(I),I=1,18),ADEN,UB,SSF,FSX,    CH3 6580
     1   (SELS(I),I=1,NEVR),(XA(K),K=1,NEVR),(XS(K),K=1,NEVR),(XTR(K),K=CH3 6590
     2   1,NEVR),(S(K),K=1,NEVR)                                        CH3 6600
        NXT10 = NXT10 + 1                                               CH3 6610
C                                                                       CH3 6620
        CALL WRDA(IWRITE,NDA10,NXT10,L10,TIM,NEVR*NEVR)                 CH3 6630
C                                                                       CH3 6640
 1206 CONTINUE                                                          CH3 6650
      IF(JSUM10 .LT. NXT10) JSUM10 = NXT10                              CH3 6660
 1270 CONTINUE                                                          CH3 6670
      NP1 = NENDP + 1                                                   CH3 6680
      NP2 = NP1 + NXS                                                   CH3 6690
      NP3 = NP2 + KMAT+55                                               CH3 6700
C                                                                       CH3 6710
      IF(IPRIN(7) .EQ. 1) CALL SHOLLI(IMAT,TCELS,A(NP1),A(NP2),A(NP3))  CH3 6720
C                                                                       CH3 6730
      IF(IPRIN(7)-NXS) 70,71,71                                         CH3 6740
   70 MAFIA = 2                                                         CH3 6750
      RETURN                                                            CH3 6760
   71 CONTINUE                                                          CH3 6770
      IPRIN(7) = 0                                                      CH3 6780
      MAFIA = 4                                                         CH3 6790
      RETURN                                                            CH3 6800
      END                                                               CH3 6810
      SUBROUTINE SHOLLI(IMAT,TCELS,NTYSP,NIDT,VF)                       HOL   10
C                                                                       HOL   20
CFZJ046                                                       26.09.06  HOL   30
      DIMENSION IDTA(10),XA(30),XS(30),SP(30),PP(30,30),XF(30),XT(30),  HOL   40
     1 DID(18),IKER(5,20),JKER(100),TCLIB(100),JDL10(100),IMAT(KMAT),   HOL   50
     2 TCELS(5,NXS),NTYSP(NXS),NIDT(KMAT+55),VF(KMAT,10)                HOL   60
C                                                                       HOL   70
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    HOL   80
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    HOL   90
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIHOL  100
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 HOL  110
C                                                                       HOL  120
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), HOL  130
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10HOL  140
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11HOL  150
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         HOL  160
C                                                                       HOL  170
      COMMON /BLOCRT/ ID,NBER,IX,MX,NX,ICOAT,MY,IBRENN,SS96(96,10),     HOL  180
     1 F(20,30),MTBL(20),VOL(20),V(30),DV(30),VV(30),VDV(30),MG(30),    HOL  190
     2 COZ(30,2),GIM(30,10),ALBEDO(30),LEAKT                            HOL  200
C                                                                       HOL  210
      COMMON /TERFLU/ KANN,NUTTE,NEUSPK,NSAEFL,A39,B39,C39,A40,B40,C40, HOL  220
     1 AD39,AD40,AD39R,AD40R,SSPU39,SSPU40                              HOL  230
C                                                                       HOL  240
CFZJ035                                                       14.09.04  HOL  250
CFZJ063                                                       26.07.11  HOL  260
      COMMON /GRENZE/ NURTHM,THERGR,IDGAM,IDZUT,IDTHER,NGAM,IDESIN,JJGM,HOL  270
     1 MSTU,MGHUS,NNOBG,NZT(10,9),IZUT(10,2,10),SIRA(68),NIRPI,NID,NDA30HOL  280
     2 ,NKER                                                            HOL  290
C                                                                       HOL  300
      COMMON /SPKZ/ DUM(29),MUP,EMU(31)                                 HOL  310
C                                                                       HOL  320
CFZJ031                                                       28.05.04  HOL  330
CFZJ062                                                       04.05.11  HOL  340
      COMMON /ORIGEN/ LOB,NOR,VOR(100),ISPK,KFISS,N200C,NXSC,KGR,       HOL  350
     1 LOBN,IEZ,FSP(33),IDOP,JNEU(15),LMAT(15),N34,JPROV(15)            HOL  360
C                                                                       HOL  370
CFZJ055                                                       25.09.07  HOL  380
CFZJ046                                                       26.09.06  HOL  390
C                                                                       HOL  400
      CHARACTER*9 CIDTHER,THERM1/'therm1515'/,THERM2/'therm1516'/,      HOL  410
     1 THERM3/'therm1517'/,THERM4/'therm1518'/,THERM5/'therm1519'/      HOL  420
C                                                                       HOL  430
      EQUIVALENCE(JTPE2,N5),(JTPE3,N6),(JTPE4,NT2)                      HOL  440
C                                                                       HOL  450
   35 FORMAT (/39H       THERMAL SHORT LIBRARY - THERMOS:/)             HOL  460
  100 FORMAT (///'   WRONG THERMOS-LIBRARY. IDT =',I6,'. **STOP** ')    HOL  470
  101 FORMAT (///' MATERIAL NO.',I6,'    IS NOT ON LIBRARY     **STOP**'HOL  480
     1 )                                                                HOL  490
CFZJ046                                                       26.09.06  HOL  500
  102 FORMAT (3X,I6,4X,18A4,',  ADIS=',E12.5)                           HOL  510
  104 FORMAT (6E12.5)                                                   HOL  520
  105 FORMAT (12I6)                                                     HOL  530
  106 FORMAT (E12.5,10I6)                                               HOL  540
  107 FORMAT (A9,3X,10I6)                                               HOL  550
  361 FORMAT (' ... JSATZ ',I4,' ON DATA SET ',I4,' STARTS ON RECORD NO'HOL  560
     1 ,I6,' ... ')                                                     HOL  570
  501 FORMAT (4I10)                                                     HOL  580
  502 FORMAT (4E14.6)                                                   HOL  590
  503 FORMAT (18A4)                                                     HOL  600
C                                                                       HOL  610
C                                                                       HOL  620
C     VORBEREITUNGEN                                                    HOL  630
C                                                                       HOL  640
      IDTHER = 0                                                        HOL  650
      TKELV = 273.                                                      HOL  660
      NUCT = 0                                                          HOL  670
      IX = 30                                                           HOL  680
      IY = IX                                                           HOL  690
      KBLIND = 0                                                        HOL  700
      NSAEFL = 0                                                        HOL  710
      NEUSPK = 0                                                        HOL  720
      ID = KANN                                                         HOL  730
C                                                                       HOL  740
CARD T1                                                                 HOL  750
C                                                                       HOL  760
      READ (N5,107) CIDTHER,NKER,NKERAB,NUTTE,NUCT,ITY,MUP              HOL  770
C                                                                       HOL  780
      IF(CIDTHER .EQ. THERM1) OPEN(NT2,FILE='Libraries\therm1515')      HOL  790
      IF(CIDTHER .EQ. THERM2) OPEN(NT2,FILE='Libraries\therm1516')      HOL  800
      IF(CIDTHER .EQ. THERM3) OPEN(NT2,FILE='Libraries\therm1517')      HOL  810
      IF(CIDTHER .EQ. THERM4) OPEN(NT2,FILE='Libraries\therm1518')      HOL  820
      IF(CIDTHER .EQ. THERM5) OPEN(NT2,FILE='Libraries\therm1519')      HOL  830
      IF(NUTTE .EQ. 0) GOTO 1002                                        HOL  840
      IF(CIDTHER .EQ. THERM1) IDTHER = 1515                             HOL  850
      IF(CIDTHER .EQ. THERM2) IDTHER = 1516                             HOL  860
      IF(CIDTHER .EQ. THERM3) IDTHER = 1517                             HOL  870
      IF(CIDTHER .EQ. THERM4) IDTHER = 1518                             HOL  880
      IF(CIDTHER .EQ. THERM5) IDTHER = 1519                             HOL  890
 1002 CONTINUE                                                          HOL  900
C                                                                       HOL  910
CARD T2                                                                 HOL  920
C                                                                       HOL  930
      IF(MUP .GT. 0) READ (N5,104) (EMU(I),I=1,MUP)                     HOL  940
C                                                                       HOL  950
      IF(ITY .EQ. 0) ITY = 1                                            HOL  960
      IF(IDTHER .LE. 0) GOTO 1001                                       HOL  970
      ID = IDTHER                                                       HOL  980
      NUTTE = 1                                                         HOL  990
 1001 CONTINUE                                                          HOL 1000
      KANN = ID                                                         HOL 1010
      IF(ID .EQ. 0) GOTO 99                                             HOL 1020
      NABS = KMAT - NKER                                                HOL 1030
      IF(NKER .EQ. 0) GOTO 5                                            HOL 1040
      DO 1 K=1,100                                                      HOL 1050
        JKER(K) = 0                                                     HOL 1060
    1 CONTINUE                                                          HOL 1070
      KKER = 0                                                          HOL 1080
      MINKER = 1000                                                     HOL 1090
      MAXKER = 99999                                                    HOL 1100
      NCL = NUCT                                                        HOL 1110
      IF(NCL .EQ. 0) NCL = 1                                            HOL 1120
      KBLIND = 0                                                        HOL 1130
      DO 2 I=1,NKER                                                     HOL 1140
C                                                                       HOL 1150
CARD T3                                                                 HOL 1160
C                                                                       HOL 1170
        READ (N5,105) (IKER(I,J),J=1,NCL)                               HOL 1180
C                                                                       HOL 1190
CARD T4                                                                 HOL 1200
C                                                                       HOL 1210
        READ (N5,104) (TCELS(I,J),J=1,NXS)                              HOL 1220
C                                                                       HOL 1230
    2 CONTINUE                                                          HOL 1240
      DO 4 K=1,100                                                      HOL 1250
        JKER(K) = MAXKER                                                HOL 1260
        DO 3 I=1,NKER                                                   HOL 1270
          DO 3 J=1,NCL                                                  HOL 1280
            IF(IKER(I,J) .GT. MINKER) JKER(K) = MIN0(JKER(K),IKER(I,J)) HOL 1290
    3   CONTINUE                                                        HOL 1300
        IF(JKER(K) .EQ. MAXKER) GOTO 5                                  HOL 1310
        MINKER = JKER(K)                                                HOL 1320
        KKER = KKER + 1                                                 HOL 1330
    4 CONTINUE                                                          HOL 1340
    5 CONTINUE                                                          HOL 1350
      IF(KBLIND .LE. 0) GOTO 7                                          HOL 1360
      DO 6 K=1,KBLIND                                                   HOL 1370
        JKER(KKER+K) = JKER(KKER)                                       HOL 1380
    6 CONTINUE                                                          HOL 1390
      KKER = KKER + KBLIND                                              HOL 1400
    7 CONTINUE                                                          HOL 1410
C                                                                       HOL 1420
CARD T5                                                                 HOL 1430
C                                                                       HOL 1440
      IF (NKERAB .GT. 0) READ (N5,106) TKG,(IDTA(I),I=1,NKERAB)         HOL 1450
C                                                                       HOL 1460
      JSATZ = 16                                                        HOL 1470
      IF (JAD10(JSATZ) .EQ. 0) JAD10(JSATZ) = JSUM10                    HOL 1480
      NXT10 = JAD10(JSATZ)                                              HOL 1490
      WRITE (N6,361) JSATZ,NDA10,NXT10                                  HOL 1500
C                                                                       HOL 1510
C     LESEN UND SCHREIBEN DES VORSPANN                                  HOL 1520
C                                                                       HOL 1530
      READ (NT2,501) IDT,NAB                                            HOL 1540
      IF(IDT .EQ. ID) GOTO 10                                           HOL 1550
      WRITE (N6,100) IDT                                                HOL 1560
      STOP                                                              HOL 1570
   10 CONTINUE                                                          HOL 1580
      READ (NT2,502) (V(I),DV(I),I=1,IX)                                HOL 1590
      READ (NT2,501) (MG(I),I=1,IX)                                     HOL 1600
      DO 11 I=1,IX                                                      HOL 1610
        VV(I) = V(I)**2                                                 HOL 1620
        VDV(I) = V(I) * DV(I)                                           HOL 1630
   11 CONTINUE                                                          HOL 1640
      EM = (V(IX)+DV(IX)/2.)**2                                         HOL 1650
      EMA = (V(IX)-DV(IX)/2.)**2                                        HOL 1660
      NIS = NABS + KKER                                                 HOL 1670
      NSTR10 = 0                                                        HOL 1680
      NSTRDL = 0                                                        HOL 1690
      NST10  = NXT10                                                    HOL 1700
      MERK2 = 0                                                         HOL 1710
      DO 12 K=1,KKER                                                    HOL 1720
        TCLIB(K) = 0.                                                   HOL 1730
        JDL10(K) = 0                                                    HOL 1740
   12 CONTINUE                                                          HOL 1750
      WRITE (NDA10,REC=NXT10) NSTR10,NSTRDL,MERK2,KKER,(TCLIB(K),       HOL 1760
     1 JDL10(K),K=1,KKER),NUCT                                          HOL 1770
      NXT10 = NXT10 + 1                                                 HOL 1780
      WRITE (NDA10,REC=NXT10) IX,(V(I),DV(I),VV(I),VDV(I),MG(I),I=1,IX),HOL 1790
     1 NABS,NKER,NCL,((IKER(I,J),I=1,NKER),J=1,NCL),(JKER(K),K=1,KKER), HOL 1800
     2 NIS,TKELV                                                        HOL 1810
      NXT10 = NXT10 + 1                                                 HOL 1820
C                                                                       HOL 1830
C     DIE EINZELNEN NUKLIDE                                             HOL 1840
C                                                                       HOL 1850
      IDT = 0                                                           HOL 1860
      NKE = 0                                                           HOL 1870
      WRITE (N6,35)                                                     HOL 1880
      IF(IDOP .GT. 0) REWIND N34                                        HOL 1890
      DO 29 N=1,NIS                                                     HOL 1900
        DO 70 I=1,IX                                                    HOL 1910
          DO 70 J=1,IX                                                  HOL 1920
            PP(I,J) = 0.                                                HOL 1930
   70   CONTINUE                                                        HOL 1940
        G = 0.                                                          HOL 1950
        NIDT(N) = 0                                                     HOL 1960
        IDAP = 999                                                      HOL 1970
        IMA = 0                                                         HOL 1980
        DO 26 I=1,NABS                                                  HOL 1990
          IMA = MAX0(IMA,IMAT(I))                                       HOL 2000
          IF(IDT .GE. IMAT(I)) GOTO 26                                  HOL 2010
          IDAP = MIN0(IDAP,IMAT(I))                                     HOL 2020
          IF(IDAP .EQ. IMAT(I)) II = I                                  HOL 2030
   26   CONTINUE                                                        HOL 2040
   21   CONTINUE                                                        HOL 2050
        IF(IDT .GE. NAB) GOTO 27                                        HOL 2060
        READ (NT2,501) IDT                                              HOL 2070
        READ (NT2,503) (DID(I),I=1,18)                                  HOL 2080
        READ (NT2,502) DFO,UB                                           HOL 2090
        READ (NT2,502) (XA(I),I=1,IX)                                   HOL 2100
        READ (NT2,502) (XS(I),I=1,IX)                                   HOL 2110
        READ (NT2,502) (XF(I),I=1,IX)                                   HOL 2120
        READ (NT2,502) (XT(I),I=1,IX)                                   HOL 2130
        READ (NT2,502) ADIS,HXS                                         HOL 2140
        IDT0 = IDT                                                      HOL 2150
        GOTO 36                                                         HOL 2160
   27   CONTINUE                                                        HOL 2170
        IF(IDOP .EQ. 0) GOTO 24                                         HOL 2180
        IF(IDT .GE. IMA) GOTO 24                                        HOL 2190
        L0 = 0                                                          HOL 2200
   59   CONTINUE                                                        HOL 2210
        DO 60 J=1,IDOP                                                  HOL 2220
          IF(JPROV(J) .EQ. IDT0) L0 = J                                 HOL 2230
   60   CONTINUE                                                        HOL 2240
        IDT0 = IDT0 + 1                                                 HOL 2250
        IF(L0 .EQ. 0) GOTO 59                                           HOL 2260
        REWIND N34                                                      HOL 2270
        DO 61 J=1,L0                                                    HOL 2280
          READ (N34) IDT,(DID(I),I=1,18),DFO,UB,(XA(I),I=1,IX),         HOL 2290
     1     (XS(I),I=1,IX),(XF(I),I=1,IX),(XT(I),I=1,IX),ADIS,HXS        HOL 2300
   61   CONTINUE                                                        HOL 2310
        GOTO 122                                                        HOL 2320
   36   IF(IDT-IDAP) 21,122,23                                          HOL 2330
  122   CONTINUE                                                        HOL 2340
        IF(IDOP .EQ. 0) GOTO 31                                         HOL 2350
        DO 30 J=1,IDOP                                                  HOL 2360
          IF(LMAT(J) .NE. IDT) GOTO 30                                  HOL 2370
          WRITE (N34) JNEU(J),(DID(I),I=1,18),DFO,UB,(XA(I),I=1,IX),    HOL 2380
     1     (XS(I),I=1,IX),(XF(I),I=1,IX),(XT(I),I=1,IX),ADIS,HXS        HOL 2390
   30   CONTINUE                                                        HOL 2400
   31   CONTINUE                                                        HOL 2410
        DO 71 IJ=1,IX                                                   HOL 2420
          PP(IJ,IJ) = XS(IJ) * V(IJ)                                    HOL 2430
   71   CONTINUE                                                        HOL 2440
        IF(NKERAB .EQ. 0) GOTO 22                                       HOL 2450
        DO 123 I=1,NKERAB                                               HOL 2460
          IF(IDT .NE. IDTA(I)) GOTO 123                                 HOL 2470
          DO 124 J=1,IX                                                 HOL 2480
            IF(V(J) .LT. 1.) GOTO 124                                   HOL 2490
            XXS = XS(J)                                                 HOL 2500
C                                                                       HOL 2510
            CALL BASK(PP,XXS,TKG,ADIS,XS)                               HOL 2520
C                                                                       HOL 2530
            G = TKG                                                     HOL 2540
            GOTO 22                                                     HOL 2550
  124     CONTINUE                                                      HOL 2560
  123   CONTINUE                                                        HOL 2570
        GOTO 22                                                         HOL 2580
   23   CONTINUE                                                        HOL 2590
        WRITE (N6,101) IDAP                                             HOL 2600
        STOP                                                            HOL 2610
   24   CONTINUE                                                        HOL 2620
        IDAP = 9999                                                     HOL 2630
        NKE = NKE + 1                                                   HOL 2640
        IF(IDT .LT. JKER(NKE)) IDAP = JKER(NKE)                         HOL 2650
        II = 0                                                          HOL 2660
        DFO = 0.                                                        HOL 2670
        DO 128 I=1,IX                                                   HOL 2680
          XF(I) = 0.                                                    HOL 2690
  128   CONTINUE                                                        HOL 2700
        IF(IDT .EQ. JKER(NKE)) GOTO 25                                  HOL 2710
   28   CONTINUE                                                        HOL 2720
        READ (NT2,501) IDT                                              HOL 2730
        READ (NT2,503) (DID(I),I=1,18)                                  HOL 2740
        READ (NT2,502) G,HXS,UB                                         HOL 2750
        READ (NT2,502) (XA(I),I=1,IX)                                   HOL 2760
        READ (NT2,502) (XS(I),I=1,IX)                                   HOL 2770
        READ (NT2,502) ((PP(I,J),J=1,IX),I=1,IX)                        HOL 2780
CFZJ046                                                       26.09.06  HOL 2790
        READ (NT2,502) (XT(I),I=1,IX)                                   HOL 2800
        READ (NT2,502) ADIS                                             HOL 2810
CFZJ046                                                       26.09.06  HOL 2820
        IF(IDT-IDAP) 28,22,23                                           HOL 2830
C                                                                       HOL 2840
C     VERBUCHEN DER DATEN AUF DER NEUEN SHORT LIBRARY                   HOL 2850
C                                                                       HOL 2860
   22   CONTINUE                                                        HOL 2870
        ALPH = ((ADIS-1.)/(ADIS+1.))**2                                 HOL 2880
        DO 750 I=1,IX                                                   HOL 2890
          SP(I) = 0.                                                    HOL 2900
  750   CONTINUE                                                        HOL 2910
        IF(EMA-ALPH*EM) 760,760,770                                     HOL 2920
  760   SP(IX) = HXS / (DV(IX)*(ADIS +0.667))                           HOL 2930
        GOTO 820                                                        HOL 2940
  770   DO 790 I=1,IX                                                   HOL 2950
          A = V(I) * (1./EM-ALPH/VV(I))                                 HOL 2960
          IF(A) 790,790,780                                             HOL 2970
  780     SP(I) = HXS * A / (1.-ALPH)                                   HOL 2980
  790   CONTINUE                                                        HOL 2990
  820   CONTINUE                                                        HOL 3000
   25   CONTINUE                                                        HOL 3010
        IF(NKE .EQ. 1)  NSTR10 = NXT10                                  HOL 3020
        IF(NKE .LE. 0) GOTO 33                                          HOL 3030
        TCLIB(NKE) = G - TKELV                                          HOL 3040
        JDL10(NKE) = NXT10                                              HOL 3050
   33   CONTINUE                                                        HOL 3060
CFZJ046                                                       26.09.06  HOL 3070
        WRITE (NDA10,REC=NXT10) N,IDT,IY,II,(XA(I),I=1,IX),             HOL 3080
     1   (XS(I),I=1,IX),(SP(I),I=1,IX),((PP(I,J),J=1,IY),I=1,IY),       HOL 3090
     2   (XF(I),I=1,IX),(XT(I),I=1,IX),DFO,UB,G                         HOL 3100
        NXT10 = NXT10 + 1                                               HOL 3110
CFZJ046                                                       26.09.06  HOL 3120
        WRITE (N6,102) IDT,(DID(I),I=1,18),ADIS                         HOL 3130
        NIDT(N) = IDT                                                   HOL 3140
        IF(NKE .EQ. 1) NSTRDL = NXT10 - NSTR10                          HOL 3150
        IF(NKE .GT. 0 .AND. N .LE. KMAT) NIDT(N) = 1000 + NKE           HOL 3160
   29 CONTINUE                                                          HOL 3170
      IXY = 5 * IX + IY**2 + 3                                          HOL 3180
      MERK2 = NXT10                                                     HOL 3190
      DO 34 N=1,NKER                                                    HOL 3200
        WRITE (NDA10,REC=NXT10) N,IDT,IY,II,(G,I=1,IXY)                 HOL 3210
        NXT10 = NXT10 + 1                                               HOL 3220
   34 CONTINUE                                                          HOL 3230
      IF(JSUM10 .LT. NXT10) JSUM10 = NXT10                              HOL 3240
      NXT10 = NST10                                                     HOL 3250
      WRITE (NDA10,REC=NXT10) NSTR10,NSTRDL,MERK2,KKER,(TCLIB(K),       HOL 3260
     1 JDL10(K),K=1,KKER),NUCT                                          HOL 3270
      NXT10 = NXT10 + 1                                                 HOL 3280
C                                                                       HOL 3290
      CALL TERINE(NIDT,ITY,NTYSP,VF,IDOP)                               HOL 3300
C                                                                       HOL 3310
   99 CONTINUE                                                          HOL 3320
      IF(IDOP .GT. 0) REWIND N34                                        HOL 3330
      RETURN                                                            HOL 3340
      END                                                               HOL 3350
      SUBROUTINE TERINE(NIDT,ITY,NTYSP,VF,IDOP)                         TER   10
C                                                                       TER   20
C     THERMOS - INPUT - EINTOPF                                         TER   30
C                                                                       TER   40
      DIMENSION NTBL(20),VB(10),VC(10),RED(11),VVH(10),RO(20),RN(20),   TER   50
     1 RI(20),BETA(21),FNA(100),FNB(100),COA(10),FRACT(15),NRMAF(15),   TER   60
     2 DEN(15),NFT(30),NISEL(50),SFT(50,30),SFMU(31),NTYSP(NXS),        TER   70
     3 NIDT(KMAT+55),VF(KMAT,10),IDSELF(200)                            TER   80
C                                                                       TER   90
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    TER  100
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    TER  110
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PITER  120
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 TER  130
C                                                                       TER  140
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), TER  150
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10TER  160
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11TER  170
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         TER  180
C                                                                       TER  190
      COMMON /BLOCRT/ ID,NBER,IX,MX,NX,ICOAT,MY,IBRENN,SS96(96,10),     TER  200
     1 F(20,30),MTBL(20),VOL(20),V(30),DV(30),VV(30),VDV(30),MG(30),    TER  210
     2 COZ(30,2),GIM(30,10),ALBEDO(30),LEAKT                            TER  220
C                                                                       TER  230
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300)                      TER  240
C                                                                       TER  250
      COMMON /SPKZ/ DU(29),MUP,EMU(31)                                  TER  260
C                                                                       TER  270
CFZJ055                                                       25.09.07  TER  280
C                                                                       TER  290
      EQUIVALENCE(JTPE2,N5),(JTPE3,N6)                                  TER  300
C                                                                       TER  310
    6 FORMAT (/' THERMOS-FUELTYPE',I2,':  SSF-SET',I3,'  NUKLID  =',I4/)TER  320
    8 FORMAT (I5,2E12.5)                                                TER  330
   91 FORMAT (//' *** ERROR: ABSOLUTE VALUE OF VARIABLE IDISO (=',I4,' ,TER  340
     1 INPUT CARD T10) EXCEEDS HIGHEST NUMBER OF THERMOS-LIBRARY. ***') TER  350
   92 FORMAT (I6,6F6.0)                                                 TER  360
  100 FORMAT (12I6)                                                     TER  370
  101 FORMAT (6E12.5)                                                   TER  380
  102 FORMAT (20I1,2I2,4E12.5)                                          TER  390
  103 FORMAT (I5,I4,I1,10F6.3)                                          TER  400
  104 FORMAT (I6,I9,5X,9E12.5)                                          TER  410
  105 FORMAT ('1THERMOS-INPUT-HOT-POT:'//' CELL DESIGN NUMBER:',I3,',  GTER  420
     1EOMETRY:',I2,', CELL VOLUME FRACTION',G13.6,',  T/T0=',G13.6,',  (TER  430
     2DATA-2 FUEL TYPE =',I3,')'//)                                     TER  440
  106 FORMAT (' CELL ZONE DEFINITION'//'      MESHPOINT:',20I3/'      ZOTER  450
     1NE     :',20I3)                                                   TER  460
  107 FORMAT (/' RADII :',9G13.5)                                       TER  470
  108 FORMAT ('1LIB-NR.  ID.NO. OF     FRACTION OF NUCLIDE DENSITIES IN TER  480
     1ZONES:'/11X,'SSF-SET',I9,8(I12))                                  TER  490
  109 FORMAT (//' THE MATERIALS IN ZONE',I2,' ARE CONTAINED IN A COATED TER  500
     1PARTICLE MATRIX'/' RADIUS OF KERNEL      ',G13.6/' RADIUS OF PARTITER  510
     2CLE    ',G13.6/' FILLING FACTOR        ',G13.6/' DENSITY MATRIX/COTER  520
     3ATING',G13.6/)                                                    TER  530
  110 FORMAT (23X,'(LAST COLUMN IS THE FRACTION OF ZONE',I3,' CONTAINED TER  540
     1IN KERNEL OF COATED PARTICLE)')                                   TER  550
  111 FORMAT (' ')                                                      TER  560
  361 FORMAT (' ... JSATZ ',I4,' ON DATA SET ',I4,' STARTS ON RECORD NO'TER  570
     1 ,I6,' ... ')                                                     TER  580
  900 FORMAT (' RI,RN,RO,VOL...',4E13.6)                                TER  590
 2090 FORMAT (/30X,'ISOTROPIC BOUNDARY'//' (I,ALBEDO(I)):'/)            TER  600
 2092 FORMAT (/30X,'WHITE BOUNDARY  I.E. ALBEDO=1')                     TER  610
 2095 FORMAT (10(I4,F8.5))                                              TER  620
 9001 FORMAT (/' *** LM =',I6,' (IN GROUP ',I2,') MUST NEVER EXCEED MUP TER  630
     1=',I6,' (CARD T1).  STOP.')                                       TER  640
C                                                                       TER  650
C                                                                       TER  660
      JSATZ = 17                                                        TER  670
      MUPO = 0                                                          TER  680
      IF (JAD10(JSATZ) .EQ. 0) JAD10(JSATZ) = JSUM10                    TER  690
      NXT10 = JAD10(JSATZ)                                              TER  700
      WRITE (N6,361) JSATZ,NDA10,NXT10                                  TER  710
C                                                                       TER  720
CARD T6                                                                 TER  730
C                                                                       TER  740
      READ (N5,100) NBER,(NTYSP(I),I=1,NXS)                             TER  750
C                                                                       TER  760
      IF(NBER .EQ. 0) GOTO 99                                           TER  770
      DO 10 K=1,10                                                      TER  780
        COA(K) = 0.0                                                    TER  790
   10 CONTINUE                                                          TER  800
CFZJ012   New identification numbers for THERMOS-cell         02.12.03  TER  810
C         definitions for the spectrum zones                  02.12.03  TER  820
      WRITE (NDA10,REC=NXT10) NBER,(NTYSP(I),I=1,NXS)                   TER  830
      NXT10 = NXT10 + 1                                                 TER  840
      NFU0 = 0                                                          TER  850
      DO 50 NBE=1,NBER                                                  TER  860
        NFU = 0                                                         TER  870
        NVR = 0                                                         TER  880
C                                                                       TER  890
CARD T7                                                                 TER  900
C                                                                       TER  910
        READ (N5,92) NGEOM,TKG,FUELL,FUTYP,STRT0,PNORM,TLEAK            TER  920
C                                                                       TER  930
CFZJ015                                                       09.12.03  TER  940
C       IF(NGEOM .EQ. 0) ITY = 0                                        TER  950
        IF(FUTYP .LE. 0.) NFU = NBE                                     TER  960
        IF(TLEAK .EQ. 0.) TLEAK = 1.                                    TER  970
        LEAKT = IFIX(TLEAK)                                             TER  980
        NSTT0 = IFIX(STRT0)                                             TER  990
        NORMP = IFIX(PNORM)                                             TER 1000
        IF(FUELL .EQ. 0.) FUELL = 1.                                    TER 1010
C                                                                       TER 1020
CARD T8                                                                 TER 1030
C                                                                       TER 1040
        READ (N5,102) (MTBL(K),K=1,20),IBRENN,ICOAT,(COA(K),K=1,4)      TER 1050
C                                                                       TER 1060
        IF(IBRENN .EQ. 0) IBRENN = 1                                    TER 1070
        DO 11 K=1,20                                                    TER 1080
          IF(MTBL(K) .LE. 0) GOTO 12                                    TER 1090
          MX = MTBL(K)                                                  TER 1100
          NX = K                                                        TER 1110
   11   CONTINUE                                                        TER 1120
   12   CONTINUE                                                        TER 1130
        IF(NORMP .LT. 0) NORMP = NX                                     TER 1140
        RED(1) = 0.                                                     TER 1150
        J = 0                                                           TER 1160
        IF(FUTYP .LE. 0.) GOTO 51                                       TER 1170
        IFT = IFIX(FUTYP)                                               TER 1180
        NXT29 = 2                                                       TER 1190
        READ (NDA29,REC=NXT29) N30,(NFT(I),I=1,N30)                     TER 1200
        NXT29 = NXT29 + 1                                               TER 1210
        DO 18 I=1,N30                                                   TER 1220
          II = I                                                        TER 1230
          IF((NFT(I)/100) .EQ. IFT) GOTO 19                             TER 1240
   18   CONTINUE                                                        TER 1250
   19   CONTINUE                                                        TER 1260
        NXT29 = II                                                      TER 1270
        READ (NDA29,REC=NXT29) NISO,(NRMAF(I),DEN(I),I=1,NISO),         TER 1280
     1   (COA(K),K=1,4),(RED(K+1),K=1,3),NZEUG,MXP1,MXZEUG,             TER 1290
     2   (FRACT(I),I=1,MXZEUG),(DUM,I=1,14),NFU,NVR,(DUM,I=1,3)         TER 1300
        NXT29 = NXT29 + 1                                               TER 1310
        DO 20 K=1,MX                                                    TER 1320
          IF(FRACT(K) .GT. 0.) IBRENN = K                               TER 1330
   20   CONTINUE                                                        TER 1340
        ICOAT = IBRENN                                                  TER 1350
        IF(RED(2) .GT. 0.) GOTO 52                                      TER 1360
        DO 17 K=2,MXP1                                                  TER 1370
          RED(K) = RED(K+1)                                             TER 1380
   17   CONTINUE                                                        TER 1390
        GOTO 52                                                         TER 1400
   51   CONTINUE                                                        TER 1410
C                                                                       TER 1420
CARD T9                                                                 TER 1430
C                                                                       TER 1440
        READ (N5,101) (RED(K+1),K=1,MX)                                 TER 1450
C                                                                       TER 1460
   52   CONTINUE                                                        TER 1470
        IF(ITY .NE. NBE) GOTO 55                                        TER 1480
        IF(MX .GT. 2) J = 1                                             TER 1490
        PRO(170) = FUELL                                                TER 1500
        PRO(171) = RED(1+J)                                             TER 1510
        PRO(172) = RED(2+J)                                             TER 1520
        PRO(173) = RED(3+J)                                             TER 1530
CFZJ017 Delete unused VARIABLE BK                             12.12.03  TER 1540
        PRO(300) = 0.                                                   TER 1550
        MUHU(5) =  0                                                    TER 1560
   55   CONTINUE                                                        TER 1570
        IF(FUTYP .GT. 0.) WRITE (N6,105) NBE,NGEOM,FUELL,TKG,NFU        TER 1580
        IF(FUTYP .LE. 0.) WRITE (N6,105) NBE,NGEOM,FUELL,TKG,NFU0       TER 1590
        WRITE (N6,106) (K,K=1,20),(MTBL(K),K=1,20)                      TER 1600
        WRITE (N6,107) (RED(K+1),K=1,MX)                                TER 1610
        IF(ICOAT .GT. 0) WRITE (N6,109) ICOAT,(COA(K),K=1,4)            TER 1620
        NEXP = NGEOM + 2                                                TER 1630
        RD = FUELL / RED(MX+1)**NEXP                                    TER 1640
        DO 13 M=1,MX                                                    TER 1650
          VVH(M) = (RED(M+1)**NEXP-RED(M)**NEXP) * RD                   TER 1660
   13   CONTINUE                                                        TER 1670
        DO 11113 N=1,NX                                                 TER 1680
          NTBL(N) = MTBL(N)                                             TER 1690
11113   CONTINUE                                                        TER 1700
        N1 = 0                                                          TER 1710
        NR = 1                                                          TER 1720
        RIP = 0.                                                        TER 1730
        NY = NX                                                         TER 1740
        DO 16 N=1,NY                                                    TER 1750
          N2 = N1 + 1                                                   TER 1760
          IF(N1 .GE. N) GOTO 15                                         TER 1770
          N1 = N2                                                       TER 1780
          NR = NTBL(N1)                                                 TER 1790
          DO 14 K=N1,NY                                                 TER 1800
            IF(NTBL(K) .EQ. NR) KK = K                                  TER 1810
            NP = KK                                                     TER 1820
   14     CONTINUE                                                      TER 1830
          KK = NP                                                       TER 1840
          NP = KK - N1 + 1                                              TER 1850
          N1 = KK                                                       TER 1860
          TH = RED(NR+1) - RED(NR)                                      TER 1870
          DT = FLOAT(NP)                                                TER 1880
          IF(N .EQ. 1) DT = DT - 0.5                                    TER 1890
          DT = TH / DT                                                  TER 1900
   15     CONTINUE                                                      TER 1910
          RN(N) = RIP + DT * 0.5                                        TER 1920
          RO(N) = RIP + DT                                              TER 1930
          IF(N .EQ. 1) RO(N) = RN(N)                                    TER 1940
          IF(N .EQ. 1) RN(N) = 0.                                       TER 1950
          VOL(N) = PI * (RO(N)**NEXP-RIP**NEXP)                         TER 1960
          IF(NGEOM .EQ. 1) VOL(N) = VOL(N) * 4. / 3.                    TER 1970
          RI(N) = RIP                                                   TER 1980
          RIP = RO(N)                                                   TER 1990
   16   CONTINUE                                                        TER 2000
        WRITE (N6,900) (RI(N),RN(N),RO(N),VOL(N),N=1,NX)                TER 2010
        MY = MX                                                         TER 2020
        IF(ICOAT .GT. 0) MY = MY + 1                                    TER 2030
        DO 30 K=1,MY                                                    TER 2040
          VC(K) = 0.                                                    TER 2050
   30   CONTINUE                                                        TER 2060
        JU = 0                                                          TER 2070
        IDISO = 0                                                       TER 2080
        MINKER = 1000                                                   TER 2090
        NKE = 0                                                         TER 2100
        KMA = KMAT                                                      TER 2110
        LI = 0                                                          TER 2120
        LT = 0                                                          TER 2130
        LC = 0                                                          TER 2140
        LL = 0                                                          TER 2150
        DO 40 NN=1,KMAT                                                 TER 2160
          N = NN                                                        TER 2170
          IDSELF(N) = 0                                                 TER 2180
          IF(NIDT(N) .LT. 1000) GOTO 38                                 TER 2190
          IF(NKE .EQ. 0) NKE = N                                        TER 2200
          MAXKER = 9999                                                 TER 2210
          DO 39 NK=NKE,KMA                                              TER 2220
            IF(NIDT(NK) .LE. MINKER .OR. NIDT(NK) .GE. MAXKER) GOTO 39  TER 2230
            MAXKER = NIDT(NK)                                           TER 2240
            N = NK                                                      TER 2250
   39     CONTINUE                                                      TER 2260
          MINKER = MAXKER                                               TER 2270
   38     CONTINUE                                                      TER 2280
          IF(IDISO .GE. NIDT(N) .OR. JU .GT. 0) GOTO 31                 TER 2290
C                                                                       TER 2300
CARD T10                                                                TER 2310
C                                                                       TER 2320
          READ (N5,103) IDISO,MUPO,JT,(VB(K),K=1,MY)                    TER 2330
C                                                                       TER 2340
          IF(IABS(IDISO) .LE. 190 .OR. IABS(IDISO) .GT. 1000 .OR. IDOP  TER 2350
     1     .GT. 0) GOTO 90                                              TER 2360
          WRITE (N6,91) IABS(IDISO)                                     TER 2370
          STOP                                                          TER 2380
   90     CONTINUE                                                      TER 2390
          IF(MUPO .LE. 0 .AND. JT .LE. 0) LL = 0                        TER 2400
          IF(MUPO .LE. 0) GOTO 5                                        TER 2410
C                                                                       TER 2420
CARD T11                                                                TER 2430
C                                                                       TER 2440
          READ (N5,101) (SFMU(K),K=1,MUP)                               TER 2450
C                                                                       TER 2460
          MUP1 = MUP + 1                                                TER 2470
          SFMU(MUP1) = 1.                                               TER 2480
          EMU(MUP1) = 2.05                                              TER 2490
          IF(EMU(MUP) .GE. EMU(MUP1)) MUP1 = MUP                        TER 2500
          LI = LI + 1                                                   TER 2510
          IF(JT .EQ. 0) LL = LI                                         TER 2520
          CT = 0.0253                                                   TER 2530
          LM = 0                                                        TER 2540
          V1 = 0.                                                       TER 2550
          E1 = 0.                                                       TER 2560
          LT = 0                                                        TER 2570
          EM1 = 0.                                                      TER 2580
          NISEL(LI) = IABS(IDISO)                                       TER 2590
    1     CONTINUE                                                      TER 2600
          IF(LT .GE. IX) GOTO 9                                         TER 2610
          LT = LT + 1                                                   TER 2620
          V1 = V1 + DV(LT)                                              TER 2630
          E0 = E1                                                       TER 2640
          E1 = CT * V1**2.                                              TER 2650
          DE = E1 - E0                                                  TER 2660
          SFT(LI,LT) = 0.                                               TER 2670
    9     CONTINUE                                                      TER 2680
          IF(EM1 .EQ. 0.) GOTO 2                                        TER 2690
          IF(E0 .LT. EM1) GOTO 3                                        TER 2700
    2     CONTINUE                                                      TER 2710
          LM = LM + 1                                                   TER 2720
          EM0 = EM1                                                     TER 2730
          EM1 = EMU(LM)                                                 TER 2740
    3     CONTINUE                                                      TER 2750
          DMI = AMIN1(EM1,E1) - AMAX1(EM0,E0)                           TER 2760
          RAT = DMI / DE                                                TER 2770
          IF(LM .LE. MUP1) GOTO 9000                                    TER 2780
          WRITE (6,9001) LM,LT,MUP1                                     TER 2790
          STOP                                                          TER 2800
 9000     CONTINUE                                                      TER 2810
          SFT(LI,LT) = SFT(LI,LT) + SFMU(LM) * RAT                      TER 2820
          IF(LT .GE. IX .AND. LM .GE. MUP1) GOTO 4                      TER 2830
          IF(E1-EM1-1.E-7) 1,1,2                                        TER 2840
    4     CONTINUE                                                      TER 2850
          WRITE (N6,6) NBE,LI,NISEL(LI)                                 TER 2860
          DVS = 0.                                                      TER 2870
          DO 7 K=1,LT                                                   TER 2880
            DVS = DVS + DV(K)                                           TER 2890
            WRITE (N6,8) K,DVS,SFT(LI,K)                                TER 2900
    7     CONTINUE                                                      TER 2910
    5     CONTINUE                                                      TER 2920
          IF(VB(1) .GE. 0.) GOTO 54                                     TER 2930
          IFRACT = -IFIX(VB(1))                                         TER 2940
          IFRACT = (IFRACT-1) * MXP1                                    TER 2950
          DO 53 K=1,MXP1                                                TER 2960
            VB(K) =FRACT(K+IFRACT)                                      TER 2970
   53     CONTINUE                                                      TER 2980
   54     CONTINUE                                                      TER 2990
          IF(IDISO .GT. 0) GOTO 31                                      TER 3000
          IDISO = IABS(IDISO)                                           TER 3010
          JU = 1                                                        TER 3020
   31     CONTINUE                                                      TER 3030
          IF(IDISO .NE. NIDT(N)) GOTO 35                                TER 3040
          IF(JT .EQ. 0) GOTO 33                                         TER 3050
          JT = 0                                                        TER 3060
          IF(MUPO .GT. 0) IDSELF(N) = LI                                TER 3070
          DO 32 K=1,MY                                                  TER 3080
            VF(N,K) = VB(K)                                             TER 3090
   32     CONTINUE                                                      TER 3100
          GOTO 37                                                       TER 3110
   33     CONTINUE                                                      TER 3120
          LC = LL                                                       TER 3130
          DO 34 K=1,MY                                                  TER 3140
            VC(K) = VB(K)                                               TER 3150
   34     CONTINUE                                                      TER 3160
   35     CONTINUE                                                      TER 3170
          IDSELF(N) = LC                                                TER 3180
          DO 36 K=1,MY                                                  TER 3190
            VF(N,K) = VC(K)                                             TER 3200
   36     CONTINUE                                                      TER 3210
   37     CONTINUE                                                      TER 3220
   40   CONTINUE                                                        TER 3230
        LI = MAX0(1,LI)                                                 TER 3240
        LT = MAX0(1,LT)                                                 TER 3250
        WRITE (N6,108) (N,N=1,MX)                                       TER 3260
        IF(ICOAT .GT. 0) WRITE (N6,110) ICOAT                           TER 3270
        WRITE (N6,111)                                                  TER 3280
        DO 41 N=1,KMAT                                                  TER 3290
          WRITE (N6,104) NIDT(N),IDSELF(N),(VF(N,K),K=1,MY)             TER 3300
   41   CONTINUE                                                        TER 3310
        DH = .05                                                        TER 3320
        CA = 1.0                                                        TER 3330
        X = 0.                                                          TER 3340
        DO 260 L=1,100                                                  TER 3350
          X = X + DH                                                    TER 3360
          CB = EXP(-X)                                                  TER 3370
          IF(NGEOM .NE. 1) CB = CB / (1.+X*(.454891+X*(-.155068+X*      TER 3380
     1     (.5191562E-1+X*(-.9179453E-2+X*.6300736E-3)))))              TER 3390
          FNB(L) = (CB-CA) / DH                                         TER 3400
          FNA(L) = CA - FNB(L) * (X-DH)                                 TER 3410
          CA = CB                                                       TER 3420
  260   CONTINUE                                                        TER 3430
        BETAD = PI / 20.                                                TER 3440
        IF(NGEOM .NE. 1) GOTO 1003                                      TER 3450
        BETA(1) = BETAD / 2.                                            TER 3460
        DO 1004 L=2,20                                                  TER 3470
          BETA(L) = BETA(L-1) + BETAD                                   TER 3480
 1004   CONTINUE                                                        TER 3490
        GOTO 1005                                                       TER 3500
 1003   CONTINUE                                                        TER 3510
        BETA(1) = 0.                                                    TER 3520
        BETA(2) = PI                                                    TER 3530
        BETA(3) = BETAD                                                 TER 3540
        DO 270 L=4,21                                                   TER 3550
          BETA(L) = BETA(L-1) + BETA(3)                                 TER 3560
  270   CONTINUE                                                        TER 3570
 1005   CONTINUE                                                        TER 3580
C                                                                       TER 3590
C     WHITE AND ISOTROPIC BOUNDARY WITH ALBEDO. NEEF,SCHERER (KFA)      TER 3600
C                                                                       TER 3610
CFZJ060                                                       26.07.10  TER 3620
        IF(LEAKT.GT.0) GOTO 1130                                        TER 3630
        WRITE (N6,2090)                                                 TER 3640
C                                                                       TER 3650
CARD T12                                                                TER 3660
C                                                                       TER 3670
        READ (N5,101) (ALBEDO(I),I=1,6)                                 TER 3680
C                                                                       TER 3690
        IF(ALBEDO(2) .NE. 0.0) GOTO 1112                                TER 3700
        DO 1113 I=2,IX                                                  TER 3710
          ALBEDO(I) = ALBEDO(1)                                         TER 3720
 1113   CONTINUE                                                        TER 3730
        GOTO 1114                                                       TER 3740
 1112   CONTINUE                                                        TER 3750
C                                                                       TER 3760
CARD T12 (CONTINUATION)                                                 TER 3770
C                                                                       TER 3780
        READ (N5,101) (ALBEDO(I),I=7,IX)                                TER 3790
C                                                                       TER 3800
 1114   CONTINUE                                                        TER 3810
        WRITE (N6,2095) (I,ALBEDO(I),I=1,IX)                            TER 3820
        GOTO 1150                                                       TER 3830
CFZJ060                                                       26.07.10  TER 3840
 1130   CONTINUE                                                        TER 3850
        WRITE (N6,2092)                                                 TER 3860
        DO 1135 I=1,IX                                                  TER 3870
          ALBEDO(I) = 1.0                                               TER 3880
 1135   CONTINUE                                                        TER 3890
        LEAKT = -1                                                      TER 3900
 1150   CONTINUE                                                        TER 3910
        NBX = NBE                                                       TER 3920
        IF(LT .LE. 1) GOTO 1151                                         TER 3930
        NBX = -NBX                                                      TER 3940
CFZJ012   New identification numbers for THERMOS-cell         02.12.03  TER 3950
C         definitions for the spectrum zones                  02.12.03  TER 3960
CFZJ034                                                       31.08.04  TER 3970
        WRITE (NDA10,REC=NXT10) NBX,NGEOM,TKG,NX,MX,MY,MTBL,BETA,VVH,   TER 3980
     1   IBRENN,ICOAT,COA,FNA,FNB,RI,RN,RO,VOL,DH,NORMP,NFU,NSTT0,LEAKT,TER 3990
     2   ALBEDO,LI,LT,(IDSELF(N),N=1,KMAT),((SFT(L,J),J=1,LT),L=1,LI)   TER 4000
        NXT10 = NXT10 + 1                                               TER 4010
C                                                                       TER 4020
        CALL WRDA(IWRITE,NDA10,NXT10,L10,VF,KMAT*10)                    TER 4030
C                                                                       TER 4040
        GOTO 50                                                         TER 4050
 1151   CONTINUE                                                        TER 4060
CFZJ012   New identification numbers for THERMOS-cell         02.12.03  TER 4070
C         definitions for the spectrum zones                  02.12.03  TER 4080
CFZJ034                                                       31.08.04  TER 4090
        WRITE (NDA10,REC=NXT10) NBX,NGEOM,TKG,NX,MX,MY,MTBL,BETA,VVH,   TER 4100
     1   IBRENN,ICOAT,COA,FNA,FNB,RI,RN,RO,VOL,DH,NORMP,NFU,NSTT0,LEAKT,TER 4110
     2   ALBEDO                                                         TER 4120
        NXT10 = NXT10 + 1                                               TER 4130
C                                                                       TER 4140
        CALL WRDA(IWRITE,NDA10,NXT10,L10,VF,KMAT*10)                    TER 4150
C                                                                       TER 4160
   50 CONTINUE                                                          TER 4170
C                                                                       TER 4180
CARD T13                                                                TER 4190
C                                                                       TER 4200
CFZJ017 Delete unused VARIABLE PRO(300)                       12.12.03  TER 4210
      IF(ITY .EQ. -1) READ(N5,101) (PRO(I),I=170,173)                   TER 4220
C                                                                       TER 4230
      IF(JSUM10 .LT. NXT10) JSUM10 = NXT10                              TER 4240
   99 CONTINUE                                                          TER 4250
      RETURN                                                            TER 4260
      END                                                               TER 4270
      SUBROUTINE BASK(P,XXS,T,ADIS,XS)                                  BAS   10
C                                                                       BAS   20
      DIMENSION XS(30),X(30),E(30),AT(10),AKT(10),AMT(10),P(30,30)      BAS   30
C                                                                       BAS   40
      COMMON /BLOCRT/ ID,NBER,IX,MX,NX,ICOAT,MY,IBRENN,SS96(96,10),     BAS   50
     1 F(20,30),MTBL(20),VOL(20),V(30),DV(30),VV(30),VDV(30),MG(30),    BAS   60
     2 COZ(30,2),GIM(30,10)                                             BAS   70
C                                                                       BAS   80
C                                                                       BAS   90
      NISO = 1                                                          BAS  100
      AT(1) = XXS                                                       BAS  110
      AKT(1) = 0.                                                       BAS  120
      AMT(1) = ADIS                                                     BAS  130
      DO 15 I=1,IX                                                      BAS  140
        XS(I) = 0.                                                      BAS  150
        DO 15 J=1,IX                                                    BAS  160
          P(I,J) = 0.                                                   BAS  170
   15 CONTINUE                                                          BAS  180
      DO 500 N=1,NISO                                                   BAS  190
        IF(AT(N)) 110,500,110                                           BAS  200
  110   AM = AMT(N)                                                     BAS  210
        BETS = AM / T                                                   BAS  220
        BET = SQRT(BETS)                                                BAS  230
        DO 120 I=1,IX                                                   BAS  240
          X(I) = BET * V(I)                                             BAS  250
          E(I) = X(I) * X(I)                                            BAS  260
  120   CONTINUE                                                        BAS  270
        TAUS = BETS / (BETS+AKT(N))                                     BAS  280
        TAU = SQRT(TAUS)                                                BAS  290
        ALPHA = AKT(N) * TAUS / BETS                                    BAS  300
        THETA = (AM+1.) / (2.*AM*TAU)                                   BAS  310
        ZETA = TAU - THETA                                              BAS  320
        OMEG = TAUS * (BETS+(AM+1.)*AKT(N)) / BETS                      BAS  330
        CONST = (AT(N)*TAU*TAUS*(AM+1.)*(AM+1.)) / (4.*AM*OMEG)         BAS  340
        DO 400 I=1,IX                                                   BAS  350
          DO 300 J=I,IX                                                 BAS  360
            X1 = THETA * X(I) + ZETA * X(J)                             BAS  370
            X2 = THETA * X(I) - ZETA * X(J)                             BAS  380
C                                                                       BAS  390
            ER1 = 1. - EIF(X1)                                          BAS  400
C                                                                       BAS  410
            ER2 = 1. - EIF(X2)                                          BAS  420
C                                                                       BAS  430
            IF(X1) 1,2,2                                                BAS  440
    1       ER1 = -ER1                                                  BAS  450
    2       IF(X2) 3,4,4                                                BAS  460
    3       ER2 = -ER2                                                  BAS  470
    4       EA = ER1 + ER2                                              BAS  480
C                                                                       BAS  490
            EB = EIF(THETA*X(J)+ZETA*X(I)) - EIF(THETA*X(J)-ZETA*X(I))  BAS  500
C                                                                       BAS  510
            WA = ALPHA * E(J)                                           BAS  520
            IF(WA-80.) 252,251,251                                      BAS  530
  251       WA = 0.                                                     BAS  540
            GOTO 253                                                    BAS  550
  252       WA = EXP(-WA)                                               BAS  560
  253       WB = (E(J)-OMEG*E(I)) / AM                                  BAS  570
            TEM = CONST * (X(I)/X(J)) * (EA*WA+EB*EXP(WB))              BAS  580
            IF(TEM-1.E-15) 350,350,270                                  BAS  590
  270       P(I,J) = P(I,J) + TEM                                       BAS  600
  300     CONTINUE                                                      BAS  610
  350     EA = TAU * X(I)                                               BAS  620
          WA = ALPHA * E(I)                                             BAS  630
          IF(WA-80.) 352,351,351                                        BAS  640
  351     WA = 0.                                                       BAS  650
          GOTO 353                                                      BAS  660
  352     WA = EXP(-WA)                                                 BAS  670
C                                                                       BAS  680
  353     EB = WA * (EA+(0.5/EA)) * (1.-EIF(EA))                        BAS  690
C                                                                       BAS  700
          IF(E(I)-80.) 355,354,354                                      BAS  710
  354     WB = 0.                                                       BAS  720
          GOTO 356                                                      BAS  730
  355     WB = EXP(-E(I))                                               BAS  740
  356     XS(I) = XS(I) + (AT(N)*TAUS*TAUS/BET) * (EB+0.5641896E0*WB)   BAS  750
  400   CONTINUE                                                        BAS  760
  500 CONTINUE                                                          BAS  770
      DO 610 I=1,IX                                                     BAS  780
        E(I) = (V(I)*V(I)) * EXP(-(V(I)*V(I))/T)                        BAS  790
  610 CONTINUE                                                          BAS  800
      DO 620 I=1,IX                                                     BAS  810
        DO 620 J=I,IX                                                   BAS  820
          P(J,I) = P(I,J) * E(J) / E(I)                                 BAS  830
  620 CONTINUE                                                          BAS  840
      DO 650 J=1,IX                                                     BAS  850
        TEM = 0.                                                        BAS  860
        DO 640 I=1,IX                                                   BAS  870
          TEM = TEM + P(I,J) * DV(I)                                    BAS  880
  640   CONTINUE                                                        BAS  890
        P(J,J) = ((XS(J)-TEM)/DV(J)) + P(J,J)                           BAS  900
  650 CONTINUE                                                          BAS  910
      DO 680 I=1,IX                                                     BAS  920
        XS(I) = XS(I) / V(I)                                            BAS  930
        DO 680 J=1,IX                                                   BAS  940
          P(I,J) = P(I,J) * DV(J)                                       BAS  950
  680 CONTINUE                                                          BAS  960
      RETURN                                                            BAS  970
      END                                                               BAS  980
      FUNCTION EIF(X)                                                   EIF   10
C                                                                       EIF   20
      B5 = 1.0614054                                                    EIF   30
      B4 = -1.4531520                                                   EIF   40
      B3 = 1.4214137                                                    EIF   50
      B2 = -0.28449674                                                  EIF   60
      B1 = 0.25482959                                                   EIF   70
      P = 0.32759110                                                    EIF   80
      SQRPIE = 1.7724539                                                EIF   90
      IF(X) 14,26,14                                                    EIF  100
   14 V = ABS(X)                                                        EIF  110
      IF(V-8.66) 16,16,24                                               EIF  120
   16 IF(V-1.7) 17,17,20                                                EIF  130
   17 ETAN = 1.0 / (1.0+P*V)                                            EIF  140
      GZ = ((((B5*ETAN+B4)*ETAN+B3)*ETAN+B2)*ETAN+B1) * ETAN            EIF  150
C                                                                       EIF  160
      EIF = GZ / EXP(X**2)                                              EIF  170
C                                                                       EIF  180
      GOTO 30                                                           EIF  190
   20 CFZ =1.0 / (V+1.0/(2.0*V+2.0/(V+3.0/(2.0*V+4.0/(V+5.0/(2.0*V+6.0/ EIF  200
     1 (V+7.0/(2.0*V+2.5))))))))                                        EIF  210
C                                                                       EIF  220
      EIF = CFZ / (SQRPIE*EXP(X**2))                                    EIF  230
C                                                                       EIF  240
      GOTO 30                                                           EIF  250
C                                                                       EIF  260
   24 EIF = 0.                                                          EIF  270
C                                                                       EIF  280
      GOTO 30                                                           EIF  290
C                                                                       EIF  300
   26 EIF = 1.                                                          EIF  310
C                                                                       EIF  320
   30 RETURN                                                            EIF  330
      END                                                               EIF  340