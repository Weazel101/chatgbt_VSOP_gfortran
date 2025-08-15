      SUBROUTINE VORSHU(NPRINT,IN1,MRY,NKEEP,DECAF,ICONPO,VOLREG,VOL,DENORS   10
     1 ,DENIOD,HM,THBURN,FADOS3,NOPOW,HMETAL,TCHG2,KRESHZ,LRZN,TCHG1,   ORS   20
     2 N240,NALT,NTYP1,TEMZUT,TCELS,VERA,IHZ,VOLIHZ,VOLWEG,HMETAV,DAV,  ORS   30
     3 DA2,SCELS,SEMZUT,IREG,JAD11,IBOX,RVCB,HPOS,NTYSP,IWATER)         ORS   40
C                                                                       ORS   50
CFZJ012   New identification numbers for THERMOS-cell         02.12.03  ORS   60
C         definitions for the spectrum zones                            ORS   70
C                                                                       ORS   80
C     F U M A N   MANAGES FUELLING OPERATIONS                           ORS   90
C                                                                       ORS  100
C     VORSHU MEANS: BEFORE SHUFFLING                                    ORS  110
C                   ----------------                                    ORS  120
C                                                                       ORS  130
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    ORS  140
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    ORS  150
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIORS  160
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 ORS  170
C                                                                       ORS  180
      EQUIVALENCE(JTPE2,NS),(JTPE3,NT)                                  ORS  190
C                                                                       ORS  200
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1ORS  210
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         ORS  220
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    ORS  230
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)ORS  240
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  ORS  250
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    ORS  260
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         ORS  270
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,ORS  280
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            ORS  290
C                                                                       ORS  300
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, ORS  310
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        ORS  320
     2 FIMAKL(20),NOPILE,MREP,MARX(10),NAJB(10),FOJB(10),NFUL(10),NBTOT,ORS  330
     3 NB0,NCY                                                          ORS  340
C                                                                       ORS  350
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), ORS  360
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10ORS  370
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11ORS  380
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         ORS  390
C                                                                       ORS  400
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300),FABC(10),REPC(10),NNEORS  410
     1 ,LISTEQ,NTIK                                                     ORS  420
C                                                                       ORS  430
CFZJ062                                                       04.05.11  ORS  440
      COMMON /ORIGEN/ LOB,NOR,VOR(100)                                  ORS  450
C                                                                       ORS  460
CFZJ035                                                       14.09.04  ORS  470
CFZJ063                                                       26.07.11  ORS  480
      COMMON /GRENZE/ NURTHM,THERGR,IDGAM,IDZUT,IDTHER,NGAM,IDESIN,JJGM,ORS  490
     1 MSTU,MGHUS,NNOBG,NZT(10,9),IZUT(10,2,10),SIRA(68),NIRPI,NID,NDA30ORS  500
     2 ,NKER,ITEMP(20)                                                  ORS  510
C                                                                       ORS  520
      COMMON /SPECTI/ ITIK(10)                                          ORS  530
C                                                                       ORS  540
CFZJ006 enlarged dimensions common QVAR                       28.11.03  ORS  550
      COMMON /QVAR/ IVAR,ENDKEF,QVOLLL,QREDUZ,QREMAX,EPQ,EPC,DQDDC,DELC,ORS  560
     1 DCN,SBU,TE(4,300),TA(300),N61,URZ,ZLEKA,ABXEN,TI(300),DQCMAX,    ORS  570
     2 PSPALT,JRESTW,JRESTR,JREST,TAU,Q0,QMI,QMA,QRAT,DMOD,DAEM,        ORS  580
     3 DUMM(305),NJ                                                     ORS  590
C                                                                       ORS  600
      COMMON /OPT/ KENN,IOPUT,NIFKO,DZT,KONIN,NI                        ORS  610
C                                                                       ORS  620
      COMMON /MPUNKT/ IMPU,KMPU,EMPU,TTTEIN,TTTAUS                      ORS  630
C                                                                       ORS  640
CFZJ010 Increase dimensions in COMMON ANISO                   01.12.03  ORS  650
      COMMON /ANISO/ JH,IZONE(200),DU(1920),KK,M1(201,4)                ORS  660
C                                                                       ORS  670
CFZJ044                                                       26.09.05  ORS  680
      COMMON /IFA/ FA0,FA1,FA2,IEND                                     ORS  690
C                                                                       ORS  700
CFZJ055                                                       25.09.07  ORS  710
C                                                                       ORS  720
      COMMON /DECAYT/ DECY(30)                                          ORS  730
C                                                                       ORS  740
      COMMON /FLUXN/ D(361),IACT                                        ORS  750
C                                                                       ORS  760
CFZJ035                                                       14.09.04  ORS  770
CFZJ063                                                       26.07.11  ORS  780
      COMMON /RI/ SM(10,9,2),IZUT1(10,9,10),IZUT2(10,9,10),SMC          ORS  790
C                                                                       ORS  800
      COMMON /IPO/ IPRINO,INAK,LNAK                                     ORS  810
C                                                                       ORS  820
      COMMON /BUC/ BU(6,200)                                            ORS  830
C                                                                       ORS  840
CFZJ061                                                       29.3.11   ORS  850
      COMMON /SPKVOL/ RV(1500)                                          ORS  860
C                                                                       ORS  870
      COMMON /PDTX/ PDTHX                                               ORS  880
C                                                                       ORS  890
CFZJ042                                                       09.09.05  ORS  900
      COMMON /IWAGA/ IWAGAM                                             ORS  910
C                                                                       ORS  920
CFZJ012                                                       02.12.03  ORS  930
CFZJ042                                                       09.09.05  ORS  940
      DIMENSION VOLREG(NDR),VOL(N200),DEN(KMAT,N200),DENIOD(N200),      ORS  950
     1 HM(N200),THBURN(N200),FADOS3(N200),NOPOW(N200),HMETAL(N200),     ORS  960
     2 TCHG2(N200),KRESHZ(N200),LRZN(N200),TCHG1(N240),NALT(N240),      ORS  970
     3 NTYP1(N240),TEMZUT(NXS),TCELS(5,NXS),VERA(N200),IHZ(NDR),        ORS  980
     4 VOLIHZ(NDR),VOLWEG(NDR),HMETAV(N200),DAV(KMAT),DA2(KMAT),        ORS  990
     5 SCELS(NXS),SEMZUT(NXS),IREG(NDR),JAD11(JD11),IBOX(MBOX),         ORS 1000
     6 RVCB(MBOX),DD(30),DC(30),NTYSP(NXS)                              ORS 1010
C                                                                       ORS 1020
      CHARACTER*4 BU                                                    ORS 1030
C                                                                       ORS 1040
    5 FORMAT (2I6,2E12.5)                                               ORS 1050
CFZJ012                                                       02.12.03  ORS 1060
CFZJ044                                                       26.09.05  ORS 1070
   11 FORMAT (6I3,3X,3I3,6I2,2X,9I2,I10)                                ORS 1080
   12 FORMAT (18I4)                                                     ORS 1090
   13 FORMAT (3I4,2E12.5)                                               ORS 1100
  114 FORMAT (//' FOR THE NEXT CYCLE THE TEMPERATURES OF THE RESONANCE AORS 1110
     1BSORBERS AND SCATTERING NUCLIDES HAVE BEEN CHANGED:'/)            ORS 1120
  115 FORMAT (6E12.5)                                                   ORS 1130
  116 FORMAT (//' REPROCESSING MIXTURE:',I10,9I8)                       ORS 1140
  117 FORMAT (' FRACTIONS OF JUMBLE BOXES:',F6.2,9F8.2)                 ORS 1150
  118 FORMAT (//' FOR THE NEXT CYCLE THE TEMPERATURES OF THE RESONANCE AORS 1160
     1BSORBERS HAVE BEEN CHANGED FROM SPECTRUM ZONES',I4,' -',I4,':'/)  ORS 1170
  119 FORMAT (//' FOR THE NEXT CYCLE THE TEMPERATURES OF THE SCATTERING ORS 1180
     1NUCLIDE',I2,' HAVE BEEN CHANGED FROM SPECTRUM ZONES',I4,' -',I4,':ORS 1190
     2 '/)                                                              ORS 1200
  120 FORMAT (//' BEFORE CHANGING THE TEMPERATURES OF THE RESONANCE ABSOORS 1210
     1RBERS THE AV. TEMP. OVER THE SPECTRUM ZONES IS:',E12.5,' (C)')    ORS 1220
  121 FORMAT (//' AFTER CHANGING THE TEMPERATURES OF THE RESONANCE ABSORORS 1230
     1BERS THE AV. TEMP. OVER THE SPECTRUM ZONES IS:',E12.5,' (C)'/)    ORS 1240
  122 FORMAT (//' BEFORE CHANGING THE TEMPERATURES OF THE SCATTERING NUCORS 1250
     1LIDE',I2,' THE AV. TEMP. OVER THE SPECTRUM ZONES IS:',E12.5,' (C)'ORS 1260
     2 )                                                                ORS 1270
  123 FORMAT (//' AFTER CHANGING THE TEMPERATURES OF THE SCATTERING NUCLORS 1280
     1IDE',I2,' THE AV. TEMP. OVER THE SPECTRUM ZONES IS:',E12.5,' (C)'/ORS 1290
     2 )                                                                ORS 1300
  334 FORMAT (/////' ********** PROGRAM STOPS, BECAUSE MAXIMUM NUMBER OFORS 1310
     1 FUEL CYCLES (MMAF ON INPUT CARD V1) IS EXCEEDED **********')     ORS 1320
  500 FORMAT ('0'//5('.'),' FOR OUT-OF-PILE BATCHES DECAY IS CALCULATED ORS 1330
     1FOR OUT-OF-PILE STORAGE TIME   TSTORE ',G12.5,5('.')///)          ORS 1340
  501 FORMAT (I3,5X,E12.5,4(3X,E12.5))                                  ORS 1350
  502 FORMAT ('0'//5('.'),' FOR THE IN-PILE BATCHES DECAY IS CALCULATED ORS 1360
     1FOR THE DOWNTIME FOR REFUELLING TDOWN ',G12.5,5('.')///)          ORS 1370
  504 FORMAT (/ ' MATERIAL',18X,'CONCENTRATION')                        ORS 1380
  505 FORMAT ('0********************************************************ORS 1390
     1*****************************************************************'ORS 1400
     2 /30X,' END OF OPERATING CYCLE',I4,5X,' RELOAD NO', I4,' IS EXECUTORS 1410
     3ED '/1X,121('*')/)                                                ORS 1420
  506 FORMAT (//' OPTIONS FOR NEXT CYCLE  :'/' IPRIN(1)=',I3,',IPRIN(2)=ORS 1430
     1',I3,',IPRIN(3)=',I3,',IPRIN(4)=',I3,',JNSTOP=',I3,',LIB=',I3,',NPORS 1440
     2RINT=',I3,',IBUCK=',I3,',MUHU(3)=',I3/' DELDAY=',E12.5,',POWER=', ORS 1450
     3 E12.5,',ZKFIND=',E12.5,',TDOWN=',E12.5/)                         ORS 1460
  507 FORMAT (3I4,3E12.5,4E6.0)                                         ORS 1470
  511 FORMAT ('1','BATCHES BEFORE RELOAD NO ',I3//' BATCH   TOT.VOLUME (ORS 1480
     1CCM)   HEAVY METAL (GR)      FIMA       FUEL TYPE     IRRADIATION ORS 1490
     2AGE       FDOSE'/)                                                ORS 1500
CFZJ059                                                       04.11.09  ORS 1510
  512 FORMAT (1H ,4A4,8X,E12.5)                                         ORS 1520
  513 FORMAT (I5,6X,G12.5,8X,G12.5,3X,E12.5,6X,I2,10X,E12.5,5X,E12.5)   ORS 1530
  599 FORMAT ('1')                                                      ORS 1540
  601 FORMAT (/' ON DIRECT-ACCESS UNIT',I3,' - ',I4,' SETS WITH CONCENTRORS 1550
     1ATIONS ARE WRITTEN.'/)                                            ORS 1560
  603 FORMAT (18I4)                                                     ORS 1570
  604 FORMAT (12I6)                                                     ORS 1580
  605 FORMAT (2E12.5,I6)                                                ORS 1590
  700 FORMAT (////' OUT OF PILE BATCHES BEFORE RELOAD NO.',I5//' TYP    ORS 1600
     1    VOLUME        AVG.AGE       AVG.BURNUP     AVG.F-DOSE      HM-ORS 1610
     2GRAM'/)                                                           ORS 1620
  710 FORMAT (//' ***CAUTION: THE INTERMEDIATE BOX',I4,' WITH VOL. =',  ORS 1630
     1 E12.5,' WILL BE DELETED.'/14X,'NO CONSIDERATION OF THIS BOX IN CAORS 1640
     2LCULATION OF COSTS.***'//)                                        ORS 1650
  720 FORMAT (////' FROM RELOAD NO.',I5,' THE FOLLOWING STORAGE BOXES ARORS 1660
     1E PREPARED.'/' THEY ARE AVAILABLE AT THIS RELOAD NO.',I5,' (DECAY ORS 1670
     2TIME IS NOT APPLIED).'//' BOX   TYPE      VOLUME      AVG.IRR.TIMEORS 1680
     3     AVG.BURNUP     AVG.F-DOSE   TOT.HEAVY METAL (GR)'/)          ORS 1690
  721 FORMAT (I3,I7,3X,E12.5,4X,E12.5,2(3X,E12.5),6X,E12.5)             ORS 1700
CFZJ012                                                        02.12.03 ORS 1710
  730 FORMAT (/100(12I6/))                                              ORS 1720
 9900 FORMAT (//' CELL TYPES CANNOT BE REDEFINED BEFORE ORIGINAL DEFINITORS 1730
     1ION HAS BEEN MADE!'//)                                            ORS 1740
 9901 FORMAT (//' THE NUMBER OF CELL DEFINITIONS ON INTERFACE FILE (',I2ORS 1750
     1 ,') DOES NOT AGREE WITH THAT ON CARD R7A (',I2')'//)             ORS 1760
C                                                                       ORS 1770
C                                                                       ORS 1780
      IF(TSTORE .LT. 0.0) TSTORE = -TXME(1) / AAAA                      ORS 1790
      TST = ABS(TSTORE)                                                 ORS 1800
      DELSTO = DELDAY * JNSTOP                                          ORS 1810
C                                                                       ORS 1820
C     NEW OPTIONS FOR NEXT CYCLE                                        ORS 1830
C                                                                       ORS 1840
      DO 29 I=1,3                                                       ORS 1850
        IVSP(I+24) = 0                                                  ORS 1860
        XVSP(I) = 0.                                                    ORS 1870
   29 CONTINUE                                                          ORS 1880
      XTDOWN = 0.                                                       ORS 1890
      KONIN = 0                                                         ORS 1900
      JREST = 0                                                         ORS 1910
      IF(ITIK(1) .GT. 0) GOTO 3                                         ORS 1920
      ITIK(10) = 0                                                      ORS 1930
      ITIK(9) = 0                                                       ORS 1940
      IT10 = 0                                                          ORS 1950
      EPQ = 1.                                                          ORS 1960
      QREMAX = 1.                                                       ORS 1970
      EM9 = 1.E-09                                                      ORS 1980
      IMPU = 0                                                          ORS 1990
      QRAT = 1.                                                         ORS 2000
      DAEM = 1.                                                         ORS 2010
      EMPU = 0.                                                         ORS 2020
      PDTHX = 0.                                                        ORS 2030
    3 CONTINUE                                                          ORS 2040
      IF(IVSP(1) .GE. 1) GOTO 21                                        ORS 2050
C                                                                       ORS 2060
CARD R7                                                                 ORS 2070
C                                                                       ORS 2080
CFZJ012                                                       02.12.03  ORS 2090
CFZJ030                                                       04.05.04  ORS 2100
CFZJ044                                                       26.09.05  ORS 2110
      READ (NS,11) (IVSP(I),I=1,6),(IVSP(I),I=9,11),IVSP(14),NKEEP,IK,  ORS 2120
     1 LK,NTIK,NNJ,IVOID,(IVSP(I),I=15,17),IVSP(19),IVSP(21),IVSP(23),  ORS 2130
     2 IWATER,IREDEF,IVSP(24)                                           ORS 2140
C                                                                       ORS 2150
      NI = 0                                                            ORS 2160
      MUHU(8) = 0                                                       ORS 2170
      IWAGAM = 0                                                        ORS 2180
      IF(IREDEF .NE. 0) THEN                                            ORS 2190
C                                                                       ORS 2200
CARD R7A                                                                ORS 2210
C                                                                       ORS 2220
        WRITE (6,*) ' REDEFINE IDENTIFICATION NUMBERS FOR THERMOS-CELL TORS 2230
     1YPES'                                                             ORS 2240
        READ (NS,'(12I6)') NBERT,(NTYSP(I),I=1,NXS)                     ORS 2250
C                                                                       ORS 2260
        WRITE (6,730) (NTYSP(I),I=1,NXS)                                ORS 2270
C                                                                       ORS 2280
        IF(JAD10(17) .EQ. 0) THEN                                       ORS 2290
          WRITE (NT,9900)                                               ORS 2300
          STOP                                                          ORS 2310
        ELSE                                                            ORS 2320
          NXT10T = JAD10(17)                                            ORS 2330
          READ (NDA10,REC=NXT10T) NBER                                  ORS 2340
          IF(NBER .NE. NBERT) THEN                                      ORS 2350
            WRITE (NT,9901) NBER,NBERT                                  ORS 2360
            STOP                                                        ORS 2370
          ENDIF                                                         ORS 2380
          WRITE (NDA10,REC=NXT10T) NBER,(NTYSP(I),I=1,NXS)              ORS 2390
        ENDIF                                                           ORS 2400
      ENDIF                                                             ORS 2410
CFZJ042                                                       09.09.05  ORS 2420
      IF(NNJ .EQ. 1) PDTHX = 1.                                         ORS 2430
      IF(PDTHX .GT. 0. .AND. IVSP(11) .LT. 1) IVSP(11) = 1              ORS 2440
      IVSP19Z = 0                                                       ORS 2450
      NJ = NNJ                                                          ORS 2460
      IF(IK .EQ. 3) INAK = 1                                            ORS 2470
      IF(LK .EQ. 3) LNAK = 1                                            ORS 2480
      IF(NNJ .EQ. 2) NJ = 3                                             ORS 2490
      IF(NNJ .EQ. 3) NJ = 2                                             ORS 2500
      IF(IVOID .LE. 0) GOTO 6                                           ORS 2510
C                                                                       ORS 2520
CARD R8                                                                 ORS 2530
C                                                                       ORS 2540
      READ (NS,12) JH,(IZONE(I),M1(I,1),M1(I,2),I=1,JH)                 ORS 2550
C                                                                       ORS 2560
      KK = JH + 1                                                       ORS 2570
      M1(KK,1) = 0                                                      ORS 2580
      IVOID = 0                                                         ORS 2590
    6 CONTINUE                                                          ORS 2600
      IVSP(20) = 100 * NI + 10 * NJ + NTIK                              ORS 2610
      IF(IVSP(20) .LT. 100) GOTO 4                                      ORS 2620
      KONIN = IVSP(20) / 100                                            ORS 2630
      IVSP(20) = IVSP(20) - 100 * KONIN                                 ORS 2640
    4 CONTINUE                                                          ORS 2650
      IF(IVSP(20) .LT. 10) GOTO 1                                       ORS 2660
      IT10 = IVSP(20) / 10                                              ORS 2670
      IF(IT10 .NE. 6) GOTO 9                                            ORS 2680
      ITIK(1) = 0                                                       ORS 2690
      ITIK(9) = 0                                                       ORS 2700
      IT10 = 5                                                          ORS 2710
      IVSP(20) = 51                                                     ORS 2720
    9 CONTINUE                                                          ORS 2730
      IVSP(20) = IVSP(20) - 10 * IT10                                   ORS 2740
      IF(IT10 .EQ. 0) GOTO 1                                            ORS 2750
      ITIK(10) = 1                                                      ORS 2760
      IF(IT10 .EQ. 1) GOTO 1                                            ORS 2770
      IF(IT10 .LE. 4) GOTO 2                                            ORS 2780
      IVAR = IT10 - 4                                                   ORS 2790
      IT10 = IT10 - 3                                                   ORS 2800
    2 CONTINUE                                                          ORS 2810
      ITIK(10) = 2                                                      ORS 2820
      IF(ITIK(9) .GT. 0) GOTO 1                                         ORS 2830
      ITIK(9) = IT10 - 5                                                ORS 2840
    1 CONTINUE                                                          ORS 2850
      MUHU(29) = 0                                                      ORS 2860
      MUHU(1) = IVSP(23)                                                ORS 2870
   21 CONTINUE                                                          ORS 2880
      NT = 6                                                            ORS 2890
      IVSP(1) = IVSP(1) - 1                                             ORS 2900
CFZJ042                                                       09.09.05  ORS 2910
      ITIK(4) = 0                                                       ORS 2920
      IPRIN(1) = IVSP(2)                                                ORS 2930
      IPRIN(2) = IVSP(3)                                                ORS 2940
      IPRIN(3) = IVSP(4)                                                ORS 2950
      IPRIN(4) = IVSP(10)                                               ORS 2960
      LIB = IVSP(21)                                                    ORS 2970
      L28 = 92                                                          ORS 2980
      IF(LIB .LT. 0) OPEN(28,ACCESS='DIRECT',RECL=L28*4,FILE='nucdens') ORS 2990
      IF(LIB .GT. 0) OPEN(64,FORM='UNFORMATTED',FILE='tinte')           ORS 3000
      NPRINT = IVSP(5)                                                  ORS 3010
      NTIK = IVSP(20)                                                   ORS 3020
      ITIK(6) = NTIK                                                    ORS 3030
      IF(NTIK .EQ. 3 .OR. NTIK .EQ. 1) ITIK(1) = 0                      ORS 3040
      NCYC = IVSP(14)                                                   ORS 3050
      NTYPE = 0                                                         ORS 3060
      IF(IVSP(11) .EQ. 2) NTYPE = 2                                     ORS 3070
      IVSP(14) = 0                                                      ORS 3080
      IN2 = IVSP(9)                                                     ORS 3090
      NKEEP = 100 * LK + 10 * IK + NKEEP                                ORS 3100
      IVSP(13) = NKEEP                                                  ORS 3110
      ISTORN = 0                                                        ORS 3120
      NKP = NKEEP - 10 * (NKEEP/10)                                     ORS 3130
      IF(NKP .EQ. 2) ISTORN = 1                                         ORS 3140
      IVSP(13) = 0                                                      ORS 3150
      LOB = 0                                                           ORS 3160
      IF(IK .EQ. 1) LOB = -1                                            ORS 3170
      PU = POWER                                                        ORS 3180
      HNUC = 0.                                                         ORS 3190
      CONPOI = 0.                                                       ORS 3200
      HPOS = 0.                                                         ORS 3210
      IVSP(28) = 1                                                      ORS 3220
C                                                                       ORS 3230
CARD R 9                                                                ORS 3240
C                                                                       ORS 3250
      IF(IVSP(15) .EQ. 1) READ (NS,507) (IVSP(I),I=25,27),(XVSP(I),I=1,3ORS 3260
     1 ),HNUC,HPOS,XTDOWN,CONPOI                                        ORS 3270
C                                                                       ORS 3280
      IF(IVSP(28) .GT. 0) NXE = 1                                       ORS 3290
      IF(CONPOI .LT. 0.) JSER = 1                                       ORS 3300
      ICONPO = IFIX(CONPOI)                                             ORS 3310
      IF(HNUC .GT. 0.) IVSP(29) = IFIX(HNUC)                            ORS 3320
      HNUC = 0.                                                         ORS 3330
      IF(XTDOWN .GT. 0.) TDOWN = XTDOWN                                 ORS 3340
      IF(XTDOWN .LT. 0.) TDOWN = 0.                                     ORS 3350
      IF(XVSP(1) .NE. 0.) DELDAY = XVSP(1)                              ORS 3360
      IF(XVSP(2) .NE. 0.) POWER = XVSP(2)                               ORS 3370
      IF(XVSP(3) .NE. 0.) ZKFIND = XVSP(3)                              ORS 3380
      IF(IVSP(25) .NE. 0) JNSTOP = IVSP(25)                             ORS 3390
      IF(IVSP(26) .NE. 0) JNUM = IVSP(26)                               ORS 3400
      IF(IVSP(24) .GT. 0) JNSTOP = -JNSTOP                              ORS 3410
      IVSP(24) = 0                                                      ORS 3420
      IVSP(15) = 0                                                      ORS 3430
      XVSP(2) = PU                                                      ORS 3440
      IF(IVSP(27) .GT. 0) MUHU(3) = IVSP(27)                            ORS 3450
      IF(NCYC .LE. 0) GOTO 42                                           ORS 3460
      IF(NCY .EQ. 0) NCYC = -NCYC                                       ORS 3470
      NCY = NCYC                                                        ORS 3480
C                                                                       ORS 3490
CARD R10                                                                ORS 3500
C                                                                       ORS 3510
      READ (NS,115) (FOJB(I),I=1,MREP)                                  ORS 3520
C                                                                       ORS 3530
      WRITE (NT,116) (I,I=1,MREP)                                       ORS 3540
      WRITE (NT,117) (FOJB(I),I=1,MREP)                                 ORS 3550
   42 CONTINUE                                                          ORS 3560
      IF(IVSP(16) .LE. 0) GOTO 43                                       ORS 3570
      IVSP(16) = 0                                                      ORS 3580
C                                                                       ORS 3590
CARD R11                                                                ORS 3600
C                                                                       ORS 3610
      READ (NS,603) (ISPEKT(I),I=1,18)                                  ORS 3620
C                                                                       ORS 3630
      ISPEKT(20) = 0                                                    ORS 3640
      DO 47 I=1,18                                                      ORS 3650
        IF(ISPEKT(I) .NE. 0) ISPEKT(20) = I                             ORS 3660
   47 CONTINUE                                                          ORS 3670
   43 CONTINUE                                                          ORS 3680
      IF(IVSP(17) .LE. 0) GOTO 49                                       ORS 3690
      IVSP(17) = 0                                                      ORS 3700
C                                                                       ORS 3710
CARD R12                                                                ORS 3720
C                                                                       ORS 3730
      READ (NS,603) (IDIFF(I),I=1,18)                                   ORS 3740
C                                                                       ORS 3750
      IDIFF(20) = 0                                                     ORS 3760
      DO 48 I=1,18                                                      ORS 3770
        IF(IDIFF(I) .NE. 0) IDIFF(20) = I                               ORS 3780
   48 CONTINUE                                                          ORS 3790
   49 CONTINUE                                                          ORS 3800
      IF(ITIK(6) .EQ. 5) GOTO 41                                        ORS 3810
      IF(NTIK .LT. 1 .OR. NTIK .GT. 2) GOTO 58                          ORS 3820
C                                                                       ORS 3830
CARD R13                                                                ORS 3840
C                                                                       ORS 3850
   41 READ (NS,603) (ITEMP(I),I=1,18)                                   ORS 3860
C                                                                       ORS 3870
      ITEMP(20) = 0                                                     ORS 3880
      DO 59 I=1,18                                                      ORS 3890
        IF(ITEMP(I) .NE. 0) ITEMP(20) = I                               ORS 3900
   59 CONTINUE                                                          ORS 3910
   58 CONTINUE                                                          ORS 3920
      IF(IVSP(19) .LE. 0) GOTO 54                                       ORS 3930
      IF(IVSP(19) .EQ. 2) GOTO 60                                       ORS 3940
C                                                                       ORS 3950
CARD R14                                                                ORS 3960
C                                                                       ORS 3970
      READ (NS,115) (SEMZUT(I),I=1,NXS)                                 ORS 3980
C                                                                       ORS 3990
      DO 56 I=1,NXS                                                     ORS 4000
        IF(SEMZUT(I) .GT. 0.) TEMZUT(I) = SEMZUT(I)                     ORS 4010
   56 CONTINUE                                                          ORS 4020
      WRITE (NT,114)                                                    ORS 4030
      WRITE (NT,115) (TEMZUT(I),I=1,NXS)                                ORS 4040
      DO 53 J=1,NKER                                                    ORS 4050
C                                                                       ORS 4060
CARD R15                                                                ORS 4070
C                                                                       ORS 4080
        READ (NS,115) (SCELS(I),I=1,NXS)                                ORS 4090
C                                                                       ORS 4100
        DO 57 I=1,NXS                                                   ORS 4110
          IF(SCELS(I) .GT. 0.) TCELS(J,I) = SCELS(I)                    ORS 4120
   57   CONTINUE                                                        ORS 4130
        WRITE (NT,115) (TCELS(J,I),I=1,NXS)                             ORS 4140
   53 CONTINUE                                                          ORS 4150
      IF(IVSP(19) .EQ. 1) GOTO 54                                       ORS 4160
   60 CONTINUE                                                          ORS 4170
CFZJ035                                                       14.09.04  ORS 4180
CFZJ063                                                       26.07.11  ORS 4190
      DO 61 I=1,IDESIN                                                  ORS 4200
        DO 61 J=1,9                                                     ORS 4210
C                                                                       ORS 4220
CARD R17                                                                ORS 4230
C                                                                       ORS 4240
          READ (NS,605) SM(I,J,1),SM(I,J,2),NZ                          ORS 4250
C                                                                       ORS 4260
CARDS R18                                                               ORS 4270
C                                                                       ORS 4280
          READ (NS,604) (IZUT1(I,J,M),M=1,NZ)                           ORS 4290
          READ (NS,604) (IZUT2(I,J,M),M=1,NZ)                           ORS 4300
C                                                                       ORS 4310
          WRITE (NT,604) NZ,(IZUT1(I,J,M),M=1,NZ)                       ORS 4320
          WRITE (NT,604) NZ,(IZUT2(I,J,M),M=1,NZ)                       ORS 4330
          NZT(I,J) = NZ                                                 ORS 4340
   61 CONTINUE                                                          ORS 4350
   54 CONTINUE                                                          ORS 4360
      IF(IVSP(19) .NE. -1) GOTO 70                                      ORS 4370
C                                                                       ORS 4380
CARD R16                                                                ORS 4390
C                                                                       ORS 4400
      IF(IVSP19Z .EQ. 0) READ (NS,13) NVAR,NXSA,NXSE,TVAR,TMIN          ORS 4410
CFZJ061                                                  29.3.11        ORS 4420
      DO 71 I=1,NDR                                                     ORS 4430
         RV(I) =VOLREG(I)                                               ORS 4440
   71 CONTINUE                                                          ORS 4450
C                                                                       ORS 4460
      IVSP19Z = 1                                                       ORS 4470
      IF(NVAR .GT. 1) GOTO 65                                           ORS 4480
      TV = 0.                                                           ORS 4490
      RVT = 0.                                                          ORS 4500
      DO 64 I=NXSA,NXSE                                                 ORS 4510
        TV = TV + TEMZUT(I) * RV(I)                                     ORS 4520
        RVT = RVT + RV(I)                                               ORS 4530
   64 CONTINUE                                                          ORS 4540
      TVAV = TV / RVT                                                   ORS 4550
      WRITE (NT,120) TVAV                                               ORS 4560
      DO 63 I=NXSA,NXSE                                                 ORS 4570
        TEMZUT(I) = TEMZUT(I) + TVAR                                    ORS 4580
        IF(TEMZUT(I) .LT. TMIN) TEMZUT(I) = TMIN                        ORS 4590
   63 CONTINUE                                                          ORS 4600
      WRITE (NT,118) NXSA,NXSE                                          ORS 4610
      WRITE (NT,115) (TEMZUT(I),I=1,NXS)                                ORS 4620
      TV = 0.                                                           ORS 4630
      DO 67 I=NXSA,NXSE                                                 ORS 4640
        TV = TV + TEMZUT(I) * RV(I)                                     ORS 4650
   67 CONTINUE                                                          ORS 4660
      TVAV = TV / RVT                                                   ORS 4670
      WRITE (NT,121) TVAV                                               ORS 4680
      GOTO 70                                                           ORS 4690
   65 CONTINUE                                                          ORS 4700
      TV = 0.                                                           ORS 4710
      RVT = 0.                                                          ORS 4720
      DO 68 I=NXSA,NXSE                                                 ORS 4730
        TV = TV + TCELS(NVAR-1,I) * RV(I)                               ORS 4740
        RVT = RVT + RV(I)                                               ORS 4750
   68 CONTINUE                                                          ORS 4760
      TVAV = TV / RVT                                                   ORS 4770
      WRITE (NT,122) NVAR-1,TVAV                                        ORS 4780
      DO 66 I=NXSA,NXSE                                                 ORS 4790
        TCELS(NVAR-1,I) = TCELS(NVAR-1,I) + TVAR                        ORS 4800
        IF(TCELS(NVAR-1,I) .LT. TMIN) TCELS(NVAR-1,I) = TMIN            ORS 4810
   66 CONTINUE                                                          ORS 4820
      WRITE (NT,119) NVAR-1,NXSA,NXSE                                   ORS 4830
      WRITE (NT,115) (TCELS(NVAR-1,I),I=1,NXS)                          ORS 4840
      TV = 0.                                                           ORS 4850
      DO 69 I=NXSA,NXSE                                                 ORS 4860
        TV = TV + TCELS(NVAR-1,I) * RV(I)                               ORS 4870
   69 CONTINUE                                                          ORS 4880
      TVAV = TV / RVT                                                   ORS 4890
      WRITE (NT,123) NVAR-1,TVAV                                        ORS 4900
   70 CONTINUE                                                          ORS 4910
      IF(IVSP(19) .NE. -1) IVSP(19) = 0                                 ORS 4920
      IF(IPRIN(3) .EQ. -2) NT = 4                                       ORS 4930
      INZW(1) = IN2                                                     ORS 4940
      IF(NPRINT .GE. 0) WRITE (NT,599)                                  ORS 4950
      IN1 = IPRIN(15) + 1                                               ORS 4960
      INZW(3) = IN1                                                     ORS 4970
      IF (IN1 .LE. MMAF) GOTO 333                                       ORS 4980
      WRITE (6,334)                                                     ORS 4990
      STOP                                                              ORS 5000
 333  CONTINUE                                                          ORS 5010
      WRITE (6,505) IN1,IN1                                             ORS 5020
      WRITE (6,506) (IPRIN(I),I=1,4),JNSTOP,LIB,NPRINT,IBUCK,MUHU(3),   ORS 5030
     1 DELDAY,POWER,ZKFIND,TDOWN                                        ORS 5040
C                                                                       ORS 5050
C     CALCULATE AVERAGE BATCH PROPERTIES BEFORE RELOAD                  ORS 5060
C                                                                       ORS 5070
      IF(NPRINT .GE. 1) WRITE (NT,511) IN1                              ORS 5080
      NCX2 = 0                                                          ORS 5090
      MRY = 0                                                           ORS 5100
      DECAF = 0.                                                        ORS 5110
      NDA28 = 28                                                        ORS 5120
      NXT28 = 1                                                         ORS 5130
      DO 191 IR=1,NRESHZ                                                ORS 5140
        NCX1 = NCX2 + 1                                                 ORS 5150
        NCX2 = KRESHZ(IR)                                               ORS 5160
        IF(IR .GT. 1 .AND. NPRINT .EQ. 2) WRITE (NT,599)                ORS 5170
        NALT(IR) = 1                                                    ORS 5180
        DO 55 M=1,KMAT                                                  ORS 5190
          DAV(M) = 0.0                                                  ORS 5200
   55   CONTINUE                                                        ORS 5210
        PARVOL = 0.0                                                    ORS 5220
        HMETAV(IR) = 0.0                                                ORS 5230
        HMGRM = 0.0                                                     ORS 5240
        BURNRX = 0.0                                                    ORS 5250
        DOSNRX = 0.0                                                    ORS 5260
        MRZ = 0                                                         ORS 5270
        IR1 = NCX1                                                      ORS 5280
   51   CONTINUE                                                        ORS 5290
        NCX = LRZN(IR1)                                                 ORS 5300
        DO 50 M=1,KMAT                                                  ORS 5310
          DAV(M) = DAV(M) + DEN(M,NCX) * VOL(NCX)                       ORS 5320
   50   CONTINUE                                                        ORS 5330
        HMETAV(IR) = HMETAV(IR) + HMETAL(NCX) * VOL(NCX)                ORS 5340
        HMGRM = HMGRM + HM(NCX)                                         ORS 5350
        BURNRX = BURNRX + THBURN(NCX) * VOL(NCX)                        ORS 5360
        DOSNRX = DOSNRX + FADOS3(NCX) * VOL(NCX)                        ORS 5370
        PARVOL = PARVOL + VOL(NCX)                                      ORS 5380
        MRZ = MRZ + 1                                                   ORS 5390
        IR1 = IR1 + 1                                                   ORS 5400
        IF(IR1 .LE. NCX2) GOTO 51                                       ORS 5410
        MRY = MAX0(MRY,MRZ)                                             ORS 5420
        VERA(IR) = PARVOL                                               ORS 5430
        HMETAV(IR) = HMETAV(IR) / PARVOL                                ORS 5440
        HMDEN = HMETAV(IR)                                              ORS 5450
        BURNRX = BURNRX / PARVOL                                        ORS 5460
        DOSNRX = DOSNRX / PARVOL                                        ORS 5470
        TCHG1(IR) = TCHG2(IR) + TXME(1)                                 ORS 5480
        IF(NPRINT .EQ. 2) WRITE (NT,504)                                ORS 5490
        HMEND = 0.                                                      ORS 5500
        DO 52 M=1,KMAT                                                  ORS 5510
          DAV(M) = DAV(M) / PARVOL                                      ORS 5520
          IF(M .LE. IACT) HMEND = HMEND + DAV(M)                        ORS 5530
          IF(NPRINT .NE. 2) GOTO 52                                     ORS 5540
CFZJ059                                                       04.11.09  ORS 5550
          WRITE (NT,512) (BU(N,M),N=1,4),DAV(M)                         ORS 5560
   52   CONTINUE                                                        ORS 5570
        IF(TDOWN .EQ. 0.0) GOTO 190                                     ORS 5580
C                                                                       ORS 5590
C     DECAY DURING RESHUFFLE PERIOD                                     ORS 5600
C                                                                       ORS 5610
        IF(NXE .LE. 0) DAV(IACT+1) = 0.                                 ORS 5620
        TDOWM = TDOWN                                                   ORS 5630
        IF(TDOWM .LT. 0.) TDOWM = ABS(TDOWN) * DELSTO                   ORS 5640
        TT = TDOWM * 3600. * 24.                                        ORS 5650
        DO 90 I=1,IACT                                                  ORS 5660
          DAV(I) = 0.                                                   ORS 5670
          TTT = TT * DECY(I)                                            ORS 5680
          IF(TTT .LE. 25.) DD(I) = EXP(-TTT)                            ORS 5690
          IF(TTT .GT. 25.) DD(I) = 0.                                   ORS 5700
   90   CONTINUE                                                        ORS 5710
        IR1 = NCX1                                                      ORS 5720
  160   CONTINUE                                                        ORS 5730
        NX = LRZN(IR1)                                                  ORS 5740
        IF(NXE .GT. 0) GOTO 155                                         ORS 5750
        DEC = EXP(-2.87E-05*TT)                                         ORS 5760
        DEN(IACT+1,NX) = (DEN(IACT+1,NX)-3.72727*DENIOD(NX)*            ORS 5770
     1   (EXP(-0.77E-05*TT)-1.0)) * EXP(-2.1E-05*TT)                    ORS 5780
        DENIOD(NX) = DENIOD(NX) * DEC                                   ORS 5790
  155   CONTINUE                                                        ORS 5800
        DO 95 I=1,IACT                                                  ORS 5810
          DC(I) = DEN(I,NX) * (1.-DD(I))                                ORS 5820
          II = I                                                        ORS 5830
          GOTO(91,91,92,92,91,91,91,91,91,91,93,91,93,91,93,93,93,91,91,ORS 5840
     1     91,93,91,91,94,91,91,91,93),II                               ORS 5850
   91     CONTINUE                                                      ORS 5860
          DEN(I,NX) = DEN(I,NX) * DD(I)                                 ORS 5870
          GOTO 99                                                       ORS 5880
   92     CONTINUE                                                      ORS 5890
          DEN(I,NX) = (DEN(I,NX)+DC(I-1)) * DD(I)                       ORS 5900
          GOTO 99                                                       ORS 5910
   93     CONTINUE                                                      ORS 5920
          DEN(I,NX) = (DEN(I,NX)+DC(I-3)) * DD(I)                       ORS 5930
          GOTO 99                                                       ORS 5940
   94     CONTINUE                                                      ORS 5950
          DEN(I,NX) = (DEN(I,NX)+DC(I-4)) * DD(I)                       ORS 5960
   99     CONTINUE                                                      ORS 5970
          DECAF = DECAF + DC(I) * VOL(NX)                               ORS 5980
          DAV(I) = DAV(I) + DEN(I,NX) * VOL(NX)                         ORS 5990
   95   CONTINUE                                                        ORS 6000
        IF(NXE .LE. 0) DAV(IACT+1) = DAV(IACT+1) + DEN(IACT+1,NX) *     ORS 6010
     1   VOL(NX)                                                        ORS 6020
        IR1 = IR1 + 1                                                   ORS 6030
        IF(IR1 .LE. NCX2) GOTO 160                                      ORS 6040
        DO 96 I=1,IACT                                                  ORS 6050
          DAV(I) = DAV(I) / PARVOL                                      ORS 6060
   96   CONTINUE                                                        ORS 6070
        IF(NXE .LE. 0) DAV(IACT+1) = DAV(IACT+1) / PARVOL               ORS 6080
  190   CONTINUE                                                        ORS 6090
C                                                                       ORS 6100
C     FIND BURNUP (FIMA-VALUE) FOR THIS REGION                          ORS 6110
C                                                                       ORS 6120
        IF(HMETAL(IR) .GT. 0.) GOTO 182                                 ORS 6130
        FIMA = 0.                                                       ORS 6140
        GOTO 183                                                        ORS 6150
  182   CONTINUE                                                        ORS 6160
        FIMA = (HMETAV(IR)-HMEND) / HMETAV(IR)                          ORS 6170
        IF(FIMA .LE. 0.) FIMA = EM9                                     ORS 6180
  183   CONTINUE                                                        ORS 6190
        NTP = NTYP1(IR)                                                 ORS 6200
        NTP1 = NTP - 1                                                  ORS 6210
        KL = 0                                                          ORS 6220
        IF(NTP1 .LE. 0) GOTO 175                                        ORS 6230
        DO 170 I=1,NTP1                                                 ORS 6240
          KL = KL + KLASSE(I)                                           ORS 6250
  170   CONTINUE                                                        ORS 6260
  175   CONTINUE                                                        ORS 6270
        IF(NOPOW(IR) .GT. 0) GOTO 184                                   ORS 6280
        KLZ = KLASSE(NTP)                                               ORS 6290
        DO 180 I=1,KLZ                                                  ORS 6300
          IKLASS = I                                                    ORS 6310
          IF(FIMA .LT. FIMAKL(KL+I)) GOTO 181                           ORS 6320
  180   CONTINUE                                                        ORS 6330
  184   CONTINUE                                                        ORS 6340
        IKLASS = 0                                                      ORS 6350
  181   CONTINUE                                                        ORS 6360
        IF(NPRINT .GE. 1) WRITE (NT,513) IR,PARVOL,HMGRM,FIMA,NTP,      ORS 6370
     1   TCHG1(IR),DOSNRX                                               ORS 6380
        JSATZ = 2 + IR                                                  ORS 6390
        IF(JAD11(JSATZ) .EQ. 0) JAD11(JSATZ) = JSUM11                   ORS 6400
        NXT11 = JAD11(JSATZ)                                            ORS 6410
        WRITE (NDA11,REC=NXT11) (DAV(L),L=1,KMAT),PARVOL,TCHG1(IR),NTP, ORS 6420
     1   IKLASS,BURNRX,DOSNRX,HMGRM,HMDEN                               ORS 6430
        NXT11 = NXT11 + 1                                               ORS 6440
        IF(JSUM11 .LT. NXT11) JSUM11 = NXT11                            ORS 6450
        IF(IVSP(21) .GE. 0) GOTO 191                                    ORS 6460
        NTYP = NTP * 100 + 1                                            ORS 6470
        WRITE (NDA28,REC=NXT28) NTYP                                    ORS 6480
        NXT28 = NXT28 + 1                                               ORS 6490
C                                                                       ORS 6500
        CALL WRDA(IWRITE,NDA28,NXT28,L28,DAV,KMAT)                      ORS 6510
C                                                                       ORS 6520
  191 CONTINUE                                                          ORS 6530
      IF(IVSP(21) .GE. 0) GOTO 600                                      ORS 6540
      IVSP(21) = 0                                                      ORS 6550
      NRS = NXT28 - 1                                                   ORS 6560
      WRITE (6,601) NDA28,NRS                                           ORS 6570
  600 CONTINUE                                                          ORS 6580
      IF(NPRINT .GT. 0) WRITE (NT,502) TDOWN                            ORS 6590
      IF(KUGL .LE. 0) GOTO 199                                          ORS 6600
C                                                                       ORS 6610
C     IHZ(KR) = NUMBER OF BATCHES / REGION                              ORS 6620
C     VOLIHZ(KR) = VOL. OF ONE REGION                                   ORS 6630
C                                                                       ORS 6640
      IRRE1 = 0                                                         ORS 6650
      DO 192 KR=1,NDR                                                   ORS 6660
        IRRE2 = IREG(KR)                                                ORS 6670
        IHZ(KR) = IRRE2 - IRRE1                                         ORS 6680
        VOLIHZ(KR) = VOLREG(KR)                                         ORS 6690
        VOLWEG(KR) = 0.0                                                ORS 6700
        IRRE1 = IRRE2                                                   ORS 6710
  192 CONTINUE                                                          ORS 6720
      IF(MBOX .LE. NBOX) GOTO 31                                        ORS 6730
C                                                                       ORS 6740
C     FILLING OF THE STORAGE BOXES FROM INTERMEDIATE BOXES              ORS 6750
C                                                                       ORS 6760
      DO 132 I=1,MBOX                                                   ORS 6770
        RVCB(I) = 0.                                                    ORS 6780
  132 CONTINUE                                                          ORS 6790
      K2 = KMAT + 8                                                     ORS 6800
      ANUL = 0.                                                         ORS 6810
      NB1 = NBOX + 1                                                    ORS 6820
      DO 33 I=NB1,MBOX                                                  ORS 6830
        RVCB(I) = 1.                                                    ORS 6840
        JSATZ = NRESHZ + 2 + I                                          ORS 6850
        IF(JAD11(JSATZ) .EQ. 0) JAD11(JSATZ) = JSUM11                   ORS 6860
        NXT11 = JAD11(JSATZ)                                            ORS 6870
        WRITE (NDA11,REC=NXT11) (ANUL,J=1,K2)                           ORS 6880
        NXT11 = NXT11 + 1                                               ORS 6890
        IF(JSUM11 .LT. NXT11) JSUM11 = NXT11                            ORS 6900
   33 CONTINUE                                                          ORS 6910
      IF(MBOX .EQ. 1) GOTO 31                                           ORS 6920
      IF(ISTORN .EQ. 1) GOTO 31                                         ORS 6930
      DO 34 I=1,NBOX                                                    ORS 6940
        JSATZ = NRESHZ + 2 + I                                          ORS 6950
        JS2 = JSATZ                                                     ORS 6960
        NXT11 = JAD11(JSATZ)                                            ORS 6970
        IF(NXT11 .EQ. 0) GOTO 34                                        ORS 6980
        READ (NDA11,REC=NXT11) (DAV(L),L=1,KMAT),PARVOL,XCHNRX,NTPNRX,  ORS 6990
     1   IKLNRX,BURNRX,DOSNRX,HMGRM,HMDEN                               ORS 7000
        NXT11 = NXT11 + 1                                               ORS 7010
        IF(IBOX(I) .EQ. 0) GOTO 32                                      ORS 7020
        JSATZ = NRESHZ + 2 + IBOX(I)                                    ORS 7030
        NXT11 = JAD11(JSATZ)                                            ORS 7040
        READ (NDA11,REC=NXT11) (DA2(L),L=1,KMAT),PARVO2,XCHNR2,NTPNR2,  ORS 7050
     1   IKLNR2,BURNR2,DOSNR2,HMGR2,HMDE2                               ORS 7060
        NXT11 = NXT11 + 1                                               ORS 7070
        PV = PARVOL + PARVO2                                            ORS 7080
        PV1 = PARVOL / PV                                               ORS 7090
        PV2 = PARVO2 / PV                                               ORS 7100
        DO 35 L=1,KMAT                                                  ORS 7110
          DA2(L) = DAV(L) * PV1 + DA2(L) * PV2                          ORS 7120
   35   CONTINUE                                                        ORS 7130
        PARVO2 = PV                                                     ORS 7140
        XCHNR2 = XCHNRX * PV1 + XCHNR2 * PV2                            ORS 7150
        NTPNR2 = NTPNRX                                                 ORS 7160
        HMGR2 = HMGRM + HMGR2                                           ORS 7170
        HMDE2 = HMDEN * PV1 + HMDE2 * PV2                               ORS 7180
        BURNR2 = BURNRX * PV1 + BURNR2 * PV2                            ORS 7190
        DOSNR2 = DOSNRX * PV1 + DOSNR2 * PV2                            ORS 7200
        DN2 = 0.                                                        ORS 7210
        DO 36 L=1,IACT                                                  ORS 7220
          DN2 = DN2 + DA2(L)                                            ORS 7230
   36   CONTINUE                                                        ORS 7240
        IF(HMDE2 .GT. 0.) FIMA = (HMDE2-DN2) / HMDE2                    ORS 7250
        IF(FIMA .LE. 0.) FIMA = EM9                                     ORS 7260
        NTP1 = NTPNR2 - 1                                               ORS 7270
        KL = 0                                                          ORS 7280
        IF(NTP1 .LE. 0) GOTO 39                                         ORS 7290
        DO 20 L=1,NTP1                                                  ORS 7300
          KL = KL + KLASSE(L)                                           ORS 7310
   20   CONTINUE                                                        ORS 7320
   39   CONTINUE                                                        ORS 7330
        KLZ = KLASSE(NTPNR2)                                            ORS 7340
        DO 37 L=1,KLZ                                                   ORS 7350
          IKLNR2 = L                                                    ORS 7360
          IF(FIMA .LT. FIMAKL(KL+L)) GOTO 38                            ORS 7370
   37   CONTINUE                                                        ORS 7380
        IKLNR2 = 0                                                      ORS 7390
   38   CONTINUE                                                        ORS 7400
        NXT11 = JAD11(JSATZ)                                            ORS 7410
        WRITE (NDA11,REC=NXT11) (DA2(L),L=1,KMAT),PARVO2,XCHNR2,NTPNR2, ORS 7420
     1   IKLNR2,BURNR2,DOSNR2,HMGR2,HMDE2                               ORS 7430
        NXT11 = NXT11 + 1                                               ORS 7440
   32   CONTINUE                                                        ORS 7450
        IF(IBOX(I) .EQ. 0 .AND. PARVOL .GT. 0.) WRITE (6,710) I,PARVOL  ORS 7460
        NXT11 = JAD11(JS2)                                              ORS 7470
        WRITE (NDA11,REC=NXT11) (ANUL,J=1,K2)                           ORS 7480
        NXT11 = NXT11 + 1                                               ORS 7490
        IBOX(I) = 0                                                     ORS 7500
   34 CONTINUE                                                          ORS 7510
      IF(NPRINT .LT. 0 .OR. IPRIN(15) .EQ. 0) GOTO 40                   ORS 7520
      IPRO = IFIX(PRO(299)) + 1                                         ORS 7530
      WRITE (NT,720) IPRO,IPRIN(15)+1                                   ORS 7540
      DO 30 I=NB1,MBOX                                                  ORS 7550
        J = I - NBOX                                                    ORS 7560
        JSATZ = NRESHZ + 2 + I                                          ORS 7570
        NXT11 = JAD11(JSATZ)                                            ORS 7580
        READ (NDA11,REC=NXT11) (DA2(L),L=1,KMAT),PARVO2,XCHNR2,NTPNR2,  ORS 7590
     1   IKLNR2,BURNR2,DOSNR2,HMGR2,HMDE2                               ORS 7600
        NXT11 = NXT11 + 1                                               ORS 7610
        WRITE (NT,721) J,NTPNR2,PARVO2,XCHNR2,BURNR2,DOSNR2,HMGR2       ORS 7620
   30 CONTINUE                                                          ORS 7630
   40 CONTINUE                                                          ORS 7640
   31 CONTINUE                                                          ORS 7650
C                                                                       ORS 7660
C     DECAY OF OUT-OF-PILE BATCHES DURING LAST CYCLE AND RESHUFFLE      ORS 7670
C                                                                       ORS 7680
      TT = TST * 3600. * 24.                                            ORS 7690
      DO 80 I=1,IACT                                                    ORS 7700
        TTS = TT * DECY(I)                                              ORS 7710
        IF(TTS .LE. 25.) DD(I) = EXP(-TTS)                              ORS 7720
        IF(TTS .GT. 25.) DD(I) = 0.                                     ORS 7730
   80 CONTINUE                                                          ORS 7740
      DO 194 N=1,NOPILE                                                 ORS 7750
        IR = NRESHZ + N                                                 ORS 7760
        JSATZ = 2 + MBOX + IR                                           ORS 7770
        NXT11 = JAD11(JSATZ)                                            ORS 7780
        READ (NDA11,REC=NXT11) (DAV(L),L=1,KMAT),PARVOL,TXCHG,NTP,IKLASSORS 7790
     1   ,BURNRX,DOSNRX,HMGRM,HMDEN                                     ORS 7800
        NXT11 = NXT11 + 1                                               ORS 7810
        IF(IKLASS .LE. 1) GOTO 194                                      ORS 7820
        TXCHG = TXCHG + TST * AAAA                                      ORS 7830
        TCHG1(IR) = TXCHG                                               ORS 7840
        IF(NXE .LE. 0) DAV(IACT+1) = 0.                                 ORS 7850
        DO 85 I=1,IACT                                                  ORS 7860
          DC(I) = DAV(I) * (1.-DD(I))                                   ORS 7870
          II = I                                                        ORS 7880
          GOTO(81,81,82,82,81,81,81,81,81,81,83,81,83,81,83,83,83,81,81,ORS 7890
     1     81,83,81,81,84,81,81,81,83),II                               ORS 7900
   81     CONTINUE                                                      ORS 7910
          DAV(I) = DAV(I) * DD(I)                                       ORS 7920
          GOTO 89                                                       ORS 7930
   82     CONTINUE                                                      ORS 7940
          DAV(I) = (DAV(I)+DC(I-1)) * DD(I)                             ORS 7950
          GOTO 89                                                       ORS 7960
   83     CONTINUE                                                      ORS 7970
          DAV(I) = (DAV(I)+DC(I-3)) * DD(I)                             ORS 7980
          GOTO 89                                                       ORS 7990
   84     CONTINUE                                                      ORS 8000
          DAV(I) = (DAV(I)+DC(I-4)) * DD(I)                             ORS 8010
   89     CONTINUE                                                      ORS 8020
          DECAF = DECAF + DC(I) * PARVOL                                ORS 8030
   85   CONTINUE                                                        ORS 8040
        NXT11 = JAD11(JSATZ)                                            ORS 8050
        WRITE (NDA11,REC=NXT11) (DAV(L),L=1,KMAT),PARVOL,TXCHG,NTP,     ORS 8060
     1   IKLASS,BURNRX,DOSNRX,HMGRM,HMDEN                               ORS 8070
        NXT11 = NXT11 + 1                                               ORS 8080
        IF(NPRINT .LT. 1) GOTO 194                                      ORS 8090
        IF(N .EQ. 1) WRITE (NT,700) IPRIN(15)+1                         ORS 8100
        WRITE (NT,501) NTP,PARVOL,TXCHG,BURNRX,DOSNRX,HMGRM             ORS 8110
        IF(NPRINT .NE. 2) GOTO 194                                      ORS 8120
        DO 193 L=1,KMAT                                                 ORS 8130
CFZJ059                                                       04.11.09  ORS 8140
          WRITE (NT,512) (BU(I,L),I=1,4),DAV(L)                         ORS 8150
  193   CONTINUE                                                        ORS 8160
  194 CONTINUE                                                          ORS 8170
      IF(NPRINT .GT. 0) WRITE (NT,500) TST                              ORS 8180
  199 CONTINUE                                                          ORS 8190
      RETURN                                                            ORS 8200
      END                                                               ORS 8210
      SUBROUTINE KUGELN(NPRINT,VR,DELDAX,IN1,FISNO,DECAF,TCHG1,NALT,    UGE   10
     1 NTYP1,N240,KMAT3,DAW,NEND14,NEND15,JAD11,RVCB)                   UGE   20
C                                                                       UGE   30
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    UGE   40
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    UGE   50
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIUGE   60
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 UGE   70
C                                                                       UGE   80
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1UGE   90
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         UGE  100
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    UGE  110
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)UGE  120
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  UGE  130
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    UGE  140
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         UGE  150
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,UGE  160
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            UGE  170
C                                                                       UGE  180
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, UGE  190
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        UGE  200
     2 FIMAKL(20),NOPILE,MREP,MARX(10),NAJB(10),FOJB(10),NFUL(10),NBTOT,UGE  210
     3 NB0,NCY                                                          UGE  220
C                                                                       UGE  230
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), UGE  240
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10UGE  250
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11UGE  260
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         UGE  270
C                                                                       UGE  280
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300)                      UGE  290
C                                                                       UGE  300
      COMMON /VARDIM/ A(8000000)                                        UGE  310
C                                                                       UGE  320
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       UGE  330
C                                                                       UGE  340
      COMMON /FLUXN/ DU(361),IACT                                       UGE  350
C                                                                       UGE  360
      COMMON /DECAYT/ DECY(30)                                          UGE  370
C                                                                       UGE  380
CFZJ055                                                       25.09.07  UGE  390
C                                                                       UGE  400
      COMMON /BUC/ BU(6,200)                                            UGE  410
C                                                                       UGE  420
      COMMON /CVX/ VX                                                   UGE  430
C                                                                       UGE  440
      DIMENSION VX(1000),VR(1000),DD(30),DC(30),TCHG1(N240),NALT(N240), UGE  450
     1 NTYP1(N240),DAW(KMAT3),JAD11(JD11),RVCB(MBOX)                    UGE  460
C                                                                       UGE  470
      EQUIVALENCE(LI(150),LDMAT),(LI(151),LAWT),(JTPE3,NT)              UGE  480
C                                                                       UGE  490
      REAL PA233/0.0/,NP239/0.0/                                        UGE  500
C                                                                       UGE  510
      CHARACTER*4 BU                                                    UGE  520
C                                                                       UGE  530
  760 FORMAT (///' FISSILE MATERIAL BALANCE, INCLUDING COMPLETE DECAY OFUGE  540
     1 PA-233 AND NP-239 IN DISCHARGED FUEL:'//37X,'PRODUCTION   /  ANNIUGE  550
     2HILATION  =  CONVERSION RATIO'//' PRECEEDING BURNUP PERIOD........UGE  560
     3:',G14.6,'  / ',G13.6,'  = ',G13.6)                               UGE  570
  761 FORMAT (' PRECEEDING LIFE OF THE REACTOR..:',G14.6,'  / ',G13.6,' UGE  580
     1 = ',G13.6/)                                                      UGE  590
  762 FORMAT (' AVERAGE ALPHA =',G12.5//)                               UGE  600
  763 FORMAT (' FISSIONS IN PRECEEDING BURNUP PERIOD:    TOTAL      FISSUGE  610
     1ILE ISOTOPES   OTHER ACTINIDES'//37X,E12.5,5X,E12.5,6X,E12.5)     UGE  620
10001 FORMAT ('1')                                                      UGE  630
10010 FORMAT ('0***** RECORD NO',I5,' SHOULD BE TYPE',I4/' ***** IT CONTUGE  640
     1AINS HOWEVER DATA FOR TYPE',I4)                                   UGE  650
10020 FORMAT (I4,3X,E12.5,3(3X,E12.5),6X,E12.5)                         UGE  660
CFZJ059                                                       04.11.09  UGE  670
10030 FORMAT (1H ,4A4,8X,E12.5)                                         UGE  680
10040 FORMAT (I4,2X,E12.5,5X,E12.5,2(3X,E12.5),6X,E12.5)                UGE  690
10041 FORMAT (13X,I2,4X,4(3X,E12.5),6X,E12.5)                           UGE  700
10050 FORMAT (///10X,'FOR THE NEXT RELOADING OF THE REACTOR'/10X,'THE FOUGE  710
     1LLOWING AMOUNTS OF FUEL ARE AVAILABLE'//' TYPE      VOLUME     AVGUGE  720
     2.IRR.TIME     AVG.BURNUP     AVG.F-DOSE   TOT.HEAVY METAL (GR)'/) UGE  730
10051 FORMAT (///10X,'FOR THE NEXT RELOADING OF THE REACTOR'/10X,'THE FOUGE  740
     1LLOWING AMOUNTS OF FUEL ARE AVAILABLE (WITHOUT CONTENT OF STORAGE UGE  750
     2BOXES)'//' TYPE      VOLUME     AVG.IRR.TIME     AVG.BURNUP     AVUGE  760
     3G.F-DOSE   TOT.HEAVY METAL (GR)'/)                                UGE  770
10060 FORMAT (///10X,'AT THIS RELOADING OF THE REACTOR'/10X,'THE FOLLOWIUGE  780
     1NG AMOUNTS OF FUEL WERE SCRAPPED'//' TYPE     VOLUME       AVG.IRRUGE  790
     2.TIME     AVG.BURNUP     AVG.F-DOSE   TOT.HEAVY METAL (GR)'/)     UGE  800
10080 FORMAT ('0',9X,'AT THIS RELOADING THE FOLLOWING AMOUNT OF THE OUT-UGE  810
     1OF-PILE BATCHES FROM THE LAST CYCLE WERE NOT RELOADED'/35X,'THEY WUGE  820
     2ILL BE TREATED AS SCRAPPED FUEL FROM THE SYSTEM'//12X,'TYPE    VOLUGE  830
     3UME      AVG.IRR.TIME     AVG.BURNUP     AVG.F-DOSE   TOT.HEAVY MEUGE  840
     4TAL (GR)'/)                                                       UGE  850
10090 FORMAT (//' AFTER RELOAD NO.',I3,',',I4,' MIXTURES HAVE BEEN PREPAUGE  860
     1RED FOR (OPTIONAL) REPROCESSING AND REFUELLING AT THE END OF THE SUGE  870
     2UBSEQUENT (',I3,')'/' OPERATING CYCLE.'/' NOTE: THE ISOTOPE CONTENUGE  880
     3T OF THE REPROCESSING MIXTURES AND OF THE AGING/JUMBLE BOXES HAS BUGE  890
     4EEN CALCULATED FOR THE END OF THE'/' COOLING PERIOD (TREPRO ON INPUGE  900
     5UT CARD R2).'//' REPROCESSING MIXTURE      VOLUME      AVG.IRR.TIMUGE  910
     6E     AVG.BURNUP     AVG.F-DOSE   TOT.HEAVY METAL (GR)'/)         UGE  920
10100 FORMAT (I12,12X,E12.5,3(3X,E12.5),6X,E12.5)                       UGE  930
C                                                                       UGE  940
C                                                                       UGE  950
      TYPEFD = 0.                                                       UGE  960
      TYPENU = 0.                                                       UGE  970
      TYPENX = 0.                                                       UGE  980
      TYPENY = 0.                                                       UGE  990
      TYPENZ = 0.                                                       UGE 1000
      NULL = 0                                                          UGE 1010
      K2 = KMAT + 2                                                     UGE 1020
C                                                                       UGE 1030
C     SCRATCH OLD DATA FOR SCRAP BATCHES AND REPROCESSING MIXTURES      UGE 1040
C                                                                       UGE 1050
      M1 = NRESHZ + NOPILE + 2 + MBOX                                   UGE 1060
      IDO = JTYP                                                        UGE 1070
      IF(MREP .EQ. 0) GOTO 4                                            UGE 1080
      IF(NCY .EQ. 0) GOTO 3                                             UGE 1090
      DO 2 K=1,MREP                                                     UGE 1100
        JSATZ = M1 + JTYP + K                                           UGE 1110
        NXT11 = JAD11(JSATZ)                                            UGE 1120
        IF(NXT11 .LE. 0) GOTO 2                                         UGE 1130
        READ (NDA11,REC=NXT11) (D,L=1,KMAT),PV,(D,L=1,5),GR,D           UGE 1140
        NXT11 = NXT11 + 1                                               UGE 1150
        VR(K) = PV - VR(K)                                              UGE 1160
        IF(NCY .GT. 0) GOTO 1                                           UGE 1170
        VR(K) = 0.                                                      UGE 1180
        VR(K+10) = 0.                                                   UGE 1190
    1   CONTINUE                                                        UGE 1200
        IF(PV .NE. 0.) PV = VR(K) / PV                                  UGE 1210
        VR(K+10) = GR * PV                                              UGE 1220
    2 CONTINUE                                                          UGE 1230
      NCY = IABS(NCY)                                                   UGE 1240
    3 CONTINUE                                                          UGE 1250
      IDO = IDO + MREP + NBTOT                                          UGE 1260
    4 CONTINUE                                                          UGE 1270
      IF(NBTOT .GT. 0) NB0 = M1 + JTYP                                  UGE 1280
      DO 30 K=1,IDO                                                     UGE 1290
        I = K                                                           UGE 1300
        IF(K .GT. JTYP) I = JTYP - K                                    UGE 1310
        JTMR = JTYP + MREP                                              UGE 1320
        IF(K .LE. JTMR) GOTO 5                                          UGE 1330
        DO 6 L=1,MREP                                                   UGE 1340
          I = -L                                                        UGE 1350
          JTMR = JTMR + NAJB(L)                                         UGE 1360
          IF(K .LE. JTMR) GOTO 5                                        UGE 1370
    6   CONTINUE                                                        UGE 1380
    5   CONTINUE                                                        UGE 1390
        JSATZ = M1 + K                                                  UGE 1400
        IF(JAD11(JSATZ) .EQ. 0) JAD11(JSATZ) = JSUM11                   UGE 1410
        NXT11 = JAD11(JSATZ)                                            UGE 1420
        WRITE (NDA11,REC=NXT11) (NULL,L=1,K2),I,NULL,NULL,NULL,NULL,NULLUGE 1430
        NXT11 = NXT11 + 1                                               UGE 1440
        IF(JSUM11 .LT. NXT11) JSUM11 = NXT11                            UGE 1450
   30 CONTINUE                                                          UGE 1460
      NBTOT = 0                                                         UGE 1470
C                                                                       UGE 1480
C     ADD LEFT-OVER FROM OLD OUT-OF-PILE FUEL TO NEW SCRAP FUEL BATCHES UGE 1490
C     SCRATCH OLD OUT-OF-PILE FUEL DATA                                 UGE 1500
C                                                                       UGE 1510
      M1 = KMAT                                                         UGE 1520
      M2 = 2 * KMAT                                                     UGE 1530
      M3 = 3 * KMAT                                                     UGE 1540
      K = 0                                                             UGE 1550
      KLZ = 0                                                           UGE 1560
      KOU = 0                                                           UGE 1570
      DO 50 I=1,NOPILE                                                  UGE 1580
        IR = NRESHZ + I                                                 UGE 1590
        JSATZ = 2 + MBOX + IR                                           UGE 1600
        NXT11 = JAD11(JSATZ)                                            UGE 1610
        READ (NDA11,REC=NXT11) (DAW(L),L=1,KMAT),PARVL1,XCHG1,NTP1,IKL1,UGE 1620
     1   BRN1,FDS1,HMG1,HMD1                                            UGE 1630
        NXT11 = NXT11 + 1                                               UGE 1640
        NTYP1(IR) = NTP1                                                UGE 1650
        NALT(IR) = 0                                                    UGE 1660
        IF(I .LE. KLZ) GOTO 45                                          UGE 1670
        K = K + 1                                                       UGE 1680
        KLZ = KLZ + KLASSE(K)                                           UGE 1690
        L1 = M2 + 1                                                     UGE 1700
        DO 44 L=L1,M3                                                   UGE 1710
          DAW(L) = 0.0                                                  UGE 1720
   44   CONTINUE                                                        UGE 1730
        XCHG3 = 0.0                                                     UGE 1740
        BRN3 = 0.0                                                      UGE 1750
        FDS3 = 0.0                                                      UGE 1760
        PARVL3 = 0.0                                                    UGE 1770
        HMG3 = 0.                                                       UGE 1780
        HMD3 = 0.                                                       UGE 1790
   45   CONTINUE                                                        UGE 1800
        IF(NTP1 .NE. K) WRITE (NT,10010) JSATZ,K,NTP1                   UGE 1810
C                                                                       UGE 1820
C     FRESH STORAGE BATCH IKL1=1 IS LEFT UNCHANGED                      UGE 1830
C                                                                       UGE 1840
        IF(IKL1 .LE. 1) GOTO 50                                         UGE 1850
        NXT11 = JAD11(JSATZ)                                            UGE 1860
        WRITE (NDA11,REC=NXT11) (NULL,L=1,K2),NTP1,IKL1,NULL,NULL,NULL, UGE 1870
     1   NULL                                                           UGE 1880
        NXT11 = NXT11 + 1                                               UGE 1890
C                                                                       UGE 1900
C     PREPARE AVERAGE OF ALL CLASSES FOR TYPE NTP1, AND TREAT           UGE 1910
C     "LEFT-OVER" FUEL FROM LAST REFUELLING AS DISCARDED                UGE 1920
C                                                                       UGE 1930
        PARVL2 = PARVL3                                                 UGE 1940
        V33 = PARVL1                                                    UGE 1950
        PARVL1 = PARVL1 - VX(I)                                         UGE 1960
        IF(PARVL1 .LT. 1.0) GOTO 51                                     UGE 1970
        PARVL3 = PARVL3 + PARVL1                                        UGE 1980
        V21 = PARVL1 / PARVL3                                           UGE 1990
        V22 = PARVL2 / PARVL3                                           UGE 2000
        V33 = PARVL1 / V33                                              UGE 2010
        DO 48 L=1,KMAT                                                  UGE 2020
          L2 = M2 + L                                                   UGE 2030
          DAW(L2) = DAW(L2) * V22 + DAW(L) * V21                        UGE 2040
   48   CONTINUE                                                        UGE 2050
        XCHG3 = XCHG3 * V22 + XCHG1 * V21                               UGE 2060
        BRN3 = BRN3 * V22 + BRN1 * V21                                  UGE 2070
        FDS3 = FDS3 * V22 + FDS1 * V21                                  UGE 2080
        HMG3 = HMG3 + HMG1 * V33                                        UGE 2090
        HMD3 = HMD3 * V22 + HMD1 * V21                                  UGE 2100
   51   CONTINUE                                                        UGE 2110
        IF(I .LT. KLZ) GOTO 50                                          UGE 2120
        KOU = KOU + 1                                                   UGE 2130
        JSATZ = NRESHZ + NOPILE + 2 + MBOX + K                          UGE 2140
        NXT11 = JAD11(JSATZ)                                            UGE 2150
        WRITE (NDA11,REC=NXT11) (DAW(L),L=L1,M3),PARVL3,XCHG3,NTP1,NULL,UGE 2160
     1   BRN3,FDS3,HMG3,HMD3                                            UGE 2170
        NXT11 = NXT11 + 1                                               UGE 2180
        IF(KOU .GT. 1) GOTO 55                                          UGE 2190
        WRITE (NT,10001)                                                UGE 2200
        WRITE (NT,10080)                                                UGE 2210
        GOTO 60                                                         UGE 2220
   55   CONTINUE                                                        UGE 2230
        IF(NPRINT .EQ. 2) WRITE (NT,10001)                              UGE 2240
   60   CONTINUE                                                        UGE 2250
        WRITE (NT,10041) NTP1,PARVL3,XCHG3,BRN3,FDS3,HMG3               UGE 2260
        IF(NPRINT .NE. 2) GOTO 50                                       UGE 2270
        DO 49 L=1,KMAT                                                  UGE 2280
          M = M2 + L                                                    UGE 2290
CFZJ059                                                       04.11.09  UGE 2300
          WRITE (NT,10030) (BU(ITEMP,L),ITEMP=1,4),DAW(M)               UGE 2310
   49   CONTINUE                                                        UGE 2320
   50 CONTINUE                                                          UGE 2330
C                                                                       UGE 2340
C     SORT DISCHARGED FUEL ACCORDING TO TYPE AND BURNUP CLASS           UGE 2350
C     DETERMINE BREAKAGE RATE OF FUEL  (BRUCH)                          UGE 2360
C                                                                       UGE 2370
      K2 = NRESHZ + 2 + MBOX                                            UGE 2380
      DO 100 IR=1,NRESHZ                                                UGE 2390
        IF(NALT(IR) .LE. 0) GOTO 100                                    UGE 2400
        XBRUCH = 1.0                                                    UGE 2410
        NALT(IR) = 0                                                    UGE 2420
        JSATZ = 2 + IR                                                  UGE 2430
        NXT11 = JAD11(JSATZ)                                            UGE 2440
        READ (NDA11,REC=NXT11) (DAW(L),L=1,KMAT),PARVL1,XCHG1,NTP1,IKL1,UGE 2450
     1   BRN1,FDS1,HMG1,HMD1                                            UGE 2460
        NXT11 = NXT11 + 1                                               UGE 2470
C                                                                       UGE 2480
C     HAS THIS BATCH BEEN DISCARDED (IKL1=0)                            UGE 2490
C                                                                       UGE 2500
        IF(IKL1 .LE. 0) GOTO 110                                        UGE 2510
        NRSATZ = 0                                                      UGE 2520
        NTP = NTP1 - 1                                                  UGE 2530
        IF(NTP .LE. 0) GOTO 106                                         UGE 2540
        DO 105 N=1,NTP                                                  UGE 2550
          KLZ = KLASSE(N)                                               UGE 2560
          NRSATZ = NRSATZ + KLZ                                         UGE 2570
  105   CONTINUE                                                        UGE 2580
  106   CONTINUE                                                        UGE 2590
        NRSATZ = NRSATZ + IKL1                                          UGE 2600
        JSATZ = K2 + NRSATZ                                             UGE 2610
        NXT11 = JAD11(JSATZ)                                            UGE 2620
        JX2 = NXT11                                                     UGE 2630
        L1 = M1 + 1                                                     UGE 2640
        L2 = M2                                                         UGE 2650
        READ (NDA11,REC=NXT11) (DAW(L),L=L1,L2),PARVL2,XCHG2,NTP2,IKL2, UGE 2660
     1   BRN2,FDS2,HMG2,HMD2                                            UGE 2670
        NXT11 = NXT11 + 1                                               UGE 2680
        IF(NTP2 .EQ. 0) NTP2 = NTP1                                     UGE 2690
        IF(NTP2 .NE. NTP1) WRITE (NT,10010) JSATZ,NTP1,NTP2             UGE 2700
        IF(IKL2 .EQ. 0 .OR. IKL2 .EQ. IKL1) GOTO 110                    UGE 2710
        WRITE (NT,10010) JSATZ,NTP1,NTP2                                UGE 2720
  110   CONTINUE                                                        UGE 2730
        JSATZ = K2 + NOPILE + NTP1                                      UGE 2740
        NXT11 = JAD11(JSATZ)                                            UGE 2750
        JX3 = NXT11                                                     UGE 2760
        L1 = M2 + 1                                                     UGE 2770
        L2 = M3                                                         UGE 2780
        READ (NDA11,REC=NXT11) (DAW(L),L=L1,L2),PARVL3,XCHG3,NTP3,IKL3, UGE 2790
     1   BRN3,FDS3,HMG3,HMD3                                            UGE 2800
        NXT11 = NXT11 + 1                                               UGE 2810
        IF(NTP3 .EQ. 0) NTP3 = NTP1                                     UGE 2820
        IF(NTP3 .NE. NTP1) WRITE (NT,10010) JSATZ,NTP1,NTP3             UGE 2830
        IF(IKL1 .LE. 0) GOTO 115                                        UGE 2840
        XBRUCH = BRUCH                                                  UGE 2850
        V21 = PARVL1 * (1.-XBRUCH) / (PARVL2+(1.-XBRUCH)*PARVL1)        UGE 2860
        V22 = PARVL2 / (PARVL2+(1.-XBRUCH)*PARVL1)                      UGE 2870
  115   CONTINUE                                                        UGE 2880
        V31 = 0.                                                        UGE 2890
        V33 = 0.                                                        UGE 2900
        PX1 = PARVL1 * XBRUCH                                           UGE 2910
        PX2 = PX1 + PARVL3                                              UGE 2920
        IF(PX2 .LE. 0.) GOTO 116                                        UGE 2930
        V31 = PX1 / PX2                                                 UGE 2940
        V33 = PARVL3 / PX2                                              UGE 2950
  116   CONTINUE                                                        UGE 2960
        DO 120 L=1,KMAT                                                 UGE 2970
          L2 = M2 + L                                                   UGE 2980
          DAW(L2) = DAW(L2) * V33 + DAW(L) * V31                        UGE 2990
          IF(IKL1 .LE. 0) GOTO 120                                      UGE 3000
          L1 = M1 + L                                                   UGE 3010
          DAW(L1) = DAW(L1) * V22 + DAW(L) * V21                        UGE 3020
  120   CONTINUE                                                        UGE 3030
        XCHG3 = XCHG3 * V33 + XCHG1 * V31                               UGE 3040
        BRN3 = BRN3 * V33 + BRN1 * V31                                  UGE 3050
        FDS3 = FDS3 * V33 + FDS1 * V31                                  UGE 3060
        HMG3 = HMG3 + HMG1 * XBRUCH                                     UGE 3070
        HMD3 = HMD3 * V33 + HMD1 * V31                                  UGE 3080
        PARVL3 = PARVL3 + PARVL1 * XBRUCH                               UGE 3090
        IKL3 = 0                                                        UGE 3100
        IF(IKL1 .LE. 0) GOTO 130                                        UGE 3110
        XCHG2 = XCHG2 * V22 + XCHG1 * V21                               UGE 3120
        BRN2 = BRN2 * V22 + BRN1 * V21                                  UGE 3130
        FDS2 = FDS2 * V22 + FDS1 * V21                                  UGE 3140
        HMG2 = HMG2 + HMG1 * (1.-XBRUCH)                                UGE 3150
        HMD2 = HMD2 * V22 + HMD1 * V21                                  UGE 3160
        PARVL2 = PARVL2 + PARVL1 * (1.-XBRUCH)                          UGE 3170
        IF(IKL2 .EQ. 0) IKL2 = IKL1                                     UGE 3180
        NXT11 = JX2                                                     UGE 3190
        L1 = M1 + 1                                                     UGE 3200
        L2 = M2                                                         UGE 3210
        WRITE (NDA11,REC=NXT11) (DAW(L),L=L1,L2),PARVL2,XCHG2,NTP2,IKL2,UGE 3220
     1   BRN2,FDS2,HMG2,HMD2                                            UGE 3230
        NXT11 = NXT11 + 1                                               UGE 3240
  130   CONTINUE                                                        UGE 3250
        NXT11 = JX3                                                     UGE 3260
        L1 = M2 + 1                                                     UGE 3270
        L2 = M3                                                         UGE 3280
        WRITE (NDA11,REC=NXT11) (DAW(L),L=L1,L2),PARVL3,XCHG3,NTP3,IKL3,UGE 3290
     1   BRN3,FDS3,HMG3,HMD3                                            UGE 3300
        NXT11 = NXT11 + 1                                               UGE 3310
  100 CONTINUE                                                          UGE 3320
C                                                                       UGE 3330
C     PRINT DATA FOR OUT-OF-PILE BATCHES                                UGE 3340
C                                                                       UGE 3350
      IF(NBOX .LE. 0) WRITE (NT,10050)                                  UGE 3360
      IF(NBOX .GT. 0) WRITE (NT,10051)                                  UGE 3370
      DO 200 N=1,NOPILE                                                 UGE 3380
        JSATZ = K2 + N                                                  UGE 3390
        NXT11 = JAD11(JSATZ)                                            UGE 3400
        READ (NDA11,REC=NXT11) (DAW(L),L=1,KMAT),PARVOL,XCHARG,NTP1,IKL1UGE 3410
     1   ,BRN1,FDS1,HMG1,HMD1                                           UGE 3420
        NXT11 = NXT11 + 1                                               UGE 3430
        WRITE (NT,10020) NTP1,PARVOL,XCHARG,BRN1,FDS1,HMG1              UGE 3440
        IF(PARVOL .LE. 0.0) GOTO 200                                    UGE 3450
        IF(NPRINT .NE. 2) GOTO 200                                      UGE 3460
        DO 220 L=1,KMAT                                                 UGE 3470
CFZJ059                                                       04.11.09  UGE 3480
          WRITE (NT,10030) (BU(I,L),I=1,4),DAW(L)                       UGE 3490
  220   CONTINUE                                                        UGE 3500
  200 CONTINUE                                                          UGE 3510
C                                                                       UGE 3520
C     RESTE ALLER ST.-BOXEN IN DIE ABBRANDKLASSEN 0 HINEINBUCHEN        UGE 3530
C                                                                       UGE 3540
      IF(NBOX .LE. 0) GOTO 70                                           UGE 3550
      NB = NBOX + 1                                                     UGE 3560
      DO 65 I=NB,MBOX                                                   UGE 3570
        IF(RVCB(I) .LE. 0.) GOTO 65                                     UGE 3580
        JSATZ = NRESHZ + 2 + I                                          UGE 3590
        NXT11 = JAD11(JSATZ)                                            UGE 3600
        READ (NDA11,REC=NXT11) (DAW(L),L=1,M1),PARVL1,XCHG1,NTP1,IKL1,  UGE 3610
     1   BRN1,FDS1,HMG1,HMD1                                            UGE 3620
        NXT11 = NXT11 + 1                                               UGE 3630
        JSATZ = NRESHZ + 2 + NOPILE + MBOX + NTP1                       UGE 3640
        JS3 = JSATZ                                                     UGE 3650
        NXT11 = JAD11(JSATZ)                                            UGE 3660
        L1 = M1 + 1                                                     UGE 3670
        READ (NDA11,REC=NXT11) (DAW(L),L=L1,M2),PARVL3,XCHG3,NTP3,IKL3, UGE 3680
     1   BRN3,FDS3,HMG3,HMD3                                            UGE 3690
        NXT11 = NXT11 + 1                                               UGE 3700
        PAR1 = PARVL1 * RVCB(I)                                         UGE 3710
        V0 = PARVL3 + PAR1                                              UGE 3720
        V31 = 0.                                                        UGE 3730
        V33 = 0.                                                        UGE 3740
        IF(V0 .LE. 0.) GOTO 63                                          UGE 3750
        V31 = PAR1 / V0                                                 UGE 3760
        V33 = PARVL3 / V0                                               UGE 3770
   63   CONTINUE                                                        UGE 3780
        DO 64 L=1,KMAT                                                  UGE 3790
          L2 = M1 + L                                                   UGE 3800
          DAW(L2) = DAW(L2) * V33 + DAW(L) * V31                        UGE 3810
   64   CONTINUE                                                        UGE 3820
        XCHG3 = XCHG3 * V33 + XCHG1 * V31                               UGE 3830
        BRN3 = BRN3 * V33 + BRN1 * V31                                  UGE 3840
        FDS3 = FDS3 * V33 + FDS1 * V31                                  UGE 3850
        HMG3 = HMG3 + HMG1 * RVCB(I)                                    UGE 3860
        HMD3 = HMD3 * V33 + HMD1 * V31                                  UGE 3870
        PARVL3 = V0                                                     UGE 3880
        RVCB(I) = 0.                                                    UGE 3890
        JSATZ = JS3                                                     UGE 3900
        NXT11 = JAD11(JSATZ)                                            UGE 3910
        L1 = M1 + 1                                                     UGE 3920
        WRITE (NDA11,REC=NXT11) (DAW(L),L=L1,M2),PARVL3,XCHG3,NTP3,IKL3,UGE 3930
     1   BRN3,FDS3,HMG3,HMD3                                            UGE 3940
        NXT11 = NXT11 + 1                                               UGE 3950
   65 CONTINUE                                                          UGE 3960
   70 CONTINUE                                                          UGE 3970
C                                                                       UGE 3980
C     PRINT DATA FOR DISCARDED FUEL BATCHES                             UGE 3990
C                                                                       UGE 4000
      IF(NPRINT .EQ. 2) WRITE (NT,10001)                                UGE 4010
      WRITE (NT,10060)                                                  UGE 4020
      K2 = K2 + NOPILE + JTYP                                           UGE 4030
      PA233 = 0.                                                        UGE 4040
      NP239 = 0.                                                        UGE 4050
      DO 240 N=1,JTYP                                                   UGE 4060
        IR = NRESHZ + NOPILE + N                                        UGE 4070
        JSATZ = 2 + MBOX + IR                                           UGE 4080
        NXT11 = JAD11(JSATZ)                                            UGE 4090
        READ (NDA11,REC=NXT11) (DAW(L),L=1,KMAT),PARVL1,XCHG1,NTP1,IKL1,UGE 4100
     1   BRN1,FDS1,HMG1,HMD1                                            UGE 4110
        NXT11 = NXT11 + 1                                               UGE 4120
        PA233 = PA233 + DAW(3) * PARVL1                                 UGE 4130
        NP239 = NP239 + DAW(13) * PARVL1                                UGE 4140
        NTYP1(IR) = NTP1                                                UGE 4150
        TCHG1(IR) = XCHG1                                               UGE 4160
        NALT(IR) = 1                                                    UGE 4170
        WRITE (NT,10040) NTP1,PARVL1,XCHG1,BRN1,FDS1,HMG1               UGE 4180
        TYPEX = BRN1 * HMG1                                             UGE 4190
        TYPEY = XCHG1 * PARVL1                                          UGE 4200
        TYPEZ = FDS1 * PARVL1                                           UGE 4210
        TYPENU = TYPENU + HMG1                                          UGE 4220
        TYPENX = TYPENX + TYPEX                                         UGE 4230
        TYPENY = TYPENY + TYPEY                                         UGE 4240
        TYPEFD = TYPEFD + TYPEZ                                         UGE 4250
        TYPENZ = TYPENZ + PARVL1                                        UGE 4260
        IF(PARVL1 .LE. 0.0) GOTO 240                                    UGE 4270
        IF(MARX(N) .LE. 0) GOTO 229                                     UGE 4280
C                                                                       UGE 4290
C     PREPARE REPROCESSING MIXTURES FROM DISCHARGED SCRAP FUEL          UGE 4300
C     (DECAY FOR REPROCESSING MIXTURES + AGING/JUMBLE BOXES IS          UGE 4310
C      CALCULATED BY TIME *TREPRO*)                                     UGE 4320
C                                                                       UGE 4330
        TT = TREPRO * 3600. * 24.                                       UGE 4340
        DO 80 I=1,IACT                                                  UGE 4350
          TTTR = TT * DECY(I)                                           UGE 4360
          IF(TTTR .LE. 25.) DD(I) = EXP(-TTTR)                          UGE 4370
          IF(TTTR .GT. 25.) DD(I) = 0.                                  UGE 4380
          DC(I) = DAW(I) * (1.-DD(I))                                   UGE 4390
          II = I                                                        UGE 4400
          GOTO(81,81,82,82,81,81,81,81,81,81,83,81,83,81,83,83,83,81,81,UGE 4410
     1     81,83,81,81,84,81,81,81,83),II                               UGE 4420
   81     CONTINUE                                                      UGE 4430
          DAW(I) = DAW(I) * DD(I)                                       UGE 4440
          GOTO 80                                                       UGE 4450
   82     CONTINUE                                                      UGE 4460
          DAW(I) = (DAW(I)+DC(I-1)) * DD(I)                             UGE 4470
          GOTO 80                                                       UGE 4480
   83     CONTINUE                                                      UGE 4490
          DAW(I) = (DAW(I)+DC(I-3)) * DD(I)                             UGE 4500
          GOTO 80                                                       UGE 4510
   84     CONTINUE                                                      UGE 4520
          DAW(I) = (DAW(I)+DC(I-4)) * DD(I)                             UGE 4530
   80   CONTINUE                                                        UGE 4540
        M = MARX(N)                                                     UGE 4550
        JSATZ = K2 + M                                                  UGE 4560
        NXT11 = JAD11(JSATZ)                                            UGE 4570
        JX3 = NXT11                                                     UGE 4580
        L1 = M2 + 1                                                     UGE 4590
        L2 = M3                                                         UGE 4600
        READ (NDA11,REC=NXT11) (DAW(L),L=L1,L2),PARVL3,XCHG3,NTP3,IKL3, UGE 4610
     1   BRN3,FDS3,HMG3,HMD3                                            UGE 4620
        NXT11 = NXT11 + 1                                               UGE 4630
        IF(NTP3 .EQ. 0) NTP3 = -M                                       UGE 4640
        IF(NTP3 .EQ. -M) GOTO 222                                       UGE 4650
        K = -M                                                          UGE 4660
        WRITE (NT,10010) JSATZ,K,IKL1,NTP3,IKL3                         UGE 4670
        GOTO 229                                                        UGE 4680
  222   CONTINUE                                                        UGE 4690
        V31 = PARVL1 / (PARVL3+PARVL1)                                  UGE 4700
        V33 = PARVL3 / (PARVL3+PARVL1)                                  UGE 4710
        DO 225 L=1,KMAT                                                 UGE 4720
          L2 = M2 + L                                                   UGE 4730
          DAW(L2) = DAW(L2) * V33 + DAW(L) * V31                        UGE 4740
  225   CONTINUE                                                        UGE 4750
        XCHG3 = XCHG3 * V33 + XCHG1 * V31                               UGE 4760
        BRN3 = BRN3 * V33 + BRN1 * V31                                  UGE 4770
        FDS3 = FDS3 * V33 + FDS1 * V31                                  UGE 4780
        HMG3 = HMG3 + HMG1                                              UGE 4790
        HMD3 = HMD3 * V33 + HMD1 * V31                                  UGE 4800
        PARVL3 = PARVL3 + PARVL1                                        UGE 4810
        IKL3 = 0                                                        UGE 4820
        NXT11 = JX3                                                     UGE 4830
        L1 = M2 + 1                                                     UGE 4840
        L2 = M3                                                         UGE 4850
        WRITE (NDA11,REC=NXT11) (DAW(L),L=L1,L2),PARVL3,XCHG3,NTP3,IKL3,UGE 4860
     1   BRN3,FDS3,HMG3,HMD3                                            UGE 4870
        NXT11 = NXT11 + 1                                               UGE 4880
  229   CONTINUE                                                        UGE 4890
        IF(NPRINT .NE. 2) GOTO 240                                      UGE 4900
        DO 230 L=1,KMAT                                                 UGE 4910
CFZJ059                                                       04.11.09  UGE 4920
          WRITE (NT,10030) (BU(I,L),I=1,4),DAW(L)                       UGE 4930
  230   CONTINUE                                                        UGE 4940
  240 CONTINUE                                                          UGE 4950
      CO1 = (XMNEW*DELDAX*8.64E+4+PA233+NP239+DECAF) * 1.E+24           UGE 4960
      CO2 = (YMNEW*DELDAX*8.64E+4) * 1.E+24                             UGE 4970
      CONVER = CO1 / CO2                                                UGE 4980
      WRITE (NT,760) CO1,CO2,CONVER                                     UGE 4990
      IF(IN1 .GT. 1) GOTO 146                                           UGE 5000
      CO1S = 0.                                                         UGE 5010
      CO2S = 0.                                                         UGE 5020
  146 CONTINUE                                                          UGE 5030
      CO1S = CO1S + CO1                                                 UGE 5040
      CO2S = CO2S + CO2                                                 UGE 5050
      CONSUM = CO1S / CO2S                                              UGE 5060
      WRITE (NT,761) CO1S,CO2S,CONSUM                                   UGE 5070
      IF(INZWXX .GT. 0) PRO(26) = CONVER                                UGE 5080
      ALFA = (CO2/FISNO) - 1.                                           UGE 5090
      WRITE (NT,762) ALFA                                               UGE 5100
      FY = FISNO / ZMNEW                                                UGE 5110
      FA = FY - FISNO                                                   UGE 5120
      WRITE (NT,763) FY,FISNO,FA                                        UGE 5130
      IF(TYPENZ .EQ. 0.) GOTO 245                                       UGE 5140
      PRO(190) = ALFA                                                   UGE 5150
      PRO(37) = TYPENY / TYPENZ                                         UGE 5160
      IF(TYPENU .GT. 50.) PRO(38) = TYPENX / TYPENU                     UGE 5170
      IF(PRO(200) .GT. 0.) PRO(39) = (TYPEFD/TYPENZ) / PRO(200)         UGE 5180
  245 CONTINUE                                                          UGE 5190
C                                                                       UGE 5200
C     PRINT DATA FOR REPROCESSING MIXTURES                              UGE 5210
C                                                                       UGE 5220
      IF(MREP .EQ. 0) GOTO 999                                          UGE 5230
      IF(NPRINT .EQ. 2) WRITE (NT,10001)                                UGE 5240
      INZW3 = INZW(3) + 1                                               UGE 5250
      WRITE (NT,10090) IPRIN(15)+1,MREP,INZW3                           UGE 5260
      L1 = 1                                                            UGE 5270
      L2 = KMAT                                                         UGE 5280
      DO 260 M=1,MREP                                                   UGE 5290
        DDAY = DELDAY                                                   UGE 5300
        JNST = IABS(JNSTOP)                                             UGE 5310
        MM = M                                                          UGE 5320
        KMAT6 = KMAT + 6                                                UGE 5330
        NP15 = NEND14                                                   UGE 5340
        NEND15 = NP15 + KMAT6 * 3                                       UGE 5350
C                                                                       UGE 5360
        IF(NCY .GT. 0) CALL REMI(MM,VR,DDAY,JNST,NPRINT,KMAT6,JAD11,    UGE 5370
     1   A(NP15),A(KA(LDMAT)),A(KA(LAWT)))                              UGE 5380
C                                                                       UGE 5390
        JSATZ = K2 + M                                                  UGE 5400
        NXT11 = JAD11(JSATZ)                                            UGE 5410
        READ (NDA11,REC=NXT11) (DAW(L),L=L1,L2),PARVL3,XCHG3,NTP3,IKL3, UGE 5420
     1   BRN3,FDS3,HMG3,HMD3                                            UGE 5430
        NXT11 = NXT11 + 1                                               UGE 5440
        WRITE (NT,10100) M,PARVL3,XCHG3,BRN3,FDS3,HMG3                  UGE 5450
        IF(PARVL3 .LE. 0.0) GOTO 260                                    UGE 5460
        IF(NPRINT .NE. 2) GOTO 260                                      UGE 5470
        DO 250 L=L1,L2                                                  UGE 5480
CFZJ059                                                       04.11.09  UGE 5490
          WRITE (NT,10030) (BU(I,L),I=1,4),DAW(L)                       UGE 5500
  250   CONTINUE                                                        UGE 5510
  260 CONTINUE                                                          UGE 5520
  999 CONTINUE                                                          UGE 5530
      RETURN                                                            UGE 5540
      END                                                               UGE 5550
      SUBROUTINE REPROX(IRNN,DAV,HMDEN,HMGRM,NEWTYP,XREPRO,HMETAV,FICOMPREP   10
     1 ,IDFISS,AWT)                                                     REP   20
C                                                                       REP   30
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    REP   40
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    REP   50
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIREP   60
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 REP   70
C                                                                       REP   80
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1REP   90
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         REP  100
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    REP  110
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)REP  120
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  REP  130
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    REP  140
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         REP  150
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,REP  160
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            REP  170
C                                                                       REP  180
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, REP  190
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        REP  200
     2 FIMAKL(20),NOPILE,MREP,MARX(10),NAJB(10),FOJB(10),NFUL(10),NBTOT,REP  210
     3 NB0,NCY                                                          REP  220
C                                                                       REP  230
      COMMON /BLOCKK/ DU(3),AVO                                         REP  240
C                                                                       REP  250
      COMMON /DECAYT/ DECY(30)                                          REP  260
C                                                                       REP  270
      COMMON /FLUXN/ D(361),IACT                                        REP  280
C                                                                       REP  290
      DIMENSION XREPRO(KMAT),DAV(KMAT),HMETAV(N200),AWT(IACT),          REP  300
     1 FICOMP(IACT),IDFISS(IACT),DD(30),DC(30)                          REP  310
C                                                                       REP  320
C                                                                       REP  330
C     DECAY DURING STORAGE AND REPROCESSING                             REP  340
C                                                                       REP  350
      TT = TREPRO * 3600. * 24.                                         REP  360
      IF(IRNN .EQ. 0) TT = ABS(TSTORE) * 3600. * 24.                    REP  370
      IF(TT .LE. 0.0) GOTO 50                                           REP  380
      DO 90 I=1,IACT                                                    REP  390
        TTR = TT * DECY(I)                                              REP  400
        IF(TTR .LE. 25.) DD(I) = EXP(-TTR)                              REP  410
        IF(TTR .GT. 25.) DD(I) = 0.                                     REP  420
   90 CONTINUE                                                          REP  430
      DO 95 I=1,IACT                                                    REP  440
        DC(I) = DAV(I) * (1.-DD(I))                                     REP  450
        II = I                                                          REP  460
        GOTO(91,91,92,92,91,91,91,91,91,91,93,91,93,91,93,93,93,91,91,91REP  470
     1   ,93,91,91,94,91,91,91,93),II                                   REP  480
   91   CONTINUE                                                        REP  490
        DAV(I) = DAV(I) * DD(I)                                         REP  500
        GOTO 95                                                         REP  510
   92   CONTINUE                                                        REP  520
        DAV(I) = (DAV(I)+DC(I-1)) * DD(I)                               REP  530
        GOTO 95                                                         REP  540
   93   CONTINUE                                                        REP  550
        DAV(I) = (DAV(I)+DC(I-3)) * DD(I)                               REP  560
        GOTO 95                                                         REP  570
   94   CONTINUE                                                        REP  580
        DAV(I) = (DAV(I)+DC(I-4)) * DD(I)                               REP  590
   95 CONTINUE                                                          REP  600
   50 CONTINUE                                                          REP  610
      IF(IRNN .LE. 0) RETURN                                            REP  620
C                                                                       REP  630
C     REPROCESSING SIMULATION                                           REP  640
C                                                                       REP  650
      NRC = IACT + 2 + NO + NLUM + NC                                   REP  660
      DO 100 M=1,KMAT                                                   REP  670
        IF(M .EQ. NRC) REALNN(5) = 1.                                   REP  680
        DAV(M) = DAV(M) * XREPRO(M) * REALNN(5)                         REP  690
  100 CONTINUE                                                          REP  700
      IF(NSPALT .EQ. 0 .AND. NEWTYP .GE. 0) GOTO 150                    REP  710
C                                                                       REP  720
      ENTRY ENRIX(IRNN,DAV,HMETAV,HMDEN,HMGRM,FICOMP,IDFISS,AWT)        REP  730
C                                                                       REP  740
C     FIND WANTED "FISSILE/HEAVY-METAL" RATIO (XSPALT) BEFORE           REP  750
C     REFUELING. THE ATOM DENSITY OF THE FISSILE ISOTOPE "NSPALT" IS    REP  760
C     ALTERED.                                                          REP  770
C                                                                       REP  780
      REALNN(5) = 1.                                                    REP  790
      FISMAT = DAV(4) + DAV(6) + DAV(16) + DAV(18)                      REP  800
      YSPALT = FISMAT / HMETAV(IRNN)                                    REP  810
      IF(NSPALT .LE. 0) GOTO 125                                        REP  820
      IF(YSPALT .GT. XSPALT) GOTO 125                                   REP  830
      XDAV = 0.0                                                        REP  840
      FICSUM = 0.0                                                      REP  850
      DO 110 IN=1,NSPALT                                                REP  860
        ISP = IDFISS(IN)                                                REP  870
        IF(.NOT. (ISP .EQ. 4 .OR. ISP .EQ. 6 .OR. ISP .EQ. 16 .OR. ISP  REP  880
     1   .EQ. 18)) GOTO 110                                             REP  890
        XDAV = XDAV + DAV(ISP)                                          REP  900
        FICSUM = FICSUM + FICOMP(IN)                                    REP  910
  110 CONTINUE                                                          REP  920
      DAVOS = XSPALT * HMETAV(IRNN) - FISMAT                            REP  930
      DO 120 IN=1,NSPALT                                                REP  940
        ISP = IDFISS(IN)                                                REP  950
        DAV(ISP)= DAVOS * FICOMP(IN) / FICSUM + DAV(ISP)                REP  960
  120 CONTINUE                                                          REP  970
      GOTO 150                                                          REP  980
  125 CONTINUE                                                          REP  990
      YSPALT = XSPALT / YSPALT                                          REP 1000
      REALNN(5) = -YSPALT                                               REP 1010
      DO 130 M=1,IACT                                                   REP 1020
        DAV(M) = DAV(M) * YSPALT                                        REP 1030
  130 CONTINUE                                                          REP 1040
  150 CONTINUE                                                          REP 1050
      IF(MAKEUP .EQ. 0) GOTO 299                                        REP 1060
C                                                                       REP 1070
C     IT IS ASSUMED THAT THE TOTAL DENSITY OF ALL HEAVY METALS AT THE   REP 1080
C     BEGINNING OF REACTOR-LIFETIME IS NOT ALTERED AFTER REPROCESSING.  REP 1090
C     THE DIFFERENCE IS MADE UP WITH MATERIAL NO. "MAKEUP"           .  REP 1100
C                                                                       REP 1110
      HMETX = 0.0                                                       REP 1120
      DO 200 M=1,IACT                                                   REP 1130
        HMETX = HMETX + DAV(M)                                          REP 1140
  200 CONTINUE                                                          REP 1150
      DAV(MAKEUP) = DAV(MAKEUP) + HMETAV(IRNN) - HMETX                  REP 1160
  299 CONTINUE                                                          REP 1170
      HMGRM = 0.                                                        REP 1180
      HMDEN = 0.                                                        REP 1190
      DO 300 M=1,IACT                                                   REP 1200
        HMDEN = HMDEN + DAV(M)                                          REP 1210
        HMGRM = HMGRM + DAV(M) * AWT(M)                                 REP 1220
  300 CONTINUE                                                          REP 1230
      HMGRM = HMGRM / AVO                                               REP 1240
      RETURN                                                            REP 1250
      END                                                               REP 1260
      SUBROUTINE KOSTPW(JUMP,TCHG1,NALT,N240,NTYP1,NNEU,AIN,AEX,VCHG,AK,KOS   10
     1 DK,FK,BK,QK,EKWH,ET,ORE1,ORE2,SEP1,SEP2,UUKOST,U3IN,U3EX,U5IN,   KOS   20
     2 U5EX,THIN,THEX,U8IN,U8EX,SM1,SM2,U6IN,U6EX,PU39IN,PU39EX,PU40IN, KOS   30
     3 PU40EX,PU41IN,PU41EX,PU42IN,PU42EX,DMAT,AWT,PA33IN,PA33EX,U4IN,  KOS   40
     4 U4EX,NP37IN,NP37EX,PU38IN,PU38EX,AM41IN,AM41EX,AM2MIN,AM2MEX,    KOS   50
     5 AM42IN,AM42EX,AM43IN,AM43EX,CM42IN,CM42EX,CM43IN,CM43EX,CM44IN,  KOS   60
     6 CM44EX,IMAT)                                                     KOS   70
C                                                                       KOS   80
C     ***  KPD  ***  PRESENT WORTH FUEL CYCLE COST EVALUATION CODE      KOS   90
C     EXTENDED AND REVISED   U. HANSEN  JULY  1974                      KOS  100
C     THIS VERSION ACCORDING TO DPR/KFA REPORT   1974                   KOS  110
C                                                                       KOS  120
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    KOS  130
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    KOS  140
     2 POWES,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIKOS  150
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 KOS  160
C                                                                       KOS  170
CFZJ043                                                       23.09.05  KOS  180
      COMMON /BLOCKK/ JMAF,B,TBURN,AVO,NRCHG,VAUF,NDUM,Z1,Z2,MXTYP,     KOS  190
     1 NRTP1(10),NRTP2(10),IALT(10),INEU(10),KSM(10),ETA,IQ,GD,F,GLD,   KOS  200
     2 IPRINT,N250,T1,NEXT,DD,TCUM,XLOSS1,NMAF,ND2O,XLOSS2,NCAN,FEED,   KOS  210
     3 TAIL,CU3O8,CO8F6,CTRENN,TORE(2,10),TFAB(2,10),TAUF(10),CF,FIF,FITKOS  220
     4 ,UNEED(12),SEP(12),EU5(12),CHIU3(10),CHITH(10),CHIPU(10),CHIU(10)KOS  230
     5 ,CU3(10),CU5(10),CTH(10),CU8(10),CPU(10),CFAB(10),CAUF(10),CHID2OKOS  240
     6 ,CDNEU,CDALT,ZD,SD,VD,YPA,ZL,SS,GMZ,SMOL(10),RES,PUFD,DOUTIN,NULLKOS  250
     7 ,Q,QXQ,QQ,CPUST(10),TIN(2,10),TEX(10),SMTYP(10),SMIN(10),SMEX(10)KOS  260
     8 ,POWER,Z3,Z4,Z5,ZIN(2,10),ZFB(2,10),ZEX(10),ZAU(10),ORE(2,10),   KOS  270
     9 MZ,NR,JEQQ,XDV,YBV,YPU,YD2O,XFA,YDV,YTH,E,XAU,XTH,YFA,XBAR,YU,BZKKOS  280
     X ,XBV,YAU,USUM,XD2O,BZKD2O,PUSUM,SMTOT,VERL,NEWCO,IESC,TOUT       KOS  290
C                                                                       KOS  300
      INTEGER*4 Q,QXQ,QQ                                                KOS  310
C                                                                       KOS  320
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, KOS  330
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAS,BRUCH,KUGL,JTYP,KLASSE(10),        KOS  340
     2 FIMAKL(20),NOPILE,MREP,MARX(10),NAJB(10),FOJB(10),NFUL(10),NBTOT,KOS  350
     3 NB0,NCY                                                          KOS  360
C                                                                       KOS  370
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300),FABC(10),REPC(10),NNEKOS  380
     1 ,LISTEQ                                                          KOS  390
C                                                                       KOS  400
      COMMON /VARDIM/ A(8000000)                                        KOS  410
C                                                                       KOS  420
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       KOS  430
C                                                                       KOS  440
      COMMON /FLUXN/ D(361),IACT                                        KOS  450
C                                                                       KOS  460
      REAL*8 FIND,FEXD,TTLID,TTLED                                      KOS  470
C                                                                       KOS  480
      DIMENSION TCHG1(N240),NALT(N240),NTYP1(N240),NNEU(N200),          KOS  490
     1 AIN(IACT,N240),AEX(IACT,N240),VCHG(N240),AK(MMAF),DK(MMAF),      KOS  500
     2 FK(MMAF),BK(MMAF),QK(MMAF),EKWH(MMAF),ET(MMAF),ORE1(MMAF),       KOS  510
     3 ORE2(MMAF),SEP1(MMAF),SEP2(MMAF),UUKOST(MMAF),U3IN(N240),        KOS  520
     4 U3EX(N240),U5IN(N240),U5EX(N240),THIN(N240),THEX(N240),U8IN(N240)KOS  530
     5 ,U8EX(N240),SM1(N240),SM2(N240),U6IN(N240),U6EX(N240),           KOS  540
     6 PU39IN(N240),PU39EX(N240),PU40IN(N240),PU40EX(N240),PU41IN(N240),KOS  550
     7 PU41EX(N240),PU42IN(N240),PU42EX(N240),PA33IN(N240),PA33EX(N240),KOS  560
     8 U4IN(N240),U4EX(N240),PU38IN(N240),PU38EX(N240),AM41IN(N240),    KOS  570
     9 AM41EX(N240),AM2MIN(N240),AM2MEX(N240),AM42IN(N240),AM42EX(N240),KOS  580
     X AM43IN(N240),AM43EX(N240),CM42IN(N240),CM42EX(N240),CM43IN(N240),KOS  590
     Y CM43EX(N240),CM44IN(N240),CM44EX(N240),IMAT(KMAT),AWT(IACT),     KOS  600
     Z DMAT(IACT+1,4,4)                                                 KOS  610
C                                                                       KOS  620
      REAL*4 NP37IN(N240),NP37EX(N240)                                  KOS  630
C                                                                       KOS  640
CFZJ043                                                       23.09.05  KOS  650
      EQUIVALENCE(NDUM,MM)                                              KOS  660
C                                                                       KOS  670
C                                                                       KOS  680
      NMA = MMAF                                                        KOS  690
      FIND = 0.D0                                                       KOS  700
      FEXD = 0.D0                                                       KOS  710
      TTLID = 0.D0                                                      KOS  720
      TTLED = 0.D0                                                      KOS  730
      GOTO(1,2),JUMP                                                    KOS  740
    1 CONTINUE                                                          KOS  750
C                                                                       KOS  760
CFZJ043                                                       23.09.05  KOS  770
      CALL KOSTIN(TCHG1,N240,NALT,NTYP1,AIN,AEX,VCHG,BK,QK,UUKOST)      KOS  780
C                                                                       KOS  790
      IF(ND2O .EQ. 0) GO TO 5                                           KOS  800
      GD = DOUTIN                                                       KOS  810
      CHID2O = GD * (CDNEU+CDALT) * 0.5 * SD / 365.                     KOS  820
      TT = GLD * 365.                                                   KOS  830
C                                                                       KOS  840
      CALL ZINS(ZD,TT,ZZD2)                                             KOS  850
C                                                                       KOS  860
      ZFAKT = ZZD2 / (ZZD2-1.)                                          KOS  870
      DD = ZFAKT * GD * (CDNEU-CDALT/ZZD2)                              KOS  880
    5 CONTINUE                                                          KOS  890
      IF(VERL .NE. 0.) VAUF = VERL                                      KOS  900
      RETURN                                                            KOS  910
    2 CONTINUE                                                          KOS  920
      VC = 0.                                                           KOS  930
      IF(NULL .GT. 0) GOTO 3                                            KOS  940
C                                                                       KOS  950
      CALL KOSTUT(TCHG1,N240,NTYP1,ET,ORE1,ORE2,SEP1,SEP2,U3IN,U3EX,U5INKOS  960
     1 ,U5EX,THIN,THEX,U8IN,U8EX,SM1,SM2,U6IN,U6EX,PU39IN,PU39EX,PU40IN,KOS  970
     2 PU40EX,PU41IN,PU41EX,PU42IN,PU42EX,PA33IN,PA33EX,U4IN,U4EX,NP37INKOS  980
     3 ,NP37EX,PU38IN,PU38EX,AM41IN,AM41EX,AM2MIN,AM2MEX,AM42IN,AM42EX, KOS  990
     4 AM43IN,AM43EX,CM42IN,CM42EX,CM43IN,CM43EX,CM44IN,CM44EX)         KOS 1000
C                                                                       KOS 1010
    3 CONTINUE                                                          KOS 1020
      NR = NRCHG                                                        KOS 1030
      IF(KUGL .GT. 0) NR = NR + NOPILE + JTYP                           KOS 1040
      IF(JMAF .NE. 0) NMAF = JMAF                                       KOS 1050
      ITPU = 0                                                          KOS 1060
      ET(MM) = TBURN / F                                                KOS 1070
      TCUM = TCUM + ET(MM) / 365.                                       KOS 1080
      TOP = TCUM * 365. * F                                             KOS 1090
      IF(TOUT .LT. 0.0) TOUT = -ET(MM)                                  KOS 1100
      XPU = 0.0                                                         KOS 1110
      UKOST = 0.                                                        KOS 1120
      IF(PUFD .NE. 0.) UKOST = UUKOST(MM)                               KOS 1130
      IF(MM .GT. 1) RES = 1.                                            KOS 1140
C                                                                       KOS 1150
C     BERECHNUNG DER GEWICHTE                                           KOS 1160
C                                                                       KOS 1170
      DO 14 I=1,MXTYP                                                   KOS 1180
        CPUST(I) = CPU(I)                                               KOS 1190
        SMTYP(I) = 0.0                                                  KOS 1200
        SMIN(I) = 0.0                                                   KOS 1210
        SMEX(I) = 0.0                                                   KOS 1220
   14 CONTINUE                                                          KOS 1230
      SMTOT = 0.0                                                       KOS 1240
      IF(INZWXX .EQ. 0) GOTO 25                                         KOS 1250
      DO 17 I=61,96                                                     KOS 1260
        PRO(I) = 0.                                                     KOS 1270
   17 CONTINUE                                                          KOS 1280
      DO 170 I=211,260                                                  KOS 1290
        PRO(I) = 0.                                                     KOS 1300
  170 CONTINUE                                                          KOS 1310
      LISTEQ = QQ                                                       KOS 1320
   25 CONTINUE                                                          KOS 1330
      DO 44 J=1,NR                                                      KOS 1340
        SM1(J) = 0.0                                                    KOS 1350
        SM2(J) = 0.0                                                    KOS 1360
        K = NTYP1(J)                                                    KOS 1370
        IF(K .LE. 0) GOTO 44                                            KOS 1380
        V = VCHG(J) * 0.001 / AVO                                       KOS 1390
        DO 4 I=1,IACT                                                   KOS 1400
          C = AIN(I,J) * AWT(I) * V                                     KOS 1410
          SM1(J) = SM1(J) + C                                           KOS 1420
          IF(J .GT. NRCHG) GOTO 15                                      KOS 1430
          IF(INZWXX .EQ. 0) GOTO 29                                     KOS 1440
C                                                                       KOS 1450
C     FUEL INVENTORY FOR *LISTE*                                        KOS 1460
C                                                                       KOS 1470
          PRO(210+I) = PRO(210+I) + (AIN(I,J)+AEX(I,J)) *  AWT(I) * V / KOS 1480
     1     2.                                                           KOS 1490
   29     CONTINUE                                                      KOS 1500
          AI = C                                                        KOS 1510
          AE = AEX(I,J) * AWT(I) * V                                    KOS 1520
          DMAT(I,1,4) = DMAT(I,1,4) + AI                                KOS 1530
          DMAT(I,2,4) = DMAT(I,2,4) + AE                                KOS 1540
          VC = VC + VCHG(J)                                             KOS 1550
C                                                                       KOS 1560
C     SUM OF ALL IN-CORE CHARGES                                        KOS 1570
C                                                                       KOS 1580
          SMTOT = SMTOT + C                                             KOS 1590
          IF(ABS(TBURN-TCHG1(J)) .GT. T1) GOTO 15                       KOS 1600
          SMIN(K) = SMIN(K) + C                                         KOS 1610
   15     CONTINUE                                                      KOS 1620
          C = AEX(I,J) * AWT(I) * V                                     KOS 1630
          SM2(J) = SM2(J) + C                                           KOS 1640
          IF(J .GT. NRCHG+NOPILE) GOTO 16                               KOS 1650
C                                                                       KOS 1660
C     SCRAP CHARGES ALREADY ACCOUNTED FOR                               KOS 1670
C                                                                       KOS 1680
          SMTYP(K) = SMTYP(K) + C                                       KOS 1690
   16     CONTINUE                                                      KOS 1700
          IF(NALT(J) .LE. 0) GOTO 4                                     KOS 1710
          SMEX(K) = SMEX(K) + C                                         KOS 1720
    4   CONTINUE                                                        KOS 1730
   44 CONTINUE                                                          KOS 1740
      DMAT(IACT+1,1,4) = DMAT(IACT+1,1,4) + VC                          KOS 1750
      POWER = B * SMTOT / TBURN * 1.0E-3                                KOS 1760
      IF(INZWXX .EQ. 0) GOTO 34                                         KOS 1770
      DO 41 I=1,IACT                                                    KOS 1780
        PRO(95) = PRO(95) + PRO(210+I)                                  KOS 1790
   41 CONTINUE                                                          KOS 1800
      GWTH = POWES / 1.E+9                                              KOS 1810
      DO 42 I=211,210+IACT                                              KOS 1820
        PRO(I) = PRO(I) / GWTH                                          KOS 1830
   42 CONTINUE                                                          KOS 1840
      PRO(95) = PRO(95) / GWTH                                          KOS 1850
   34 CONTINUE                                                          KOS 1860
C                                                                       KOS 1870
C     DECAY OF PA233 TO U233 PER YPA (SEE INPUT-CARD K4)                KOS 1880
C     TOTAL DECAY OF:                                                   KOS 1890
C     TH233 --> PA233  |  U 237 --> NP237  |  NP238 --> PU238           KOS 1900
C     U 239 --> NP239  |  NP239 --> PU239  |  NP240 --> PU240           KOS 1910
C     PU243 --> AM243  |  AM244 --> CM244                               KOS 1920
C                                                                       KOS 1930
      DO 6 J=1,NR                                                       KOS 1940
        L = J                                                           KOS 1950
        FAKTOR = VCHG(L) * 0.001 / AVO                                  KOS 1960
        THIN(J) = AIN(1,L) * AWT(1) * FAKTOR                            KOS 1970
        THEX(J) = AEX(1,L) * AWT(1) * FAKTOR                            KOS 1980
        PA33IN(J) = AIN(3,L) * AWT(3) * FAKTOR                          KOS 1990
        IF(NALT(J) .EQ. 1) AEX(3,L) = AEX(3,L) + AEX(2,L)               KOS 2000
        PA33EX(J) = AEX(3,L) * AWT(3) * FAKTOR                          KOS 2010
        U3IN(J) = AIN(4,L) * AWT(4) * FAKTOR                            KOS 2020
        IF(NALT(J) .EQ. 1) AEX(4,L) = AEX(4,L) + AEX(3,L) * YPA         KOS 2030
        U3EX(J) = AEX(4,L) * AWT(4) * FAKTOR                            KOS 2040
        U4IN(J) = AIN(5,L) * AWT(5) * FAKTOR                            KOS 2050
        U4EX(J) = AEX(5,L) * AWT(5) * FAKTOR                            KOS 2060
        U5IN(J) = AIN(6,L) * AWT(6) * FAKTOR                            KOS 2070
        U5EX(J) = AEX(6,L) * AWT(6) * FAKTOR                            KOS 2080
        U6IN(J) = AIN(7,L) * AWT(7) * FAKTOR                            KOS 2090
        U6EX(J) = AEX(7,L) * AWT(7) * FAKTOR                            KOS 2100
        U8IN(J) = AIN(9,L) * AWT(9) * FAKTOR                            KOS 2110
        U8EX(J) = AEX(9,L) * AWT(9) * FAKTOR                            KOS 2120
        NP37IN(J) = AIN(11,L) * AWT(11) * FAKTOR                        KOS 2130
        IF(NALT(J) .EQ. 1) AEX(11,L) = AEX(11,L) + AEX(8,L)             KOS 2140
        NP37EX(J) = AEX(11,L) * AWT(11) * FAKTOR                        KOS 2150
        PU38IN(J) = AIN(15,L) * AWT(15) * FAKTOR                        KOS 2160
        IF(NALT(J) .EQ. 1) AEX(15,L) = AEX(15,L) + AEX(12,L)            KOS 2170
        PU38EX(J) = AEX(15,L) * AWT(15) * FAKTOR                        KOS 2180
        PU39IN(J) = AIN(16,L) * AWT(16) * FAKTOR                        KOS 2190
        IF(NALT(J) .EQ. 1) AEX(16,L) = AEX(16,L) + AEX(13,L) + AEX(10,L)KOS 2200
        PU39EX(J) = AEX(16,L) * AWT(16) * FAKTOR                        KOS 2210
        PU40IN(J) = AIN(17,L) * AWT(17) * FAKTOR                        KOS 2220
        IF(NALT(J) .EQ. 1) AEX(17,L) = AEX(17,L) + AEX(14,L)            KOS 2230
        PU40EX(J) = AEX(17,L) * AWT(17) * FAKTOR                        KOS 2240
        PU41IN(J) = AIN(18,L) * AWT(18) * FAKTOR                        KOS 2250
        PU41EX(J) = AEX(18,L) * AWT(18) * FAKTOR                        KOS 2260
        PU42IN(J) = AIN(19,L) * AWT(19) * FAKTOR                        KOS 2270
        PU42EX(J) = AEX(19,L) * AWT(19) * FAKTOR                        KOS 2280
        AM41IN(J) = AIN(21,L) * AWT(21) * FAKTOR                        KOS 2290
        AM41EX(J) = AEX(21,L) * AWT(21) * FAKTOR                        KOS 2300
        AM2MIN(J) = AIN(22,L) * AWT(22) * FAKTOR                        KOS 2310
        AM2MEX(J) = AEX(22,L) * AWT(22) * FAKTOR                        KOS 2320
        AM42IN(J) = AIN(23,L) * AWT(23) * FAKTOR                        KOS 2330
        AM42EX(J) = AEX(23,L) * AWT(23) * FAKTOR                        KOS 2340
        AM43IN(J) = AIN(24,L) * AWT(24) * FAKTOR                        KOS 2350
        IF(NALT(J) .EQ. 1) AEX(24,L) = AEX(24,L) + AEX(20,L)            KOS 2360
        AM43EX(J) = AEX(24,L) * AWT(24) * FAKTOR                        KOS 2370
        CM42IN(J) = AIN(26,L) * AWT(26) * FAKTOR                        KOS 2380
        CM42EX(J) = AEX(26,L) * AWT(26) * FAKTOR                        KOS 2390
        CM43IN(J) = AIN(27,L) * AWT(27) * FAKTOR                        KOS 2400
        CM43EX(J) = AEX(27,L) * AWT(27) * FAKTOR                        KOS 2410
        CM44IN(J) = AIN(28,L) * AWT(28) * FAKTOR                        KOS 2420
        IF(NALT(J) .EQ. 1) AEX(28,L) = AEX(28,L) + AEX(25,L)            KOS 2430
        CM44EX(J) = AEX(28,L) * AWT(28) * FAKTOR                        KOS 2440
    6 CONTINUE                                                          KOS 2450
      IF(INZWXX .EQ. 0) GOTO 26                                         KOS 2460
C                                                                       KOS 2470
C     SUPPLY RATES AND DISCHARGE RATES FOR *LISTE*                      KOS 2480
C                                                                       KOS 2490
      DO 23 J=1,NNE                                                     KOS 2500
        DO 23 K=1,NRCHG                                                 KOS 2510
          IF(K .NE. NNEU(J)) GOTO 23                                    KOS 2520
          PRO(61) = PRO(61) + THIN(K)                                   KOS 2530
          PRO(62) = PRO(62) + PA33IN(K)                                 KOS 2540
          PRO(63) = PRO(63) + U3IN(K)                                   KOS 2550
          PRO(64) = PRO(64) + U4IN(K)                                   KOS 2560
          PRO(65) = PRO(65) + U5IN(K)                                   KOS 2570
          PRO(66) = PRO(66) + U6IN(K)                                   KOS 2580
          PRO(67) = PRO(67) + U8IN(K)                                   KOS 2590
          PRO(68) = PRO(68) + NP37IN(K)                                 KOS 2600
          PRO(69) = PRO(69) + PU38IN(K)                                 KOS 2610
          PRO(70) = PRO(70) + PU39IN(K)                                 KOS 2620
          PRO(71) = PRO(71) + PU40IN(K)                                 KOS 2630
          PRO(72) = PRO(72) + PU41IN(K)                                 KOS 2640
          PRO(73) = PRO(73) + PU42IN(K)                                 KOS 2650
          PRO(74) = PRO(74) + AM41IN(K)                                 KOS 2660
          PRO(75) = PRO(75) + AM2MIN(K)                                 KOS 2670
          PRO(76) = PRO(76) + AM42IN(K)                                 KOS 2680
          PRO(77) = PRO(77) + AM43IN(K)                                 KOS 2690
          PRO(78) = PRO(78) + CM42IN(K)                                 KOS 2700
          PRO(79) = PRO(79) + CM43IN(K)                                 KOS 2710
          PRO(80) = PRO(80) + CM44IN(K)                                 KOS 2720
   23 CONTINUE                                                          KOS 2730
      NNA = NRCHG + NOPILE + 1                                          KOS 2740
      NNB = NNA + JTYP - 1                                              KOS 2750
      DO 24 J=NNA,NNB                                                   KOS 2760
        PRO(241) = PRO(241) + THEX(J)                                   KOS 2770
        PRO(242) = PRO(242) + PA33EX(J)                                 KOS 2780
        PRO(243) = PRO(243) + U3EX(J)                                   KOS 2790
        PRO(244) = PRO(244) + U4EX(J)                                   KOS 2800
        PRO(245) = PRO(245) + U5EX(J)                                   KOS 2810
        PRO(246) = PRO(246) + U6EX(J)                                   KOS 2820
        PRO(247) = PRO(247) + U8EX(J)                                   KOS 2830
        PRO(248) = PRO(248) + NP37EX(J)                                 KOS 2840
        PRO(249) = PRO(249) + PU38EX(J)                                 KOS 2850
        PRO(250) = PRO(250) + PU39EX(J)                                 KOS 2860
        PRO(251) = PRO(251) + PU40EX(J)                                 KOS 2870
        PRO(252) = PRO(252) + PU41EX(J)                                 KOS 2880
        PRO(253) = PRO(253) + PU42EX(J)                                 KOS 2890
        PRO(254) = PRO(254) + AM41EX(J)                                 KOS 2900
        PRO(255) = PRO(255) + AM2MEX(J)                                 KOS 2910
        PRO(256) = PRO(256) + AM42EX(J)                                 KOS 2920
        PRO(257) = PRO(257) + AM43EX(J)                                 KOS 2930
        PRO(258) = PRO(258) + CM42EX(J)                                 KOS 2940
        PRO(259) = PRO(259) + CM43EX(J)                                 KOS 2950
        PRO(260) = PRO(260) + CM44EX(J)                                 KOS 2960
   24 CONTINUE                                                          KOS 2970
   26 CONTINUE                                                          KOS 2980
C                                                                       KOS 2990
C     STEUER-FAKTOREN                                                   KOS 3000
C                                                                       KOS 3010
      TS = TBURN * 0.5 / F                                              KOS 3020
      STIN = 1. + SS * TS / 365.                                        KOS 3030
      STEX = 1. - SS * TS / 365.                                        KOS 3040
      TT = -TBURN / F                                                   KOS 3050
C                                                                       KOS 3060
      CALL ZINS(Z4,TT,ZF)                                               KOS 3070
C                                                                       KOS 3080
      CALL ZIN1(Z4,TT,ZF4)                                              KOS 3090
C                                                                       KOS 3100
      TT = -ABS(TOUT)                                                   KOS 3110
C                                                                       KOS 3120
      CALL ZINS(Z4,TT,ZFX)                                              KOS 3130
C                                                                       KOS 3140
      ST4 = 1. + SS * (-TT*0.5) / 365.                                  KOS 3150
      ST5 = 1. - SS * (-TT*0.5) / 365.                                  KOS 3160
C                                                                       KOS 3170
C     BARWERT SPALTSTOFFVERBRAUCH                                       KOS 3180
C                                                                       KOS 3190
   50 CONTINUE                                                          KOS 3200
      IF(XPU .LE. 0.0) GOTO 40                                          KOS 3210
      DO 39 K=1,MXTYP                                                   KOS 3220
        CPU(K) = XPU                                                    KOS 3230
   39 CONTINUE                                                          KOS 3240
   40 CONTINUE                                                          KOS 3250
      KTYP = 0                                                          KOS 3260
      XBV = 0.                                                          KOS 3270
      XTH = 0.                                                          KOS 3280
      USUM = 0.0                                                        KOS 3290
      PUSUM = 0.0                                                       KOS 3300
      ORE1(MM) = 0.                                                     KOS 3310
      ORE2(MM) = 0.                                                     KOS 3320
      SEP1(MM) = 0.                                                     KOS 3330
      SEP2(MM) = 0.                                                     KOS 3340
      DO 9 J=1,NR                                                       KOS 3350
        L = 0                                                           KOS 3360
        K = NTYP1(J)                                                    KOS 3370
        NN = 2                                                          KOS 3380
        IF(ABS(TOP-TCHG1(J)) .LT. T1) NN = 1                            KOS 3390
        IF(K .GT. 0) ZF1 = ZIN(NN,K)                                    KOS 3400
        ZFJ = ZF                                                        KOS 3410
        TT = (TCHG1(J)-TBURN) / F                                       KOS 3420
        STX = STEX                                                      KOS 3430
        STI = STIN                                                      KOS 3440
        IF(ABS(TT) .GT. T1) GOTO 12                                     KOS 3450
        IF(K .GT. 0) STI = STI + SS * TIN(NN,K) / 365.                  KOS 3460
        L = K                                                           KOS 3470
   12   CONTINUE                                                        KOS 3480
        IF(J .LE. NRCHG .OR. J .GT. NRCHG+NOPILE) GOTO 20               KOS 3490
        ZFJ = ZFX                                                       KOS 3500
        STI = ST4                                                       KOS 3510
        STX = ST5                                                       KOS 3520
        IF(NEWCO .EQ. 1) GOTO 20                                        KOS 3530
        IF(K .EQ. KTYP) GOTO 20                                         KOS 3540
        KTYP = K                                                        KOS 3550
        GOTO 9                                                          KOS 3560
   20   CONTINUE                                                        KOS 3570
        IF(K .GT. 0) CU5IN = CU5(K)                                     KOS 3580
        IF(K .GT. 0) CU5EX = CU5(K)                                     KOS 3590
        IF(NCAN .EQ. 0) GOTO 13                                         KOS 3600
        IF(U5IN(J) .LE. 0.0) GOTO 11                                    KOS 3610
        ENU235 = U5IN(J) / (U8IN(J)+U5IN(J)+U6IN(J))                    KOS 3620
C                                                                       KOS 3630
        CALL ENRICO(ENU235,CP,K,NN)                                     KOS 3640
C                                                                       KOS 3650
        CU5IN = CP / ENU235                                             KOS 3660
        IF(L .EQ. 0) GOTO 11                                            KOS 3670
        ORE1(MM) = ORE1(MM) + UNEED(L) * (U8IN(J)+U5IN(J)+U6IN(J))      KOS 3680
        SEP1(MM) = SEP1(MM) + SEP(L) * (U8IN(J)+U5IN(J)+U6IN(J))        KOS 3690
   11   CONTINUE                                                        KOS 3700
        L = 0                                                           KOS 3710
        IF(U5EX(J) .LE. 0.) GOTO 13                                     KOS 3720
        L = K                                                           KOS 3730
        ENU235 = U5EX(J) / (U8EX(J)+U5EX(J)+U6EX(J))                    KOS 3740
C                                                                       KOS 3750
        CALL ENRICO(ENU235,CP,K,NN)                                     KOS 3760
C                                                                       KOS 3770
        CU5EX = CP / ENU235                                             KOS 3780
   13   CONTINUE                                                        KOS 3790
        IF(NALT(J) .EQ. 1) GOTO 8                                       KOS 3800
        IF(K .LE. 0) GOTO 9                                             KOS 3810
        FIN = (U3IN(J)*CU3(K)+U5IN(J)*CU5IN+PU39IN(J)*CPU(K)+PU41IN(J)* KOS 3820
     1   CPU(K)) * ZF1 * STI                                            KOS 3830
        FEX = (U3EX(J)*CU3(K)+U5EX(J)*CU5EX+PU39EX(J)*CPU(K)+PU41EX(J)* KOS 3840
     1   CPU(K)) * ZF1 * ZFJ * STX                                      KOS 3850
        TTLI = ZF1 * (CTH(K)*THIN(J)+CU8(K)*U8IN(J))                    KOS 3860
        TTLE = ZF1 * ZFJ * (CTH(K)*THEX(J)+CU8(K)*U8EX(J))              KOS 3870
        FIND = FIND + DBLE(FIN)                                         KOS 3880
        FEXD = FEXD + DBLE(FEX)                                         KOS 3890
        TTLID = TTLID + DBLE(TTLI)                                      KOS 3900
        TTLED = TTLED + DBLE(TTLE)                                      KOS 3910
        GOTO 9                                                          KOS 3920
C                                                                       KOS 3930
C     DISCHARGE BATCH                                                   KOS 3940
C                                                                       KOS 3950
    8   CONTINUE                                                        KOS 3960
        ZFJ = ZF1                                                       KOS 3970
        IF(KUGL .LE. 0) GOTO 10                                         KOS 3980
        ZFJ = ZF1 * ZF                                                  KOS 3990
        STI = STEX                                                      KOS 4000
   10   CONTINUE                                                        KOS 4010
        ZF3 = ZEX(K)                                                    KOS 4020
        STX = STEX - SS * TEX(K) / 365.                                 KOS 4030
        BTL = ZFJ * (U3IN(J)*CU3(K)+U5IN(J)*CU5IN+PU39IN(J)*CPU(K)+     KOS 4040
     1   PU41IN(J)*CPU(K)) * STI                                        KOS 4050
        UERL = ZF * ZF3 * VAUF * (U3EX(J)*CU3(K)*CHIU3(K)+U5EX(J)*CU5EX*KOS 4060
     1   CHIU(K)) * STX                                                 KOS 4070
        USUM = USUM + UERL                                              KOS 4080
        PUERL = ZF * ZF3 * VAUF * (PU39EX(J)+PU41EX(J)) * CHIPU(K) *    KOS 4090
     1   CPU(K) * STX                                                   KOS 4100
        PUSUM = PUSUM + PUERL                                           KOS 4110
        TTLI = ZFJ * (THIN(J)*CTH(K)+U8IN(J)*CU8(K))                    KOS 4120
        TTLE = ZF * ZF3 * VAUF * (THEX(J)*CTH(K)+U8EX(J)*CU8(K)) *      KOS 4130
     1   CHITH(K)                                                       KOS 4140
        TTLID = TTLID + DBLE(TTLI)                                      KOS 4150
        TTLED = TTLED + DBLE(TTLE)                                      KOS 4160
        XBV = XBV + BTL                                                 KOS 4170
        IF(L .EQ. 0) GOTO 9                                             KOS 4180
        ORE2(MM) = ORE2(MM) + UNEED(L) * (U8EX(J)+U5EX(J)+U6EX(J))      KOS 4190
        SEP2(MM) = SEP2(MM) + SEP(L) * (U8EX(J)+U5EX(J)+U6EX(J))        KOS 4200
    9 CONTINUE                                                          KOS 4210
      FIND = FIND - FEXD                                                KOS 4220
      TTLID = TTLID - TTLED                                             KOS 4230
      XBV = XBV + SNGL(FIND)                                            KOS 4240
      XTH = XTH + SNGL(TTLID)                                           KOS 4250
      IF(INZWXX .EQ. 0) GOTO 28                                         KOS 4260
      OZU = 1. + (8.*16./(3.*238.))                                     KOS 4270
      PRO(82) = ORE1(MM) * OZU                                          KOS 4280
      PRO(81) = SEP1(MM)                                                KOS 4290
      PRO(83) = ORE2(MM) * OZU                                          KOS 4300
      PRO(96) = SEP2(MM)                                                KOS 4310
      DO 27 J=61,83                                                     KOS 4320
        PRO(J) = PRO(J) / (POWES*TBURN) * 1.E+9                         KOS 4330
        IF(J .GT. 80) GOTO 27                                           KOS 4340
        PRO(J+180) = PRO(J+180) / (POWES*TBURN) * 1.E+9                 KOS 4350
   27 CONTINUE                                                          KOS 4360
      PRO(96)= PRO(96)/ (POWES*TBURN) * 1.E+9                           KOS 4370
   28 CONTINUE                                                          KOS 4380
C                                                                       KOS 4390
C     BARWERT FABRIKATION UND AUFARBEITUNG                              KOS 4400
C                                                                       KOS 4410
      XFA = 0.0                                                         KOS 4420
      XAU = 0.0                                                         KOS 4430
      NN = 2                                                            KOS 4440
      IF(MM .EQ. 1) NN = 1                                              KOS 4450
      DO 19 K=1,MXTYP                                                   KOS 4460
        ZF2 = ZFB(NN,K)                                                 KOS 4470
        XFA = XFA + SMIN(K) * CFAB(K) * RES * ZF2                       KOS 4480
        ZF5 = ZAU(K)                                                    KOS 4490
        XAU = XAU + SMEX(K) * CAUF(K) * ZF5 * ZF                        KOS 4500
   19 CONTINUE                                                          KOS 4510
C                                                                       KOS 4520
C     D2O, STEUERN D2O, VERLUSTE D2O                                    KOS 4530
C                                                                       KOS 4540
      XDV = 0.                                                          KOS 4550
      XD2O = 0.                                                         KOS 4560
      IF(ND2O .EQ. 0) GOTO 21                                           KOS 4570
      ZF2 = ZFB(2,1)                                                    KOS 4580
      TT = ET(MM)                                                       KOS 4590
C                                                                       KOS 4600
      CALL ZINS(ZD,TT,ZZD2)                                             KOS 4610
C                                                                       KOS 4620
      XD2O = (DD*(ZZD2-1.)+CHID2O*TT) / ZZD2                            KOS 4630
      XDV = GD * VD * CDNEU * ZF2 * ET(MM-1) / 365.                     KOS 4640
      IF(ITPU .EQ. 0) QK(MM) = XD2O                                     KOS 4650
   21 CONTINUE                                                          KOS 4660
C                                                                       KOS 4670
C     BARWERT NETTOKOSTEN                                               KOS 4680
C                                                                       KOS 4690
      XBAR = XBV + XTH + XFA + XDV + XAU - USUM - PUSUM                 KOS 4700
      IF(ITPU .NE. 0) GOTO 18                                           KOS 4710
      AK(MM) = XAU                                                      KOS 4720
      BK(MM) = XBV + XTH - USUM - PUSUM                                 KOS 4730
      DK(MM) = XDV                                                      KOS 4740
      FK(MM) = XFA                                                      KOS 4750
   18 CONTINUE                                                          KOS 4760
      XBAR = XBAR + XD2O                                                KOS 4770
C                                                                       KOS 4780
C     ERZEUGTE ENERGIE                                                  KOS 4790
C                                                                       KOS 4800
      E = B * SMTOT * 24. * ETA                                         KOS 4810
C     (SMTOT = INCORE SM)                                               KOS 4820
C                                                                       KOS 4830
C     BRENNSTOFFZYKLUSKOSTEN                                            KOS 4840
C                                                                       KOS 4850
      TT = ET(MM) / GMZ                                                 KOS 4860
C                                                                       KOS 4870
      CALL ZIN1(Z4,TT,ZFMZ)                                             KOS 4880
C                                                                       KOS 4890
      FAKTOR = -QXQ * GMZ * ZFMZ / ZF4                                  KOS 4900
      FDE = 0.                                                          KOS 4910
      IF(E .GT. 0.) FDE = FAKTOR / E                                    KOS 4920
      IF(ITPU .EQ. 0) EKWH(MM) = E / FAKTOR                             KOS 4930
      YBV = XBV * FDE                                                   KOS 4940
      YTH = XTH * FDE                                                   KOS 4950
      YFA = XFA * FDE                                                   KOS 4960
      YAU = XAU * FDE                                                   KOS 4970
      YDV = XDV * FDE                                                   KOS 4980
      YD2O = XD2O * FDE                                                 KOS 4990
      YU = USUM * FDE                                                   KOS 5000
      YPU = PUSUM * FDE                                                 KOS 5010
      BZK = YBV + YTH + YFA + YDV + YAU - YU - YPU                      KOS 5020
      BZKD2O = BZK + YD2O                                               KOS 5030
      IF(INZWXX .EQ. 0) GOTO 49                                         KOS 5040
      PRO(23) = YBV + YTH - YPU - YU                                    KOS 5050
      PRO(24) = YFA + YAU                                               KOS 5060
      PRO(25) = BZK                                                     KOS 5070
      PRO(60) = YAU                                                     KOS 5080
   49 CONTINUE                                                          KOS 5090
      IF(ITPU .NE. 0) GOTO 51                                           KOS 5100
C                                                                       KOS 5110
C     OUTPUT                                                            KOS 5120
C                                                                       KOS 5130
      CALL KOSTUT(TCHG1,N240,NTYP1,ET,ORE1,ORE2,SEP1,SEP2,U3IN,U3EX,U5INKOS 5140
     1 ,U5EX,THIN,THEX,U8IN,U8EX,SM1,SM2,U6IN,U6EX,PU39IN,PU39EX,PU40IN,KOS 5150
     2 PU40EX,PU41IN,PU41EX,PU42IN,PU42EX,PA33IN,PA33EX,U4IN,U4EX,NP37INKOS 5160
     3 ,NP37EX,PU38IN,PU38EX,AM41IN,AM41EX,AM2MIN,AM2MEX,AM42IN,AM42EX, KOS 5170
     4 AM43IN,AM43EX,CM42IN,CM42EX,CM43IN,CM43EX,CM44IN,CM44EX)         KOS 5180
C                                                                       KOS 5190
      GOTO 51                                                           KOS 5200
    7 CONTINUE                                                          KOS 5210
C                                                                       KOS 5220
      CALL FCCUT                                                        KOS 5230
C                                                                       KOS 5240
   51 CONTINUE                                                          KOS 5250
C                                                                       KOS 5260
      IF(INZWXX .GT. 0) CALL LISTE(IMAT)                                KOS 5270
C                                                                       KOS 5280
      IF(UKOST .EQ. 0.0) GOTO 55                                        KOS 5290
      XPU = CPU(1)                                                      KOS 5300
C                                                                       KOS 5310
      CALL PUEQ(UKOST,BZK,XPU,ITPU,Q,QQ,*50,*7)                         KOS 5320
C                                                                       KOS 5330
   55 CONTINUE                                                          KOS 5340
      DO 60 K=1,MXTYP                                                   KOS 5350
        CPU(K) = CPUST(K)                                               KOS 5360
   60 CONTINUE                                                          KOS 5370
      IF(TCUM .GT. GLD) GOTO 9001                                       KOS 5380
      IF(MM .EQ. NMAF) GOTO 9000                                        KOS 5390
      RETURN                                                            KOS 5400
C                                                                       KOS 5410
C     BERECHNUNG RESTWERT AM ENDE DER LEBENSDAUER                       KOS 5420
C                                                                       KOS 5430
 9001 NMAF = MM                                                         KOS 5440
 9000 XREST = 0.                                                        KOS 5450
      IF(NR .EQ. 1) GOTO 32                                             KOS 5460
      OREST = 0.                                                        KOS 5470
      SEPRST = 0.                                                       KOS 5480
      KTYP = 0                                                          KOS 5490
      NDO = NR                                                          KOS 5500
      IF(KUGL .GT. 0) NDO = NR - JTYP                                   KOS 5510
      DO 30 J=1,NDO                                                     KOS 5520
        K = NTYP1(J)                                                    KOS 5530
        L = K                                                           KOS 5540
        NN = 2                                                          KOS 5550
        IF(ABS(TOP-TCHG1(J)) .LT. T1) NN = 1                            KOS 5560
        IF(K .GT. 0) ZF1 = ZIN(NN,K)                                    KOS 5570
        ZFJ = ZF1 * ZF                                                  KOS 5580
        STI = STEX                                                      KOS 5590
        IF(J .LE. NRCHG) GOTO 35                                        KOS 5600
        ZFJ = ZF1 * ZFX                                                 KOS 5610
        STI = ST5                                                       KOS 5620
        IF(NEWCO .EQ. 1) GOTO 35                                        KOS 5630
        IF(K .EQ. KTYP) GOTO 35                                         KOS 5640
C                                                                       KOS 5650
C     DO NOT REPROCESS CLASS "1" I.E. FRESH FUEL,OF OUT-OF-PILE CHARGES KOS 5660
C                                                                       KOS 5670
        SMTYP(K) = SMTYP(K) - SM2(J)                                    KOS 5680
        KTYP = K                                                        KOS 5690
        GOTO 30                                                         KOS 5700
   35   CONTINUE                                                        KOS 5710
        IF(NALT(J) .EQ. 1) GOTO 30                                      KOS 5720
        IF(K .GT. 0) ZF3 = ZEX(K)                                       KOS 5730
        FAKTOR = ZF * ZF3 * VAUF                                        KOS 5740
        IF(K .GT. 0) STX = 1. - SS * TEX(K) / 365.                      KOS 5750
        XAVO3 = VCHG(J) * 0.001 / AVO * AWT(4)                          KOS 5760
        XAVO9 = VCHG(J) * 0.001 / AVO * AWT(16)                         KOS 5770
        IF(K .GT. 0) CU5EX = CU5(K)                                     KOS 5780
        IF(NCAN .EQ. 0) GOTO 33                                         KOS 5790
        IF(U5EX(J) .LE. 0.) GOTO 33                                     KOS 5800
        ENU235 = U5EX(J) / (U8EX(J)+U5EX(J)+U6EX(J))                    KOS 5810
C                                                                       KOS 5820
        CALL ENRICO(ENU235,CP,L,NN)                                     KOS 5830
C                                                                       KOS 5840
        CU5EX = CP / ENU235                                             KOS 5850
        IF(L .LE. 0) GOTO 33                                            KOS 5860
        OREST = OREST + UNEED(L) * (U8EX(J)+U5EX(J)+U6EX(J))            KOS 5870
        SEPRST = SEPRST + SEP(L) * (U8EX(J)+U5EX(J)+U6EX(J))            KOS 5880
   33   CONTINUE                                                        KOS 5890
        IF(K .LE. 0) GOTO 30                                            KOS 5900
        RESTF = FAKTOR * (CHIU3(K)*CU3(K)*(U3EX(J)+AEX(2,J)*XAVO3)+CU5EXKOS 5910
     1   *U5EX(J)*CHIU(K)+CHIPU(K)*CPU(K)*(PU39EX(J)+AEX(8,J)*XAVO9+    KOS 5920
     2   PU41EX(J))) * STX - ZFJ * (CU3(K)*U3EX(J)+CU5EX*U5EX(J)+CPU(K)*KOS 5930
     3   (PU39EX(J)+PU41EX(J))) * STI                                   KOS 5940
        RESTB = FAKTOR * CHITH(K) * (CTH(K)*THEX(J)+CU8(K)*U8EX(J)) -   KOS 5950
     1   ZFJ * (CTH(K)*THEX(J)+CU8(K)*U8EX(J))                          KOS 5960
        XREST = XREST + RESTF + RESTB                                   KOS 5970
   30 CONTINUE                                                          KOS 5980
      IF(KUGL .LE. 0) GOTO 38                                           KOS 5990
C                                                                       KOS 6000
C     CORRECTION FOR DISCHARGED SCRAP BATCHES                           KOS 6010
C                                                                       KOS 6020
      N = NDO + 1                                                       KOS 6030
      NDO = NDO + JTYP                                                  KOS 6040
      DO 37 J=N,NDO                                                     KOS 6050
        K = NTYP1(J)                                                    KOS 6060
        L = K                                                           KOS 6070
        NN = 2                                                          KOS 6080
        IF(ABS(TOP-TCHG1(J)) .LT.  T1) NN = 1                           KOS 6090
        IF(K .GT. 0) ZF1 = ZIN(NN,K)                                    KOS 6100
        ZFJ = ZF1 * ZF                                                  KOS 6110
        IF(K .GT. 0) ZF3 = ZEX(K)                                       KOS 6120
        FAKTOR = ZF * ZF3 * VAUF                                        KOS 6130
        IF(K .GT. 0) STX = 1. - SS * TEX(K) / 365.                      KOS 6140
        IF(K .GT. 0) CU5EX = CU5(K)                                     KOS 6150
        IF(NCAN .EQ. 0) GOTO 36                                         KOS 6160
        IF(U5EX(J) .LE. 0.) GOTO 36                                     KOS 6170
        ENU235 = U5EX(J) / (U8EX(J)+U5EX(J)+U6EX(J))                    KOS 6180
C                                                                       KOS 6190
        CALL ENRICO(ENU235,CP,L,NN)                                     KOS 6200
C                                                                       KOS 6210
        CU5EX = CP / ENU235                                             KOS 6220
   36   CONTINUE                                                        KOS 6230
        CU5IN = CU5EX                                                   KOS 6240
        IF(K .LE. 0) GOTO 37                                            KOS 6250
        RESTF = FAKTOR * (CHIU3(K)*CU3(K)*U3EX(J)+CHIU(K)*CU5EX*U5EX(J)+KOS 6260
     1   CHIPU(K)*CPU(K)*(PU39EX(J)+PU41EX(J))) * STX - ZFJ * (CU3(K)*  KOS 6270
     2   U3IN(J)+CU5IN*U5IN(J)+CPU(K)*(PU39IN(J)+PU41IN(J))) * STEX     KOS 6280
        RESTB = FAKTOR * CHITH(K) * (CTH(K)*THEX(J)+CU8(K)*U8EX(J)) -   KOS 6290
     1   ZFJ * (CTH(K)*THIN(J)+CU8(K)*U8IN(J))                          KOS 6300
        XREST = XREST - RESTF - RESTB                                   KOS 6310
   37 CONTINUE                                                          KOS 6320
      OREST = OREST - ORE2(MM)                                          KOS 6330
      SEPRST = SEPRST - SEP2(MM)                                        KOS 6340
   38 CONTINUE                                                          KOS 6350
      RAUF = 0.0                                                        KOS 6360
      DO 31 K=1,MXTYP                                                   KOS 6370
        ZF5 = ZAU(K)                                                    KOS 6380
        RAUF = RAUF + ZF * ZF5 * CAUF(K) * (SMTYP(K)-SMEX(K))           KOS 6390
   31 CONTINUE                                                          KOS 6400
   32 CONTINUE                                                          KOS 6410
C                                                                       KOS 6420
      IF(IAVC .EQ. 0) CALL AVCOST(XREST,RAUF,OREST,SEPRST,AK,DK,FK,BK,QKKOS 6430
     1 ,EKWH,ET,ORE1,ORE2,SEP1,SEP2,NMA)                                KOS 6440
C                                                                       KOS 6450
      JMAF = 0                                                          KOS 6460
      RETURN                                                            KOS 6470
      END                                                               KOS 6480
      SUBROUTINE KOSTIN(TCHG1,N240,NALT,NTYP1,AIN,AEX,VCHG,BK,QK,UUKOST)OST   10
C                                                                       OST   20
CFZJ043                                                       23.09.05  OST   30
C                                                                       OST   40
C     READS INPUT DATA                                                  OST   50
C                                                                       OST   60
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    OST   70
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    OST   80
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIOST   90
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D             OST  100
C                                                                       OST  110
CFZJ043                                                       23.09.05  OST  120
      COMMON /BLOCKK/ JMAF,B,TBURN,AVO,NRCHG,VAUF,NDUM,Z1,Z2,MXTYP,     OST  130
     1 NRTP1(10),NRTP2(10),IALT(10),INEU(10),KSM(10),ETA,IQ,GD,F,GLD,   OST  140
     2 IPRINT,N250,T1,NEXT,DD,TCUM,XLOSS1,NMAF,ND2O,XLOSS2,NCAN,FEED,   OST  150
     3 TAIL,CU3O8,CO8F6,CTRENN,TORE(2,10),TFAB(2,10),TAUF(10),CF,FIF,FITOST  160
     4 ,UNEED(12),SEP(12),EU5(12),CHIU3(10),CHITH(10),CHIPU(10),CHIU(10)OST  170
     5 ,CU3(10),CU5(10),CTH(10),CU8(10),CPU(10),CFAB(10),CAUF(10),CHID2OOST  180
     6 ,CDNEU,CDALT,ZD,SD,VD,YPA,ZL,SS,GMZ,SMOL(10),RES,PUFD,DOUTIN,NULLOST  190
     7 ,Q,QXQ,QQ,CPUST(10),TIN(2,10),TEX(10),SMTYP(10),SMIN(10),SMEX(10)OST  200
     8 ,POWEX,Z3,Z4,Z5,ZIN(2,10),ZFB(2,10),ZEX(10),ZAU(10),ORE(2,10),   OST  210
     9 MZ,NR,JEQQ,XDV,YBV,YPU,YD2O,XFA,YDV,YTH,E,XAU,XTH,YFA,XBAR,YU,BZKOST  220
     X ,XBV,YAU,USUM,XD2O,BZKD2O,PUSUM,SMTOT,VERL,NEWCO,IESC,TOUT       OST  230
C                                                                       OST  240
      INTEGER*4 Q,QXQ,QQ                                                OST  250
C                                                                       OST  260
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, OST  270
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAS,BRUCH,KUGL,JTYP,KLASSE(10),        OST  280
     2 FIMAKL(20),NOPILE,MREP,MARX(10),NAJB(10),FOJB(10),NFUL(10),NBTOT,OST  290
     3 NB0,NCY                                                          OST  300
C                                                                       OST  310
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), OST  320
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10OST  330
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11OST  340
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         OST  350
C                                                                       OST  360
      COMMON /FLUXN/ D(361),IACT                                        OST  370
C                                                                       OST  380
CFZJ043                                                       23.09.05  OST  390
      COMMON /NEWC/ NEWCOST                                             OST  400
C                                                                       OST  410
CFZJ043                                                       23.09.05  OST  420
      DIMENSION TCHG1(N240),NALT(N240),NTYP1(N240),AIN(IACT,N240),      OST  430
     1 AEX(IACT,N240),VCHG(N240),BK(MMAF),QK(MMAF),UUKOST(MMAF)         OST  440
C                                                                       OST  450
      EQUIVALENCE(JTPE2,NS),(JTPE3,NT),(NDUM,MM)                        OST  460
C                                                                       OST  470
 1001 FORMAT (8X,7I4,A4,I4,A4)                                          OST  480
 1002 FORMAT (6E12.5)                                                   OST  490
C                                                                       OST  500
C                                                                       OST  510
      NULL = 0                                                          OST  520
      JMAF = 0                                                          OST  530
      NN = 1                                                            OST  540
C                                                                       OST  550
CARD K1                                                                 OST  560
C                                                                       OST  570
CFZJ043                                                       23.09.05  OST  580
      READ (NS,1001) NMAF,MXTYP,ND2O,NPUFD,IPRINT,IQ,NEWCO,Q,QXQ,QQ     OST  590
C                                                                       OST  600
      IF(QXQ .LE. 0) QXQ = 100                                          OST  610
      DO 103 J=1,MMAF                                                   OST  620
        BK(J) = 0.                                                      OST  630
        QK(J) = 0.                                                      OST  640
  103 CONTINUE                                                          OST  650
      CF = 0.0                                                          OST  660
      PUFD = NPUFD                                                      OST  670
C                                                                       OST  680
CARD K2                                                                 OST  690
C                                                                       OST  700
      READ (NS,1002) F,ETA,GLD,GMZ                                      OST  710
C                                                                       OST  720
CFZJ043                                                       23.09.05  OST  730
      IAVC = 0                                                          OST  740
      IF(GLD .LT. 0.) IAVC = 1                                          OST  750
      GLD = ABS(GLD)                                                    OST  760
      IF(GMZ .EQ. 0.) GMZ = 1.                                          OST  770
      MZ = GMZ                                                          OST  780
C                                                                       OST  790
CARD K3                                                                 OST  800
C                                                                       OST  810
      READ (NS,1002) Z1,Z2,Z4,Z3,Z5,ZL                                  OST  820
C                                                                       OST  830
CARD K4                                                                 OST  840
C                                                                       OST  850
      READ (NS,1002) SS,RES,VERL,YPA,TOUT                               OST  860
C                                                                       OST  870
      IF(RES .EQ. 0.) RES = 1.                                          OST  880
C                                                                       OST  890
CARD K5                                                                 OST  900
C                                                                       OST  910
      READ (NS,1002) CU3O8,CO8F6,CTRENN,TAIL,XLOSS1,XLOSS2              OST  920
C                                                                       OST  930
      FEED = 0.00711                                                    OST  940
C                                                                       OST  950
CARD K6                                                                 OST  960
C                                                                       OST  970
      READ (NS,1002) CTH232,CU233,CPUFIS                                OST  980
C                                                                       OST  990
      NCAN = 0                                                          OST 1000
      DO 25 K=1,MXTYP                                                   OST 1010
C                                                                       OST 1020
CARD K7                                                                 OST 1030
C                                                                       OST 1040
CFZJ043                                                       23.09.05  OST 1050
        READ (NS,1002) ANSM,CU5(K),CU8(K),CFABK,CAUFK                   OST 1060
C                                                                       OST 1070
        IF(NEWCOST .NE. 0) GOTO 26                                      OST 1080
        CFAB(K) = CFABK                                                 OST 1090
        CAUF(K) = CAUFK                                                 OST 1100
        GOTO 27                                                         OST 1110
   26   CONTINUE                                                        OST 1120
        IF(CFABK .GE. 0.) CFAB(K) = CFABK                               OST 1130
        IF(CAUFK .GE. 0.) CAUF(K) = CAUFK                               OST 1140
   27   CONTINUE                                                        OST 1150
        NSM = IFIX(ANSM)                                                OST 1160
        KSM(K) = NSM                                                    OST 1170
C                                                                       OST 1180
CARD K8                                                                 OST 1190
C                                                                       OST 1200
        READ (NS,1002) CHITH(K),CHIU3(K),CHIU(K),CHIPU(K)               OST 1210
C                                                                       OST 1220
CARD K9                                                                 OST 1230
C                                                                       OST 1240
        READ (NS,1002) TORE(2,K),TIN(2,K),TFAB(2,K),TEX(K),TAUF(K)      OST 1250
C                                                                       OST 1260
CARD K10                                                                OST 1270
C                                                                       OST 1280
        READ (NS,1002) TORE(1,K),TIN(1,K),TFAB(1,K)                     OST 1290
C                                                                       OST 1300
        TT = TIN(1,K)                                                   OST 1310
C                                                                       OST 1320
        CALL ZINS(Z1,TT,ZF0)                                            OST 1330
C                                                                       OST 1340
        ZIN(1,K) = ZF0                                                  OST 1350
        TT = TIN(2,K)                                                   OST 1360
C                                                                       OST 1370
        CALL ZINS(Z1,TT,ZF0)                                            OST 1380
C                                                                       OST 1390
        ZIN(2,K) = ZF0                                                  OST 1400
        TT = TFAB(1,K)                                                  OST 1410
C                                                                       OST 1420
        CALL ZINS(Z2,TT,ZF0)                                            OST 1430
C                                                                       OST 1440
        ZFB(1,K) = ZF0                                                  OST 1450
        TT = TFAB(2,K)                                                  OST 1460
C                                                                       OST 1470
        CALL ZINS(Z2,TT,ZF0)                                            OST 1480
C                                                                       OST 1490
        ZFB(2,K) = ZF0                                                  OST 1500
        TT = -TEX(K)                                                    OST 1510
C                                                                       OST 1520
        CALL ZINS(Z3,TT,ZF0)                                            OST 1530
C                                                                       OST 1540
        ZEX(K) = ZF0                                                    OST 1550
        TT = -TAUF(K)                                                   OST 1560
C                                                                       OST 1570
        CALL ZINS(Z5,TT,ZF0)                                            OST 1580
C                                                                       OST 1590
        ZAU(K) = ZF0                                                    OST 1600
        IF(CU8(K) .GT. 0.) GOTO 21                                      OST 1610
        NCAN = 1                                                        OST 1620
        TT = TORE(1,K) - TIN(1,K)                                       OST 1630
C                                                                       OST 1640
        CALL ZINS(Z1,TT,ZF0)                                            OST 1650
C                                                                       OST 1660
        ORE(1,K) = CU3O8 * ZF0                                          OST 1670
        TT = TORE(2,K) - TIN(2,K)                                       OST 1680
C                                                                       OST 1690
        CALL ZINS(Z1,TT,ZF0)                                            OST 1700
C                                                                       OST 1710
        ORE(2,K) = CU3O8 * ZF0                                          OST 1720
        L = K                                                           OST 1730
        IF(L .GT. 1) GOTO 20                                            OST 1740
        ENU235 = 0.93                                                   OST 1750
C                                                                       OST 1760
        CALL ENRICO(ENU235,CP,L,2)                                      OST 1770
C                                                                       OST 1780
        CP = CP / 0.93                                                  OST 1790
        IF(CU233 .LT. 0.) CU233 = CP * ABS(CU233)                       OST 1800
        IF(CPUFIS .LT. 0.) CPUFIS = CP * ABS(CPUFIS)                    OST 1810
   20   CONTINUE                                                        OST 1820
        CU8(K) = 0.                                                     OST 1830
        ENU235 = CU5(K)                                                 OST 1840
        EU5(K) = CU5(K) * 100.                                          OST 1850
C                                                                       OST 1860
        CALL ENRICO(ENU235,CP,L,NN)                                     OST 1870
C                                                                       OST 1880
        CU5(K) = CP / CU5(K)                                            OST 1890
   21   CONTINUE                                                        OST 1900
        CU3(K) = CU233                                                  OST 1910
        CPU(K) = CPUFIS                                                 OST 1920
        CTH(K) = CTH232                                                 OST 1930
   25 CONTINUE                                                          OST 1940
CFZJ043                                                       23.09.05  OST 1950
      NEWCOST = 0                                                       OST 1960
C                                                                       OST 1970
CARD K11                                                                OST 1980
C                                                                       OST 1990
      IF(PUFD .NE. 0.) READ (NS,1002) (UUKOST(I),I=1,NMAF)              OST 2000
C                                                                       OST 2010
      IF(ND2O .EQ. 0) RETURN                                            OST 2020
C                                                                       OST 2030
CARD K12                                                                OST 2040
C                                                                       OST 2050
      READ (NS,1002) DOUTIN,CDNEU,CDALT,ZD,SD,VD                        OST 2060
C                                                                       OST 2070
CFZJ043                                                       23.09.05  OST 2080
      RETURN                                                            OST 2090
      END                                                               OST 2100