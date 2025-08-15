      BLOCK DATA                                                        CK    10
C                                                                       CK    20
      COMMON /BASIS/ TBASE                                              CK    30
C                                                                       CK    40
      COMMON /BEZ/ PKOM(12),UET(12)                                     CK    50
C                                                                       CK    60
      COMMON /RSTRT/ IFRSTA,ZRST,ILOGR                                  CK    70
C                                                                       CK    80
      COMMON /ERROR/ ABTEXT(20),IERRM,IERRL,IERRF(5),NHIN,NWARN,NFEHL   CK    90
C                                                                       CK   100
      COMMON /PHI98/ PHIA,PHIE,IOPEN                                    CK   110
C                                                                       CK   120
      CHARACTER*4 ABTEXT/'PROG','RAM ','STOP','S BE','CAUS','E MA',     CK   130
     1 'XIMU','M ER','ROR ','LEVE','L IS',' REA','CHED',7*'    '/,PKOM/ CK   140
     2 'COMP','OSIT','IONS',9*'    '/,UET/'TEMP','ERAT','URES',' (DE',  CK   150
     3 'G C)',7*'    '/                                                 CK   160
C                                                                       CK   170
      DATA ILOGR/5/,IERRF/1,2,5,25,1000/,TBASE/0./,IOPEN/0/             CK   180
      END                                                               CK   190
      SUBROUTINE MAITHX(N200,NXS,NDR,KMAT,JN,JNSTOP,RADP,RAD,PHI,NKOM,  THX   10
     1 KOM,KART,T,WI,WT,TQ,IFBER,IFBH,KKB,NHZON,IKO,QX,STRAHL,MIX,NRG,  THX   20
     2 AGEFAC,VREG,VRP)                                                 THX   30
C                                                                       THX   40
CFZJ042                                                       09.09.05  THX   50
C                                                                       THX   60
C     MAIN OF THERMIX SECTION                                           THX   70
C                                                                       THX   80
      COMMON /ERROR/ ABTEXT(20),IERRM,IERRL,IERRF(5),NHIN,NWARN,NFEHL   THX   90
C                                                                       THX  100
CFZJ004 enlarged dimensions common trans                      28.11.03  THX  110
      COMMON /TRANS/ IFINST,INTVAL,DZEIT(300),ZEI(300),NPRINT(300),     THX  120
     1 NKONV(300)                                                       THX  130
C                                                                       THX  140
CFZJ042                                                       09.09.05  THX  150
      COMMON /PLOT/ IPUN                                                THX  160
C                                                                       THX  170
      COMMON /BEZ/ PKOM(12),UET(12)                                     THX  180
C                                                                       THX  190
      COMMON /PRINT1/ TITLE(20),INDGEO                                  THX  200
C                                                                       THX  210
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    THX  220
C                                                                       THX  230
      COMMON /KOMP1/ KMAX                                               THX  240
C                                                                       THX  250
      COMMON /ITER1/ TRMAX,MIT,TN,OVREL,ITMAX,T1,T2,TRELA,NTMAX,IFKOR,  THX  260
     1 ETHA,ORMIN,MITMAX,IKORM,IFREL                                    THX  270
C                                                                       THX  280
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                THX  290
C                                                                       THX  300
      COMMON /SPECTI/ ITIK(10)                                          THX  310
C                                                                       THX  320
      COMMON /OPT/ KENN,IOPUT,NIFKO,DZT,KONIN,NI                        THX  330
C                                                                       THX  340
      COMMON /RSTRT/ IFRSTA,ZRST                                        THX  350
C                                                                       THX  360
CFZJ006 enlarged dimensions common QVAR                       28.11.03  THX  370
      COMMON /QVAR/ DUM(1817),JRESTW,JRESTR,JREST,TAU,D(311),NJ         THX  380
C                                                                       THX  390
CFZJ042                                                       09.09.05  THX  400
      COMMON /BLINDL/ TMITL,M24,NGEOM,CIZET0                            THX  410
C                                                                       THX  420
      COMMON /KSUM/ ITI,DU,SIG,VORZ,BKV,RHOKS,RL,SM,BURN,DI,A0          THX  430
C                                                                       THX  440
CFZJ005 enlarged dimensions common SPEIKO                     28.11.03  THX  450
      COMMON /SPEIKO/ F(300,9),MX,MM,TMADBH(300),TMIDBH(300)            THX  460
C                                                                       THX  470
CFZJ048 enlarged dimension                                    11.04.07  THX  480
      COMMON /VARDIT/ B(5000000)                                        THX  490
C                                                                       THX  500
CFZJ042                                                       09.09.05  THX  510
      COMMON /ADDRT/ KX(240),KY(240),LZ(240),NENDPT                     THX  520
C                                                                       THX  530
CFZJ055                                                       25.09.07  THX  540
C                                                                       THX  550
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             THX  560
C                                                                       THX  570
      COMMON /COUPL/ IPRINT,IPRS                                        THX  580
C                                                                       THX  590
      COMMON /PHI98/ PHIA,PHIE,IOPEN                                    THX  600
C                                                                       THX  610
      COMMON /PDTX/ PDTHX,RENORM,WQSUM,QNORM1,INORM                     THX  620
C                                                                       THX  630
CFZJ042                                                       09.09.05  THX  640
      DIMENSION PLAM(12),TEXT(20),RADP(IMAZ),RAD(IMAZ+1),PHI(NMAZ+1),   THX  650
     1 NKOM(KMAZ),KOM(IMAZ,NMAZ),KART(KMAZ),T(IMAZ,NMAZ),WI(IMAZ,NMAZ), THX  660
     2 WT(IMAZ,NMAZ),TQ(IMAZ,NMAZ),IFBER(IMAZ,NMAZ),IFBH(ICO,NCO),      THX  670
     3 KKB(ICO,NCO),NHZON(KMAZ),IKO(KMAZ),QX(KMAZ),STRAHL(IMAZ,NMAZ),   THX  680
     4 PLAMS(12),MIX(NDR),NRG(N200),AGEFAC(N200),VREG(NDR),VRP(NDR)     THX  690
C                                                                       THX  700
      CHARACTER*4 TEXT/'CALC','ULAT','ION ','SUCC','ESSF','ULLY',' FIN',THX  710
     1 'ISHE','D - ','CONG','RATU','LATI','ONS ',7*'    '/,PLAMS/' LAM',THX  720
     2 'BDA ','(E-3',' W/C','M/K)',', RA','DIAT','ION ',4*'    '/,PLAM/ THX  730
     3 ' LAM','BDA ','(E-3',' W/C','M/K)',', CO','NDUC','TION',' + C',  THX  740
     4 'ONVE','CTIO','N   '/,UET                                        THX  750
C                                                                       THX  760
CFZJ042                                                       09.09.05  THX  770
      EQUIVALENCE(LZ(20),IPOV),(LZ(21),IZF),(LZ(22),INRY),(LZ(23),IQNW),THX  780
     1 (LZ(52),IHEPS),(LZ(53),IHKUG),(LZ(54),IDI),(LZ(55),INHM1),       THX  790
     2 (LZ(56),INHM2),(LZ(57),IXFWQ),(LZ(58),IIFHE),(LZ(59),IIFWK),     THX  800
     3 (LZ(60),IIFLT),(LZ(61),IWPR),(LZ(62),JDOS),(LZ(63),IPHIP),       THX  810
     4 (LZ(64),IDR),(LZ(65),IDPH),(LZ(66),ILAM),(LZ(67),ITVOR),         THX  820
     5 (LZ(68),IALP),(LZ(69),IRE),(LZ(70),IIFTV),(LZ(72),IRHO),         THX  830
     6 (LZ(73),IC),(LZ(74),IEPS),(LZ(75),IIDIR),(LZ(76),IIFAN),         THX  840
     7 (LZ(77),IZEIV),(LZ(78),ITKV),(LZ(79),INTVA),(LZ(86),IMFR),       THX  850
     8 (LZ(87),IMFZ),(LZ(89),IWQ),(LZ(90),IWQR),(LZ(91),IE),            THX  860
     9 (LZ(92),IBU1),(LZ(95),JRE),(LZ(233),IDOST),(LZ(234),ITHEN),      THX  870
     X (LZ(96),IPHE),(LZ(97),IWG),(LZ(98),IWS),(LZ(99),IXE),            THX  880
     Y (LZ(111),IZDOS),(LZ(102),ITFLU),(LZ(112),IZD),(LZ(113),IZH),     THX  890
     Z (LZ(114),IAU),(LZ(115),IISO),(LZ(116),IISU),(LZ(117),INSL)       THX  900
      EQUIVALENCE(LZ(118),INSR),(LZ(100),IDOSI),(LZ(119),IKSTR),        THX  910
     1 (LZ(120),IVOLK),(LZ(121),IVOLS),(LZ(122),IIFFU),(LZ(123),IVOLZ), THX  920
     2 (LZ(124),IVK),(LZ(125),IXFAK),(LZ(126),IBU),(LZ(232),IDOSP),     THX  930
     3 (LZ(127),IBR),(LZ(129),IWWK),(LZ(130),IWL),(LZ(231),IPBET),      THX  940
     4 (LZ(131),IAR),(LZ(132),IDROH),(LZ(133),IQTV),(LZ(134),ILTV),     THX  950
     5 (LZ(135),IXGEO),(LZ(136),INRHL),(LZ(137),IIFB),(LZ(138),IXKK),   THX  960
     6 (LZ(139),IMR),(LZ(140),IP),(LZ(141),IKON),(LZ(142),IPVOR),       THX  970
     7 (LZ(143),IIFBR),(LZ(144),IALPH),(LZ(145),IIFBQ),(LZ(146),IALFI), THX  980
     8 (LZ(81),IR),(LZ(82),IZ),(LZ(149),IFZQ),(LZ(150),IFRQ),           THX  990
     9 (LZ(151),IDRK),(LZ(84),IDZ),(LZ(153),IFZQ1),(LZ(154),IFRQ1),     THX 1000
     X (LZ(155),IZPK),(LZ(156),IRP),(LZ(157),IEPSI),(LZ(158),IXKON),    THX 1010
     Y (LZ(159),IDHYD),(LZ(160),ISTZU),(LZ(161),ITFLV),(LZ(162),IIFZS), THX 1020
     Z (LZ(163),IIFZT),(LZ(164),IVOL),(LZ(165),IIOVE),(LZ(166),IIUVE)   THX 1030
      EQUIVALENCE(LZ(167),IXVER),(LZ(168),IIOKU),(LZ(170),IXKUL),       THX 1040
     1 (LZ(171),IXZSU),(LZ(172),IXNSU),(LZ(173),ITFL),(LZ(174),ISTRB),  THX 1050
     2 (LZ(175),IFQRO),(LZ(176),IZKUG),(LZ(177),IWKAP),(LZ(178),ITHAL), THX 1060
     3 (LZ(179),IWQN),(LZ(180),IWQK),(LZ(103),IDU),(LZ(104),JTGRA),     THX 1070
     4 (LZ(105),ITFUL),(LZ(182),ICU),(LZ(183),IQSPE),(LZ(230),IDOSD),   THX 1080
     5 (LZ(184),IQKON),(LZ(185),IQNUK),(LZ(186),ITKT),(LZ(187),IJUG),   THX 1090
     6 (LZ(188),IQQUE),(LZ(189),IBUIN),(LZ(190),IQSP1),(LZ(192),IQKO1), THX 1100
     7 (LZ(193),IFAK1),(LZ(194),IFAK2),(LZ(195),ITKOI),(LZ(196),ITKOA), THX 1110
     8 (LZ(197),IVKOM),(LZ(169),IIUKU),(LZ(1),IVL),(LZ(229),IVOLT),     THX 1120
     9 (LZ(215),IIPAR),(LZ(216),IJPAR),(LZ(217),INPAR),(LZ(228),IPOWT), THX 1130
     X (LZ(218),IXKP),(LZ(219),IROGP),(LZ(88),IPDTH),(LZ(221),IITK),    THX 1140
     Y (LZ(222),IKOC),(LZ(223),IKG),(LZ(225),ITCON),(LZ(224),IFFTH),    THX 1150
     Z (LZ(226),IPOWP),(LZ(227),IVOLP)                                  THX 1160
CFZJ023   NEW ARRAY FFTHX                                     02.02.04  THX 1170
CFZJ024   NEW ARRAY TCOND                                     02.02.04  THX 1180
C                                                                       THX 1190
   77 FORMAT (//' DISPLACEMENT OF CORE-RANGE MESHES AGAINST THERMIX-RANGTHX 1200
     1E MESHES: IDIFF (RADIAL) =',I3,'  NDIFF (AXIAL) =',I3/)           THX 1210
   78 FORMAT (' NUMBER OF CORE-RANGE MESHES: (RADIAL) =',I3,'  (AXIAL) =THX 1220
     1',I3)                                                             THX 1230
   85 FORMAT (/' **START TIME VORZ =',E12.5,' HOURS. FOLLOWING INFORMATITHX 1240
     1ON ON TIME REFERS TO THE BEGIN ZRST =',E12.5,' HOURS**'/)         THX 1250
  101 FORMAT (I6,3E12.5)                                                THX 1260
  111 FORMAT ((3(E10.3,E10.3)))                                         THX 1270
  113 FORMAT (72I1)                                                     THX 1280
  200 FORMAT (7I10)                                                     THX 1290
  201 FORMAT (7(I5,I2,I3))                                              THX 1300
  202 FORMAT (2E10.3,8I5)                                               THX 1310
  801 FORMAT (/' ATTENTION: VALIDITY AREA OF SM FOR THE CALCULATION OF ATHX 1320
     1CTIVATION PRODUCTS IS EXCEEDED.'/'            THE PROGRAM SETS SM THX 1330
     2=',F8.3)                                                          THX 1340
C                                                                       THX 1350
C                                                                       THX 1360
CFZJ042                                                       09.09.05  THX 1370
      IPR = 0                                                           THX 1380
      IRII = 0                                                          THX 1390
CFZJ055                                                       25.09.07  THX 1400
      IFDF = 0                                                          THX 1410
      NGEOM = 37                                                        THX 1420
      IERRM = 300                                                       THX 1430
      IERRL = 0                                                         THX 1440
      NHIN = 0                                                          THX 1450
      NWARN = 0                                                         THX 1460
      NFEHL = 0                                                         THX 1470
      PDTHX = 0.                                                        THX 1480
CFZJ038                                                                 THX 1490
      RENORM = 1.                                                       THX 1500
      INORM = 0                                                         THX 1510
      IF(ITIK(1) .NE. 1) GOTO 1                                         THX 1520
      ITI = ITIK(1)                                                     THX 1530
      WTGINT = 0.                                                       THX 1540
    1 CONTINUE                                                          THX 1550
      IRI = 0                                                           THX 1560
      IF(ITIK(1) .GT. 1 .AND. ITIK(10) .EQ. 1) IRI = 1                  THX 1570
      IF(ITIK(9) .GT. 0 .OR. IRI .EQ. 1) IRII = 1                       THX 1580
      MIT = 0                                                           THX 1590
      QX1 = 0.                                                          THX 1600
C                                                                       THX 1610
CARDS TX1 - TX13                                                        THX 1620
C                                                                       THX 1630
CFZJ042                                                       09.09.05  THX 1640
      CALL EINL1(ITLAM,IFKON,TDIFF,NLOOP,QNORM,IFRED,NHET,CP0,IFTEST,   THX 1650
     1 IFDF,IEXPR,B(KX(IHEPS)),B(KX(IHKUG)),B(KX(IDI)),B(KX(INHM1)),    THX 1660
     2 B(KX(INHM2)),NHZON,B(KX(IXFWQ)),B(KX(IIFHE)),B(KX(IIFWK)),       THX 1670
     3 B(KX(IIFLT)),B(KX(IWPR)),B(KX(JDOS)),IFBER,WI,B(KX(IPHIP)),RADP, THX 1680
     4 B(KX(IDR)),B(KX(IDPH)),RAD,PHI,KOM,B(KX(ILAM)),B(KX(ITVOR)),     THX 1690
     5 B(KX(IALP)),B(KX(IRE)),B(KX(IIFTV)),KART,B(KX(IRHO)),B(KX(IC)),  THX 1700
     6 B(KX(IEPS)),B(KX(IIDIR)),B(KX(IIFAN)),B(KX(IZEIV)),B(KX(ITKV)),  THX 1710
     7 B(KX(INTVA)),B(KX(IMFR)),B(KX(IMFZ)),ITM3,B(KX(IPVOR)),          THX 1720
     8 B(KX(IIFBR)),B(KX(IALPH)),B(KX(IIFBQ)),B(KX(IEPSI)),B(KX(IXKON)),THX 1730
     9 B(KX(IDHYD)),B(KX(ISTZU)),B(KX(ITFLV)),B(KX(IIFZS)),B(KX(IIFZT)),THX 1740
     X B(KX(IITK)),B(KX(IKOC)),B(KX(IKG)),B(KX(IFFTH)),IRII)            THX 1750
CFZJ023                                                       02.02.04  THX 1760
CFZJ038                                                       14.12.04  THX 1770
      IF(ITIK(9) .GT. 0 .OR. IRI .EQ. 1) GOTO 50                        THX 1780
      IM1 = IMAX - 1                                                    THX 1790
      NM1 = NMAX - 1                                                    THX 1800
      DO 2 K=1,KMAX                                                     THX 1810
        QX(K) = 0.                                                      THX 1820
    2 CONTINUE                                                          THX 1830
CFZJ042                                                       09.09.05  THX 1840
   50 CONTINUE                                                          THX 1850
      NENDK0 = NENDPT + 1                                               THX 1860
      IF(ITIK(9) .GT. 0 .OR. IRI .EQ. 1) GOTO 95                        THX 1870
C                                                                       THX 1880
      CALL SUCHET(NHET,B(KX(IHKUG)),B(KX(IDI)),NHZON,B(KX(IXFWQ)),      THX 1890
     1 B(KX(IIFHE)),B(KX(IVOLK)),B(KX(IVOLS)),B(KX(IIFFU)),B(KX(IVOLZ)),THX 1900
     2 B(KX(IVK)),IFBH,KKB,KOM,B(KX(IXFAK)))                            THX 1910
C                                                                       THX 1920
   95 CONTINUE                                                          THX 1930
C                                                                       THX 1940
CFZJ042                                                       09.09.05  THX 1950
      CALL SETE(QNORM,N200,NDR,DELDZ,QTHX,KMAT,B(KX(IPHIP)),RADP,KOM,   THX 1960
     1 KART,B(KX(IWPR)),T,B(KX(IWQ)),B(KX(IWQR)),B(KX(IE)),TQ,          THX 1970
     2 B(KX(IBU1)),RAD,PHI,B(KX(JRE)),B(KX(IPHE)),B(KX(IWG)),B(KX(IWS)),THX 1980
     3 B(KX(IXE)),B(KX(IIFHE)),MIX,NRG,B(KX(IPDTH)),B(KX(IPOWP)),       THX 1990
     4 B(KX(IVOLP)),B(KX(IPOWT)),B(KX(IVOLT)),AGEFAC,B(KX(IVL)),        THX 2000
     5 B(KX(IDOSD)),B(KX(IDOSP)),B(KX(IDOST)),VREG)                     THX 2010
C                                                                       THX 2020
      IF(ITIK(9) .GT. 0 .OR. IRI .EQ. 1) GOTO 97                        THX 2030
      DO 75 I=1,IM1                                                     THX 2040
        DO 75 N=1,NM1                                                   THX 2050
          IFBER(I,N)= 5                                                 THX 2060
   75 CONTINUE                                                          THX 2070
   97 CONTINUE                                                          THX 2080
C                                                                       THX 2090
      CALL VORKON(RAD,PHI,B(KX(IAU)),RADP)                              THX 2100
C                                                                       THX 2110
      IF(ITIK(9) .GT. 0 .OR. IRI .EQ. 1) GOTO 98                        THX 2120
C                                                                       THX 2130
      CALL SETSTR(KOM,B(KX(IEPS)),B(KX(IIDIR)),B(KX(IISO)),B(KX(IISU)), THX 2140
     1 B(KX(INSL)),B(KX(INSR)),B(KX(IKSTR)))                            THX 2150
C                                                                       THX 2160
      IF(NHET .EQ. 0) GOTO 40                                           THX 2170
      WRITE (6,77) IDIFF,NDIFF                                          THX 2180
      WRITE (6,78) IMH-1,NMH-1                                          THX 2190
   40 CONTINUE                                                          THX 2200
C                                                                       THX 2210
CARDS TX15 - TX17                                                       THX 2220
C                                                                       THX 2230
CFZJ024   NEW ARRAY TCOND                                     02.02.04  THX 2240
CFZJ026                                                       16.03.04  THX 2250
CFZJ042                                                       09.09.05  THX 2260
      CALL SETT(NHET,WTGINT,NHZON,IFBH,KKB,T,WT,KOM,B(KX(ITVOR)),RAD,PHITHX 2270
     1 ,B(KX(IBU)),B(KX(IBR)),B(KX(JRE)),B(KX(IPHE)),B(KX(IWG)),        THX 2280
     2 B(KX(IWS)),B(KX(IFFTH)),B(KX(ITCON)),B(KX(IVOLT)),B(KX(IPOWT)),  THX 2290
     3 B(KX(IDOST)),B(KX(ITHEN)))                                       THX 2300
C                                                                       THX 2310
      IF(ITIK(1) .NE. 1) GOTO 100                                       THX 2320
      DO 79 I=1,KMAX                                                    THX 2330
        IKO(I) = 0                                                      THX 2340
   79 CONTINUE                                                          THX 2350
      MX = 0                                                            THX 2360
      MC2 = 0                                                           THX 2370
C                                                                       THX 2380
CARD TX18                                                               THX 2390
C                                                                       THX 2400
CFZJ022       INPUT DATA CHANGE                               28.01.04  THX 2410
      IF(IFINST .NE. 0) READ (5,101) MC2,SIG,A0,SM                      THX 2420
      MZNORM = 0                                                        THX 2430
C                                                                       THX 2440
C     GUELTIGKEITSBEREICHE FUER RL, SM, BURN                            THX 2450
C                                                                       THX 2460
      IF(IFINST .EQ. 0) GOTO 903                                        THX 2470
      IF(A0 .LE. 0.) A0 = 10.                                           THX 2480
      IF(SM .GE. 4. .AND. SM .LE. 12.) GOTO 903                         THX 2490
      IF(SM .GT. 4.) GOTO 902                                           THX 2500
      SM = 4.                                                           THX 2510
      WRITE (6,801) SM                                                  THX 2520
      GOTO 903                                                          THX 2530
  902 CONTINUE                                                          THX 2540
      SM = 12.                                                          THX 2550
      WRITE (6,801) SM                                                  THX 2560
  903 CONTINUE                                                          THX 2570
      IF(MC2 .LE. 0) GOTO 74                                            THX 2580
C                                                                       THX 2590
CARD TX19                                                               THX 2600
C                                                                       THX 2610
      READ (5,113) (IKO(I),I=1,KMAX)                                    THX 2620
C                                                                       THX 2630
      DO 73 I=1,KMAX                                                    THX 2640
        IF(IKO(I) .NE. 0) IKO(I) = IKO(I) + 4                           THX 2650
        MX = MAX0(MX,IKO(I))                                            THX 2660
   73 CONTINUE                                                          THX 2670
   74 CONTINUE                                                          THX 2680
      VORZ = 0.                                                         THX 2690
      IF(MZNORM .LE. 0) GOTO 80                                         THX 2700
      VORZ = ZRST                                                       THX 2710
      ZRST = 0.                                                         THX 2720
      WRITE (6,85) VORZ,ZRST                                            THX 2730
   80 CONTINUE                                                          THX 2740
C                                                                       THX 2750
      CALL TEMPK0(B(KX(IIFLT)))                                         THX 2760
C                                                                       THX 2770
  100 CONTINUE                                                          THX 2780
C                                                                       THX 2790
CFZJ026                                                       16.03.04  THX 2800
      CALL KONST1(1,IFBER,B(KX(IEPS)),B(KX(IIDIR)),B(KX(IIFAN)),        THX 2810
     1 B(KX(IHKUG)),B(KX(IDI)),NHZON,B(KX(IIFHE)),B(KX(IWWK)),B(KX(IDR))THX 2820
     2 ,B(KX(IDPH)),RAD,KOM,B(KX(IALP)),KART,WI,B(KX(IWL)),WT,B(KX(IAR))THX 2830
     3 ,RADP,B(KX(IISO)),B(KX(IISU)),B(KX(INSL)),B(KX(INSR)),           THX 2840
     4 B(KX(IKSTR)),B(KX(JDOS)),B(KX(IZDOS)),B(KX(IIFLT)),T,PHI,        THX 2850
     5 B(KX(ILAM)),B(KX(IDOSI)),B(KX(IFFTH)),B(KX(ITCON)))              THX 2860
C                                                                       THX 2870
      CALL SYMBOL(B(KX(IEPS)),WI,B(KX(IWL)),IFBER,B(KX(IIFLT)),KOM,KART)THX 2880
C                                                                       THX 2890
CFZJ042                                                       09.09.05  THX 2900
   98 CONTINUE                                                          THX 2910
      IF(ITIK(9) .GT. 0 .AND. KONIN .EQ. 0) GOTO 96                     THX 2920
      IF(IRI .EQ. 1 .AND. NI .LE. 0) GOTO 96                            THX 2930
C                                                                       THX 2940
CARDS TX25 - TX26                                                       THX 2950
C                                                                       THX 2960
      CALL EINL(IFZW,ITM3,B(KX(IDROH)),B(KX(IQTV)),B(KX(ILTV)),         THX 2970
     1 B(KX(IXGEO)),B(KX(INRHL)),B(KX(IIFB)),B(KX(IXKK)),B(KX(IMR)),    THX 2980
     2 B(KX(IP)),B(KX(IKON)),B(KX(IPVOR)),B(KX(IIFBR)),B(KX(IALPH)),    THX 2990
     3 B(KX(IIFBQ)),B(KX(IALFI)),B(KX(IR)),B(KX(IZ)),B(KX(IFZQ)),       THX 3000
     4 B(KX(IFRQ)),B(KX(IDRK)),B(KX(IDZ)),B(KX(IFZQ1)),B(KX(IFRQ1)),    THX 3010
     5 B(KX(IZPK)),B(KX(IRP)),B(KX(IEPSI)),B(KX(IXKON)),B(KX(IDHYD)),   THX 3020
     6 B(KX(ISTZU)),B(KX(ITFLV)),B(KX(IIFZS)),B(KX(IIFZT)),B(KX(IVOL)), THX 3030
     7 B(KX(IIOVE)),B(KX(IIUVE)),B(KX(IXVER)),B(KX(IIOKU)),B(KX(IIUKU)),THX 3040
     8 B(KX(IXKUL)),B(KX(IXZSU)),B(KX(IXNSU)),B(KX(ITFL)),B(KX(ISTRB)), THX 3050
     9 B(KX(IFQRO)),B(KX(IIPAR)),B(KX(IJPAR)),B(KX(INPAR)))             THX 3060
C                                                                       THX 3070
      KONIN = 0                                                         THX 3080
C                                                                       THX 3090
      IF(IFREF .EQ. 1) CALL REFL(T)                                     THX 3100
C                                                                       THX 3110
   96 CONTINUE                                                          THX 3120
C                                                                       THX 3130
CFZJ026                                                       16.03.04  THX 3140
CFZJ042                                                       09.09.05  THX 3150
      CALL STEUER(IFKON,ITLAM,TDIFF,NLOOP,IFRED,IFZW,ITM3,CP0,IFTEST,   THX 3160
     1 NHET,ZEITH,IEXPR,N200,NDR,NXS,B(KX(IPOV)),B(KX(IZF)),B(KX(INRY)),THX 3170
     2 B(KX(IQNW)),DELDZ,QTHX,WTGINT,B(KX(IHKUG)),B(KX(IDI)),NHZON,     THX 3180
     3 B(KX(IXFWQ)),B(KX(IIFHE)),B(KX(IZKUG)),IFBH,B(KX(IWKAP)),        THX 3190
     4 B(KX(ITHAL)),B(KX(IWQN)),B(KX(IWQK)),KKB,IFBER,B(KX(IAU)),       THX 3200
     5 B(KX(IAR)),B(KX(IBU)),B(KX(IBR)),B(KX(IZEIV)),B(KX(ITKV)),       THX 3210
     6 B(KX(INTVA)),RAD,PHI,KOM,KART,T,WI,B(KX(IWL)),WT,TQ,B(KX(ITFLU)),THX 3220
     7 B(KX(IBU1)),B(KX(IDU)),B(KX(JTGRA)),B(KX(ITFUL)),B(KX(ISTZU)),   THX 3230
     8 B(KX(ITFLV)),IKO,B(KX(ICU)),B(KX(IQSPE)),B(KX(IQKON)),           THX 3240
     9 B(KX(IQNUK)),B(KX(ITKT)),B(KX(IJUG)),B(KX(IQQUE)),B(KX(IBUIN)),  THX 3250
     X B(KX(IQSP1)),QX,B(KX(IQKO1)),B(KX(IFAK1)),B(KX(IFAK2)),          THX 3260
     Y B(KX(ITKOI)),B(KX(ITKOA)),B(KX(IVKOM)),B(KX(IHEPS)),B(KX(INHM2)),THX 3270
     Z B(KX(IVOLK)),B(KX(IVOLZ)),B(KX(IRHO)),B(KX(IC)),B(KX(IIFWK)),    THX 3280
     Z B(KX(IVOLS)),B(KX(IIFFU)),B(KX(IWWK)),B(KX(INHM1)),B(KX(JDOS)),  THX 3290
     Z B(KX(IZDOS)),B(KX(IDPH)),B(KX(ILAM)),B(KX(IPHIP)),B(KX(IE)),     THX 3300
     Z B(KX(IIPAR)),B(KX(IJPAR)),B(KX(INPAR)),B(KX(IXKP)),B(KX(IROGP)), THX 3310
     Z KMAT,MIX,B(KX(IFFTH)),B(KX(ITCON)),B(KX(IPOWT)),B(KX(IVOLT)),    THX 3320
     Z B(KX(IPBET)),B(KX(IDOST)),B(KX(ITHEN)),VRP,VREG)                 THX 3330
C                                                                       THX 3340
      IF(JN .LT. JNSTOP .AND. NJ .GT. 1) GOTO 99                        THX 3350
C                                                                       THX 3360
      CALL KUEHLK(0,QX,IFINST,QX1,B(KX(IDR)),B(KX(IDPH)),RAD,B(KX(IAR)),THX 3370
     1 KOM,KART,T,WI,B(KX(IWL)),WT,IFBER)                               THX 3380
C                                                                       THX 3390
      IF(INDGEO .NE. 1 .OR. RAD0 .NE. 0.) GOTO 102                      THX 3400
C                                                                       THX 3410
C     SETZEN DER ZENTRALTEMPERATUR                                      THX 3420
C                                                                       THX 3430
      RR = RADP(2)                                                      THX 3440
      R = RAD(2)                                                        THX 3450
      PG = 0.                                                           THX 3460
      TZG = 0.                                                          THX 3470
      DO 103 N=2,NM1                                                    THX 3480
        K = KOM(1,N)                                                    THX 3490
        IF(KART(K) .NE. 3) GOTO 1033                                    THX 3500
        DPP = (PHI(N+1)-PHI(N-1)) / 2.                                  THX 3510
        TZ = TQ(2,N) * 2. / (RR*RR*DPP)                                 THX 3520
        PG = PG + DPP                                                   THX 3530
C                                                                       THX 3540
CFZJ026                                                       16.03.04  THX 3550
        XLA = XLAM(1,N,XLAMS,B(KX(IISO)),B(KX(IISU)),B(KX(INSL)),       THX 3560
     1   B(KX(INSR)),B(KX(IKSTR)),B(KX(JDOS)),B(KX(IZDOS)),B(KX(IEPS)), THX 3570
     2   B(KX(IIDIR)),B(KX(IIFLT)),RADP,T,RAD,PHI,KOM,B(KX(ILAM)),      THX 3580
     3   B(KX(IDOSI)),B(KX(IFFTH)),B(KX(ITCON)))                        THX 3590
C                                                                       THX 3600
        TZ = TZ * R * R / (4.*XLA) + T(2,N)                             THX 3610
 1033   TZG = TZG + TZ * DPP                                            THX 3620
  103 CONTINUE                                                          THX 3630
      IF(PG .EQ. 0.) GOTO 102                                           THX 3640
      TZ = TZG / PG                                                     THX 3650
      DO 104 N=1,NMAX                                                   THX 3660
        T(1,N) = TZ                                                     THX 3670
  104 CONTINUE                                                          THX 3680
  102 CONTINUE                                                          THX 3690
      DO 105 I=1,IM1                                                    THX 3700
        DO 105 N=1,NM1                                                  THX 3710
C                                                                       THX 3720
CFZJ026                                                       16.03.04  THX 3730
          XLA = XLAM(I,N,XLAMS,B(KX(IISO)),B(KX(IISU)),B(KX(INSL)),     THX 3740
     1     B(KX(INSR)),B(KX(IKSTR)),B(KX(JDOS)),B(KX(IZDOS)),B(KX(IEPS))THX 3750
     2     ,B(KX(IIDIR)),B(KX(IIFLT)),RADP,T,RAD,PHI,KOM,B(KX(ILAM)),   THX 3760
     3     B(KX(IDOSI)),B(KX(IFFTH)),B(KX(ITCON)))                      THX 3770
C                                                                       THX 3780
          STRAHL(I,N) = XLAMS * 1000.                                   THX 3790
          WI(I,N) = XLA * 1000.                                         THX 3800
  105 CONTINUE                                                          THX 3810
C                                                                       THX 3820
      IF(IPRINT .LT. -1) GOTO 121                                       THX 3830
C                                                                       THX 3840
      CALL MARK(0,KOM,KART)                                             THX 3850
C                                                                       THX 3860
      CALL PRFELD(WI,IM1,NM1,PLAM,0,PHI,RAD,1)                          THX 3870
C                                                                       THX 3880
      IF(IPRS .EQ. 0) GOTO 120                                          THX 3890
C                                                                       THX 3900
      CALL MARK(0,KOM,KART)                                             THX 3910
C                                                                       THX 3920
      CALL PRFELD(STRAHL,IM1,NM1,PLAMS,0,PHI,RAD,1)                     THX 3930
C                                                                       THX 3940
  120 CONTINUE                                                          THX 3950
C                                                                       THX 3960
      CALL MARK(1,KOM,KART)                                             THX 3970
C                                                                       THX 3980
      TMI = 1.E10                                                       THX 3990
      TMA = -1.E10                                                      THX 4000
      DO 106 I=1,IMAX                                                   THX 4010
        DO 106 N=1,NMAX                                                 THX 4020
          IF(TMA .LT. T(I,N)) TMA = T(I,N)                              THX 4030
          IF(TMI .GT. T(I,N)) TMI = T(I,N)                              THX 4040
          WI(I,N) = T(I,N)                                              THX 4050
  106 CONTINUE                                                          THX 4060
      TAB = TMI - (TMA-TMI) / 3.                                        THX 4070
      IF(TAB .LT. 0.) TAB = 0.                                          THX 4080
C                                                                       THX 4090
      CALL PRFELD(WI,IMAX,NMAX,UET,1,PHI,RAD,1)                         THX 4100
C                                                                       THX 4110
  121 CONTINUE                                                          THX 4120
CFZJ042                                                       09.09.05  THX 4130
      IF(IPUN .EQ. 0) GOTO 212                                          THX 4140
C                                                                       THX 4150
CFZJ026                                                       16.03.04  THX 4160
CFZJ042                                                       09.09.05  THX 4170
      CALL SECURE(NHET,ZEITH,0,N200,B(KX(IPOV)),B(KX(IZF)),B(KX(INRY)), THX 4180
     1 B(KX(IQNW)),DELDZ,QTHX,WTGINT,RAD,PHI,T,WI,IFBER,IFBH,KKB,NHZON, THX 4190
     2 B(KX(IFFTH)),B(KX(ITCON)),B(KX(IVOLT)),B(KX(IPOWT)),B(KX(IDOST)),THX 4200
     3 B(KX(ITHEN)))                                                    THX 4210
C                                                                       THX 4220
  212 CONTINUE                                                          THX 4230
C                                                                       THX 4240
      CALL ABEND(0)                                                     THX 4250
C                                                                       THX 4260
      IF(IPRINT .GT. -3) CALL BILD(1,TEXT,CP0)                          THX 4270
C                                                                       THX 4280
   99 RETURN                                                            THX 4290
      END                                                               THX 4300
      SUBROUTINE STEUER(IFKON,ITLAM,TDIFF,NLOOP,IFRED,IFZW,ITM3,CP0,    STE   10
     1 IFTEST,NHET,ZEITH,IEXPR,N200,NDR,NXS,POV,ZF,NRY,QNW,DELDZ,QTHX,  STE   20
     2 WTGINT,HKUG,DI,NHZON,XFWQZ,IFHET,ZKUG,IFBH,WKAPH,THALT,WQN,WQK,  STE   30
     3 KKB,IFBER,AU,AR,BU,BR,ZEIV,TKV,NTVAR,RAD,PHI,KOM,KART,T,WI,WL,WT,STE   40
     4 TQ,TFLU,BU1,DU,TGRA,TFUL,STZUK,TFLVOR,IKO,CU,QSPEIZ,QKONVZ,QNUKLZSTE   50
     5 ,TKT,JUG,QQUEL1,BUINS,QSPEI1,QX,QKONS1,FAKZ1,FAKZ2,TKOMI,TKOMA,  STE   60
     6 VKOM,HEPS,NHMAT2,VOLK,VOLZ,RHO,C,IFWKT,VOLS,IFFUEL,WWK,NHMAT1,DOSSTE   70
     7 ,ZDOS,DPH,LAM,PHIP,E,IPAR,JPAR,NPAR,XKP,ROGP,KMAT,MIX,FFTHX,TCONDSTE   80
     8 ,POWPT,VOLPT,PBET,DOSPT,THETNEW,VRP,VREG)                        STE   90
C                                                                       STE  100
CFZJ042                                                       09.09.05  STE  110
C                                                                       STE  120
C     UEBERGEORDNETES STEUERPROGRAMM                                    STE  130
C                                                                       STE  140
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                STE  150
C                                                                       STE  160
      COMMON /BILANZ/ TGASM,IZAEL                                       STE  170
C                                                                       STE  180
      COMMON /RSTRT/ IFRSTA,ZRST,ILOGR                                  STE  190
C                                                                       STE  200
      COMMON /WKAPT/ IFWKPT                                             STE  210
C                                                                       STE  220
      COMMON /PRINT1/ TITLE(20),INDGEO                                  STE  230
C                                                                       STE  240
CFZJ004 enlarged dimensions common trans                      28.11.03  STE  250
      COMMON /TRANS/ IFINST,INTVAL,DZEIT(300),ZEI(300),NPRINT(300),     STE  260
     1 NKONV(300)                                                       STE  270
C                                                                       STE  280
      COMMON /LAMT/ IFLAMT                                              STE  290
C                                                                       STE  300
      COMMON /COUPL/ IPRINT                                             STE  310
C                                                                       STE  320
CFZJ042                                                       09.09.05  STE  330
      COMMON /KOPPLG/ DTVOR                                             STE  340
C                                                                       STE  350
      COMMON /ITER1/ TRMAX,MIT,TN,OVREL,ITMAX,T1,T2,TRELA,NTMAX,IFKOR,  STE  360
     1 ETHA,ORMIN,MITMAX,IKORM,IFREL                                    STE  370
C                                                                       STE  380
      COMMON /TVAR/ IFTKV                                               STE  390
C                                                                       STE  400
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    STE  410
C                                                                       STE  420
      COMMON /KOMP1/ KMAX                                               STE  430
C                                                                       STE  440
      COMMON /CALC/ IFWQ                                                STE  450
C                                                                       STE  460
      COMMON /SPECTI/ ITIK(10)                                          STE  470
C                                                                       STE  480
      COMMON /KONTHX/ FALAST                                            STE  490
C                                                                       STE  500
      COMMON /OPT/ KENN,IOPUT,NIFKO                                     STE  510
C                                                                       STE  520
CFZJ006 enlarged dimensions common QVAR                       28.11.03  STE  530
      COMMON /QVAR/ IVAR,ENDKEF,QVOLLL,QREDUZ,QREMAX,EPQ,EPC,DQDDC,DELC,STE  540
     1 DCN,SBU,TE(4,300),TA(300),N61,URZ,ZLEKA,ABXEN,TI(300)            STE  550
C                                                                       STE  560
CFZJ042                                                       09.09.05  STE  570
      COMMON /BLINDL/ TMITL,M24,NGEOM,CIZET0                            STE  580
C                                                                       STE  590
      COMMON /MPUNKT/ IMPU,KMPU,EMPU,TTTEIN,TTTAUS                      STE  600
C                                                                       STE  610
      COMMON /KOMVAK/ KOMVAR,KONVAR,XKSUMK,NQVAR,DUM(13),QWUNS(10)      STE  620
C                                                                       STE  630
      COMMON /MPUTA/ TEIMIN,TEIMAX,EMP0,TAU0,MPUTAU,QWU,DTAU,TEI0,NVR   STE  640
C                                                                       STE  650
      COMMON /KSUM/ D(3),VORZ,DA(5),ZEITMI                              STE  660
C                                                                       STE  670
CFZJ005 enlarged dimensions common SPEIKO                     28.11.03  STE  680
      COMMON /SPEIKO/ F(300,9),MX,MM,TMADBH(300),TMIDBH(300)            STE  690
C                                                                       STE  700
CFZJ048 enlarged dimension                                    11.04.07  STE  710
      COMMON /VARDIT/ B(5000000)                                        STE  720
C                                                                       STE  730
CFZJ042                                                       09.09.05  STE  740
      COMMON /ADDRT/ KX(240),KY(240),LZ(240),NENDPT                     STE  750
C                                                                       STE  760
CFZJ055                                                       25.09.07  STE  770
C                                                                       STE  780
      COMMON /STA/ IST,SB,TEX,JNS,RMI,RMA,TKI,TKA,TC                    STE  790
C                                                                       STE  800
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             STE  810
C                                                                       STE  820
      COMMON /PHI98/ PHIA,PHIE                                          STE  830
C                                                                       STE  840
CFZJ042                                                       09.09.05  STE  850
      COMMON /VRT/ MIXM                                                 STE  860
C                                                                       STE  870
      COMMON /KES/ KS                                                   STE  880
C                                                                       STE  890
      COMMON /IPRP/ IPR,MIXTH                                           STE  900
C                                                                       STE  910
CFZJ042                                                       09.09.05  STE  920
      DIMENSION VSCH(5),POV(N200),ZF(7,N200),NRY(N200),QNW(N200),       STE  930
     1 HKUG(KMAZ),DI(KMAZ,5),NHZON(KMAZ),XFWQZ(KMAZ,5),IFHET(KMAZ),     STE  940
     2 ZKUG(ICO,NCO),IFBH(ICO,NCO),WKAPH(ICO,NCO,5),THALT(ICO,NCO,5),   STE  950
     3 WQN(ICO,NCO),WQK(ICO,NCO),KKB(ICO,NCO),IFBER(IMAZ,NMAZ),         STE  960
     4 AU(IMAZ,NMAZ),AR(IMAZ,NMAZ),BU(IMAZ,NMAZ),BR(IMAZ,NMAZ),         STE  970
     5 ZEIV(10,KMAZ),TKV(10,KMAZ),NTVAR(KMAZ),RAD(IMAZ+1),PHI(NMAZ+1),  STE  980
     6 KOM(IMAZ,NMAZ),KART(KMAZ),T(IMAZ,NMAZ),WI(IMAZ,NMAZ),            STE  990
     7 WL(IMAZ,NMAZ),WT(IMAZ,NMAZ),TQ(IMAZ,NMAZ),TFLU(IMAZ,NMAZ),       STE 1000
     8 BU1(IMAZ,NMAZ),DU(IMAZ,NMAZ),TGRA(IMAZ,NMAZ),TFUL(IMAZ,NMAZ),    STE 1010
     9 STZUK(KOMAX),TFLVOR(KOMAX),IKO(KMAZ),CU(IMAZ,NMAZ),QSPEIZ(KMAZ,5)STE 1020
     X ,QKONVZ(KMAZ,5),QNUKLZ(KMAZ,5),TKT(KMAZ),JUG(KMAZ),QQUEL1(KMAZ), STE 1030
     Y BUINS(KMAZ),QSPEI1(KMAZ),QX(KMAZ),QKONS1(KMAZ),FAKZ1(NMAZ),      STE 1040
     Z THETNEW(ICO,NCO,5,15),VRP(NDR),VREG(NDR)                         STE 1050
CFZJ026                                                       16.03.04  STE 1060
      DIMENSION FAKZ2(NMAZ),TKOMI(KMAZ),TKOMA(KMAZ),VKOM(KMAZ),         STE 1070
     1 HEPS(KMAZ),NHMAT2(KMAZ,5),VOLK(KMAZ,5),VOLZ(KMAZ,5),RHO(KMAZ),   STE 1080
     2 C(KMAZ),IFWKT(KMAZ),VOLS(KMAZ,5),IFFUEL(KMAZ,5),WWK(KMAZ,5),     STE 1090
     3 NHMAT1(KMAZ,5),DOS(KMAZ),ZDOS(NMAZ),DPH(NMAZ),PHIP(NMAZ),        STE 1100
     4 E(IMAZ,NMAZ),IPAR(KONI*2,10),JPAR(KONI*2,10),NPAR(KONI*2,10),    STE 1110
     5 XKP(KONI*2),ROGP(KONI*2),MIX(NDR),FFTHX(IMAZ,NMAZ),              STE 1120
     6 TCOND(IMAZ,NMAZ),POWPT(IMAZ,NMAZ,15),VOLPT(IMAZ,NMAZ,15),        STE 1130
     7 PBET(N200),DOSPT(IMAZ,NMAZ,15)                                   STE 1140
C                                                                       STE 1150
      REAL LAM(KMAZ)                                                    STE 1160
C                                                                       STE 1170
CFZJ042                                                       09.09.05  STE 1180
      EQUIVALENCE(LZ(1),IVL),(LZ(7),IPOWL),(LZ(100),IDOSI),(LZ(74),IEPS)STE 1190
     1 ,(LZ(75),IIDIR),(LZ(76),IIFAN),(LZ(64),IDR),(LZ(68),IALP),       STE 1200
     2 (LZ(29),IRADP),(LZ(115),IISO),(LZ(116),IISU),(LZ(117),INSL),     STE 1210
     3 (LZ(118),INSR),(LZ(119),IKSTR),(LZ(60),IIFLT),(LZ(147),IROGG),   STE 1220
     4 (LZ(157),IEPSI),(LZ(159),IDHYD),(LZ(160),ISTZU),(LZ(161),ITFLV), STE 1230
     5 (LZ(148),ITKO),(LZ(173),ITFL),(LZ(152),IRHOK),(LZ(137),IIFB),    STE 1240
     6 (LZ(149),IFZQ),(LZ(84),IDZ),(LZ(198),IMZ),(LZ(139),IMR),         STE 1250
     7 (LZ(140),IP),(LZ(141),IKON),(LZ(155),IZPK),(LZ(156),IRP),        STE 1260
     8 (LZ(199),ISTRO),(LZ(162),IIFZS),(LZ(163),IIFZT),(LZ(200),IHG),   STE 1270
     9 (LZ(201),IHF),(LZ(70),IIFTV),(LZ(212),ITB),(LZ(213),IPGES),      STE 1280
     X (LZ(214),IZD),(LZ(71),IFELD)                                     STE 1290
C                                                                       STE 1300
    2 FORMAT (10E12.5)                                                  STE 1310
   35 FORMAT(/T32,59(1H_)/T31,1H|,59X,1H|/T31,1H|,5X,'TIME:',F11.3,' H  STE 1320
     1OR   ',1PE9.2,' S, DT=',0PF8.2,' S   |'/T31,1H|,59(1H_),1H|/130   STE 1330
     2 (1H=))                                                           STE 1340
   39 FORMAT (16X,I3,'.CYCLE, MAX. REL. CHANGE IN TEMPERATURE =',2PF7.2 STE 1350
     1 ,' PERCENT AT (I,N) = (',I3,',',I3,')')                          STE 1360
   52 FORMAT (/T45,'*** START OF STEADY STATE CALCULATION ***',/)       STE 1370
   80 FORMAT (/10X,4(1PE10.2,3X),5X,0PF7.2,' %')                        STE 1380
   81 FORMAT (/12X,'PRODUCTION  CONVECTION     STORED  COOLING CHANNELS STE 1390
     1  T-AVG.    T-MAX.'/12X,'(KW*SEC)                  (KW*SEC)       STE 1400
     2             (DEG)     (DEG)'/)                                   STE 1410
   82 FORMAT (3X,I2,5X,4(1PE10.2,3X),3X,0PF6.1,4X,0PF6.1)               STE 1420
   96 FORMAT (3X,I2,'/1',3X,4(1PE10.2,3X),3X,0PF6.1,4X,0PF6.1)          STE 1430
   97 FORMAT (3X,I2,'/',I1,3X,2(1PE10.2,16X))                           STE 1440
  144 FORMAT (/84X,'XKSUM(',I2,'):',E12.5,'    TKT(',I2,'):',E12.5)     STE 1450
  417 FORMAT (1H1,T45,'*** START OF TRANSIENT CALCULATION ***')         STE 1460
  789 FORMAT (//T40,'*** CALCULATION OF TEMPERATURES OF SOLID MATTER ***STE 1470
     1')                                                                STE 1480
  857 FORMAT (' **WARNING** TIME LIMIT OF 1.ST INTERVAL IS TOO SMALL -->STE 1490
     1 ',1PE10.3,' H ASSUMED')                                          STE 1500
  908 FORMAT (' **CONSIDERATION** MAGNIFICATION FACTOR OF TIME STEP WIDTSTE 1510
     1H (THERMIX) > 2.')                                                STE 1520
  910 FORMAT (' **WARNING** REDUCTION FACTOR OF TIME STEP WIDTH (THERMIXSTE 1530
     1) < 0.25')                                                        STE 1540
 2346 FORMAT (5X,'ACTUAL POWER OF DECAY HEAT AMOUNTS TO',1PE9.2,' KW')  STE 1550
 3002 FORMAT (200E12.5)                                                 STE 1560
 3003 FORMAT (12X,200E12.5)                                             STE 1570
C                                                                       STE 1580
C                                                                       STE 1590
CFZJ042                                                       09.09.05  STE 1600
      IF(MIXTH .GT. 0) MIXM = 1                                         STE 1610
      NENDK0 = NENDPT + 1                                               STE 1620
      NULEIN = -1                                                       STE 1630
      TKEL = 273.                                                       STE 1640
      N55 = 55                                                          STE 1650
      ITI55 = 0                                                         STE 1660
      IF(ITIK(7) .EQ. 1) ITI55 = 1                                      STE 1670
      ITIK(7) = 0                                                       STE 1680
      IRI = 0                                                           STE 1690
      IF(ITIK(1) .GT. 1 .AND. ITIK(10) .EQ. 1) IRI = 1                  STE 1700
      IF(ITIK(9) .GT. 0) GOTO 12                                        STE 1710
      IFWARN = 0                                                        STE 1720
      IFPPP = 0                                                         STE 1730
      IFKST = 0                                                         STE 1740
      IF(IFINST .EQ. 0 .AND. IFKON .NE. 0) GOTO 11                      STE 1750
      GOTO 13                                                           STE 1760
   11 IFKST = 1                                                         STE 1770
   13 CONTINUE                                                          STE 1780
CFZJ055                                                       25.09.07  STE 1790
      IFZENT = 1                                                        STE 1800
      IF(IMH .LT. 1 .OR. NMH .LT. 1) GOTO 703                           STE 1810
      DO 702 I=1,IMH                                                    STE 1820
        DO 702 N=1,NMH                                                  STE 1830
          IA = I + IDIFF                                                STE 1840
          NA = N + NDIFF                                                STE 1850
          K = KOM(IA,NA)                                                STE 1860
          IF3 = IFBH(I,N)                                               STE 1870
          WQK(I,N) = 0.                                                 STE 1880
          WQN(I,N) = 0.                                                 STE 1890
          TT = T(IA,NA)                                                 STE 1900
          IF(IF3 .NE. 0 .AND. IFRSTA .GE. 3) GOTO 702                   STE 1910
CFZJ042                                                       09.09.05  STE 1920
          DO 707 IS=2,5                                                 STE 1930
            DO 707 ID=1,MIXM                                            STE 1940
              THETNEW(I,N,IS,ID) = 0.                                   STE 1950
  707     CONTINUE                                                      STE 1960
          DO 704 ID=1,MIXM                                              STE 1970
            THETNEW(I,N,1,ID) = TT                                      STE 1980
  704     CONTINUE                                                      STE 1990
          IF(IF3 .EQ. 0) GOTO 702                                       STE 2000
          KK1 = KKB(I,N)                                                STE 2010
          NZZ = NHZON(KK1)                                              STE 2020
          DO 701 NZ=2,NZZ                                               STE 2030
            DO 701 ID=1,MIXM                                            STE 2040
              THETNEW(I,N,NZ,ID) = TT                                   STE 2050
  701     CONTINUE                                                      STE 2060
  702 CONTINUE                                                          STE 2070
  703 CONTINUE                                                          STE 2080
      NZP = 0                                                           STE 2090
      IFSELB = 0                                                        STE 2100
CFZJ042                                                       09.09.05  STE 2110
      ZEITH = 0.                                                        STE 2120
      IF(IFRSTA .EQ. 2 .OR. IFRSTA .GE. 4) ZEITH = ZRST                 STE 2130
      IZAEL = 0                                                         STE 2140
      OVRM = OVREL                                                      STE 2150
      Z1 = ZEI(1)                                                       STE 2160
      Z2 = ZEITH + DZEIT(1) / 3600.                                     STE 2170
      IF(IFINST .NE. 1 .OR. Z1 .GE. Z2) GOTO 856                        STE 2180
      ZEI(1) = ZEITH + 2. * DZEIT(1) / 3600.                            STE 2190
      WRITE (6,857) ZEI(1)                                              STE 2200
C                                                                       STE 2210
      CALL ABEND(2)                                                     STE 2220
C                                                                       STE 2230
  856 CONTINUE                                                          STE 2240
      DO 6 K=1,KMAX                                                     STE 2250
        JUG(K) = 0                                                      STE 2260
        IF(NTVAR(K) .GT. 0) JUG(K) = 2                                  STE 2270
    6 CONTINUE                                                          STE 2280
      IFNEU = 0                                                         STE 2290
      ZEITS = ZEITH * 3600.                                             STE 2300
      DELTA = .1                                                        STE 2310
      DO 5 K=1,KMAX                                                     STE 2320
        QQUEL1(K) = 0.                                                  STE 2330
        BUINS(K) = 0.                                                   STE 2340
        QSPEI1(K) = 0.                                                  STE 2350
        QKONS1(K) = 0.                                                  STE 2360
        QX(K) = 0.                                                      STE 2370
        DO 93 NZ=1,5                                                    STE 2380
          QSPEIZ(K,NZ) = 0.                                             STE 2390
          QKONVZ(K,NZ) = 0.                                             STE 2400
          QNUKLZ(K,NZ) = 0.                                             STE 2410
   93   CONTINUE                                                        STE 2420
    5 CONTINUE                                                          STE 2430
      GEOFAK = 1.                                                       STE 2440
      IF(INDGEO .EQ. 2) GEOFAK = 1000.                                  STE 2450
      IFKO1 = 1                                                         STE 2460
      IF(IFKON .EQ. -1) IFKO1 = -1                                      STE 2470
      FAKZ = 1.                                                         STE 2480
CFZJ042                                                       09.09.05  STE 2490
      INTV = 1                                                          STE 2500
      NLP = 0                                                           STE 2510
      IFSTOP = 0                                                        STE 2520
      IM = IMAX                                                         STE 2530
      NM = NMAX                                                         STE 2540
      IM1 = IM - 1                                                      STE 2550
      NM1 = NM - 1                                                      STE 2560
      IFLT1 = IFLAMT + 1                                                STE 2570
      IFDT2 = 0                                                         STE 2580
      NKO = 0                                                           STE 2590
      DTEM1 = DZEIT(INTV)                                               STE 2600
      DTEM4 = DTEM1                                                     STE 2610
      IFSFB = 0                                                         STE 2620
      IF(IMH .LT. 1 .OR. NMH .LT. 1) GOTO 1234                          STE 2630
      DO 123 II=1,IMH                                                   STE 2640
        DO 123 NN=1,NMH                                                 STE 2650
          IGG = II + IDIFF                                              STE 2660
          NGG = NN + NDIFF                                              STE 2670
          KK = KOM(IGG,NGG)                                             STE 2680
          IF1 = IFBH(II,NN)                                             STE 2690
          IF(IF1 .EQ. 0) GOTO 123                                       STE 2700
C                                                                       STE 2710
          ZKUG(II,NN) = ZKUGL(II,NN,KK,AU,IFBH,HEPS,HKUG,KKB)           STE 2720
C                                                                       STE 2730
  123 CONTINUE                                                          STE 2740
 1234 CONTINUE                                                          STE 2750
      IF(IFINST .EQ. 0) NZ = NHZON(KK1)                                 STE 2760
      NPR = 0                                                           STE 2770
      IF(IFINST .EQ. 0) GOTO 970                                        STE 2780
      DO 15 N=1,NMAX                                                    STE 2790
        FAKZ2(N) = 1.                                                   STE 2800
        DO 15 I=1,IMAX                                                  STE 2810
          BU(I,N) = T(I,N)                                              STE 2820
          TFLU(I,N) = 0.                                                STE 2830
   15 CONTINUE                                                          STE 2840
      IFPH = 0                                                          STE 2850
      IF(NHET .NE. 0 .AND. IFRSTA .GE. 3) IFPH = 1                      STE 2860
      WRITE (6,417)                                                     STE 2870
C                                                                       STE 2880
CFZJ042                                                       09.09.05  STE 2890
      CALL PRINTT(ZEITH,ZEITS,DTEM1,BU,IFPH,RAD,PHI,IFHET,KOM,THETNEW)  STE 2900
C                                                                       STE 2910
C      AUSGABE DER FESTSTOFFTEMPERATUREN AUF EINEN EXTERNEN SPEICHER    STE 2920
C      FUER EVTL. 3-D-DARSTELLUNGEN ODER ZUR EXCEL-ANWENDUNG            STE 2930
C                                                                       STE 2940
  970 CONTINUE                                                          STE 2950
CFZJ020                                                       28.01.04  STE 2960
      IF(IEXPR .EQ. 0 .OR. IFINST .EQ. 0) GOTO 75                       STE 2970
      WRITE (IEXPR,3002) ZEITS                                          STE 2980
      WRITE (IEXPR,3003) (RAD(I),I=1,IMAX)                              STE 2990
      DO 3000 N=1,NMAX                                                  STE 3000
        IF(PHI(N) .LT. PHIA .OR. PHI(N) .GT. PHIE) GOTO 3000            STE 3010
        WRITE (IEXPR,3002) PHI(N),(BU(I,N),I=1,IMAX)                    STE 3020
 3000 CONTINUE                                                          STE 3030
   75 CONTINUE                                                          STE 3040
      IF(IFINST .EQ. 0) GOTO 12                                         STE 3050
C                                                                       STE 3060
CFZJ042                                                       09.09.05  STE 3070
      CALL WKAP(DTEM1,NHMAT2,NHZON,VOLK,ZKUG,VOLZ,IFBH,WKAPH,KKB,KOM,AU,STE 3080
     1 BR,RHO,C,IFBER,T,WT,IFWKT,THETNEW)                               STE 3090
C                                                                       STE 3100
CFZJ042                                                       09.09.05  STE 3110
      CALL TPROZ(NHET,DTEM1,IFKO1,NHZON,XFWQZ,VOLS,IFFUEL,ZKUG,WWK,IFBH,STE 3120
     1 WKAPH,WQN,WQK,NHMAT1,DOS,ZDOS,KKB,DPH,PHI,KOM,LAM,AU,T,WI,WL,WT, STE 3130
     2 TFLU,B(KX(IDOSI)),VOLPT,POWPT,THETNEW,NPR)                       STE 3140
C                                                                       STE 3150
CFZJ042                                                       09.09.05  STE 3160
   12 CONTINUE                                                          STE 3170
      DO 16 I=1,IMAX                                                    STE 3180
        DO 16 N=1,NMAX                                                  STE 3190
          CU(I,N) = 0.                                                  STE 3200
          BU(I,N) = DU(I,N)                                             STE 3210
          DU(I,N) = 0.                                                  STE 3220
          IF(ITIK(9) .GT. 0) GOTO 16                                    STE 3230
          BU(I,N) = 0.                                                  STE 3240
   16 CONTINUE                                                          STE 3250
      I9 = ITIK(9)                                                      STE 3260
      IF(I9 .NE. 1 .AND. I9 .NE. -3) GOTO 18                            STE 3270
      DO 17 I=1,IMAX                                                    STE 3280
        DO 17 N=1,NMAX                                                  STE 3290
          CU(I,N) = TQ(I,N)                                             STE 3300
          IF(I9 .EQ. -3) CU(I,N) = CU(I,N) * FALAST                     STE 3310
          IF(I9 .EQ. -3) GOTO 17                                        STE 3320
          TQ(I,N) = BU1(I,N)                                            STE 3330
   17 CONTINUE                                                          STE 3340
   18 CONTINUE                                                          STE 3350
      IDRU = 0                                                          STE 3360
CFZJ042                                                       09.09.05  STE 3370
      IFU = KS                                                          STE 3380
      VMAT = 0.                                                         STE 3390
      VKUG = 0.                                                         STE 3400
      NSCH = NHZON(IFU)                                                 STE 3410
      RA = HKUG(IFU) / 2.                                               STE 3420
      DO 14 I=1,NSCH                                                    STE 3430
        RI = DI(IFU,I) / 2.                                             STE 3440
        VSCH(I) = RA**3.                                                STE 3450
        RI3 = RI**3.                                                    STE 3460
        IF(RI .GT. 0.) VSCH(I) = VSCH(I) - RI3                          STE 3470
        VKUG = VKUG + VSCH(I)                                           STE 3480
        IF(XFWQZ(IFU,I) .GT. 0.) VMAT = VMAT + VSCH(I)                  STE 3490
        RA = RI                                                         STE 3500
   14 CONTINUE                                                          STE 3510
      IF(ITIK(9) .GT. 0) GOTO 61                                        STE 3520
      IF(IFINST .EQ. 0.) WRITE (6,52)                                   STE 3530
      IDRU = 1                                                          STE 3540
      IF(IRI .EQ. 1) GOTO 920                                           STE 3550
      IIT = 0                                                           STE 3560
      NULEIN = NULEIN + 1                                               STE 3570
   20 CONTINUE                                                          STE 3580
      IIT = IIT + 1                                                     STE 3590
      IF(IDRU .EQ. 1) GOTO 8888                                         STE 3600
 8889 IDRU = 0                                                          STE 3610
      IFTP = 0                                                          STE 3620
      ZEITS = ZEITS + DTEM1                                             STE 3630
      ZEITH = ZEITS / 3600.                                             STE 3640
      IOPUT = 0                                                         STE 3650
      DELZIN = ABS(ZEITH-ZEI(INTV))                                     STE 3660
      IF(DELZIN .LE. 1.E-4) IOPUT = 1                                   STE 3670
      IF(IFKST .EQ. 1) GOTO 989                                         STE 3680
CFZJ042                                                       09.09.05  STE 3690
      IF(IFTKV .EQ. 0) GOTO 150                                         STE 3700
      TVMIN = 1.E10                                                     STE 3710
      DO 105 K=1,KMAX                                                   STE 3720
        IF(JUG(K) .GT. NTVAR(K)) JUG(K) = 0                             STE 3730
        JJ = JUG(K)                                                     STE 3740
        IF(JJ .EQ. 0) GOTO 105                                          STE 3750
        IF(ZEIV(JJ,K) .GT. TVMIN) GOTO 105                              STE 3760
        TVMIN = ZEIV(JJ,K)                                              STE 3770
  105 CONTINUE                                                          STE 3780
      IF(ZEITH .LE. TVMIN) GOTO 115                                     STE 3790
      IF(IFNEU .EQ. 1) GOTO 115                                         STE 3800
      IFNEU = 1                                                         STE 3810
      ZEITS = ZEITS - DTEM1                                             STE 3820
      DTEM2 = DTEM1 + (TVMIN-ZEITH) * 3599.                             STE 3830
      IFDT2 = 1                                                         STE 3840
      NPR = NPRINT(INTV)                                                STE 3850
      GOTO 70                                                           STE 3860
  115 IFNEU = 0                                                         STE 3870
      DO 125 K=1,KMAX                                                   STE 3880
        JJ = JUG(K)                                                     STE 3890
        IF(JJ .EQ. 0) GOTO 125                                          STE 3900
        DZ1 = ZEIV(JJ,K) - ZEIV(JJ-1,K)                                 STE 3910
        DZ0 = ZEIV(JJ,K) - ZEITH                                        STE 3920
        IF(DZ0 .GT. 0.) GOTO 135                                        STE 3930
        JUG(K) = JUG(K) + 1                                             STE 3940
  135   CONTINUE                                                        STE 3950
        TKT(K) = TKV(JJ,K) - (TKV(JJ,K)-TKV(JJ-1,K)) * DZ0 / DZ1        STE 3960
  125 CONTINUE                                                          STE 3970
  150 CONTINUE                                                          STE 3980
      FAK3 = 2.                                                         STE 3990
CFZJ042                                                       09.09.05  STE 4000
      IF(IFWQ .EQ. 0) GOTO 10                                           STE 4010
      IF(IFINST .EQ. 1) GOTO 155                                        STE 4020
      IF(FAKZ .EQ. 0.) GOTO 10                                          STE 4030
      FAK1 = 1.                                                         STE 4040
      FAK2 = 1.                                                         STE 4050
      FAK3 = 1.                                                         STE 4060
      GOTO 156                                                          STE 4070
  155 CONTINUE                                                          STE 4080
      REDEG = 0.                                                        STE 4090
      REDHAL = 0.                                                       STE 4100
      TEG = 0.                                                          STE 4110
      THAL = 0.                                                         STE 4120
      DO 21 N=1,NMAX                                                    STE 4130
        FAKZ1(N) = 1.                                                   STE 4140
CFZJ042                                                       09.09.05  STE 4150
C                                                                       STE 4160
C     FAKZ = 1: KONVEKTIONSRECHNUNG WURDE DURCHGEFUEHRT,BU = KONV.WQ.   STE 4170
C     FAK1 =    ANTEIL DER AUFZUADDIERENDEN ,VORGEG. WAERMEQUELLEN TQ   STE 4180
C     WT   =    AKTUELLE ,LOKALE, RELATIVE NACHWAERMELEISTUNG           STE 4190
C                                                                       STE 4200
        DO 21 I=1,IMAX                                                  STE 4210
          DU(I,N) = 0.                                                  STE 4220
          WT(I,N) = 0.                                                  STE 4230
   21 CONTINUE                                                          STE 4240
      SUMNW = 0.                                                        STE 4250
      ZEITM = ZEITS - DTEM1 / 2. + VORZ * 3600.                         STE 4260
C                                                                       STE 4270
CFZJ042                                                       09.09.05  STE 4280
      IF(IFRED .EQ. 3) CALL EXPLIZ(ZEITM,N200,B(KX(IVL)),B(KX(IPOWL)),  STE 4290
     1 QNW,DELDZ,QTHX,DTEM1,WTGINT,NENDK0,DU,PHIP,WT,TQ,RAD,PHI,NDR,    STE 4300
     2 B(KX(ITB)),B(KX(IPGES)),B(KX(IZD)),KMAT,MIX,PBET,POWPT,VOLPT,    STE 4310
     3 DOSPT)                                                           STE 4320
C                                                                       STE 4330
CFZJ042                                                       09.09.05  STE 4340
      IF(IFRED .GE. 1) CALL REDUZ(ZEITS,DTEM1,REDEG,REDHAL,TEG,THAL)    STE 4350
C                                                                       STE 4360
      DO 22 I=1,IM1                                                     STE 4370
        DO 22 N=2,NM1                                                   STE 4380
C                                                                       STE 4390
C     BERECHNUNG DER LOKALEN NWL(=WT), BERUECKS. DER SP-FREISETZG.      STE 4400
C                                                                       STE 4410
          SPAB = 1.                                                     STE 4420
          IF(IFRED .NE. 3) WT(I,N) = FAKZ1(N)                           STE 4430
          IF(T(I,N) .GT. TEG) SPAB = SPAB - REDEG / 100.                STE 4440
          IF(T(I,N) .GT. THAL) SPAB = SPAB - REDHAL / 100.              STE 4450
          WT(I,N) = WT(I,N) * SPAB                                      STE 4460
          K = KOM(I,N)                                                  STE 4470
          ADDD = TQ(I,N) * WT(I,N) / GEOFAK                             STE 4480
          II = I - IDIFF                                                STE 4490
          NN = N - NDIFF                                                STE 4500
          SUMNW = SUMNW + ADDD                                          STE 4510
          IF(II .LT. 1 .OR. NN .LT. 1 .OR. II .GT. IMH .OR. NN .GT. NMH)STE 4520
     1     GOTO 2345                                                    STE 4530
          IF(IFBH(II,NN) .NE. 0) GOTO 22                                STE 4540
 2345     QQUEL1(K) = QQUEL1(K) + ADDD * DTEM1                          STE 4550
   22 CONTINUE                                                          STE 4560
      SBU = SUMNW                                                       STE 4570
      IF(IST .EQ. 0) SB = SBU                                           STE 4580
      IST = 1                                                           STE 4590
      IF(IFRED .GT. 0) WRITE (6,2346) SUMNW                             STE 4600
  156 CONTINUE                                                          STE 4610
CFZJ042                                                       09.09.05  STE 4620
      DO 25 N=2,NM1                                                     STE 4630
        DO 25 I=1,IM1                                                   STE 4640
          BU1(I,N) = 0.                                                 STE 4650
          IF(IFINST .EQ. 0) GOTO 26                                     STE 4660
          FAK1 = WT(I,N)                                                STE 4670
          FAK2 = 2. * FAK1                                              STE 4680
          FAK1 = 2. * FAK1                                              STE 4690
   26     CONTINUE                                                      STE 4700
          CU0 = CU(I,N) * 2.                                            STE 4710
          IF(IFKO1 .NE. -1) GOTO 2514                                   STE 4720
          BU1(I,N) = TQ(I,N) * FAK2 + CU0                               STE 4730
          GOTO 2515                                                     STE 4740
 2514     CONTINUE                                                      STE 4750
          IF(FAKZ .EQ. 0.) BU(I,N) = 0.                                 STE 4760
          ADD = TQ(I,N) * FAK1 + CU0                                    STE 4770
          BU(I,N) = BU(I,N) + ADD                                       STE 4780
 2515     K = KOM(I,N)                                                  STE 4790
          II = I - IDIFF                                                STE 4800
          NN = N - NDIFF                                                STE 4810
          IF(II .LT. 1 .OR. NN .LT. 1 .OR. II .GT. IMH .OR. NN .GT .NMH)STE 4820
     1     GOTO 25                                                      STE 4830
          IF(IFBH(II,NN) .EQ. 0) GOTO 25                                STE 4840
          IF(IFKO1 .NE. -1) GOTO 2516                                   STE 4850
          WQN(II,NN) = BU1(I,N) / FAK3                                  STE 4860
          DU(I,N) = WQN(II,NN)                                          STE 4870
          GOTO 25                                                       STE 4880
 2516     WQN(II,NN) = BU(I,N) / FAK3 - WQK(II,NN)                      STE 4890
          DU(I,N) = WQN(II,NN)                                          STE 4900
C                                                                       STE 4910
C     UNTERSCHEIDUNG DER KONV. UND NUKL.WAERMEQUELLEN                   STE 4920
C                                                                       STE 4930
   25 CONTINUE                                                          STE 4940
C                                                                       STE 4950
      CALL WDUKON(IFRED,KOM,AU,DU,E)                                    STE 4960
C                                                                       STE 4970
      IF(IPRINT .NE. 3) GOTO 10                                         STE 4980
C                                                                       STE 4990
C     *** OUTPUT WQN + NW (NUKLEAR + NACHWAERME) ***                    STE 5000
C                                                                       STE 5010
      DO 3 N=2,NM1                                                      STE 5020
        WRITE (6,2) (DU(I,N),I=1,IM1)                                   STE 5030
    3 CONTINUE                                                          STE 5040
   10 CONTINUE                                                          STE 5050
C                                                                       STE 5060
CFZJ042                                                       09.09.05  STE 5070
      IF(IFWKPT .EQ. 1 .AND. IFINST .EQ. 1) CALL WKAP(DTEM1,NHMAT2,NHZONSTE 5080
     1 ,VOLK,ZKUG,VOLZ,IFBH,WKAPH,KKB,KOM,AU,BR,RHO,C,IFBER,T,WT,IFWKT, STE 5090
     2 THETNEW)                                                         STE 5100
C                                                                       STE 5110
      GOTO(27,28),IFLT1                                                 STE 5120
   27 CONTINUE                                                          STE 5130
      DO 210 I=2,IM1                                                    STE 5140
        DO 210 N=2,NM1                                                  STE 5150
          IF(IFBER(I,N) .LE. 0) GOTO 210                                STE 5160
          WT(I,N) = WI(I,N) + WL(I,N) + WI(I-1,N) + WL(I,N-1)           STE 5170
  210 CONTINUE                                                          STE 5180
      DO 211 N=2,NM1                                                    STE 5190
        WT(1,N) = WI(1,N) + WL(1,N) + WL(1,N-1)                         STE 5200
  211 CONTINUE                                                          STE 5210
      GOTO 29                                                           STE 5220
C                                                                       STE 5230
CFZJ026                                                       16.03.04  STE 5240
   28 CALL KONST1(0,IFBER,B(KX(IEPS)),B(KX(IIDIR)),B(KX(IIFAN)),HKUG,DI,STE 5250
     1 NHZON,IFHET,WWK,B(KX(IDR)),DPH,RAD,KOM,B(KX(IALP)),KART,WI,WL,WT,STE 5260
     2 AR,B(KX(IRADP)),B(KX(IISO)),B(KX(IISU)),B(KX(INSL)),B(KX(INSR)), STE 5270
     3 B(KX(IKSTR)),DOS,ZDOS,B(KX(IIFLT)),T,PHI,LAM,B(KX(IDOSI)),FFTHX, STE 5280
     4 TCOND)                                                           STE 5290
C                                                                       STE 5300
   29 CONTINUE                                                          STE 5310
      IF(IFINST .EQ. 0) GOTO 34                                         STE 5320
      IF(ABS(ZEITH-ZEI(INTV)) .LE. 1.E-4) ITIK(7) = 1                   STE 5330
      IF(INTV .LT. INTVAL) ITIK(7) = 0                                  STE 5340
      DO 30 I=1,IM                                                      STE 5350
        DO 30 N=1,NM                                                    STE 5360
          AR(I,N) = T(I,N)                                              STE 5370
   30 CONTINUE                                                          STE 5380
      IF(IFTKV .EQ. 0) GOTO 1455                                        STE 5390
      DO 145 I=1,IMAX                                                   STE 5400
        DO 145 N=1,NMAX                                                 STE 5410
          IF(IFBER(I,N) .NE. -1) GOTO 145                               STE 5420
          II = I                                                        STE 5430
          IF(II .EQ. IMAX) II = II - 1                                  STE 5440
          NN = N                                                        STE 5450
          IF(NN .EQ. NMAX) NN = NN - 1                                  STE 5460
          K = KOM(II,NN)                                                STE 5470
          IF(K .EQ. KOMVAR .AND. NQVAR .GT. 0) GOTO 143                 STE 5480
          IF(KART(K) .NE. 2 .OR. JUG(K) .EQ. 0) GOTO 145                STE 5490
  143     T(I,N) = TKT(K)                                               STE 5500
  145 CONTINUE                                                          STE 5510
 1455 CONTINUE                                                          STE 5520
      IF(IMH .LT. 1 .OR. NMH .LT. 1) GOTO 234                           STE 5530
      DO 700 II=1,IMH                                                   STE 5540
        DO 700 NN=1,NMH                                                 STE 5550
          IF3 = IFBH(II,NN)                                             STE 5560
          IF(IF3 .EQ. 0) GOTO 700                                       STE 5570
          KK1 = KKB(II,NN)                                              STE 5580
          NZ = NHZON(KK1)                                               STE 5590
CFZJ042                                                       09.09.05  STE 5600
          DO 7090 KK=1,NZ                                               STE 5610
            THALT(II,NN,KK) = THETNEW(II,NN,KK,1)                       STE 5620
C                                                                       STE 5630
C     ALTE TEMPERATUREN                                                 STE 5640
C                                                                       STE 5650
 7090     CONTINUE                                                      STE 5660
  700 CONTINUE                                                          STE 5670
  234 CONTINUE                                                          STE 5680
      DO 31 I=1,IM1                                                     STE 5690
        DO 31 N=2,NM1                                                   STE 5700
          K = KOM(I,N)                                                  STE 5710
          II = I - IDIFF                                                STE 5720
          NN = N - NDIFF                                                STE 5730
          IF(II .LT. 1 .OR. NN .LT. 1 .OR. II .GT. IMH .OR. NN .GT. NMH)STE 5740
     1     GOTO 2512                                                    STE 5750
          IFF = IFBH(II,NN)                                             STE 5760
          IF(IFF .EQ. 0) GOTO 2512                                      STE 5770
          GOTO 31                                                       STE 5780
 2512     WT(I,N) = WT(I,N) + BR(I,N)                                   STE 5790
   31 CONTINUE                                                          STE 5800
      GOTO 32                                                           STE 5810
   34 CONTINUE                                                          STE 5820
      DO 33 I=1,IM                                                      STE 5830
        DO 33 N=1,NM                                                    STE 5840
          BR(I,N) = T(I,N)                                              STE 5850
   33 CONTINUE                                                          STE 5860
   32 CONTINUE                                                          STE 5870
      IF(IFINST .EQ. 1) WRITE (6,35) ZEITH,ZEITS,DTEM1                  STE 5880
      IF(KOMVAR .GT. 0) WRITE (6,144) KONVAR,XKSUMK,KOMVAR,TKT(KOMVAR)  STE 5890
      IF(IPRINT. GE. 1) WRITE (6,789)                                   STE 5900
C                                                                       STE 5910
CFZJ026                                                       16.03.04  STE 5920
CFZJ042                                                       09.09.05  STE 5930
      CALL TFELD(ITLAM,OVRM,IFKO1,IFWARN,CP0,IFZENT,NHMAT1,IFBH,RAD,PHI,STE 5940
     1 KOM,T,AR,IFBER,B(KX(IDOSI)),DOS,B(KX(IEPS)),B(KX(IIDIR)),        STE 5950
     2 B(KX(IIFAN)),HKUG,DI,NHZON,IFHET,WWK,B(KX(IDR)),DPH,B(KX(IALP)), STE 5960
     3 KART,WI,WL,WT,B(KX(IRADP)),B(KX(IISO)),B(KX(IISU)),B(KX(INSL)),  STE 5970
     4 B(KX(INSR)),B(KX(IKSTR)),ZDOS,B(KX(IIFLT)),LAM,KKB,WQN,WQK,XFWQZ,STE 5980
     5 ZKUG,TFLU,BU1,BU,WKAPH,THALT,FFTHX,TCOND,POWPT,VOLPT,DOSPT,      STE 5990
     6 THETNEW)                                                         STE 6000
C                                                                       STE 6010
      DO 2509 I=1,IMAX                                                  STE 6020
        DO 2509 N=1,NMAX                                                STE 6030
          TGRA(I,N) = T(I,N)                                            STE 6040
          TFUL(I,N) = 0.0                                               STE 6050
 2509 CONTINUE                                                          STE 6060
CFZJ042                                                       09.09.05  STE 6070
      DO 2513 II=1,IMH                                                  STE 6080
        DO 2513 NN=1,NMH                                                STE 6090
          IA = II + IDIFF                                               STE 6100
          NA = NN + NDIFF                                               STE 6110
          NT = 1                                                        STE 6120
          TMITL = 0.                                                    STE 6130
          DO 300 ID=1,MIXM                                              STE 6140
            TMITL = TMITL + THETNEW(II,NN,NT,ID) * VOLPT(IA,NA,ID)      STE 6150
  300     CONTINUE                                                      STE 6160
          DELT = T(IA,NA) - TMITL                                       STE 6170
          GRA = 0.                                                      STE 6180
          FUL = 0.                                                      STE 6190
          VV = 0.                                                       STE 6200
          VOLF = 0.                                                     STE 6210
          DO 301 ID=1,MIXM                                              STE 6220
            THETNEW(II,NN,NT,ID) = THETNEW(II,NN,NT,ID) + DELT          STE 6230
            IF(TMITL .LE. 0.) THETNEW(II,NN,NT,ID) = T(IA,NA)           STE 6240
            DO 2511 M=1,NZ                                              STE 6250
              THT = THETNEW(II,NN,M,ID)                                 STE 6260
              IF(M .NE. NZ) THT = (THT+THETNEW(II,NN,M+1,ID)) * 0.5     STE 6270
              THT = THT * VSCH(M)                                       STE 6280
              GRA = GRA + VOLPT(IA,NA,ID) * THT / VKUG                  STE 6290
              IF(POWPT(IA,NA,ID) .LE. 0.) GOTO 2511                     STE 6300
              IF(XFWQZ(IFU,M) .LE. 0.) GOTO 2511                        STE 6310
              FUL = FUL + VOLPT(IA,NA,ID) * THT / VMAT                  STE 6320
 2511       CONTINUE                                                    STE 6330
            IF(POWPT(IA,NA,ID) .GT. 0.) VOLF = VOLF + VOLPT(IA,NA,ID)   STE 6340
  301     CONTINUE                                                      STE 6350
          TGRA(IA,NA) = GRA                                             STE 6360
          TFUL(IA,NA) = FUL                                             STE 6370
          IF(VOLF .GT. 0.) TFUL(IA,NA) = TFUL(IA,NA) / VOLF             STE 6380
 2513 CONTINUE                                                          STE 6390
C                                                                       STE 6400
      IF(IFKO1 .EQ. -1) CALL BUBIL(GEOFAK,BUINS,T,TFLU,AR,BU,KOM)       STE 6410
C                                                                       STE 6420
      DO 95 K=1,KMAX                                                    STE 6430
        QKONS1(K) = QKONS1(K) + BUINS(K) * DTEM1                        STE 6440
   95 CONTINUE                                                          STE 6450
CFZJ042                                                       09.09.05  STE 6460
      NLP = NLP + 1                                                     STE 6470
      NKO = NKO + 1                                                     STE 6480
      NPR = NPR + 1                                                     STE 6490
      IF(IFINST .EQ. 1) GOTO 40                                         STE 6500
      IF(IFKON .EQ. 0) GOTO 55                                          STE 6510
      TDM = 0.                                                          STE 6520
      DO 38 I=1,IM1                                                     STE 6530
        DO 38 N=2,NM1                                                   STE 6540
          IF(T(I,N) .EQ. 0.) GOTO 38                                    STE 6550
          DT = ABS((BR(I,N)-T(I,N))/T(I,N))                             STE 6560
          IF(DT .LT. TDM) GOTO 38                                       STE 6570
          TDM = DT                                                      STE 6580
          IIMA = I                                                      STE 6590
          NNMA = N                                                      STE 6600
   38 CONTINUE                                                          STE 6610
      IF(TDM .LT. TDIFF .OR. NLP .GE. NLOOP) IFSTOP = 1                 STE 6620
      IF(ITIK(1) .EQ. 1 .OR. IPRINT .GE. -2) WRITE (6,39) NLP,TDM,IIMA, STE 6630
     1 NNMA                                                             STE 6640
      NPR = 0                                                           STE 6650
      NKO = NKONV(INTV)                                                 STE 6660
   40 CONTINUE                                                          STE 6670
      IFPPP = 0                                                         STE 6680
      IF(IFSTOP .EQ. 1) NPR = NPRINT(INTV)                              STE 6690
      IF(IFSTOP .EQ. 1) ITIK(7) = 1                                     STE 6700
      IF(NPR .LT. NPRINT(INTV)) GOTO 42                                 STE 6710
 8888 CONTINUE                                                          STE 6720
      DO 43 I=1,IMAX                                                    STE 6730
        DO 43 N=1,NMAX                                                  STE 6740
          WT(I,N) = T(I,N)                                              STE 6750
   43 CONTINUE                                                          STE 6760
      IF(IDRU .EQ. 0) IFPPP = 1                                         STE 6770
      IFPH = 1                                                          STE 6780
      IF(ITIK(9) .GT. 0) IFPH = 0                                       STE 6790
C                                                                       STE 6800
CFZJ042                                                       09.09.05  STE 6810
      CALL PRINTT(ZEITH,ZEITS,DTEM1,WT,IFPH,RAD,PHI,IFHET,KOM,THETNEW)  STE 6820
C                                                                       STE 6830
      IF(IDRU .EQ. 1) GOTO 8889                                         STE 6840
C                                                                       STE 6850
C     AUSGABE DER FESTSTOFFTEMPERATUREN AUF EINEN EXTERNEN SPEICHER     STE 6860
C                                                                       STE 6870
      IF(IEXPR .EQ. 0) GOTO 3005                                        STE 6880
CFZJ020                                                       28.01.04  STE 6890
      IF(ITIK(7) .EQ. 0) GOTO 3005                                      STE 6900
      IF(IFINST .NE. 0) GOTO 3006                                       STE 6910
      PHIA = PHI(1)                                                     STE 6920
      PHIE = PHI(NMAX)                                                  STE 6930
      REWIND IEXPR                                                      STE 6940
 3006 CONTINUE                                                          STE 6950
      IF(IFINST .NE. 0) WRITE (IEXPR,3002) ZEITS                        STE 6960
      WRITE (IEXPR,3003) (RAD(I),I=1,IMAX)                              STE 6970
      DO 3004 N=1,NMAX                                                  STE 6980
        IF(PHI(N) .LT. PHIA .OR. PHI(N) .GT. PHIE) GOTO 3004            STE 6990
        WRITE (IEXPR,3002) PHI(N),(WT(I,N),I=1,IMAX)                    STE 7000
 3004 CONTINUE                                                          STE 7010
 3005 CONTINUE                                                          STE 7020
   42 CONTINUE                                                          STE 7030
      FAKZ = 0.                                                         STE 7040
      NIFKO = 0                                                         STE 7050
      IF(IFKON .EQ. 0 .OR. NKO .LT. NKONV(INTV)) NIFKO = 1              STE 7060
  989 CONTINUE                                                          STE 7070
C                                                                       STE 7080
CFZJ042                                                       09.09.05  STE 7090
      CALL KONVEK(IM1,NM1,RAD,PHI,T,BU,WT,TFLU,BU1,IFKON,IFKO1,IFZW,ITM3STE 7100
     1 ,ZEITH,ZEITS,GEOFAK,IFPPP,IFSTOP,IFINST,N200,NDR,NXS,NENDK0,     STE 7110
     2 B(KX(IROGG)),B(KX(IEPSI)),B(KX(IDHYD)),B(KX(ISTZU)),B(KX(ITFLV)),STE 7120
     3 B(KX(ITKO)),B(KX(ITFL)),B(KX(IRHOK)),B(KX(IIFB)),B(KX(IFZQ)),    STE 7130
     4 B(KX(IDZ)),B(KX(IMZ)),B(KX(IMR)),B(KX(IP)),B(KX(IKON)),          STE 7140
     5 B(KX(IZPK)),B(KX(IRP)),B(KX(ISTRO)),B(KX(IIFZS)),B(KX(IIFZT)),   STE 7150
     6 TGRA,TFUL,B(KX(IHG)),B(KX(IHF)),IPAR,JPAR,NPAR,XKP,ROGP,MIX,     STE 7160
     7 B(KX(IFELD)),VRP,VREG)                                           STE 7170
C                                                                       STE 7180
CFZJ042                                                       09.09.05  STE 7190
      IF(NIFKO .EQ. 1) GOTO 45                                          STE 7200
      NKO = 0                                                           STE 7210
      IFKST = 0                                                         STE 7220
C                                                                       STE 7230
      CALL WPKON(IFKO1,AU,BU,T,WT,TFLU,BU1,KOM,B(KX(IIFTV)))            STE 7240
C                                                                       STE 7250
      IF(IMH .LT. 1 .OR. NMH .LT. 1) GOTO 237                           STE 7260
      DO 23 I=1,IMH                                                     STE 7270
        DO 23 N=1,NMH                                                   STE 7280
          II = I + IDIFF                                                STE 7290
          NN = N + NDIFF                                                STE 7300
          IF1 = IFBH(I,N)                                               STE 7310
          IF(IF1 .EQ. 0) GOTO 23                                        STE 7320
          WQK(I,N) = BU(II,NN)                                          STE 7330
   23 CONTINUE                                                          STE 7340
  237 CONTINUE                                                          STE 7350
      IF(IFINST .NE. 1) GOTO 51                                         STE 7360
      DO 46 K=1,KMAX                                                    STE 7370
        BUINS(K) = 0.                                                   STE 7380
   46 CONTINUE                                                          STE 7390
      DO 50 I=1,IM1                                                     STE 7400
        DO 50 N=2,NM1                                                   STE 7410
          IF(IFKO1 .EQ. -1) GOTO 500                                    STE 7420
          K = KOM(I,N)                                                  STE 7430
          BUINS(K) = BUINS(K) + BU(I,N) / GEOFAK                        STE 7440
  500     BU(I,N) = BU(I,N) * 2.                                        STE 7450
   50 CONTINUE                                                          STE 7460
      IF(IPRINT .NE. 3) GOTO 4                                          STE 7470
C                                                                       STE 7480
C     *** OUTPUT WQK (KONVEKTIV) ***                                    STE 7490
C                                                                       STE 7500
      DO 1 N=2,NM1                                                      STE 7510
        WRITE (6,2) (BU(I,N),I=1,IM1)                                   STE 7520
    1 CONTINUE                                                          STE 7530
    4 CONTINUE                                                          STE 7540
   51 FAKZ = 1.                                                         STE 7550
   45 CONTINUE                                                          STE 7560
      IF(NPR .GE. NPRINT(INTV)) NPR = 0                                 STE 7570
      IF(IFSTOP .EQ. 1) GOTO 56                                         STE 7580
      GOTO 57                                                           STE 7590
C                                                                       STE 7600
CFZJ042                                                       09.09.05  STE 7610
   56 CALL TPROZ(NHET,DTEM1,IFKO1,NHZON,XFWQZ,VOLS,IFFUEL,ZKUG,WWK,IFBH,STE 7620
     1 WKAPH,WQN,WQK,NHMAT1,DOS,ZDOS,KKB,DPH,PHI,KOM,LAM,AU,T,WI,WL,WT, STE 7630
     2 TFLU,B(KX(IDOSI)),VOLPT,POWPT,THETNEW,NPR)                       STE 7640
C                                                                       STE 7650
      GOTO 55                                                           STE 7660
   57 CONTINUE                                                          STE 7670
      IF(IFINST .EQ. 0) GOTO 20                                         STE 7680
C                                                                       STE 7690
C     BEI INSTATIONAERER RECHNUNG                                       STE 7700
C                                                                       STE 7710
      DO 59 I=1,KMAX                                                    STE 7720
        TKOMI(I) = 0.                                                   STE 7730
        TKOMA(I) = 0.                                                   STE 7740
        VKOM(I) = 0.                                                    STE 7750
   59 CONTINUE                                                          STE 7760
      DMAX = 0.                                                         STE 7770
      DO 99 I=1,IM1                                                     STE 7780
        DO 99 N=2,NM1                                                   STE 7790
          TM = T(I,N)                                                   STE 7800
          IF1 = 0                                                       STE 7810
          II = I - IDIFF                                                STE 7820
          NN = N - NDIFF                                                STE 7830
          IF(II .LT. 1 .OR. NN .LT. 1 .OR. II .GT. IMH .OR. NN .GT. NMH)STE 7840
     1     GOTO 92                                                      STE 7850
          IF1 = IFBH(II,NN)                                             STE 7860
          IF(IF1 .EQ. 0) GOTO 92                                        STE 7870
          K1 = KKB(II,NN)                                               STE 7880
          NZZ = NHZON(K1)                                               STE 7890
CFZJ042                                                       09.09.05  STE 7900
          DO 94 NZ=1,NZZ                                                STE 7910
            DTZ = (THETNEW(II,NN,NZ,1)-THALT(II,NN,NZ)) *               STE 7920
     1       WKAPH(II,NN,NZ)                                            STE 7930
            QSPEIZ(K1,NZ) = QSPEIZ(K1,NZ) + DTZ * DTEM1 * ZKUG(II,NN) / STE 7940
     1       GEOFAK                                                     STE 7950
            XFF = XFWQZ(K1,NZ)                                          STE 7960
            QNUKLZ(K1,NZ) = QNUKLZ(K1,NZ) + WQN(II,NN) * XFF * DTEM1 /  STE 7970
     1       GEOFAK                                                     STE 7980
   94     CONTINUE                                                      STE 7990
   92     CONTINUE                                                      STE 8000
          K = KOM(I,N)                                                  STE 8010
          TKOMI(K) = TKOMI(K) + TM * AU(I,N)                            STE 8020
          VKOM(K) = VKOM(K) + AU(I,N)                                   STE 8030
          TKOMA(K) = AMAX1(TKOMA(K),TM)                                 STE 8040
          DTT = T(I,N) - AR(I,N)                                        STE 8050
          IF(AR(I,N) .EQ. 0.) GOTO 90                                   STE 8060
          DTR = ABS(DTT/AR(I,N))                                        STE 8070
          IF(DTR .GT. DMAX) DMAX = DTR                                  STE 8080
          IF(IF1 .NE. 0) GOTO 99                                        STE 8090
C                                                                       STE 8100
C     BR(I,N) = WAERMEKAPAZITAET  (SR WKAP)                             STE 8110
C                                                                       STE 8120
   90     QSPEI1(K) = QSPEI1(K) + BR(I,N) * DTT / GEOFAK / 2. * DTEM1   STE 8130
   99 CONTINUE                                                          STE 8140
      DO 100 K=1,KMAX                                                   STE 8150
        IF(VKOM(K) .EQ. 0.) GOTO 100                                    STE 8160
        TKOMI(K) = TKOMI(K) / VKOM(K)                                   STE 8170
  100 CONTINUE                                                          STE 8180
      IT = ITIK(1) + 1                                                  STE 8190
      TMADBH(IT) = TKOMA(MM)                                            STE 8200
      TMIDBH(IT) = TKOMI(MM)                                            STE 8210
C                                                                       STE 8220
      CALL KUEHLK(NPR,QX,IFINST,DTEM1,B(KX(IDR)),DPH,RAD,AR,KOM,KART,T, STE 8230
     1 WI,WL,WT,IFBER)                                                  STE 8240
C                                                                       STE 8250
      IF(NPR .NE. 0) GOTO 63                                            STE 8260
      WRITE (6,81)                                                      STE 8270
      QKONS = 0.                                                        STE 8280
      QQUELL = 0.                                                       STE 8290
      QSPEI = 0.                                                        STE 8300
      QPS = 0.                                                          STE 8310
      QPOS = 0.                                                         STE 8320
      QNEG = 0.                                                         STE 8330
      DO 98 KA=5,9                                                      STE 8340
        F(IT,KA) = 0.                                                   STE 8350
   98 CONTINUE                                                          STE 8360
      DO 91 K=1,KMAX                                                    STE 8370
        IK = IKO(K)                                                     STE 8380
        IF(IFHET(K) .NE. 2) GOTO 83                                     STE 8390
        WRITE (6,96) K,QNUKLZ(K,1),QKONS1(K),QSPEIZ(K,1),QX(K),         STE 8400
     1   TKOMI(K),TKOMA(K)                                              STE 8410
        NZZ = NHZON(K)                                                  STE 8420
        QQUELL = QQUELL + QNUKLZ(K,1)                                   STE 8430
        QSPEI = QSPEI + QSPEIZ(K,1)                                     STE 8440
        DO 195 NZ=2,NZZ                                                 STE 8450
          WRITE (6,97) K,NZ,QNUKLZ(K,NZ),QSPEIZ(K,NZ)                   STE 8460
          QQUELL = QQUELL + QNUKLZ(K,NZ)                                STE 8470
          QSPEI = QSPEI + QSPEIZ(K,NZ)                                  STE 8480
  195   CONTINUE                                                        STE 8490
        F(IT,4) = QSPEI                                                 STE 8500
        DO 196 NZ=1,NZZ                                                 STE 8510
          IF(QSPEIZ(K,NZ) .GT. 0.) GOTO 197                             STE 8520
          QNEG = QNEG - QSPEIZ(K,NZ)                                    STE 8530
          GOTO 196                                                      STE 8540
  197     QPOS = QPOS + QSPEIZ(K,NZ)                                    STE 8550
  196   CONTINUE                                                        STE 8560
        QKONS = QKONS + QKONS1(K)                                       STE 8570
        GOTO 85                                                         STE 8580
   83   WRITE (6,82) K,QQUEL1(K),QKONS1(K),QSPEI1(K),QX(K),TKOMI(K),    STE 8590
     1   TKOMA(K)                                                       STE 8600
        QQUELL = QQUELL + QQUEL1(K)                                     STE 8610
        QKONS = QKONS + QKONS1(K)                                       STE 8620
        QSPEI = QSPEI + QSPEI1(K)                                       STE 8630
        IF(IK .NE. 0) F(IT,IK) = F(IT,IK) + QSPEI1(K)                   STE 8640
        IF(QSPEI1(K) .GT. 0.) GOTO 84                                   STE 8650
        QNEG = QNEG - QSPEI1(K)                                         STE 8660
        GOTO 85                                                         STE 8670
   84   QPOS = QPOS + QSPEI1(K)                                         STE 8680
   85   CONTINUE                                                        STE 8690
        QPS = QPS + QX(K)                                               STE 8700
   91 CONTINUE                                                          STE 8710
      IF(QNEG .GT. QPOS) QPOS = QNEG                                    STE 8720
      IF(QPOS .EQ. 0.) QPOS = 1.E-10                                    STE 8730
      FEHL = ABS((QQUELL+QKONS-QSPEI+QPS)/QPOS ) * 100.                 STE 8740
      WRITE (6,80)QQUELL,QKONS,QSPEI,QPS,FEHL                           STE 8750
      F(IT,3) = QSPEI                                                   STE 8760
      F(IT,2) = QQUELL                                                  STE 8770
   63 CONTINUE                                                          STE 8780
C                                                                       STE 8790
CFZJ042                                                       09.09.05  STE 8800
      CALL TPROZ(NHET,DTEM1,IFKO1,NHZON,XFWQZ,VOLS,IFFUEL,ZKUG,WWK,IFBH,STE 8810
     1 WKAPH,WQN,WQK,NHMAT1,DOS,ZDOS,KKB,DPH,PHI,KOM,LAM,AU,T,WI,WL,WT, STE 8820
     2 TFLU,B(KX(IDOSI)),VOLPT,POWPT,THETNEW,NPR)                       STE 8830
C                                                                       STE 8840
C     ZEITSCHRITTSTEUERUNG                                              STE 8850
C                                                                       STE 8860
      DTM1 = DTEM1                                                      STE 8870
      IF(IFSFB .EQ. 0) DTEM4 = DTEM1                                    STE 8880
      IF(IFSFB .EQ. 1) DTM1 = DTEM4                                     STE 8890
      Z1 = ZEITH                                                        STE 8900
      Z2 = ZEI(INTV)                                                    STE 8910
      Z3 = Z1 + DTM1 / 3600.                                            STE 8920
      DTEM2 = DTM1                                                      STE 8930
      IF(IFSELB .EQ. 0) GOTO 62                                         STE 8940
      IFDT2 = 0                                                         STE 8950
      DZEITN = DTVOR * DTEM1 / DMAX                                     STE 8960
      DZEITN = AMAX1(DZEITN,ZEITMI)                                     STE 8970
      ADEL = ABS(DZEITN-DTM1) / DTM1                                    STE 8980
      IF(ADEL .LT. DELTA) GOTO 62                                       STE 8990
      ADEL = DZEITN / DTM1                                              STE 9000
      IF(ADEL .LE. 2.0) GOTO 907                                        STE 9010
      DZEITN = DTM1 * 2.0                                               STE 9020
      GOTO 909                                                          STE 9030
  907 IF(ADEL .GE. 0.25) GOTO 909                                       STE 9040
      DZEITN = DTM1 * 0.25                                              STE 9050
      WRITE (6,910)                                                     STE 9060
C                                                                       STE 9070
      CALL ABEND(2)                                                     STE 9080
C                                                                       STE 9090
  909 DTEM2 = DZEITN                                                    STE 9100
      IF(DTEM3 .NE. 0. .AND. DTEM2 .GT. DTEM3) DTEM2 = DTEM3            STE 9110
      DTEM4 = DTEM2                                                     STE 9120
      Z3 = Z1 + DTEM2 / 3600.                                           STE 9130
      IF(Z3 .LT. Z2) GOTO 65                                            STE 9140
   62 CONTINUE                                                          STE 9150
      IF(IFDT2 .NE. 0) GOTO 64                                          STE 9160
      IF(Z3 .LT. Z2) GOTO 65                                            STE 9170
      Z4 = ABS((Z1-Z2)/Z1)                                              STE 9180
      IF(Z4 .LT. 1.E-5) GOTO 61                                         STE 9190
      DTEM2 = (Z2-Z1) * 3600.                                           STE 9200
      IF(INTV .GE. INTVAL) NPR = NPRINT(INTV)                           STE 9210
      GOTO 65                                                           STE 9220
   61 CONTINUE                                                          STE 9230
C                                                                       STE 9240
C     GANZ SCHLUSS                                                      STE 9250
C                                                                       STE 9260
      IF(INTV .GE. INTVAL) GOTO 55                                      STE 9270
C                                                                       STE 9280
C     INS NAECHSTE GROSSE INTERVALL                                     STE 9290
C                                                                       STE 9300
      INTV = INTV + 1                                                   STE 9310
      NPR = NPRINT(INTV)                                                STE 9320
      NKO = NKONV(INTV)                                                 STE 9330
   64 DTEM2 = DZEIT(INTV)                                               STE 9340
      DTEM3 = ABS(DTEM2)                                                STE 9350
      DTEM4 = DTEM3                                                     STE 9360
      IF(DTEM2 .EQ. 0.) DTEM4 = DTM1                                    STE 9370
      IFDT2 = 0                                                         STE 9380
      IFSELB = 0                                                        STE 9390
      IF(DTEM2 .GT. 0.) GOTO 65                                         STE 9400
      IFSELB = 1                                                        STE 9410
      DTEM2 = ABS(.25*DTEM2)                                            STE 9420
      IF(DTEM2 .GT. DTM1) DTEM2 = DTM1                                  STE 9430
      IF(DTEM2 .EQ. 0.) DTEM2 = DTM1                                    STE 9440
   65 CONTINUE                                                          STE 9450
      DTEM2 = ABS(DTEM2)                                                STE 9460
CFZJ042                                                       09.09.05  STE 9470
   70 CONTINUE                                                          STE 9480
      DO 60 I=1,IM1                                                     STE 9490
        DO 60 N=2,NM1                                                   STE 9500
          K = KOM(I,N)                                                  STE 9510
          II = I - IDIFF                                                STE 9520
          NN = N - NDIFF                                                STE 9530
          IF(II .LT. 1 .OR. NN .LT. 1 .OR. II .GT. IMH .OR. NN .GT. NMH)STE 9540
     1     GOTO 600                                                     STE 9550
          IF3 = IFBH(II,NN)                                             STE 9560
          IF(IF3 .EQ. 0) GOTO 600                                       STE 9570
          KK1 = KKB(II,NN)                                              STE 9580
          NZ = NHZON(KK1)                                               STE 9590
CFZJ042                                                       09.09.05  STE 9600
          DO 6666 NZZ=1,NZ                                              STE 9610
              WKAPH(II,NN,NZZ) = WKAPH(II,NN,NZZ) * DTEM1 / DTEM2       STE 9620
 6666     CONTINUE                                                      STE 9630
  600     BR(I,N) = BR(I,N) * DTEM1 / DTEM2                             STE 9640
   60 CONTINUE                                                          STE 9650
      DTVERG = DTEM2 / DTEM1                                            STE 9660
      IF(DTVERG .LT. 2.) GOTO 919                                       STE 9670
      WRITE (6,908)                                                     STE 9680
C                                                                       STE 9690
      CALL ABEND(1)                                                     STE 9700
C                                                                       STE 9710
  919 DTEM1 = DTEM2                                                     STE 9720
      IF(ITIK(9) .LE. 0) GOTO 19                                        STE 9730
  920 CONTINUE                                                          STE 9740
      IF(ITI55 .EQ. 0) GOTO 19                                          STE 9750
CFZJ042                                                       09.09.05  STE 9760
      READ (N55) T,WI,WL,THETNEW                                        STE 9770
      REWIND N55                                                        STE 9780
      ITI55 = 0                                                         STE 9790
   19 CONTINUE                                                          STE 9800
      NULEIN = NULEIN + 1                                               STE 9810
      GOTO 20                                                           STE 9820
   55 CONTINUE                                                          STE 9830
      IF(ITIK(7) .EQ. 0) GOTO 58                                        STE 9840
CFZJ042                                                       09.09.05  STE 9850
      WRITE (N55) T,WI,WL,THETNEW                                       STE 9860
      REWIND N55                                                        STE 9870
   58 CONTINUE                                                          STE 9880
      DO 54 I=1,IMAX                                                    STE 9890
        DO 54 N=1,NMAX                                                  STE 9900
          DU(I,N) = BU(I,N)                                             STE 9910
   54 CONTINUE                                                          STE 9920
      RETURN                                                            STE 9930
      END                                                               STE 9940
      SUBROUTINE EXPLIZ(ZEITM,N200,VL,POWL,QNW,DELDZ,QTHX,DT,WTGINT,    EXP   10
     1 NENDK0,DU,PHIP,WT,TQ,RAD,PHI,NDR,TB,PGES,ZD,KMAT,MIX,PBET,POWPT, EXP   20
     2 VOLPT,DOSPT)                                                     EXP   30
C                                                                       EXP   40
CFZJ042                                                       09.09.05  EXP   50
C                                                                       EXP   60
C     BERECHNUNG DER EXPLIZITEN NACHWAERMEFUNKTION                      EXP   70
C                                                                       EXP   80
      COMMON /BLOTIK/ N197,HCORE,NRAD,POWER,IX,JZ,ISP,NLIB,LAYER,DELZ,  EXP   90
     1 TIN,TOUT,LAY(20),RAT(20),JR(20),RZ(2,50),IZ(2,50),Q(50,50,2),    EXP  100
     2 A(3,25,25),ILA                                                   EXP  110
C                                                                       EXP  120
CFZJ048 enlarged dimension                                    11.04.07  EXP  130
      COMMON /KONTHX/ F,NEU,JT0,JT1,JT2,NT0,NT1,NT2,K0,IADVT(4000,2),   EXP  140
     1 VOLVT(4000),PI                                                   EXP  150
C                                                                       EXP  160
      COMMON /PRINT1/ TITLE(20),INDGEO                                  EXP  170
C                                                                       EXP  180
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    EXP  190
C                                                                       EXP  200
      COMMON /REDUZR/ VFLIES                                            EXP  210
C                                                                       EXP  220
      COMMON /OPT/ KENN,IOPUT                                           EXP  230
C                                                                       EXP  240
CFZJ006 enlarged dimensions common QVAR                       28.11.03  EXP  250
      COMMON /QVAR/ DUM(1511),N61,DUMM(315),RMAX,RMIN                   EXP  260
C                                                                       EXP  270
      COMMON /SPECTI/ ITIK(10)                                          EXP  280
C                                                                       EXP  290
CFZJ005 enlarged dimensions common SPEIKO                     28.11.03  EXP  300
      COMMON /SPEIKO/ FF(300,9)                                         EXP  310
C                                                                       EXP  320
CFZJ048 enlarged dimension                                    11.04.07  EXP  330
      COMMON /VARDIT/ B(5000000)                                        EXP  340
C                                                                       EXP  350
CFZJ042                                                       09.09.05  EXP  360
      COMMON /ADDRT/ KX(240),KY(240),LZ(240),NENDPT                     EXP  370
C                                                                       EXP  380
      COMMON /EPTI/ EPSST,ZEITNW                                        EXP  390
C                                                                       EXP  400
      COMMON /STA/ IST,SB,TEX,JNS,RMI,RMA                               EXP  410
C                                                                       EXP  420
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             EXP  430
C                                                                       EXP  440
      COMMON /VARDIM/ AA(8000000)                                       EXP  450
C                                                                       EXP  460
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       EXP  470
C                                                                       EXP  480
      COMMON /NUKDAT/ DDLAM(30),IIU(30),FFB1(30),FFP(30),FFP1(30),      EXP  490
     1 FFT(30),FFA(30),FFSF(30),FFNG1(30),FFN2N1(30),QMEV(30)           EXP  500
C                                                                       EXP  510
CFZJ042                                                       09.09.05  EXP  520
      DIMENSION VL(N200),POWL(N200),QNW(N200),DU(IMAZ,NMAZ),PHIP(NMAZ), EXP  530
     1 WT(IMAZ,NMAZ),TQ(IMAZ,NMAZ),RAD(IMAZ+1),PHI(NMAZ+1),TB(50,N200), EXP  540
     2 PGES(50,N200),ZD(7,50,N200),PBET(N200),PBER(NDR),MIX(NDR),       EXP  550
     3 POWPT(IMAZ,NMAZ,15),VOLPT(IMAZ,NMAZ,15),DOSPT(IMAZ,NMAZ,15)      EXP  560
C                                                                       EXP  570
CFZJ042                                                       09.09.05  EXP  580
      EQUIVALENCE(LZ(35),IT),(LI(31),LDEN)                              EXP  590
C                                                                       EXP  600
  240 FORMAT (//20X,'RELATIVE DECAY HEAT POWER AT TIME T =',E12.5,' SEC.EXP  610
     1:'//)                                                             EXP  620
  241 FORMAT (1X,F7.2,1X,100(13F9.5/))                                  EXP  630
  242 FORMAT (9X,13F9.2)                                                EXP  640
  243 FORMAT (/)                                                        EXP  650
  501 FORMAT (/' TIME: ',E12.5,' SEC., AVERAGE OF DECAY HEAT POWER = ', EXP  660
     1 E12.5,' %, MAXIMUM = ',E12.5,' %, MINIMUM = ',E12.5,' %'/' TOTAL EXP  670
     2POWER OF DECAY HEAT = ',E12.5,' W IN VOLUME ',E12.5,' CM**3,  TOT.EXP  680
     3 DECAY HEAT SINCE START (KWH) = ',E12.5/)                         EXP  690
C                                                                       EXP  700
C                                                                       EXP  710
      N51 = 51                                                          EXP  720
      DELDZ = VFLIES * 3600. * 24.                                      EXP  730
      NPK01 = NENDPT + 1                                                EXP  740
      NPK02 = NPK01 + N200                                              EXP  750
      NPK03 = NPK02 + (N200 * 24 * 4)                                   EXP  760
      NENDK0 = NPK03 + N200                                             EXP  770
      ZEITM = ZEITM + ZEITNW                                            EXP  780
C                                                                       EXP  790
      CALL NACHW(ZEITM,N200,QNW,VL,B(NPK01),B(NPK02),B(NPK03),TB,PGES,ZDEXP  800
     1 ,AA(KA(LDEN)),KMAT,PBET)                                         EXP  810
C                                                                       EXP  820
      I = 0                                                             EXP  830
      RMAX = 0.                                                         EXP  840
      RMIN = 1.                                                         EXP  850
      DO 33 L=1,LAYER                                                   EXP  860
        VP = 0.                                                         EXP  870
        VO = 0.                                                         EXP  880
        VB = 0.                                                         EXP  890
        PBER(L) = 0.                                                    EXP  900
        DO 31 M=1,MIX(L)                                                EXP  910
          I = I + 1                                                     EXP  920
          VP = VP + QNW(I)                                              EXP  930
          VB = VB + PBET(I)                                             EXP  940
          VO = VO + VL(I)                                               EXP  950
   31   CONTINUE                                                        EXP  960
        QNW(L) = VP / VO                                                EXP  970
        IF(VB .GT. 0.) PBER(L) = VP / VB                                EXP  980
        RMAX = AMAX1(RMAX,PBER(L))                                      EXP  990
        IF(PBER(L) .GT. 0.) RMIN = AMIN1(RMIN,PBER(L))                  EXP 1000
   33 CONTINUE                                                          EXP 1010
      IF(IST .EQ. 0) RMA = RMAX                                         EXP 1020
      IF(IST .EQ. 0) RMI = RMIN                                         EXP 1030
      LP1 = LAYER + 1                                                   EXP 1040
      DO 34 L=LP1,N197                                                  EXP 1050
        QNW(L) = 0.                                                     EXP 1060
   34 CONTINUE                                                          EXP 1070
      KIND = 2                                                          EXP 1080
      ID = 1                                                            EXP 1090
C                                                                       EXP 1100
CFZJ042                                                       09.09.05  EXP 1110
      CALL VOLMA1(KIND,POWL,QNW,N200,B(KX(IT)),DU,POWPT,VOLPT,DOSPT,NDR,EXP 1120
     1 ID)                                                              EXP 1130
C                                                                       EXP 1140
      WFGES = 0.                                                        EXP 1150
      WTGES = 0.                                                        EXP 1160
      TQGES = 0.                                                        EXP 1170
      PI2 = 2. * PI                                                     EXP 1180
      IM1 = IMAX - 1                                                    EXP 1190
      NM1 = NMAX - 1                                                    EXP 1200
      N1 = NM1                                                          EXP 1210
      N2 = 2                                                            EXP 1220
      I1 = IM1                                                          EXP 1230
      I2 = 1                                                            EXP 1240
      DO 220 I=1,IM1                                                    EXP 1250
        DO 220 N=2,NM1                                                  EXP 1260
          DF = 0.                                                       EXP 1270
          IF(I .NE. 1) R1 = (RAD(I-1)+RAD(I)) / 2.                      EXP 1280
          R2 = RAD(I)                                                   EXP 1290
          R3 = (RAD(I)+RAD(I+1)) / 2.                                   EXP 1300
          P1 = PHIP(N-1)                                                EXP 1310
          P2 = PHI(N)                                                   EXP 1320
          P3 = PHIP(N)                                                  EXP 1330
          EIN = DU(I,N)                                                 EXP 1340
C                                                                       EXP 1350
          DFH = DDF1(R3,R2,P3,P2,INDGEO,R3)                             EXP 1360
C                                                                       EXP 1370
          IF(EIN .LE. 0.) GOTO 40                                       EXP 1380
          DF = DF + DFH                                                 EXP 1390
          WFGES = WFGES + DFH                                           EXP 1400
          WT(I,N) = WT(I,N) + DFH * EIN                                 EXP 1410
   40     CONTINUE                                                      EXP 1420
          IF(I .EQ. 1) GOTO 221                                         EXP 1430
          EIN = DU(I-1,N)                                               EXP 1440
C                                                                       EXP 1450
          DFH = DDF1(R2,R1,P3,P2,INDGEO,R1)                             EXP 1460
C                                                                       EXP 1470
          IF(EIN .LE. 0.) GOTO 41                                       EXP 1480
          DF = DF + DFH                                                 EXP 1490
          WFGES = WFGES + DFH                                           EXP 1500
          WT(I,N) = WT(I,N) + DFH * EIN                                 EXP 1510
   41     CONTINUE                                                      EXP 1520
          EIN = DU(I-1,N-1)                                             EXP 1530
C                                                                       EXP 1540
          DFH = DDF1(R2,R1,P2,P1,INDGEO,R1)                             EXP 1550
C                                                                       EXP 1560
          IF(EIN .LE. 0.) GOTO 42                                       EXP 1570
          DF = DF + DFH                                                 EXP 1580
          WFGES = WFGES + DFH                                           EXP 1590
          WT(I,N) = WT(I,N) + DFH * EIN                                 EXP 1600
   42     CONTINUE                                                      EXP 1610
  221     CONTINUE                                                      EXP 1620
          EIN = DU(I,N-1)                                               EXP 1630
C                                                                       EXP 1640
          DFH = DDF1(R3,R2,P2,P1,INDGEO,R3)                             EXP 1650
C                                                                       EXP 1660
          IF(EIN .LE. 0.) GOTO 43                                       EXP 1670
          DF = DF + DFH                                                 EXP 1680
          WFGES = WFGES + DFH                                           EXP 1690
          WT(I,N) = WT(I,N) + DFH * EIN                                 EXP 1700
   43     CONTINUE                                                      EXP 1710
          IF(INDGEO .NE. 2) GOTO 68                                     EXP 1720
          DF = DF * PI2                                                 EXP 1730
          WT(I,N) = WT(I,N) * PI2                                       EXP 1740
   68     CONTINUE                                                      EXP 1750
          IF(DF .EQ. 0.) GOTO 220                                       EXP 1760
          IF(TQ(I,N) .GT. 0.) GOTO 222                                  EXP 1770
          WT(I,N) = 0.                                                  EXP 1780
          GOTO 223                                                      EXP 1790
  222     CONTINUE                                                      EXP 1800
          TQGES = TQGES + TQ(I,N)                                       EXP 1810
          WTGES = WTGES + WT(I,N)                                       EXP 1820
          WT(I,N) = WT(I,N) / TQ(I,N)                                   EXP 1830
  223     CONTINUE                                                      EXP 1840
          N1 = MIN0(N,N1)                                               EXP 1850
          N2 = MAX0(N,N2)                                               EXP 1860
          I1 = MIN0(I,I1)                                               EXP 1870
          I2 = MAX0(I,I2)                                               EXP 1880
  220 CONTINUE                                                          EXP 1890
      WRITE (N51) DU                                                    EXP 1900
      IF(IOPUT .LT. 1 .OR. JNS .LT. 1) GOTO 230                         EXP 1910
      WRITE (6,240) ZEITM                                               EXP 1920
      WRITE (6,242) (RAD(I),I=I1,I2)                                    EXP 1930
      WRITE (6,243)                                                     EXP 1940
      DO 231 N=N1,N2                                                    EXP 1950
        WRITE (6,241) PHI(N),(WT(I,N),I=I1,I2)                          EXP 1960
  231 CONTINUE                                                          EXP 1970
  230 CONTINUE                                                          EXP 1980
      IF(INDGEO.EQ.2) WFGES = WFGES * PI2                               EXP 1990
      WNAWER = WTGES / QTHX * 100.                                      EXP 2000
      SMAX = RMAX * 100.                                                EXP 2010
      SMIN = RMIN * 100.                                                EXP 2020
      WTGINT = WTGINT + WTGES * DT / 3.6E6                              EXP 2030
      IT1 = ITIK(1) + 1                                                 EXP 2040
      FF(IT1,1) = WTGINT * 3600.                                        EXP 2050
      WRITE (6,501) ZEITM,WNAWER,SMAX,SMIN,WTGES,WFGES,WTGINT           EXP 2060
      RETURN                                                            EXP 2070
      END                                                               EXP 2080
      SUBROUTINE ABEND(LEVER)                                           ABE   10
C                                                                       ABE   20
C     BEWIRKT DAS ERROR-HANDLING                                        ABE   30
C                                                                       ABE   40
      COMMON /ERROR/ ABTEXT(20),IERRM,IERRL,IERRF(5),NHIN,NWARN,NFEHL   ABE   50
C                                                                       ABE   60
      CHARACTER*4 ABTEXT                                                ABE   70
C                                                                       ABE   80
CFZJ042                                                       09.09.05  ABE   90
      COMMON /COUPL/ IPRINT                                             ABE  100
C                                                                       ABE  110
   10 FORMAT (///T25,89(1H+)/T25,'+   INTERRUPTION,  PROGRAM ARRIVES AT ABE  120
     1MAX. ERROR LEVEL  ,          ERROR-LEVEL = ',I4,'   +'/T25,'+   STABE  130
     2ATISTICS: ',I3,'-TIMES **CONSIDERATION** ',I3,'-TIMES **WARNING** ABE  140
     3',I3,'-TIMES **ERROR**   +'/T25,89(1H+))                          ABE  150
   20 FORMAT (/////T25,89(1H+)/T25,'+   STATISTICS: ',I3,'-TIMES **CONSIABE  160
     1DERATION** ',I3,'-TIMES **WARNING** ',I3,'-TIMES **ERROR**   +'/  ABE  170
     2 T25,89(1H+))                                                     ABE  180
C                                                                       ABE  190
C                                                                       ABE  200
      IF(LEVER .EQ. 0) GOTO 21                                          ABE  210
      IERRL = IERRL + IERRF(LEVER) * LEVER                              ABE  220
      IF(LEVER .EQ. 1) NHIN = NHIN + 1                                  ABE  230
      IF(LEVER .EQ. 2) NWARN = NWARN + 1                                ABE  240
      IF(LEVER .GE. 3) NFEHL = NFEHL + 1                                ABE  250
      IF(IERRL .LT. IERRM) RETURN                                       ABE  260
      WRITE (6,10) IERRL,NHIN,NWARN,NFEHL                               ABE  270
C                                                                       ABE  280
CFZJ042                                                       09.09.05  ABE  290
      IF(IPRINT .GT. -3) CALL BILD(1,ABTEXT,CP0)                        ABE  300
C                                                                       ABE  310
      CALL EXIT                                                         ABE  320
C                                                                       ABE  330
CFZJ042                                                       09.09.05  ABE  340
   21 IF(IPRINT .GT. -3) WRITE (6,20) NHIN,NWARN,NFEHL                  ABE  350
      RETURN                                                            ABE  360
      END                                                               ABE  370
      SUBROUTINE BILD(IFEND,TEXT,CP0)                                   BIL   10
C                                                                       BIL   20
C     ERSTELLT DECKBLATT UND SCHLUSS-SEITE                              BIL   30
C                                                                       BIL   40
      COMMON /CPBIL/ CPTER,CPKIN,CPSTR,CPGAS                            BIL   50
C                                                                       BIL   60
      COMMON /JAMOTA/ IJ,IM,IT                                          BIL   70
C                                                                       BIL   80
      COMMON /HOUMIN/ IH,IMI                                            BIL   90
C                                                                       BIL  100
CFZJ055                                                       25.09.07  BIL  110
C                                                                       BIL  120
      CHARACTER*4 TEXT(20)                                              BIL  130
C                                                                       BIL  140
   10 FORMAT (1H1//T9,                                                  BIL  150
     1 '@@@@@@@@@@ @@@@   @@@@ @@@@@@@@@@ @@@@@@@@@     @@@@       @@@@ BIL  160
     2 @@@@ @@@@     @@@@           @@@@@@     @@@@@@@@'/T9,'@@@@@@@@@@ BIL  170
     3@@@@   @@@@ @@@@@@@@@@ @@@@   @@@@   @@@@@     @@@@@  @@@@  @@@@  BIL  180
     4 @@@@           @@   @@@    @@@@   @@@'/T9,'   @@@@    @@@@   @@@@BIL  190
     5 @@@@       @@@@    @@@@  @@@@@@   @@@@@@  @@@@   @@@@ @@@@       BIL  200
     6         @@@     @@@@    @@@')                                    BIL  210
   11 FORMAT (////1X,T30,'CASE : ',20A4)                                BIL  220
   12 FORMAT (////1X,T30,'REASON FOR END OF JOB: ',20A4)                BIL  230
   20 FORMAT (////T43,42(1H_)/T42,1H|,T85,1H|/T42,1H|,T85,1H|/T42,1H|,  BIL  240
     1 T50,'DATE OF CALC.  ',T67,': ',2(I2,'.'),I4,T85,1H|/T42,1H|,T85, BIL  250
     2 1H|/T42,1H|,T50,'START TIME',T67,':  ',I2,' : ',I2,T85,1H|/T42,  BIL  260
     3 1H|,42(1H_),1H|)                                                 BIL  270
   25 FORMAT (////T43,42(1H_)/T42,1H|,T85,1H|/T42,1H|,T85,1H|/T42,1H|,  BIL  280
     1 T43,'EXECUTION-TIME TOTAL',T67,':',I7,' SEC',T85,1H|/T42,1H|,T50,BIL  290
     2 '   FOR  TFELD',T67,':',I7,' SEC',T85,1H|/T42,1H|,T50,'        KIBIL  300
     3NEX',T67,':',I7,' SEC',T85,1H|/T42,1H|,T50,'       STROEM',T67,':'BIL  310
     4 ,I7,' SEC',T85,1H|/T42,1H|,T50,'       GASTEM',T67,':',I7,' SEC',BIL  320
     5 T85,1H|/T42,1H|,T50,'        OTHER',T67,':',I7,' SEC',T85,1H|/T42BIL  330
     6 ,1H|,T85,1H|/T42,1H|,T50,'DATE OF CALC.  ',T67,': ',2(I2,'.'),I4,BIL  340
     7 T85,1H|/T42,1H|,T85,1H|/T42,1H|,T50,'JOB-END  ',T67,':  ',I2,' : BIL  350
     8',I2,T85,1H|/T42,1H|,42(1H_),1H|)                                 BIL  360
  101 FORMAT (1X,T9,'   @@@@    @@@@   @@@@ @@@@       @@@@   @@@@   @@@BIL  370
     1@       @@@@  @@@@   @@@@ @@@@            @@@         @@@@    @@@'BIL  380
     2 /T9,'   @@@@    @@@@   @@@@ @@@@@@@@@@ @@@@    @@@@  @@@@       @BIL  390
     3@@@  @@@@  @@@@   @@@@          @@@    @@    @@@@   @@@'/T9,'   @@BIL  400
     4@@    @@@@   @@@@ @@@@@@@@@@ @@@@     @@@@ @@@@       @@@@  @@@@ @BIL  410
     5@@@     @@@@         @@@@@@@@@    @@@@@@@@'///T24,'SOURCE-MODULES:BIL  420
     6 KONVEK1, KONVEK2, THERMIX1, THERMIX2, THERMIX3, THERMIX4, TIK, DEBIL  430
     7CHEAT')                                                           BIL  440
  102 FORMAT (1X,T9,'   @@@@    @@@@@@@@@@@ @@@@@@@@   @@@@   @@@@   @@@BIL  450
     1@ @@ @@ @@@@  @@@@     @@@@@     @@@@@@      @@@      @@@@    @@@@BIL  460
     2'/T9,'   @@@@    @@@@@@@@@@@ @@@@@@@@   @@@@@@@@@@    @@@@  @@@  @BIL  470
     3@@@  @@@@     @@@@@     @@@@@@     @@@       @@@@    @@@@')       BIL  480
 1000 FORMAT (//////37X,'**',5X,'**',7X,7('*'),8X,7('*'),7X,8('*')/37X, BIL  490
     1 '**',5X,'**',3(6X,'**',5X,'**')/37X,'**',5X,'**',6X,'**',13X,'**'BIL  500
     2 ,5X,'**',6X,8('*')/38X,'**',3X,'**',8X,7('*'),7X,'**',5X,'**',6X,BIL  510
     3 '**'/39X,'**',1X,'**',4X,'**',2X,'*',6X,'**',2(2X,'**'),5X,'**', BIL  520
     4 2(2X,'**'),9X,'**'/40X,'***',5X,'**',3X,7('*'),3X,'**',3X,7('*'),BIL  530
     5 3X,'**',2X,'**',9X,'**')                                         BIL  540
 1001 FORMAT (//////8X,'JAN. 2012',87X,'REPORT: V.S.O.P.(99/11)'/112X,'JBIL  550
     1UEL - 4348'/8X,'JUNE 2010',87X,'REPORT: V.S.O.P.(99/09)'/112X,'JUEBIL  551
     2L - 4326'/112X,'SECTION 4.4.12')                                  BIL  560
C                                                                       BIL  570
C                                                                       BIL  580
      IF(IFEND .EQ. 0) CALL WATCH(CP)                                   BIL  590
C                                                                       BIL  600
      CP0 = CP                                                          BIL  610
      WRITE (6,10)                                                      BIL  620
      WRITE (6,102)                                                     BIL  630
      WRITE (6,101)                                                     BIL  640
      WRITE (6,1000)                                                    BIL  650
      WRITE (6,1001)                                                    BIL  660
      IF(IFEND .EQ. 1) GOTO 100                                         BIL  670
      CPTER = 0.                                                        BIL  680
      CPKIN = 0.                                                        BIL  690
      CPSTR = 0.                                                        BIL  700
      CPGAS = 0.                                                        BIL  710
      WRITE (6,11) TEXT                                                 BIL  720
C                                                                       BIL  730
      CALL DATEMS                                                       BIL  740
C                                                                       BIL  750
      WRITE (6,20) IT,IM,IJ,IH,IMI                                      BIL  760
      RETURN                                                            BIL  770
  100 CONTINUE                                                          BIL  780
      WRITE (6,12) TEXT                                                 BIL  790
C                                                                       BIL  800
      CALL DATEMS                                                       BIL  810
C                                                                       BIL  820
      CALL WATCH(ENDE)                                                  BIL  830
C                                                                       BIL  840
      IB = IFIX(ENDE)                                                   BIL  850
      IB = IB - IFIX(CP)                                                BIL  860
      ICP = IB                                                          BIL  870
      ICPT = IFIX(CPTER)                                                BIL  880
      ICPK = IFIX(CPKIN)                                                BIL  890
      ICPS = IFIX(CPSTR)                                                BIL  900
      ICPG = IFIX(CPGAS)                                                BIL  910
      CPTER = CPTER + CPKIN + CPSTR + CPGAS                             BIL  920
      ICPSO = IB-IFIX(CPTER)                                            BIL  930
      WRITE (6,25) ICP,ICPT,ICPK,ICPS,ICPG,ICPSO,IT,IM,IJ,IH,IMI        BIL  940
      RETURN                                                            BIL  950
      END                                                               BIL  960
      SUBROUTINE BUBIL(GEOFAK,BUINS,T,TFLU,AR,BU,KOM)                   BUB   10
C                                                                       BUB   20
C     BERECHNET DIE AKTUELLEN KONVEKTIVEN WAERMEQUELLEN UND SUMMIERT    BUB   30
C     DIESE KOMPOSITIONSWEISE AUF. WIRD NUR AKTIV, WENN DIE WAERME-     BUB   40
C     QUELLEN MIT ALPHA*F UND TFLU GEBILDET WERDEN (---> IFKO1 = -1).   BUB   50
C                                                                       BUB   60
      COMMON /KOMP1/ KMAX                                               BUB   70
C                                                                       BUB   80
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    BUB   90
C                                                                       BUB  100
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             BUB  110
C                                                                       BUB  120
      DIMENSION BUINS(KMAZ),T(IMAZ,NMAZ),TFLU(IMAZ,NMAZ),AR(IMAZ,NMAZ), BUB  130
     1 BU(IMAZ,NMAZ),KOM(IMAZ,NMAZ)                                     BUB  140
C                                                                       BUB  150
C                                                                       BUB  160
      DO 200 K=1,KMAX                                                   BUB  170
        BUINS(K) = 0.                                                   BUB  180
  200 CONTINUE                                                          BUB  190
      IM1 = IMAX - 1                                                    BUB  200
      NM1 = NMAX - 1                                                    BUB  210
      DO 100 I=1,IM1                                                    BUB  220
        DO 100 N=2,NM1                                                  BUB  230
          TM = (T(I,N)+AR(I,N)) / 2.                                    BUB  240
          K = KOM(I,N)                                                  BUB  250
          B = BU(I,N) * (TFLU(I,N)-TM)                                  BUB  260
          BUINS(K) = BUINS(K) + B / GEOFAK / 2.                         BUB  270
  100 CONTINUE                                                          BUB  280
      RETURN                                                            BUB  290
      END                                                               BUB  300
      FUNCTION CALT(I,N,IFKO1,T,WI,WL,WT,TFLU,BU1,BU)                   ALT   10
C                                                                       ALT   20
C     BERECHNET DIE STATIONAEREN FESTSTOFFTEMPERATUREN                  ALT   30
C                                                                       ALT   40
      COMMON /BASIS/ TBASE                                              ALT   50
C                                                                       ALT   60
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             ALT   70
C                                                                       ALT   80
      DIMENSION T(IMAZ,NMAZ),WI(IMAZ,NMAZ),WL(IMAZ,NMAZ),WT(IMAZ,NMAZ), ALT   90
     1 TFLU(IMAZ,NMAZ),BU1(IMAZ,NMAZ),BU(IMAZ,NMAZ)                     ALT  100
C                                                                       ALT  110
C                                                                       ALT  120
      TNN = T(I,N-1)                                                    ALT  130
      TFF = 1.                                                          ALT  140
      IF(IFKO1 .EQ. -1) TFF = TFLU(I,N) - TBASE - TNN                   ALT  150
      B1 = BU(I,N) * TFF + BU1(I,N)                                     ALT  160
      B2 = BU(I,N)                                                      ALT  170
      IF(IFKO1 .NE. -1) B2 = 0.                                         ALT  180
      IF(I .EQ. 1) GOTO 10                                              ALT  190
C                                                                       ALT  200
      CALT = ((T(I-1,N)-TNN)*WI(I-1,N)+(T(I,N+1)-TNN)*WL(I,N)+(T(I+1,N)-ALT  210
     1 TNN)*WI(I,N)+B1) / (WT(I,N)+B2) + TNN                            ALT  220
C                                                                       ALT  230
      RETURN                                                            ALT  240
C                                                                       ALT  250
   10 CALT = ((T(I,N+1)-TNN)*WL(I,N)+(T(I+1,N)-TNN)*WI(I,N)+B1) /       ALT  260
     1 (WT(I,N)+B2) + TNN                                               ALT  270
C                                                                       ALT  280
      RETURN                                                            ALT  290
      END                                                               ALT  300
      SUBROUTINE CALTA(I,N,IFKO1,NHZON,KKB,WQN,WQK,NHMAT1,XFWQZ,ZKUG,WWKLTA   10
     1 ,T,WI,WL,WT,TFLU,LAM,DOS,ZDOS,POWPT,VOLPT,DOSPT,THETNEW)         LTA   20
C                                                                       LTA   30
CFZJ042                                                       09.09.05  LTA   40
C                                                                       LTA   50
C     BERECHNUNG DER HETEROGENEN FESTSTOFFTEMPERATUREN (STATIONAERE     LTA   60
C     RECHNUNG).LOESUNG DES GLEICHUNGSSYSTEMS WIE IN SR CALT2 (MATRIX-  LTA   70
C     ELIMINATION)                                                      LTA   80
C     *** VORBEREITUNG FUER SR CALTAH ***                               LTA   90
C                                                                       LTA  100
CFZJ042                                                       09.09.05  LTA  110
      COMMON /BLINDL/ TMITL,M24,NGEOM,CIZET0                            LTA  120
C                                                                       LTA  130
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             LTA  140
C                                                                       LTA  150
CFZJ042                                                       09.09.05  LTA  160
      COMMON /BLF/ IPASS                                                LTA  170
C                                                                       LTA  180
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                LTA  190
C                                                                       LTA  200
CFZJ042                                                       09.09.05  LTA  210
      COMMON /VRT/ MIXM                                                 LTA  220
C                                                                       LTA  230
CFZJ042                                                       09.09.05  LTA  240
      DIMENSION NHZON(KMAZ),KKB(ICO,NCO),WQN(ICO,NCO),WQK(ICO,NCO),     LTA  250
     1 NHMAT1(KMAZ,5),XFWQZ(KMAZ,5),ZKUG(ICO,NCO),WWK(KMAZ,5),          LTA  260
     2 T(IMAZ,NMAZ),WI(IMAZ,NMAZ),WL(IMAZ,NMAZ),WT(IMAZ,NMAZ),          LTA  270
     3 TFLU(IMAZ,NMAZ),DOS(KMAZ),ZDOS(NMAZ),THE(5),POWPT(IMAZ,NMAZ,15), LTA  280
     4 VOLPT(IMAZ,NMAZ,15),DOSPT(IMAZ,NMAZ,15),THETNEW(ICO,NCO,5,15)    LTA  290
C                                                                       LTA  300
      REAL LAM(KMAZ)                                                    LTA  310
C                                                                       LTA  320
C                                                                       LTA  330
      K1 = 1                                                            LTA  340
      WQNIN = WQN(I,N)                                                  LTA  350
      WQKIN = WQK(I,N)                                                  LTA  360
      KO = KKB(I,N)                                                     LTA  370
      NZ = NHZON(KO)                                                    LTA  380
CFZJ042                                                       09.09.05  LTA  390
      TMITL = 0.                                                        LTA  400
      SVOLPT = 0.                                                       LTA  410
      DO 3 ID=1,MIXM                                                    LTA  420
        SVOLPT = SVOLPT + VOLPT(I+IDIFF,N+NDIFF,ID)                     LTA  430
    3 CONTINUE                                                          LTA  440
      DO 90 ID=1,MIXM                                                   LTA  450
        DO 1 K=1,NZ                                                     LTA  460
          THE(K) = THETNEW(I,N,K,ID)                                    LTA  470
    1   CONTINUE                                                        LTA  480
C                                                                       LTA  490
        CALL CALTAH(I,N,IFKO1,WQNIN,WQKIN,THE,NZ,KO,NHMAT1,XFWQZ,ZKUG,  LTA  500
     1   WWK,T,WI,WL,WT,TFLU,LAM,DOS,ZDOS,POWPT,VOLPT,DOSPT,ID)         LTA  510
C                                                                       LTA  520
        DO 2 K=1,NZ                                                     LTA  530
          THETNEW(I,N,K,ID) = THE(K)                                    LTA  540
    2   CONTINUE                                                        LTA  550
        IF(SVOLPT .EQ. 0.) VOLPT(I+IDIFF,N+NDIFF,ID) = 1. / MIXM        LTA  560
        TMITL = TMITL + THETNEW(I,N,K1,ID) * VOLPT(I+IDIFF,N+NDIFF,ID)  LTA  570
   90 CONTINUE                                                          LTA  580
      RETURN                                                            LTA  590
      END                                                               LTA  600
      SUBROUTINE CALTAH(I,N,IFKO1,WQNIN,WQKIN,THE,NZ,KO,NHMAT1,XFWQZ,   TAH   10
     1 ZKUG,WWK,T,WI,WL,WT,TFLU,LAM,DOS,ZDOS,POWPT,VOLPT,DOSPT,ID)      TAH   20
C                                                                       TAH   30
CFZJ042                                                       09.09.05  TAH   40
C                                                                       TAH   50
C     BERECHNUNG DER HETEROGENEN FESTSTOFFTEMPERATUREN (STATIONAERE     TAH   60
C     RECHNUNG).LOESUNG DES GLEICHUNGSSYSTEMS WIE IN SR CALT2 (MATRIX-  TAH   70
C     ELIMINATION)                                                      TAH   80
C                                                                       TAH   90
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                TAH  100
C                                                                       TAH  110
      COMMON /BASIS/ TBASE                                              TAH  120
C                                                                       TAH  130
CFZJ055                                                       25.09.07  TAH  140
C                                                                       TAH  150
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             TAH  160
C                                                                       TAH  170
CFZJ042                                                       09.09.05  TAH  180
      COMMON /BLF/ IPASS                                                TAH  190
C                                                                       TAH  200
      COMMON /PDTX/ PDTHX,RENORM,WQSUM,QNORM1,INORM                     TAH  210
C                                                                       TAH  220
CFZJ042                                                       09.09.05  TAH  230
      DIMENSION NHMAT1(KMAZ,5),XFWQZ(KMAZ,5),ZKUG(ICO,NCO),WWK(KMAZ,5), TAH  240
     1 T(IMAZ,NMAZ),WI(IMAZ,NMAZ),WL(IMAZ,NMAZ),WT(IMAZ,NMAZ),          TAH  250
     2 TFLU(IMAZ,NMAZ),DOS(KMAZ),ZDOS(NMAZ),THE(5),TM(4),WC(5),XINH(5), TAH  260
     3 XKOEFF(5,5),X(5),POWPT(IMAZ,NMAZ,15),VOLPT(IMAZ,NMAZ,15),        TAH  270
     4 DOSPT(IMAZ,NMAZ,15)                                              TAH  280
C                                                                       TAH  290
      REAL LAM(KMAZ)                                                    TAH  300
C                                                                       TAH  310
  500 FORMAT (/' XKOEFF(',I2,',',I2,') = 0.,   I =',I3,'   N =',I3,'   WTAH  320
     1TT =',E12.5,'   WQ1 =',E12.5,'   WQ2 =',E12.5/)                   TAH  330
  501 FORMAT (/' WC(K), K=1,5',52X,'THE(K), K=1,5'/)                    TAH  340
  502 FORMAT (5E12.5,5X,5E12.5)                                         TAH  350
C                                                                       TAH  360
C                                                                       TAH  370
      IFPRNT = 0                                                        TAH  380
      IA = I + IDIFF                                                    TAH  390
      NA = N + NDIFF                                                    TAH  400
      TF = TFLU(IA,NA)                                                  TAH  410
      BRKON = 1.                                                        TAH  420
      IF(IFKO1 .EQ. -1) GOTO 9                                          TAH  430
      X1 = 1.                                                           TAH  440
      TT = TF - T(IA,NA)                                                TAH  450
      IF(ABS(TT) .LT. X1) TT = SIGN(X1,TT)                              TAH  460
      BRKON = (TF-THE(1)) / TT                                          TAH  470
    9 CONTINUE                                                          TAH  480
      WTT = WT(IA,NA) / ZKUG(I,N)                                       TAH  490
      WC(1) = WL(IA,NA-1)                                               TAH  500
      WC(3) = WL(IA,NA)                                                 TAH  510
      WC(4) = WI(IA,NA)                                                 TAH  520
      TM(1) = T(IA,NA-1) + TBASE                                        TAH  530
      TM(3) = T(IA,NA+1) + TBASE                                        TAH  540
      TM(4) = T(IA+1,NA) + TBASE                                        TAH  550
      IF(IA .EQ. 1) GOTO 125                                            TAH  560
      WC(2) = WI(IA-1,NA)                                               TAH  570
CFZJ042                                                       09.09.05  TAH  580
      TM(2) = T(IA-1,NA) + TBASE                                        TAH  590
      GOTO 126                                                          TAH  600
  125 WC(2) = 0.                                                        TAH  610
      TM(2) = 0.                                                        TAH  620
  126 ARAND = 0.                                                        TAH  630
      DO 10 J=1,4                                                       TAH  640
        ARAND = ARAND + WC(J) * TM(J) / ZKUG(I,N)                       TAH  650
   10 CONTINUE                                                          TAH  660
      WQ1 = WQKIN / ZKUG(I,N) * BRKON                                   TAH  670
      IF(IFKO1 .EQ. -1) WTT = WTT + WQ1                                 TAH  680
      IF(IFKO1 .EQ. -1) WQ1 = WQ1 * TF                                  TAH  690
      WQ2 = 0.                                                          TAH  700
CFZJ033                                                       17.08.04  TAH  710
CFZJ038                                                       14.12.04  TAH  720
CFZJ042                                                       09.09.05  TAH  730
      IF(VOLPT(IA,NA,ID) .GT. 0.) WQ2 = RENORM * WQNIN * POWPT(IA,NA,ID)TAH  740
     1 / (ZKUG(I,N)*VOLPT(IA,NA,ID))                                    TAH  750
      WQSUM = WQSUM + WQ2 * (ZKUG(I,N)*VOLPT(IA,NA,ID))                 TAH  760
      NZM = NZ - 1                                                      TAH  770
CFZJ042                                                       09.09.05  TAH  780
      DO 15 K=1,NZM                                                     TAH  790
        TT = (THE(K)+THE(K+1)) / 2.                                     TAH  800
        DO = DOSPT(IA,NA,ID)                                            TAH  810
        IF(DO .LT. 0.) DO = ZDOS(NA)                                    TAH  820
        MAT = NHMAT1(KO,K)                                              TAH  830
        ALAM0 = LAM(KO)                                                 TAH  840
        TIRR = 950.                                                     TAH  850
CFZJ026 Heat conductivity = f(irradiation temp. TIRR + dose)  15.03.04  TAH  860
C                                                                       TAH  870
        IF(WQ2 .GT. 0. .OR. K .EQ. 1) CALL SLAMT(TT,DO,MAT,ALAM0,IFPRNT,TAH  880
     1   XLAMT,TIRR)                                                    TAH  890
C                                                                       TAH  900
        IF(WQ2 .GT. 0. .OR. K .EQ. 1) XLA = XLAMT                       TAH  910
        WC(K) = WWK(KO,K) * XLA                                         TAH  920
   15 CONTINUE                                                          TAH  930
      DO 16 J=1,NZ                                                      TAH  940
        DO 16 K=1,NZ                                                    TAH  950
          XKOEFF(J,K) = 0.                                              TAH  960
   16 CONTINUE                                                          TAH  970
      XKOEFF(1,1) = -WTT - WC(1)                                        TAH  980
      XKOEFF(1,2) = WC(1)                                               TAH  990
      XKOEFF(NZ,NZ-1) = WC(NZ-1)                                        TAH 1000
      XKOEFF(NZ,NZ) = -WC(NZ-1)                                         TAH 1010
      XINH(1) = -ARAND - WQ1 - WQ2 * XFWQZ(KO,1)                        TAH 1020
      XINH(NZ) = -WQ2 * XFWQZ(KO,NZ)                                    TAH 1030
      IF(NZ .LE. 2) GOTO 21                                             TAH 1040
      DO 20 NK=2,NZM                                                    TAH 1050
        XINH(NK) = -WQ2 * XFWQZ(KO,NK)                                  TAH 1060
        XKOEFF(NK,NK-1) = WC(NK-1)                                      TAH 1070
        XKOEFF(NK,NK) = -WC(NK) - WC(NK-1)                              TAH 1080
        XKOEFF(NK,NK+1) = WC(NK)                                        TAH 1090
   20 CONTINUE                                                          TAH 1100
   21 CONTINUE                                                          TAH 1110
      DO 30 K=1,NZM                                                     TAH 1120
        K1 = K + 1                                                      TAH 1130
        XMULT = XKOEFF(K1,K) / XKOEFF(K,K)                              TAH 1140
        DO 40 KK=1,NZ                                                   TAH 1150
          XKOEFF(K1,KK) = XKOEFF(K1,KK) - XMULT * XKOEFF(K,KK)          TAH 1160
   40   CONTINUE                                                        TAH 1170
        XINH(K1) = XINH(K1) - XMULT * XINH(K)                           TAH 1180
   30 CONTINUE                                                          TAH 1190
      DO 35 J=1,NZ                                                      TAH 1200
        IF(XKOEFF(J,J) .NE. 0.) GOTO 35                                 TAH 1210
        WRITE (6,500) J,J,I,N,WTT,WQ1,WQ2                               TAH 1220
        WRITE (6,501)                                                   TAH 1230
        WRITE (6,502) (WC(K),K=1,5),(THE(K),K=1,5)                      TAH 1240
        XKOEFF(J,J) = XINH(J) / THE(J)                                  TAH 1250
   35 CONTINUE                                                          TAH 1260
      DO 50 K=1,NZ                                                      TAH 1270
        KK = NZ + 1 - K                                                 TAH 1280
        X(KK) = XINH(KK) / XKOEFF(KK,KK)                                TAH 1290
        THE(KK) = X(KK)                                                 TAH 1300
        IF(KK .EQ. 1) GOTO 50                                           TAH 1310
        XINH(KK-1) = XINH(KK-1) - X(KK) * XKOEFF(KK-1,KK)               TAH 1320
   50 CONTINUE                                                          TAH 1330
      RETURN                                                            TAH 1340
      END                                                               TAH 1350
      FUNCTION CALT1(I,N,IFKO1,AR,BU,T,WI,WL,WT,TFLU,BU1)               LT1   10
C                                                                       LT1   20
C     BERECHNET DIE HOMOGENEN FESTSTOFFTEMPERATUREN (INSTATIONAER)      LT1   30
C                                                                       LT1   40
      COMMON /BASIS/ TBASE                                              LT1   50
C                                                                       LT1   60
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             LT1   70
C                                                                       LT1   80
      DIMENSION AR(IMAZ,NMAZ),BU(IMAZ,NMAZ),T(IMAZ,NMAZ),WI(IMAZ,NMAZ), LT1   90
     1 WL(IMAZ,NMAZ),WT(IMAZ,NMAZ),TFLU(IMAZ,NMAZ),BU1(IMAZ,NMAZ),TM(4),LT1  100
     2 WC(4)                                                            LT1  110
C                                                                       LT1  120
C                                                                       LT1  130
      TF = 1.                                                           LT1  140
      TM(1) = T(I,N-1) + AR(I,N-1)                                      LT1  150
      TM(3) = T(I,N+1) + AR(I,N+1)                                      LT1  160
      TM(4) = T(I+1,N) + AR(I+1,N)                                      LT1  170
      IF(IFKO1 .EQ. -1) TF = TFLU(I,N) - TBASE                          LT1  180
      WC(1) = WL(I,N-1)                                                 LT1  190
      WC(3) = WL(I,N)                                                   LT1  200
      WC(4) = WI(I,N)                                                   LT1  210
      IF(I .EQ. 1) GOTO 125                                             LT1  220
      TM(2) = T(I-1,N) + AR(I-1,N)                                      LT1  230
      WC(2) = WI(I-1,N)                                                 LT1  240
      GOTO 126                                                          LT1  250
  125 WC(2) = 0.                                                        LT1  260
      TM(2) = 0.                                                        LT1  270
  126 TST = 2. * AR(I,N)                                                LT1  280
      XZ = 0.                                                           LT1  290
      B1 = BU(I,N) * TF + BU1(I,N)                                      LT1  300
      B2 = BU(I,N) / 2.                                                 LT1  310
      IF(IFKO1 .NE. -1) B2 = 0.                                         LT1  320
      DO 10 J=1,4                                                       LT1  330
        XZ = XZ + (TM(J)-TST) * WC(J)                                   LT1  340
   10 CONTINUE                                                          LT1  350
      XZ = XZ + B1 - B2 * TST                                           LT1  360
      XN = WT(I,N) + B2                                                 LT1  370
C                                                                       LT1  380
      CALT1 = XZ / XN + AR(I,N)                                         LT1  390
C                                                                       LT1  400
      RETURN                                                            LT1  410
      END                                                               LT1  420
      SUBROUTINE CALT2(I,N,IFKO1,NHZON,KKB,THALT,WQN,WQK,AR,NHMAT1,XFWQZLT2   10
     1 ,ZKUG,WWK,WKAPH,T,WI,WL,WT,TFLU,LAM,DOS,ZDOS,THETNEW)            LT2   20
C                                                                       LT2   30
CFZJ042                                                       09.09.05  LT2   40
C                                                                       LT2   50
C     BERECHNUNG DER HETEROGENEN FESTSTOFFTEMPERATUREN (KUGELZONEN-     LT2   60
C     TEMPERATUREN),LOESUNG DES GLEICHUNGSSYSTEMS DURCH TRIDIAG,        LT2   70
C     MATRIX-ELIMINATION (GAUSS)                                        LT2   80
C     *** VORBEREITUNG FUER SR CALT2H ***                               LT2   90
C                                                                       LT2  100
CFZJ042                                                       09.09.05  LT2  110
      COMMON /BLINDL/ TMITL,M24,NGEOM,CIZET0                            LT2  120
C                                                                       LT2  130
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             LT2  140
C                                                                       LT2  150
CFZJ042                                                       09.09.05  LT2  160
      COMMON /BLF/ IPASS                                                LT2  170
C                                                                       LT2  180
      COMMON /FELD2/ IDIFF,NDIFF                                        LT2  190
C                                                                       LT2  200
CFZJ042                                                       09.09.05  LT2  210
      DIMENSION NHZON(KMAZ),KKB(ICO,NCO),THALT(ICO,NCO,5),WQN(ICO,NCO), LT2  220
     1 WQK(ICO,NCO),AR(IMAZ,NMAZ),NHMAT1(KMAZ,5),XFWQZ(KMAZ,5),         LT2  230
     2 ZKUG(ICO,NCO),WWK(KMAZ,5),WKAPH(ICO,NCO,5),T(IMAZ,NMAZ),         LT2  240
     3 WI(IMAZ,NMAZ),WL(IMAZ,NMAZ),WT(IMAZ,NMAZ),TFLU(IMAZ,NMAZ),       LT2  250
     4 DOS(KMAZ),ZDOS(NMAZ),THE(5),THA(5),THETNEW(ICO,NCO,5,15)         LT2  260
C                                                                       LT2  270
      REAL LAM(KMAZ)                                                    LT2  280
C                                                                       LT2  290
C                                                                       LT2  300
      K1 = 1                                                            LT2  310
      WQNIN = WQN(I,N)                                                  LT2  320
      WQKIN = WQK(I,N)                                                  LT2  330
      KO = KKB(I,N)                                                     LT2  340
      NZ = NHZON(KO)                                                    LT2  350
CFZJ042                                                       09.09.05  LT2  360
      DO 1 K=1,NZ                                                       LT2  370
        THE(K) = THETNEW(I,N,K,1)                                       LT2  380
        THA(K) = THALT(I,N,K)                                           LT2  390
    1 CONTINUE                                                          LT2  400
C                                                                       LT2  410
      CALL CALT2H(I,N,IFKO1,WQNIN,WQKIN,THE,THA,NZ,KO,AR,NHMAT1,XFWQZ,  LT2  420
     1 ZKUG,WWK,WKAPH,T,WI,WL,WT,TFLU,LAM,DOS,ZDOS)                     LT2  430
C                                                                       LT2  440
      DO 2 K=1,NZ                                                       LT2  450
        THETNEW(I,N,K,1) = THE(K)                                       LT2  460
    2 CONTINUE                                                          LT2  470
      TMITL = THETNEW(I,N,K1,1)                                         LT2  480
      RETURN                                                            LT2  490
      END                                                               LT2  500
      SUBROUTINE CALT2H(I,N,IFKO1,WQNIN,WQKIN,THE,THA,NZ,KO,AR,NHMAT1,  T2H   10
     1 XFWQZ,ZKUG,WWK,WKAPH,T,WI,WL,WT,TFLU,LAM,DOS,ZDOS)               T2H   20
C                                                                       T2H   30
CFZJ042                                                       09.09.05  T2H   40
C                                                                       T2H   50
C     BERECHNUNG DER HETEROGENEN FESTSTOFFTEMPERATUREN (KUGELZONEN-     T2H   60
C     TEMPERATUREN).LOESUNG DES GLEICHUNGSSYSTEMS DURCH TRIDIAG.        T2H   70
C     MATRIX-ELIMINATION (GAUSS)                                        T2H   80
C                                                                       T2H   90
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                T2H  100
C                                                                       T2H  110
      COMMON /BASIS/ TBASE                                              T2H  120
C                                                                       T2H  130
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             T2H  140
C                                                                       T2H  150
CFZJ042                                                       09.09.05  T2H  160
      COMMON /BLF/ IPASS                                                T2H  170
C                                                                       T2H  180
CFZJ042                                                       09.09.05  T2H  190
      DIMENSION AR(IMAZ,NMAZ),NHMAT1(KMAZ,5),XFWQZ(KMAZ,5),ZKUG(ICO,NCO)T2H  200
     1 ,WWK(KMAZ,5),WKAPH(ICO,NCO,5),T(IMAZ,NMAZ),WI(IMAZ,NMAZ),        T2H  210
     2 WL(IMAZ,NMAZ),WT(IMAZ,NMAZ),TFLU(IMAZ,NMAZ),DOS(KMAZ),ZDOS(NMAZ),T2H  220
     3 TM(4),WC(5),XINH(5),XKOEFF(5,5),X(5),THE(5),THA(5)               T2H  230
C                                                                       T2H  240
      REAL LAM(KMAZ)                                                    T2H  250
C                                                                       T2H  260
  500 FORMAT (/' XKOEFF(',I2,',',I2,') = 0.,   I =',I3,'   N =',I3,'   WT2H  270
     1TT =',E12.5,'   WQ1 =',E12.5,'   WQ2 =',E12.5/)                   T2H  280
  501 FORMAT (/' WC(K), K=1,NZ',52X,'THE(K), K=1,NZ'/)                  T2H  290
  502 FORMAT (5E12.5,5X,5E12.5)                                         T2H  300
  503 FORMAT (//' THA(K), K=1,NZ'/)                                     T2H  310
  504 FORMAT (5E12.5)                                                   T2H  320
C                                                                       T2H  330
C                                                                       T2H  340
      IFPRNT = 0                                                        T2H  350
      IA = I + IDIFF                                                    T2H  360
      NA = N + NDIFF                                                    T2H  370
      TF = TFLU(IA,NA)                                                  T2H  380
      BRKON = 1.                                                        T2H  390
      WTT = WT(IA,NA) / ZKUG(I,N)                                       T2H  400
      WC(1) = WL(IA,NA-1)                                               T2H  410
      WC(3) = WL(IA,NA)                                                 T2H  420
      WC(4) = WI(IA,NA)                                                 T2H  430
      TM(1) = (T(IA,NA-1)+AR(IA,NA-1)) / 2. + TBASE                     T2H  440
      TM(3) = (T(IA,NA+1)+AR(IA,NA+1)) / 2. + TBASE                     T2H  450
      TM(4) = (T(IA+1,NA)+AR(IA+1,NA)) / 2. + TBASE                     T2H  460
      IF(IA .EQ. 1) GOTO 125                                            T2H  470
      WC(2) = WI(IA-1,NA)                                               T2H  480
      TM(2) = (T(IA-1,NA)+AR(IA-1,NA)) / 2. + TBASE                     T2H  490
      GOTO 126                                                          T2H  500
  125 WC(2) = 0.                                                        T2H  510
      TM(2) = 0.                                                        T2H  520
  126 ARAND = 0.                                                        T2H  530
      DO 10 J=1,4                                                       T2H  540
        ARAND = ARAND + WC(J) * TM(J) / ZKUG(I,N)                       T2H  550
   10 CONTINUE                                                          T2H  560
      WQ1 = WQKIN / ZKUG(I,N) * BRKON                                   T2H  570
      IF(IFKO1 .EQ. -1) WTT = WTT + WQ1                                 T2H  580
      IF(IFKO1 .EQ. -1) WQ1 = WQ1 * TF                                  T2H  590
CFZJ042                                                       09.09.05  T2H  600
      WQ2 = WQNIN / ZKUG(I,N)                                           T2H  610
      NZM = NZ - 1                                                      T2H  620
      DO 15 K=1,NZM                                                     T2H  630
        TT = (THE(K)+THE(K+1)) / 2.                                     T2H  640
        DO = DOS(KO)                                                    T2H  650
        IF(DO .LT. 0.) DO = ZDOS(NA)                                    T2H  660
        MAT = NHMAT1(KO,K)                                              T2H  670
        ALAM0 = LAM(KO)                                                 T2H  680
CFZJ026 Heat conductivity = f(irradiation temp. TIRR + dose)  15.03.04  T2H  690
C                                                                       T2H  700
        TIRR = 950.                                                     T2H  710
        IF(WQ2 .GT. 0. .OR. K .EQ. 1) CALL SLAMT(TT,DO,MAT,ALAM0,IFPRNT,T2H  720
     1   XLAMT,TIRR)                                                    T2H  730
C                                                                       T2H  740
        IF(WQ2 .GT. 0. .OR. K .EQ. 1) XLA = XLAMT                       T2H  750
C                                                                       T2H  760
        WC(K) = WWK(KO,K) * XLA                                         T2H  770
   15 CONTINUE                                                          T2H  780
      DO 16 J=1,NZ                                                      T2H  790
        DO 16 K=1,NZ                                                    T2H  800
          XKOEFF(J,K) = 0.                                              T2H  810
   16 CONTINUE                                                          T2H  820
CFZJ042                                                       09.09.05  T2H  830
      WKA1 = WKAPH(I,N,1)                                               T2H  840
      XKOEFF(1,1) = -WTT - WC(1) - 2. * WKA1                            T2H  850
      XKOEFF(1,2) = WC(1)                                               T2H  860
      XKOEFF(NZ,NZ-1) = WC(NZ-1)                                        T2H  870
CFZJ042                                                       09.09.05  T2H  880
      WKA = WKAPH(I,N,NZ)                                               T2H  890
      XKOEFF(NZ,NZ) = -WC(NZ-1) - 2. * WKA                              T2H  900
      XINH(1) = -ARAND - WQ1 - WQ2 * XFWQZ(KO,1) - 2. * WKA1 * THA(1)   T2H  910
      XINH(NZ) = -WQ2 * XFWQZ(KO,NZ) - 2. * WKA * THA(NZ)               T2H  920
      IF(NZ .LE. 2) GOTO 21                                             T2H  930
CFZJ042                                                       09.09.05  T2H  940
      DO 20 NK=2,NZM                                                    T2H  950
        WKA = WKAPH(I,N,NK)                                             T2H  960
        XINH(NK) = -WQ2 * XFWQZ(KO,NK) - 2. * WKA * THA(NK)             T2H  970
        XKOEFF(NK,NK-1) = WC(NK-1)                                      T2H  980
        XKOEFF(NK,NK) = -WC(NK) - WC(NK-1) - 2. * WKA                   T2H  990
        XKOEFF(NK,NK+1) = WC(NK)                                        T2H 1000
   20 CONTINUE                                                          T2H 1010
   21 CONTINUE                                                          T2H 1020
      DO 30 K=1,NZM                                                     T2H 1030
        K1 = K + 1                                                      T2H 1040
        XMULT = XKOEFF(K1,K) / XKOEFF(K,K)                              T2H 1050
        DO 40 KK=1,NZ                                                   T2H 1060
          XKOEFF(K1,KK) = XKOEFF(K1,KK) - XMULT * XKOEFF(K,KK)          T2H 1070
   40   CONTINUE                                                        T2H 1080
        XINH(K1) = XINH(K1) - XMULT * XINH(K)                           T2H 1090
   30 CONTINUE                                                          T2H 1100
      DO 35 J=1,NZ                                                      T2H 1110
        IF(XKOEFF(J,J) .NE. 0.) GOTO 35                                 T2H 1120
        WRITE (6,500) J,J,I,N,WTT,WQ1,WQ2                               T2H 1130
        WRITE (6,501)                                                   T2H 1140
        WRITE (6,502) (WC(K),K=1,NZ),(THE(K),K=1,NZ)                    T2H 1150
        WRITE (6,503)                                                   T2H 1160
        WRITE (6,504) (THA(K),K=1,NZ)                                   T2H 1170
        XKOEFF(J,J) = XINH(J) / THA(J)                                  T2H 1180
   35 CONTINUE                                                          T2H 1190
      DO 50 K=1,NZ                                                      T2H 1200
        KK = NZ + 1 - K                                                 T2H 1210
        X(KK) = XINH(KK) / XKOEFF(KK,KK)                                T2H 1220
        THE(KK) = 2. * X(KK) - THA(KK)                                  T2H 1230
        IF(KK .EQ. 1) GOTO 50                                           T2H 1240
        XINH(KK-1) = XINH(KK-1) - X(KK) * XKOEFF(KK-1,KK)               T2H 1250
   50 CONTINUE                                                          T2H 1260
      RETURN                                                            T2H 1270
      END                                                               T2H 1280