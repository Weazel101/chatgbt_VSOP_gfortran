      SUBROUTINE CH7(MAFIA,VOLREG,IREG,SSLUMP,NHOT,VOL,VPART,DEN,HM,    CH7   10
     1 THBURN,FADOS3,HMETAL,TCHG2,KRESHZ,LRZN,NTYP2,TCHG1,N240,NTYP1,   CH7   20
     2 JAD11,DMAT,CONC,SS,NTYSP)                                        CH7   30
C                                                                       CH7   40
CFZJ012   New identification numbers for THERMOS-cell         02.12.03  CH7   50
C         definitions for the spectrum zones                  02.12.03  CH7   60
C                                                                       CH7   70
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    CH7   80
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    CH7   90
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PICH7  100
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 CH7  110
C                                                                       CH7  120
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1CH7  130
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         CH7  140
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    CH7  150
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)CH7  160
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  CH7  170
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    CH7  180
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         CH7  190
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,CH7  200
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            CH7  210
C                                                                       CH7  220
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, CH7  230
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        CH7  240
     2 FIMAKL(20),NOPILE,MREP,MARX(10),NAJB(10),FOJB(10),NFUL(10),NBTOT,CH7  250
     3 NB0,NCY                                                          CH7  260
C                                                                       CH7  270
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), CH7  280
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10CH7  290
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11CH7  300
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         CH7  310
C                                                                       CH7  320
      COMMON /GRENZE/ NURTHM                                            CH7  330
C                                                                       CH7  340
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300),FABC(10),REPC(10),NNECH7  350
     1 ,LISTEQ                                                          CH7  360
C                                                                       CH7  370
      COMMON /VARDIM/ A(8000000)                                        CH7  380
C                                                                       CH7  390
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       CH7  400
C                                                                       CH7  410
      COMMON /FLUXN/ D(361),IACT                                        CH7  420
C                                                                       CH7  430
CFZJ043                                                       23.09.05  CH7  440
      COMMON /NEWC/ NEWCOST                                             CH7  450
C                                                                       CH7  460
      EQUIVALENCE(IPRIN(13),NKOST),(LI(4),LTOSI),(LI(8),LUMRE),         CH7  470
     1 (LI(20),LPOIL),(LI(21),LPINI),(LI(22),LPINA),(LI(23),LNPOI),     CH7  480
     2 (LI(51),LNOPO),(LI(60),LNALT),(LI(68),LANU),(LI(69),LA1),        CH7  490
     3 (LI(70),LA2),(LI(71),LA3),(LI(72),LA4),(LI(73),LA5),             CH7  500
     4 (LI(100),LNNEU),(LI(102),LNRY),(LI(103),LDOR),(LI(60),LNALT),    CH7  510
     5 (LI(107),LAINO),(LI(108),LAIN),(LI(109),LAEX),(LI(110),LVCHG),   CH7  520
     6 (LI(147),LIBOX),(LI(149),LRVCB),(LI(1),LIMAT),(LI(154),LFICO),   CH7  530
     7 (LI(155),LIDFI),(LI(151),LAWT),(LI(65),LTCEL)                    CH7  540
C                                                                       CH7  550
CFZJ012                                                       02.12.03  CH7  560
      DIMENSION VOLREG(NDR),IREG(NDR),SSLUMP(N26,NDR),NHOT(N200),       CH7  570
     1 VOL(N200),VPART(N200),DEN(KMAT,N200),HM(N200),THBURN(N200),      CH7  580
     2 FADOS3(N200),HMETAL(N200),TCHG2(N200),KRESHZ(N200),LRZN(N200),   CH7  590
     3 NTYP2(N200),TCHG1(N240),NTYP1(N240),CONC(KMAT),JAD11(JD11),      CH7  600
     4 DMAT(IACT+1,4,4),SS(N26,N200),NTYSP(NXS)                         CH7  610
C                                                                       CH7  620
C                                                                       CH7  630
      MM = IPRIN(15) + 1                                                CH7  640
CFZJ043                                                       23.09.05  CH7  650
      IF(NEWCOST .GT. 0) GOTO 216                                       CH7  660
      IF(MM .GT. 1) GOTO 225                                            CH7  670
      DO 215 IR=1,N200                                                  CH7  680
        TCHG2(IR) = 0.0                                                 CH7  690
  215 CONTINUE                                                          CH7  700
  216 CONTINUE                                                          CH7  710
      IF(NKOST .LE. 0) GOTO 225                                         CH7  720
C                                                                       CH7  730
C     PREPARE COST CALCULATION                                          CH7  740
C                                                                       CH7  750
      MSTRT = NRSTRT                                                    CH7  760
C                                                                       CH7  770
      CALL MANKO(1,YTIM,YBURN,MSTRT,0,VOL,DEN,TCHG2,KRESHZ,LRZN,NTYP2,  CH7  780
     1 A(KA(LNALT)),N240,NTYP1,A(KA(LAINO)),A(KA(LAIN)),A(KA(LAEX)),    CH7  790
     2 A(KA(LVCHG)),JAD11,DMAT,A(KA(LIMAT)))                            CH7  800
C                                                                       CH7  810
  225 CONTINUE                                                          CH7  820
      YTIM = TXME(1)                                                    CH7  830
      YBURN = TXME(2)                                                   CH7  840
      IF(NRSTRT .GT. 0) GOTO 240                                        CH7  850
      DO 230 IR=1,N200                                                  CH7  860
        TCHG1(IR) = TCHG2(IR) + YTIM                                    CH7  870
  230 CONTINUE                                                          CH7  880
      GOTO 250                                                          CH7  890
C                                                                       CH7  900
C     ARE FUEL MANAGEMENT OPERATIONS TO BE PERFORMED ?                  CH7  910
C                                                                       CH7  920
  240 CONTINUE                                                          CH7  930
      NP1 = NENDP + 1 + KMAT                                            CH7  940
      NP2 = NP1 + N200                                                  CH7  950
      NP3 = NP1 + N200 * 2                                              CH7  960
      NP4 = NP3 + NDR                                                   CH7  970
      NP5 = NP3 + NDR * 2                                               CH7  980
      NP6 = NP3 + NDR * 3                                               CH7  990
      NP7 = NP6 + KMAT                                                  CH7 1000
      NP8 = NP6 + KMAT * 2                                              CH7 1010
      NP81 = NP8 + KMAT                                                 CH7 1020
      NEND8 = NP81 + MBOX                                               CH7 1030
C                                                                       CH7 1040
CFZJ012                                                       02.12.03  CH7 1050
      CALL FUMAN(IREG,A(KA(LPOIL)),A(KA(LPINI)),A(KA(LPINA)),           CH7 1060
     1 A(KA(LNPOI)),NHOT,VOL,DEN,HM,THBURN,FADOS3,A(KA(LNOPO)),HMETAL,  CH7 1070
     2 TCHG2,KRESHZ,LRZN,NTYP2,A(KA(LNALT)),NTYP1,N240,A(KA(LNNEU)),    CH7 1080
     3 A(KA(LNRY)),A(KA(LDOR)),JAD11,A(KA(LIBOX)),A(NP1),A(NP2),A(NP3), CH7 1090
     4 A(NP4),A(NP5),A(NP6),A(NP7),A(NP8),A(NP81),NEND8,A(KA(LRVCB)),   CH7 1100
     5 A(KA(LIMAT)),A(KA(LFICO)),A(KA(LIDFI)),SS,NTYSP,A(KA(LTCEL)))    CH7 1110
C                                                                       CH7 1120
C     END OF OPERATION ?                                                CH7 1130
C                                                                       CH7 1140
  250 CONTINUE                                                          CH7 1150
      IF(JNSTOP .LT. 0) MAFIA = 1                                       CH7 1160
      IF(NKOST .LE. 0) GOTO 450                                         CH7 1170
C                                                                       CH7 1180
C     EXECUTE COST CALCULATION                                          CH7 1190
C                                                                       CH7 1200
      IF(MAFIA .EQ. 1) MM = -MM                                         CH7 1210
      IPRVS = IVSP(6)                                                   CH7 1220
C                                                                       CH7 1230
      CALL MANKO(2,YTIM,YBURN,MM,IPRVS,VOL,DEN,TCHG2,KRESHZ,LRZN,NTYP2, CH7 1240
     1 A(KA(LNALT)),N240,NTYP1,A(KA(LAINO)),A(KA(LAIN)),A(KA(LAEX)),    CH7 1250
     2 A(KA(LVCHG)),JAD11,DMAT,A(KA(LIMAT)))                            CH7 1260
C                                                                       CH7 1270
  450 CONTINUE                                                          CH7 1280
C                                                                       CH7 1290
      IF(NKOST .LE. 0 .AND. INZWXX .GT. 0) CALL LISTE(A(KA(LIMAT)))     CH7 1300
C                                                                       CH7 1310
      IF(NRSTRT .LE. 0) GOTO 50                                         CH7 1320
C                                                                       CH7 1330
      CALL INIT(A(KA(LIMAT)),A(KA(LAWT)))                               CH7 1340
C                                                                       CH7 1350
      IF(NXE .EQ. 1) JN = 0                                             CH7 1360
      IPRIN(12) = JN                                                    CH7 1370
      DO 2060 I=1,NRESHZ                                                CH7 1380
        NTYP1(I) = NTYP2(I)                                             CH7 1390
        IF(TCHG2(I) .GT. 0.) GOTO 2060                                  CH7 1400
        NY = I                                                          CH7 1410
        NCX1 = 1                                                        CH7 1420
        IF(NY .GT. 1) NCX1 = KRESHZ(NY-1) + 1                           CH7 1430
        NCX2 = KRESHZ(NY)                                               CH7 1440
        DO 2040 NCX=NCX1,NCX2                                           CH7 1450
          IR = LRZN(NCX)                                                CH7 1460
          THBURN(IR) = 0.0                                              CH7 1470
          FADOS3(IR) = 0.0                                              CH7 1480
          HM(IR) = 0.0                                                  CH7 1490
          HMETAL(IR) = 0.0                                              CH7 1500
          DO 2040 M=1,IACT                                              CH7 1510
            HMETAL(IR) = HMETAL(IR) + DEN(M,IR)                         CH7 1520
 2040   CONTINUE                                                        CH7 1530
 2060 CONTINUE                                                          CH7 1540
      IF(KUGL .LE. 0) GOTO 1000                                         CH7 1550
      IRRE2 = 0                                                         CH7 1560
      DO 2020 KR=1,NDR                                                  CH7 1570
        IRRE1 = IRRE2 + 1                                               CH7 1580
        IRRE2 = IREG(KR)                                                CH7 1590
        DO 2010 IR=IRRE1,IRRE2                                          CH7 1600
          VPART(IR) = VOL(IR) / VOLREG(KR)                              CH7 1610
 2010   CONTINUE                                                        CH7 1620
 2020 CONTINUE                                                          CH7 1630
 1000 CONTINUE                                                          CH7 1640
      IF(IPRIN(4) .EQ. 0 .AND. JNSTOP .GE. 0) GOTO 150                  CH7 1650
C                                                                       CH7 1660
C     CALCULATE AVERAGE DENSITIES FOR SPECTRUM CALCULATIONS             CH7 1670
C     WRITE ATOM DENSITIES ON DIRECT ACCESS UNIT NDA11                  CH7 1680
C                                                                       CH7 1690
      IE = N26                                                          CH7 1700
      NRO = IACT + 2 + NO                                               CH7 1710
      NRL = NRO + NLUM                                                  CH7 1720
      DO 110 N=1,NXS                                                    CH7 1730
        KR = 1                                                          CH7 1740
        IRRE2 = IREG(KR)                                                CH7 1750
        KREG = KR                                                       CH7 1760
        B = 0.                                                          CH7 1770
        DO 100 M=1,KMAT                                                 CH7 1780
          CONC(M) = 0.0                                                 CH7 1790
  100   CONTINUE                                                        CH7 1800
        DO 102 IR=1,N200                                                CH7 1810
          IXS = NHOT(IR)                                                CH7 1820
          IF(IXS .NE. N) GOTO 102                                       CH7 1830
          IF(NLUM .EQ. 0) GOTO 105                                      CH7 1840
          IRSUB = IR                                                    CH7 1850
  109     CONTINUE                                                      CH7 1860
          IF(IR .LE. IRRE2) GOTO 104                                    CH7 1870
          KR = KR + 1                                                   CH7 1880
          IRRE2 = IREG(KR)                                              CH7 1890
          KREG = KR                                                     CH7 1900
          GOTO 109                                                      CH7 1910
  104     CONTINUE                                                      CH7 1920
C                                                                       CH7 1930
          CALL LUMP(A(KA(LTOSI)),KL(LTOSI),A(KA(LUMRE)),SSLUMP,NHOT,DEN,CH7 1940
     1     A(KA(LANU)),A(KA(LA1)),A(KA(LA2)),A(KA(LA3)),A(KA(LA4)),     CH7 1950
     2     A(KA(LA5)))                                                  CH7 1960
C                                                                       CH7 1970
  105     CONTINUE                                                      CH7 1980
          B = B + VOL(IR)                                               CH7 1990
          DO 101 M=1,KMAT                                               CH7 2000
            IF(M .LE. NRO .OR. M .GT. NRL) GOTO 107                     CH7 2010
            CONC(M) = CONC(M) + DEN(M,IR) * VOL(IR) * SSLUMP(IE,IR)     CH7 2020
            GOTO 101                                                    CH7 2030
  107       CONTINUE                                                    CH7 2040
            CONC(M) = CONC(M) + DEN(M,IR) * VOL(IR)                     CH7 2050
  101     CONTINUE                                                      CH7 2060
  102   CONTINUE                                                        CH7 2070
        DO 103 M=1,KMAT                                                 CH7 2080
          CONC(M) = CONC(M) / B                                         CH7 2090
  103   CONTINUE                                                        CH7 2100
        JSATZ = 2 + N                                                   CH7 2110
        NXT11 = JAD11(JSATZ)                                            CH7 2120
        WRITE (NDA11,REC=NXT11) (CONC(M),M=1,KMAT)                      CH7 2130
        NXT11 = NXT11 + 1                                               CH7 2140
  110 CONTINUE                                                          CH7 2150
  150 CONTINUE                                                          CH7 2160
C                                                                       CH7 2170
C     WRITE RESTART DATA FOR JTPE9 > 0                                  CH7 2180
C                                                                       CH7 2190
      IF(JTPE9 .GT. 0) CALL START1(JAD11)                               CH7 2200
C                                                                       CH7 2210
      IF(MAFIA .EQ. 1) GOTO 50                                          CH7 2220
      IF(IPRIN(4) .EQ. 0) GOTO 70                                       CH7 2230
      IF(ISPEKT(1) .NE. 0) GOTO 70                                      CH7 2240
      MAFIA = 4                                                         CH7 2250
      NURTHM = 0                                                        CH7 2260
      IF(IPRIN(4) .EQ. 2 .OR. IPRIN(4) .EQ. 4) NURTHM = 1               CH7 2270
      IF(NURTHM .EQ. 1) MAFIA = 5                                       CH7 2280
      RETURN                                                            CH7 2290
C                                                                       CH7 2300
C     START A NEW PROBLEM                                               CH7 2310
C                                                                       CH7 2320
   50 CONTINUE                                                          CH7 2330
      JTPE7 = -1                                                        CH7 2340
      MAFIA = 1                                                         CH7 2350
      RETURN                                                            CH7 2360
C                                                                       CH7 2370
C     PERFORM NEW NEUTRONIC AND DEPLETION CALCULATIONS WITHOUT          CH7 2380
C     REPEATING THE SPECTRUM MODULE                                     CH7 2390
C                                                                       CH7 2400
   70 CONTINUE                                                          CH7 2410
      IPRIN(10) = -1                                                    CH7 2420
      MAFIA = 6                                                         CH7 2430
      RETURN                                                            CH7 2440
      END                                                               CH7 2450
      SUBROUTINE MANKO(KE,XTIM,XBURN,KK,IPRVS,VOL,DEN,TCHG2,KRESHZ,LRZN,MAN   10
     1 NTYP2,NALT,N240,NTYP1,AINO,AIN,AEX,VCHG,JAD11,DMAT,IMAT)         MAN   20
C                                                                       MAN   30
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    MAN   40
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    MAN   50
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIMAN   60
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 MAN   70
C                                                                       MAN   80
      COMMON /BLOCKK/ JMAF,B,TBURN,AVO,NRCHG,VAUF,NDUM,Z1,Z2,MXTYP,     MAN   90
     1 NRTP1(10),NRTP2(10),IALT(10),INEU(10),KSM(10),ETA,IQ,GD,F,GLD,   MAN  100
     2 IPRINT,N250,T1                                                   MAN  110
C                                                                       MAN  120
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, MAN  130
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        MAN  140
     2 FIMAKL(20),NOPILE,MREP,MARX(10),NAJB(10),FOJB(10),NFUL(10),NBTOT,MAN  150
     3 NB0,NCY                                                          MAN  160
C                                                                       MAN  170
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), MAN  180
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10MAN  190
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11MAN  200
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         MAN  210
C                                                                       MAN  220
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300),FABC(10),REPC(10),NNEMAN  230
     1 ,LISTEQ                                                          MAN  240
C                                                                       MAN  250
      COMMON /VARDIM/ A(8000000)                                        MAN  260
C                                                                       MAN  270
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       MAN  280
C                                                                       MAN  290
      COMMON /DECAYT/ DECY(30)                                          MAN  300
C                                                                       MAN  310
      COMMON /FLUXN/ D(361),IACT                                        MAN  320
C                                                                       MAN  330
      DIMENSION VOL(N200),DEN(KMAT,N200),TCHG2(N200),KRESHZ(N200),      MAN  340
     1 LRZN(N200),NTYP2(N200),NALT(N240),NTYP1(N240),AINO(KMAT),        MAN  350
     2 AIN(IACT,N240),AEX(IACT,N240),VCHG(N240),JAD11(JD11),            MAN  360
     3 DMAT(IACT+1,4,4),IMAT(KMAT),DS(30),DR(30)                        MAN  370
C                                                                       MAN  380
      EQUIVALENCE(NDUM,MM),(LI(59),LTCH1),(LI(100),LNNEU),(LI(111),LAK),MAN  390
     1 (LI(112),LDK),(LI(113),LFK),(LI(114),LBK),(LI(115),LKQ),         MAN  400
     2 (LI(116),LEKWH),(LI(117),LET),(LI(118),LORE1),(LI(119),LORE2),   MAN  410
     3 (LI(120),LSEP1),(LI(121),LSEP2),(LI(122),LUUKO),(LI(123),LU3IN), MAN  420
     4 (LI(124),LU3EX),(LI(125),LU5IN),(LI(126),LU5EX),(LI(127),LTHIN), MAN  430
     5 (LI(128),LTHEX),(LI(129),LU8IN),(LI(130),LU8EX),(LI(131),LSM1),  MAN  440
     6 (LI(132),LSM2),(LI(133),LU6IN),(LI(134),LU6EX),(LI(135),LPU9I),  MAN  450
     7 (LI(136),LPU9E),(LI(137),LPU0I),(LI(138),LPU0E),(LI(139),LPU1I), MAN  460
     8 (LI(140),LPU1E),(LI(141),LPU2I),(LI(142),LPU2E),(LI(151),LAWT),  MAN  470
     9 (LI(156),LPA3I),(LI(157),LPA3E),(LI(158),LU4IN),(LI(159),LU4EX), MAN  480
     X (LI(160),LNP7I),(LI(161),LNP7E),(LI(162),LPU8I),(LI(163),LPU8E), MAN  490
     Y (LI(164),LAM1I),(LI(165),LAM1E),(LI(166),LA2MI),(LI(167),LA2ME), MAN  500
     Z (LI(168),LAM2I),(LI(169),LAM2E),(LI(170),LAM3I),(LI(171),LAM3E), MAN  510
     Z (LI(172),LCM2I),(LI(173),LCM2E),(LI(174),LCM3I),(LI(175),LCM3E), MAN  520
     Z (LI(176),LCM4I),(LI(177),LCM4E)                                  MAN  530
C                                                                       MAN  540
C     ONLY FOR DECAY!                                                   MAN  550
C     ---------------                                                   MAN  560
C     FOLLOWING NUCLIDES ARE ASSUMED TO DECAY COMPLETELY INTO:          MAN  570
C     TH-233  --->  PA-233 | U -237  --->  NP-237 | U -239  --->  NP-239MAN  580
C     NP-238  --->  PU-238 | NP-240  --->  PU-240 | PU-243  --->  AM-243MAN  590
C     AM-244  --->  CM-244                                              MAN  600
C                                                                       MAN  610
      T1 = 0.01                                                         MAN  620
      KEY = KE                                                          MAN  630
      GOTO(100,200,9000),KEY                                            MAN  640
  100 CONTINUE                                                          MAN  650
      DO 101 I=1,IACT+1                                                 MAN  660
        DO 102 IR=1,4                                                   MAN  670
          DO 103 M=1,4                                                  MAN  680
            DMAT(I,IR,M) = 0.                                           MAN  690
  103     CONTINUE                                                      MAN  700
  102   CONTINUE                                                        MAN  710
  101 CONTINUE                                                          MAN  720
      NRSTRT = KK                                                       MAN  730
      DO 180 IR=1,N240                                                  MAN  740
        VCHG(IR) = 0.0                                                  MAN  750
        DO 170 I=1,IACT                                                 MAN  760
          AIN(I,IR) = 0.0                                               MAN  770
          AEX(I,IR) = 0.0                                               MAN  780
  170   CONTINUE                                                        MAN  790
  180 CONTINUE                                                          MAN  800
      IF(NRSTRT .GT. 0) GOTO 199                                        MAN  810
      NRESHZ = N200                                                     MAN  820
      JSATZ = 1                                                         MAN  830
      NEXT = JAD11(JSATZ)                                               MAN  840
      DO 190 IR=1,N200                                                  MAN  850
        NXT11 = NEXT + IR - 1                                           MAN  860
        READ (NDA11,REC=NXT11) (AINO(L),L=1,KMAT)                       MAN  870
        NXT11 = NXT11 + 1                                               MAN  880
        VCHG(IR) = VOL(IR)                                              MAN  890
        NALT(IR) = 1                                                    MAN  900
        NTYP1(IR) = 1                                                   MAN  910
        DO 190 M=1,IACT                                                 MAN  920
          AIN(M,IR) = AINO(M)                                           MAN  930
          AEX(M,IR) = DEN(M,IR)                                         MAN  940
  190 CONTINUE                                                          MAN  950
  199 CONTINUE                                                          MAN  960
C                                                                       MAN  970
C     PREPARE DATA FOR COST CALCULATION                                 MAN  980
C                                                                       MAN  990
      CALL KOSTPW(1,A(KA(LTCH1)),NALT,N240,NTYP1,A(KA(LNNEU)),AIN,AEX,  MAN 1000
     1 VCHG,A(KA(LAK)),A(KA(LDK)),A(KA(LFK)),A(KA(LBK)),A(KA(LKQ)),     MAN 1010
     2 A(KA(LEKWH)),A(KA(LET)),A(KA(LORE1)),A(KA(LORE2)),A(KA(LSEP1)),  MAN 1020
     3 A(KA(LSEP2)),A(KA(LUUKO)),A(KA(LU3IN)),A(KA(LU3EX)),A(KA(LU5IN)),MAN 1030
     4 A(KA(LU5EX)),A(KA(LTHIN)),A(KA(LTHEX)),A(KA(LU8IN)),A(KA(LU8EX)),MAN 1040
     5 A(KA(LSM1)),A(KA(LSM2)),A(KA(LU6IN)),A(KA(LU6EX)),A(KA(LPU9I)),  MAN 1050
     6 A(KA(LPU9E)),A(KA(LPU0I)),A(KA(LPU0E)),A(KA(LPU1I)),A(KA(LPU1E)),MAN 1060
     7 A(KA(LPU2I)),A(KA(LPU2E)),DMAT,A(KA(LAWT)),A(KA(LPA3I)),         MAN 1070
     8 A(KA(LPA3E)),A(KA(LU4IN)),A(KA(LU4EX)),A(KA(LNP7I)),A(KA(LNP7E)),MAN 1080
     9 A(KA(LPU8I)),A(KA(LPU8E)),A(KA(LAM1I)),A(KA(LAM1E)),A(KA(LA2MI)),MAN 1090
     X A(KA(LA2ME)),A(KA(LAM2I)),A(KA(LAM2E)),A(KA(LAM3I)),A(KA(LAM3E)),MAN 1100
     Y A(KA(LCM2I)),A(KA(LCM2E)),A(KA(LCM3I)),A(KA(LCM3E)),A(KA(LCM4I)),MAN 1110
     Z A(KA(LCM4E)),IMAT)                                               MAN 1120
C                                                                       MAN 1130
      GD = 0.                                                           MAN 1140
      GOTO 9000                                                         MAN 1150
  200 CONTINUE                                                          MAN 1160
      IPRINT = IPRVS                                                    MAN 1170
      TBURN = XTIM                                                      MAN 1180
      B = XBURN                                                         MAN 1190
      MM = IABS(KK)                                                     MAN 1200
      IF(KK .LT. 0) JMAF = MM                                           MAN 1210
      IF(MM .GT. 1) GOTO 300                                            MAN 1220
      NRCHG = NRESHZ                                                    MAN 1230
      DO 260 I=1,MXTYP                                                  MAN 1240
        IALT(I) = 0                                                     MAN 1250
        INEU(I) = 0                                                     MAN 1260
        NRTP1(I) = 0                                                    MAN 1270
  260 CONTINUE                                                          MAN 1280
      DO 270 IR=1,NRESHZ                                                MAN 1290
        I = NTYP1(IR)                                                   MAN 1300
        IF(I .EQ. 0) GOTO  270                                          MAN 1310
        NRTP1(I) = NRTP1(I) + 1                                         MAN 1320
        INEU(I) = INEU(I) + 1                                           MAN 1330
  270 CONTINUE                                                          MAN 1340
  300 CONTINUE                                                          MAN 1350
      IF(NRSTRT .LE. 0) GOTO 350                                        MAN 1360
C                                                                       MAN 1370
C     DATA FOR IN-CORE BATCHES END OF CYCLE                             MAN 1380
C                                                                       MAN 1390
      DO 191 IR=1,NRESHZ                                                MAN 1400
        JSATZ = 2 + IR                                                  MAN 1410
        NXT11 = JAD11(JSATZ)                                            MAN 1420
        READ (NDA11,REC=NXT11) (AEX(I,IR),I=1,IACT),(AINO(L),L=IACT+1,  MAN 1430
     1   KMAT),PARVOL,XCHG1,NTP1,IKL1                                   MAN 1440
        NXT11 = NXT11 + 1                                               MAN 1450
        VCHG(IR) = PARVOL                                               MAN 1460
  191 CONTINUE                                                          MAN 1470
C                                                                       MAN 1480
C     IN-CORE CHARGES BEGINNING OF CYCLE                                MAN 1490
C                                                                       MAN 1500
      NCX2 = 0                                                          MAN 1510
      JSATZ = 1                                                         MAN 1520
      NEXT = JAD11(JSATZ)                                               MAN 1530
      DO 250 IR=1,NRESHZ                                                MAN 1540
        NCX1 = NCX2 + 1                                                 MAN 1550
        NCX2 = KRESHZ(IR)                                               MAN 1560
        NCX = LRZN(NCX1)                                                MAN 1570
C                                                                       MAN 1580
C     REMEMBER ALL SUBREGIONS OF ONE REFUELLING AREA HAVE THE SAME      MAN 1590
C     (AVERAGED) ATOM DENSITIES                                         MAN 1600
C                                                                       MAN 1610
        NXT11 = NEXT + NCX - 1                                          MAN 1620
        READ (NDA11,REC=NXT11) (AIN(I,IR),I=1,IACT),(AINO(L),L=IACT+1,  MAN 1630
     1   KMAT)                                                          MAN 1640
        NXT11 = NXT11 + 1                                               MAN 1650
  250 CONTINUE                                                          MAN 1660
      IF(KUGL .LE. 0) GOTO 350                                          MAN 1670
      TT = ABS(TSTORE) * 3600. * 24.                                    MAN 1680
      TR = TREPRO * 3600. * 24.                                         MAN 1690
      DO 10 N=1,IACT                                                    MAN 1700
        TTD = TT * DECY(N)                                              MAN 1710
        TTR = TR * DECY(N)                                              MAN 1720
        IF(TTD .LE. 25.) DS(N) = EXP(-TTD)                              MAN 1730
        IF(TTD .GT. 25.) DS(N) = 0.                                     MAN 1740
        IF(TTR .LE. 25.) DR(N) = EXP(-TTR)                              MAN 1750
        IF(TTR .GT. 25.) DR(N) = 0.                                     MAN 1760
   10 CONTINUE                                                          MAN 1770
      IF(MM .GT. 1) GOTO 311                                            MAN 1780
C                                                                       MAN 1790
C     DATA FOR OUT-OF-PILE BATCHES FOR INITIAL CYCLE                    MAN 1800
C                                                                       MAN 1810
      DO 310 N=1,NOPILE                                                 MAN 1820
        IR = NRESHZ + N                                                 MAN 1830
        JSATZ = 2 + MBOX + IR                                           MAN 1840
        NXT11 = JAD11(JSATZ)                                            MAN 1850
        READ (NDA11,REC=NXT11) (AEX(I,IR),I=1,IACT),(AINO(L),L=IACT+1,  MAN 1860
     1   KMAT),PARVOL,XCHG1,NTP1,IKL1                                   MAN 1870
        NXT11 = NXT11 + 1                                               MAN 1880
        IF(IKL1 .NE. 1) GOTO 310                                        MAN 1890
        VCHG(IR) = PARVOL                                               MAN 1900
        DO 305 I=1,IACT                                                 MAN 1910
          AIN(I,IR) = AEX(I,IR)                                         MAN 1920
  305   CONTINUE                                                        MAN 1930
  310 CONTINUE                                                          MAN 1940
      GOTO 315                                                          MAN 1950
C                                                                       MAN 1960
C     DATA FOR OUT-OF-PILE BATCHES CYCLES 2,3 .......N                  MAN 1970
C                                                                       MAN 1980
  311 CONTINUE                                                          MAN 1990
      K = 0                                                             MAN 2000
      KLZ = 0                                                           MAN 2010
      DO 313 N=1,NOPILE                                                 MAN 2020
        IR = NRESHZ + N                                                 MAN 2030
        DO 312 M=1,IACT                                                 MAN 2040
          AEX(M,IR) = AIN(M,IR)                                         MAN 2050
  312   CONTINUE                                                        MAN 2060
        IF(N .LE. KLZ) GOTO 313                                         MAN 2070
C                                                                       MAN 2080
C     DECAY OF OUT-OF-PILE FUEL EXCEPT CLASS=1 (FRESH FUEL)             MAN 2090
C                                                                       MAN 2100
        K = K + 1                                                       MAN 2110
        KLZ = KLZ + KLASSE(K)                                           MAN 2120
        DO 20 M=1,IACT                                                  MAN 2130
          AEX(M,IR) = AIN(M,IR) * DS(M)                                 MAN 2140
          IF(M .NE. 3) GOTO 21                                          MAN 2150
          AEX(M,IR) = (AIN(M,IR)+AIN(M-1,IR)) * DS(M)                   MAN 2160
          AEX(M-1,IR) = 0.                                              MAN 2170
   21     CONTINUE                                                      MAN 2180
          IF(M .NE. 4) GOTO 22                                          MAN 2190
          AEX(M,IR) = (AIN(M,IR)+AIN(M-1,IR)*(1.-DS(M-1))) * DS(M)      MAN 2200
   22     CONTINUE                                                      MAN 2210
          IF(M .NE. 11 .AND. M .NE. 13 .AND. M .NE. 15 .AND. M .NE. 17  MAN 2220
     1     .AND. M .NE. 28) GOTO 23                                     MAN 2230
          AEX(M,IR) = (AIN(M,IR)+AIN(M-3,IR)) * DS(M)                   MAN 2240
          AEX(M-3,IR) = 0.                                              MAN 2250
   23     CONTINUE                                                      MAN 2260
          IF(M .NE. 16 .AND. M .NE. 21) GOTO 24                         MAN 2270
          AEX(M,IR) = (AIN(M,IR)+AIN(M-3,IR)*(1.-DS(M-3))) * DS(M)      MAN 2280
   24     CONTINUE                                                      MAN 2290
          IF(M .NE. 24) GOTO 20                                         MAN 2300
          AEX(M,IR) = (AIN(M,IR)+AIN(M-4,IR)) * DS(M)                   MAN 2310
          AEX(M-4,IR) = 0.                                              MAN 2320
   20   CONTINUE                                                        MAN 2330
  313 CONTINUE                                                          MAN 2340
  315 CONTINUE                                                          MAN 2350
C                                                                       MAN 2360
C     DATA FOR SCRAP BATCHES                                            MAN 2370
C                                                                       MAN 2380
      DO 320 N=1,JTYP                                                   MAN 2390
        IR = NRESHZ + NOPILE + N                                        MAN 2400
        JSATZ = 2 + MBOX + IR                                           MAN 2410
        NXT11 = JAD11(JSATZ)                                            MAN 2420
        READ (NDA11,REC=NXT11) (AIN(I,IR),I=1,IACT),(AINO(L),L=IACT+1,  MAN 2430
     1   KMAT),PARVOL,XCHG1,NTP1,IKL1,BRN1,FDS1,HMG1,HMD1               MAN 2440
        NXT11 = NXT11 + 1                                               MAN 2450
        VCHG(IR) = PARVOL                                               MAN 2460
C                                                                       MAN 2470
C     DECAY OF SCRAP FUEL DURING STORAGE AND REPROCESSING               MAN 2480
C                                                                       MAN 2490
        DO 30 M=1,IACT                                                  MAN 2500
          AEX(M,IR) = AIN(M,IR) * DR(M)                                 MAN 2510
          IF(M .NE. 3) GOTO 31                                          MAN 2520
          AEX(M,IR) = (AIN(M,IR)+AIN(M-1,IR)) * DR(M)                   MAN 2530
          AEX(M-1,IR) = 0.                                              MAN 2540
   31     CONTINUE                                                      MAN 2550
          IF(M .NE. 4) GOTO 32                                          MAN 2560
          AEX(M,IR) = (AIN(M,IR)+AIN(M-1,IR)*(1.-DR(M-1))) * DR(M)      MAN 2570
   32     CONTINUE                                                      MAN 2580
          IF(M .NE. 11 .AND. M .NE. 13 .AND. M .NE. 15 .AND. M .NE. 17  MAN 2590
     1     .AND. M .NE. 28) GOTO 33                                     MAN 2600
          AEX(M,IR) = (AIN(M,IR)+AIN(M-3,IR)) * DR(M)                   MAN 2610
          AEX(M-3,IR) = 0.                                              MAN 2620
   33     CONTINUE                                                      MAN 2630
          IF(M .NE. 16 .AND. M .NE. 21) GOTO 34                         MAN 2640
          AEX(M,IR) = (AIN(M,IR)+AIN(M-3,IR)*(1.-DR(M-3))) * DR(M)      MAN 2650
   34     CONTINUE                                                      MAN 2660
          IF(M .NE. 24) GOTO 30                                         MAN 2670
          AEX(M,IR) = (AIN(M,IR)+AIN(M-4,IR)) * DR(M)                   MAN 2680
          AEX(M-4,IR) = 0.                                              MAN 2690
   30   CONTINUE                                                        MAN 2700
  320 CONTINUE                                                          MAN 2710
  350 CONTINUE                                                          MAN 2720
C                                                                       MAN 2730
C     PERFORM COST CALCULATION                                          MAN 2740
C                                                                       MAN 2750
      CALL KOSTPW(2,A(KA(LTCH1)),NALT,N240,NTYP1,A(KA(LNNEU)),AIN,AEX,  MAN 2760
     1 VCHG,A(KA(LAK)),A(KA(LDK)),A(KA(LFK)),A(KA(LBK)),A(KA(LKQ)),     MAN 2770
     2 A(KA(LEKWH)),A(KA(LET)),A(KA(LORE1)),A(KA(LORE2)),A(KA(LSEP1)),  MAN 2780
     3 A(KA(LSEP2)),A(KA(LUUKO)),A(KA(LU3IN)),A(KA(LU3EX)),A(KA(LU5IN)),MAN 2790
     4 A(KA(LU5EX)),A(KA(LTHIN)),A(KA(LTHEX)),A(KA(LU8IN)),A(KA(LU8EX)),MAN 2800
     5 A(KA(LSM1)),A(KA(LSM2)),A(KA(LU6IN)),A(KA(LU6EX)),A(KA(LPU9I)),  MAN 2810
     6 A(KA(LPU9E)),A(KA(LPU0I)),A(KA(LPU0E)),A(KA(LPU1I)),A(KA(LPU1E)),MAN 2820
     7 A(KA(LPU2I)),A(KA(LPU2E)),DMAT,A(KA(LAWT)),A(KA(LPA3I)),         MAN 2830
     8 A(KA(LPA3E)),A(KA(LU4IN)),A(KA(LU4EX)),A(KA(LNP7I)),A(KA(LNP7E)),MAN 2840
     9 A(KA(LPU8I)),A(KA(LPU8E)),A(KA(LAM1I)),A(KA(LAM1E)),A(KA(LA2MI)),MAN 2850
     X A(KA(LA2ME)),A(KA(LAM2I)),A(KA(LAM2E)),A(KA(LAM3I)),A(KA(LAM3E)),MAN 2860
     Y A(KA(LCM2I)),A(KA(LCM2E)),A(KA(LCM3I)),A(KA(LCM3E)),A(KA(LCM4I)),MAN 2870
     Z A(KA(LCM4E)),IMAT)                                               MAN 2880
C                                                                       MAN 2890
      IF(NRSTRT .LE. 0) GOTO 9000                                       MAN 2900
C                                                                       MAN 2910
C     PREPARING NEXT RESHUFFLE STEP                                     MAN 2920
C                                                                       MAN 2930
      DO 400 I=1,MXTYP                                                  MAN 2940
        IALT(I) = 0                                                     MAN 2950
        INEU(I) = 0                                                     MAN 2960
  400 CONTINUE                                                          MAN 2970
C                                                                       MAN 2980
C     DATA FOR IN-CORE BATCHES START OF CYCLES 2,3 ........N            MAN 2990
C                                                                       MAN 3000
      DO 420 IR=1,NRESHZ                                                MAN 3010
        NCX2 = KRESHZ(IR)                                               MAN 3020
        NCX = LRZN(NCX2)                                                MAN 3030
        IF(TCHG2(IR) .GT. 0.) GOTO 410                                  MAN 3040
        I = NTYP2(IR)                                                   MAN 3050
        INEU(I) = INEU(I) + 1                                           MAN 3060
  410   CONTINUE                                                        MAN 3070
  420 CONTINUE                                                          MAN 3080
      IF(KUGL .LE. 0) GOTO 9000                                         MAN 3090
C                                                                       MAN 3100
C     DATA FOR OUT-OF-PILE BATCHES FOR CYCLES 2,3 .......N              MAN 3110
C                                                                       MAN 3120
      DO 440 N=1,NOPILE                                                 MAN 3130
        IR = NRESHZ + N                                                 MAN 3140
        JSATZ = 2 + MBOX + IR                                           MAN 3150
        NXT11 = JAD11(JSATZ)                                            MAN 3160
        READ (NDA11,REC=NXT11) (AIN(I,IR),I=1,IACT),(AINO(L),L=IACT+1,  MAN 3170
     1   KMAT),PARVOL,XCHG1,NTP1,IKL1,BRN1,FDS1,HMG1,HMD1               MAN 3180
        NXT11 = NXT11 + 1                                               MAN 3190
        VCHG(IR) = PARVOL                                               MAN 3200
  440 CONTINUE                                                          MAN 3210
 9000 CONTINUE                                                          MAN 3220
      RETURN                                                            MAN 3230
      END                                                               MAN 3240
      SUBROUTINE FUMAN(IREG,POISL,PINMIN,PINMAX,NPOIS,NHOT,VOL,DEN,HM,  FUM   10
     1 THBURN,FADOS3,NOPOW,HMETAL,TCHG2,KRESHZ,LRZN,NTYP2,NALT,NTYP1,   FUM   20
     2 N240,NNEU,NRY,DOR,JAD11,IBOX,HMETAV,VERA,IHZ,VOLIHZ,VOLWEG,DAV,  FUM   30
     3 CPX,NPX,ISTOB,NEND8,RVCB,IMAT,FICOMP,IDFISS,SS,NTYSP,TCELS)      FUM   40
C                                                                       FUM   50
CFZJ012   New identification numbers for THERMOS-cell         02.12.03  FUM   60
C         definitions for the spectrum zones                  02.12.03  FUM   70
C                                                                       FUM   80
C     F U M A N   MANAGES FUELLING OPERATIONS                           FUM   90
C                                                                       FUM  100
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    FUM  110
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    FUM  120
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIFUM  130
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D,NLAYP       FUM  140
C                                                                       FUM  150
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1FUM  160
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         FUM  170
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    FUM  180
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)FUM  190
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  FUM  200
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    FUM  210
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         FUM  220
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,FUM  230
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            FUM  240
C                                                                       FUM  250
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, FUM  260
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        FUM  270
     2 FIMAKL(20),NOPILE,MREP,MARX(10),NAJB(10),FOJB(10),NFUL(10),NBTOT,FUM  280
     3 NB0,NCY                                                          FUM  290
C                                                                       FUM  300
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), FUM  310
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10FUM  320
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11FUM  330
     3 ,L13,L28,L29,L30,L40,IWRITE,IREA,JSUM13                          FUM  340
C                                                                       FUM  350
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300),FABC(10),REPC(10),NNEFUM  360
     1 ,LISTEQ,NTIK                                                     FUM  370
C                                                                       FUM  380
CFZJ062                                                      04.05.11   FUM  390
      COMMON /ORIGEN/ LOB,NOR,VOR(100),ISPK,KFISS,N200C,NXSC,KGR,       FUM  400
     1 LOBN,IEZ                                                         FUM  410
C                                                                       FUM  420
      COMMON /NUCNAM/ T1(200),T2(200),GR(2),PU,FL,MATNR(200)            FUM  430
C                                                                       FUM  440
      COMMON /BLOCKK/ D(3),AVO                                          FUM  450
C                                                                       FUM  460
      COMMON /VARDIM/ A(8000000)                                        FUM  470
C                                                                       FUM  480
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       FUM  490
C                                                                       FUM  500
CFZJ055                                                       25.09.07  FUM  510
C                                                                       FUM  520
      COMMON /X/ IV29                                                   FUM  530
C                                                                       FUM  540
      COMMON /FLUXN/ DUMM(361),IACT                                     FUM  550
C                                                                       FUM  560
      COMMON /IPO/ IPRINO,INAK,LNAK,JNT                                 FUM  570
C                                                                       FUM  580
      COMMON /BUC/ BU(6,200)                                            FUM  590
C                                                                       FUM  600
      COMMON /CVX/ VX                                                   FUM  610
C                                                                       FUM  620
      CHARACTER*4 T1,T2,GR,PU,FL,BU                                     FUM  630
C                                                                       FUM  640
CFZJ012                                                       02.12.03  FUM  650
CFZJ062                                                       04.05.11  FUM  660
      DIMENSION VX(1000),IX(9),RX(3),VR(1000),IY(9),RY(3),IBAE(999),    FUM  670
     1 INEW(12),DNEW(12),ITOP(30),ITOC(30,5),DEPTH(30),FIB(999),IBA(30),FUM  680
     2 FIO(30),IREG(NDR),POISL(2,NDR),PINMIN(NDR),PINMAX(NDR),NPOIS(NDR)FUM  690
     3 ,NHOT(N200),VOL(N200),DEN(KMAT,N200),HM(N200),THBURN(N200),      FUM  700
     4 FADOS3(N200),NOPOW(N200),HMETAL(N200),TCHG2(N200),KRESHZ(N200),  FUM  710
     5 LRZN(N200),NTYP2(N200),NALT(N240),NTYP1(N240),NNEU(N200),        FUM  720
     6 NRY(N200),DOR(100,KMAT),HMETAV(N200),VERA(N200),IHZ(NDR),        FUM  730
     7 VOLIHZ(NDR),VOLWEG(NDR),DAV(KMAT),CPX(KMAT),NPX(KMAT),IBOX(MBOX),FUM  740
     8 ISTOB(MBOX),JAD11(JD11),RVCB(MBOX),IMAT(KMAT),FICOMP(IACT),      FUM  750
     9 IDFISS(IACT),SS(N26,N200),NTYSP(NXS),TCELS(5,NXS)                FUM  760
C                                                                       FUM  770
      EQUIVALENCE(JTPE2,NS),(JTPE3,NT),(TXME(1),TBURN),                 FUM  780
     1 (LI(9),LVOLR),(LI(37),LDENI),(LI(55),LXREP),(LI(59),LTCH1),      FUM  790
     2 (LI(64),LTEMZ),(LI(65),LTCEL),(LI(91),LSGA),(LI(92),LSGTR),      FUM  800
     3 (LI(93),LV1),(LI(94),LFKEN),(LI(151),LAWT)                       FUM  810
C                                                                       FUM  820
   10 FORMAT (/' *** ORIGEN-LIBRARY ALLOWS NO MORE THAN 50 BATCHES OF FRFUM  830
     1ESH FUEL ***')                                                    FUM  840
  114 FORMAT (/' FOR BATCH',I5,' THE PROVIDED VOLUME HAS BEEN MODIFIED FFUM  850
     1ROM',E12.5,' TO',E12.5,' RESPECTING THE PROVIDED ENRICH-'/' MENT OFUM  860
     2F',E12.5)                                                         FUM  870
  115 FORMAT (6E12.5)                                                   FUM  880
  118 FORMAT (//' FOR BATCH',I5,' THE VOLUME FRACTION OF THE PROVIDED OUFUM  890
     1T OF PILE FUEL IS CHANGED FROM',E12.5,' TO',E12.5)                FUM  900
  122 FORMAT (//' ***** V E R Y  I N T E L L I G E N T    (THE PROGRAM) FUM  910
     1*****'/' OUT OF PILE BOX VOLUME WAS TOO SMALL. THEREFORE THE VOLUMFUM  920
     2E OF THIS BATCH POSITION HAS BEEN REDUCED.')                      FUM  930
  207 FORMAT (//' FOR BATCH',I5,' THE OUT OF PILE FUEL VOLUME FRACTION IFUM  940
     1S MODIFIED TO',E12.5)                                             FUM  950
  214 FORMAT (/' BATCH',I5,' HAS BEEN FILLED WITH A NEW FORMED FUEL TYPEFUM  960
     1',I3,', VOLUME =',E12.5,', HEAVY METAL =',E12.5,' GRAM.')         FUM  970
  218 FORMAT (I12,3E12.5)                                               FUM  980
  400 FORMAT (' BATCH',I5,14H IS RECHARGED )                            FUM  990
  401 FORMAT (I5,16X,I4,5X,I2,27X,E12.5)                                FUM 1000
  402 FORMAT (' BATCH',I5,16H IS NOT ALTERED ,32X,E12.5)                FUM 1010
  403 FORMAT (/' BATCH',I5,' NOW CONTAINES THE AVERAGE COMPOSITION'//' MFUM 1020
     1ATERIAL',15X,'CONCENTRATION')                                     FUM 1030
  404 FORMAT ('+',58X,E12.5)                                            FUM 1040
  503 FORMAT (2I5,I2,6I4,3E12.5)                                        FUM 1050
CFZJ059                                                       04.11.09  FUM 1060
  512 FORMAT (1H ,4A4,8X,E12.5)                                         FUM 1070
  515 FORMAT (/' THE DENSITY OF THE FOLLOWING MATERIAL IN BATCH',I5,' WAFUM 1080
     1S ADJUSTED TO MAKE THE "FISSILE/HEAVY-METAL"-RATIO =',F7.5/' MATERFUM 1090
     2IAL',18X,'COMPOSITION')                                           FUM 1100
CFZJ059                                                       04.11.09  FUM 1110
  516 FORMAT (' ',4A4,8X,' WAS USED AS "MAKE-UP"-ISOTOPE TO MAKE THE HEAFUM 1120
     1VY-METAL DENSITY'/' EQUAL TO THE ORIGINAL HM-DENSITY IN THIS BATCHFUM 1130
     2'/)                                                               FUM 1140
  520 FORMAT (4(I6,E12.5))                                              FUM 1150
  550 FORMAT ('1','BATCHES AFTER RELOAD NO ',I4,56X,'INPUT DATA',I4//' BFUM 1160
     1ATCH   CAME FROM   BATCH   TYPE',25X,'NEW VOLUME (CCM)'/)         FUM 1170
  565 FORMAT ('1','*** RELOAD FUEL FOR BATCH ',I4,' SHOULD BE FUEL TYPE'FUM 1180
     1 ,I4/' *** BUT FROM DISC RECORD NO',I5,' HAS BEEN READ CONTAINING FUM 1190
     2DATA FOR TYPE',I4)                                                FUM 1200
  570 FORMAT ('0 ATTENTION PLEASE. BATCH',I5,' BELONGING TO REGION',I5,'FUM 1210
     1 IS NOT SITUATED IN THE UPPER REGION .....')                      FUM 1220
  575 FORMAT (' *** FUEL TYPE ',I4,' IS NOT AVAILABLE FOR RELOADING.....FUM 1230
     1.. TYPE ',I4,' WILL BE LOADED ...')                               FUM 1240
  580 FORMAT ('0.......... ATTENTION PLEASE ........'/' ALL THE FUEL OF FUM 1250
     1TYPE ',I2,' HAS BEEN USED. ORIGINAL VOLUME ON STORE =',E12.5,' REFFUM 1260
     2UELLED VOLUME =',E12.5/' RELOADING WILL CONTINUE AS REQUESTED AND FUM 1270
     3REGARD STORAGE AS UNLIMITED .....')                               FUM 1280
  599 FORMAT ('1')                                                      FUM 1290
  601 FORMAT (' BATCH',I5,' GETS THE ATOM CONCENTRATIONS OF BATCH',I5,' FUM 1300
     1TYPE..',I2,' AFTER REPROCESSING')                                 FUM 1310
  602 FORMAT (' BATCH',I5,' AFTER REPROCESSING')                        FUM 1320
  603 FORMAT (18I4)                                                     FUM 1330
  604 FORMAT (12I6)                                                     FUM 1340
  700 FORMAT (//' ***PROGRAM STOPS BECAUSE YOU RETRIEVE MATERIALS FROM SFUM 1350
     1TORAGE BOX NO.',I4,', BUT THE DIMENSION ONLY ALLOWS',I4,'.'/'    CFUM 1360
     2HECK THE INPUT OF CARD V1 (MSTOB) IN CONNECTION WITH CARDS R21 (NRFUM 1370
     3X). ***')                                                         FUM 1380
  710 FORMAT (//' ***PROGRAM STOPS BECAUSE THE MAXIMUM NUMBER OF BATCHESFUM 1390
     1 TO BE FILLED IN STORAGE BOXES IS TOO SMALL.'/'    CHECK THE INPUTFUM 1400
     2 OF CARD V1 (MBATCH) IN CONNECTION WITH CARDS R21. ***')          FUM 1410
  720 FORMAT (I5,16X,I4,5X,I2,4X,'(STO.-BOX',I3,')',10X,E12.5)          FUM 1420
  730 FORMAT (//' ***FOR BATCH',I5,' THE FM-SCHEME RETRIEVES MATERIALS FFUM 1430
     1ROM STORAGE BOX',I4,' BUT THIS BOX WAS NOT FILLED UP BEFORE.'/'   FUM 1440
     2 PROGRAM STOPS BECAUSE YOU CAN NOT DIP INTO THE POCKET OF A NAKED FUM 1450
     3MAN. ***')                                                        FUM 1460
  900 FORMAT (' FOLLOWING NUCLIDES ARE CHANGED IN THEIR ATOM DENSITIES FFUM 1470
     1OR THE NEXT CYCLE (IN THE WHOLE CORE):'//3(4(I6,E12.5)/)/)        FUM 1480
  901 FORMAT (' IN FOLLOWING INDIVIDUAL BATCHES CHANGES IN THE ATOM DENSFUM 1490
     1ITIES ARE PERFORMED FOR THE NEXT CYCLE:'//3(16I6/)/)              FUM 1500
  902 FORMAT (//' NUCLIDE./ATOM DENSITIES:'//3(4(I6,E12.5)/)/)          FUM 1510
 2000 FORMAT (2E12.5,3I6)                                               FUM 1520
 2010 FORMAT (//' WATER INGRESS TO ACTIVE CORE, PARTIAL STEAM PRESSURE IFUM 1530
     1NCREASED BY',F6.1,' BAR (VSOP-MAT-NO:',2I4,')'//)                 FUM 1540
 2020 FORMAT (' WATER INGRESS TO REFL.-REG.',I5,', PARTIAL STEAM PRESSURFUM 1550
     1E INCREASED BY',F6.1,' BAR (VSOP-MAT-NO:',2I4,'), T=',F6.0,'°C')  FUM 1560
 2222 FORMAT (/1X,F4.0,'SET ON UNIT 60',15X,' CYCLE',I4,' ITEST = ',I2) FUM 1570
C                                                                       FUM 1580
C                                                                       FUM 1590
C     READ RESHUFFLE DATA                                               FUM 1600
C                                                                       FUM 1610
      DO 78 I=1,999                                                     FUM 1620
        FIB(I) = 1.                                                     FUM 1630
   78 CONTINUE                                                          FUM 1640
      IFI = 1                                                           FUM 1650
      NP9 = NEND8                                                       FUM 1660
      NP10 = NP9 + N200                                                 FUM 1670
      NEND10 = NP10 + N200                                              FUM 1680
C                                                                       FUM 1690
      IF(IPRIN(15) .LE. 0) CALL FUREAD(A(KA(LXREP)),KRESHZ,LRZN,NTYP1,  FUM 1700
     1 N240,DAV,A(NP9),A(NP10),JAD11,A(KA(LAWT)))                       FUM 1710
C                                                                       FUM 1720
C     BEFORE RESHUFFLE                                                  FUM 1730
C                                                                       FUM 1740
      KLO = NOPILE + JTYP                                               FUM 1750
      DO 40 M=1,1000                                                    FUM 1760
        VX(M) = 0.0                                                     FUM 1770
        VR(M) = 0.                                                      FUM 1780
   40 CONTINUE                                                          FUM 1790
      DO 42 M=1,MBOX                                                    FUM 1800
        ISTOB(M) = 0                                                    FUM 1810
   42 CONTINUE                                                          FUM 1820
      FISNO = FIWATT * POWER * TBURN * 86400. * ZMNEW                   FUM 1830
      DELDAX = DELDAY                                                   FUM 1840
      NP11 = NEND10                                                     FUM 1850
      NP12 = NP11 + KMAT                                                FUM 1860
      NP13 = NP12 + NXS                                                 FUM 1870
      NEND13 = NP13 + NXS                                               FUM 1880
C                                                                       FUM 1890
CFZJ012                                                       02.12.03  FUM 1900
      CALL VORSHU(NPRINT,IN1,MRY,NKEEP,DECAF,ICONPO,A(KA(LVOLR)),VOL,DENFUM 1910
     1 ,A(KA(LDENI)),HM,THBURN,FADOS3,NOPOW,HMETAL,TCHG2,KRESHZ,LRZN,   FUM 1920
     2 A(KA(LTCH1)),N240,NALT,NTYP1,A(KA(LTEMZ)),TCELS,VERA,IHZ,        FUM 1930
     3 VOLIHZ,VOLWEG,HMETAV,DAV,A(NP11),A(NP12),A(NP13),IREG,JAD11,IBOX,FUM 1940
     4 RVCB,HPOS,NTYSP,IWATER)                                          FUM 1950
C                                                                       FUM 1960
      IVSP29 = IVSP(29)                                                 FUM 1970
      IAE = IFIX(HPOS)                                                  FUM 1980
      IF(IVSP29 .LE. 0) GOTO 50                                         FUM 1990
C                                                                       FUM 2000
CARD R19A                                                               FUM 2010
C                                                                       FUM 2020
      READ (NS,520) (INEW(I),DNEW(I),I=1,IVSP29)                        FUM 2030
C                                                                       FUM 2040
      IV29 = IVSP29                                                     FUM 2050
   50 CONTINUE                                                          FUM 2060
      IF(IVSP(29) .LE. 0) GOTO 51                                       FUM 2070
C                                                                       FUM 2080
CARD R19B                                                               FUM 2090
C                                                                       FUM 2100
      IF(IAE .GT. 0) READ (NS,604) (IBAE(I),I=1,IAE)                    FUM 2110
C                                                                       FUM 2120
      IF(IAE .LE. 0) WRITE (6,900) (INEW(I),DNEW(I),I=1,IVSP29)         FUM 2130
      IF(IAE .GT. 0) WRITE (6,901) (IBAE(I),I=1,IAE)                    FUM 2140
      IF(IAE .GT. 0) WRITE (6,902) (INEW(I),DNEW(I),I=1,IVSP29)         FUM 2150
   51 CONTINUE                                                          FUM 2160
C                                                                       FUM 2170
C     AFTER RESHUFFLE                                                   FUM 2180
C                                                                       FUM 2190
      IREAD = 1                                                         FUM 2200
      NCX2 = 0                                                          FUM 2210
      NNE = 0                                                           FUM 2220
      NOR = 0                                                           FUM 2230
      NN = NS                                                           FUM 2240
      LOBNEW = NKEEP / 100                                              FUM 2250
      IF(LOBNEW .GT. 0) LOBN = LOBNEW                                   FUM 2260
      NORSH = NKEEP * 0.1 - LOBNEW * 10                                 FUM 2270
      IF(NORSH .EQ. 2) MUHU(24) = 60                                    FUM 2280
      N60 = MUHU(24)                                                    FUM 2290
      NKEEP = NKEEP - LOBNEW * 100 - NORSH * 10                         FUM 2300
      NWRITE = NKEEP                                                    FUM 2310
      IF(NKEEP .EQ. 0) NN = NDA13                                       FUM 2320
      IF(NKEEP .EQ. 2) IREAD = 0                                        FUM 2330
      IF(NPRINT .GE. 1) WRITE (NT,550) IPRIN(15)+1,NN                   FUM 2340
      KR = 1                                                            FUM 2350
      NXT13 = 1                                                         FUM 2360
      IZWBO = 0                                                         FUM 2370
      DO 200 IR=1,NRESHZ                                                FUM 2380
        WERA = VERA(IR)                                                 FUM 2390
        IWRA = 0                                                        FUM 2400
        JXTYPE = 0                                                      FUM 2410
        XMETAV = 0.                                                     FUM 2420
        NEWTYP = 0                                                      FUM 2430
        NREPTY = 0                                                      FUM 2440
        NMAKP = 0                                                       FUM 2450
        NREMI = 0                                                       FUM 2460
        ISTB = 0                                                        FUM 2470
        NSB = 0                                                         FUM 2480
        NCX1 = NCX2 + 1                                                 FUM 2490
        NCX2 = KRESHZ(IR)                                               FUM 2500
        IF(IR .GT. 1 .AND. NPRINT .EQ. 2) WRITE (NT,599)                FUM 2510
        IX(1) = 0                                                       FUM 2520
        IF(IR .GT. 1 .AND. NKEEP .EQ. 5) GOTO 160                       FUM 2530
        DO 141 I=2,9                                                    FUM 2540
          IX(I) = 0                                                     FUM 2550
          IF(I .LE. 3) RX(I) = 0.                                       FUM 2560
  141   CONTINUE                                                        FUM 2570
  160   CONTINUE                                                        FUM 2580
        IVOL = 0                                                        FUM 2590
        IHOT = 0                                                        FUM 2600
        IY(9) = 0                                                       FUM 2610
        MANAGE = 0                                                      FUM 2620
        IF(IREAD .LE. 0) GOTO 301                                       FUM 2630
C                                                                       FUM 2640
CARD R21                                                                FUM 2650
C                                                                       FUM 2660
        IF(NN .EQ. NS) READ (NN,503) (IY(I),I=1,3),NREP,NSPALT,MAKEUP,  FUM 2670
     1   MANAGE,(IY(I),I=7,8),(RY(I),I=1,3)                             FUM 2680
C                                                                       FUM 2690
        IFU = 1                                                         FUM 2700
        IF(MANAGE .EQ. 0 .AND. IY(7) .GT. 0) IY(8) = 1                  FUM 2710
        IF(NN .NE. NDA13) GOTO 301                                      FUM 2720
        READ (NDA13,REC=NXT13) (IY(I),I=1,3),NREP,NSPALT,MAKEUP,MANAGE, FUM 2730
     1   (IY(I),I=7,8),(RY(I),I=1,3)                                    FUM 2740
        NXT13 = NXT13 + 1                                               FUM 2750
  301   CONTINUE                                                        FUM 2760
        IX1 = IABS(IY(1))                                               FUM 2770
        IF(NKEEP .LE. 1) GOTO 144                                       FUM 2780
        IF(NKEEP .GT. 2 .AND. IX1 .EQ. IR) GOTO 144                     FUM 2790
        NIX = 0                                                         FUM 2800
        IF(NKEEP .NE. 3) GOTO 140                                       FUM 2810
        IF(NOPOW(IR) .GT. 0) GOTO 140                                   FUM 2820
        IF(IR .GT. IREG(KR)) KR = KR + 1                                FUM 2830
        NIX = IHZ(KR)                                                   FUM 2840
  140   CONTINUE                                                        FUM 2850
        IF(NKEEP .EQ. 6 .AND. NOPOW(IR) .EQ. 0) NIX = 1                 FUM 2860
        IF(NKEEP .EQ. 5 .AND. NOPOW(IR) .EQ. 0) GOTO 161                FUM 2870
        IX(2) = IR - NIX * NLAYP                                        FUM 2880
  161   CONTINUE                                                        FUM 2890
        IREAD = 0                                                       FUM 2900
        GOTO 149                                                        FUM 2910
  144   CONTINUE                                                        FUM 2920
        IREAD = 1                                                       FUM 2930
        IF(IY(1) .GE. 0) GOTO 142                                       FUM 2940
        IREAD = 0                                                       FUM 2950
        IY(1) = -IY(1)                                                  FUM 2960
  142   CONTINUE                                                        FUM 2970
        DO 143 I=1,9                                                    FUM 2980
          IX(I) = IY(I)                                                 FUM 2990
          IF(I .LE. 3) RX(I) = RY(I)                                    FUM 3000
  143   CONTINUE                                                        FUM 3010
  149   CONTINUE                                                        FUM 3020
C                                                                       FUM 3030
        IF(MANAGE .GT. 0) CALL FULOAD(MANAGE,IX,RX)                     FUM 3040
C                                                                       FUM 3050
        NRX = IX(2)                                                     FUM 3060
        IF(NRX .LE. 10000) GOTO 150                                     FUM 3070
        ISTB = 1                                                        FUM 3080
        NRX = NRX - 10000 + NRESHZ + NBOX                               FUM 3090
        IF(NRX .LE. (MBOX+NRESHZ)) GOTO 150                             FUM 3100
        NRX = NRX - NRESHZ - NBOX                                       FUM 3110
        MSTOB = MBOX - NBOX                                             FUM 3120
        WRITE (6,700) NRX,MSTOB                                         FUM 3130
        STOP                                                            FUM 3140
  150   CONTINUE                                                        FUM 3150
        IF(IX(3) .LE. 0 .OR. NKEEP .EQ. 2) GOTO 151                     FUM 3160
        IZWBO = IZWBO + 1                                               FUM 3170
        NSB = IZWBO                                                     FUM 3180
        IBOX(IZWBO) = IX(3) + NBOX                                      FUM 3190
        IF(IZWBO .EQ. 1) PRO(299) = FLOAT(IPRIN(15))                    FUM 3200
  151   CONTINUE                                                        FUM 3210
        IX(4) = NREP                                                    FUM 3220
        IX(5) = MAKEUP                                                  FUM 3230
        IX(6) = NSPALT                                                  FUM 3240
        IXTYPE = IX(7)                                                  FUM 3250
        IXCLSS = IX(8)                                                  FUM 3260
        XSPALT = ABS(RX(1))                                             FUM 3270
        XVOL = RX(2)                                                    FUM 3280
        XVOLIN = 1.                                                     FUM 3290
        IF(NRX .GT. 0 .AND. NRX .LE. NRESHZ .AND. XVOL .GT. 0.) XVOLIN =FUM 3300
     1   XVOL                                                           FUM 3310
        IF(RX(1) .LT. 0.)  NSPALT = -1                                  FUM 3320
        IF(RX(3) .GE. 0.) GOTO 198                                      FUM 3330
        NEWTYP = IXCLSS                                                 FUM 3340
        DO 196 I=1,NDR                                                  FUM 3350
          KR = I                                                        FUM 3360
          IF(IREG(I) .GE. IR) GOTO 197                                  FUM 3370
  196   CONTINUE                                                        FUM 3380
  197   CONTINUE                                                        FUM 3390
        IWRA = 1                                                        FUM 3400
        WERA = ABS(RX(3)) * VOLIHZ(KR)                                  FUM 3410
  198   CONTINUE                                                        FUM 3420
        IF(IXCLSS .GE. 0) GOTO 211                                      FUM 3430
        NREPTY = IXCLSS                                                 FUM 3440
        IXTYPE = IABS(IXCLSS)                                           FUM 3450
        IXCLSS = 1                                                      FUM 3460
        GOTO 211                                                        FUM 3470
  209   CONTINUE                                                        FUM 3480
        NRP11 = NXT11                                                   FUM 3490
        NREPTY = IABS(NREPTY)                                           FUM 3500
        IXCLSS = 0                                                      FUM 3510
        IXTYPE = IX(7)                                                  FUM 3520
  211   CONTINUE                                                        FUM 3530
        IF(NRX .EQ. 0 .AND. XVOL .LT. 0.) IVOL = 1                      FUM 3540
        XVOL = ABS(XVOL)                                                FUM 3550
        IF(RX(3) .GE. 0.) IHOT = IFIX(RX(3))                            FUM 3560
        IF(IHOT .EQ. 0) GOTO 320                                        FUM 3570
        DO 310 IR1=NCX1,NCX2                                            FUM 3580
          NCX = LRZN(IR1)                                               FUM 3590
          NHOT(NCX) = IHOT                                              FUM 3600
  310   CONTINUE                                                        FUM 3610
  320   CONTINUE                                                        FUM 3620
        IF(NREPTY .GT. 0) GOTO 212                                      FUM 3630
        IF(NWRITE .LE. 0) GOTO 212                                      FUM 3640
        WRITE (NDA13,REC=NXT13) (IX(I),I=1,3),NREP,NSPALT,MAKEUP,MANAGE,FUM 3650
     1   (IX(I),I=7,8),(RX(I),I=1,3)                                    FUM 3660
        NXT13 = NXT13 + 1                                               FUM 3670
  212   CONTINUE                                                        FUM 3680
        IF(NSB .LE. 0) GOTO 220                                         FUM 3690
C                                                                       FUM 3700
C     STORE COMPOSITION OF RESHUFFLE AREA IR IN INTERMEDIATE BOX NSB    FUM 3710
C                                                                       FUM 3720
        ISTOB(NSB) = IR                                                 FUM 3730
        JSATZ = 2 + IR                                                  FUM 3740
        NXT11 = JAD11(JSATZ)                                            FUM 3750
        READ (NDA11,REC=NXT11) (DAV(L),L=1,KMAT),PARVOL,XCHNRX,NTPNRX,  FUM 3760
     1   IKLNRX,BURNRX,DOSNRX,HMGRM,HMDEN                               FUM 3770
        NXT11 = NXT11 + 1                                               FUM 3780
        NALT(IR) = 0                                                    FUM 3790
        JSATZ = 2 + NRESHZ + NSB                                        FUM 3800
        IF(JAD11(JSATZ) .EQ. 0) JAD11(JSATZ) = JSUM11                   FUM 3810
        NXT11 = JAD11(JSATZ)                                            FUM 3820
        WRITE (NDA11,REC=NXT11) (DAV(L),L=1,KMAT),PARVOL,XCHNRX,NTPNRX, FUM 3830
     1   IKLNRX,BURNRX,DOSNRX,HMGRM,HMDEN                               FUM 3840
        NXT11 = NXT11 + 1                                               FUM 3850
        IF(JSUM11 .LT. NXT11) JSUM11 = NXT11                            FUM 3860
  220   CONTINUE                                                        FUM 3870
        XTYPE = IXTYPE                                                  FUM 3880
        VCORR = 1.0                                                     FUM 3890
        IF(NREPTY .LT. 0) GOTO 230                                      FUM 3900
        IF(NRX) 225,230,235                                             FUM 3910
C                                                                       FUM 3920
C     READ NEW MATERIALS FROM INPUT FOR THIS RELOAD AREA                FUM 3930
C                                                                       FUM 3940
  225   CONTINUE                                                        FUM 3950
        NRX = -NRX                                                      FUM 3960
C                                                                       FUM 3970
CARD R22                                                                FUM 3980
C                                                                       FUM 3990
        IF(NN .EQ. NS) READ (NN,520) (NPX(M),CPX(M),M=1,NRX)            FUM 4000
C                                                                       FUM 4010
        IF(NN .EQ. NDA13) CALL WRDA(IREA,NDA13,NXT13,L13,NPX,NRX)       FUM 4020
C                                                                       FUM 4030
        IF(NN .EQ. NDA13) CALL WRDA(IREA,NDA13,NXT13,L13,CPX,NRX)       FUM 4040
C                                                                       FUM 4050
        IF(NWRITE .LE. 0) GOTO 215                                      FUM 4060
C                                                                       FUM 4070
        CALL WRDA(IWRITE,NDA13,NXT13,L13,NPX,NRX)                       FUM 4080
C                                                                       FUM 4090
        CALL WRDA(IWRITE,NDA13,NXT13,L13,CPX,NRX)                       FUM 4100
C                                                                       FUM 4110
  215   CONTINUE                                                        FUM 4120
        DO 201 IR1=NCX1,NCX2                                            FUM 4130
          NCX = LRZN(IR1)                                               FUM 4140
          DO 202 M=1,KMAT                                               FUM 4150
            DEN(M,NCX) = 0.0                                            FUM 4160
  202     CONTINUE                                                      FUM 4170
          DO 203 M=1,NRX                                                FUM 4180
            NCXXX = NPX(M)                                              FUM 4190
            DEN(NCXXX,NCX) = CPX(M)                                     FUM 4200
  203     CONTINUE                                                      FUM 4210
  201   CONTINUE                                                        FUM 4220
        TCHG2(IR) = 0.0                                                 FUM 4230
        NTYP2(IR) = NTYP1(IR)                                           FUM 4240
        IF(XTYPE .NE. 0.) NTYP2(IR) = XTYPE                             FUM 4250
        IF(NPRINT .GT. 0) WRITE (NT,400) IR                             FUM 4260
        GOTO 210                                                        FUM 4270
C                                                                       FUM 4280
C     TAKE MATERIALS FROM OUT-OF-PILE CHARGE FOR THIS AREA              FUM 4290
C                                                                       FUM 4300
  230   CONTINUE                                                        FUM 4310
        IF(IXCLSS .GT. 0) GOTO 231                                      FUM 4320
        IF(IXTYPE .LT. 0) GOTO 234                                      FUM 4330
        NRSATZ = NOPILE + IXTYPE                                        FUM 4340
        JSATZ = NRESHZ + 2 + MBOX + NRSATZ                              FUM 4350
        GOTO 236                                                        FUM 4360
  231   CONTINUE                                                        FUM 4370
        NRSATZ = 0                                                      FUM 4380
        NTP = IXTYPE - 1                                                FUM 4390
        IF(NTP .LE. 0) GOTO 233                                         FUM 4400
        DO 232 N=1,NTP                                                  FUM 4410
          KLZ = KLASSE(N)                                               FUM 4420
          NRSATZ = NRSATZ + KLZ                                         FUM 4430
  232   CONTINUE                                                        FUM 4440
  233   CONTINUE                                                        FUM 4450
        NRSATZ = NRSATZ + IXCLSS                                        FUM 4460
        JSATZ = NRESHZ + 2 + MBOX + NRSATZ                              FUM 4470
        GOTO 236                                                        FUM 4480
  234   CONTINUE                                                        FUM 4490
        NRSATZ = IABS(IXTYPE)                                           FUM 4500
        JSATZ = NRESHZ + 2 + MBOX + NOPILE + JTYP + NRSATZ              FUM 4510
        NREMI = NRSATZ                                                  FUM 4520
        GOTO 236                                                        FUM 4530
C                                                                       FUM 4540
C     SHUFFLE RELOAD AREA NRX TO THIS RELOAD AREA IR, ... OR            FUM 4550
C     RETRIEVE MATERIALS FROM STORAGE BOX  (I.E. NRX > NRESHZ )         FUM 4560
C                                                                       FUM 4570
  235   CONTINUE                                                        FUM 4580
        JSATZ = 2 + NRX                                                 FUM 4590
  236   CONTINUE                                                        FUM 4600
        MRSATZ = NRSATZ                                                 FUM 4610
        IF(NRX .GT. NRESHZ) MRSATZ = KLO + MREP + NRX - NRESHZ          FUM 4620
        IF(NREMI .GT. 0) MRSATZ = KLO + NREMI                           FUM 4630
        NXT11 = JAD11(JSATZ)                                            FUM 4640
        IF(NREPTY .LT. 0) GOTO 209                                      FUM 4650
        READ (NDA11,REC=NXT11) (DAV(L),L=1,KMAT),PARVOL,XCHNRX,NTPNRX,  FUM 4660
     1   IKLNRX,BURNRX,DOSNRX,HMGRM,HMDEN                               FUM 4670
        NXT11 = NXT11 + 1                                               FUM 4680
C                                                                       FUM 4690
C     USE MODIFIED VOLUME OF A STORAGE BOX                              FUM 4700
C                                                                       FUM 4710
        IF(NRX .LE. NRESHZ) GOTO 237                                    FUM 4720
        IF(PARVOL .GT. 0.) GOTO 238                                     FUM 4730
        MSTOB = NRX - NRESHZ - NBOX                                     FUM 4740
        WRITE (6,730) IR,MSTOB                                          FUM 4750
        STOP                                                            FUM 4760
  238   CONTINUE                                                        FUM 4770
        IF(XVOL .LE. 0.) XVOL = 1.                                      FUM 4780
  237   CONTINUE                                                        FUM 4790
        MHM = 0                                                         FUM 4800
C                                                                       FUM 4810
C     IS THIS A CORE CHARGE AND WILL IT NOT BE REPROCESSED              FUM 4820
C                                                                       FUM 4830
        IF(NRX .LE. NRESHZ .AND. NRX .GT. 0 .AND. NREP .EQ. 0) NALT(NRX)FUM 4840
     1   = 0                                                            FUM 4850
        IF(KUGL .LE. 0 .OR. NRX .GT. 0) GOTO 125                        FUM 4860
        IF(NTPNRX .NE. IXTYPE .OR. IKLNRX .NE. IXCLSS) WRITE (NT,565) IRFUM 4870
     1   ,IXTYPE,JSATZ,NTPNRX                                           FUM 4880
        IF(PARVOL .GE. 1.0) GOTO 125                                    FUM 4890
        IXCLSS = 1                                                      FUM 4900
        WRITE (NT,575) NTPNRX,IXTYPE                                    FUM 4910
        GOTO 231                                                        FUM 4920
  125   CONTINUE                                                        FUM 4930
        TCHG2(IR) = XCHNRX                                              FUM 4940
        NTYP2(IR) = IABS(NTPNRX)                                        FUM 4950
        IF(NREPTY .GT. 0) NTYP2(IR) = NREPTY                            FUM 4960
        IF(IVOL .GT. 0) XVOL = XVOL * (PARVOL-VX(MRSATZ)) / PARVOL      FUM 4970
        IF(IVOL .GT. 0) WRITE (NT,207) IR,XVOL                          FUM 4980
        IF(NSPALT .EQ. 0 .AND. NEWTYP .GE. 0) GOTO 112                  FUM 4990
        IF(NSPALT .LE. 0) GOTO 131                                      FUM 5000
C                                                                       FUM 5010
CARD R23                                                                FUM 5020
C                                                                       FUM 5030
        IF(NN .EQ. NS) READ (NN,520) (IDFISS(M),FICOMP(M),M=1,NSPALT),  FUM 5040
     1   JXTYPE,XMETAV                                                  FUM 5050
C                                                                       FUM 5060
        IF(NN .NE. NDA13) GOTO 131                                      FUM 5070
        READ (NDA13,REC=NXT13) JXTYPE,XMETAV                            FUM 5080
        NXT13 = NXT13 + 1                                               FUM 5090
C                                                                       FUM 5100
        CALL WRDA(IREA,NDA13,NXT13,L13,IDFISS,NSPALT)                   FUM 5110
C                                                                       FUM 5120
        CALL WRDA(IREA,NDA13,NXT13,L13,FICOMP,NSPALT)                   FUM 5130
C                                                                       FUM 5140
  131   CONTINUE                                                        FUM 5150
        IF(NREPTY .EQ. 0) GOTO 213                                      FUM 5160
        JXTYPE = 0                                                      FUM 5170
        NXT11 = NRP11                                                   FUM 5180
        NRC = IACT + 2 + NO + NLUM + NC                                 FUM 5190
        NRC1 = NRC + 1                                                  FUM 5200
        READ (NDA11,REC=NXT11) (DU,L=1,NRC),(DAV(L),L=NRC1,KMAT),DU,DU, FUM 5210
     1   IDUM,IDU1,(DU,L=1,3),XMETAV                                    FUM 5220
        NXT11 = NXT11 + 1                                               FUM 5230
  213   CONTINUE                                                        FUM 5240
        IF(NSPALT .LE. 0) GOTO 132                                      FUM 5250
        IF(NWRITE .LE. 0) GOTO 132                                      FUM 5260
        WRITE (NDA13,REC=NXT13) JXTYPE,XMETAV                           FUM 5270
        NXT13 = NXT13 + 1                                               FUM 5280
C                                                                       FUM 5290
        CALL WRDA(IWRITE,NDA13,NXT13,L13,IDFISS,NSPALT)                 FUM 5300
C                                                                       FUM 5310
        CALL WRDA(IWRITE,NDA13,NXT13,L13,FICOMP,NSPALT)                 FUM 5320
C                                                                       FUM 5330
  132   CONTINUE                                                        FUM 5340
        IF(JXTYPE .NE. 0) NTYP2(IR) = JXTYPE                            FUM 5350
        HMETAV(IR) = HMDEN                                              FUM 5360
        IF(XMETAV .GT. 0.0) HMETAV(IR) = XMETAV                         FUM 5370
        IF(NREP .NE. 0) GOTO 113                                        FUM 5380
        IRNN = IR                                                       FUM 5390
        REALNN(5) = 1.0                                                 FUM 5400
        VCORR = 1.0                                                     FUM 5410
C                                                                       FUM 5420
        CALL ENRIX(IRNN,DAV,HMETAV,HMDEN,HMGRM,FICOMP,IDFISS,A(KA(LAWT))FUM 5430
     1   )                                                              FUM 5440
C                                                                       FUM 5450
        IF(REALNN(5) .LT. 0.) NMAKP = 1                                 FUM 5460
        TCHG2(IR) = 0.0                                                 FUM 5470
        HMGRM = HMGRM * PARVOL * XVOL                                   FUM 5480
        MHM = 1                                                         FUM 5490
        GOTO 120                                                        FUM 5500
  112   CONTINUE                                                        FUM 5510
        IF(IWRA .EQ. 1) VCORR = WERA / (PARVOL*XVOL)                    FUM 5520
        IF(NREP .EQ. 0) GOTO 120                                        FUM 5530
  113   CONTINUE                                                        FUM 5540
C                                                                       FUM 5550
C     OUT OF PILE BOX VOL./THIS BATCH POSITION VOL.  OPB/TBP            FUM 5560
C                                                                       FUM 5570
        REALNN(5) = PARVOL * XVOL / WERA                                FUM 5580
        VCORR = 1. / REALNN(5)                                          FUM 5590
        IRNN = IR                                                       FUM 5600
C                                                                       FUM 5610
        CALL REPROX(IRNN,DAV,HMDEN,HMGRM,NEWTYP,A(KA(LXREP)),HMETAV,    FUM 5620
     1   FICOMP,IDFISS,A(KA(LAWT)))                                     FUM 5630
C                                                                       FUM 5640
        IF(REALNN(5) .GE. 0.) GOTO 119                                  FUM 5650
        NMAKP = 1                                                       FUM 5660
        VCORR = VCORR / ABS(REALNN(5))                                  FUM 5670
        IF(ABS(REALNN(5)) .GT. 1.) GOTO 121                             FUM 5680
C                                                                       FUM 5690
C     MODIFY VOL. OF OUT OF PILE BOX                                    FUM 5700
C                                                                       FUM 5710
        XVOL1 = XVOL * ABS(REALNN(5))                                   FUM 5720
        WRITE (NT,118) IR,XVOL,XVOL1                                    FUM 5730
        XVOL = XVOL1                                                    FUM 5740
        GOTO 111                                                        FUM 5750
  121   CONTINUE                                                        FUM 5760
C                                                                       FUM 5770
C     ADJUST VOL. OF THIS BATCH POSITION                                FUM 5780
C                                                                       FUM 5790
        WERA1 = WERA / ABS(REALNN(5))                                   FUM 5800
        WRITE (NT,114) IR,WERA,WERA1,XSPALT                             FUM 5810
        IF(NEWTYP .LT. 0) WRITE (NT,122)                                FUM 5820
        WERA = WERA1                                                    FUM 5830
  111   CONTINUE                                                        FUM 5840
        REALNN(5) = 1.                                                  FUM 5850
  119   CONTINUE                                                        FUM 5860
        TCHG2(IR) = 0.0                                                 FUM 5870
C                                                                       FUM 5880
C     ALL VARIABLES FOR FRESH FUEL CONDITION ARE INITIALIZED IN CH7     FUM 5890
C                                                                       FUM 5900
        HMGRM = HMGRM * WERA                                            FUM 5910
        MHM = 1                                                         FUM 5920
  120   CONTINUE                                                        FUM 5930
        DO 206 IR1=NCX1,NCX2                                            FUM 5940
          NCX = LRZN(IR1)                                               FUM 5950
          IF(MRY .LE. 1 .AND. NRX .GT. 0) VOL(NCX) = PARVOL * XVOLIN    FUM 5960
          THBURN(NCX) = BURNRX                                          FUM 5970
          FADOS3(NCX) = DOSNRX                                          FUM 5980
          HM(NCX) = HMGRM * VOL(NCX) / PARVOL                           FUM 5990
          HMETAL(NCX) = HMDEN                                           FUM 6000
C                                                                       FUM 6010
C     INSERT NEW NUCLIDES IN ALL POWER GENERATING BATCHES OR SOME       FUM 6020
C     INDIVIDUAL BATCHES                                                FUM 6030
C                                                                       FUM 6040
          IF(IVSP(29) .LT. 1 .AND. HPOS .LE. 0.) GOTO 300               FUM 6050
          IF(IAE .GT. 0) GOTO 297                                       FUM 6060
          IF(NOPOW(IR) .GT. 0) GOTO 300                                 FUM 6070
  297     CONTINUE                                                      FUM 6080
          IF(IAE .LE. 0) GOTO 299                                       FUM 6090
          DO 298 I=1,IAE                                                FUM 6100
            IFI = I                                                     FUM 6110
            IF(IR .EQ. IBAE(I)) GOTO 299                                FUM 6120
  298     CONTINUE                                                      FUM 6130
          GOTO 300                                                      FUM 6140
  299     CONTINUE                                                      FUM 6150
          DO 302 I=1,IV29                                               FUM 6160
            II = INEW(I)                                                FUM 6170
            DAV(II) = DNEW(I) * FIB(IFI)                                FUM 6180
  302     CONTINUE                                                      FUM 6190
  300     CONTINUE                                                      FUM 6200
          DO 205 M=1,KMAT                                               FUM 6210
            DEN(M,NCX) = DAV(M)                                         FUM 6220
  205     CONTINUE                                                      FUM 6230
          IF(KUGL .LE. 0) GOTO 204                                      FUM 6240
          IF(NRX .GT. 0 .AND. NRX .LE. NRESHZ) GOTO 204                 FUM 6250
          IRRE1 = 0                                                     FUM 6260
          DO 260 I=1,NDR                                                FUM 6270
            KR = I                                                      FUM 6280
            IF(NCX .LE. IREG(I)) GOTO 261                               FUM 6290
            IRRE1 = IREG(I)                                             FUM 6300
  260     CONTINUE                                                      FUM 6310
  261     CONTINUE                                                      FUM 6320
          IRRE2 = IRRE1 + IHZ(KR)                                       FUM 6330
          IF(NCX .LE.  IRRE2 .OR. KUGL .EQ. 2) GOTO 265                 FUM 6340
          WRITE (NT,570) NCX,KR                                         FUM 6350
  265     CONTINUE                                                      FUM 6360
C                                                                       FUM 6370
C     NEW VOLUME TO BE FILLED (=WERA)                                   FUM 6380
C                                                                       FUM 6390
          VOLXX = PARVOL * XVOL * VCORR * VOL(NCX) / VERA(IR)           FUM 6400
          IF(NRX .GT. NRESHZ) VOLXX = PARVOL * XVOL                     FUM 6410
C                                                                       FUM 6420
C     FUELLE HOECHSTENS SOVIEL VOLUMEN IN DIE REGION KR HINEIN WIE      FUM 6430
C     PLATZ VORHANDEN IST                                               FUM 6440
C                                                                       FUM 6450
          IF(VOLXX+VOLWEG(KR) .LE. VOLIHZ(KR)) GOTO 270                 FUM 6460
          VOLXX = VOLIHZ(KR) - VOLWEG(KR)                               FUM 6470
  270     CONTINUE                                                      FUM 6480
C                                                                       FUM 6490
C     VOLXX IST DAS WIRKLICH EINGESETZTE VOLUMEN                        FUM 6500
C                                                                       FUM 6510
          IF(VOLXX .LE. 0.0) VOLXX = 1.0                                FUM 6520
          VOL(NCX) = VOLXX                                              FUM 6530
C                                                                       FUM 6540
C     VOLWEG: SOVIEL IST IN DER REGION BEREITS BESETZT                  FUM 6550
C                                                                       FUM 6560
          VOLWEG(KR) = VOLWEG(KR) + VOLXX                               FUM 6570
          VPX = PARVOL * XVOL * VCORR                                   FUM 6580
          IF(MHM .EQ. 0) VPX = PARVOL                                   FUM 6590
          HM(NCX) = HMGRM * VOLXX / VPX                                 FUM 6600
          VL = VOLXX / VCORR                                            FUM 6610
          VX(MRSATZ) = VX(MRSATZ) + VL                                  FUM 6620
          IF(NREMI .GT. 0) VR(NREMI) = VX(MRSATZ)                       FUM 6630
          EPS = (VX(MRSATZ)-PARVOL) / PARVOL                            FUM 6640
          IF(EPS .LT. 0.0001 .OR. IKLNRX .EQ. 1) GOTO 204               FUM 6650
          WRITE (NT,580) NTPNRX,PARVOL,VX(MRSATZ)                       FUM 6660
  204     CONTINUE                                                      FUM 6670
          IF(ISTB .EQ. 0) GOTO 219                                      FUM 6680
          I = NRX - NRESHZ                                              FUM 6690
          RVCB(I) = RVCB(I) - VOL(NCX) / PARVOL                         FUM 6700
  219     CONTINUE                                                      FUM 6710
  206   CONTINUE                                                        FUM 6720
        NCX = LRZN(NCX1)                                                FUM 6730
        IF(NPRINT .LE. 0) GOTO 210                                      FUM 6740
        IF(NRX-IR) 405,406,405                                          FUM 6750
  405   CONTINUE                                                        FUM 6760
        IF(NREP .NE. 0) GOTO 605                                        FUM 6770
        NRZ = NRX - NBOX                                                FUM 6780
        NRZS = NRZ - NRESHZ                                             FUM 6790
        IF(NRZ .LE. NRESHZ) WRITE (NT,401) IR,NRX,NTPNRX,VOL(NCX)       FUM 6800
        IF(NRZ .GT. NRESHZ) WRITE (NT,720) IR,NRX,NTPNRX,NRZS,VOL(NCX)  FUM 6810
        GOTO 1000                                                       FUM 6820
  605   IF(NTPNRX .GE. 0) WRITE (NT,601) IR,NRX,NTPNRX                  FUM 6830
        GOTO 210                                                        FUM 6840
  406   CONTINUE                                                        FUM 6850
        IF(NREP .NE. 0) GOTO 606                                        FUM 6860
        WRITE (NT,402) IR,VOL(NCX)                                      FUM 6870
        GOTO 1000                                                       FUM 6880
  606   WRITE (NT,602) IR                                               FUM 6890
  210   CONTINUE                                                        FUM 6900
        IF(NREPTY .NE. 0) WRITE (NT,214) IR,NTYP2(IR),WERA,HMGRM        FUM 6910
        IF(KUGL .GT. 0 .AND. NPRINT .GT. 0 .AND. NREPTY .EQ. 0) WRITE   FUM 6920
     1   (NT,404) VOL(NCX)                                              FUM 6930
 1000   CONTINUE                                                        FUM 6940
        IF(NORSH .EQ. 1 .OR. N60 .GT. 0 .OR. LOBN .EQ. 2) NRY(IR) = NRX FUM 6950
        IF(LOB .GE. 0) GOTO 45                                          FUM 6960
        IF(NRX .GT. 0 .AND. NRX .LE. NRESHZ) GOTO 45                    FUM 6970
CFZJ062                                                    04.05.11     FUM 6980
        IF(NOR .LT. 100) GOTO 46                                        FUM 6990
        WRITE (NT,10)                                                   FUM 7000
        GOTO 45                                                         FUM 7010
   46   CONTINUE                                                        FUM 7020
        NOR = NOR + 1                                                   FUM 7030
        VOR(NOR) = VOL(NCX)                                             FUM 7040
CFZJ062                                                       04.05.11  FUM 7050
        DO 44 M=1,KMAT                                                  FUM 7060
          DOR(NOR,M) = DEN(M,NCX)                                       FUM 7070
   44   CONTINUE                                                        FUM 7080
   45   CONTINUE                                                        FUM 7090
        IF(NPRINT .NE. 2) GOTO 525                                      FUM 7100
        WRITE (NT,403) IR                                               FUM 7110
        DO 522 M=1,KMAT                                                 FUM 7120
CFZJ059                                                       04.11.09  FUM 7130
          WRITE (NT,512) (BU(N,M),N=1,4),DEN(M,NCX)                     FUM 7140
  522   CONTINUE                                                        FUM 7150
  525   CONTINUE                                                        FUM 7160
        IF(TCHG2(IR) .GT. 0.0) GOTO 208                                 FUM 7170
        NNE = NNE + 1                                                   FUM 7180
        NNEU(NNE) = IR                                                  FUM 7190
  208   CONTINUE                                                        FUM 7200
        IF(NMAKP .GT. 0) GOTO 200                                       FUM 7210
        IF(NSPALT .LE. 0) GOTO 530                                      FUM 7220
        WRITE (NT,515) IR,XSPALT                                        FUM 7230
        DO 529 IT=1,NSPALT                                              FUM 7240
          J  = IDFISS(IT)                                               FUM 7250
CFZJ059                                                       04.11.09  FUM 7260
          WRITE (NT,512) (BU(N,J),N=1,4),FICOMP(IT)                     FUM 7270
  529   CONTINUE                                                        FUM 7280
  530   CONTINUE                                                        FUM 7290
        IF(MAKEUP .EQ. 0) GOTO 200                                      FUM 7300
CFZJ059                                                       04.11.09  FUM 7310
        WRITE (NT,516) (BU(N,MAKEUP),N=1,4)                             FUM 7320
  200 CONTINUE                                                          FUM 7330
CFZJ030                                                       04.05.04  FUM 7340
      IF(IWATER .LE. 0) GOTO 1003                                       FUM 7350
C                                                                       FUM 7360
CARD R20A                                                               FUM 7370
C                                                                       FUM 7380
      READ (5,2000) PPC,EPSI,NRVO,NRVH,NZ                               FUM 7390
C                                                                       FUM 7400
      IF(PPC .LE. 0.) GOTO 1001                                         FUM 7410
      WRITE (6,2010) PPC,NRVO,NRVH                                      FUM 7420
      DO 1002 IR=1,N200C                                                FUM 7430
        TC = TCELS(1,NHOT(IR)) + 273.                                   FUM 7440
C                                                                       FUM 7450
        CALL WATER(PPC,TC,EPSI,RNOX,RNH)                                FUM 7460
C                                                                       FUM 7470
        DEN(NRVO,IR) = DEN(NRVO,IR) + RNOX                              FUM 7480
        DEN(NRVH,IR) = DEN(NRVH,IR) + RNH                               FUM 7490
 1002 CONTINUE                                                          FUM 7500
 1001 CONTINUE                                                          FUM 7510
      IF(NZ .EQ. 0) GOTO 1003                                           FUM 7520
      DO 1004 N=1,NZ                                                    FUM 7530
C                                                                       FUM 7540
CARD R20B                                                               FUM 7550
C                                                                       FUM 7560
        READ (5,218) NR,PPR,EPSI,T                                      FUM 7570
C                                                                       FUM 7580
        TR = T + 273.                                                   FUM 7590
        DO 1005 IR=N200C+1,N200                                         FUM 7600
          IF(IR .NE. NR) GOTO 1005                                      FUM 7610
          WRITE (6,2020) NR,PPR,NRVO,NRVH,T                             FUM 7620
C                                                                       FUM 7630
          CALL WATER(PPR,TR,EPSI,RNOX,RNH)                              FUM 7640
C                                                                       FUM 7650
          DEN(NRVO,IR) = DEN(NRVO,IR) + RNOX                            FUM 7660
          DEN(NRVH,IR) = DEN(NRVH,IR) + RNH                             FUM 7670
          GOTO 1004                                                     FUM 7680
 1005   CONTINUE                                                        FUM 7690
 1004 CONTINUE                                                          FUM 7700
 1003 CONTINUE                                                          FUM 7710
      JSUM13 = NXT13                                                    FUM 7720
      IF(LOBN .NE. 2) GOTO 380                                          FUM 7730
      IF(IEZ .GT. 0) GOTO 370                                           FUM 7740
      DO 360 I=1,KMAT                                                   FUM 7750
        MATNR(I) = IMAT(I)                                              FUM 7760
  360 CONTINUE                                                          FUM 7770
      ITOR = 4                                                          FUM 7780
      WRITE (39) ITOR,KMAT,KFISS,N200C,N26,AVO                          FUM 7790
      ITOR = 5                                                          FUM 7800
      WRITE (39) ITOR,(MATNR(I),I=1,KMAT)                               FUM 7810
      ITOR = 6                                                          FUM 7820
      WRITE (39) ITOR,(NHOT(IR),IR=1,N200C)                             FUM 7830
      IEZ = 1                                                           FUM 7840
  370 CONTINUE                                                          FUM 7850
      ITOR = 3                                                          FUM 7860
      WRITE (39) ITOR,IPRIN(15),JN,NXSC,ISPK,DELDAY                     FUM 7870
      DO 390 IR=1,N200C                                                 FUM 7880
        WRITE (39) VOL(IR),NRY(IR),(DEN(I,IR),I=1,KMAT)                 FUM 7890
  390 CONTINUE                                                          FUM 7900
  380 CONTINUE                                                          FUM 7910
      IF(IZWBO .LE. NBOX) GOTO 152                                      FUM 7920
      WRITE (6,710) IZWBO,NBOX                                          FUM 7930
      STOP                                                              FUM 7940
  152 CONTINUE                                                          FUM 7950
      IF(N60 .EQ. 0) GOTO 199                                           FUM 7960
      DU = 0.                                                           FUM 7970
      ITEST = -1                                                        FUM 7980
      PRO(298) = PRO(298) + 1.                                          FUM 7990
      IPZ = IPRIN(15) + 1                                               FUM 8000
      WRITE (N60) ITEST                                                 FUM 8010
      WRITE (N60) IPRIN(15),N200,TBURN,NBOX,(NRY(IR),NOPOW(IR),VOL(IR), FUM 8020
     1 DU,IR=1,N200),(IBOX(I),ISTOB(I),I=1,NBOX)                        FUM 8030
      WRITE (NT,2222) PRO(298),IPZ,ITEST                                FUM 8040
      IF(NORSH .EQ. 3) MUHU(24) = 0                                     FUM 8050
  199 CONTINUE                                                          FUM 8060
      IVSP(29) = 0                                                      FUM 8070
      IF(ICONPO .LE. 0) GOTO 217                                        FUM 8080
      DO 216 I=1,ICONPO                                                 FUM 8090
C                                                                       FUM 8100
CARD R24                                                                FUM 8110
C                                                                       FUM 8120
        READ (NS,218) KR,(POISL(J,KR),J=1,NC)                           FUM 8130
C                                                                       FUM 8140
  216 CONTINUE                                                          FUM 8150
      KSIM = 0                                                          FUM 8160
C                                                                       FUM 8170
CARD R25 / IDENTIC TO CARD V15                                          FUM 8180
C                                                                       FUM 8190
      READ (NS,603) JSMAX,JSSMAX,LSIM,KSS,(NPOIS(I),I=1,KSS)            FUM 8200
C                                                                       FUM 8210
CARD R26 / IDENTIC TO CARD V16                                          FUM 8220
C                                                                       FUM 8230
      READ (NS,115) (PINMIN(I),I=1,KSS)                                 FUM 8240
C                                                                       FUM 8250
CARD R27 / IDENTIC TO CARD V17                                          FUM 8260
C                                                                       FUM 8270
      READ (NS,115) (PINMAX(I),I=1,KSS)                                 FUM 8280
C                                                                       FUM 8290
      JSER = 0                                                          FUM 8300
  217 CONTINUE                                                          FUM 8310
      KMAT3 = KMAT * 3                                                  FUM 8320
      NP14 = NEND13                                                     FUM 8330
      NEND14 = NP14 + KMAT3                                             FUM 8340
      NEND15 = NEND14                                                   FUM 8350
C                                                                       FUM 8360
      IF(KUGL .GT. 0) CALL KUGELN(NPRINT,VR,DELDAX,IN1,FISNO,DECAF,     FUM 8370
     1 A(KA(LTCH1)),NALT,NTYP1,N240,KMAT3,A(NP14),NEND14,NEND15,JAD11,  FUM 8380
     2 RVCB)                                                            FUM 8390
C                                                                       FUM 8400
      NP16 = NEND15                                                     FUM 8410
      NFUMM = NP16 + N200                                               FUM 8420
C                                                                       FUM 8430
      CALL FUMMEL(DEN,TCHG2,KRESHZ,LRZN,NTYP2,HMETAV,A(NP10),A(NP16),   FUM 8440
     1 A(KA(LSGA)),A(KA(LSGTR)),A(KA(LV1)),A(KA(LFKEN)),NFUMM,SS)       FUM 8450
C                                                                       FUM 8460
      IPRIN(15) = IPRIN(15) + 1                                         FUM 8470
      RETURN                                                            FUM 8480
      END                                                               FUM 8490
      SUBROUTINE WATER (P,T,EPSI,RNOX,RNH)                              ER    10
C                                                                       ER    20
      COMMON /BLOCKK/ D(3),AVO                                          ER    30
C                                                                       ER    40
      R = 8.3144                                                        ER    50
      RNOX = 0.1 * P * AVO * EPSI / (R * T)                             ER    60
      RNH = 2. * RNOX                                                   ER    70
      RETURN                                                            ER    80
      END                                                               ER    90