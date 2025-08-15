      SUBROUTINE CH6(MAFIA,IREG,SSLUMP,NHOT,VOL,DEN,SGA,SGTR,V1,FKEN,   CH6   10
     1 JAD11,GS,HMETAL,CONC,SS,MIX)                                     CH6   20
C                                                                       CH6   30
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    CH6   40
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    CH6   50
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PICH6   60
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 CH6   70
C                                                                       CH6   80
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1CH6   90
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         CH6  100
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    CH6  110
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)CH6  120
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  CH6  130
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    CH6  140
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         CH6  150
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,CH6  160
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            CH6  170
C                                                                       CH6  180
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, CH6  190
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        CH6  200
     2 FIMAKL(20),NOPILE,MREP,MARX(10)                                  CH6  210
C                                                                       CH6  220
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), CH6  230
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10CH6  240
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11CH6  250
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         CH6  260
C                                                                       CH6  270
      COMMON /GRENZE/ NURTHM                                            CH6  280
C                                                                       CH6  290
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300),FABC(10),REPC(10),NNECH6  300
     1 ,LISTEQ,NTIK                                                     CH6  310
C                                                                       CH6  320
      COMMON /GER/ GERWIN                                               CH6  330
C                                                                       CH6  340
      COMMON /SPECTI/ ITIK(10),TI,TO,PO,NTITO,SEARCH,NYS,CRIT0,EPSKTO,  CH6  350
     1 DKDDT,DTIDTO,REFLET,RE0,TO0,DELDAU,TI1,TO1                       CH6  360
C                                                                       CH6  370
CFZJ062                                                      04.05.11   CH6  380
      COMMON /ORIGEN/ LOB,NOR,VOR(100),ISPK,KFISS,N200C,NXSC,KGR,       CH6  390
     1 LOBN,IEZ                                                         CH6  400
C                                                                       CH6  410
      COMMON /VARDIM/ A(8000000)                                        CH6  420
C                                                                       CH6  430
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       CH6  440
C                                                                       CH6  450
      COMMON /FLUXN/ D(361),IACT                                        CH6  460
C                                                                       CH6  470
CFZJ041                                                       15.02.05  CH6  480
CFZJ058                                                       05.11.08  CH6  490
      COMMON /KEFFT/ TKEFF(5000),RKEFF(5000),IPKEFF,NKEFF,TKUM,POWLMAX, CH6  500
     1 THETMX,POWLM(5000),POWB(5000),BUSCRAP(5000),THETM(5000),         CH6  510
     2 POWERF(5000),TEIN(5000),TAUS(5000),TEINTR,TAUSTR                 CH6  520
C                                                                       CH6  530
CFZJ011 Increase dimensions of arrays SGA,SGTR,FKEN           01.12.03  CH6  540
      DIMENSION IREG(NDR),SSLUMP(N26,NDR),NHOT(N200),VOL(N200),         CH6  550
     1 DEN(KMAT,N200),SGA(20,N26),SGTR(20,N26,N26),V1(201,N26,3),       CH6  560
     2 FKEN(20,N26),CONC(KMAT),JAD11(JD11),GS(IACT),HMETAL(N200),       CH6  570
     3 SS(N26,N200),MIX(NDR)                                            CH6  580
C                                                                       CH6  590
CFZJ042                                                       09.09.05  CH6  600
      EQUIVALENCE(JTPE3,NT),(LI(11),LRPHI),(LI(4),LTOSI),(LI(5),LABSI), CH6  610
     1 (LI(6),LFISI),(LI(7),LXNU),(LI(18),LCFRA),(LI(19),LPOIM),        CH6  620
     2 (LI(20),LPOIL),(LI(21),LPINI),(LI(22),LPINA),(LI(23),LNPOI),     CH6  630
     3 (LI(8),LUMRE),(LI(33),LFISM),(LI(34),LXXXN),(LI(35),LYYYN),      CH6  640
     4 (LI(36),LCRNE),(LI(37),LDENI),(LI(38),LYIEX),(LI(39),LYIEJ),     CH6  650
     5 (LI(40),LHM),(LI(41),LTHBU),(LI(42),LFADO),(LI(51),LNOPO),       CH6  660
     6 (LI(64),LTEMZ),(LI(65),LTCEL),(LI(68),LANU),(LI(69),LA1),        CH6  670
     7 (LI(70),LA2),(LI(71),LA3),(LI(72),LA4),(LI(73),LA5),(LI(97),LTFU)CH6  680
     8 ,(LI(98),LTMO),(LI(99),LTRAT),(LI(101),LB),(LI(103),LDOR),       CH6  690
     9 (LI(104),LFIRA),(LI(105),LABSX),(LI(106),LDSP),(LI(102),LNRY),   CH6  700
     X (LI(1),LIMAT),(LI(151),LAWT),(LI(153),LGM),(LI(178),LSN2N),      CH6  710
     Y (LI(48),LNRG),(LI(26),LAGEF),(LI(27),LVREG),(LI(47),LVRP)        CH6  720
C                                                                       CH6  730
    2 FORMAT (36H      REACTOR                       ,14X,10H,K-EFF =  ,CH6  740
     1 1PE11.5)                                                         CH6  750
   62 FORMAT (/' *** ZKFIND = ',E14.7,' REACT(2) = ',E14.7,' ABKEFF =', CH6  760
     1 E14.7/)                                                          CH6  770
  200 FORMAT (/' *** FLUXES (CIT.-NORMIERT):',8E12.5)                   CH6  780
  500 FORMAT (65H1 EXCEEDED MAXIMUM PERMITTED TOTAL OF CONTROL POISON ADCH6  790
     1JUSTMENTS )                                                       CH6  800
  501 FORMAT (72H1 EXCEEDED MAXIMUM PERMITTED CONTROL POISON ADJUSTMENTSCH6  810
     1 FOR SOME REGION )                                                CH6  820
C                                                                       CH6  830
C                                                                       CH6  840
      IPRIN(10) = 0                                                     CH6  850
      IF(INZWX .EQ. 0) PRO(174) = IPRIN(3)                              CH6  860
      IF(FF(1) .GT. 0.) GOTO 122                                        CH6  870
      DO 123 I=1,4                                                      CH6  880
        FF(I) = PRO(I+169)                                              CH6  890
  123 CONTINUE                                                          CH6  900
  122 CONTINUE                                                          CH6  910
C                                                                       CH6  920
C     STROEMUNGSKORREKTUR                                               CH6  930
C                                                                       CH6  940
      IF(MUHU(3) .NE. 1) GOTO 20                                        CH6  950
      F = FF(1)                                                         CH6  960
      GERWIN = 2. / 3. * F * (((1.-F)/F)**2+1./8.)                      CH6  970
   20 CONTINUE                                                          CH6  980
      IPRIN(7) = 0                                                      CH6  990
  111 CONTINUE                                                          CH6 1000
      IF(MUHU(28) .GT. 0) GOTO 112                                      CH6 1010
      IF(IVSP(11) .EQ. 0) GOTO 114                                      CH6 1020
      ID20 = IDIFF(20)                                                  CH6 1030
      IF(ID20 .EQ. 0) GOTO 112                                          CH6 1040
      IF(REACT(2) .LE. 0.) GOTO 112                                     CH6 1050
      DO 113 I=1,ID20                                                   CH6 1060
        IF(IDIFF(I) .EQ. JN) GOTO 112                                   CH6 1070
  113 CONTINUE                                                          CH6 1080
  114 CONTINUE                                                          CH6 1090
C                                                                       CH6 1100
      CALL DENOUT(IREG,A(KA(LCFRA)),A(KA(LPOIL)),A(KA(LNPOI)),VOL,DEN,  CH6 1110
     1 A(KA(LHM)),A(KA(LTHBU)),A(KA(LIMAT)),A(KA(LAWT)),A(KA(LGM)),GS)  CH6 1120
C                                                                       CH6 1130
      GOTO 61                                                           CH6 1140
C                                                                       CH6 1150
C     PERFORM DIFFUSION CALCULATION                                     CH6 1160
C                                                                       CH6 1170
  112 CONTINUE                                                          CH6 1180
      MUHU(7) = 0                                                       CH6 1190
      MUHU(2) = 2                                                       CH6 1200
      MUHU(6) = 1                                                       CH6 1210
      NFUMM = 0                                                         CH6 1220
C                                                                       CH6 1230
      CALL CITA(1,SGA,SGTR,V1,FKEN,NFUMM,SS)                            CH6 1240
C                                                                       CH6 1250
CFZJ041                                                       15.02.05  CH6 1260
      NKEFF = NKEFF + 1                                                 CH6 1270
      TKEFF(NKEFF) = TKUM                                               CH6 1280
      RKEFF(NKEFF) = REACT(2)                                           CH6 1290
      POWLM(NKEFF) = POWLMAX                                            CH6 1300
      POWB(NKEFF) = PRO(46)                                             CH6 1310
      BUSCRAP(NKEFF) = PRO(38)                                          CH6 1320
      THETM(NKEFF) = THETMX                                             CH6 1330
      POWERF(NKEFF) = POWER                                             CH6 1340
      TEIN(NKEFF) = TEINTR                                              CH6 1350
      TAUS(NKEFF) = TAUSTR                                              CH6 1360
   61 CONTINUE                                                          CH6 1370
      IF(JN .LT. 0) GOTO 30                                             CH6 1380
      M10 = MUHU(10)                                                    CH6 1390
      JJ = 2 * JN                                                       CH6 1400
      IF(M10 .GT. 0) WRITE (M10) JN,REACT(2),SPECK(JJ+2),DELDAY,(GS(I), CH6 1410
     1 I=1,IACT)                                                        CH6 1420
   30 CONTINUE                                                          CH6 1430
      ABKEFF = ABS(ZKFIND-REACT(2))                                     CH6 1440
      IF(JSER .EQ. 0) WRITE (NT,62) ZKFIND,REACT(2),ABKEFF              CH6 1450
      KINIT = 1                                                         CH6 1460
C                                                                       CH6 1470
C     AKTUELLE FLUSSNORMIERUNG FUER AUSDRUCK AM ZYKLUSENDE              CH6 1480
C     (NUR BEI ZELL-ABBRANDRECHNUNG OHNE CITATION)                      CH6 1490
C                                                                       CH6 1500
      IF(MUHU(28) .LE. 0) GOTO 600                                      CH6 1510
      IF(JN .LT. JNSTOP) GOTO 600                                       CH6 1520
      MUHU(2) = 1                                                       CH6 1530
C                                                                       CH6 1540
      CALL CITA(2,SGA,SGTR,V1,FKEN,NFUMM,SS)                            CH6 1550
C                                                                       CH6 1560
      MUHU(2) = 0                                                       CH6 1570
      WRITE (NT,200) (SS(IE,1),IE=1,N26)                                CH6 1580
  600 CONTINUE                                                          CH6 1590
C                                                                       CH6 1600
C     JSER = 0  : BURNUP WITH CONTROL POISON SEARCH                     CH6 1610
C     JSER = 1  : BURNUP                                                CH6 1620
C     JSER = 2  : DIFFUSION CALCULATION WITHOUT BURNUP                  CH6 1630
C                                                                       CH6 1640
      IF(JSER-1) 6,5,7                                                  CH6 1650
    5 CONTINUE                                                          CH6 1660
      IF(ITIK(5) .LE. 0 .OR. ITIK(6) .LE. 0) GOTO 9                     CH6 1670
      IF(JN .NE. JNSTOP) GOTO 9                                         CH6 1680
      R2C = CRIT0 - REACT(2)                                            CH6 1690
      IF(ABS(R2C) .LE. EPSKTO) GOTO 7                                   CH6 1700
      GOTO 8                                                            CH6 1710
    9 CONTINUE                                                          CH6 1720
      IF(JN .GE. JNSTOP) GOTO 7                                         CH6 1730
    8 IF(REACT(2)-ZKFIND) 7,7,10                                        CH6 1740
C                                                                       CH6 1750
C     ONLY FOR CONTROL-POISON CASES                                     CH6 1760
C                                                                       CH6 1770
    6 CONTINUE                                                          CH6 1780
      IF(ABKEFF .GE. SERCON) GOTO 12                                    CH6 1790
      IF(JN .GE. JNSTOP) GOTO 7                                         CH6 1800
      JS = 0                                                            CH6 1810
   10 CONTINUE                                                          CH6 1820
      WRITE (6,2) REACT(2)                                              CH6 1830
      JSPEC = JSPEC + 1                                                 CH6 1840
      SPECK(JSPEC) = REACT(2)                                           CH6 1850
      JSPEC = JSPEC + 1                                                 CH6 1860
      NSYN = 0                                                          CH6 1870
      NP34 = NEND33                                                     CH6 1880
      NP341 = NP34 + KMAT                                               CH6 1890
      NP342 = NP341 + N200                                              CH6 1900
      NEND34 = NP342 + N200                                             CH6 1910
      KMAT2 = KMAT + 2                                                  CH6 1920
      N266 = N26 * 2 + 2                                                CH6 1930
C                                                                       CH6 1940
      CALL SYNOPS(NSYN,VOL,DEN,A(KA(LFISM)),A(KA(LTHBU)),A(KA(LFADO)),  CH6 1950
     1 A(KA(LB)),JAD11,KMAT2,N266,A(KA(LIMAT)),A(NP34),A(NP341),A(NP342)CH6 1960
     2 ,GS,MIX,A(KA(LNRG)))                                             CH6 1970
C                                                                       CH6 1980
      NP35 = NEND34                                                     CH6 1990
      NP36 = NP35 + N200 * 8                                            CH6 2000
      NP37 = NP36 + N200 * N26                                          CH6 2010
      NP38 = NP37 + N200                                                CH6 2020
      NPA = NP38 + N26                                                  CH6 2030
      NPF = NPA + N26 * KMAT                                            CH6 2040
      NPN = NPF + N26 * KFISS                                           CH6 2050
      NEND38 = NPN + N26 * KFISS                                        CH6 2060
      TKUM = TKUM + DELDAY                                              CH6 2070
C                                                                       CH6 2080
CFZJ042                                                       09.09.05  CH6 2090
      CALL BURNUP(A(KA(LABSI)),KL(LABSI),A(KA(LFISI)),KL(LFISI),        CH6 2100
     1 A(KA(LXNU)),IREG,A(KA(LRPHI)),SSLUMP,NHOT,VOL,DEN,SS,A(KA(LXXXN))CH6 2110
     2 ,A(KA(LYYYN)),A(KA(LCRNE)),A(KA(LDENI)),A(KA(LYIEX)),A(KA(LYIEJ))CH6 2120
     3 ,A(KA(LHM)),A(KA(LTHBU)),A(KA(LFADO)),A(KA(LNOPO)),A(KA(LTEMZ)), CH6 2130
     4 A(KA(LTCEL)),A(KA(LTFU)),A(KA(LTMO)),A(KA(LTRAT)),A(KA(LNRY)),   CH6 2140
     5 A(KA(LDOR)),A(KA(LFIRA)),A(KA(LABSX)),A(KA(LDSP)),A(NP35),A(NP36)CH6 2150
     6 ,A(NP37),A(NP38),A(NPA),A(NPF),NEND38,NEND46,A(KA(LIMAT)),GS,    CH6 2160
     7 A(KA(LSN2N)),A(NPN),HMETAL,MIX,A(KA(LNRG)),SGA,SGTR,V1,FKEN,     CH6 2170
     8 A(KA(LAGEF)),A(KA(LVREG)),A(KA(LVRP)))                           CH6 2180
C                                                                       CH6 2190
      LSER = 1                                                          CH6 2200
      GOTO 1000                                                         CH6 2210
C                                                                       CH6 2220
C     CONTROL POISON ADJUSTMENT                                         CH6 2230
C                                                                       CH6 2240
   12 CONTINUE                                                          CH6 2250
      NP47 = NEND33                                                     CH6 2260
      NP48 = NP47 + NDR * 2                                             CH6 2270
C                                                                       CH6 2280
      CALL CRPULL(IREG,A(KA(LPOIM)),A(KA(LPOIL)),A(KA(LPINI)),          CH6 2290
     1 A(KA(LPINA)),A(KA(LNPOI)),DEN,A(NP47),A(NP48))                   CH6 2300
C                                                                       CH6 2310
      LSER = 0                                                          CH6 2320
C                                                                       CH6 2330
C     NSWIT = 1  : ALL POISON GONE                                      CH6 2340
C                                                                       CH6 2350
      IF(NSWIT-1) 18,7,18                                               CH6 2360
C                                                                       CH6 2370
C     OVERALL NUMBER OF C.-P. ADJUSTMENTS > MAX NR  ?                   CH6 2380
C                                                                       CH6 2390
   18 IF(JSS-JSSMAX) 15,17,17                                           CH6 2400
C                                                                       CH6 2410
C     NUMBER OF C.-P. ADJUSTMENTS FOR ANY REGION > MAX NR  ?            CH6 2420
C                                                                       CH6 2430
   15 IF(JS-JSMAX) 111,77,77                                            CH6 2440
   17 WRITE (NT,500)                                                    CH6 2450
      GOTO 7                                                            CH6 2460
   77 WRITE (NT,501)                                                    CH6 2470
      GOTO 7                                                            CH6 2480
C                                                                       CH6 2490
C     PREPARING SPECTRUM CALCULATION FOR NEXT TIME STEP                 CH6 2500
C                                                                       CH6 2510
 1000 CONTINUE                                                          CH6 2520
      NURTHM = 0                                                        CH6 2530
C                                                                       CH6 2540
C     IPRIN(4) = 0 : SPECTRUM CALCULATION IS NOT REPEATED               CH6 2550
C                                                                       CH6 2560
      IF(IPRIN(4) .EQ. 0) GOTO 111                                      CH6 2570
C                                                                       CH6 2580
C     ISPEKT(20) > 0 : REPEATE SPECTRUM CALCULATIONS ONLY IN TIME STEPS CH6 2590
C                      JN = ISPEKT(JNSP),  JNSP=1,18                    CH6 2600
C                < 0 : ONLY THERMAL SPECTRUM                            CH6 2610
C                                                                       CH6 2620
      ID20 = ISPEKT(20)                                                 CH6 2630
      IF(ID20 .EQ. 0) GOTO 119                                          CH6 2640
      DO 115 I=JNSP,ID20                                                CH6 2650
        M = I                                                           CH6 2660
        IF(ISPEKT(I) .EQ. JN) GOTO 108                                  CH6 2670
  115 CONTINUE                                                          CH6 2680
      GOTO 111                                                          CH6 2690
  108 CONTINUE                                                          CH6 2700
      JNSP = M                                                          CH6 2710
  119 CONTINUE                                                          CH6 2720
C                                                                       CH6 2730
C     CALCULATE AVERAGE DENSITIES FOR SPECTRUM CALCULATIONS             CH6 2740
C     WRITE ATOM DENSITIES ON DIRECT ACCESS UNIT NDA11                  CH6 2750
C                                                                       CH6 2760
      IE = N26                                                          CH6 2770
      NRO = IACT + 2 + NO                                               CH6 2780
      NRL = NRO + NLUM                                                  CH6 2790
      DO 110 N=1,NXS                                                    CH6 2800
        KR = 1                                                          CH6 2810
        IRRE2 = IREG(KR)                                                CH6 2820
        KREG = KR                                                       CH6 2830
        B = 0.                                                          CH6 2840
        DO 100 M=1,KMAT                                                 CH6 2850
          CONC(M) = 0.0                                                 CH6 2860
  100   CONTINUE                                                        CH6 2870
        DO 102 IR=1,N200                                                CH6 2880
          IXS = NHOT(IR)                                                CH6 2890
          IF(IXS .NE. N) GOTO 102                                       CH6 2900
          IF(NLUM .EQ.0) GOTO 105                                       CH6 2910
          IRSUB = IR                                                    CH6 2920
  109     CONTINUE                                                      CH6 2930
          IF(IR. LE. IRRE2) GOTO 104                                    CH6 2940
          KR = KR + 1                                                   CH6 2950
          IRRE2 = IREG(KR)                                              CH6 2960
          KREG = KR                                                     CH6 2970
          GOTO 109                                                      CH6 2980
  104     CONTINUE                                                      CH6 2990
C                                                                       CH6 3000
          CALL LUMP(A(KA(LTOSI)),KL(LTOSI),A(KA(LUMRE)),SSLUMP,NHOT,DEN,CH6 3010
     1     A(KA(LANU)),A(KA(LA1)),A(KA(LA2)),A(KA(LA3)),A(KA(LA4)),     CH6 3020
     2     A(KA(LA5)))                                                  CH6 3030
C                                                                       CH6 3040
  105     CONTINUE                                                      CH6 3050
          B = B + VOL(IR)                                               CH6 3060
          DO 101 M=1,KMAT                                               CH6 3070
            IF(M .LE. NRO .OR . M .GT. NRL) GOTO 107                    CH6 3080
            CONC(M) = CONC(M) + DEN(M,IR) * VOL(IR) * SSLUMP(IE,IR)     CH6 3090
            GOTO 101                                                    CH6 3100
  107       CONTINUE                                                    CH6 3110
            CONC(M) = CONC(M) + DEN(M,IR) * VOL(IR)                     CH6 3120
  101     CONTINUE                                                      CH6 3130
  102   CONTINUE                                                        CH6 3140
        DO 103 M=1,KMAT                                                 CH6 3150
          CONC(M) = CONC(M) / B                                         CH6 3160
  103   CONTINUE                                                        CH6 3170
        JSATZ = 2 + N                                                   CH6 3180
        NXT11 = JAD11(JSATZ)                                            CH6 3190
        WRITE (NDA11,REC=NXT11) (CONC(M),M=1,KMAT)                      CH6 3200
        NXT11 = NXT11 + 1                                               CH6 3210
  110 CONTINUE                                                          CH6 3220
      MAFIA = 4                                                         CH6 3230
      RETURN                                                            CH6 3240
C                                                                       CH6 3250
C     TERMINATE CALCULATIONS                                            CH6 3260
C                                                                       CH6 3270
    7 WRITE (6,2) REACT(2)                                              CH6 3280
      JSPEC = JSPEC + 1                                                 CH6 3290
      SPECK(JSPEC) = REACT(2)                                           CH6 3300
      JSPEC = JSPEC + 1                                                 CH6 3310
C                                                                       CH6 3320
      CALL FINAL(NP341,NP342,MIX,A(KA(LNRG)))                           CH6 3330
C                                                                       CH6 3340
      JNSP = 1                                                          CH6 3350
      MAFIA = 7                                                         CH6 3360
      RETURN                                                            CH6 3370
      END                                                               CH6 3380
      SUBROUTINE LUMP(TOSIG,LA0,UMREG,SSLUMP,NHOT,DEN,A0,A1,A2,A3,A4,A5)LUM   10
C                                                                       LUM   20
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    LUM   30
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    LUM   40
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PILUM   50
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 LUM   60
C                                                                       LUM   70
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1LUM   80
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         LUM   90
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    LUM  100
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)LUM  110
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  LUM  120
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    LUM  130
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         LUM  140
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,LUM  150
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            LUM  160
C                                                                       LUM  170
      COMMON /FLUXN/ D(361),IACT                                        LUM  180
C                                                                       LUM  190
      DIMENSION WTOT(3),TOSIG(LA0),UMREG(NDR),SSLUMP(N26,NDR),NHOT(N200)LUM  200
     1 ,DEN(KMAT,N200),A0(N26),A1(N26),A2(N26),A3(N26),A4(N26),A5(N26)  LUM  210
C                                                                       LUM  220
C                                                                       LUM  230
      KR = KREG                                                         LUM  240
      IR = IRSUB                                                        LUM  250
      DO 200 IE=1,N26                                                   LUM  260
        TOTAL = 0.                                                      LUM  270
        DO 100 L=NK,NL                                                  LUM  280
          K = L - IACT - 2 - NO                                         LUM  290
          IF(IE .GT. 1) GOTO 110                                        LUM  300
          WTOT(K) = DEN(L,IR) * UMREG(KR)                               LUM  310
  110     CONTINUE                                                      LUM  320
          IL = ((NHOT(IR)-1)*KMAT + (L-1)) * N26                        LUM  330
          TOTAL = TOTAL + WTOT(K) * TOSIG(IE+IL)                        LUM  340
  100   CONTINUE                                                        LUM  350
        SSLU = A0(IE) + A1(IE) * TOTAL + A2(IE) * TOTAL**2 + A3(IE) *   LUM  360
     1   TOTAL**3 + A4(IE) * TOTAL**4 + A5(IE) * TOTAL**5               LUM  370
        SSLUMP(IE,IR) = 1.0 / SQRT(SSLU)                                LUM  380
  200 CONTINUE                                                          LUM  390
      RETURN                                                            LUM  400
      END                                                               LUM  410
      SUBROUTINE DENOUT(IREG,CFRAC,POISL,NPOIS,VOL,DEN,HM,THBURN,IMAT,  DEN   10
     1 AWT,GM,GS)                                                       DEN   20
C                                                                       DEN   30
C     PRINT OUT ATOM DENSITIES AND SELF SHIELDING FACTORS,CALCULATION   DEN   40
C     OF WEIGHTS OF MATERIALS AND CONTROL POISON INSERTION              DEN   50
C                                                                       DEN   60
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    DEN   70
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    DEN   80
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIDEN   90
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 DEN  100
C                                                                       DEN  110
      EQUIVALENCE(JTPE3,NT)                                             DEN  120
C                                                                       DEN  130
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1DEN  140
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         DEN  150
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    DEN  160
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)DEN  170
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  DEN  180
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    DEN  190
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         DEN  200
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,DEN  210
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            DEN  220
C                                                                       DEN  230
      COMMON /BLOCKK/ DU(3),AVO                                         DEN  240
C                                                                       DEN  250
      COMMON /ORIGEN/ DUM(104),N200C                                    DEN  260
C                                                                       DEN  270
      COMMON /FLUXN/ D(361),IACT                                        DEN  280
C                                                                       DEN  290
      COMMON /BUC/ BU(6,200)                                            DEN  300
C                                                                       DEN  310
      DIMENSION IREG(NDR),CFRAC(2,NDR),POISL(2,NDR),NPOIS(NDR),VOL(N200)DEN  320
     1 ,DEN(KMAT,N200),HM(N200),THBURN(N200),IMAT(KMAT),GS(IACT),       DEN  330
     2 AWT(IACT),GM(IACT,N200)                                          DEN  340
C                                                                       DEN  350
      CHARACTER*4 W2(2)/'REG','ION'/,BU                                 DEN  360
C                                                                       DEN  370
    5 FORMAT (///15X,40HSTATUS OF CONTROL POISONS  --  TIME STEP,I3//13XDEN  380
     1 ,2A3,41H  MATERIAL  ATOM DENSITY  INSERTION FRACT//)             DEN  390
    6 FORMAT (8X,2I9,2E16.5)                                            DEN  400
   11 FORMAT ('1TIME STEP',I4,37X,'ATOMIC DENSITIES'//' BATCHES',17X,   DEN  410
     1 8I11/)                                                           DEN  420
CFZJ059                                                       04.11.09  DEN  430
   13 FORMAT (1H ,4A4,8X,8(1PE11.4))                                    DEN  440
CFZJ059                                                       04.11.09  DEN  450
   14 FORMAT (1H ,4A4,8X,9(1PE11.4))                                    DEN  460
  601 FORMAT (11H1 TIME STEP,I4,31X,28HWEIGHTS OF MATERIALS (GRAMS))    DEN  470
  690 FORMAT (/' BATCHES                 ',8I11/)                       DEN  480
  692 FORMAT (/' BATCHES                 ',8I11,'      TOTAL'/)         DEN  490
  696 FORMAT (/' TOTAL OF EACH BATCH     ',8(1PE11.4))                  DEN  500
C                                                                       DEN  510
C                                                                       DEN  520
C  S T A R T                                                            DEN  530
C                                                                       DEN  540
      IF(KINIT .GT. 0 .AND. LSER .EQ. 0) GOTO 400                       DEN  550
      IF(IPRIN(2) .LE. 1 .OR. IPRIN(3) .LT. 0) GOTO 95                  DEN  560
      IRC = N200                                                        DEN  570
      DO 90 IR=1,IRC,8                                                  DEN  580
        IRB = IR + 7                                                    DEN  590
        IF(IRB .GT. N200) IRB = N200                                    DEN  600
        WRITE (NT,11) JN,(I,I=IR,IRB)                                   DEN  610
        DO 70 L=1,KMAT                                                  DEN  620
CFZJ059                                                       04.11.09  DEN  630
          WRITE (NT,13) (BU(N,L),N=1,4),(DEN(L,IRA),IRA=IR,IRB)         DEN  640
   70   CONTINUE                                                        DEN  650
   90 CONTINUE                                                          DEN  660
C                                                                       DEN  670
C     GRAMS OF MATERIALS                                                DEN  680
C                                                                       DEN  690
   95 CONTINUE                                                          DEN  700
      DO 290 L=1,IACT                                                   DEN  710
        GS(L) = 0.0                                                     DEN  720
  290 CONTINUE                                                          DEN  730
      IF(KINIT .GT. 0) GOTO 310                                         DEN  740
      DO 300 I=1,N200                                                   DEN  750
        IF(THBURN(I) .GT. 0.0) GOTO 300                                 DEN  760
        HM(I) = 0.0                                                     DEN  770
  300 CONTINUE                                                          DEN  780
      HMTOT = 0.0                                                       DEN  790
  310 CONTINUE                                                          DEN  800
      N8 = 8                                                            DEN  810
      DO 620 IR=1,N200C                                                 DEN  820
        DO 621 L=1,IACT                                                 DEN  830
          GM(L,IR) = DEN(L,IR) * VOL(IR) * AWT(L) / AVO                 DEN  840
          IF(KINIT .LE. 0 .AND. THBURN(IR) .LE. 0.) HM(IR) = HM(IR) +   DEN  850
     1     GM(L,IR)                                                     DEN  860
          GS(L) = GS(L) + GM(L,IR)                                      DEN  870
  621   CONTINUE                                                        DEN  880
  620 CONTINUE                                                          DEN  890
      IF(IPRIN(2) .LT. 2 .OR. IPRIN(3) .LT. 0) GOTO 650                 DEN  900
      DO 625 IR=1,N200C,N8                                              DEN  910
        IRB = IR + 7                                                    DEN  920
        IF(IRB .GT. N200C) IRB = N200C                                  DEN  930
        WRITE (NT,601) JN                                               DEN  940
        IF(IRB .LT. N200C) WRITE (NT,690) (I,I=IR,IRB)                  DEN  950
        IF(IRB .EQ. N200C) WRITE (NT,692) (I,I=IR,IRB)                  DEN  960
        DO 624 L=1,IACT                                                 DEN  970
CFZJ059                                                       04.11.09  DEN  980
          IF(IRB .LT. N200C) WRITE (NT,13) (BU(N,L),N=1,4),(GM(L,IRA),  DEN  990
     1     IRA=IR,IRB)                                                  DEN 1000
CFZJ059                                                       04.11.09  DEN 1010
          IF(IRB .EQ. N200C) WRITE (NT,14) (BU(N,L),N=1,4),(GM(L,IRA),  DEN 1020
     1     IRA=IR,IRB),GS(L)                                            DEN 1030
          IF(L .NE. IACT) GOTO 624                                      DEN 1040
          IF(KINIT .LE. 0) WRITE (NT,696) (HM(IRA),IRA=IR,IRB)          DEN 1050
  624   CONTINUE                                                        DEN 1060
  625 CONTINUE                                                          DEN 1070
  650 CONTINUE                                                          DEN 1080
      IF(KINIT .GT. 0) GOTO 400                                         DEN 1090
      DO 635 I=1,IACT                                                   DEN 1100
        HMTOT = HMTOT + GS(I)                                           DEN 1110
  635 CONTINUE                                                          DEN 1120
  400 CONTINUE                                                          DEN 1130
C                                                                       DEN 1140
C     CONTROL ROD INSERTION FRACTION                                    DEN 1150
C                                                                       DEN 1160
      LMC = 0                                                           DEN 1170
      KR = 1                                                            DEN 1180
      IRRE1 = IREG(KR)                                                  DEN 1190
      DO 450 IR=1,N200                                                  DEN 1200
        IF(IR .LE. IRRE1) GOTO 401                                      DEN 1210
        KR = KR + 1                                                     DEN 1220
        IRRE1 = IREG(KR)                                                DEN 1230
  401   CONTINUE                                                        DEN 1240
        IF(JSER .NE. 0) GOTO 450                                        DEN 1250
        NK = IACT + 3 + NO + NLUM                                       DEN 1260
        NL = NK - 1 + NC                                                DEN 1270
        LMC = LMC + 1                                                   DEN 1280
        IF(LMC .EQ. 1) WRITE (NT,5) JN,W2                               DEN 1290
        DO 420 L=NK,NL                                                  DEN 1300
          K = L - NK + 1                                                DEN 1310
          IF(POISL(K,KR) .GT. 0.0) CFRAC(K,KR) = DEN(L,IR) / POISL(K,KR)DEN 1320
          DO 421 I=1,KSS                                                DEN 1330
            IF(KR .EQ. NPOIS(I)) WRITE (NT,6) KR,L,DEN(L,IR),CFRAC(K,KR)DEN 1340
  421     CONTINUE                                                      DEN 1350
  420   CONTINUE                                                        DEN 1360
  450 CONTINUE                                                          DEN 1370
      RETURN                                                            DEN 1380
      END                                                               DEN 1390
      SUBROUTINE LIBREX(OUSIG,LA0,TOSIG,ABSIG,FISIG,LF0,XNU,NHOT,VOL,DENLIB   10
     1 ,THBURN,FADOS3,NOPOW,NXSKM,SJNU,SJGF,SJGT,SJGA,SJGR,CHI,DN,IZON, LIB   20
     2 IMAT,COSSHM,TEML,HM,POWSUB,MIX,NRG)                              LIB   30
C                                                                       LIB   40
CFZJ042                                                       09.09.05  LIB   50
C                                                                       LIB   60
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    LIB   70
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    LIB   80
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PILIB   90
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 LIB  100
C                                                                       LIB  110
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1LIB  120
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         LIB  130
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    LIB  140
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)LIB  150
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  LIB  160
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    LIB  170
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         LIB  180
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,LIB  190
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            LIB  200
C                                                                       LIB  210
      COMMON /NORM/ PDUMMY,IMX,ICI(9999),IVS(9999),VFV(9999),VFC(9999)  LIB  220
C                                                                       LIB  230
      COMMON /STA/ IST,SB,TEX,JNS,DUM(5),POWAV,POWL(10000),LAYT         LIB  240
C                                                                       LIB  250
CFZJ042                                                       09.09.05  LIB  260
      DIMENSION THB(1500),FADO(1500),YXE(5),YIO(5),NOP(1500),NSP(1500), LIB  270
     1 XLAM(49),OUSIG(LA0),TOSIG(LA0),ABSIG(LA0),FISIG(LF0),XNU(LF0),   LIB  280
     2 NHOT(N200),VOL(N200),DEN(KMAT,N200),THBURN(N200),FADOS3(N200),   LIB  290
     3 NOPOW(N200),SJNU(N26),SJGF(N26),SJGT(N26),SJGA(N26),SJGR(N26,N26)LIB  300
     4 ,CHI(N26),DN(NXSKM),IZON(N200),IMAT(KMAT),COSSHM(NXS),           LIB  310
     5 TEML(3,N200),HM(N200),POWSUB(N200),MIX(NDR),NRG(N200)            LIB  320
C                                                                       LIB  330
      CHARACTER*4 TITEL(18)                                             LIB  340
C                                                                       LIB  350
      EQUIVALENCE(JTPE2,N5),(JTPE3,N6),(DIRAC(1,5),XLAM(1))             LIB  360
C                                                                       LIB  370
    2 FORMAT (I7,I5,I9,E16.5,4E12.5)                                    LIB  380
    4 FORMAT ('  BATCH  NOPOW  SPEC.ZONE     POWER      BURNUP     FASTDLIB  390
     1OSE     TGRAPH       TFUEL'/)                                     LIB  400
  100 FORMAT (18A4)                                                     LIB  410
  101 FORMAT (/1X,18A4//)                                               LIB  420
  120 FORMAT ('1',3X,'DENSITIES:'/4X,10('=')/60(I4,6X,10E12.5/))        LIB  430
C                                                                       LIB  440
      NAMELIST /HAAS/ MMAX,JMAT,KONUC                                   LIB  450
C                                                                       LIB  460
C                                                                       LIB  470
CARD R34                                                                LIB  480
C                                                                       LIB  490
      READ(N5,100) (TITEL(I),I=1,18)                                    LIB  500
      WRITE(6,101) (TITEL(I),I=1,18)                                    LIB  510
C                                                                       LIB  520
      NXT = NXS                                                         LIB  530
      IF(NLB .GT. 0) NXT = NXT + 1                                      LIB  540
      NEXLIB = 64                                                       LIB  550
      NEGNO = -1                                                        LIB  560
      ARB = 1.                                                          LIB  570
      KONUC = NXT * KMAT                                                LIB  580
      NSCATD = N26 - 1                                                  LIB  590
      NSCATU = 0                                                        LIB  600
      IND = 0                                                           LIB  610
      JMAT = KONUC                                                      LIB  620
      DO 4999 J=1,N200                                                  LIB  630
        IZON(J) = 1                                                     LIB  640
 4999 CONTINUE                                                          LIB  650
      IM = 1                                                            LIB  660
      MMAX = 0                                                          LIB  670
   30 CONTINUE                                                          LIB  680
      MMAX = MMAX + 1                                                   LIB  690
      MIX0 = 1                                                          LIB  700
      NREG = NRG(IM)                                                    LIB  710
      IF(NOPOW(IM) .EQ. 0) MIX0 = MIX(NREG)                             LIB  720
      IM = IM + MIX0                                                    LIB  730
      IF(IM .LE. N200) GOTO 30                                          LIB  740
      WRITE (NEXLIB) MMAX,JMAT,(IZON(J),J=1,MMAX),KMAT,NXS,FIWATT,POWER,LIB  750
     1 (MIX(J),J=1,MMAX)                                                LIB  760
      WRITE (NEXLIB) (IMAT(J),J=1,KMAT)                                 LIB  770
      IM = 1                                                            LIB  780
      DO 28 J=1,MMAX                                                    LIB  790
        THB(J) = 0.                                                     LIB  800
        FADO(J) = 0.                                                    LIB  810
        NOP(J) = 0                                                      LIB  820
        NSP(J) = 0                                                      LIB  830
        THBZW = 0.                                                      LIB  840
        FADOZW = 0.                                                     LIB  850
        VOLZW = 0.                                                      LIB  860
        HMZW = 0.                                                       LIB  870
        MIX0 = 1                                                        LIB  880
        IF(NOPOW(IM) .EQ. 0) MIX0 = MIX(J)                              LIB  890
        DO 26 L=1,MIX0                                                  LIB  900
          IZWL = IM - 1 + L                                             LIB  910
          VOLZW = VOLZW + VOL(IZWL)                                     LIB  920
          HMZW = HMZW + HM(IZWL)                                        LIB  930
          THBZW = THBZW + THBURN(IZWL) * HM(IZWL)                       LIB  940
          FADOZW = FADOZW + FADOS3(IZWL) * VOL(IZWL)                    LIB  950
   26   CONTINUE                                                        LIB  960
        IF(NOPOW(IM) .GT. 0) GOTO 27                                    LIB  970
        THB(J) = 0.                                                     LIB  980
        IF(HMZW .NE. 0.) THB(J) = THBZW / HMZW                          LIB  990
        FADO(J) = FADOZW / VOLZW                                        LIB 1000
   27   CONTINUE                                                        LIB 1010
        NOP(J) = NOPOW(IM)                                              LIB 1020
        NSP(J) = NHOT(IM)                                               LIB 1030
        IM = IM + MIX0                                                  LIB 1040
   28 CONTINUE                                                          LIB 1050
      WRITE (NEXLIB) (THB(I),FADO(I),NOP(I),NSP(I),I=1,MMAX)            LIB 1060
      WRITE (NEXLIB) (TEML(1,I),TEML(2,I),I=1,MMAX)                     LIB 1070
      WRITE (NEXLIB) (COSSHM(I),I=1,NXS)                                LIB 1080
      IF(IPRIN(1) .GE. 1) WRITE (N6,HAAS)                               LIB 1090
      J = 0                                                             LIB 1100
      IM = 1                                                            LIB 1110
      IZW = 1                                                           LIB 1120
   40 CONTINUE                                                          LIB 1130
      J = J + 1                                                         LIB 1140
      MIX0 = 1                                                          LIB 1150
      NREG = NRG(IM)                                                    LIB 1160
      IF(NOPOW(IM) .EQ. 0) MIX0 = MIX(NREG)                             LIB 1170
      IM = IM + MIX0                                                    LIB 1180
      DO 21 I=1,JMAT                                                    LIB 1190
        DN(I) = 0.                                                      LIB 1200
   21 CONTINUE                                                          LIB 1210
      DO 25 I=1,JMAT                                                    LIB 1220
        II = (I-1) / KMAT + 1                                           LIB 1230
        JJ = I - KMAT * (II-1)                                          LIB 1240
        DENZW = 0.                                                      LIB 1250
        VOLZW = 0.                                                      LIB 1260
        DO 24 L=1,MIX0                                                  LIB 1270
          IZWL = IZW - 1 + L                                            LIB 1280
          VOLZW = VOLZW + VOL(IZWL)                                     LIB 1290
          D = 0.                                                        LIB 1300
          IF(NHOT(IZWL) .EQ. II) D = DEN(JJ,IZWL)                       LIB 1310
          DENZW = DENZW + D * VOL(IZWL)                                 LIB 1320
   24   CONTINUE                                                        LIB 1330
        DN(I) = DENZW / VOLZW                                           LIB 1340
   25 CONTINUE                                                          LIB 1350
      IZW = IZW + MIX0                                                  LIB 1360
      WRITE (NEXLIB) (DN(I),I=1,JMAT)                                   LIB 1370
      IF(IM .LE. N200) GOTO 40                                          LIB 1380
      IF(IPRIN(1) .LE. 0) GOTO 12                                       LIB 1390
      III = (N200+9) / 10                                               LIB 1400
      I2 = 0                                                            LIB 1410
      DO 3 L=1,III                                                      LIB 1420
        I1 = I2 + 1                                                     LIB 1430
        I2 = I1 + 9                                                     LIB 1440
        WRITE (N6,120) (I,(DEN(I,J),J=I1,I2),I=1,KMAT)                  LIB 1450
    3 CONTINUE                                                          LIB 1460
   12 CONTINUE                                                          LIB 1470
      DO 5 N=2,N26                                                      LIB 1480
        CHI(N) = 0.                                                     LIB 1490
    5 CONTINUE                                                          LIB 1500
      CHI(1) = 1.                                                       LIB 1510
      DO 9 K=1,N26                                                      LIB 1520
        DO 9 J=1,N26                                                    LIB 1530
          SJGR(K,J) = 0.                                                LIB 1540
    9 CONTINUE                                                          LIB 1550
      NO1 = NO + 1                                                      LIB 1560
      YIO(1) = YIELD1(NO1)                                              LIB 1570
      YIO(2) = YIELD2(NO1)                                              LIB 1580
      YIO(3) = YIELD3(NO1)                                              LIB 1590
      YIO(4) = YIELD4(NO1)                                              LIB 1600
      YIO(5) = XLAM(NO1)                                                LIB 1610
      YXE(1) = YIELD1(1) - YIO(1)                                       LIB 1620
      YXE(2) = YIELD2(1) - YIO(2)                                       LIB 1630
      YXE(3) = YIELD3(1) - YIO(3)                                       LIB 1640
      YXE(4) = YIELD4(1) - YIO(4)                                       LIB 1650
      YXE(5) = XLAM(1)                                                  LIB 1660
      WRITE (NEXLIB) (TITEL(I),I=1,18)                                  LIB 1670
      WRITE (NEXLIB) N200,N20,NDR,KROT,IMX,(ICI(I),IVS(I),VFV(I),VFC(I),LIB 1680
     1 I=1,IMX)                                                         LIB 1690
      WRITE (NEXLIB) (VOL(I),I=1,N200)                                  LIB 1700
CFZJ042                                                       09.09.05  LIB 1710
      DU = 0.                                                           LIB 1720
      WRITE (NEXLIB) POWAV,LAYT,(DU,POWL(I),I=1,LAYT)                   LIB 1730
      WRITE (NEXLIB) (POWSUB(I),FADOS3(I),I=1,N200)                     LIB 1740
      REWIND NEXLIB                                                     LIB 1750
      WRITE (N6,4)                                                      LIB 1760
      DO 1 I=1,N200                                                     LIB 1770
        WRITE (N6,2) I,NOPOW(I),NHOT(I),POWSUB(I),THBURN(I),FADOS3(I),  LIB 1780
     1   TEML(1,I),TEML(2,I)                                            LIB 1790
    1 CONTINUE                                                          LIB 1800
      RETURN                                                            LIB 1810
      END                                                               LIB 1820
      SUBROUTINE BURNUP(ABSIG,LA0,FISIG,LF0,XNU,IREG,RPHIAV,SSLUMP,NHOT,BUR   10
     1 VOL,DEN,SS,XXXNEW,YYYNEW,CRNEW,DENIOD,YIELDX,YIELDJ,HM,THBURN,   BUR   20
     2 FADOS3,NOPOW,TEMZUT,TCELS,TFU,TMO,TRAT,NRY,DOR,FIRATE,ABSXE,DSP, BUR   30
     3 ZF,FLX,POWSUB,FX,ASGMA,FSGMA,NEND38,NEND46,IMAT,GS,SN2N,SGMN2N,  BUR   40
     4 HMETAL,MIX,NRG,SGA,SGTR,V1,FKEN,AGEFAC,VREG,VRP)                 BUR   50
C                                                                       BUR   60
CFZJ042                                                       09.09.05  BUR   70
C                                                                       BUR   80
C     DEPLETION AND BUILDUP CALCULATION                                 BUR   90
C                                                                       BUR  100
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    BUR  110
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    BUR  120
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIBUR  130
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D             BUR  140
C                                                                       BUR  150
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1BUR  160
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         BUR  170
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    BUR  180
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)BUR  190
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  BUR  200
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    BUR  210
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         BUR  220
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,BUR  230
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW,CIZET0,    BUR  240
     9 NMAXC,DLAY(101)                                                  BUR  250
C                                                                       BUR  260
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300),FABC(10),REPC(10),NNEBUR  270
     1 ,LISTEQ,NTIK                                                     BUR  280
C                                                                       BUR  290
CFZJ031                                                       28.05.04  BUR  300
CFZJ062                                                       04.05.11  BUR  310
      COMMON /ORIGEN/ LOB,NOR,VOR(100),ISPK,KFISS,N200C,NXSC,KGR,       BUR  320
     1 LOBN,IEZ,FSP(33)                                                 BUR  330
C                                                                       BUR  340
CFZJ035                                                       14.09.04  BUR  350
CFZJ063                                                       26.07.11  BUR  360
      COMMON /GRENZE/ NURTHM,THERGR,IDGAM,IDZUT,IDTHER,NGAM,IDESIN,JJGM,BUR  370
     1 MSTU,MGHUS,NNOBG,NZT(10,9),IZUT(10,2,10),SIRA(68),NIRPI,NID,NDA30BUR  380
     2 ,NKER,ITEMP(20)                                                  BUR  390
C                                                                       BUR  400
      COMMON /SPECTI/ ITIK(10),TI,TO,PO,NTITO,SEARCH,NYS,CRIT0,EPSKTO,  BUR  410
     1 DKDDT,DTIDTO,REFLET,RE0,TO0,DELDAU,TI1,TO1,TRAND,DREFS,NREST     BUR  420
C                                                                       BUR  430
CFZJ006 enlarged dimensions common QVAR                       28.11.03  BUR  440
      COMMON /QVAR/ IVAR,ENDKEF,QVOLLL,QREDUZ,QREMAX,EPQ,EPC,DQDDC,DELC,BUR  450
     1 DCN,SBU,TE(4,300),TA(300),N61,URZ,ZLEKA,ABXEN,TJ(300),DU,DUM,    BUR  460
     2 JRESTW,JRESTR,JREST,TAU,DDU(311),NJ                              BUR  470
C                                                                       BUR  480
      COMMON /VARDIM/ A(8000000)                                        BUR  490
C                                                                       BUR  500
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       BUR  510
C                                                                       BUR  520
CFZJ048 enlarged dimension                                    11.04.07  BUR  530
      COMMON /VARDIT/ B(5000000)                                        BUR  540
C                                                                       BUR  550
CFZJ042                                                       09.09.05  BUR  560
      COMMON /ADDRT/ KX(240),KY(240),LZ(240),NENDPT                     BUR  570
C                                                                       BUR  580
      COMMON /NUCNAM/ T1(200),T2(200),GR(2),PU,FL,MATN(200)             BUR  590
C                                                                       BUR  600
CFZJ044                                                       26.09.05  BUR  610
      COMMON /IFA/ FA0,FA1,FA2,IEND                                     BUR  620
C                                                                       BUR  630
CFZJ055                                                       25.09.07  BUR  640
C                                                                       BUR  650
      COMMON /DECAYT/ DECY(30)                                          BUR  660
C                                                                       BUR  670
      COMMON /VSDA4/ DX(263),RFAST                                      BUR  680
C                                                                       BUR  690
      COMMON /FLUXN/ DUMM(361),IACT                                     BUR  700
C                                                                       BUR  710
      COMMON /SFFLUX/ SFF                                               BUR  720
C                                                                       BUR  730
CFZJ031                                                       28.05.04  BUR  740
      COMMON /VSDATA/ NRKONZ,KFI,N2,DELT,NB10,KM,AKONZ(30),MATNR(30),   BUR  750
     1 NR(30),FLAX(33),THERMA(30),RESA(30),FASTV(30),THERMF(30),RESF(30)BUR  760
     2 ,FASTF(30),FASTAB(30),SIGMA(33,30),SIGMF(33,30),SIGMN2(33,30)    BUR  770
C                                                                       BUR  780
      COMMON /EQ/ XZERO(120),XZH(120),XTEMP(120),XNEW(120)              BUR  790
C                                                                       BUR  800
      COMMON /STA/ IST,SB,TEX,JNS,DUN(5),POWAV,POWL(10000),LAYT         BUR  810
C                                                                       BUR  820
      COMMON /BUC / BU(6,200)                                           BUR  830
C                                                                       BUR  840
CFZJ058                                                       05.11.08  BUR  850
      COMMON /KEFFT/ TKEFF(5000),RKEFF(5000),IPKEFF,NKEFF,TKUM,POWLMAX, BUR  860
     1 THETMX,POWLM(5000),POWB(5000),BUSCRAP(5000),THETM(5000)          BUR  870
C                                                                       BUR  880
      COMMON /FLEX/ DUMMY(6),ERR                                        BUR  890
C                                                                       BUR  900
      COMMON /PDTX/ PDTHX                                               BUR  910
C                                                                       BUR  920
CFZJ053                                                       19.07.07  BUR  930
      COMMON /FIW/ FIWISO                                               BUR  940
C                                                                       BUR  950
CFZJ042                                                       09.09.05  BUR  960
      EQUIVALENCE(JTPE3,NT),(DIRAC(1,5),XLAM(1)),(LI(4),LTOSI),         BUR  970
     1 (LI(8),LUMRE),(LI(68),LANU),(LI(69),LA1),(LI(70),LA2),           BUR  980
     2 (LI(71),LA3),(LI(72),LA4),(LI(73),LA5),(LI(91),LSGA),            BUR  990
     3 (LI(92),LSGTR),(LI(93),LV1),(LI(94),LFKEN),(LI(3),LOUSI),        BUR 1000
     4 (LI(145),LCOSS),(LZ(8),ITEML),(LZ(29),IRADP),(LZ(30),IRAD),      BUR 1010
     5 (LZ(31),IPHI),(LZ(32),INKOM),(LZ(33),IKOMP),(LZ(34),IKART),      BUR 1020
     6 (LZ(35),IT),(LZ(36),IWI),(LZ(37),IWT),(LZ(38),ITQ),(LZ(39),IIFBE)BUR 1030
     7 ,(LZ(43),IIFBH),(LZ(44),IKKB),(LZ(45),INHZO),(LZ(46),IIKO),      BUR 1040
     8 (LZ(47),IQX),(LZ(220),ISTRA)                                     BUR 1050
C                                                                       BUR 1060
      CHARACTER*4 T1,T2,GR,PU,FL,BU                                     BUR 1070
C                                                                       BUR 1080
      CHARACTER*6 IDENTI/'vsop99'/                                      BUR 1090
CFZJ011 Increase dimensions of arrays SGA, SGTR, FKEN         01.12.03  BUR 1100
CFZJ042                                                       09.09.05  BUR 1110
CFZJ062                                                       04.05.11  BUR 1120
      DIMENSION XLAM(49),YT(49),ODEN(49),ZFS(5),JREG(20),BB(4),JA(30),  BUR 1130
     1 JF(30),AB(30),CA(30),FI(30),Z(4),ABSIG(LA0),FISIG(LF0),XNU(LF0), BUR 1140
     2 IREG(NDR),RPHIAV(N26,NDR),SSLUMP(N26,NDR),NHOT(N200),VOL(N200),  BUR 1150
     3 DEN(KMAT,N200),SS(N26,N200),XXXNEW(N200),YYYNEW(N200),CRNEW(N200)BUR 1160
     4 ,DENIOD(N200),YIELDX(N200),YIELDJ(N200),HM(N200),THBURN(N200),   BUR 1170
     5 FADOS3(N200),NOPOW(N200),TEMZUT(NXS),TCELS(5,NXS),TFU(NXS),      BUR 1180
     6 TMO(NXS),TRAT(NXS),NRY(N200),DOR(100,KMAT),FIRATE(N200),         BUR 1190
     7 ABSXE(N200),DSP(NXS),ZF(8,N200),FLX(N26,N200),POWSUB(N200),      BUR 1200
     8 FX(N26),GS(IACT),ASGMA(N26,KMAT),FSGMA(N26,KFISS),IMAT(KMAT),    BUR 1210
     9 SN2N(LF0),SGMN2N(N26,KFISS),HMETAL(N200),MIX(NDR),NRG(N200),     BUR 1220
     X SGA(20,N26),SGTR(20,N26,N26),V1(201,N26,3),FKEN(20,N26),         BUR 1230
     Y AGEFAC(N200),VREG(NDR),VRP(NDR)                                  BUR 1240
C                                                                       BUR 1250
C     SPALTUNGEN PRO WATT * SEC FUER:                                   BUR 1260
C     1) U-233*, 2) U-235+, 3) PU-239+, 4) PU-241+, 5) U-238+ UND ANDEREBUR 1270
C                                                                       BUR 1280
C     * = BNL-NCS-51363 VON 1981                                        BUR 1290
C     + = DIN 25485 VON 1990                                            BUR 1300
C                                                                       BUR 1310
CFZJ053                                                       19.07.07  BUR 1320
      REAL*4 FIWISO(5)/3.15231E+10,3.08675E+10,2.95522E+10,2.92064E+10, BUR 1330
     1 3.03719E+10/                                                     BUR 1340
C                                                                       BUR 1350
    1 FORMAT (13H1   TIME STEP,I3//72H      BATCH     -----------FRACTIOBUR 1360
     1N OF FISSIONS FROM--------------------/19X,6H U-233,9X,6H U-235,8XBUR 1370
     2 ,7H PU-239,8X,7H PU-241,16X,'FISSILE FISSIONS/TOTAL FISSIONS'//) BUR 1380
    2 FORMAT (I10,1PE17.5,3(1PE15.5),23X,E12.5)                         BUR 1390
    3 FORMAT ('0'////'      BATCH    VOLUME         CONV RATIO         PBUR 1400
     1OWER          BURNUP           FASTDOSE'/'                 CM3    BUR 1410
     2                       BATCH/AVG         MWD/T             1/CM2'/BUR 1420
     3/)                                                                BUR 1430
    4 FORMAT (I10,5G16.6,8X,I4,G16.6)                                   BUR 1440
    5 FORMAT ('0AVERAGE :',16X,2G16.6,' MW/M3',G16.6,' MW/TO.HM'//' POWEBUR 1450
     1R GENERATING VOLUME =',G16.6,' CM3          POWER PRODUCED IN TIMEBUR 1460
     2 STEP ',G16.6,' MWD')                                             BUR 1470
    7 FORMAT (/' *** DATA FOR THERMIX HAVE BEEN PREPARED. ***')         BUR 1480
    8 FORMAT (I10,8G15.6/(10X,8G15.6))                                  BUR 1490
    9 FORMAT (//' TIME INTERVAL FROM STEP',I3,' TO',I3)                 BUR 1500
   13 FORMAT ('1'/' SPEK.ZONE  TEMP.RES.ABS.    TEMP.MOD.'/)            BUR 1510
   15 FORMAT (I6,2(8X,F7.0))                                            BUR 1520
   18 FORMAT (' CYLINDER ZONE',10X,'INTEGRATED POWER'/28X,'ZONE/AVG'/)  BUR 1530
   21 FORMAT (I10,14X,G16.6)                                            BUR 1540
   29 FORMAT ('0'////'      BATCH    VOLUME         CONV RATIO         PBUR 1550
     1OWER          BURNUP           FASTDOSE',8X,'REGION     POWER'/'  BUR 1560
     2               CM3                           BATCH/AVG         MWDBUR 1570
     3/T             1/CM2',18X,'REGION/AVG'//)                         BUR 1580
   33 FORMAT (/10X,8G15.6)                                              BUR 1590
   38 FORMAT (//' ** W A R N I N G : **'/' ---------------------'/' SUM BUR 1600
     1OF POWER GENERATING BATCH-VOLUMES IS NOT EQUAL TO VOLUME OF ACTIVEBUR 1610
     2 CORE (',E13.6,').'/' CHECK THE INPUT-ITEMS *PARVL1* (CARD R3) ANDBUR 1620
     3 *R2* (CARD R21).'//)                                             BUR 1630
   53 FORMAT (/' OVER ALL CORE FISSILE FRACTIONS:'/20X,'U-233',10X,'U-23BUR 1640
     15',9X,'PU-239',9X,'PU-241',24X,'OTHER ACTINIDES'//15X,1PE12.5,    BUR 1650
     2 3(1PE15.5),23X,E12.5//' FIWATT (FISS./ENERGY (WS)) :',1PE13.5)   BUR 1660
   90 FORMAT (1H1)                                                      BUR 1670
  500 FORMAT (/' *** ORIGEN-LIBRARY OK. ***'/)                          BUR 1680
  920 FORMAT (/' -----   CONVERSION-RATIO AVERAGE IN TIME STEP : CAVNEW BUR 1690
     1=',G14.6,'    -----'//)                                           BUR 1700
 2222 FORMAT (/1X,F4.0,'SET ON UNIT 60, TIME INTERVAL',I3,' CYCLE',I4,' BUR 1710
     1ITEST = ',I2)                                                     BUR 1720
 7000 FORMAT (12I6)                                                     BUR 1730
 7001 FORMAT (6E12.5)                                                   BUR 1740
 7002 FORMAT (A6)                                                       BUR 1750
 9009 FORMAT (' POWER: MIN/AV=',E13.5,' IN BATCH ',I4,', MAX/AV=',E13.5,BUR 1760
     1 ' IN ',I4)                                                       BUR 1770
 9010 FORMAT (//'      REGION',5X,A4,I2,7(9X,A4,I2)/(17X,A4,I2,7(9X,A4, BUR 1780
     1 I2)))                                                            BUR 1790
 9011 FORMAT (/)                                                        BUR 1800
C                                                                       BUR 1810
      NAMELIST /DKDT/ ITIK,DELDAY,REACT,DKDDT,TI1,TO1,RE0,TO0           BUR 1820
C                                                                       BUR 1830
C                                                                       BUR 1840
      N2 = N26                                                          BUR 1850
      KM = KMAT                                                         BUR 1860
      KFI = KFISS                                                       BUR 1870
      DO 406 I=1,IACT                                                   BUR 1880
        MATNR(I) = MATN(I)                                              BUR 1890
  406 CONTINUE                                                          BUR 1900
      IF(IPRIN(15) .EQ. 0) NTIK = 0                                     BUR 1910
      N38 = 38                                                          BUR 1920
      JO = JN                                                           BUR 1930
      JN = JN + 1                                                       BUR 1940
      IF(LOB .EQ. JN .AND. LOB .NE. 0) JNUM = 1                         BUR 1950
      DELDAU = 0.                                                       BUR 1960
   80 CONTINUE                                                          BUR 1970
      IF(ITIK(10) .LT. 2) GOTO 82                                       BUR 1980
      DELDAX = 0.                                                       BUR 1990
      IF(ITIK(10) .NE. 3) GOTO 81                                       BUR 2000
      ITIK(10) = 2                                                      BUR 2010
      GOTO 82                                                           BUR 2020
C                                                                       BUR 2030
C     MINISCHRITT                                                       BUR 2040
C                                                                       BUR 2050
   81 CONTINUE                                                          BUR 2060
      DAYMIN = 1. / 1440.                                               BUR 2070
      DELDAX = DELDAY                                                   BUR 2080
      DELDAY = DAYMIN                                                   BUR 2090
      JNDAU = JNUM                                                      BUR 2100
      JNUM = 1                                                          BUR 2110
      ITIK(10) = 3                                                      BUR 2120
      CK = REACT(2)                                                     BUR 2130
      GS5 = GS(4)                                                       BUR 2140
      GS9 = GS(16)                                                      BUR 2150
      JNS = 0                                                           BUR 2160
      IF(JN .EQ. JNSTOP) JNS = 1                                        BUR 2170
C                                                                       BUR 2180
      IF(NJ .GT. 1) CALL QFIX(CK,DELDAX,GS5,GS9)                        BUR 2190
C                                                                       BUR 2200
   82 CONTINUE                                                          BUR 2210
      IF(NTIK .LE. 0) GOTO 79                                           BUR 2220
      IF(ITIK(5) .LE. 0) GOTO 79                                        BUR 2230
      IT20 = ITEMP(20)                                                  BUR 2240
      IF(IT20 .EQ. 0) GOTO 70                                           BUR 2250
      DO 71 J=1,IT20                                                    BUR 2260
        IF(ITEMP(J) .EQ. JN) GOTO 70                                    BUR 2270
   71 CONTINUE                                                          BUR 2280
      GOTO 79                                                           BUR 2290
C                                                                       BUR 2300
C     PERFORM TIK CALCULATION                                           BUR 2310
C                                                                       BUR 2320
   70 CONTINUE                                                          BUR 2330
      IF(ITIK(5) .LE. 1) GOTO 74                                        BUR 2340
      DKDDT = (RE0-REACT(2)) / (TO0-TO)                                 BUR 2350
   74 CONTINUE                                                          BUR 2360
      TO0 = TO                                                          BUR 2370
      RE0 = REACT(2)                                                    BUR 2380
      R2C = CRIT0 - RE0                                                 BUR 2390
      IF(ABS(R2C) .GT. EPSKTO) GOTO 73                                  BUR 2400
      ITIK(5) = 1                                                       BUR 2410
      GOTO 79                                                           BUR 2420
C                                                                       BUR 2430
C     BEREITEN DES KLEINEN ZEITSCHRITTES                                BUR 2440
C                                                                       BUR 2450
   73 CONTINUE                                                          BUR 2460
      TO1 = TO + R2C / DKDDT                                            BUR 2470
   75 CONTINUE                                                          BUR 2480
      TI1 = TI + (TO1-TO0) * DTIDTO                                     BUR 2490
      IF(TI1 .GT. 1.) GOTO 76                                           BUR 2500
      TO1 = TO1 + 1.                                                    BUR 2510
      GOTO 75                                                           BUR 2520
   76 CONTINUE                                                          BUR 2530
      DO 72 I=1,NXS                                                     BUR 2540
        IF(REFLET .EQ. 0.) GOTO 72                                      BUR 2550
        TCELS(1,I) = TI1 + (TO1-TI1) * TRAT(I)                          BUR 2560
        IF(DSP(I) .NE. 0.) TCELS(1,I) = TRAND + (TCELS(1,I)-TRAND) *    BUR 2570
     1   DSP(I) / DREFS                                                 BUR 2580
   72 CONTINUE                                                          BUR 2590
      JN = JN - 1                                                       BUR 2600
      ITIK(5) = ITIK(5) + 1                                             BUR 2610
      DELDAU = DELDAY                                                   BUR 2620
      DELDAY = DELDAY / 1440.                                           BUR 2630
   79 CONTINUE                                                          BUR 2640
      IF(ITIK(5) .GT. 0) WRITE (6,DKDT)                                 BUR 2650
      INZW(2) = JN                                                      BUR 2660
      IPRIN(3) = PRO(174)                                               BUR 2670
C                                                                       BUR 2680
C     JINT<0 BERECHNUNG WAEHREND EINES XE-TRANSIENTEN                   BUR 2690
C                                                                       BUR 2700
      IF(JN .EQ. INZW(1)) INZW(4) = JN                                  BUR 2710
      INZWX = 0                                                         BUR 2720
      IF(INZW(1) .NE. 0 .AND. INZW(1) .EQ. INZW(2)) INZWX = 1           BUR 2730
      IF(INZWX .GT. 0) INZWXX = INZWX                                   BUR 2740
      IF(INZWX .GT. 0 .AND. IPRIN(3) .LE. 0) IPRIN(3) = 1               BUR 2750
      IF(IPRIN(3) .LE. 1) WRITE (NT,9) JO,JN                            BUR 2760
      ZJNUM = JNUM                                                      BUR 2770
      DELSEC = 0.0                                                      BUR 2780
      IF(JN .LE. 0) GOTO 24                                             BUR 2790
      DELSEC = DELDAY * 8.64E+4 / ZJNUM                                 BUR 2800
      XPPNEW(JN) = 0.                                                   BUR 2810
      YPPNEW(JN) = 0.                                                   BUR 2820
   24 JNN = 0                                                           BUR 2830
      WATT = DELSEC * 1.0E+20 / (8.64*FIWATT)                           BUR 2840
      OUTPUT = 0.                                                       BUR 2850
   25 CONTINUE                                                          BUR 2860
      MUHU(2) = 1                                                       BUR 2870
      JNN = JNN + 1                                                     BUR 2880
      NFUMM = 0                                                         BUR 2890
C                                                                       BUR 2900
      CALL CITA(2,A(KA(LSGA)),A(KA(LSGTR)),A(KA(LV1)),A(KA(LFKEN)),NFUMMBUR 2910
     1 ,SS)                                                             BUR 2920
C                                                                       BUR 2930
      MUHU(2) = 0                                                       BUR 2940
      XXPNEW = 0.0                                                      BUR 2950
      YYPNEW = 0.0                                                      BUR 2960
      POWAV = 0.0                                                       BUR 2970
      KR = 1                                                            BUR 2980
      IRRE2 = IREG(KR)                                                  BUR 2990
      IR = 0                                                            BUR 3000
C                                                                       BUR 3010
C     PERFORM CALCULATIONS FOR ALL SUBREGIONS                           BUR 3020
C                                                                       BUR 3030
      IF(IPRIN(3) .GE. 1 .AND. JNN .EQ. JNUM) WRITE (NT,1) JN           BUR 3040
   30 IR = IR + 1                                                       BUR 3050
      IF(IR .LE. IRRE2) GOTO 35                                         BUR 3060
      KR = KR + 1                                                       BUR 3070
      IRRE2 = IREG(KR)                                                  BUR 3080
   35 CONTINUE                                                          BUR 3090
      DO 44 J=1,IACT+1                                                  BUR 3100
        IF(IR .EQ. 1) GOTO 40                                           BUR 3110
        IF(NHOT(IR) .EQ. NHOT(IR-1)) GOTO 45                            BUR 3120
   40   CONTINUE                                                        BUR 3130
        IF(J .GT. 1) GOTO 43                                            BUR 3140
        LA = (NHOT(IR)-1) * KMAT                                        BUR 3150
        LF = (NHOT(IR)-1) * IACT                                        BUR 3160
   43   CONTINUE                                                        BUR 3170
        JA(J) = (LA+J-1) * N26                                          BUR 3180
        IF(J .GT. IACT) GOTO 45                                         BUR 3190
        JF(J) = (LF+J-1) * N26                                          BUR 3200
   45   CONTINUE                                                        BUR 3210
        AB(J) = 0.                                                      BUR 3220
        CA(J) = 0.                                                      BUR 3230
   44 CONTINUE                                                          BUR 3240
      ZF1 = 0.                                                          BUR 3250
      ZF2 = 0.                                                          BUR 3260
      ZF3 = 0.                                                          BUR 3270
      ZF4 = 0.                                                          BUR 3280
      ZF5 = 0.                                                          BUR 3290
      XYZ = 0.                                                          BUR 3300
      XYZT = 0.                                                         BUR 3310
      DO 50 IE=1,N26                                                    BUR 3320
        SSFAC = SS(IE,IR) * RPHIAV(IE,KR)                               BUR 3330
        IF(JNN .EQ. JNUM) FLX(IE,IR) = SSFAC * 1.E+24                   BUR 3340
        DO 55 J=1,IACT+1                                                BUR 3350
C                                                                       BUR 3360
C     ABSORPTION RATE                                                   BUR 3370
C                                                                       BUR 3380
          AB(J) = AB(J) + ABSIG(IE+JA(J)) * SSFAC                       BUR 3390
          IF(J .GT. IACT) GOTO 55                                       BUR 3400
C                                                                       BUR 3410
C     CAPTURE RATE                                                      BUR 3420
C                                                                       BUR 3430
          IF(XNU(IE+JF(J)) .EQ. 0.) GOTO 301                            BUR 3440
          CA(J) = CA(J) + (ABSIG(IE+JA(J))-FISIG(IE+JF(J))/             BUR 3450
     1     XNU(IE+JF(J))) * SSFAC                                       BUR 3460
          GOTO 55                                                       BUR 3470
  301     CONTINUE                                                      BUR 3480
          CA(J) = CA(J) + ABSIG(IE+JA(J)) * SSFAC                       BUR 3490
   55   CONTINUE                                                        BUR 3500
        FLAX(IE) = SS(IE,IR)                                            BUR 3510
   50 CONTINUE                                                          BUR 3520
      IF(NOPOW(IR) .GT. 0 .OR. HMETAL(IR) .LE. 0.) GOTO 801             BUR 3530
      RFAST = SFF / FSP(1)                                              BUR 3540
C                                                                       BUR 3550
C     DENSITIES BEFORE ADAGE                                            BUR 3560
C                                                                       BUR 3570
      DO 401 IO=1,IACT                                                  BUR 3580
        XZERO(IO) = DEN(IO,IR)                                          BUR 3590
  401 CONTINUE                                                          BUR 3600
C                                                                       BUR 3610
C     CROSS SECTIONS (ABSORPTION, FISSION AND N-2N)                     BUR 3620
C                                                                       BUR 3630
      DELT = DELSEC                                                     BUR 3640
      INSP = NHOT(IR) - 1                                               BUR 3650
      DO 302 NUC=1,IACT                                                 BUR 3660
        I1 = (INSP*KMAT+NUC-1) * N26                                    BUR 3670
        DO 302 IG=1,N26                                                 BUR 3680
          I2 = I1 + IG                                                  BUR 3690
          SIGMA(IG,NUC) = ABSIG(I2)                                     BUR 3700
  302 CONTINUE                                                          BUR 3710
      DO 303 NUC=1,IACT                                                 BUR 3720
        I1 = (INSP*IACT+NUC-1) * N26                                    BUR 3730
        DO 303 IG=1,N26                                                 BUR 3740
          I2 = I1 + IG                                                  BUR 3750
          XN = 1.                                                       BUR 3760
          IF(XNU(I2) .GT. 0.) XN = XNU(I2)                              BUR 3770
          SIGMF(IG,NUC) = FISIG(I2) / XN                                BUR 3780
          SIGMN2(IG,NUC) = SN2N(I2)                                     BUR 3790
  303 CONTINUE                                                          BUR 3800
      NB10 = 0                                                          BUR 3810
      JSBTS = JNN                                                       BUR 3820
C                                                                       BUR 3830
C     CALL FOR BURNUP-PART (ADAGE)                                      BUR 3840
C                                                                       BUR 3850
      CALL ADAGE(IR,JSBTS,NT)                                           BUR 3860
C                                                                       BUR 3870
      DO 311 IO=1,IACT                                                  BUR 3880
C                                                                       BUR 3890
C     NEW DENSITIES AFTER ADAGE                                         BUR 3900
C                                                                       BUR 3910
        DEN(IO,IR) = XNEW(IO)                                           BUR 3920
C                                                                       BUR 3930
C     FISSION RATE                                                      BUR 3940
C                                                                       BUR 3950
        FI(IO) = AB(IO) - CA(IO)                                        BUR 3960
        IF(FI(IO) .LT. 0.) FI(IO) = 0.                                  BUR 3970
  311 CONTINUE                                                          BUR 3980
C                                                                       BUR 3990
C     TOTAL FISSION DENSITY IN SUBREGION                                BUR 4000
C                                                                       BUR 4010
      DO 150 I=1,IACT                                                   BUR 4020
        AE = (DEN(I,IR)+XZERO(I)) / 2. / ERR                            BUR 4030
        AE = AE * FI(I)                                                 BUR 4040
        IF(AE .LT. 1.) AE = 0.                                          BUR 4050
        XYZ = XYZ + AE * ERR                                            BUR 4060
  150 CONTINUE                                                          BUR 4070
C                                                                       BUR 4080
C     FRACTION OF FISSIONS FROM U233 U235 PU239 AND PU241               BUR 4090
C                                                                       BUR 4100
      ZF1 = (DEN(4,IR)+XZERO(4)) / 2. / ERR * FI(4)                     BUR 4110
      ZF2 = (DEN(6,IR)+XZERO(6)) / 2. / ERR * FI(6)                     BUR 4120
      ZF3 = (DEN(16,IR)+XZERO(16)) / 2. / ERR * FI(16)                  BUR 4130
      ZF4 = (DEN(18,IR)+XZERO(18)) / 2. / ERR * FI(18)                  BUR 4140
      XYZT = ZF1 + ZF2 + ZF3 + ZF4                                      BUR 4150
      IF(XYZT .EQ. 0.0) GOTO 8888                                       BUR 4160
      ZF1 = ZF1 / XYZT                                                  BUR 4170
      ZF2 = ZF2 / XYZT                                                  BUR 4180
      ZF3 = ZF3 / XYZT                                                  BUR 4190
      ZF4 = ZF4 / XYZT                                                  BUR 4200
      XYZT = XYZT * ERR                                                 BUR 4210
      R1 = (DEN(1,IR)+XZERO(1)) * CA(1) * .5 / XYZT                     BUR 4220
      R2 = (DEN(9,IR)+XZERO(9)) * CA(9) * .5 / XYZT                     BUR 4230
 8888 CONTINUE                                                          BUR 4240
      IF(XYZ .GT. 0.) ZF5 = XYZT / XYZ                                  BUR 4250
      IF(JNN-JNUM) 124,120,120                                          BUR 4260
  120 CONTINUE                                                          BUR 4270
      Z(1) = ZF1 * ZF5                                                  BUR 4280
      Z(2) = ZF2 * ZF5                                                  BUR 4290
      Z(3) = ZF3 * ZF5                                                  BUR 4300
      Z(4) = ZF4 * ZF5                                                  BUR 4310
      IF(IPRIN(3) .GE. 1) WRITE (NT,2) IR,(Z(I),I=1,4),ZF5              BUR 4320
      ZF(1,IR) = ZF1                                                    BUR 4330
      ZF(2,IR) = ZF2                                                    BUR 4340
      ZF(3,IR) = ZF3                                                    BUR 4350
      ZF(4,IR) = ZF4                                                    BUR 4360
      ZF(5,IR) = ZF5                                                    BUR 4370
      ZF(6,IR) = R1                                                     BUR 4380
      ZF(7,IR) = R2                                                     BUR 4390
      ZF(8,IR) = ZF5                                                    BUR 4400
C                                                                       BUR 4410
C     CALCULATIONS FOR XE135 AND NON-SATURATING FISSION PRODUCT         BUR 4420
C                                                                       BUR 4430
  124 CONTINUE                                                          BUR 4440
      DO 125 K=1,2                                                      BUR 4450
        YT(K) = YIELD1(K) * ZF1 + YIELD2(K) * ZF2 + YIELD3(K) * ZF3 +   BUR 4460
     1   YIELD4(K) * ZF4                                                BUR 4470
  125 CONTINUE                                                          BUR 4480
      NO3 = NO + 3                                                      BUR 4490
      YIELDX(IR) = YT(1)                                                BUR 4500
      YIELDJ(IR) = YIELD1(NO3) * ZF1 + YIELD2(NO3) * ZF2 + YIELD3(NO3) *BUR 4510
     1 ZF3 + YIELD4(NO3) * ZF4                                          BUR 4520
      FIRATE(IR) = XYZ                                                  BUR 4530
      ABSXE(IR) = AB(IACT+1)                                            BUR 4540
      ODEN(1) = DEN(IACT+1,IR)                                          BUR 4550
      IRR = IR                                                          BUR 4560
C                                                                       BUR 4570
      CALL XENON(IRR,DEN,DENIOD,YIELDX,YIELDJ,FIRATE,ABSXE)             BUR 4580
C                                                                       BUR 4590
      ODEN(2) = DEN(IACT+2,IR)                                          BUR 4600
      DEN(IACT+2,IR) = ODEN(2) + YT(2) * DELSEC * XYZ                   BUR 4610
      IF(NO) 210,210,200                                                BUR 4620
C                                                                       BUR 4630
C     CALCULATIONS FOR TIME DEPENDANT FISSION PRODUCT CHAINS            BUR 4640
C                                                                       BUR 4650
  200 NK = IACT + 3                                                     BUR 4660
      NL = NK - 1 + NO                                                  BUR 4670
      DO 205 L=NK,NL                                                    BUR 4680
        IL = (LA+L-1) * N26                                             BUR 4690
        ILA = (LA+L-2) * N26                                            BUR 4700
        Y16 = 0.0                                                       BUR 4710
        Y16A = 0.0                                                      BUR 4720
        DO 116 IE=1,N26                                                 BUR 4730
          Y16 = Y16 + RPHIAV(IE,KR) * ABSIG(IE+IL) * SS(IE,IR)          BUR 4740
  116   CONTINUE                                                        BUR 4750
        K = L - IACT - 2                                                BUR 4760
        KFU = K + 2                                                     BUR 4770
        YT(K) = YIELD1(KFU) * ZF1 + YIELD2(KFU) * ZF2 + YIELD3(KFU) *   BUR 4780
     1   ZF3 + YIELD4(KFU) * ZF4                                        BUR 4790
        ODEN(KFU) = DEN(L,IR)                                           BUR 4800
        SPLAT = YT(K) * XYZ                                             BUR 4810
        DO 10 I=1,4                                                     BUR 4820
          J = L - I                                                     BUR 4830
          IF(DIRAC(KFU,I)) 11,10,12                                     BUR 4840
   12     Y16A = 0.0                                                    BUR 4850
          ILA = (LA+J-1) * N26                                          BUR 4860
          DO 14 IE=1,N26                                                BUR 4870
            Y16A = Y16A + RPHIAV(IE,KR) * ABSIG(IE+ILA) * SS(IE,IR)     BUR 4880
   14     CONTINUE                                                      BUR 4890
          SPLAT = SPLAT + 0.5 * (DEN(J,IR)+ODEN(J-IACT)) * Y16A *       BUR 4900
     1     DIRAC(KFU,I)                                                 BUR 4910
          GOTO 10                                                       BUR 4920
   11     KK = KFU - I                                                  BUR 4930
          SPLAT = SPLAT - 0.5 * (DEN(J,IR)+ODEN(J-IACT)) * DIRAC(KFU,I) BUR 4940
     1     * XLAM(KK)                                                   BUR 4950
   10   CONTINUE                                                        BUR 4960
        DEVIDE = Y16 + XLAM(KFU)                                        BUR 4970
        DEDE = DEVIDE * DELSEC                                          BUR 4980
        EXDD = EXP(-DEDE)                                               BUR 4990
        D1 = DEN(L,IR) * EXDD                                           BUR 5000
        IF(DEDE .LT. 0.001) GOTO 206                                    BUR 5010
        D2 = (1.-EXDD) / DEVIDE                                         BUR 5020
        GOTO 207                                                        BUR 5030
  206   D2 = DELSEC * (1.-DEDE*(0.5-DEDE/6.))                           BUR 5040
  207   CONTINUE                                                        BUR 5050
        DEN(L,IR) = D1 + SPLAT * D2                                     BUR 5060
  205 CONTINUE                                                          BUR 5070
  210 CONTINUE                                                          BUR 5080
      IF(NLUM) 230,230,220                                              BUR 5090
C                                                                       BUR 5100
C     CALCULATIONS FOR ABSORBER ROD MATERIALS (LUMPED POISONS)          BUR 5110
C                                                                       BUR 5120
  220 NK = IACT + 3 + NO                                                BUR 5130
      NL = NK - 1 + NLUM                                                BUR 5140
      KREG = KR                                                         BUR 5150
      IRSUB = IR                                                        BUR 5160
C                                                                       BUR 5170
      CALL LUMP(A(KA(LTOSI)),KL(LTOSI),A(KA(LUMRE)),SSLUMP,NHOT,DEN,    BUR 5180
     1 A(KA(LANU)),A(KA(LA1)),A(KA(LA2)),A(KA(LA3)),A(KA(LA4)),         BUR 5190
     2 A(KA(LA5)))                                                      BUR 5200
C                                                                       BUR 5210
      DO 225 L=NK,NL                                                    BUR 5220
        IL = (LA+L-1) * N26                                             BUR 5230
        Y17 = 0.0                                                       BUR 5240
        DO 117 IE=1,N26                                                 BUR 5250
          Y17 = Y17 + RPHIAV(IE,KR) * ABSIG(IE+IL) * SSLUMP(IE,IR) *    BUR 5260
     1     SS(IE,IR)                                                    BUR 5270
  117   CONTINUE                                                        BUR 5280
        DEN(L,IR) = DEN(L,IR) * EXP(-Y17*DELSEC)                        BUR 5290
  225 CONTINUE                                                          BUR 5300
  230 CONTINUE                                                          BUR 5310
C                                                                       BUR 5320
C     CONVERSION RATIO CALCULATIONS                                     BUR 5330
C                                                                       BUR 5340
      XXX = (DECY(3)/ERR*DEN(3,IR)+CA(5)/ERR*DEN(5,IR)+DECY(13)/ERR*    BUR 5350
     1 DEN(13,IR)+CA(17)/ERR*DEN(17,IR)) * ERR * VOL(IR)                BUR 5360
      XXXNEW(IR) = XXX                                                  BUR 5370
      YYYNEW(IR) = (DEN(4,IR)/ERR*AB(4)+DEN(6,IR)/ERR*AB(6)+DEN(16,IR)/ BUR 5380
     1 ERR*AB(16)+DEN(18,IR)/ERR*AB(18)) * ERR * VOL(IR)                BUR 5390
      XXPNEW = XXPNEW + XXXNEW(IR)                                      BUR 5400
      YYPNEW = YYPNEW + YYYNEW(IR)                                      BUR 5410
      CRNEW(IR) = 0.                                                    BUR 5420
      IF(YYYNEW(IR) .NE. 0.) CRNEW(IR) = XXXNEW(IR) / YYYNEW(IR)        BUR 5430
  801 CONTINUE                                                          BUR 5440
C                                                                       BUR 5450
C     FAST NEUTRON DOSE                                                 BUR 5460
C                                                                       BUR 5470
      FADOS3(IR) = FADOS3(IR) + RPHIAV(1,KR) * DELSEC * 1.0E+24 *       BUR 5480
     1 SS(1,IR)                                                         BUR 5490
C                                                                       BUR 5500
C     THERMAL BURNUP                                                    BUR 5510
C                                                                       BUR 5520
      IF(XYZ .LE. 0. .OR. HM(IR) .EQ. 0.) GOTO 800                      BUR 5530
      W = WATT * XYZ * VOL(IR)                                          BUR 5540
      OUTPUT = OUTPUT + W                                               BUR 5550
      THBURN(IR) = THBURN(IR) + W / HM(IR)                              BUR 5560
C                                                                       BUR 5570
C     POWER IN BATCH                                                    BUR 5580
C                                                                       BUR 5590
  800 CONTINUE                                                          BUR 5600
      POWSUB(IR) = XYZ / FIWATT * 1.0E+18                               BUR 5610
      IF(NOPOW(IR) .GT. 0 .OR. HMETAL(IR) .LE. 0.) POWSUB(IR) = 0.      BUR 5620
      POWAV = POWAV + POWSUB(IR) * VOL(IR)                              BUR 5630
      IF(IR .LT. N200) GOTO 30                                          BUR 5640
      HMRAT = POWAV / HMTOT                                             BUR 5650
      POWAV = POWAV / VOLUME                                            BUR 5660
      DO 1210 IR=1,N200                                                 BUR 5670
        POWSUB(IR) = POWSUB(IR) / POWAV                                 BUR 5680
C       IF(VOL(IR) .LT. 113.) POWSUB(IR) = 0.                           BUR 5690
 1210 CONTINUE                                                          BUR 5700
C                                                                       BUR 5710
C     BERECHNUNG GEMITTELTER KONVERSIONSRATEN IM ZEITSCHRITT JN         BUR 5720
C                                                                       BUR 5730
      XPNEW2 = XXPNEW                                                   BUR 5740
      YPNEW2 = YYPNEW                                                   BUR 5750
      IF(JN .LT. 1) GOTO 990                                            BUR 5760
      IF(JN .LE. 1 .AND. JNN .EQ. 1) GOTO 950                           BUR 5770
  910 CONTINUE                                                          BUR 5780
      XPPNEW(JN) = XPPNEW(JN) + (XPNEW1+XPNEW2) / 2.                    BUR 5790
      YPPNEW(JN) = YPPNEW(JN) + (YPNEW1+YPNEW2) / 2.                    BUR 5800
      GOTO 990                                                          BUR 5810
  950 CONTINUE                                                          BUR 5820
      IF(NXE .EQ. 0) GOTO 910                                           BUR 5830
      XPPNEW(JN) = XPNEW2                                               BUR 5840
      YPPNEW(JN) = YPNEW2                                               BUR 5850
  990 CONTINUE                                                          BUR 5860
      XPNEW1 = XPNEW2                                                   BUR 5870
      YPNEW1 = YPNEW2                                                   BUR 5880
      IF(JNN-JNUM) 25,1001,1001                                         BUR 5890
 1001 CONTINUE                                                          BUR 5900
      IF(DELDAU .GT. 0.) DELDAY = DELDAU                                BUR 5910
      IF(I3D .GT. 0) GOTO 1002                                          BUR 5920
      LY = 0                                                            BUR 5930
      DO 61 I=1,N20                                                     BUR 5940
        LY = LY + LAYER(I)                                              BUR 5950
        JREG(I) = IREG(LY)                                              BUR 5960
   61 CONTINUE                                                          BUR 5970
 1002 CONTINUE                                                          BUR 5980
      CAVNEW = 0.                                                       BUR 5990
      IF(JN .LT. 1) GOTO 1200                                           BUR 6000
      DO 51 II=1,5                                                      BUR 6010
        ZFS(II) = 0.                                                    BUR 6020
   51 CONTINUE                                                          BUR 6030
      DO 52 IR=1,N200C                                                  BUR 6040
        PV = POWSUB(IR) * VOL(IR) * ZF(5,IR) / VOLUME                   BUR 6050
        DO 52 II=1,4                                                    BUR 6060
          ZFT = ZF(II,IR) * PV                                          BUR 6070
          ZFS(II) = ZFS(II) + ZFT                                       BUR 6080
          ZFS(5) = ZFS(5) + ZFT                                         BUR 6090
   52 CONTINUE                                                          BUR 6100
      ZFS(5) = 1. - ZFS(5)                                              BUR 6110
C                                                                       BUR 6120
C     NEUE FIWATT-BESTIMMUNG NACH DIN 25463 VON 1990                    BUR 6130
C                                                                       BUR 6140
      FW = 0.                                                           BUR 6150
      DO 54 II=1,5                                                      BUR 6160
        FW = FW + ZFS(II) / FIWISO(II)                                  BUR 6170
   54 CONTINUE                                                          BUR 6180
      FIWATT = 1. / FW                                                  BUR 6190
      PRO(175) = FIWATT / 1.E+10                                        BUR 6200
      IF(INZWX .GT. 0) PRO(176) = PRO(175)                              BUR 6210
      IF(IPRIN(3) .GE. 1) WRITE (NT,53) (ZFS(II),II=1,5),FIWATT         BUR 6220
      ZPPNEW(JN) = 1. - ZFS(5)                                          BUR 6230
      XPPNEW(JN) = XPPNEW(JN) / JNUM                                    BUR 6240
      YPPNEW(JN) = YPPNEW(JN) / JNUM                                    BUR 6250
      CAVNEW = XPPNEW(JN) / YPPNEW(JN)                                  BUR 6260
 1200 CONTINUE                                                          BUR 6270
      CRT = XXPNEW / YYPNEW                                             BUR 6280
      SPECK(JSPEC+2) = OUTPUT / HMTOT                                   BUR 6290
      POWAV = POWAV * 1.0E+06                                           BUR 6300
      PRO(46) = POWAV                                                   BUR 6310
      OUTPUT = OUTPUT * 1.0E-06                                         BUR 6320
      JNN = 0                                                           BUR 6330
      I = 0                                                             BUR 6340
      VP = 0.                                                           BUR 6350
      VO = 0.                                                           BUR 6360
      VO1 = 0.                                                          BUR 6370
      VP1 = 0.                                                          BUR 6380
      VOC = 0.                                                          BUR 6390
      LAY = 0                                                           BUR 6400
CFZJ042                                                       09.09.05  BUR 6410
      MIXM = 0                                                          BUR 6420
      DO 60 IX=1,NDR                                                    BUR 6430
        MIXM = MAX0(MIXM,MIX(IX))                                       BUR 6440
        VRP(IX) = 0.                                                    BUR 6450
   60 CONTINUE                                                          BUR 6460
      DO 26 IR=1,N200                                                   BUR 6470
        NREG = NRG(IR)                                                  BUR 6480
        IF(MIXM .EQ. 1 .AND. IR .EQ. 1 .AND. IPRIN(3) .GE. 1) WRITE (NT,BUR 6490
     1   3)                                                             BUR 6500
        IF(MIXM .GT. 1 .AND. IR .EQ. 1 .AND. IPRIN(3) .GE. 1) WRITE (NT,BUR 6510
     1   29)                                                            BUR 6520
CFZJ042                                                       09.09.05  BUR 6530
        IF(NOPOW(IR) .GT. 0) GOTO 28                                    BUR 6540
        IF(POWSUB(IR) .GT. 0.) VRP(NREG) = VRP(NREG) + VOL(IR)          BUR 6550
        IF(MIXM .EQ. 1) GOTO 28                                         BUR 6560
        I = I + 1                                                       BUR 6570
        VP = VP + POWSUB(IR) * VOL(IR)                                  BUR 6580
        VO = VO + VOL(IR)                                               BUR 6590
CFZJ042                                                       09.09.05  BUR 6600
        VP0 = VP                                                        BUR 6610
        IF(I .EQ. MIX(NREG)) GOTO 27                                    BUR 6620
   28   CONTINUE                                                        BUR 6630
        IF(IPRIN(3) .GE. 1) WRITE (NT,4) IR,VOL(IR),CRNEW(IR),POWSUB(IR)BUR 6640
     1   ,THBURN(IR),FADOS3(IR)                                         BUR 6650
        IF(MIXM .EQ. 1 .AND. NOPOW(IR) .EQ. 0) LAY = IR                 BUR 6660
        IF(MIXM .EQ. 1 .AND. NOPOW(IR) .EQ. 0) POWL(LAY) = POWSUB(IR)   BUR 6670
        GOTO 36                                                         BUR 6680
   27   CONTINUE                                                        BUR 6690
        VP = VP / VO                                                    BUR 6700
CFZJ042                                                       09.09.05  BUR 6710
        LAY = LAY + 1                                                   BUR 6720
        POWL(LAY) = VP                                                  BUR 6730
CFZJ042                                                       09.09.05  BUR 6740
        VREG(LAY) = VO                                                  BUR 6750
        IF(IPRIN(3) .GE. 1) WRITE (NT,4) IR,VOL(IR),CRNEW(IR),POWSUB(IR)BUR 6760
     1   ,THBURN(IR),FADOS3(IR),LAY,VP                                  BUR 6770
        VP = 0.                                                         BUR 6780
        VO = 0.                                                         BUR 6790
        I = 0                                                           BUR 6800
CFZJ042                                                       09.09.05  BUR 6810
        DO 105 II=IR-MIX(NREG)+1,IR                                     BUR 6820
          AGEFAC(II) = 0.                                               BUR 6830
          IF(VP0 .GT. 0.) AGEFAC(II) = POWSUB(II) * VOL(II) / VP0       BUR 6840
  105   CONTINUE                                                        BUR 6850
        VO1 = 0.                                                        BUR 6860
        VP1 = 0.                                                        BUR 6870
   36   IF(NOPOW(IR) .EQ. 0) VOC = VOC + VOL(IR)                        BUR 6880
   26 CONTINUE                                                          BUR 6890
      LAYT = LAY                                                        BUR 6900
      IF(IPRIN(3) .GE. 1) WRITE (NT,5) CRT,POWAV,HMRAT*1.E6,VOC,OUTPUT  BUR 6910
      VOVO = (VOC-VOLUME) / VOLUME                                      BUR 6920
      IF(ABS(VOVO) .GT. 1.E-3 .AND. IPRIN(3) .GE. 1) WRITE (6,38) VOLUMEBUR 6930
      POWLMAX = 0.                                                      BUR 6940
      DO 39 I=1,LAY                                                     BUR 6950
        POWLMAX = AMAX1(POWLMAX,POWL(I))                                BUR 6960
   39 CONTINUE                                                          BUR 6970
      POWLMAX = POWLMAX * POWAV                                         BUR 6980
      DELDAZ = DELDAY * JNSTOP                                          BUR 6990
      N60 = MUHU(24)                                                    BUR 7000
      IF(N60 .EQ. 0) GOTO 9995                                          BUR 7010
      ITEST = 1                                                         BUR 7020
      PRO(298) = PRO(298) + 1.                                          BUR 7030
      IPZ = IPRIN(15) + 1                                               BUR 7040
      WRITE (N60) ITEST                                                 BUR 7050
      WRITE (N60) IPRIN(15),JN,POWER,DELDAY,(POWSUB(IR),THBURN(IR),     BUR 7060
     1 (ZF(I,IR),I=1,4),(ZF(I,IR),I=6,8),IR=1,N200)                     BUR 7070
      WRITE (NT,2222) PRO(298),JN,IPZ,ITEST                             BUR 7080
 9995 CONTINUE                                                          BUR 7090
      IF(NTIK .LE. 0) GOTO 9991                                         BUR 7100
      IT20 = ITEMP(20)                                                  BUR 7110
      IF(IT20 .EQ. 0) GOTO 9970                                         BUR 7120
      DO 9971 J=1,IT20                                                  BUR 7130
        IF(ITEMP(J) .EQ. JN) GOTO 9970                                  BUR 7140
 9971 CONTINUE                                                          BUR 7150
      GOTO 9991                                                         BUR 7160
 9970 CONTINUE                                                          BUR 7170
      IF(ITIK(10) .NE. 2) GOTO 102                                      BUR 7180
      DELDAY = DELDAV                                                   BUR 7190
      GOTO 9991                                                         BUR 7200
  102 CONTINUE                                                          BUR 7210
      ITIK(1) = ITIK(1) + 1                                             BUR 7220
      ITIK(2) = 0                                                       BUR 7230
      ITIK(3) = 1                                                       BUR 7240
      IF(IVSP(8) .LE. 2) GOTO 100                                       BUR 7250
      ITIK(3) = 0                                                       BUR 7260
      ITIK(4) = IVSP(8)                                                 BUR 7270
  100 CONTINUE                                                          BUR 7280
      IF(ITIK(10) .NE. 3) GOTO 101                                      BUR 7290
      DELDAV = DELDAX                                                   BUR 7300
      DELDAY = DELDAX - DAYMIN                                          BUR 7310
      JNUM = JNDAU                                                      BUR 7320
  101 CONTINUE                                                          BUR 7330
      IF(JN .GE. JNSTOP) N61 = 6                                        BUR 7340
      IF(JN .GE. JNSTOP) JREST = JRESTW                                 BUR 7350
CFZJ017 Delete unused variable BK                             12.12.03  BUR 7360
      IF(ITIK(1) .GT. 1 .AND. ITIK(9) .LT. 0) ITIK(9) = ITIK(9) + 4     BUR 7370
      BB(1) = 1.                                                        BUR 7380
      BB(2) = 0.                                                        BUR 7390
      BB(3) = 2.5                                                       BUR 7400
      BB(4) = 3.0                                                       BUR 7410
      WRITE (N38) RINN,N20,HCORE,POWER,(BB(N),N=1,4),DELDAV,(LAYER(N),  BUR 7420
     1 JREG(N),RADR(N),N=1,N20),(VOL(N),THBURN(N),FADOS3(N),POWSUB(N),  BUR 7430
     2 NHOT(N),N=1,N200),(NOPOW(N),N=1,N200),RADR,CIZET0,NMAXC,         BUR 7440
     3 (DLAY(N),N=1,NMAXC)                                              BUR 7450
CFZJ042                                                       09.09.05  BUR 7460
      WRITE (N38) ((ZF(I,N),I=1,4),(ZF(I,N),I=6,8),N=1,N200),(NRY(N),N=1BUR 7470
     1 ,N200),(HM(N),N=1,N200)                                          BUR 7480
      REWIND N38                                                        BUR 7490
      WRITE (NT,7)                                                      BUR 7500
      DO 9982 I=1,NXS                                                   BUR 7510
        TFU(I) = 0.                                                     BUR 7520
        TMO(I) = -TCELS(1,I)                                            BUR 7530
 9982 CONTINUE                                                          BUR 7540
      NYS = NXS                                                         BUR 7550
      IF(ITIK(10) .LE. 0) GOTO 9983                                     BUR 7560
      NREST = ITIK(1)                                                   BUR 7570
      NZW = N200                                                        BUR 7580
      NSP = NXS                                                         BUR 7590
      NDER = NDR                                                        BUR 7600
      KMT = KMAT                                                        BUR 7610
      NFUMM = 0                                                         BUR 7620
C                                                                       BUR 7630
      IF(PDTHX .GT. 0.) CALL CITA(1,SGA,SGTR,V1,FKEN,NFUMM,SS)          BUR 7640
C                                                                       BUR 7650
CFZJ042                                                       09.09.05  BUR 7660
      CALL MAITHX(NZW,NSP,NDER,KMT,JN,JNSTOP,B(KX(IRADP)),B(KX(IRAD)),  BUR 7670
     1 B(KX(IPHI)),B(KX(INKOM)),B(KX(IKOMP)),B(KX(IKART)),B(KX(IT)),    BUR 7680
     2 B(KX(IWI)),B(KX(IWT)),B(KX(ITQ)),B(KX(IIFBE)),B(KX(IIFBH)),      BUR 7690
     3 B(KX(IKKB)),B(KX(INHZO)),B(KX(IIKO)),B(KX(IQX)),B(KX(ISTRA)),    BUR 7700
     4 MIX,NRG,AGEFAC,VREG,VRP)                                         BUR 7710
C                                                                       BUR 7720
 9983 CONTINUE                                                          BUR 7730
      WRITE (NT,13)                                                     BUR 7740
CFZJ050                                                       24.04.07  BUR 7750
      DO 9980 I=1,NXS                                                   BUR 7760
        TEMZUT(I) = TFU(I)                                              BUR 7770
        TCELS(1,I) = TMO(I)                                             BUR 7780
        TCELS(2,I) = TFU(I)                                             BUR 7790
        WRITE (NT,15) I,TEMZUT(I),TCELS(1,I)                            BUR 7800
 9980 CONTINUE                                                          BUR 7810
      WRITE (NT,90)                                                     BUR 7820
 9991 CONTINUE                                                          BUR 7830
      IF(IVSP(21) .EQ. 0) GOTO 9989                                     BUR 7840
      IF(IVSP(21) .NE. JN) GOTO 9989                                    BUR 7850
      NP39 = NEND38                                                     BUR 7860
      NP40 = NP39 + N26                                                 BUR 7870
      NP41 = NP39 + N26 * 2                                             BUR 7880
      NP42 = NP39 + N26 * 3                                             BUR 7890
      NP43 = NP39 + N26 * 4                                             BUR 7900
      NP44 = NP43 + N26 * N26                                           BUR 7910
      NP45 = NP44 + N26                                                 BUR 7920
      NP46 = NP45 + NXS * KMAT                                          BUR 7930
      NEND46 = NP46 + N200                                              BUR 7940
      NXSKM = NXS * KMAT                                                BUR 7950
C                                                                       BUR 7960
CFZJ042                                                       09.09.05  BUR 7970
      CALL LIBREX(A(KA(LOUSI)),KL(LOUSI),A(KA(LTOSI)),ABSIG,FISIG,LF0,  BUR 7980
     1 XNU,NHOT,VOL,DEN,THBURN,FADOS3,NOPOW,NXSKM,A(NP39),A(NP40),      BUR 7990
     2 A(NP41),A(NP42),A(NP43),A(NP44),A(NP45),A(NP46),IMAT,A(KA(LCOSS))BUR 8000
     3 ,B(KX(ITEML)),HM,POWSUB,MIX,NRG)                                 BUR 8010
C                                                                       BUR 8020
      IVSP(21) = 0                                                      BUR 8030
 9989 CONTINUE                                                          BUR 8040
      IF(IABS(LOB) .NE. JN) GOTO 9994                                   BUR 8050
      IF(LOB) 9992,9994,9993                                            BUR 8060
 9992 IPRIN(4) = 1                                                      BUR 8070
      ISPEKT(1) = JN                                                    BUR 8080
      LOB = -LOB + 1                                                    BUR 8090
      GOTO 9994                                                         BUR 8100
 9993 CONTINUE                                                          BUR 8110
      OPEN(69,FILE='origmod')                                           BUR 8120
      WRITE (69,7002) IDENTI                                            BUR 8130
      WRITE (69,7000) NOR,KMAT,N200C,N26,LA0,LF0,KFISS                  BUR 8140
      WRITE (69,7000) (IMAT(I),I=1,KMAT)                                BUR 8150
      WRITE (69,7000) (NHOT(I),I=1,N200C)                               BUR 8160
      WRITE (69,7000) (NRY(I),I=1,N200C)                                BUR 8170
      WRITE (69,7001) POWER,DELDAZ                                      BUR 8180
      WRITE (69,7001) (VOR(I),I=1,NOR)                                  BUR 8190
      WRITE (69,7001) ((DOR(J,M),J=1,NOR),M=1,KMAT)                     BUR 8200
      WRITE (69,7001) ((FLX(I,N),I=1,N26),N=1,N200C)                    BUR 8210
      WRITE (69,7001) (XNU(I),I=1,LF0)                                  BUR 8220
      WRITE (69,7001) (FISIG(I),I=1,LF0)                                BUR 8230
      WRITE (69,7001) (ABSIG(I),I=1,LA0)                                BUR 8240
      WRITE (69,7001) (SN2N(I),I=1,LF0)                                 BUR 8250
      ENDFILE 69                                                        BUR 8260
      WRITE (NT,500)                                                    BUR 8270
      LOB = 0                                                           BUR 8280
 9994 CONTINUE                                                          BUR 8290
      IF(LOBN .NE. 2) GOTO 710                                          BUR 8300
      IF(KGR) 701,701,702                                               BUR 8310
  701 IF(JO .EQ. 0 .AND. ISPK .EQ. 0) GOTO 702                          BUR 8320
      GOTO 710                                                          BUR 8330
  702 CONTINUE                                                          BUR 8340
      ISPK = ISPK + 1                                                   BUR 8350
      ITOR = 1                                                          BUR 8360
      WRITE (39) ITOR,IPRIN(15),JN,NXSC,ISPK,DELDAY                     BUR 8370
      DO 703 IN=1,NXSC                                                  BUR 8380
        DO 704 NUC=1,KMAT                                               BUR 8390
          I1 = ((IN-1)*KMAT+NUC-1) * N26                                BUR 8400
          DO 704 IG=1,N26                                               BUR 8410
            I2 = I1 + IG                                                BUR 8420
            ASGMA(IG,NUC) = ABSIG(I2)                                   BUR 8430
  704   CONTINUE                                                        BUR 8440
        DO 705 NUC=1,KFISS                                              BUR 8450
          I1 = ((IN-1)*KFISS+NUC-1) * N26                               BUR 8460
          DO 705 IG=1,N26                                               BUR 8470
            I2 = I1 + IG                                                BUR 8480
            XN = 1.                                                     BUR 8490
            IF(XNU(I2) .GT. 0.) XN = XNU(I2)                            BUR 8500
            FSGMA(IG,NUC) = FISIG(I2) / XN                              BUR 8510
            SGMN2N(IG,NUC) = SN2N(I2)                                   BUR 8520
  705   CONTINUE                                                        BUR 8530
        WRITE (39) ((FSGMA(IG,NUC),IG=1,N26),NUC=1,KFISS)               BUR 8540
        WRITE (39) ((ASGMA(IG,NUC),IG=1,N26),NUC=1,KMAT)                BUR 8550
        WRITE (39) ((SGMN2N(IG,NUC),IG=1,N26),NUC=1,KFISS)              BUR 8560
  703 CONTINUE                                                          BUR 8570
      KGR = 0                                                           BUR 8580
  710 CONTINUE                                                          BUR 8590
      POWMI = POWSUB(1)                                                 BUR 8600
      POWMA = POWMI                                                     BUR 8610
      IRMI = 1                                                          BUR 8620
      IRMA = 1                                                          BUR 8630
      DO 9001 IR =1,N200                                                BUR 8640
        IF(POWSUB(IR) .EQ. 0.) GOTO 9001                                BUR 8650
        IF(POWSUB(IR) .GE. POWMI) GOTO 9000                             BUR 8660
        POWMI = POWSUB(IR)                                              BUR 8670
        IRMI = IR                                                       BUR 8680
 9000   CONTINUE                                                        BUR 8690
        IF(POWSUB(IR) .LE. POWMA) GOTO 9001                             BUR 8700
        POWMA = POWSUB(IR)                                              BUR 8710
        IRMA = IR                                                       BUR 8720
 9001 CONTINUE                                                          BUR 8730
      PRO(1) = POWMA                                                    BUR 8740
      IF(FF(1) .EQ. 0.) FF(1) = 1.                                      BUR 8750
      VKU = PI * 4. * FF(4)**3 / (FF(1)*1000.*3.)                       BUR 8760
      PRO(46) = (PRO(1)*PRO(46)) * VKU                                  BUR 8770
      WRITE (NT,9009) POWMI,IRMI,POWMA,IRMA                             BUR 8780
      WRITE (NT,920) CAVNEW                                             BUR 8790
      IF(I3D .GT. 0) GOTO 9999                                          BUR 8800
      KMA = 0                                                           BUR 8810
      WRITE (NT,18)                                                     BUR 8820
      DO 17 M=1,N20                                                     BUR 8830
        LETZT = KMA + 1                                                 BUR 8840
        KMA = JREG(M)                                                   BUR 8850
        POWSUM = 0.                                                     BUR 8860
        VOLSUM = 0.                                                     BUR 8870
        POWINT = 0.                                                     BUR 8880
        DO 19 N=LETZT,KMA                                               BUR 8890
          IF(NOPOW(N) .GT. 0) GOTO 19                                   BUR 8900
          VOLSUM = VOLSUM + VOL(N)                                      BUR 8910
          POWSUM = POWSUM + (POWSUB(N)*VOL(N))                          BUR 8920
   19   CONTINUE                                                        BUR 8930
        IF(VOLSUM .LE. 0.) GOTO 17                                      BUR 8940
        POWINT = POWSUM / VOLSUM                                        BUR 8950
        WRITE (NT,21) M,POWINT                                          BUR 8960
   17 CONTINUE                                                          BUR 8970
 9999 CONTINUE                                                          BUR 8980
      IF(LOBN .NE. 2) GOTO 37                                           BUR 8990
      ITOR = 2                                                          BUR 9000
      WRITE (39) ITOR,IPRIN(15),JN,NXSC,ISPK,DELDAY                     BUR 9010
      WRITE (39) ((FLX(I,IR),I=1,N26),IR=1,N200C)                       BUR 9020
   37 CONTINUE                                                          BUR 9030
      IF(IPRIN(3) .LT. 1) GOTO 22                                       BUR 9040
      WRITE (NT,9010) (FL,I,I=1,N26)                                    BUR 9050
      WRITE (NT,9011)                                                   BUR 9060
      FLT = 0.                                                          BUR 9070
      J = 0                                                             BUR 9080
      LAY = 0                                                           BUR 9090
      DO 20 I=1,N26                                                     BUR 9100
        FX(I) = 0.                                                      BUR 9110
   20 CONTINUE                                                          BUR 9120
      DO 16 N=1,N200                                                    BUR 9130
        NREG = NRG(N)                                                   BUR 9140
        IF(NOPOW(N) .GT. 0) GOTO 34                                     BUR 9150
        DO 31 I=1,N26                                                   BUR 9160
          FX(I) = FX(I) + VOL(N) * FLX(I,N)                             BUR 9170
   31   CONTINUE                                                        BUR 9180
        IF(MIX(NREG) .EQ. 1) GOTO 34                                    BUR 9190
        J = J + 1                                                       BUR 9200
        IF(J .EQ. MIX(NREG)) GOTO 34                                    BUR 9210
        GOTO 16                                                         BUR 9220
   34   CONTINUE                                                        BUR 9230
        LAY = LAY + 1                                                   BUR 9240
        WRITE (NT,8) LAY,(FLX(I,N),I=1,N26)                             BUR 9250
        J = 0                                                           BUR 9260
   16 CONTINUE                                                          BUR 9270
      DO 32 I=1,N26                                                     BUR 9280
        FX(I) = FX(I) / VOLUME                                          BUR 9290
        FLT = FLT + FX(I)                                               BUR 9300
   32 CONTINUE                                                          BUR 9310
      WRITE (NT,33) (FX(I),I=1,N26)                                     BUR 9320
      PRO(207) = FX(N26) / 1.E+14                                       BUR 9330
      PRO(208) = FLT / 1.E+14                                           BUR 9340
   22 CONTINUE                                                          BUR 9350
      IF(ITIK(10) .EQ. 3) GOTO 80                                       BUR 9360
      RETURN                                                            BUR 9370
      END                                                               BUR 9380
      SUBROUTINE QFIX(CK,DELDAY,GS5,GS9)                                QFI   10
C                                                                       QFI   20
CFZJ006 enlarged dimensions common QVAR                       28.11.03  QFI   30
      COMMON /QVAR/ IVAR,ENDKEF,QVOLLL,QREDUZ,QREMAX,EPQ,EPC,DQDDC,DELC,QFI   40
     1 DCN,SBU,TE(4,300),TA(300),N61,URZ,ZLEKA,ABXEN,TI(300),DQCMAX,DU, QFI   50
     2 JRESTW,JRESTR,JREST,TAU,Q0,QMI,QMA,QRAT,DMOD,DAEM,RMAX,RMIN,QAV, QFI   60
     3 TK1,D,ST(300),NJ                                                 QFI   70
C                                                                       QFI   80
      COMMON /SPECTI/ ITIK(10)                                          QFI   90
C                                                                       QFI  100
      COMMON /KOMVAK/ KOMVAR,KONVAR,XKSUMK,NQVAR                        QFI  110
C                                                                       QFI  120
      COMMON /MPUTA/ TEIMIN,TEIMAX,EMP0,TAU0,MPUTAU,QWU                 QFI  130
C                                                                       QFI  140
CFZJ005 enlarged dimensions common SPEIKO                     28.11.03  QFI  150
      COMMON /SPEIKO/ F(300,9),MX,MM,TMADBH(300),TMIDBH(300)            QFI  160
C                                                                       QFI  170
CFZJ055                                                       25.09.07  QFI  180
C                                                                       QFI  190
      COMMON /STA/ IST,SB,TEX,JNS,RMI,RMA,TKI,TKA,TC                    QFI  200
C                                                                       QFI  210
CFZJ008 enlarged dimension common STAF                        28.11.03  QFI  220
      COMMON /STAF/ TF,TFC(300)                                         QFI  230
C                                                                       QFI  240
      COMMON /BLOCK1/ DUMM(34),POWER                                    QFI  250
C                                                                       QFI  260
CFZJ007 enlarged local dimensions                             28.11.03  QFI  270
      DIMENSION C(300),DD(300),CE(300),Q(300),AFTH(300),OUTPUT(10,300), QFI  280
     1 DELKEF(300),AFTMI(300),AFTMA(300),QAVS(300),T(300),QW(300)       QFI  290
C                                                                       QFI  300
  100 FORMAT (I4,F9.5,F7.2,F9.6,2F10.6,2F9.6,2F9.1,F8.1,2F10.1,F8.1,F7.1QFI  310
     1 )                                                                QFI  320
  101 FORMAT (/'   I DEL-DAYS   HOUR   K-EFF  REL.POWER AFTERHEAT   AFTMQFI  330
     1IN   AFTMAX    T-MAX   T-FUEL   T-MOD  T-MAX PV  T-AVG PV    T-IN QFI  340
     2 T-OUT'/)                                                         QFI  350
  102 FORMAT (I4,F10.5,F9.5,3E12.5,F11.5,F12.5,F14.5,F9.5,2F12.5)       QFI  360
  103 FORMAT (/'   I XE-ABSORP  LEAKAGE     U-233      PU-239     MASS FQFI  370
     1LOW  INTEGR.NW  PRODUCTION  STORED TOTAL  IN CORE  FRACTION 1  FRAQFI  380
     2CTION 2'/)                                                        QFI  390
  106 FORMAT (/'   I FRACTION 3  FRACTION 4  FRACTION 5'/)              QFI  400
  107 FORMAT (I4,3F12.5)                                                QFI  410
  108 FORMAT (/'   I DEL-DAYS   HOUR   K-EFF  REL.POWER AFTERHEAT   AFTMQFI  420
     1IN   AFTMAX    T-MAX   T-FUEL   T-MOD  T-MAX PV  T-AVG PV    T-IN QFI  430
     2 T-OUT XE-ABSORP  LEAKAGE     U-233      PU-239     MASS FLOW  INTQFI  440
     3EGR.NW  PRODUCTION  STORED TOTAL  IN CORE  FRACTION 1  FRACTION 2 QFI  450
     4 FRACTION 3  FRACTION 4  FRACTION 5'/)                            QFI  460
  111 FORMAT (I4,F9.5,F7.2,F9.6,2F10.6,2F9.6,2F9.1,F8.1,2F10.1,F8.1,F7.1QFI  470
     1 ,F10.5,F9.5,3E12.5,F11.5,F12.5,F14.5,F9.5,5F12.5)                QFI  480
C                                                                       QFI  490
C                                                                       QFI  500
      I = ITIK(1) + 1                                                   QFI  510
      DQ = 0.                                                           QFI  520
      C(I) = CK                                                         QFI  530
      DD(I) = DELDAY                                                    QFI  540
      IF(I .EQ. 1) GOTO 110                                             QFI  550
      IF(NQVAR .LE. 0) GOTO 110                                         QFI  560
      IF(KOMVAR .EQ. -1) GOTO 109                                       QFI  570
      QAVS(I) = QAV                                                     QFI  580
      T(I) = TK1                                                        QFI  590
  109 CONTINUE                                                          QFI  600
      QW(I) = QWU                                                       QFI  610
  110 CONTINUE                                                          QFI  620
      IF(I .GT. 1) GOTO 1                                               QFI  630
      OPEN(58,FILE='therlist')                                          QFI  640
      IST = 0                                                           QFI  650
      DD1 = DD(I)                                                       QFI  660
      QVOLLL = 1000.                                                    QFI  670
      SBU = 1.                                                          QFI  680
      Q(I) = 0.                                                         QFI  690
      CE(I) = 0.                                                        QFI  700
      IF(ENDKEF .LE. 0.) ENDKEF = C(I)                                  QFI  710
      DCQ = 0.                                                          QFI  720
      DCQSCH = 0.                                                       QFI  730
      QMIN = 0.00001                                                    QFI  740
      DQMIN = -0.001                                                    QFI  750
      DELQ = 0.                                                         QFI  760
      DO 6 K=1,4                                                        QFI  770
        TE(K,I) = 0.                                                    QFI  780
    6 CONTINUE                                                          QFI  790
      DO 7 N=1,300                                                      QFI  800
        QAVS(N) = 0.                                                    QFI  810
        T(N) = 0.                                                       QFI  820
        QW(N) = 0.                                                      QFI  830
        TI(N) = 0.                                                      QFI  840
        TA(N) = 0.                                                      QFI  850
        DO 8 M=1,9                                                      QFI  860
          F(N,M) = 0.                                                   QFI  870
    8   CONTINUE                                                        QFI  880
    7 CONTINUE                                                          QFI  890
      FREL = 0.                                                         QFI  900
      GOTO 3                                                            QFI  910
    1 CONTINUE                                                          QFI  920
      FREL = 1. / (3.6*QVOLLL)                                          QFI  930
      DD1 = DD(I-1)                                                     QFI  940
      TE(4,I) = TE(4,I-1) + DD(I-1) * 24.                               QFI  950
      DELC = (C(I)-C(I-1)) / DD(I-1)                                    QFI  960
      IF(I .GT. 2) DELQ = Q(I-1) - Q(I-2)                               QFI  970
      IF(DELQ .NE. 0.) GOTO 2                                           QFI  980
      DCM = (C(I)-CE(I)) / DD(I-1)                                      QFI  990
      IF(I .GT. 2) DCN = DCM                                            QFI 1000
      EPQ = EPQ / QRAT                                                  QFI 1010
      EPQ = AMAX1(QMI,EPQ)                                              QFI 1020
      GOTO 3                                                            QFI 1030
    2 CONTINUE                                                          QFI 1040
      EPQ = EPQ * QRAT                                                  QFI 1050
      EPQ = AMIN1(QMA,EPQ)                                              QFI 1060
      DCQ = C(I) - CSCHAZ                                               QFI 1070
      IF(ABS(DCQ) .LT. EPC) GOTO 3                                      QFI 1080
      DQA = DQDDC                                                       QFI 1090
      DQMX = DQDDC * DMOD                                               QFI 1100
      DQMY = DQDDC / DMOD                                               QFI 1110
      DQMX = AMIN1(DQMX,DQMIN)                                          QFI 1120
      DQDDC = DELQ / DCQ * DD(I-1)                                      QFI 1130
      IF(DQDDC .GT. DQCMAX) DQDDC = DQA                                 QFI 1140
      DQDDC = AMIN1(DQDDC,DQMX)                                         QFI 1150
      DQDDC = AMAX1(DQDDC,DQMY)                                         QFI 1160
      DQDDC = DQDDC + DAEM * (DQA-DQDDC)                                QFI 1170
    3 CONTINUE                                                          QFI 1180
      CE(I+1) = C(I) + DELC * DD(I)                                     QFI 1190
      CSCHAZ = CE(I+1) + DCN * DD(I)                                    QFI 1200
      IF(I .EQ. 1) GOTO 5                                               QFI 1210
      IF(C(I) .LE. ENDKEF .AND. CSCHAZ .LE. ENDKEF .AND. Q(I-1) .LE.    QFI 1220
     1 QMIN) GOTO 4                                                     QFI 1230
C                                                                       QFI 1240
C     BERECHNUNG EINES DQ                                               QFI 1250
C                                                                       QFI 1260
      DCQSCH = (ENDKEF-CSCHAZ) / DD(I)                                  QFI 1270
      DQ = DQDDC * DCQSCH                                               QFI 1280
      IF(ABS(DQ) .LT. EPQ) DQ = 0.                                      QFI 1290
    4 CONTINUE                                                          QFI 1300
      Q(I) = Q(I-1) + DQ                                                QFI 1310
      PQ = POWER / QVOLLL                                               QFI 1320
      Q(I) = AMIN1(Q(I),QREMAX)                                         QFI 1330
      Q(I) = AMAX1(Q(I),PQ)                                             QFI 1340
    5 CONTINUE                                                          QFI 1350
      QREDUZ = Q(I)                                                     QFI 1360
      Q(I) = 0.                                                         QFI 1370
      IF(NJ .EQ. 2 .AND. I .GT. 1) Q(I) = POWER / QVOLLL                QFI 1380
      AFTH(I) = SBU * 1000. / QVOLLL                                    QFI 1390
      IF(I .NE. 2) GOTO 12                                              QFI 1400
      AFTH(1) = SB * 1000. / QVOLLL                                     QFI 1410
      TE(1,1) = TEX                                                     QFI 1420
      Q(1) = Q(I)                                                       QFI 1430
      AFTMI(1) = RMI                                                    QFI 1440
      AFTMA(1) = RMA                                                    QFI 1450
      TE(3,1) = TC                                                      QFI 1460
      TFC(1) = TF                                                       QFI 1470
   12 CONTINUE                                                          QFI 1480
      AFTMI(I) = RMIN                                                   QFI 1490
      AFTMA(I) = RMAX                                                   QFI 1500
      OUTPUT(1,I) = DCN * DD1                                           QFI 1510
      OUTPUT(2,I) = DCQ                                                 QFI 1520
      OUTPUT(3,I) = CSCHAZ                                              QFI 1530
      OUTPUT(4,I) = DCQSCH * DD(I)                                      QFI 1540
      OUTPUT(5,I) = DQDDC                                               QFI 1550
      OUTPUT(6,I) = ABXEN                                               QFI 1560
      OUTPUT(7,I) = ZLEKA                                               QFI 1570
      OUTPUT(8,I) = GS5                                                 QFI 1580
      OUTPUT(9,I) = GS9                                                 QFI 1590
      OUTPUT(10,I) = EPQ                                                QFI 1600
      DELKEF(I) = C(I) - ENDKEF                                         QFI 1610
      DO 9 L=1,9                                                        QFI 1620
        F(I,L) = F(I,L) * FREL                                          QFI 1630
    9 CONTINUE                                                          QFI 1640
C                                                                       QFI 1650
C     AUSDRUCKEN                                                        QFI 1660
C                                                                       QFI 1670
      N1 = I                                                            QFI 1680
      IF(JNS .EQ. 1) N1 = JNS                                           QFI 1690
      WRITE (6,101)                                                     QFI 1700
      WRITE (6,100) (J-1,DD(J),TE(4,J),C(J),Q(J),AFTH(J),AFTMI(J),      QFI 1710
     1 AFTMA(J),TE(1,J),TFC(J),TE(3,J),TMADBH(J),TMIDBH(J),TI(J),TA(J), QFI 1720
     2 J=N1,I)                                                          QFI 1730
      WRITE (6,103)                                                     QFI 1740
      WRITE (6,102) (J-1,(OUTPUT(L,J),L=6,9),ST(J),(F(J,L),L=1,6),J=N1,IQFI 1750
     1 )                                                                QFI 1760
      IF(MX .LE. 6) GOTO 10                                             QFI 1770
      WRITE (6,106)                                                     QFI 1780
      WRITE (6,107) (J-1,(F(J,L),L=7,9),J=N1,I)                         QFI 1790
   10 CONTINUE                                                          QFI 1800
      N58 = 58                                                          QFI 1810
      IF(JNS .NE. 1) GOTO 120                                           QFI 1820
      WRITE (N58,108)                                                   QFI 1830
      WRITE (N58,111) (J-1,DD(J),TE(4,J),C(J),Q(J),AFTH(J),AFTMI(J),    QFI 1840
     1 AFTMA(J),TE(1,J),TFC(J),TE(3,J),TMADBH(J),TMIDBH(J),TI(J),TA(J), QFI 1850
     2 (OUTPUT(L,J),L=6,9),ST(J),(F(J,L),L=1,9),J=N1,I)                 QFI 1860
      ENDFILE N58                                                       QFI 1870
      REWIND N58                                                        QFI 1880
  120 CONTINUE                                                          QFI 1890
      RETURN                                                            QFI 1900
      END                                                               QFI 1910
      SUBROUTINE CRPULL(IREG,POISM,POISL,PINMIN,PINMAX,NPOIS,DEN,POIS1, CRP   10
     1 POIS2)                                                           CRP   20
C                                                                       CRP   30
C     CONTROL ROD POISON CONCENTRATION ADJUSTMENT                       CRP   40
C                                                                       CRP   50
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    CRP   60
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    CRP   70
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PICRP   80
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 CRP   90
C                                                                       CRP  100
      EQUIVALENCE(JTPE3,NT)                                             CRP  110
C                                                                       CRP  120
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1CRP  130
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         CRP  140
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    CRP  150
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)CRP  160
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  CRP  170
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    CRP  180
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         CRP  190
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,CRP  200
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            CRP  210
C                                                                       CRP  220
      COMMON /FLUXN/ D(361),IACT                                        CRP  230
C                                                                       CRP  240
CFZJ055                                                       25.09.07  CRP  250
C                                                                       CRP  260
      DIMENSION IREG(NDR),POISM(2,NDR),POISL(2,NDR),PINMIN(NDR),        CRP  270
     1 PINMAX(NDR),NPOIS(NDR),DEN(KMAT,N200),POIS1(2,NDR),POIS2(2,NDR)  CRP  280
C                                                                       CRP  290
    3 FORMAT (4I10,2E12.5)                                              CRP  300
    4 FORMAT (5I10,2E12.5)                                              CRP  310
   99 FORMAT (52H1 ALL CONTROL REGIONS SPECIFIED HAVE BEEN ADJUSTED  )  CRP  320
  361 FORMAT (46H1 SOUP UP CONTROL POISON OR TRY BOMB BUSINESS )        CRP  330
  371 FORMAT (/' ALL CONTROL POISON HAS BEEN REMOVED, K-EFF = ',E14.6)  CRP  340
C                                                                       CRP  350
C                                                                       CRP  360
C     NBUMP = 0:  NORMAL                                                CRP  370
C     NBUMP = 1:  MAX INSERTION                                         CRP  380
C     NBUMP = 2:  MIN INSERTION                                         CRP  390
C     NBUMP = 3:  INITIAL STATE                                         CRP  400
C     DELK  = K(CONTROL) - K(EFF)                                       CRP  410
C                                                                       CRP  420
      DELK = ZKFIND - REACT(2)                                          CRP  430
      SERK2 = REACT(2)                                                  CRP  440
      IF(NBUMP-3) 1000,7,1000                                           CRP  450
 1000 IF(NBUMP-1) 10,1,2                                                CRP  460
    1 IF(DELK) 7,10,10                                                  CRP  470
    2 IF(DELK) 10,10,507                                                CRP  480
    7 JS = 0                                                            CRP  490
      KSIM = JSIM + 1                                                   CRP  500
      JSIM = JSIM + LSIM                                                CRP  510
      GOTO 10                                                           CRP  520
  507 JS = 0                                                            CRP  530
      JSIM = KSIM - 1                                                   CRP  540
      KSIM = KSIM - LSIM                                                CRP  550
      IF(KSIM) 370,370,10                                               CRP  560
   10 CONTINUE                                                          CRP  570
      IF(IPRIN(3) .GT. 0) WRITE (NT,3) NBUMP,LBUMP,JSIM,KSIM,DELK,SERCONCRP  580
      NK = IACT + 3 + NO + NLUM                                         CRP  590
      NL = IACT + 2 + NO + NLUM + NC                                    CRP  600
      IF(JS) 20,20,15                                                   CRP  610
   15 DS = SERK1 - SERK2                                                CRP  620
      IF(DS .EQ. 0.) GOTO 20                                            CRP  630
      ADJ = (ZKFIND-SERK2) / DS                                         CRP  640
   20 CONTINUE                                                          CRP  650
      DO 200 IRR=KSIM,JSIM                                              CRP  660
        KR = NPOIS(IRR)                                                 CRP  670
        IF(KR .LE. 0) GOTO 350                                          CRP  680
        IQ = KR - 1                                                     CRP  690
        IS = 1                                                          CRP  700
        IT = IREG(KR)                                                   CRP  710
        IF(KR .GT. 1) IS = IREG(IQ) + 1                                 CRP  720
        XMIN = PINMIN(IRR)                                              CRP  730
        XMAX = PINMAX(IRR)                                              CRP  740
        IF(IPRIN(3) .GT. 0) WRITE (NT,4) IRR,KR,IS,IT,JS,SERK1,SERK2    CRP  750
        DO 150 L=NK,NL                                                  CRP  760
          K = L - NO - NLUM - IACT - 2                                  CRP  770
          IF(JS .LE. 0) GOTO 40                                         CRP  780
          POIS2(K,KR) = ADJ * (POIS1(K,KR)-POIS2(K,KR)) + POIS2(K,KR)   CRP  790
          POIS1(K,KR) = DEN(L,IS)                                       CRP  800
          DO 35 IR=IS,IT                                                CRP  810
            DEN(L,IR) = POIS2(K,KR)                                     CRP  820
   35     CONTINUE                                                      CRP  830
          GOTO 100                                                      CRP  840
   40     CONTINUE                                                      CRP  850
          TEU = DEN(L,IS) - (POISL(K,KR)*XMIN/0.75)                     CRP  860
          IF(TEU .GT. 0.0) GOTO 940                                     CRP  870
          DO 942 IR=IS,IT                                               CRP  880
            DEN(L,IR) = 0.0                                             CRP  890
  942     CONTINUE                                                      CRP  900
  940     CONTINUE                                                      CRP  910
          POIS1(K,KR) = DEN(L,IS)                                       CRP  920
          POIS2(K,KR) = 0.75 * DEN(L,IS)                                CRP  930
          IF(POIS2(K,KR)-POIS1(K,KR)) 50,45,50                          CRP  940
   45     POIS2(K,KR) = POISL(K,KR) * (XMAX+XMIN) / 2.0                 CRP  950
   50     CONTINUE                                                      CRP  960
          POI = POIS2(K,KR)                                             CRP  970
          IR = IS - 1                                                   CRP  980
   56     CONTINUE                                                      CRP  990
          IR = IR + 1                                                   CRP 1000
          DEN(L,IR) = POI                                               CRP 1010
          IF(IR .LT. IT) GOTO 56                                        CRP 1020
  100     CONTINUE                                                      CRP 1030
          IF(POIS2(K,KR)-POISL(K,KR)*XMAX) 110,105,105                  CRP 1040
  105     POIS2(K,KR) = POISL(K,KR) * XMAX                              CRP 1050
          NBUMP = 1                                                     CRP 1060
          GOTO 131                                                      CRP 1070
  110     IF(POISL(K,KR)*XMIN-POIS2(K,KR)) 130,115,115                  CRP 1080
  115     POIS2(K,KR) = POISL(K,KR) * XMIN                              CRP 1090
          NBUMP = 2                                                     CRP 1100
          GOTO 131                                                      CRP 1110
  130     LBUMP = 3                                                     CRP 1120
  131     CONTINUE                                                      CRP 1130
          POI = POIS2(K,KR)                                             CRP 1140
          IR = IS - 1                                                   CRP 1150
   57     CONTINUE                                                      CRP 1160
          IR = IR + 1                                                   CRP 1170
          DEN(L,IR) = POI                                               CRP 1180
          IF(IR .LT. IT) GOTO 57                                        CRP 1190
          POISM(K,KR) = POISL(K,KR) * XMIN                              CRP 1200
  150   CONTINUE                                                        CRP 1210
  200 CONTINUE                                                          CRP 1220
      IF(LBUMP-3) 300,240,300                                           CRP 1230
  240 NBUMP = 0                                                         CRP 1240
      LBUMP = 0                                                         CRP 1250
  300 JS = JS + 1                                                       CRP 1260
      SERK1 = SERK2                                                     CRP 1270
      JSS = JSS + 1                                                     CRP 1280
      RETURN                                                            CRP 1290
  350 NSWIT = 1                                                         CRP 1300
      WRITE (NT,99)                                                     CRP 1310
      WRITE (NT,361)                                                    CRP 1320
      RETURN                                                            CRP 1330
  370 CONTINUE                                                          CRP 1340
      WRITE (NT,371) REACT(2)                                           CRP 1350
      NSWIT = 1                                                         CRP 1360
      RETURN                                                            CRP 1370
      END                                                               CRP 1380
      SUBROUTINE XENON(IRR,DEN,DENIOD,YIELDX,YIELDJ,FIRATE,ABSXE)       XEN   10
C                                                                       XEN   20
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    XEN   30
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    XEN   40
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIXEN   50
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 XEN   60
C                                                                       XEN   70
      EQUIVALENCE(JTPE3,NT)                                             XEN   80
C                                                                       XEN   90
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1XEN  100
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         XEN  110
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    XEN  120
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)XEN  130
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  XEN  140
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TINE(192),STORE(7,96),NSTO(96),    XEN  150
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         XEN  160
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,XEN  170
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            XEN  180
C                                                                       XEN  190
      COMMON /BLKXEN/ NLP,NLPMAX,LASTMX,TYME(168),FORM(168),QTH(168),   XEN  200
     1 XTIME(84),XFORM(84),XQTH(84),DTXME,QTH0,QTH1,IMMER,QLAST,IXSTOP, XEN  210
     2 XKFIND,NDATEI,NSATZ,NONTRL                                       XEN  220
C                                                                       XEN  230
      COMMON /FLUXN/ D(361),IACT                                        XEN  240
C                                                                       XEN  250
      REAL CXE1(50)/50*0./,PSI(50)/50*0./,CJOD(50)/50*0./,PHI(50)/50*0./XEN  260
     1 ,LMJ/2.87E-5/,LMX/2.1 E-5/                                       XEN  270
C                                                                       XEN  280
      DIMENSION DEN(KMAT,N200),DENIOD(N200),YIELDX(N200),YIELDJ(N200),  XEN  290
     1 FIRATE(N200),ABSXE(N200)                                         XEN  300
C                                                                       XEN  310
  750 FORMAT ('0  QTH0 =',E14.6,'   QTH1 =',E14.6,I6,2E12.5)            XEN  320
  751 FORMAT ('0  CJOD(1) =',E14.6,'   CXE1(1) =',E14.6)                XEN  330
  753 FORMAT ('   IX =',I3,'   PHI(IX) =',E14.6,'   CJOD(IX+1) =',E14.6,XEN  340
     1 '   CXE1(IX+1) =',E14.6,'    PSI(IX) =',E14.6)                   XEN  350
C                                                                       XEN  360
C                                                                       XEN  370
      DTXME = DELSEC                                                    XEN  380
      NLP = (JN-1) * JNUM + JNN                                         XEN  390
      QTH1 = 1.                                                         XEN  400
      QTH0 = QTH1                                                       XEN  410
      IF(NLP .EQ. 1) QTH0 = XVSP(2) / POWER                             XEN  420
      IR = IRR                                                          XEN  430
      IF(NONTRL .GT. 3) WRITE (NT,750) QTH0,QTH1,IR,FIRATE(IR),DTXME    XEN  440
      GMJ = YIELDJ(IR)                                                  XEN  450
      GMX = YIELDX(IR) - YIELDJ(IR)                                     XEN  460
      PHI1 = QTH1 * FIRATE(IR)                                          XEN  470
      PSI1 = QTH1 * ABSXE(IR)                                           XEN  480
      CJOD(1) = DENIOD(IR)                                              XEN  490
      CXE1(1) = DEN(IACT+1,IR)                                          XEN  500
      IF(NONTRL .GT. 3) WRITE (NT,751) CJOD(1),CXE1(1)                  XEN  510
      NPHI = 0                                                          XEN  520
      DTPHI = 2. * DTXME                                                XEN  530
      PHI(1) = PHI1                                                     XEN  540
      PSI(1) = PSI1                                                     XEN  550
      NPHIX = NPHI + 1                                                  XEN  560
      IX = 1                                                            XEN  570
      DDT = DTPHI / 2.                                                  XEN  580
      EXJ = EXP(-LMJ*DDT)                                               XEN  590
      UFX = LMX + PSI(IX)                                               XEN  600
CFZJ047                                                       17.10.06  XEN  610
      IF(UFX-LMJ .EQ. 0.) RETURN                                        XEN  620
      EXX = EXP(-UFX*DDT)                                               XEN  630
      CJOD(IX+1) = EXJ * CJOD(IX) + GMJ / LMJ * PHI(IX) * (1.-EXJ)      XEN  640
      CXE1(IX+1) = EXX * CXE1(IX) + (1.-EXX) * (GMX+GMJ) * PHI(IX) / UFXXEN  650
     1 - (EXX-EXJ) * (CJOD(IX)*LMJ-GMJ*PHI(IX)) / (UFX-LMJ)             XEN  660
      IF(NONTRL .GT. 3) WRITE(NT,753) IX,PHI(IX),CJOD(IX+1),CXE1(IX+1), XEN  670
     1 PSI(IX)                                                          XEN  680
      DENIOD(IR) = CJOD(NPHIX+1)                                        XEN  690
      DEN(IACT+1,IR) = CXE1(NPHIX+1)                                    XEN  700
      RETURN                                                            XEN  710
      END                                                               XEN  720
      SUBROUTINE SYNOPS(NSYN,VOL,DEN,FISMAC,THBURN,FADOS3,B,JAD11,KMAT2,SYN   10
     1 N266,IMAT,DICHTE,RESTHB,RESFAD,GS,MIX,NRG)                       SYN   20
C                                                                       SYN   30
C     STORES SYNOPSIS OF CASE HISTORY AND PRINTS OUT AT END OF RUN      SYN   40
C                                                                       SYN   50
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    SYN   60
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    SYN   70
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PISYN   80
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 SYN   90
C                                                                       SYN  100
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1SYN  110
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         SYN  120
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    SYN  130
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)SYN  140
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  SYN  150
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    SYN  160
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         SYN  170
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,SYN  180
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            SYN  190
C                                                                       SYN  200
CFZJ055                                                       25.09.07  SYN  210
C                                                                       SYN  220
      COMMON /BUC/ BU(6,200)                                            SYN  230
C                                                                       SYN  240
      EQUIVALENCE(JTPE3,NT),(JTPE2,NS),(TXME(1),TBURN),(TXME(2),ABBRND) SYN  250
C                                                                       SYN  260
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), SYN  270
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10SYN  280
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11SYN  290
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         SYN  300
C                                                                       SYN  310
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300),FABC(10),REPC(10),NNESYN  320
     1 ,LISTEQ,NTIK,K2,NRC,NRL,NRO                                      SYN  330
C                                                                       SYN  340
      COMMON /NUCNAM/ T1(200),T2(200),GR(2),PU,FL,MATNR(200)            SYN  350
C                                                                       SYN  360
CFZJ062                                                       04.05.11  SYN  370
      COMMON /ORIGEN/ LOB,NOR,VOR(100),ISPK,KFISS,N200C,NXSC,KGR,       SYN  380
     1 LOBN,IEZ                                                         SYN  390
C                                                                       SYN  400
CFZJ044                                                       26.09.05  SYN  410
      COMMON /IFA/ FA0,FA1,FA2,IEND                                     SYN  420
C                                                                       SYN  430
      COMMON /IPO/ IPRINO,INAK,LNAK,JNT                                 SYN  440
C                                                                       SYN  450
      COMMON /FLUXN/ D(361),IACT                                        SYN  460
C                                                                       SYN  470
      REAL ANRCHG(3)/3*0.0/,ZCROSS/0./,PA233/0./,NP239/0./              SYN  480
C                                                                       SYN  490
      CHARACTER*4 T1,T2,GR,PU,FL,BU                                     SYN  500
C                                                                       SYN  510
      DIMENSION SUM(10),NUPRI(20),VD(20),VOL(N200),DEN(KMAT,N200),      SYN  520
     1 FISMAC(N26,N200),THBURN(N200),FADOS3(N200),B(KMAT2,N266),        SYN  530
     2 DICHTE(KMAT),RESTHB(N200),RESFAD(N200),JAD11(JD11),IMAT(KMAT),   SYN  540
     3 GS(IACT),MIX(NDR),NRG(N200)                                      SYN  550
C                                                                       SYN  560
  300 FORMAT (//11X,48HAVERAGE NEUTRON BALANCE OF THE LAST BURNUP CYCLE SYN  570
     1 //12X,'FRACTIONAL ABSORPTIONS (PER ONE LOST NEUTRON)'//'    MATERSYN  580
     2IAL'/' VSOP-NO  NUCL    ALL GROUPS     ',8(2A4,I2,2X)/(33X,8(2A4, SYN  590
     3 I2,2X)))                                                         SYN  600
  301 FORMAT (I5,3X,2A4,1PE12.4,3X,8(1PE12.4)/(31X,8(1PE12.4)))         SYN  610
  302 FORMAT (3X,13('.'),1PE12.4,3X,3A4)                                SYN  620
  304 FORMAT (//'        LEAKAGE:',1PE12.4,3X,8(1PE12.4)/(31X,8(1PE12.4)SYN  630
     1 ))                                                               SYN  640
  306 FORMAT (//11X,'FRACTIONAL PRODUCTIONS (PER PRODUCED NEUTRON)'//'  SYN  650
     1  MATERIAL'/' VSOP-NO  NUCL    ALL GROUPS     ',8(2A4,I2,2X)/(33X,SYN  660
     2 8(2A4,I2,2X)))                                                   SYN  670
  307 FORMAT (/)                                                        SYN  680
  502 FORMAT (101H0 TIME  K-EFFECTIVE   BURNUP    CONVERSION    TH-232  SYN  690
     1     U-233       U-235      PU-239      PU-241  /101H  STEP       SYN  700
     2          MWD/T      RATIO       (GRAMS)     (GRAMS)     (GRAMS)  SYN  710
     3   (GRAMS)     (GRAMS)  //)                                       SYN  720
  503 FORMAT (I5,1PE13.4,7(1PE12.4))                                    SYN  730
  711 FORMAT (//' BATCHES',17X,8I12)                                    SYN  740
CFZJ059                                                       04.11.09  SYN  750
  713 FORMAT (1H ,4A4,8X,8(1PE12.5))                                    SYN  760
  750 FORMAT (//' AFTER',G14.6,' DAYS  K-EFF EQUAL THE SPECIFIED',F8.5) SYN  770
  751 FORMAT ('1',40X,'ATOM DENSITIES END OF LIFE')                     SYN  780
  752 FORMAT (///////' CYCLE BURNUP ...................',G14.6,' MWD/THMSYN  790
     1'/'=======================================================')      SYN  800
  753 FORMAT (////' FISSILE/HEAVY-METAL   RATIO BEGINNING OF CYCLE......SYN  810
     1',F10.4,' %'//' FISSILE/HEAVY-METAL   RATIO END OF CYCLE..........SYN  820
     3..',F10.4,' % ,   AVERAGE = ',E13.6,' %')                         SYN  830
  754 FORMAT ('1',30X,'ATOM DENSITIES END OF LIFE (LAST COLUMN = REGION SYN  840
     1DENSITY)')                                                        SYN  850
  755 FORMAT (//' AFTER THE SPECIFIED CYCLE LENGTH  ',G14.6,' DAYS     ISYN  860
     1S K-EFF =', F8.5)                                                 SYN  870
  757 FORMAT (//' AVERAGE CONVERSION RATIO   ...............',G16.6//)  SYN  880
  760 FORMAT (//' AVERAGE CONVERSION RATIO FOR BURNUP CYCLE'/' ASSUMING SYN  890
     1COMPLETE DECAY OF PA233 AND NP239 .....',G12.6)                   SYN  900
  770 FORMAT (//'*** CONTINUOUS REFUELLING  :  AVERAGE K-EFF =',G12.5,' SYN  910
     1 ZCROSS =',G12.5,'  EXTRAPOLATED TIMESTEPS =',G12.5,'  ***'/)     SYN  920
  780 FORMAT (12I6)                                                     SYN  930
C                                                                       SYN  940
C                                                                       SYN  950
      DO 20 M=1,10                                                      SYN  960
        SUM(M) = 0.                                                     SYN  970
   20 CONTINUE                                                          SYN  980
      IF(NSYN) 10,10,500                                                SYN  990
   10 CONTINUE                                                          SYN 1000
      JX = JN + 2 - NXE                                                 SYN 1010
      JXP = JX - 1                                                      SYN 1020
      ZJN = JN                                                          SYN 1030
      IF(JN) 17,17,21                                                   SYN 1040
   17 ZJN = 0.0                                                         SYN 1050
      REALNN(1) = ZKFIND                                                SYN 1060
   21 CONTINUE                                                          SYN 1070
      PMAX = 0.0                                                        SYN 1080
      IPS = 0                                                           SYN 1090
      STORE(2,JX) = CRT                                                 SYN 1100
      STORE(3,JX) = GS(1)                                               SYN 1110
      STORE(4,JX) = GS(4)                                               SYN 1120
      STORE(5,JX) = GS(6)                                               SYN 1130
      STORE(6,JX) = GS(16)                                              SYN 1140
      STORE(7,JX) = GS(18)                                              SYN 1150
      STORE(1,JX) = PMAX * CORE / REACT(2)                              SYN 1160
      NSTO(JX) = IPS                                                    SYN 1170
      IF(JN .GT. 0) GOTO 50                                             SYN 1180
      HMANFG = 0.                                                       SYN 1190
      FITOT1 = 0.                                                       SYN 1200
   50 CONTINUE                                                          SYN 1210
      MATER = KMAT                                                      SYN 1220
      IF(JN .NE. 0) GOTO 110                                            SYN 1230
      JSATZ = 1                                                         SYN 1240
      IF(JAD11(JSATZ) .EQ. 0) JAD11(JSATZ) = JSUM11                     SYN 1250
      NXT11 = JAD11(JSATZ)                                              SYN 1260
      DO 100 IR=1,N200                                                  SYN 1270
        WRITE (NDA11,REC=NXT11) (DEN(L,IR),L=1,MATER)                   SYN 1280
        NXT11 = NXT11 + 1                                               SYN 1290
        FITOT1 = FITOT1 + VOL(IR) * (DEN(4,IR)+DEN(6,IR)+DEN(16,IR)+    SYN 1300
     1   DEN(18,IR))                                                    SYN 1310
        DO 100 L=1,IACT                                                 SYN 1320
          HMANFG = HMANFG + DEN(L,IR) * VOL(IR)                         SYN 1330
  100 CONTINUE                                                          SYN 1340
      IF(JSUM11 .LT. NXT11) JSUM11 = NXT11                              SYN 1350
  110 CONTINUE                                                          SYN 1360
      IF(NSYN .LT. 0) GOTO 150                                          SYN 1370
      JSATZ = 2                                                         SYN 1380
      IF(JAD11(JSATZ) .EQ. 0) JAD11(JSATZ) = JSUM11                     SYN 1390
      NXT11 = JAD11(JSATZ)                                              SYN 1400
      DO 120 IR=1,N200                                                  SYN 1410
        WRITE (NDA11,REC=NXT11) (DEN(L,IR),L=1,MATER)                   SYN 1420
        NXT11 = NXT11 + 1                                               SYN 1430
        RESFAD(IR) = FADOS3(IR)                                         SYN 1440
        RESTHB(IR) = THBURN(IR)                                         SYN 1450
  120 CONTINUE                                                          SYN 1460
      IF(JSUM11 .LT. NXT11) JSUM11 = NXT11                              SYN 1470
  150 CONTINUE                                                          SYN 1480
      RETURN                                                            SYN 1490
  500 CONTINUE                                                          SYN 1500
      NQUIT = NQUIT + 1                                                 SYN 1510
      IF(NQUIT-1) 511,511,650                                           SYN 1520
  511 CONTINUE                                                          SYN 1530
      IF(INZW(1) .GE. 0) GOTO 69                                        SYN 1540
      IF(JN .LT. 1) GOTO 61                                             SYN 1550
      ZYKZAL = B(KMAT+1,N26+2)                                          SYN 1560
      DO 60 M=1,KMAT2                                                   SYN 1570
        DO 60 I=1,N266                                                  SYN 1580
          B(M,I) = B(M,I) / ZYKZAL                                      SYN 1590
   60 CONTINUE                                                          SYN 1600
   61 CONTINUE                                                          SYN 1610
C                                                                       SYN 1620
      CALL HEAD(2)                                                      SYN 1630
C                                                                       SYN 1640
      WRITE (NT,300) (GR(1),GR(2),I,I=1,N26)                            SYN 1650
      IP = 0                                                            SYN 1660
   98 CONTINUE                                                          SYN 1670
      IP = IP + 1                                                       SYN 1680
      IF(IP .EQ. 2) WRITE (NT,306) (GR(1),GR(2),I,I=1,N26)              SYN 1690
      IF(N26 .NE. 8) WRITE (NT,307)                                     SYN 1700
      IZ = 1                                                            SYN 1710
      IF(IP .EQ. 2) SUM(IZ) = 0.                                        SYN 1720
      DO 62 M=1,KMAT2                                                   SYN 1730
        IF(IP .EQ. 2 .AND. M .GT. IACT) GOTO 99                         SYN 1740
        IF(M .EQ. IACT+1) IZ = 2                                        SYN 1750
        IF(M .LE. NRO) GOTO 63                                          SYN 1760
        IZ = 3                                                          SYN 1770
        IF(M .GT. NRL) GOTO 64                                          SYN 1780
        GOTO 63                                                         SYN 1790
   64   CONTINUE                                                        SYN 1800
        IZ = 4                                                          SYN 1810
        IF(M .GT. NRC) GOTO 65                                          SYN 1820
        GOTO 63                                                         SYN 1830
   65   CONTINUE                                                        SYN 1840
        IZ = 5                                                          SYN 1850
   63   CONTINUE                                                        SYN 1860
        IF(M .GT. KMAT) GOTO 67                                         SYN 1870
        KIK = IMAT(M)                                                   SYN 1880
        IF(IP .EQ. 1) WRITE (NT,301) M,T1(KIK),T2(KIK),B(M,N26+1),      SYN 1890
     1   (B(M,I),I=1,N26)                                               SYN 1900
        IF(IP .EQ. 2) WRITE (NT,301) M,T1(KIK),T2(KIK),B(M,N266),       SYN 1910
     1   (B(M,N26+1+I),I=1,N26)                                         SYN 1920
        GOTO 68                                                         SYN 1930
   67   CONTINUE                                                        SYN 1940
        IF(IP .EQ. 1 .AND. M .EQ. KMAT2) WRITE (NT,304) B(M,N26+1),     SYN 1950
     1   (B(M,I),I=1,N26)                                               SYN 1960
        IZ = 10                                                         SYN 1970
   68   CONTINUE                                                        SYN 1980
        IF(IP .EQ. 1) SUM(IZ) = SUM(IZ) + B(M,N26+1)                    SYN 1990
        IF(IP .EQ. 2) SUM(IZ) = SUM(IZ) + B(M,N266)                     SYN 2000
        IF(M .EQ. IACT .OR. M .EQ. NRO .OR. M .EQ. NRL .OR. M .EQ. NRC  SYN 2010
     1   .OR. M .EQ. KMAT) WRITE (NT,302) SUM(IZ),PU,PU,PU              SYN 2020
        IF(M .NE. KMAT) GOTO 62                                         SYN 2030
        DO 66 N=2,9                                                     SYN 2040
          SUM(1) = SUM(1) + SUM(N)                                      SYN 2050
   66   CONTINUE                                                        SYN 2060
        IF(IP .EQ. 1) WRITE (NT,302) SUM(1),PU,PU,PU                    SYN 2070
   62 CONTINUE                                                          SYN 2080
      IF(IP .EQ. 1) GOTO 98                                             SYN 2090
   99 CONTINUE                                                          SYN 2100
   69 CONTINUE                                                          SYN 2110
C                                                                       SYN 2120
      CALL HEAD(2)                                                      SYN 2130
C                                                                       SYN 2140
      WRITE (NT,502)                                                    SYN 2150
      ABBRND = 0.                                                       SYN 2160
      DO 600 J=1,JX                                                     SYN 2170
        JJ = 2 * J - 2                                                  SYN 2180
        JTIME = J - 2 + NXE                                             SYN 2190
        IF(INZWXX .EQ. 0) GOTO 1                                        SYN 2200
        IF(J .NE. 1) GOTO 4                                             SYN 2210
        U2331 = STORE(4,J)                                              SYN 2220
        U2351 = STORE(5,J)                                              SYN 2230
        PU2391 = STORE(6,J)                                             SYN 2240
        PU2411 = STORE(7,J)                                             SYN 2250
    4   IF(J .NE. JX) GOTO 5                                            SYN 2260
        PRO(33) = STORE(4,J) - U2331                                    SYN 2270
        PRO(34) = STORE(5,J) - U2351                                    SYN 2280
        PRO(35) = STORE(6,J) - PU2391                                   SYN 2290
        PRO(36) = STORE(7,J) - PU2411                                   SYN 2300
    5   IF(JTIME .NE. INZW(1)) GOTO 1                                   SYN 2310
        PRO(28) = SPECK(JJ+1)                                           SYN 2320
        IX = 3                                                          SYN 2330
        DO 3 K=29,32                                                    SYN 2340
          IX = IX + 1                                                   SYN 2350
          PRO(K) = STORE(IX,J) / 1000.                                  SYN 2360
    3   CONTINUE                                                        SYN 2370
    1   ABBRND = ABBRND + SPECK(JJ+2)                                   SYN 2380
        WRITE (NT,503) JTIME,SPECK(JJ+1),SPECK(JJ+2),(STORE(L,J),L=2,7) SYN 2390
  600 CONTINUE                                                          SYN 2400
      IF(LOBN .EQ. 2) JNT = JNT + JXP                                   SYN 2410
      IF(JSER) 610,610,500                                              SYN 2420
  610 CONTINUE                                                          SYN 2430
      IF(IPRIN(3) .LE. 1) GOTO 650                                      SYN 2440
      IF(NQUIT .GT. 1) GOTO 500                                         SYN 2450
C                                                                       SYN 2460
C     GENAUE BESTIMMUNG DER ERREICHBAREN ABBRANDZEIT                    SYN 2470
C                                                                       SYN 2480
  650 CONTINUE                                                          SYN 2490
      ZCROSS = 1.                                                       SYN 2500
      JBURN = JX + NXE - 3                                              SYN 2510
      JJIF = JX * 2 - 1                                                 SYN 2520
      REALNN(2) = SPECK(JJIF)                                           SYN 2530
      IF(SPECK(JJIF) .GE. ZKFIND) GOTO 800                              SYN 2540
      IF(JSER .EQ. 0) GOTO 800                                          SYN 2550
      ZCROSS = (SPECK(JJIF-2)-ZKFIND) / (SPECK(JJIF-2)-SPECK(JJIF))     SYN 2560
      IF(IPRIN(6) .EQ. 0) GOTO 680                                      SYN 2570
C                                                                       SYN 2580
C     CONTINUOUS REFUELLING: FIND LENGTH OF PERIOD IN WHICH AVERAGE     SYN 2590
C     K-EFF IS EQUAL TO ZKFIND                                          SYN 2600
C     IPRIN(6): NO OF RELOADING INTERVAL IN WHICH CON OPERATION STARTS  SYN 2610
C                                                                       SYN 2620
      IF((IPRIN(15)+1) .LT. IPRIN(6)) GOTO 680                          SYN 2630
      J0 = 2 - NXE                                                      SYN 2640
      J0 = J0 * 2 - 1                                                   SYN 2650
      A = 0.                                                            SYN 2660
      IF(JBURN .LE. 0) GOTO 670                                         SYN 2670
      DO 660 J=1,JBURN                                                  SYN 2680
        A = A + 0.5 * ((SPECK(J0+2)-ZKFIND)+(SPECK(J0)-ZKFIND))         SYN 2690
        J0 = J0 + 2                                                     SYN 2700
  660 CONTINUE                                                          SYN 2710
  670 CONTINUE                                                          SYN 2720
      A = A + 0.5 * (SPECK(J0)-ZKFIND) * ZCROSS                         SYN 2730
      X = SQRT(2.*A*(1.-ZCROSS)/(ZKFIND-SPECK(J0+2)))                   SYN 2740
      WRITE (NT,770) ZKFIND,ZCROSS,X                                    SYN 2750
      ZCROSS = ZCROSS + X                                               SYN 2760
  680 CONTINUE                                                          SYN 2770
      MATER = KMAT                                                      SYN 2780
      JSATZ = 2                                                         SYN 2790
      NXT11 = JAD11(JSATZ)                                              SYN 2800
      DO 710 IR=1,N200                                                  SYN 2810
        READ (NDA11,REC=NXT11) (DICHTE(L),L=1,MATER)                    SYN 2820
        NXT11 = NXT11 + 1                                               SYN 2830
        DO 700 L=1,MATER                                                SYN 2840
          DEN(L,IR) = DICHTE(L) + ZCROSS * (DEN(L,IR)-DICHTE(L))        SYN 2850
  700   CONTINUE                                                        SYN 2860
        FADOS3(IR) = RESFAD(IR) + (FADOS3(IR)-RESFAD(IR)) * ZCROSS      SYN 2870
        THBURN(IR) = RESTHB(IR) + (THBURN(IR)-RESTHB(IR)) * ZCROSS      SYN 2880
  710 CONTINUE                                                          SYN 2890
      REALNN(2) = SPECK(JJIF) - (JNSTOP-JN) * (SPECK(JJIF-2)-SPECK(JJIF)SYN 2900
     1 )                                                                SYN 2910
  800 CONTINUE                                                          SYN 2920
      IF(JSER .NE. 0) GOTO 7                                            SYN 2930
      F1 = FA0 - FA1                                                    SYN 2940
      F2 = FA1 - FA2                                                    SYN 2950
      IF(F2 .EQ. 0.) GOTO 7                                             SYN 2960
      RATIO = F1 / F2                                                   SYN 2970
      IF(RATIO .LE. 0. .OR. RATIO .GT. 1.) GOTO 7                       SYN 2980
      ZCROSS = RATIO                                                    SYN 2990
    7 CONTINUE                                                          SYN 3000
      TBURN = (JBURN+ZCROSS) * DELDAY                                   SYN 3010
      IF(INZWXX .EQ. 0) GOTO 2                                          SYN 3020
      DO 6 K=33,36                                                      SYN 3030
        PRO(K) = PRO(K) / TBURN                                         SYN 3040
    6 CONTINUE                                                          SYN 3050
    2 ABBRND = ABBRND - SPECK(JJIF+1) * (1.0-ZCROSS)                    SYN 3060
      IF(SPECK(JJIF) .GE. ZKFIND) GOTO 801                              SYN 3070
      WRITE (NT,750) TBURN,ZKFIND                                       SYN 3080
      REACT(2) = ZKFIND                                                 SYN 3090
      GOTO 805                                                          SYN 3100
  801 CONTINUE                                                          SYN 3110
      WRITE (NT,755) TBURN,SPECK(JJIF)                                  SYN 3120
  805 CONTINUE                                                          SYN 3130
      MU1 = MUHU(1)                                                     SYN 3140
C                                                                       SYN 3150
CARD(S) R33                                                             SYN 3160
C                                                                       SYN 3170
      IF(MU1 .GT. 0) READ (NS,780) (NUPRI(I),I=1,MU1)                   SYN 3180
C                                                                       SYN 3190
      DO 811 I=1,20                                                     SYN 3200
        VD(I) = 0.                                                      SYN 3210
  811 CONTINUE                                                          SYN 3220
      VO = 0.                                                           SYN 3230
      II = 0                                                            SYN 3240
      IL = 0                                                            SYN 3250
      IRC = 1                                                           SYN 3260
      N8 = 8                                                            SYN 3270
      IF(MU1) 786,786,787                                               SYN 3280
  786 CONTINUE                                                          SYN 3290
      IF(IPRIN(2) .LE. 1 .OR. IPRIN(3) .LT. 0) GOTO 809                 SYN 3300
  787 CONTINUE                                                          SYN 3310
      IF(MU1 .GT. 0) GOTO 783                                           SYN 3320
      DO 90 IR=1,N200,N8                                                SYN 3330
        IT = IR                                                         SYN 3340
        IRB = IR + 7                                                    SYN 3350
        IF(IRB .GT. N200) IRB = N200                                    SYN 3360
        WRITE (NT,751)                                                  SYN 3370
        WRITE (NT,711) (I,I=IR,IRB)                                     SYN 3380
        DO 70 L=1,KMAT                                                  SYN 3390
CFZJ059                                                       04.11.09  SYN 3400
          WRITE (NT,713) (BU(N,L),N=1,4),(DEN(L,IRA),IRA=IR,IRB)        SYN 3410
   70   CONTINUE                                                        SYN 3420
        IRC = IRB + 1                                                   SYN 3430
   90 CONTINUE                                                          SYN 3440
  783 CONTINUE                                                          SYN 3450
      IF(MU1 .EQ.0) GOTO 809                                            SYN 3460
      IRC = 1                                                           SYN 3470
      N8 = 8                                                            SYN 3480
      IRD = IRC                                                         SYN 3490
      WRITE (NT,754)                                                    SYN 3500
      DO 812 IR=IRC,N200                                                SYN 3510
        NREG = NRG(IR)                                                  SYN 3520
        IRB = MIX(NREG)                                                 SYN 3530
        IF(IRB .GT. N8) IRB = N8                                        SYN 3540
        II = II + 1                                                     SYN 3550
        DO 813 LL=1,MU1                                                 SYN 3560
          L = NUPRI(LL)                                                 SYN 3570
          VD(LL) = VD(LL) + DEN(L,IR) * VOL(IR)                         SYN 3580
  813   CONTINUE                                                        SYN 3590
        VO = VO + VOL(IR)                                               SYN 3600
        IF(II .LT. MIX(NREG)) GOTO 812                                  SYN 3610
  815   CONTINUE                                                        SYN 3620
        IF(IRB .EQ. N8) WRITE (NT,711) (I,I=IRD,IRB+IRD-1)              SYN 3630
        IF(IRB .NE. N8) WRITE (NT,711) (I,I=IRD,IRB+IRD-1),NREG         SYN 3640
        DO 814 LL=1,MU1                                                 SYN 3650
          L = NUPRI(LL)                                                 SYN 3660
          IF(IRB .NE. N8) VD(LL) = VD(LL) / VO                          SYN 3670
CFZJ059                                                       04.11.09  SYN 3680
          IF(IRB .EQ. N8) WRITE (NT,713) (BU(N,L),N=1,4),(DEN(L,IRA),IRASYN 3690
     1     =IRD,IRB+IRD-1)                                              SYN 3700
CFZJ059                                                       04.11.09  SYN 3710
          IF(IRB .NE. N8) WRITE (NT,713) (BU(N,L),N=1,4),(DEN(L,IRA),IRASYN 3720
     1     =IRD,IRB+IRD-1),VD(LL)                                       SYN 3730
  814   CONTINUE                                                        SYN 3740
        IF(IRB .NE. N8) GOTO 816                                        SYN 3750
C                                                                       SYN 3760
C     AENDERUNG NOETIG WENN MIX > 15 <---                               SYN 3770
C                                                                       SYN 3780
        IRD = IRB + IRD                                                 SYN 3790
        IRB = MIX(NREG) - N8                                            SYN 3800
        GOTO 815                                                        SYN 3810
  816   CONTINUE                                                        SYN 3820
        DO 817 I=1,20                                                   SYN 3830
          VD(I) = 0.                                                    SYN 3840
  817   CONTINUE                                                        SYN 3850
        VO = 0.                                                         SYN 3860
        II = 0                                                          SYN 3870
        IRD = IRB + IRD                                                 SYN 3880
  812 CONTINUE                                                          SYN 3890
  809 CONTINUE                                                          SYN 3900
      ZKFIND = REALNN(1)                                                SYN 3910
      WRITE (NT,752) ABBRND                                             SYN 3920
      IF(LOBN .NE. 2) GOTO 95                                           SYN 3930
      TB = DELDAY * ZCROSS                                              SYN 3940
      ITOR = 7                                                          SYN 3950
      WRITE (39) ITOR,IPRIN(15),JN,NXSC,ISPK,TB                         SYN 3960
   95 CONTINUE                                                          SYN 3970
C                                                                       SYN 3980
C     BESTIMMUNG VON ANFANGS- UND ENDANREICHERUNGEN                     SYN 3990
C                                                                       SYN 4000
      HMENDE = 0.                                                       SYN 4010
      FITOT2 = 0.                                                       SYN 4020
      DO 820 IR=1,N200                                                  SYN 4030
        HME = 0.                                                        SYN 4040
        DO 810 M=1,IACT                                                 SYN 4050
          HME = HME + DEN(M,IR)                                         SYN 4060
  810   CONTINUE                                                        SYN 4070
        HMENDE = HMENDE + HME * VOL(IR)                                 SYN 4080
        FITOT2 = FITOT2 + VOL(IR) * (DEN(4,IR)+DEN(6,IR)+DEN(16,IR)+    SYN 4090
     1   DEN(18,IR))                                                    SYN 4100
  820 CONTINUE                                                          SYN 4110
      ANRCHG(1) = FITOT1 / HMANFG * 100.                                SYN 4120
      ANRCHG(2) = FITOT2 / HMENDE * 100.                                SYN 4130
      ANRAV = (ANRCHG(1)+ANRCHG(2)) / 2.                                SYN 4140
      IF(INZWXX .GT. 0) PRO(27) = ANRAV                                 SYN 4150
      WRITE (NT,753) ANRCHG(1),ANRCHG(2),ANRAV                          SYN 4160
C                                                                       SYN 4170
C     BESTIMMUNG DER MITTLEREN CONVERSIONS RATE                         SYN 4180
C                                                                       SYN 4190
      XMNEW = 0.                                                        SYN 4200
      YMNEW = 0.                                                        SYN 4210
      ZMNEW = 0.                                                        SYN 4220
      CCR = 0.                                                          SYN 4230
  822 CONTINUE                                                          SYN 4240
      IF(ZCROSS .GT. 0.0) GOTO 825                                      SYN 4250
      JBURN = JBURN - 1                                                 SYN 4260
      ZCROSS = ZCROSS + 1.0                                             SYN 4270
      GOTO 822                                                          SYN 4280
  825 CONTINUE                                                          SYN 4290
      JJBURN = JBURN + 1                                                SYN 4300
      JLOOP = JJBURN                                                    SYN 4310
      IF(JLOOP .LT. 1) JLOOP = 1.                                       SYN 4320
      DO 830 JAN=1,JLOOP                                                SYN 4330
        CCROSS = 1.                                                     SYN 4340
        IF(JAN .EQ. JJBURN) CCROSS = ZCROSS                             SYN 4350
        XMNEW = XMNEW + XPPNEW(JAN) * CCROSS                            SYN 4360
        YMNEW = YMNEW + YPPNEW(JAN) * CCROSS                            SYN 4370
        ZMNEW = ZMNEW + ZPPNEW(JAN) * CCROSS                            SYN 4380
        CCR = CCR + CCROSS                                              SYN 4390
  830 CONTINUE                                                          SYN 4400
      ZMNEW = ZMNEW / CCR                                               SYN 4410
      CONNEW = XMNEW / YMNEW                                            SYN 4420
      WRITE (NT,757) CONNEW                                             SYN 4430
      IF(NRSTRT .GT. 0) GOTO 999                                        SYN 4440
      PA233 = 0.                                                        SYN 4450
      NP239 = 0.                                                        SYN 4460
      DO 860 IR=1,N200                                                  SYN 4470
        PA233 = PA233 + DEN(3,IR) * VOL(IR)                             SYN 4480
        NP239 = NP239 + DEN(13,IR) * VOL(IR)                            SYN 4490
  860 CONTINUE                                                          SYN 4500
      CONVER = (XMNEW*DELDAY*8.64E+4+PA233+NP239) / (YMNEW*DELDAY*8.64E+SYN 4510
     1 4)                                                               SYN 4520
      IF(INZWXX .GT. 0) PRO(26) = CONVER                                SYN 4530
      WRITE (NT,760) CONVER                                             SYN 4540
  999 CONTINUE                                                          SYN 4550
      RETURN                                                            SYN 4560
      END                                                               SYN 4570
      SUBROUTINE FINAL(NP341,NP342,MIX,NRG)                             INA   10
C                                                                       INA   20
C     PREPARES TO TERMINATE CASE                                        INA   30
C                                                                       INA   40
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    INA   50
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    INA   60
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIINA   70
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 INA   80
C                                                                       INA   90
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1INA  100
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         INA  110
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    INA  120
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)INA  130
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  INA  140
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    INA  150
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         INA  160
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,INA  170
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            INA  180
C                                                                       INA  190
      COMMON /VARDIM/ A(8000000)                                        INA  200
C                                                                       INA  210
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       INA  220
C                                                                       INA  230
      EQUIVALENCE(LI(10),LIREG),(LI(18),LCFRA),(LI(20),LPOIL),          INA  240
     1 (LI(23),LNPOI),(LI(29),LVOL),(LI(31),LDEN),(LI(33),LFISM),       INA  250
     2 (LI(40),LHM),(LI(41),LTHBU),(LI(42),LFADO),(LI(101),LB),         INA  260
     3 (LI(146),LJAD1),(LI(1),LIMAT),(LI(151),LAWT),(LI(152),LGS),      INA  270
     4 (LI(153),LGM )                                                   INA  280
C                                                                       INA  290
      DIMENSION MIX(NDR),NRG(N200)                                      INA  300
C                                                                       INA  310
C                                                                       INA  320
      N266 = N26 * 2 + 2                                                INA  330
      IF(JSER-1) 2,4,100                                                INA  340
    2 IF(JS-JSMAX) 1,3,3                                                INA  350
    1 IF(JSS-JSSMAX) 4,3,3                                              INA  360
C                                                                       INA  370
    3 CALL DENOUT(A(KA(LIREG)),A(KA(LCFRA)),A(KA(LPOIL)),A(KA(LNPOI)),  INA  380
     1 A(KA(LVOL)),A(KA(LDEN)),A(KA(LHM)),A(KA(LTHBU)),A(KA(LIMAT)),    INA  390
     2 A(KA(LAWT)),A(KA(LGM)),A(KA(LGS)))                               INA  400
C                                                                       INA  410
    4 CONTINUE                                                          INA  420
      NSYN = -1                                                         INA  430
      NP34 = NEND33                                                     INA  440
      KMAT2 = KMAT + 2                                                  INA  450
C                                                                       INA  460
      CALL SYNOPS(NSYN,A(KA(LVOL)),A(KA(LDEN)),A(KA(LFISM)),A(KA(LTHBU))INA  470
     1 ,A(KA(LFADO)),A(KA(LB)),A(KA(LJAD1)),KMAT2,N266,A(KA(LIMAT)),    INA  480
     2 A(NP34),A(NP341),A(NP342),A(KA(LGS)),MIX,NRG)                    INA  490
C                                                                       INA  500
      NSYN = 1                                                          INA  510
C                                                                       INA  520
      CALL SYNOPS(NSYN,A(KA(LVOL)),A(KA(LDEN)),A(KA(LFISM)),A(KA(LTHBU))INA  530
     1 ,A(KA(LFADO)),A(KA(LB)),A(KA(LJAD1)),KMAT2,N266,A(KA(LIMAT)),    INA  540
     2 A(NP34),A(NP341),A(NP342),A(KA(LGS)),MIX,NRG)                    INA  550
C                                                                       INA  560
  100 CONTINUE                                                          INA  570
      RETURN                                                            INA  580
      END                                                               INA  590