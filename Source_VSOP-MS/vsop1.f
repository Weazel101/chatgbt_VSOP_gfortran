      SUBROUTINE CH1(MAFIA,IREG,SSLUMP,NHOT,VOL,DEN,NOPOW,HMETAL,HMNULL,CH1   10
     1 JAD11,CONC,MIX)                                                  CH1   20
C                                                                       CH1   30
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    CH1   40
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    CH1   50
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PICH1   60
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D             CH1   70
C                                                                       CH1   80
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1CH1   90
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         CH1  100
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    CH1  110
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)CH1  120
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  CH1  130
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    CH1  140
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         CH1  150
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,CH1  160
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            CH1  170
C                                                                       CH1  180
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, CH1  190
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        CH1  200
     2 FIMAKL(20),NOPILE,MREP,MARX(10)                                  CH1  210
C                                                                       CH1  220
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), CH1  230
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10CH1  240
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11CH1  250
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13,JSUM29                  CH1  260
C                                                                       CH1  270
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300)                      CH1  280
C                                                                       CH1  290
      COMMON /VARDIM/ A(8000000)                                        CH1  300
C                                                                       CH1  310
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       CH1  320
C                                                                       CH1  330
      COMMON /BLOCKK/ DU(3),AVO                                         CH1  340
C                                                                       CH1  350
      COMMON /FLUXN/ D(361),IACT                                        CH1  360
C                                                                       CH1  370
CFZJ055                                                       25.09.07  CH1  380
C                                                                       CH1  390
      COMMON /IPO/ IPRINO,INAK,LNAK,JNT                                 CH1  400
C                                                                       CH1  410
      COMMON /ORIGEN/ DUMM(104),N200C,NXSC                              CH1  420
C                                                                       CH1  430
      COMMON /IDIFKO/ INGC24                                            CH1  440
C                                                                       CH1  450
CFZJ041                                                       15.02.05  CH1  460
CFZJ058                                                       05.11.08  CH1  470
      COMMON /KEFFT/ TKEFF(5000),RKEFF(5000),IPKEFF,NKEFF,TKUM,POWLMAX, CH1  480
     1 THETMX,POWLM(5000),POWB(5000),BUSCRAP(5000),THETM(5000),         CH1  490
     2 POWERF(5000),TEIN(5000),TAUS(5000),TEINTR,TAUSTR                 CH1  500
C                                                                       CH1  510
      COMMON /CITINP/ INPCIT                                            CH1  520
C                                                                       CH1  530
CFZJ043                                                       23.09.05  CH1  540
      COMMON /NEWC/ NEWCOST                                             CH1  550
C                                                                       CH1  560
      DIMENSION IREG(NDR),SSLUMP(N26,NDR),NHOT(N200),VOL(N200),         CH1  570
     1 DEN(KMAT,N200),NOPOW(N200),HMETAL(N200),HMNULL(IACT,N200),       CH1  580
     2 CONC(KMAT),JAD11(JD11),MIX(NDR),XLAM(49),ISPWR(18)               CH1  590
C                                                                       CH1  600
      CHARACTER*72 A66                                                  CH1  610
C                                                                       CH1  620
      EQUIVALENCE(LI(1),LIMAT),(LI(68),LANU ),(LI(3),LOUSI),            CH1  630
     1 (LI(4),LTOSI),(LI(5),LABSI),(LI(6),LFISI),(LI(7),LXNU),          CH1  640
     2 (LI(8),LUMRE),(LI(9),LVOLR),(LI(12),LNBUR),(LI(14),LVLP),        CH1  650
     3 (LI(19),LPOIM),(LI(20),LPOIL),(LI(21),LPINI),(LI(22),LPINA),     CH1  660
     4 (LI(23),LNPOI),(LI(24),LVFRE),(LI(30),LVPAR),(LI(151),LAWT),     CH1  670
     5 (LI(48),LNRG),(LI(79),LBUG),(LI(69),LA1),(LI(70),LA2),           CH1  680
     6 (LI(71),LA3),(LI(72),LA4),(LI(73),LA5),(LI(102),LNRY),           CH1  690
     7 (DIRAC(1,5),XLAM(1)),(JTPE2,NS),(JTPE3,NT),(LI(2),LNOPK)         CH1  700
C                                                                       CH1  710
    1 FORMAT (//' NUMBER OF USED "DIRECT-ACCESS"-RECORDS ON FOLLOWING UNCH1  720
     1ITS:'/1X,58('-')/' UNIT  8:',I6/' UNIT  9:',I6/' UNIT 10:',I6/' UNCH1  730
     2IT 11:',I6/' UNIT 13:',I6/' UNIT 28:',I6/' UNIT 29:',I6)          CH1  740
   40 FORMAT (A72)                                                      CH1  750
  110 FORMAT (18I4)                                                     CH1  760
  112 FORMAT (6I3,I1,I2,5I3,3E12.5)                                     CH1  770
  205 FORMAT (////' NO. OF FISSIONS PER INITIAL FISSIONABLE ATOM AND DAYCH1  780
     1 "FIFA" =',E20.8)                                                 CH1  790
  300 FORMAT ('0'////' TOTAL HEAVY-METAL DENSITY IN BATCHES 1-',I4//    CH1  800
     1 (10E13.5/))                                                      CH1  810
  402 FORMAT (10E12.5)                                                  CH1  820
  403 FORMAT (I5,F9.1,F11.5,F9.2,F11.2,F12.0,F13.0,2F9.0,F8.0)          CH1  830
  404 FORMAT ( 5X,'   TIME (D)   K-EFF    POW-DENS   POW/BALL   FUEL TEMCH1  840
     1P    DISCH.-BU   POWER    TEMP.   TEMP.'/)                        CH1  850
  405 FORMAT (/5X,'                         MAX.       MAX.        MAX. CH1  860
     1        AV.     THERMAL   INLET   OUTLET')                        CH1  870
  506 FORMAT (//' OPTIONS FOR NEXT CYCLE  :'/' IPRIN(1)=',I3,',IPRIN(2)=CH1  880
     1',I3,',IPRIN(3)=',I3,',IPRIN(4)=',I3,',JNSTOP=',I3,',JNUM=',I3,',ICH1  890
     2BUCK=',I3,',MUHU(3)=',I3/' DELDAY=',E12.5,',POWER=',E12.5,',ZKFINDCH1  900
     3=',E12.5,',TDOWN=',E12.5//)                                       CH1  910
  555 FORMAT (1H1)                                                      CH1  920
  999 FORMAT (//////52X,' ***  END OF CASES  ***')                      CH1  930
 1500 FORMAT (I10,10X,1PE16.5)                                          CH1  940
 1502 FORMAT (36H1  COARSE MESH NO.      OUTER RADIUS//)                CH1  950
 1503 FORMAT (/10H0    RINN:,10X,1PE16.5)                               CH1  960
 2003 FORMAT (///' ***  SPECTRUM IS NOT RECALCULATED ***'/)             CH1  970
 2004 FORMAT (///' ***  RECALCULATION OF SPECTRUM IN EACH TIME STEP ***'CH1  980
     1 /)                                                               CH1  990
 2005 FORMAT (///' RECALCULATION OF FAST AND THERMAL')                  CH1 1000
 2006 FORMAT (' SPECTRUM IN TIME STEP ',22('.'),18I4)                   CH1 1010
 2007 FORMAT (// ' RECALCULATION OF THERMAL')                           CH1 1020
 2009 FORMAT (//' *** STREAMING CORRECTION OF THE DIFFUSION CONSTANT ACCCH1 1030
     1ORDING SCHERER-GERWIN ***')                                       CH1 1040
 2010 FORMAT (//' *** STREAMING CORRECTION OF THE DIFFUSION CONSTANT ACCCH1 1050
     1ORDING J.LIEBEROTH,A.STOJADINOVIC: NUCL.SCI.ENG. 1981 ***')       CH1 1060
C                                                                       CH1 1070
C                                                                       CH1 1080
C     INITIALIZATION OF UNIT DESIGNATIONS                               CH1 1090
C                                                                       CH1 1100
C     THERMOS-LIBRARY                                                   CH1 1110
      JTPE4 = 18                                                        CH1 1120
C     THERMALIZATION-LIBRARY                                            CH1 1130
      JTPE6  = 17                                                       CH1 1140
C     GAM-LIBRARY                                                       CH1 1150
      JTPE10 = 16                                                       CH1 1160
C                                                                       CH1 1170
C     IPRIN(8) : RESTART UNIT FOR NDA8  (=14)                           CH1 1180
C     IPRIN(9) : RESTART UNIT FOR NDA9  (=15)                           CH1 1190
C     IPRIN(11): RESTART UNIT FOR NDA10 (=20)                           CH1 1200
C                                                                       CH1 1210
      NEQ = 0                                                           CH1 1220
      INGC24 = 0                                                        CH1 1230
      IPRIN(6) = NEQ                                                    CH1 1240
      IF(JTPE7 .EQ. 0) GOTO 11                                          CH1 1250
      IF(JTPE7 .GT. 0) GOTO 50                                          CH1 1260
C                                                                       CH1 1270
C     END OF OPERATION                                                  CH1 1280
C                                                                       CH1 1290
      IF(IPKEFF .LE. 0) GOTO 401                                        CH1 1300
      OPEN(99,FILE='keff')                                              CH1 1310
      WRITE (6,405)                                                     CH1 1320
      WRITE (6,404)                                                     CH1 1330
CFZJ041                                                       15.02.05  CH1 1340
CFZJ042                                                       09.09.05  CH1 1350
      DO I=1,NKEFF                                                      CH1 1360
        WRITE (99,402) TKEFF(I),RKEFF(I),POWLM(I),POWB(I),THETM(I),     CH1 1370
     1   BUSCRAP(I),POWERF(I)/1.E+06,TEIN(I),TAUS(I)                    CH1 1380
        WRITE (6,403) I,TKEFF(I),RKEFF(I),POWLM(I),POWB(I),THETM(I),    CH1 1390
     1   BUSCRAP(I),POWERF(I)/1.E+06,TEIN(I),TAUS(I)                    CH1 1400
      END DO                                                            CH1 1410
 401  CONTINUE                                                          CH1 1420
      J8 = JSUM8 - 1                                                    CH1 1430
      J9 = JSUM9 - 1                                                    CH1 1440
      IF(IPRIN(8) .GT. 0 .AND. IPRIN(9) .EQ. 0) J9 = J9 - 1             CH1 1450
      J10 = JSUM10 - 1                                                  CH1 1460
      J11 = JSUM11 - 1                                                  CH1 1470
      J13 = JSUM13 - 1                                                  CH1 1480
      J13 = MAX0(J13,0)                                                 CH1 1490
      J28 = NXT28 - 1                                                   CH1 1500
      J29 = JSUM29 - 1                                                  CH1 1510
      IF(IPRIN(8) .GT. 0) J29 = 0                                       CH1 1520
      WRITE (NT,1) J8,J9,J10,J11,J13,J28,J29                            CH1 1530
      WRITE (NT,555)                                                    CH1 1540
C                                                                       CH1 1550
      CALL DATEMS                                                       CH1 1560
C                                                                       CH1 1570
      CALL COVER(NT)                                                    CH1 1580
C                                                                       CH1 1590
      WRITE (NT,999)                                                    CH1 1600
      MIXM = 0                                                          CH1 1610
      DO 49 I=1,NDR                                                     CH1 1620
        MIXM = MAX0(MIXM,MIX(I))                                        CH1 1630
   49 CONTINUE                                                          CH1 1640
C                                                                       CH1 1650
      IF(INAK .GT. 0) CALL LIFE(MIXM,N200,MBOX)                         CH1 1660
C                                                                       CH1 1670
      IF(LNAK .GT. 0) CALL PRIMA(N200C,NXSC,INZW(3),JNT,KMAT,IACT,N26)  CH1 1680
C                                                                       CH1 1690
      IF(INPCIT .GT. 0) GOTO 52                                         CH1 1700
      CLOSE (37)                                                        CH1 1710
      OPEN (37,POSITION='APPEND',FILE='geom')                           CH1 1720
      REWIND 66                                                         CH1 1730
   51 READ (66,40,END=52) A66                                           CH1 1740
      WRITE (37,40) A66                                                 CH1 1750
      GOTO 51                                                           CH1 1760
   52 CONTINUE                                                          CH1 1770
      STOP                                                              CH1 1780
   50 CONTINUE                                                          CH1 1790
C                                                                       CH1 1800
      CALL ADMAIN(A(KA(LIMAT)))                                         CH1 1810
C                                                                       CH1 1820
C     RESTART DATA                                                      CH1 1830
C                                                                       CH1 1840
      MUHU(7) = MUHU(1)                                                 CH1 1850
      MUHU(4) = 0                                                       CH1 1860
C                                                                       CH1 1870
C     CHANGING THE INPUT OF THE FIRST CYCLE AT RESTART                  CH1 1880
C                                                                       CH1 1890
      JEEP = 0                                                          CH1 1900
      IXEN = 1                                                          CH1 1910
CFZJ043                                                       23.09.05  CH1 1920
      NEWCOST = 0                                                       CH1 1930
      IF(IRR9 .EQ. 0) GOTO 3                                            CH1 1940
C                                                                       CH1 1950
CARD R6                                                                 CH1 1960
C                                                                       CH1 1970
      READ (NS,112) (IPRIN(I),I=1,4),NNSTOP,NNUM,NEWCOST,NIAVC,IBUC,    CH1 1980
     1 MUHU3,NOCPA,IVSP11,INGC24,XDAY,XPOW,XKAY                         CH1 1990
C                                                                       CH1 2000
      IF(NIAVC .NE. 0) IAVC = NIAVC                                     CH1 2010
      IF(IAVC .LT. 0) IAVC = 0                                          CH1 2020
      IF(IXEN .GT. 0) IVSP(28) = IXEN                                   CH1 2030
      IF(NNSTOP .GT. 0) JNSTOP = NNSTOP                                 CH1 2040
      IF(NNUM .GT. 0) JNUM = NNUM                                       CH1 2050
      IF(XDAY .GT. 0.) DELDAY = XDAY                                    CH1 2060
      IF(XPOW .GT. 0.) POWER = XPOW                                     CH1 2070
      IF(XKAY .GT. 0.) ZKFIND = XKAY                                    CH1 2080
      IF(IBUC .NE. 0) IBUCK = IBUC                                      CH1 2090
      IF(IBUC .EQ. 3) IBUCK = 0                                         CH1 2100
      IF(MUHU3 .NE. 0) MUHU(3) = MUHU3                                  CH1 2110
      IF(MUHU3 .EQ. 3) MUHU(3) = 0                                      CH1 2120
      IF(NOCPA .LT. 0) JSER = 1                                         CH1 2130
      IF(IVSP11 .GT. 0) IVSP(11) = IVSP11                               CH1 2140
      IF(IVSP11 .LT. 0) IVSP(11) = 0                                    CH1 2150
    3 CONTINUE                                                          CH1 2160
      PRO(299) = FLOAT(IPRIN(15)) - 1.                                  CH1 2170
      JNSTOP = IABS(JNSTOP)                                             CH1 2180
      WRITE (6,506) (IPRIN(I),I=1,4),JNSTOP,JNUM,IBUCK,MUHU(3),DELDAY,  CH1 2190
     1 POWER,ZKFIND,TDOWN                                               CH1 2200
      IF(MUHU(3) .EQ. 1) WRITE (NT,2009)                                CH1 2210
      IF(MUHU(3) .EQ. 2) WRITE (NT,2010)                                CH1 2220
      MAFIA = 4                                                         CH1 2230
      IF(IPRIN(4) .GT. 0 .AND. ISPEKT(1) .EQ. 0) GOTO 60                CH1 2240
      IPRIN(10) = -1                                                    CH1 2250
      MAFIA = 6                                                         CH1 2260
   60 CONTINUE                                                          CH1 2270
      RETURN                                                            CH1 2280
   11 CONTINUE                                                          CH1 2290
      AVO = 0.6022045                                                   CH1 2300
C                                                                       CH1 2310
      CALL ADMAIN(A(KA(LIMAT)))                                         CH1 2320
C                                                                       CH1 2330
      CALL INPUTA(A(KA(LIMAT)),A(KA(LOUSI)),KL(LOUSI),A(KA(LTOSI)),     CH1 2340
     1 A(KA(LABSI)),A(KA(LFISI)),KL(LFISI),A(KA(LXNU)),A(KA(LBUG)))     CH1 2350
C                                                                       CH1 2360
      CALL INIT(A(KA(LIMAT)),A(KA(LAWT)))                               CH1 2370
C                                                                       CH1 2380
      NP1 = NENDP + 1                                                   CH1 2390
      NP2 = NP1 + NDR * 3                                               CH1 2400
      NP3 = NP1 + NDR * 6                                               CH1 2410
      NP4 = NP1 + NDR * 9                                               CH1 2420
      NP5 = NP1 + NDR * 12                                              CH1 2430
C                                                                       CH1 2440
      CALL INPUTB(A(KA(LUMRE)),A(KA(LVOLR)),IREG,A(KA(LNBUR)),          CH1 2450
     1 A(KA(LVLP)),SSLUMP,A(KA(LPOIM)),A(KA(LPOIL)),A(KA(LPINI)),       CH1 2460
     2 A(KA(LPINA)),A(KA(LNPOI)),A(KA(LVFRE)),NHOT,VOL,A(KA(LVPAR)),DEN,CH1 2470
     3 A(KA(LNRG)),NOPOW,A(KA(LANU)),A(KA(LA1)),A(KA(LA2)),A(KA(LA3)),  CH1 2480
     4 A(KA(LA4)),A(KA(LA5)),A(NP1),A(NP2),A(NP3),A(NP4),A(NP5),        CH1 2490
     5 A(KA(LNRY)),MIX,A(KA(LNOPK)))                                    CH1 2500
C                                                                       CH1 2510
      IPRIN(12) = JN                                                    CH1 2520
C                                                                       CH1 2530
CARD V15                                                                CH1 2540
C                                                                       CH1 2550
      READ (NS,110)(IPRIN(L),L=1,4),IPRINO                              CH1 2560
C                                                                       CH1 2570
C     SPEKTRAL RECHNUNG                                                 CH1 2580
C                                                                       CH1 2590
CARD V16                                                                CH1 2600
C                                                                       CH1 2610
      READ (NS,110) (ISPEKT(J),J=1,18)                                  CH1 2620
C                                                                       CH1 2630
      ISPEKT(20) = 0                                                    CH1 2640
      DO 202 J=1,18                                                     CH1 2650
        IF(ISPEKT(J) .NE. 0) ISPEKT(20) = J                             CH1 2660
  202 CONTINUE                                                          CH1 2670
      IF(MUHU(28) .GT. 0) GOTO 206                                      CH1 2680
C                                                                       CH1 2690
C     DIFFUSIONS-RECHNUNG                                               CH1 2700
C                                                                       CH1 2710
CARD V17                                                                CH1 2720
C                                                                       CH1 2730
      READ (NS,110) (IDIFF(J),J=1,18)                                   CH1 2740
C                                                                       CH1 2750
      IDIFF(20) = 0                                                     CH1 2760
      DO 203 J=1,18                                                     CH1 2770
        IF(IDIFF(J) .NE. 0) IDIFF(20) = J                               CH1 2780
  203 CONTINUE                                                          CH1 2790
      IVSP(11) = 1                                                      CH1 2800
  206 CONTINUE                                                          CH1 2810
C                                                                       CH1 2820
C     CALCULATE AVERAGE DENSITIES FOR SPECTRUM CALCULATIONS             CH1 2830
C     WRITE ATOM DENSITIES ON DIRECT ACCESS UNIT NDA11                  CH1 2840
C                                                                       CH1 2850
      IE = N26                                                          CH1 2860
      NRO = IACT + 2 + NO                                               CH1 2870
      NRL = NRO + NLUM                                                  CH1 2880
      DO 105 N=1,NXS                                                    CH1 2890
        KR = 1                                                          CH1 2900
        IRRE2 = IREG(KR)                                                CH1 2910
        KREG = KR                                                       CH1 2920
        B = 0.                                                          CH1 2930
        DO 100 M=1,KMAT                                                 CH1 2940
          CONC(M) = 0.0                                                 CH1 2950
  100   CONTINUE                                                        CH1 2960
        DO 102 IR=1,N200                                                CH1 2970
          IXS = NHOT(IR)                                                CH1 2980
          IF(IXS .NE. N) GOTO 102                                       CH1 2990
          IF(NLUM .EQ. 0) GOTO 108                                      CH1 3000
          IRSUB = IR                                                    CH1 3010
  109     CONTINUE                                                      CH1 3020
          IF(IR. LE. IRRE2) GOTO 106                                    CH1 3030
          KR = KR + 1                                                   CH1 3040
          IRRE2 = IREG(KR)                                              CH1 3050
          KREG = KR                                                     CH1 3060
          GOTO 109                                                      CH1 3070
  106     CONTINUE                                                      CH1 3080
  108     CONTINUE                                                      CH1 3090
          B = B + VOL(IR)                                               CH1 3100
          DO 101 M=1,KMAT                                               CH1 3110
            IF(M .LE. NRO .OR . M .GT . NRL) GOTO 107                   CH1 3120
            CONC(M) = CONC(M) + DEN(M,IR) * VOL(IR) * SSLUMP(IE,KR)     CH1 3130
            GOTO 101                                                    CH1 3140
  107       CONTINUE                                                    CH1 3150
            CONC(M) = CONC(M) + DEN(M,IR) * VOL(IR)                     CH1 3160
  101     CONTINUE                                                      CH1 3170
  102   CONTINUE                                                        CH1 3180
        DO 103 M=1,KMAT                                                 CH1 3190
          CONC(M) = CONC(M) / B                                         CH1 3200
  103   CONTINUE                                                        CH1 3210
        JSATZ = 2 + N                                                   CH1 3220
        IF(JAD11(JSATZ) .EQ. 0) JAD11(JSATZ) = JSUM11                   CH1 3230
        NXT11 = JAD11(JSATZ)                                            CH1 3240
        WRITE (NDA11,REC=NXT11) (CONC(M),M=1,KMAT)                      CH1 3250
        NXT11 = NXT11 + 1                                               CH1 3260
        IF(JSUM11 .LT. NXT11) JSUM11 = NXT11                            CH1 3270
  105 CONTINUE                                                          CH1 3280
C                                                                       CH1 3290
C     DATEN AUSDRUCKEN                                                  CH1 3300
C                                                                       CH1 3310
      IF(I3D .GT. 0) GOTO 1321                                          CH1 3320
      NRAD = IABS(IFIX(RADR(20)))                                       CH1 3330
      WRITE (NT,1502)                                                   CH1 3340
      DO 1320 IR=1,NRAD                                                 CH1 3350
        WRITE (NT,1500) IR,RADR(IR)                                     CH1 3360
 1320 CONTINUE                                                          CH1 3370
      WRITE (NT,1503) RINN                                              CH1 3380
 1321 CONTINUE                                                          CH1 3390
      IF(IPRIN(4) .NE. 0) GOTO 2020                                     CH1 3400
      WRITE (NT,2003)                                                   CH1 3410
      GOTO 2050                                                         CH1 3420
 2020 CONTINUE                                                          CH1 3430
      IF(ISPEKT(20) .GT. 0) GOTO 2030                                   CH1 3440
      WRITE (NT,2004)                                                   CH1 3450
      GOTO 2050                                                         CH1 3460
 2030 CONTINUE                                                          CH1 3470
      JNSP = 0                                                          CH1 3480
      DO 2040 I=1,18                                                    CH1 3490
        IF(ISPEKT(I) .LE. 0) GOTO 2040                                  CH1 3500
        JNSP = JNSP + 1                                                 CH1 3510
        ISPWR(JNSP) = ISPEKT(I)                                         CH1 3520
 2040 CONTINUE                                                          CH1 3530
      IF(JNSP .EQ. 0) GOTO 2045                                         CH1 3540
      WRITE (NT,2005)                                                   CH1 3550
      WRITE (NT,2006) (ISPWR(I),I=1,JNSP)                               CH1 3560
      JNSP = 0                                                          CH1 3570
 2045 CONTINUE                                                          CH1 3580
      DO 2035 I=1,18                                                    CH1 3590
        IF(ISPEKT(I) .GE. 0) GOTO 2035                                  CH1 3600
        JNSP = JNSP + 1                                                 CH1 3610
        ISPWR(JNSP) = -ISPEKT(I)                                        CH1 3620
 2035 CONTINUE                                                          CH1 3630
      IF(JNSP .EQ. 0) GOTO 2050                                         CH1 3640
      WRITE (NT,2007)                                                   CH1 3650
      WRITE (NT,2006) (ISPWR(I),I=1,JNSP)                               CH1 3660
 2050 CONTINUE                                                          CH1 3670
      JNSP = 1                                                          CH1 3680
      FITOT = 0.0                                                       CH1 3690
      DO 204 IR=1,N200                                                  CH1 3700
        HMETAL(IR) = 0.                                                 CH1 3710
        IF(NOPOW(IR) .GT. 0) GOTO 204                                   CH1 3720
        DO 104 M=1,IACT                                                 CH1 3730
          HMNULL(M,IR) = DEN(M,IR)                                      CH1 3740
          HMETAL(IR) = HMETAL(IR) + DEN(M,IR)                           CH1 3750
  104   CONTINUE                                                        CH1 3760
        FITOT = FITOT + VOL(IR) * (DEN(4,IR)+DEN(6,IR)+DEN(16,IR)+      CH1 3770
     1   DEN(18,IR))                                                    CH1 3780
  204 CONTINUE                                                          CH1 3790
      FIFA = (((8.64*POWER)*FIWATT)/FITOT) * 1.0E-20                    CH1 3800
      WRITE (NT,205) FIFA                                               CH1 3810
      FIFA = 1.0                                                        CH1 3820
      WRITE (NT,300) N200,(HMETAL(IR),IR=1,N200)                        CH1 3830
      MAFIA = 2                                                         CH1 3840
      RETURN                                                            CH1 3850
      END                                                               CH1 3860
      SUBROUTINE COVER(NT)                                              COV   10
C                                                                       COV   20
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300)                      COV   30
C                                                                       COV   40
 1000 FORMAT ('1'////)                                                  COV   50
 1001 FORMAT (//////////23X,'***',10X,'***',9X,'************',9X,'******COV   60
     1*****',9X,'************',/23X,'***',10X,'***',8X,'*************',8COV   70
     2X,'*************',8X,'*************',/23X,'***',10X,'***',8X,'****COV   80
     3',17X,'****',5X,'****',8X,'***',6X,'****',/23X,'***',10X,'***',8X,COV   90
     4'***',18X,'***',7X,'***',8X,'***',7X,'***',/24X,'***',8X,'***',9X,COV  100
     5'****',17X,'***',7X,'***',8X,'***',6X,'****',/24X,'***',8X,'***',9COV  110
     6X,'************',9X,'***',7X,'***',8X,'*************',/25X,'***',6COV  120
     7X,'***',11X,'************',8X,'***',7X,'***',8X,'************',/25COV  130
     8X,'***',6X,'***',19X,'****',8X,'***',7X,'***',8X,'***',/26X,'***',COV  140
     94X,'***',21X,'***',8X,'***',7X,'***',8X,'***')                    COV  150
 1002 FORMAT (///////////22X,' VERSION: ............. VSOP(99/11),  REPOCOV  160
     1RTS: JUEL - 4348, JAN. 2012, JUEL - 4326, JUNE 2010'/)            COV  170
 1003 FORMAT (/////56X,'( ',I2,'.',I2,'.',I4,' )')                      COV  180
 1004 FORMAT (27X,'***',2X,'***',21X,'****',8X,'****',5X,'****',8X,'***'COV  190
     1 /28X,'******',7X,'***',3X,'*************',2X,'***',3X,'**********COV  200
     2***',2X,'***',3X,'***',12X,'***'/29X,'****',8X,'***',3X,'*********COV  210
     3***',3X,'***',4X,'***********',3X,'***',3X,'***',12X,'***')       COV  220
C                                                                       COV  230
C                                                                       COV  240
      WRITE (NT,1000)                                                   COV  250
      WRITE (NT,1001)                                                   COV  260
      WRITE (NT,1004)                                                   COV  270
      WRITE (NT,1003) (INZW(J),J=5,7)                                   COV  280
      WRITE (NT,1002)                                                   COV  290
      RETURN                                                            COV  300
      END                                                               COV  310
      SUBROUTINE STREAM(ST,STRM)                                        TRE   10
C                                                                       TRE   20
C     STREAMING CORRECTION  NACH LIEBEROTH                              TRE   30
C                                                                       TRE   40
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1TRE   50
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         TRE   60
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    TRE   70
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)TRE   80
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  TRE   90
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    TRE  100
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         TRE  110
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,TRE  120
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            TRE  130
C                                                                       TRE  140
C                                                                       TRE  150
      F = FF(1)                                                         TRE  160
      R = FF(4)                                                         TRE  170
      Q = 1.96716                                                       TRE  180
      IF(F .EQ. 0.61) GOTO 2                                            TRE  190
      Q1 = 1.9609                                                       TRE  200
      Q2 = 1.9695                                                       TRE  210
      Q3 = 1.9876                                                       TRE  220
      F1 = 0.52360                                                      TRE  230
      F2 = 0.63080                                                      TRE  240
      F3 = 0.74050                                                      TRE  250
      D = F1 * F2 * (F2-F1) + F1 * F3 * (F1-F3) + F2 * F3 * (F3-F2)     TRE  260
      A = (Q3*F1*F2*(F2-F1)+Q2*F1*F3*(F1-F3)+Q1*F2*F3*(F3-F2)) / D      TRE  270
      B = (Q3*(F1**2-F2**2)+Q2*(F3**2-F1**2)+Q1*(F2**2-F3**2)) / D      TRE  280
      C = (Q3*(F2-F1)+Q2*(F1-F3)+Q1*(F3-F2)) / D                        TRE  290
      Q = A + B * F + C * F**2                                          TRE  300
    2 CONTINUE                                                          TRE  310
      ALF = 2. * R * ST / F                                             TRE  320
      ALF3 = ALF / 3.                                                   TRE  330
      P = 1. - (1.+ALF) * EXP(-ALF)                                     TRE  340
      STRM = 1. + (1.-F)**2 * (ALF3*Q-1.+2.*ALF3*P/(0.5*ALF**2-P))      TRE  350
      RETURN                                                            TRE  360
      END                                                               TRE  370
      SUBROUTINE INPUTA(IMAT,OUSIG,LA0,TOSIG,ABSIG,FISIG,LF0,XNU,BUG)   NPU   10
C                                                                       NPU   20
C     READS AND PRINTS LIBRARY DATA                                     NPU   30
C                                                                       NPU   40
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    NPU   50
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    NPU   60
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PINPU   70
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 NPU   80
C                                                                       NPU   90
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1NPU  100
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         NPU  110
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    NPU  120
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)NPU  130
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  NPU  140
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    NPU  150
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         NPU  160
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,NPU  170
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            NPU  180
C                                                                       NPU  190
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, NPU  200
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        NPU  210
     2 FIMAKL(20),NOPILE,MREP,MARX(10)                                  NPU  220
C                                                                       NPU  230
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), NPU  240
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10NPU  250
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11NPU  260
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         NPU  270
C                                                                       NPU  280
      COMMON /HILF/ JMAT                                                NPU  290
C                                                                       NPU  300
      COMMON /NUCNAM/ T1(200),T2(200),GR(2),PU,FL,MATNR(200),WGN(200)   NPU  310
C                                                                       NPU  320
CFZJ031                                                       28.05.04  NPU  330
CFZJ062                                                       04.05.11  NPU  340
      COMMON /ORIGEN/ LOB,NOR,VOR(100),ISPK,KFISS,N200C,NXSC,KGR,       NPU  350
     1 LOBN,IEZ,FSP(33),IDOP,JNEU(15),LMAT(15)                          NPU  360
C                                                                       NPU  370
      COMMON /FLUXN/ D(361),IACT                                        NPU  380
C                                                                       NPU  390
      CHARACTER*4 T1,T2,GR,PU,FL                                        NPU  400
C                                                                       NPU  410
      DIMENSION IMAT(KMAT),OUSIG(LA0),TOSIG(LA0),ABSIG(LA0),FISIG(LF0), NPU  420
     1 XNU(LF0),BUG(N26),XLAM(49),XKETTE(4,44)                          NPU  430
C                                                                       NPU  440
      EQUIVALENCE(DIRAC(1,5),XLAM(1)),(XKETTE(1,1),XKET44(1,1)),        NPU  450
     1 (JTPE2,NS),(JTPE3,NT)                                            NPU  460
C                                                                       NPU  470
      REAL XKET44(4,44)/4*0.,4*0.,0.,+1.,2*0.,4*0.,4*0.,-1.,3*0.,4*0.,  NPU  480
     1 4*0.,4*0.,4*0.,-1.,3*0.,4*0.,-1.,3*0.,4*0.,+1.,3*0.,4*0.,4*0.,-1.NPU  490
     2 ,3*0.,4*0.,-1.,3*0.,+1.,3*0.,4*0.,4*0.,-1.,3*0.,+1.,+1.,2*0.,+1.,NPU  500
     3 3*0.,+1.,3*0.,+1.,3*0.,0.473,3*0.,-0.065,0.527,2*0.,2*0.,-1.,0., NPU  510
     4 +1.,-1.,-0.935,0.,2*0.,+1.,+1.,-1.,+1.,2*0.,+1.,3*0.,4*0.,-1.,+1.NPU  520
     5 ,2*0.,+1.,3*0.,+1.,3*0.,1.,3*0.,1.,3*0.,-1.,3*0.,1.,1.,2*0.,1.,  NPU  530
     6 3*0./,XIRAC(4)/4*0./,SUM(4)/4*0./                                NPU  540
C                                                                       NPU  550
      INTEGER KMOR(4)/4*0/                                              NPU  560
C                                                                       NPU  570
      CHARACTER*4 HOL1(4),HOL2(4),HBLANK(2)/'    ','    '/,HDEC(2)/     NPU  580
     1 'DECA','Y  :'/,HCAP(2)/'CAPT','URE:'/                            NPU  590
C                                                                       NPU  600
    1 FORMAT (///' THE FIXED LIBRARY CONTAINS THE FOLLOWING DATA'//'NU*FNPU  610
     1ISSION  TRANSPORT   ABSORPTION SCATTER OUT NEUTS/FISSION')        NPU  620
    2 FORMAT (10H0BLOCK NO.,I4)                                         NPU  630
    3 FORMAT (I4,68H                                                    NPU  640
     1                )                                                 NPU  650
    4 FORMAT (18I4)                                                     NPU  660
    5 FORMAT (4E12.5)                                                   NPU  670
    6 FORMAT (5E12.5)                                                   NPU  680
    7 FORMAT ('    MATERIAL          DECAY                   FISSION YIENPU  690
     1LDS'/' VSOP-NO  NUCL.      CONSTANT   FROM U233   FROM U235   FROMNPU  700
     2 PU239   FROM PU241'/)                                            NPU  710
    8 FORMAT (/' SUM OF ALL EXPLICIT'/' FISSION PRODUCTS:',12X,4E12.4)  NPU  720
    9 FORMAT (I5,4X,2A4,1X,5E12.4)                                      NPU  730
   10 FORMAT ('1'/45X,'BUILD-UP OF FISSION PRODUCT CHAIN'/45X,33('=')//'NPU  740
     1 A FISSION PRODUCT MAY HAVE 1 TO 4 PARENTS FROM WHICH IT ORIGINATENPU  750
     2S THROUGH CAPTURE OR DECAY WITH A RELATIVE YIELD'///'    MATERIAL NPU  760
     3',5X,'PARENT1         YIELD',8X,'PARENT2         YIELD',8X,'PARENTNPU  770
     43         YIELD',8X,'PARENT4         YIELD'/' VSOP-NO  NUCL.'//)  NPU  780
   11 FORMAT (I5,4X,2A4,1X,I2,1X,2A4,G10.5,3(8X,I2,1X,2A4,G10.5))       NPU  790
   12 FORMAT (///9X,'VSOP MATERIALS ALLOCATION'/9X,25('=')//' VSOP MAT NNPU  800
     1O.',5X,'GAM-LIB. ID.',5X,'MATERIAL'//)                            NPU  810
   13 FORMAT (1X,43('.'))                                               NPU  820
   14 FORMAT (I8,14X,I3,11X,2A4)                                        NPU  830
   15 FORMAT ('1')                                                      NPU  840
   16 FORMAT (I6,6X,5E12.5)                                             NPU  850
   17 FORMAT (/I2,':  NEW NUCLIDE',I5,'  DUPLICATED FROM',I5)           NPU  860
C                                                                       NPU  870
C                                                                       NPU  880
      IPAGE = 0                                                         NPU  890
      IDKET = 44                                                        NPU  900
      NLUM = 0                                                          NPU  910
C                                                                       NPU  920
CARD V2                                                                 NPU  930
C                                                                       NPU  940
      READ (NS,4) NO,KETT,NLT,NC                                        NPU  950
C                                                                       NPU  960
      IF(NO .LE. 0) NO = IDKET                                          NPU  970
      KETTE = NO - KETT                                                 NPU  980
      NKET = IABS(KETTE)                                                NPU  990
      IPRIN(14) = IACT + NO                                             NPU 1000
      READ (NDA29,REC=NXT29) KDUM,(IMAT(L+IACT),L=1,KDUM)               NPU 1010
      NXT29 = NXT29 + 1                                                 NPU 1020
      IDOP = 0                                                          NPU 1030
      DO 31 L=1,KMAT                                                    NPU 1040
        NADD = IMAT(L)                                                  NPU 1050
        IF(NADD .LE. 190) GOTO 31                                       NPU 1060
        IDOP = IDOP + 1                                                 NPU 1070
   31 CONTINUE                                                          NPU 1080
      WRITE (NT,15)                                                     NPU 1090
      IF(IDOP .EQ. 0) GOTO 33                                           NPU 1100
      DO 32 L=1,IDOP                                                    NPU 1110
C                                                                       NPU 1120
CARD V3                                                                 NPU 1130
C                                                                       NPU 1140
        READ (NS,4) JNEU(L),LMAT(L)                                     NPU 1150
C                                                                       NPU 1160
        WRITE (NT,17) L,JNEU(L),LMAT(L)                                 NPU 1170
   32 CONTINUE                                                          NPU 1180
   33 CONTINUE                                                          NPU 1190
      DO 110 IE=1,N26                                                   NPU 1200
        BUG(IE) = 1.0                                                   NPU 1210
  110 CONTINUE                                                          NPU 1220
      IL = 1                                                            NPU 1230
      IF(NKET .LE. 0) GOTO 125                                          NPU 1240
      KK = 0                                                            NPU 1250
      IMAT(IACT+2) = 160                                                NPU 1260
      IL = 3                                                            NPU 1270
      DO 120 I=1,NKET                                                   NPU 1280
        K = I + KK                                                      NPU 1290
        DO 120 J=1,4                                                    NPU 1300
          DIRAC(I,J) = XKETTE(J,K)                                      NPU 1310
  120 CONTINUE                                                          NPU 1320
  125 CONTINUE                                                          NPU 1330
      WRITE (NT,12)                                                     NPU 1340
      DO 126 L=1,KMAT                                                   NPU 1350
        IML = IMAT(L)                                                   NPU 1360
        IMJ = IML                                                       NPU 1370
        IF(IMJ .GT. 190) IMJ = LMAT(IMJ-190)                            NPU 1380
        MATNR(L) = IMAT(L)                                              NPU 1390
        WRITE (NT,14) L,IML,T1(IMJ),T2(IMJ)                             NPU 1400
        IF(L .EQ. IACT .OR. L .EQ. (IACT+NO) .OR. L .EQ. (IACT+NO+NLUM) NPU 1410
     1   .OR. L .EQ. (IACT+NO+NLUM+NC)) WRITE (NT,13)                   NPU 1420
  126 CONTINUE                                                          NPU 1430
      IF(NKET .GE. NO) GOTO 130                                         NPU 1440
      NKET1 = NKET + 1                                                  NPU 1450
C                                                                       NPU 1460
CARD V4                                                                 NPU 1470
C                                                                       NPU 1480
      READ (NS,5) ((DIRAC(K,I),I=1,4),K=NKET1,NO)                       NPU 1490
C                                                                       NPU 1500
  130 CONTINUE                                                          NPU 1510
      WRITE (NT,10)                                                     NPU 1520
      DO 140 K=1,NO                                                     NPU 1530
        KIK = K + IACT                                                  NPU 1540
        DO 135 J=1,4                                                    NPU 1550
          KMOR(J) = KIK - J                                             NPU 1560
          IF(KMOR(J) .LE. IACT) KMOR(J) = 0                             NPU 1570
          HOL1(J) = HBLANK(1)                                           NPU 1580
          HOL2(J) = HBLANK(2)                                           NPU 1590
          XIRAC(J) = ABS(DIRAC(K,J))                                    NPU 1600
          IF(DIRAC(K,J) .EQ. 0.) GOTO 135                               NPU 1610
          IF(DIRAC(K,J) .GT. 0.) GOTO 132                               NPU 1620
          HOL1(J) = HDEC(1)                                             NPU 1630
          HOL2(J) = HDEC(2)                                             NPU 1640
          GOTO 135                                                      NPU 1650
  132     CONTINUE                                                      NPU 1660
          HOL1(J) = HCAP(1)                                             NPU 1670
          HOL2(J) = HCAP(2)                                             NPU 1680
  135   CONTINUE                                                        NPU 1690
        NONU = IMAT(KIK)                                                NPU 1700
        WRITE (NT,11) KIK,T1(NONU),T2(NONU),(KMOR(J),HOL1(J),HOL2(J),   NPU 1710
     1   XIRAC(J),J=1,4)                                                NPU 1720
  140 CONTINUE                                                          NPU 1730
C                                                                       NPU 1740
      CALL YIELDS(IMAT)                                                 NPU 1750
C                                                                       NPU 1760
      IF(NLT .LE. 0) GOTO 200                                           NPU 1770
      DO 170 I=1,NLT                                                    NPU 1780
C                                                                       NPU 1790
CARD V5                                                                 NPU 1800
C                                                                       NPU 1810
        READ (NS,16) KK,YIELD1(KK-IACT),YIELD2(KK-IACT),YIELD3(KK-IACT),NPU 1820
     1   YIELD4(KK-IACT),XLAM(KK-IACT)                                  NPU 1830
C                                                                       NPU 1840
  170 CONTINUE                                                          NPU 1850
  200 CONTINUE                                                          NPU 1860
      WRITE (NT,15)                                                     NPU 1870
      WRITE (NT,7)                                                      NPU 1880
      DO 210 K=1,NO                                                     NPU 1890
        KK = IACT + K                                                   NPU 1900
        IF(KK .EQ. IACT+2) GOTO 209                                     NPU 1910
        SUM(1) = SUM(1) + YIELD1(K)                                     NPU 1920
        SUM(2) = SUM(2) + YIELD2(K)                                     NPU 1930
        SUM(3) = SUM(3) + YIELD3(K)                                     NPU 1940
        SUM(4) = SUM(4) + YIELD4(K)                                     NPU 1950
  209   CONTINUE                                                        NPU 1960
        NONU = IMAT(KK)                                                 NPU 1970
        WRITE (NT,9) KK,T1(NONU),T2(NONU),XLAM(K),YIELD1(K),YIELD2(K),  NPU 1980
     1   YIELD3(K),YIELD4(K)                                            NPU 1990
  210 CONTINUE                                                          NPU 2000
      WRITE (NT,8) (SUM(I),I=1,4)                                       NPU 2010
      RETURN                                                            NPU 2020
      END                                                               NPU 2030
      SUBROUTINE INPUTB(UMREG,VOLREG,IREG,NBURN,VLP,SSLUMP,POISM,POISL, PUT   10
     1 PINMIN,PINMAX,NPOIS,VFREG,NHOT,VOL,VPART,DEN,NRG,NOPOW,A0,A1,A2, PUT   20
     2 A3,A4,A5,RODS,DILP,DOLP,HLP,NFTV,NRY,MIX,NOPOWK)                 PUT   30
C                                                                       PUT   40
C     READS CASE INPUT DATA                                             PUT   50
C                                                                       PUT   60
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    PUT   70
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    PUT   80
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIPUT   90
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 PUT  100
C                                                                       PUT  110
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1PUT  120
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         PUT  130
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    PUT  140
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)PUT  150
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  PUT  160
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    PUT  170
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         PUT  180
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,PUT  190
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            PUT  200
C                                                                       PUT  210
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, PUT  220
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        PUT  230
     2 FIMAKL(20),NOPILE,MREP,MARX(10)                                  PUT  240
C                                                                       PUT  250
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), PUT  260
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10PUT  270
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11PUT  280
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13,JSUM29                  PUT  290
C                                                                       PUT  300
      COMMON /NUCNAM/ T1(200),T2(200),GR(2),PU,FL,MATNR(200),WGN(200)   PUT  310
C                                                                       PUT  320
CFZJ062                                                    4.05.11      PUT  330
      COMMON /ORIGEN/ LOB,NOR,VOR(100),ISPK,KFISS,N200C,NXSC,KGR,       PUT  340
     1 LOBN,IEZ                                                         PUT  350
C                                                                       PUT  360
      COMMON /BLOCKK/ DU(3),AVO                                         PUT  370
C                                                                       PUT  380
      COMMON /FLUXN/ D(361),IACT                                        PUT  390
C                                                                       PUT  400
      COMMON /IPO/ IPRINO,INAK,LNAK,JNT                                 PUT  410
C                                                                       PUT  420
      COMMON /FLEX/ FLUX,MMN,MOUT,INDEX,QXN,AXN,ERR,NDUMMY,MZERO,ITMAX, PUT  430
     1 ILMAX,IAMAX,IFMAX,IZMAX                                          PUT  440
C                                                                       PUT  450
      CHARACTER*4 T1,T2,GR,PU,FL                                        PUT  460
C                                                                       PUT  470
      DIMENSION UMREG(NDR),VOLREG(NDR),IREG(NDR),NBURN(NDR),VLP(3,NDR), PUT  480
     1 SSLUMP(N26,NDR),POISM(2,NDR),POISL(2,NDR),PINMIN(NDR),PINMAX(NDR)PUT  490
     2 ,NPOIS(NDR),VFREG(NDR),NHOT(N200),VOL(N200),VPART(N200),         PUT  500
     3 DEN(KMAT,N200),NRG(N200),NOPOW(N200),A0(N26),A1(N26),A2(N26),    PUT  510
     4 A3(N26),A4(N26),A5(N26),RODS(3,NDR),DILP(3,NDR),DOLP(3,NDR),     PUT  520
     5 HLP(3,NDR),NFTV(N200),NRY(N200),MIX(NDR),NFT(30),DAT(2),KD(2,8), PUT  530
     6 DENIR(200),NOPOWK(KROT)                                          PUT  540
C                                                                       PUT  550
      EQUIVALENCE(IPRIN(13),NKOST),(JTPE2,NS),(JTPE3,NT)                PUT  560
C                                                                       PUT  570
      DATA FCP018/3HREG/,FCP019/3HION/                                  PUT  580
C                                                                       PUT  590
    2 FORMAT (18I4)                                                     PUT  600
    3 FORMAT (6E12.5)                                                   PUT  610
CFZJ037                                                       30.09.04  PUT  620
    4 FORMAT (I8,6I4,2E12.5)                                            PUT  630
CFZJ042                                                       09.09.05  PUT  640
    5 FORMAT (4I6,6X,E12.5,18X,2I6)                                     PUT  650
    7 FORMAT (I4,4X,E12.5)                                              PUT  660
  101 FORMAT (/' FUEL TYPE',I6,'  ON DATA-2-LIBRARY NOT EXISTENT. STOP')PUT  670
  403 FORMAT (/' CONCENTRATIONS FOR FIRST-CORE ARE READ COMPLETELY OR INPUT  680
     1 PARTS FROM DIRECT-ACCESS UNIT NO.',I3/)                          PUT  690
  708 FORMAT (1H0,7X,64HCORE HEIGHT     POWER(WATTS)     FISS/WATT-SEC  PUT  700
     1                //3(1PE18.5)//73H0MIN OR SEARCH KEFF  TIME STEP(DAPUT  710
     2YS)    MAX TIME STEPS  SMALL/LARGE STEPS//2(1PE18.5),I13,I17)     PUT  720
  713 FORMAT ('1',14X,'...... MATERIAL SUMMARY ......'//' BATCH  REGION PUT  730
     1 CONTR.    VOLUME    SIGMA     FUEL      NO-'/'                POIPUT  740
     2SON   FRACTION    SET   TYPE  VAR.  POWER'/)                      PUT  750
  714 FORMAT (I5,I7,I8,E14.5,2I6,I5,I7)                                 PUT  760
  720 FORMAT (20H1 LUMPED POISON DATA/'0REG MAT    RODS        I.D.     PUT  770
     1  O.D.        LENGTH       VOLUME     DENSITY    VOL.RATIO      SSPUT  780
     2LUMP '//)                                                         PUT  790
  722 FORMAT (2I4,8E12.4)                                               PUT  800
  723 FORMAT (78H0  EGRP   COEFF 0     COEFF 1     COEFF 2      COEFF 3 PUT  810
     1    COEFF 4     COEFF 5//)                                        PUT  820
  724 FORMAT (70H0    MAX TOTAL ADJ.    MAX ADJ (ONE REG)    NO.REG.ADJ.PUT  830
     1SIMULTANEOUSLY //I11,I21,I25/68H0 ORDER OF USE    CONTROL REG    MPUT  840
     1AX INSERT.FRACT  MIN INSERT.FRACT //(I8,I16,5X,2(1PE18.5)))       PUT  850
  725 FORMAT (I8,6E12.5)                                                PUT  860
  777 FORMAT ('1',5X,'****   WARNING  ***    IN CORE REGION ',I4,' THE PPUT  870
     1ARTIAL VOLUMINA DO NOT SUM UP TO 1.0 :'/'0     THE TOTAL SUM EQUALPUT  880
     2S ',G14.6)                                                        PUT  890
  999 FORMAT (///' *** PROGRAM STOPS BECAUSE IN THE CORE BATCH',I5,' NO PUT  900
     1FUEL TYPE NO. IS DEFINED. SEE INPUT CARD V7. ***')                PUT  910
 1724 FORMAT (21H1 CONTROL POISON DATA//3X,2A3,48H  MATERIAL   MAX ATOM PUT  920
     1DENSITY   MIN ATOM DENSITY//)                                     PUT  930
 1725 FORMAT (I7,I9,2(1PE19.5))                                         PUT  940
 2009 FORMAT (//' *** STREAMING CORRECTION OF THE DIFFUSION CONSTANT ACCPUT  950
     1ORDING SCHERER-GERWIN ***')                                       PUT  960
 2010 FORMAT (//' *** STREAMING CORRECTION OF THE DIFFUSION CONSTANT ACCPUT  970
     1ORDING J.LIEBEROTH,A.STOJADINOVIC: NUCL.SCI.ENG. 76, 336-344 (1980PUT  980
     2) ***')                                                           PUT  990
C                                                                       PUT 1000
C                                                                       PUT 1010
      N200C = 0                                                         PUT 1020
      NXSC = 0                                                          PUT 1030
      ICOR = 0                                                          PUT 1040
      JCOR = 0                                                          PUT 1050
      NXE = 1                                                           PUT 1060
      IXEN = 1                                                          PUT 1070
      REFNO = 1.                                                        PUT 1080
      W3 = (+FCP018)                                                    PUT 1090
      W4 = (+FCP019)                                                    PUT 1100
      PI = 3.14159                                                      PUT 1110
C                                                                       PUT 1120
      CALL HEAD(2)                                                      PUT 1130
C                                                                       PUT 1140
CARD V6                                                                 PUT 1150
C                                                                       PUT 1160
CFZJ037                                                       30.09.04  PUT 1170
      READ (NS,4) NRSTRT,NKOST,IBUCK,MUHU(3),LOBNEW,IBASCH,IPRIN2,SERCONPUT 1180
     1 ,ERR                                                             PUT 1190
C                                                                       PUT 1200
      IF(ERR .LE. 0.) ERR = 1.E-25                                      PUT 1210
      IF(LOBNEW .GT. 0) LOBN = LOBNEW                                   PUT 1220
      IF(JSER .NE. 3) GOTO 29                                           PUT 1230
      MUHU(22) = 1                                                      PUT 1240
      JSER = 0                                                          PUT 1250
   29 CONTINUE                                                          PUT 1260
      IF(JSER .LT. 4) GOTO 31                                           PUT 1270
      MUHU(28) = 1                                                      PUT 1280
      IF(JSER .EQ. 4) JSER = 1                                          PUT 1290
      IF(JSER .EQ. 5) JSER = 0                                          PUT 1300
   31 CONTINUE                                                          PUT 1310
      IF(IXEN .GT. 0) IVSP(28) = IXEN                                   PUT 1320
      IF(MUHU(3) .EQ. 1) WRITE (NT,2009)                                PUT 1330
      IF(MUHU(3) .EQ. 2) WRITE (NT,2010)                                PUT 1340
      IF(NXE) 30,35,30                                                  PUT 1350
   30 JN = 0                                                            PUT 1360
   35 CONTINUE                                                          PUT 1370
C                                                                       PUT 1380
      CALL INPUTC(VOLREG,NBURN,VFREG,VPART,NRG,NOPOW,MIX,NOPOWK)        PUT 1390
C                                                                       PUT 1400
      IF(IPRIN2 .GE. 0) WRITE (NT,713)                                  PUT 1410
C                                                                       PUT 1420
C     REGION DATA                                                       PUT 1430
C                                                                       PUT 1440
      LUMPED = 0                                                        PUT 1450
      NLUMPS = 0                                                        PUT 1460
      NCONTR = 0                                                        PUT 1470
      N33 = 0                                                           PUT 1480
      NFU = 0                                                           PUT 1490
      NVR = 0                                                           PUT 1500
      KR = 1                                                            PUT 1510
      NK = 0                                                            PUT 1520
      NDA28 = 28                                                        PUT 1530
      NX = 0                                                            PUT 1540
      LES = 1                                                           PUT 1550
      NCONU = 0                                                         PUT 1560
      DO 34 IR=1,N200                                                   PUT 1570
        IF(NOPOW(IR) .EQ. 0) JCOR = IR                                  PUT 1580
        IF(REFNO .LE. 0.) GOTO 34                                       PUT 1590
        IF(NOPOW(IR) .EQ. 0) ICOR = IR                                  PUT 1600
        IF(NOPOW(IR) .EQ. 2) NCONU = NCONU + 1                          PUT 1610
   34 CONTINUE                                                          PUT 1620
      KD(1,3) = 0                                                       PUT 1630
      KD(1,5) = 0                                                       PUT 1640
CFZJ042                                                       09.09.05  PUT 1650
      KD(1,6) = 0                                                       PUT 1660
      DO 150 IR=1,N200                                                  PUT 1670
        NFTV(IR) = 0                                                    PUT 1680
        IF(LES .NE. 1) GOTO 38                                          PUT 1690
C                                                                       PUT 1700
CARD V7                                                                 PUT 1710
C                                                                       PUT 1720
        READ (NS,5) (KD(1,K),K=1,2),KD(1,4),KD(1,7),DAT(1),MULT,KD(1,8) PUT 1730
C                                                                       PUT 1740
        KD(1,8) = KD(1,8) + MULT * IBASCH                               PUT 1750
   38   LES = 0                                                         PUT 1760
        KD18 = IABS(KD(1,8))                                            PUT 1770
        IF(KD18 .EQ. 0) GOTO 39                                         PUT 1780
        IF(NOPOW(IR) .EQ. 1) KD18 = KD18 + JCOR + NCONU                 PUT 1790
        IF(KD18 .NE. IR) GOTO 41                                        PUT 1800
   39   CONTINUE                                                        PUT 1810
        DO 40 K=1,8                                                     PUT 1820
          KD(2,K) = KD(1,K)                                             PUT 1830
   40   CONTINUE                                                        PUT 1840
        DAT(2) = DAT(1)                                                 PUT 1850
        IF(KD(1,8) .GE. 0) LES = 1                                      PUT 1860
   41   CONTINUE                                                        PUT 1870
        NREAD = KD(2,1)                                                 PUT 1880
        NCH1 = KD(2,2)                                                  PUT 1890
        NCH3 = KD(2,3)                                                  PUT 1900
        NCH5 = KD(2,4)                                                  PUT 1910
        NLUMPS = KD(2,5)                                                PUT 1920
        NFTST = KD(2,7)                                                 PUT 1930
        WPART = DAT(2)                                                  PUT 1940
        NREG = NRG(IR)                                                  PUT 1950
CFZJ042                                                       09.09.05  PUT 1960
        NHOT(IR) = NREG                                                 PUT 1970
        IF(NOPOW(IR) .GT. 0) GOTO 163                                   PUT 1980
        IF(NREG .NE. 1) GOTO 160                                        PUT 1990
        IF(WPART) 163,163,162                                           PUT 2000
  160   IF(WPART) 161,161,162                                           PUT 2010
  161   VPART(IR) = VPART(IR-MIX(NREG))                                 PUT 2020
        GOTO 163                                                        PUT 2030
  162   VPART(IR) = WPART                                               PUT 2040
  163   CONTINUE                                                        PUT 2050
        IF(NREG .GT. KR) KR = KR + 1                                    PUT 2060
        IREG(KR) = IR                                                   PUT 2070
        IF(NLUMPS .GT. 0) LUMPED = 1                                    PUT 2080
        IF(NCH1) 401,106,400                                            PUT 2090
  400   CONTINUE                                                        PUT 2100
        IF(NOPOW(IR) .EQ. 1) NCH1 = NCH1 + ICOR + NCONU                 PUT 2110
        DO 405 L=1,KMAT                                                 PUT 2120
          DEN(L,IR) = DEN(L,NCH1)                                       PUT 2130
  405   CONTINUE                                                        PUT 2140
        NFTV(IR) = NFTV(NCH1)                                           PUT 2150
        GOTO 106                                                        PUT 2160
  401   CONTINUE                                                        PUT 2170
        IA = 0                                                          PUT 2180
        IF(NCH1 .GE. 0) GOTO 8                                          PUT 2190
        IF(IA .EQ. 0) OPEN(28,ACCESS='DIRECT',RECL=L28*4,FILE='nucdens')PUT 2200
        IA = 1                                                          PUT 2210
    8   CONTINUE                                                        PUT 2220
        NXT28 = IABS(NCH1)                                              PUT 2230
        READ (NDA28,REC=NXT28) NFTV(IR)                                 PUT 2240
        NXT28 = NXT28 + 1                                               PUT 2250
C                                                                       PUT 2260
        CALL WRDA(IREAD,NDA28,NXT28,L28,DENIR,KMAT)                     PUT 2270
C                                                                       PUT 2280
        DO 1 K=1,KMAT                                                   PUT 2290
          DEN(K,IR) = DENIR(K)                                          PUT 2300
    1   CONTINUE                                                        PUT 2310
        NX = 1                                                          PUT 2320
  106   IF(NREAD) 105,114,104                                           PUT 2330
  104   CONTINUE                                                        PUT 2340
        IF(NREAD .LE. 100) GOTO 108                                     PUT 2350
        NXT29 = 2                                                       PUT 2360
        READ (NDA29,REC=NXT29) N30,(NFT(I),I=1,N30)                     PUT 2370
        NXT29 = NXT29 + 1                                               PUT 2380
        DO 102 I=1,N30                                                  PUT 2390
          II = I                                                        PUT 2400
          IF(NREAD .EQ. NFT(I)) GOTO 103                                PUT 2410
  102   CONTINUE                                                        PUT 2420
  105   CONTINUE                                                        PUT 2430
        WRITE (NT,101) NREAD                                            PUT 2440
        STOP                                                            PUT 2450
  103   CONTINUE                                                        PUT 2460
        NFTV(IR) = NREAD                                                PUT 2470
        NREAD = 1 - II                                                  PUT 2480
        NXT29 = IABS(NREAD) + 1                                         PUT 2490
        READ (NDA29,REC=NXT29) NNREAD,(LR,DEN(LR,IR),L=1,NNREAD)        PUT 2500
        NXT29 = NXT29 + 1                                               PUT 2510
        GOTO 114                                                        PUT 2520
  108   CONTINUE                                                        PUT 2530
        IF(NOPOW(IR) .EQ. 0) NFTV(IR) = NFTST * 100 + 1                 PUT 2540
        DO 3002 LR=1,NREAD                                              PUT 2550
C                                                                       PUT 2560
CARD V8                                                                 PUT 2570
C                                                                       PUT 2580
          READ (NS,7) L,DEN(L,IR)                                       PUT 2590
C                                                                       PUT 2600
 3002   CONTINUE                                                        PUT 2610
  114   CONTINUE                                                        PUT 2620
        NFU = NFTV(IR) / 100                                            PUT 2630
        NVR = NFTV(IR) - NFU * 100                                      PUT 2640
        IF(IPRIN2 .GE. 0) WRITE (NT,714) IR,KR,NC,VPART(IR),NHOT(IR),NFUPUT 2650
     1   ,NVR,NOPOW(IR)                                                 PUT 2660
        IF(NC .LE. 0) GOTO 151                                          PUT 2670
        NCONTR = NC                                                     PUT 2680
        DO 175 K=1,NCONTR                                               PUT 2690
          IF(NCH5 .LT. 0) GOTO 170                                      PUT 2700
          IF(NCH5 .EQ. 0) NCH5 = 1                                      PUT 2710
          IF(NCH5 .GT. 1) NCH5 = NRG(NCH5)                              PUT 2720
          POISM(K,KR) = POISM(K,NCH5)                                   PUT 2730
          POISL(K,KR) = POISL(K,NCH5)                                   PUT 2740
          GOTO 175                                                      PUT 2750
C                                                                       PUT 2760
CARD V9                                                                 PUT 2770
C                                                                       PUT 2780
  170     READ (NS,3) POISM(K,KR),POISL(K,KR)                           PUT 2790
C                                                                       PUT 2800
  175   CONTINUE                                                        PUT 2810
  151   CONTINUE                                                        PUT 2820
        IF(NOPOW(IR) .GT. 0) GOTO 150                                   PUT 2830
        N200C = MAX0(N200C,IR)                                          PUT 2840
        NXSC = MAX0(NXSC,NHOT(IR))                                      PUT 2850
        IF(NFU .GT. 0) GOTO 150                                         PUT 2860
        WRITE (NT,999) IR                                               PUT 2870
        STOP                                                            PUT 2880
  150 CONTINUE                                                          PUT 2890
      IF(NX .EQ. 0) GOTO 402                                            PUT 2900
      WRITE (6,403) NDA28                                               PUT 2910
  402 CONTINUE                                                          PUT 2920
      NO = NO - 2                                                       PUT 2930
      N33 = 30                                                          PUT 2940
      NXT29 = N33                                                       PUT 2950
C                                                                       PUT 2960
      CALL WRDA(IWRITE,NDA29,NXT29,L29,NFTV,N200)                       PUT 2970
C                                                                       PUT 2980
      JSUM29 = NXT29                                                    PUT 2990
C                                                                       PUT 3000
C     BURNUP DATA                                                       PUT 3010
C                                                                       PUT 3020
      FIWATT = 3.08714E+10                                              PUT 3030
      POWER = 1.0                                                       PUT 3040
      IF(JSER-1) 152,152,210                                            PUT 3050
C                                                                       PUT 3060
CARD V10                                                                PUT 3070
C                                                                       PUT 3080
  152 READ (NS,3) DELDAY,POWER,FIWAT,ZKFIND                             PUT 3090
C                                                                       PUT 3100
      IF(FIWAT .GT. 0.) FIWATT = FIWAT                                  PUT 3110
C                                                                       PUT 3120
CARD V11                                                                PUT 3130
C                                                                       PUT 3140
      READ (NS,2) JNSTOP,JNUM                                           PUT 3150
C                                                                       PUT 3160
C     CONTROL SEARCH DATA                                               PUT 3170
C                                                                       PUT 3180
      IF(JSER-1) 155,210,210                                            PUT 3190
C                                                                       PUT 3200
CARD V12                                                                PUT 3210
C                                                                       PUT 3220
  155 READ (NS,2) JSMAX,JSSMAX,LSIM,KSS,(NPOIS(IRR),IRR=1,KSS)          PUT 3230
C                                                                       PUT 3240
CARD V13                                                                PUT 3250
C                                                                       PUT 3260
      READ (NS,3) (PINMIN(IRR),IRR=1,KSS)                               PUT 3270
C                                                                       PUT 3280
CARD V14                                                                PUT 3290
C                                                                       PUT 3300
      READ (NS,3) (PINMAX(IRR),IRR=1,KSS)                               PUT 3310
C                                                                       PUT 3320
  210 CONTINUE                                                          PUT 3330
      IF(NLUM .LE. 0) GOTO 219                                          PUT 3340
      DO 220 IR=1,NDR                                                   PUT 3350
        IF(VLP(1,IR) .LE. 0.0) GOTO 220                                 PUT 3360
        UMREG(IR) = VOLREG(IR) / VLP(1,IR)                              PUT 3370
  220 CONTINUE                                                          PUT 3380
  219 CONTINUE                                                          PUT 3390
      KR = 1                                                            PUT 3400
      IRRE1 = 1                                                         PUT 3410
      IRRE2 = IREG(KR)                                                  PUT 3420
  500 VOLK = 0.                                                         PUT 3430
      DO 510 IR=IRRE1,IRRE2                                             PUT 3440
        VOL(IR) = VOLREG(KR) * VPART(IR)                                PUT 3450
        VOLK = VOLK + VPART(IR)                                         PUT 3460
        IF(LUMPED .LE. 0) GOTO 620                                      PUT 3470
        DO 610 K=1,NLUM                                                 PUT 3480
          L = K + NO + IACT + 2                                         PUT 3490
          IF(UMREG(KR) .LE. 0.0) GOTO 610                               PUT 3500
          DEN(L,IR) = DEN(L,IR) / UMREG(KR)                             PUT 3510
  610   CONTINUE                                                        PUT 3520
  620   CONTINUE                                                        PUT 3530
  510 CONTINUE                                                          PUT 3540
      IF(ABS(VOLK-1.0) .LT. 0.001) GOTO 520                             PUT 3550
      WRITE (NT,777) KR,VOLK                                            PUT 3560
  520 CONTINUE                                                          PUT 3570
      IF(KR .GE. NDR) GOTO 550                                          PUT 3580
      KR = KR + 1                                                       PUT 3590
      IRRE1 = 1 + IRRE2                                                 PUT 3600
      IRRE2 = IREG(KR)                                                  PUT 3610
      GOTO 500                                                          PUT 3620
  550 CONTINUE                                                          PUT 3630
      KFISS = IACT                                                      PUT 3640
      IF(LOBN .NE. 2) GOTO 408                                          PUT 3650
      ITOR = 4                                                          PUT 3660
      WRITE (39) ITOR,KMAT,KFISS,N200C,N26,AVO                          PUT 3670
      ITOR = 5                                                          PUT 3680
      WRITE (39) ITOR,(MATNR(I),I=1,KMAT)                               PUT 3690
      ITOR = 6                                                          PUT 3700
      WRITE (39) ITOR,(NHOT(IR),IR=1,N200C)                             PUT 3710
      ITOR = 3                                                          PUT 3720
      WRITE (39) ITOR,IPRIN(15),JN,NXSC,ISPK,DELDAY                     PUT 3730
      DO 407 IR=1,N200C                                                 PUT 3740
        WRITE (39) VOL(IR),NRY(IR),(DEN(I,IR),I=1,KMAT)                 PUT 3750
  407 CONTINUE                                                          PUT 3760
      IEZ = 1                                                           PUT 3770
  408 CONTINUE                                                          PUT 3780
C                                                                       PUT 3790
C     PRINT OUT INPUT DATA                                              PUT 3800
C                                                                       PUT 3810
      IF(JSER-1) 809,809,811                                            PUT 3820
  809 WRITE (NT,708) HCORE,POWER,FIWATT,ZKFIND,DELDAY,JNSTOP,JNUM       PUT 3830
  811 CONTINUE                                                          PUT 3840
      IF(NLUM .EQ. 0) GOTO 826                                          PUT 3850
      WRITE (NT,720)                                                    PUT 3860
      KR = 1                                                            PUT 3870
      DO 823 IR=1,N200                                                  PUT 3880
        IF(IR .GT. IREG(KR)) KR = KR + 1                                PUT 3890
        DO 822 K=1,NLUM                                                 PUT 3900
          L = IACT + 2 + NO + K                                         PUT 3910
          WRITE (NT,722) IR,L,RODS(1,KR),DILP(1,KR),DOLP(1,KR),HLP(1,KR)PUT 3920
     1     ,VLP(1,KR),DEN(L,IR),UMREG(KR),SSLUMP(N26,KR)                PUT 3930
  822   CONTINUE                                                        PUT 3940
  823 CONTINUE                                                          PUT 3950
      WRITE (NT,723)                                                    PUT 3960
      WRITE (NT,725) (IE,A0(IE),A1(IE),A2(IE),A3(IE),A4(IE),A5(IE),IE=1,PUT 3970
     1 N26)                                                             PUT 3980
  826 CONTINUE                                                          PUT 3990
      IF(JSER .GT. 0) GOTO 831                                          PUT 4000
      WRITE (NT,1724) W3,W4                                             PUT 4010
      KR = 1                                                            PUT 4020
      DO 829 IR=1,N200                                                  PUT 4030
        IF(IR .GT. IREG(KR)) KR = KR + 1                                PUT 4040
      NCONTR = NC                                                       PUT 4050
        DO 828 K=1,NCONTR                                               PUT 4060
          L = IACT + 2 + NO + NLUM + K                                  PUT 4070
          WRITE (NT,1725) IR,L,POISL(K,KR),POISM(K,KR)                  PUT 4080
  828   CONTINUE                                                        PUT 4090
  829 CONTINUE                                                          PUT 4100
      WRITE (NT,724) JSSMAX,JSMAX,LSIM,(IRR,NPOIS(IRR),PINMAX(IRR),     PUT 4110
     1 PINMIN(IRR),IRR=1,KSS)                                           PUT 4120
  831 RETURN                                                            PUT 4130
      END                                                               PUT 4140
      SUBROUTINE INPUTC(VOLREG,NBURN,VFREG,VPART,NRG,NOPOW,MIX,NOPOWK)  UTC   10
C                                                                       UTC   20
C     READ DATA FOR DIFFUSION CALCULATION                               UTC   30
C     DIRECTS PREPARATION OF GENERAL GEOMETRICAL DATA                   UTC   40
C                                                                       UTC   50
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    UTC   60
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    UTC   70
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIUTC   80
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D             UTC   90
C                                                                       UTC  100
      EQUIVALENCE(JTPE3,NT)                                             UTC  110
C                                                                       UTC  120
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1UTC  130
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         UTC  140
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    UTC  150
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)UTC  160
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  UTC  170
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    UTC  180
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         UTC  190
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,UTC  200
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW,CIZET0,    UTC  210
     9 NMAXC,DLAY(101)                                                  UTC  220
C                                                                       UTC  230
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, UTC  240
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        UTC  250
     2 FIMAKL(20),NOPILE,MREP,MARX(10)                                  UTC  260
C                                                                       UTC  270
      COMMON /NORM/ PDUMMY,IMX,ICI(9999),IVS(9999),VFV(9999),VFC(9999)  UTC  280
C                                                                       UTC  290
      COMMON /BIRVSO/ NBIR(4),NBIR1(20),VBIR(2000),NBIR2(2000),         UTC  300
     1 NBIR3(2000),VBIR1(1515),NBIR4,NBIR5(2000),NBIR6(2000),VBIR2(2000)UTC  310
     2 ,VBIR3(2000),VBIR4,NBIR7,VBIR5(20),VBIR6(2),NBIR8,VBIR7(101),    UTC  320
     3 NBIR9(1515),NBIR10(2000)                                         UTC  330
C                                                                       UTC  340
      COMMON /TRIVSO/ NTRI(2),NTRI1(9999),NTRI2(9999),NTRI3(9999),      UTC  350
     1 NTRI4(9999),VTRI,VTRI1(9999),VTRI2(9999),VTRI3(9999),VTRI4(9999),UTC  360
     2 NTRI5(9999)                                                      UTC  370
C                                                                       UTC  380
CFZJ061                                                       29.3.11   UTC  390
      COMMON /SPKVOL/ RV(1500)                                          UTC  400
C                                                                       UTC  410
      DIMENSION VOLREG(NDR),NBURN(NDR),VFREG(NDR),VPART(N200),NRG(N200),UTC  420
     1 NOPOW(N200),MIX(NDR),NOPOWK(KROT)                                UTC  430
C                                                                       UTC  440
10005 FORMAT (/////' VOLUME OF REGIONS (CM3):' /(10E12.5))              UTC  450
10007 FORMAT (//' FRACTIONAL REGION VOLUME OF FISSILE CORE:'/(10E12.5)) UTC  460
10008 FORMAT (/' TOTAL VOLUME OF FISSILE CORE =',E12.5,' CM3')          UTC  470
C                                                                       UTC  480
C                                                                       UTC  490
      IF(I3D) 10,10,20                                                  UTC  500
   10 CONTINUE                                                          UTC  510
      DO 1000 I=1,N20                                                   UTC  520
        LAYER(I) = NBIR1(I)                                             UTC  530
 1000 CONTINUE                                                          UTC  540
      DO 1001 I=1,N200                                                  UTC  550
        VPART(I) = VBIR(I)                                              UTC  560
        NRG(I) = NBIR2(I)                                               UTC  570
        NOPOW(I) = NBIR3(I)                                             UTC  580
 1001 CONTINUE                                                          UTC  590
      DO 1002 I=1,NDR                                                   UTC  600
        MIX(I) = NBIR9(I)                                               UTC  610
        VOLREG(I) = VBIR1(I)                                            UTC  620
 1002 CONTINUE                                                          UTC  630
      DO 1005 I=1,KROT                                                  UTC  640
        NOPOWK(I) = NBIR10(I)                                           UTC  650
 1005 CONTINUE                                                          UTC  660
      IMX = NBIR4                                                       UTC  670
      DO 1003 I=1,IMX                                                   UTC  680
        ICI(I) = NBIR5(I)                                               UTC  690
        IVS(I) = NBIR6(I)                                               UTC  700
        VFV(I) = VBIR2(I)                                               UTC  710
        VFC(I) = VBIR3(I)                                               UTC  720
 1003 CONTINUE                                                          UTC  730
      HCORE = VBIR4                                                     UTC  740
      NRAD = NBIR7                                                      UTC  750
      DO 1004 I=1,NRAD                                                  UTC  760
        RADR(I) = VBIR5(I)                                              UTC  770
 1004 CONTINUE                                                          UTC  780
      RINN = VBIR6(1)                                                   UTC  790
      CIZET0 = VBIR6(2)                                                 UTC  800
      NMAXC = NBIR8                                                     UTC  810
      DO 1006 I=1,NMAXC                                                 UTC  820
        DLAY(I) = VBIR7(I)                                              UTC  830
 1006 CONTINUE                                                          UTC  840
      RADR(20) = -FLOAT(NRAD)                                           UTC  850
      GOTO 30                                                           UTC  860
   20 CONTINUE                                                          UTC  870
      IMX = NTRI(2)                                                     UTC  880
      HCORE = VTRI                                                      UTC  890
      DO 200 J=1,N200                                                   UTC  900
        VPART(J) = VTRI1(J)                                             UTC  910
        NRG(J) = NTRI1(J)                                               UTC  920
        NOPOW(J) = NTRI2(J)                                             UTC  930
  200 CONTINUE                                                          UTC  940
      DO 201 J=1,NDR                                                    UTC  950
        VOLREG(J) = VTRI2(J)                                            UTC  960
        MIX(J) = NTRI5(J)                                               UTC  970
        ICI(J) = NTRI3(J)                                               UTC  980
        IVS(J) = NTRI4(J)                                               UTC  990
        VFV(J) = VTRI3(J)                                               UTC 1000
        VFC(J) = VTRI4(J)                                               UTC 1010
  201 CONTINUE                                                          UTC 1020
   30 CONTINUE                                                          UTC 1030
      DO 51 IR=1,N200                                                   UTC 1040
        ILA = NRG(IR)                                                   UTC 1050
        IF(NOPOW(IR) .GT. 0) GOTO 51                                    UTC 1060
        NBURN(ILA) = ILA                                                UTC 1070
        N71 = ILA                                                       UTC 1080
   51 CONTINUE                                                          UTC 1090
      WRITE (NT,10005) (VOLREG(L),L=1,NDR)                              UTC 1100
CFZJ061                                                    29.3.11      UTC 1110
      DO 52 L=1,NDR                                                     UTC 1120
        RV(L) = VOLREG(L)                                               UTC 1130
   52 CONTINUE                                                          UTC 1140
      VOLUME = 0.0                                                      UTC 1150
      DO 300 I=1,N71                                                    UTC 1160
        IR = NBURN(I)                                                   UTC 1170
        VOLUME = VOLUME + VOLREG(IR)                                    UTC 1180
  300 CONTINUE                                                          UTC 1190
      DO 350 I=1,N71                                                    UTC 1200
        IR = NBURN(I)                                                   UTC 1210
        VFREG(IR) = VOLREG(IR) / VOLUME                                 UTC 1220
  350 CONTINUE                                                          UTC 1230
      WRITE (NT,10007) (VFREG(IR),IR=1,NDR)                             UTC 1240
      WRITE (NT,10008) VOLUME                                           UTC 1250
      RETURN                                                            UTC 1260
      END                                                               UTC 1270
      SUBROUTINE INIT(IMAT,AWT)                                         INI   10
C                                                                       INI   20
C     INITIALIZES DATA IN STORAGE FROM LAST PROBLEM                     INI   30
C                                                                       INI   40
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    INI   50
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    INI   60
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIINI   70
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 INI   80
C                                                                       INI   90
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1INI  100
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         INI  110
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    INI  120
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)INI  130
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  INI  140
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    INI  150
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         INI  160
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,INI  170
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            INI  180
C                                                                       INI  190
      COMMON /NUCNAM/ T(404),MATNR(200),WGN(200)                        INI  200
C                                                                       INI  210
      COMMON /FLUXN/ D(361),IACT                                        INI  220
C                                                                       INI  230
      CHARACTER*4 T                                                     INI  240
C                                                                       INI  250
      DIMENSION IMAT(KMAT),AWT(IACT)                                    INI  260
C                                                                       INI  270
C                                                                       INI  280
      K96 = 96                                                          INI  290
      CRT = 0.0                                                         INI  300
      CORE = 1.0E+4                                                     INI  310
      JN = -1                                                           INI  320
      JS = 0                                                            INI  330
      JSS = 0                                                           INI  340
      LSER = 0                                                          INI  350
      NSWIT = 0                                                         INI  360
      IF(KSIM .GT. 0) JSIM = KSIM - 1                                   INI  370
      KSIM = 0                                                          INI  380
      NBUMP = 3                                                         INI  390
      LBUMP = 0                                                         INI  400
      KINIT = 0                                                         INI  410
      JSPEC = 0                                                         INI  420
      NQUIT=0                                                           INI  430
      K97 = 2 * K96                                                     INI  440
      DO 15 I=1,K97                                                     INI  450
        SPECK(I) = 0.0                                                  INI  460
        TXME(I) = 0.0                                                   INI  470
   15 CONTINUE                                                          INI  480
      DO 20 L=1,K96                                                     INI  490
        STORE(1,L) = 0.0                                                INI  500
        NSTO(L) = 0                                                     INI  510
   20 CONTINUE                                                          INI  520
      IF(IPRIN(15) .GT. 0) GOTO 99                                      INI  530
      DO 30 I=1,IACT                                                    INI  540
        J = IMAT(I)                                                     INI  550
        AWT(I) = WGN(J)                                                 INI  560
   30 CONTINUE                                                          INI  570
   99 CONTINUE                                                          INI  580
      RETURN                                                            INI  590
      END                                                               INI  600
      SUBROUTINE HEAD(KZZZ)                                             HEA   10
C                                                                       HEA   20
C     READS AND PRINTS CASE IDENTIFICATION CARD                         HEA   30
C                                                                       HEA   40
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    HEA   50
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    HEA   60
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIHEA   70
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 HEA   80
C                                                                       HEA   90
      EQUIVALENCE(JTPE2,NS),(JTPE3,NT)                                  HEA  100
C                                                                       HEA  110
      CHARACTER*72 T                                                    HEA  120
C                                                                       HEA  130
   11 FORMAT (A72)                                                      HEA  140
   12 FORMAT (1H1)                                                      HEA  150
C                                                                       HEA  160
C                                                                       HEA  170
      GOTO(1,2,3),KZZZ                                                  HEA  180
    1 READ (NS,11) T                                                    HEA  190
      GOTO 4                                                            HEA  200
    2 WRITE (NT,12)                                                     HEA  210
    3 WRITE (NT,11) T                                                   HEA  220
    4 RETURN                                                            HEA  230
      END                                                               HEA  240
      SUBROUTINE YIELDS(IMAT)                                           YIE   10
C                                                                       YIE   20
C     FIXED LIBRARY OF FISSION YIELDS AND DECAY CONSTANTS               YIE   30
C     DATA FROM:                                                        YIE   40
C     LISMAN ET AL NS&E 42, P191-205, (1970)                            YIE   50
C     J.LIU JUEL-678-RG, P50-51,(1970)                                  YIE   60
C     L.MASSIMO, EUR 3119.E, TABLE 5-11, (1966)                         YIE   70
C     COMPILED BY U.ELOWSSON AND U.HANSEN 1973, DRAGON WINFRITH         YIE   80
C                                                                       YIE   90
C     ALLE NICHT MIT *ALT* GEKENNZEICHNETEN YIELDS SIND ENDF/B-IV-DATEN YIE  100
C     ----------------------------------------------------------------- YIE  110
C     (ERSTELLT AUGUST 1985,  HAAS)                                     YIE  120
C                                                                       YIE  130
C     KUMULIERTE YIELDS CHAIN 44 ERNEUERT                               YIE  140
C                                                                       YIE  150
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    YIE  160
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    YIE  170
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIYIE  180
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 YIE  190
C                                                                       YIE  200
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1YIE  210
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         YIE  220
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    YIE  230
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)YIE  240
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  YIE  250
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    YIE  260
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         YIE  270
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,YIE  280
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            YIE  290
C                                                                       YIE  300
      COMMON /FLUXN/ D(361),IACT                                        YIE  310
C                                                                       YIE  320
      DIMENSION XLAM(49),YIELD(5,180),IMAT(KMAT)                        YIE  330
C                                                                       YIE  340
      EQUIVALENCE(DIRAC(1,5),XLAM(1)),(YIELD(1,22),X22(1)),             YIE  350
     1 (YIELD(1,33),SE82(1)),(YIELD(1,34),BR81(1)),(YIELD(1,35),KR83(1))YIE  360
     2 ,(YIELD(1,36),KR84(1)),(YIELD(1,37),KR85(1)),                    YIE  370
     3 (YIELD(1,38),KR86(1)),(YIELD(1,39),RB85(1)),(YIELD(1,40),RB87(1))YIE  380
     4 ,(YIELD(1,41),SR88(1)),(YIELD(1,42),SR90(1)),(YIELD(1,43),Y89(1))YIE  390
     5 ,(YIELD(1,44),ZRNAT(1)),(YIELD(1,45),ZR90(1)),                   YIE  400
     6 (YIELD(1,46),ZR91(1)),(YIELD(1,47),ZR92(1)),(YIELD(1,48),ZR93(1))YIE  410
     7 ,(YIELD(1,49),ZR94(1)),(YIELD(1,50),ZR96(1)),(YIELD(1,51),X51(1))YIE  420
     8 ,(YIELD(1,52),MO95(1)),(YIELD(1,53),MO96(1)),                    YIE  430
     9 (YIELD(1,54),MO97(1)),(YIELD(1,55),MO98(1)),                     YIE  440
     X (YIELD(1,56),MO100(1)),(YIELD(1,57),TC99(1)),                    YIE  450
     Y (YIELD(1,59),RU101(1)),(YIELD(1,60),RU102(1)),                   YIE  460
     Z (YIELD(1,61),RU104(1)),(YIELD(1,62),RH103(1))                    YIE  470
C                                                                       YIE  480
      EQUIVALENCE(YIELD(1,63),X63(1)),(YIELD(1,64),PD105(1)),           YIE  490
     1 (YIELD(1,65),PD106(1)),(YIELD(1,66),PD107(1)),                   YIE  500
     2 (YIELD(1,67),PD108(1)),(YIELD(1,68),PD110(1)),                   YIE  510
     3 (YIELD(1,69),AG109(1)),(YIELD(1,70),IN115(1)),                   YIE  520
     4 (YIELD(1,71),X71(1)),(YIELD(1,73),CD111(1)),                     YIE  530
     5 (YIELD(1,74),CD112(1)),(YIELD(1,75),CD113(1)),                   YIE  540
     6 (YIELD(1,76),CD114(1)),(YIELD(1,77),TE126(1)),                   YIE  550
     7 (YIELD(1,78),TE128(1)),(YIELD(1,79),TE130(1)),                   YIE  560
     8 (YIELD(1,80),I127(1)),(YIELD(1,81),I129(1)),(YIELD(1,83),X83(1)),YIE  570
     9 (YIELD(1,84),XE131(1)),(YIELD(1,85),XE132(1)),                   YIE  580
     X (YIELD(1,86),XE134(1)),(YIELD(1,87),XE135(1)),                   YIE  590
     Y (YIELD(1,88),XE136(1)),(YIELD(1,89),CS133(1)),                   YIE  600
     Z (YIELD(1,90),CS135(1)),(YIELD(1,91),CS137(1))                    YIE  610
C                                                                       YIE  620
      EQUIVALENCE(YIELD(1,93),X93(1)),(YIELD(1,95),BA138(1)),           YIE  630
     1 (YIELD(1,96),LA139(1)),(YIELD(1,97),CE140(1)),                   YIE  640
     2 (YIELD(1,98),CE142(1)),(YIELD(1,99),PR141(1)),                   YIE  650
     3 (YIELD(1,100),ND142(1)),(YIELD(1,101),ND143(1)),                 YIE  660
     4 (YIELD(1,102),ND144(1)),(YIELD(1,103),ND145(1)),                 YIE  670
     5 (YIELD(1,104),ND146(1)),(YIELD(1,105),ND148(1)),                 YIE  680
     6 (YIELD(1,106),ND150(1)),(YIELD(1,107),PM147(1)),                 YIE  690
     7 (YIELD(1,108),SM147(1)),(YIELD(1,109),SM148(1)),                 YIE  700
     8 (YIELD(1,110),SM149(1)),(YIELD(1,111),SM150(1)),                 YIE  710
     9 (YIELD(1,112),SM151(1)),(YIELD(1,113),SM152(1)),                 YIE  720
     X (YIELD(1,114),SM154(1)),(YIELD(1,116),EU153(1)),                 YIE  730
     Y (YIELD(1,117),EU154(1)),(YIELD(1,118),EU155(1)),                 YIE  740
     Z (YIELD(1,119),GD154(1)),(YIELD(1,120),GD155(1))                  YIE  750
C                                                                       YIE  760
      EQUIVALENCE(YIELD(1,121),GD156(1)),(YIELD(1,122),GD157(1)),       YIE  770
     1 (YIELD(1,123),GD158(1)),(YIELD(1,124),TB159(1)),                 YIE  780
     2 (YIELD(1,125),X125(1)),(YIELD(1,130),BO10(1)),                   YIE  790
     3 (YIELD(1,142),RU105(1)),(YIELD(1,143),RH105(1)),                 YIE  800
     4 (YIELD(1,144),CS134(1)),(YIELD(1,145),CE144(1)),                 YIE  810
     5 (YIELD(1,147),PM148G(1)),(YIELD(1,148),PM148M(1)),               YIE  820
     6 (YIELD(1,149),ZR95(1)),(YIELD(1,151),RU103(1)),                  YIE  830
     7 (YIELD(1,152),XE133(1)),(YIELD(1,153),CE141(1)),                 YIE  840
     8 (YIELD(1,154),PR143(1)),(YIELD(1,155),PM149(1)),                 YIE  850
     9 (YIELD(1,156),I131(1)),(YIELD(1,160),FP44(1)),                   YIE  860
     X (YIELD(1,161),FP39(1)),(YIELD(1,162),FP34(1)),                   YIE  870
     Y (YIELD(1,163),FP29(1)),(YIELD(1,175),PM151(1)),                  YIE  880
     Z (YIELD(1,176),I135(1))                                           YIE  890
C                                                                       YIE  900
C     YIELD VON     U233       U235       PU239      PU241      ZERFALL YIE  910
C                                                                       YIE  920
      REAL SE82(5)/5.6262E-03,3.3405E-03,2.1092E-03,1.1602E-03,0.0/     YIE  930
      REAL BR81(5)/3.1171E-03,2.1005E-03,1.7680E-03,6.4690E-04,0.0/     YIE  940
      REAL KR83(5)/1.0178E-02,5.3076E-03,2.9608E-03,2.0498E-03,0.0/     YIE  950
      REAL KR84(5)/1.7034E-02,9.8786E-03,4.8029E-03,3.5393E-03,0.0/     YIE  960
      REAL KR85(5)/2.1946E-02,1.3140E-02,5.6834E-03,3.9618E-03,         YIE  970
     1 2.0740E-09/                                                      YIE  980
      REAL KR86(5)/2.8581E-02,1.9528E-02,7.5863E-03,6.1392E-03,0.0/     YIE  990
      REAL RB85(5)/6.5296E-07,8.2300E-07,5.8500E-07,5.3024E-09,0.0/     YIE 1000
      REAL RB87(5)/4.0088E-02,2.5510E-02,9.4936E-03,7.5709E-03,0.0/     YIE 1010
      REAL SR88(5)/5.4953E-02,3.6228E-02,1.3703E-02,9.7473E-03,0.0/     YIE 1020
      REAL SR90(5)/6.7952E-02,5.9137E-02,2.1134E-02,1.5363E-02,0.0/     YIE 1030
      REAL Y89 (5)/6.2568E-02,4.8469E-02,1.7075E-02,1.2146E-02,0.0/     YIE 1040
C *ALT*                                                                 YIE 1050
      REAL ZRNAT(5)/6.4467E-02,5.8030E-02,2.6405E-02,2.6450E-02,0.0/    YIE 1060
C *ALT*                                                                 YIE 1070
      REAL ZR90(5)/5.0000E-04,4.7000E-04,1.6400E-04,1.6400E-04,0./      YIE 1080
      REAL ZR91(5)/6.5194E-02,5.9260E-02,2.4941E-02,1.8315E-02,0.0/     YIE 1090
      REAL ZR92(5)/6.5949E-02,5.9660E-02,3.0180E-02,2.2781E-02,0.0/     YIE 1100
      REAL ZR93(5)/7.0110E-02,6.3703E-02,3.9031E-02,2.9643E-02,0.0/     YIE 1110
      REAL ZR94(5)/6.8076E-02,6.4228E-02,4.4431E-02,3.4018E-02,0.0/     YIE 1120
      REAL ZR95(5)/6.2478E-02,6.4678E-02,4.9212E-02,4.0456E-02,         YIE 1130
     1 1.2300E-07/                                                      YIE 1140
      REAL ZR96(5)/5.6694E-02,6.2506E-02,5.0958E-02,4.4232E-02,0.0/     YIE 1150
      REAL MO95(5)/9.5909E-06,1.6410E-06,1.4920E-05,1.2927E-07,0.0/     YIE 1160
C *ALT*                                                                 YIE 1170
      REAL MO96(5)/6.5000E-05,5.8500E-06,7.7000E-06,7.7000E-06,0.0/     YIE 1180
      REAL MO97(5)/5.4533E-02,5.9600E-02,5.6080E-02,4.8208E-02,0.0/     YIE 1190
      REAL MO98(5)/5.1587E-02,5.7787E-02,5.8542E-02,5.2217E-02,0.0/     YIE 1200
      REAL MO100(5)/4.4094E-02,6.3096E-02,6.9770E-02,6.2311E-02,0.0/    YIE 1210
      REAL TC99(5)/4.9573E-02,6.1284E-02,6.1405E-02,6.2085E-02,0.0/     YIE 1220
      REAL RU101(5)/3.2258E-02,5.0501E-02,5.9135E-02,6.0948E-02,0.0/    YIE 1230
      REAL RU102(5)/2.4492E-02,4.2032E-02,6.0201E-02,6.4843E-02,0.0/    YIE 1240
      REAL RU103(5)/1.7066E-02,3.1411E-02,6.9845E-02,6.2611E-02,        YIE 1250
     1 2.0300E-07/                                                      YIE 1260
      REAL RU104(5)/1.0276E-02,1.8239E-02,5.9539E-02,6.9764E-02,0.0/    YIE 1270
C *ALT*                                                                 YIE 1280
      REAL RU105(5)/4.8000E-03,9.0000E-03,5.4700E-02,5.4700E-02,        YIE 1290
     1 4.3500E-05/                                                      YIE 1300
      REAL RH103(5)/1.4219E-11,1.8580E-11,1.3580E-09,5.6028E-07,0.0/    YIE 1310
      REAL RH105(5)/4.7126E-03,1.0199E-02,5.4261E-02,6.2183E-02,        YIE 1320
     1 5.3500E-06/                                                      YIE 1330
      REAL PD105(5)/3.4998E-13,9.8300E-13,2.0300E-10,1.6908E-08,0.0/    YIE 1340
C *ALT*                                                                 YIE 1350
      REAL PD106(5)/2.4063E-03,3.7759E-03,4.6234E-02,4.6314E-02,0.0/    YIE 1360
      REAL PD107(5)/1.1417E-03,1.6317E-03,3.2361E-02,5.3339E-02,0.0/    YIE 1370
      REAL PD108(5)/6.1481E-04,7.1032E-04,2.2319E-02,4.0191E-02,0.0/    YIE 1380
      REAL PD110(5)/2.5376E-04,2.2338E-04,6.2204E-03,1.2091E-02,0.0/    YIE 1390
      REAL AG109(5)/4.3363E-04,2.9903E-04,1.4115E-02,2.2836E-02,0.0/    YIE 1400
      REAL CD111(5)/2.0268E-04,1.9714E-04,2.7428E-03,5.7261E-03,0.0/    YIE 1410
      REAL CD112(5)/1.4602E-04,1.2802E-04,1.0707E-03,2.3001E-03,0.0/    YIE 1420
      REAL CD113(5)/1.3152E-04,1.2425E-04,7.8216E-04,1.5494E-03,0.0/    YIE 1430
      REAL CD114(5)/1.2268E-04,1.1256E-04,4.6789E-04,7.5514E-04,0.0/    YIE 1440
C *ALT*                                                                 YIE 1450
      REAL IN115(5)/2.0052E-04,9.9367E-05,4.0467E-04,4.0537E-04,0.0/    YIE 1460
      REAL TE126(5)/2.4081E-03,5.7818E-04,1.9996E-03,7.7127E-04,0.0/    YIE 1470
      REAL TE128(5)/9.4592E-03,3.5046E-03,8.5079E-03,3.5555E-03,0.0/    YIE 1480
      REAL TE130(5)/2.3671E-02,1.4466E-02,2.4971E-02,1.6617E-02,0.0/    YIE 1490
      REAL I127(5)/6.7853E-03,1.3037E-03,4.9173E-03,2.3046E-03,0.0/     YIE 1500
      REAL I129(5)/1.6160E-02,6.5911E-03,1.5039E-02,7.7864E-03,0.0/     YIE 1510
      REAL I131(5)/3.7089E-02,2.8325E-02,3.7380E-02,3.1411E-02,         YIE 1520
     1 9.9500E-07/                                                      YIE 1530
      REAL I135(5)/4.8597E-02,6.3482E-02,6.3007E-02,6.9500E-02,         YIE 1540
     1 2.87E-05/                                                        YIE 1550
      REAL XE131(5)/8.4795E-07,1.5400E-08,1.6520E-07,1.3066E-08,0.0/    YIE 1560
      REAL XE132(5)/4.8038E-02,4.2498E-02,5.2688E-02,4.6411E-02,0.0/    YIE 1570
      REAL XE133(5)/6.0307E-02,6.7859E-02,6.9758E-02,6.7410E-02,        YIE 1580
     1 1.5200E-06/                                                      YIE 1590
      REAL XE134(5)/5.7588E-02,7.6825E-02,7.3890E-02,8.1081E-02,0.0/    YIE 1600
      REAL XE135(5)/6.1971E-02,6.6023E-02,7.4524E-02,7.1792E-02,        YIE 1610
     1 2.1000E-05/                                                      YIE 1620
      REAL XE136(5)/6.7934E-02,6.2701E-02,6.6153E-02,7.2871E-02,0.0/    YIE 1630
      REAL CS133(5)/3.6998E-07,5.0800E-07,1.6100E-07,4.3020E-09,0.0/    YIE 1640
      REAL CS134(5)/1.1969E-05,3.5700E-07,4.6100E-06,3.5416E-07,        YIE 1650
     1 1.0500E-08/                                                      YIE 1660
C *ALT*                                                                 YIE 1670
      REAL CS135(5)/6.1000E-02,6.4500E-02,7.2200E-02,7.8000E-02,0.0/    YIE 1680
      REAL CS137(5)/6.7889E-02,6.2690E-02,6.6834E-02,6.6980E-02,7.3E-10/YIE 1690
      REAL BA138(5)/5.8863E-02,6.8272E-02,5.7173E-02,6.4446E-02,0.0/    YIE 1700
      REAL LA139(5)/5.8850E-02,6.4933E-02,5.6456E-02,6.2283E-02,0.0/    YIE 1710
      REAL CE140(5)/6.4334E-02,6.3229E-02,5.5751E-02,5.8940E-02,0.0/    YIE 1720
C *ALT*                                                                 YIE 1730
      REAL CE141(5)/6.2400E-02,5.7300E-02,6.1100E-02,6.1100E-02,        YIE 1740
     1 2.4700E-07/                                                      YIE 1750
      REAL CE142(5)/6.6304E-02,5.9247E-02,5.0173E-02,4.8150E-02,0.0/    YIE 1760
C *ALT*                                                                 YIE 1770
      REAL CE144(5)/4.5117E-02,5.9620E-02,4.4514E-02,4.8644E-02,        YIE 1780
     1 2.8248E-08/                                                      YIE 1790
      REAL PR141(5)/6.6224E-02,5.8929E-02,5.3634E-02,4.8534E-02,0.0/    YIE 1800
      REAL PR143(5)/5.8513E-02,5.9710E-02,4.5613E-02,4.5017E-02,        YIE 1810
     1 5.9100E-07/                                                      YIE 1820
C *ALT*                                                                 YIE 1830
      REAL ND142(5)/0.0,0.900E-04,0.900E-05,0.09  E-04,0.09  E-04/      YIE 1840
      REAL ND143(5)/2.4799E-10,9.5000E-13,4.9000E-12,1.2106E-12,0.0/    YIE 1850
      REAL ND144(5)/4.6495E-02,5.4523E-02,3.8340E-02,4.1564E-02,0.0/    YIE 1860
      REAL ND145(5)/3.4248E-02,3.9339E-02,3.0833E-02,3.2046E-02,0.0/    YIE 1870
      REAL ND146(5)/2.5973E-02,2.9912E-02,2.5333E-02,2.7401E-02,0.0/    YIE 1880
      REAL ND148(5)/1.2867E-02,1.6900E-02,1.6982E-02,1.9257E-02,0.0/    YIE 1890
      REAL ND150(5)/4.9846E-03,6.4593E-03,9.9451E-03,1.1960E-02,0.0/    YIE 1900
      REAL PM147(5)/1.7753E-02,2.2701E-02,2.0769E-02,2.2601E-02,        YIE 1910
     1 8.4200E-09/                                                      YIE 1920
      REAL PM148M(5)/2.7899E-07,7.4900E-09,2.0900E-08,5.4125E-09,       YIE 1930
     1 1.9100E-07/                                                      YIE 1940
      REAL PM148G(5)/9.4395E-09,5.7300E-08,2.0900E-08,5.4125E-09,       YIE 1950
     1 1.4900E-06/                                                      YIE 1960
C *ALT*                                                                 YIE 1970
      REAL PM149(5)/7.6953E-03,1.0888E-02,1.2617E-02,1.4635E-02,        YIE 1980
     1 3.6260E-06/                                                      YIE 1990
      REAL PM151(5)/3.2293E-03,4.2044E-03,7.7720E-03,9.0238E-03,        YIE 2000
     1 6.7800E-06/                                                      YIE 2010
      REAL SM147(5)/0.0,0.0,0.0,0.0,0.0/                                YIE 2020
      REAL SM148(5)/1.7999E-10,6.9500E-13,2.8000E-12,4.2720E-13,0.0/    YIE 2030
      REAL SM149(5)/0.E-0,0.E-0,0.E-0,0.E-0,0.0/                        YIE 2040
      REAL SM150(5)/2.5782E-05,5.4130E-06,1.7009E-05,3.9438E-06,0.0/    YIE 2050
      REAL SM151(5)/0.E-0,0.E-0,0.E-0,0.E-0,2.6100E-10/                 YIE 2060
      REAL SM152(5)/2.0784E-03,2.7057E-03,5.9618E-03,7.1704E-03,0.0/    YIE 2070
      REAL SM154(5)/4.5580E-04,7.4689E-04,2.7682E-03,3.7979E-03,0.0/    YIE 2080
      REAL EU153(5)/1.0686E-03,1.6264E-03,3.7224E-03,5.2815E-03,0.0/    YIE 2090
      REAL EU154(5)/3.7198E-07,1.6300E-08,3.5400E-07,5.5626E-08,        YIE 2100
     1 1.3700E-09/                                                      YIE 2110
      REAL EU155(5)/2.1252E-04,3.3025E-04,1.7082E-03,2.3181E-03,        YIE 2120
     1 1.2100E-08/                                                      YIE 2130
      REAL GD154(5)/0.E-0,0.E-0,0.E-0,0.E-0,0.E-0/                      YIE 2140
      REAL GD155(5)/3.6198E-09,4.4100E-11,2.8300E-09,1.9109E-10,0.0/    YIE 2150
      REAL GD156(5)/1.1737E-04,1.3517E-04,1.1989E-03,1.6955E-03,0.0/    YIE 2160
      REAL GD157(5)/6.7747E-05,6.4651E-05,7.6297E-04,1.3153E-03,0.0/    YIE 2170
      REAL GD158(5)/2.2298E-05,3.2163E-05,4.0955E-04,8.6707E-04,0.0/    YIE 2180
      REAL TB159(5)/9.2311E-06,1.0394E-05,2.1205E-04,4.6741E-04,0.0/    YIE 2190
C *ALT*  ALLE X..                                                       YIE 2200
      REAL X93(10)/10*0./                                               YIE 2210
      REAL X71(10)/10*0./,X63(5)/5*0./,X83(5)/5*0./,X125(25)/25*0./     YIE 2220
      REAL X22(55)/55*0./,X51(5)/5*0./,BO10(60)/60*0./                  YIE 2230
C                                                                       YIE 2240
C     FP44 - FP29 STEHEN IN DEN LIBRARIES ALS FP40 - FP25               YIE 2250
C                                                                       YIE 2260
C     ADAPTED TO ORIGEN-EXJUEL-II 9/90 (ENDF/B-V,JEF-1)                 YIE 2270
C                                                                       YIE 2280
      REAL FP44(5)/1.123 E-00,0.9476E-00,1.156 E-00,1.104 E-00,0.0/     YIE 2290
C *ALT*                                                                 YIE 2300
      REAL FP39(5)/1.541 E-00,1.324 E-00,1.590 E-00,1.509 E-00,0.0/     YIE 2310
C *ALT*                                                                 YIE 2320
      REAL FP34(5)/2.052 E-00,1.839 E-00,2.762 E-00,2.660 E-00,0.0/     YIE 2330
C                                                                       YIE 2340
C     ADAPTED TO CHAIN 44 (FP44), ORIGEN-EXJUEL-II 9/90 (ENDF/B-V,JEF-1)YIE 2350
C                                                                       YIE 2360
      REAL FP29(5)/2.9957E-00,2.6842E-00,3.9882E-00,3.8817E-00,0.0/     YIE 2370
C                                                                       YIE 2380
C                                                                       YIE 2390
      NO1 = NO + 1                                                      YIE 2400
C     NXEN = 0                                                          YIE 2410
      DO 100 K=1,NO1                                                    YIE 2420
        JMT = IMAT(K+IACT)                                              YIE 2430
C       IF(JMT .EQ. 163) NXEN = 72                                      YIE 2440
        IF(K .EQ. NO1) JMT = 176                                        YIE 2450
        XLAM(K) = YIELD(5,JMT)                                          YIE 2460
C       IF(JMT .EQ. 84) JMT = JMT + NXEN                                YIE 2470
        YIELD1(K) = YIELD(1,JMT)                                        YIE 2480
        YIELD2(K) = YIELD(2,JMT)                                        YIE 2490
        YIELD3(K) = YIELD(3,JMT)                                        YIE 2500
        YIELD4(K) = YIELD(4,JMT)                                        YIE 2510
  100 CONTINUE                                                          YIE 2520
      RETURN                                                            YIE 2530
      END                                                               YIE 2540
      SUBROUTINE DIMDEF(IAB,IV,IT)                                      DIM   10
C                                                                       DIM   20
C     DEFINITION DER VARIABLEN DIMENSIONIERUNG UND STARTPUNKT IM A-FELD DIM   30
C     DES COMMONS /VARDIM/.                                             DIM   40
C     DEFINITION DER VARIABLEN DIMENSIONIERUNG UND STARTPUNKT IM B-FELD DIM   50
C     DES COMMONS /VARDIT/.                                             DIM   60
C                                                                       DIM   70
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    DIM   80
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    DIM   90
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIDIM  100
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D,NLAYP,ITTT, DIM  110
     4 LIMT                                                             DIM  120
C                                                                       DIM  130
      EQUIVALENCE(JTPE2,NS),(JTPE3,NT)                                  DIM  140
C                                                                       DIM  150
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1DIM  160
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         DIM  170
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    DIM  180
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)DIM  190
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  DIM  200
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    DIM  210
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         DIM  220
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,DIM  230
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            DIM  240
C                                                                       DIM  250
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, DIM  260
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        DIM  270
     2 FIMAKL(20),NOPILE,MREP,MARX(10)                                  DIM  280
C                                                                       DIM  290
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP,VAR(200)              DIM  300
C                                                                       DIM  310
CFZJ042                                                       09.09.05  DIM  320
      COMMON /ADDRT/ KX(240),KY(240),LZ(240),NENDPT,VAT(240)            DIM  330
C                                                                       DIM  340
      COMMON /HILF/ JMAT,JABOX                                          DIM  350
C                                                                       DIM  360
      COMMON /FLUXN/ D(361),IACT                                        DIM  370
C                                                                       DIM  380
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             DIM  390
C                                                                       DIM  400
      COMMON /BIRVSO/ NBIR(4),NBIR1(20),VBIR(2000),NBIR2(2000),         DIM  410
     1 NBIR3(2000),VBIR1(1515),NBIR4,NBIR5(2000),NBIR6(2000),VBIR2(2000)DIM  420
     2 ,VBIR3(2000),VBIR4,NBIR7,VBIR5(20),VBIR6(2),NBIR8,VBIR7(101),    DIM  430
     3 NBIR9(1515),NBIR10(2000)                                         DIM  440
C                                                                       DIM  450
      COMMON /TRIVSO/ NTRI(2),NTRI1(9999),NTRI2(9999),NTRI3(9999),      DIM  460
     1 NTRI4(9999),VTRI,VTRI1(9999),VTRI2(9999),VTRI3(9999),VTRI4(9999),DIM  470
     2 NTRI5(9999)                                                      DIM  480
C                                                                       DIM  490
      COMMON /VARIAD/ KMATD                                             DIM  500
C                                                                       DIM  510
      CHARACTER*54 VAR,VAT                                              DIM  520
C                                                                       DIM  530
   10 FORMAT (1H1/' NUMBER               ARRAY NAME                  LABDIM  540
     1EL-ID.   LABEL     ENTRY POINT     DIMENSION   (VSOP)'/97('=')/)  DIM  550
   20 FORMAT (I5,7X,A54,6X,I8,9X,I7)                                    DIM  560
   30 FORMAT (/' TOTAL LENGTH AS REQUIRED OUT OF THE FIELD A :',41X,I8//DIM  570
     1 )                                                                DIM  580
   40 FORMAT (18I4)                                                     DIM  590
   50 FORMAT (1H1/' NUMBER               ARRAY NAME                  LABDIM  600
     1EL-ID.   LABEL     ENTRY POINT     DIMENSION   (THERMIX/KONVEK)'/ DIM  610
     2 97('=')/)                                                        DIM  620
   60 FORMAT (/' TOTAL LENGTH AS REQUIRED OUT OF THE FIELD B :',41X,I8//DIM  630
     1 )                                                                DIM  640
   70 FORMAT (//' *** THE DIMENSION RESERVED FOR DATA STORAGE IS BY ABOUDIM  650
     1T',I8,' WORDS TOO SMALL.'/'     INCREASE THE DIMENSION *A* IN COMMDIM  660
     2ON /VARDIM/ PLUS THE VARIABLE IV,'/'     BECAUSE PROGRAM ALSO INTEDIM  670
     3RNALLY REQUIRES MORE STORAGE CAPACITY. ***')                      DIM  680
   80 FORMAT (//' *** THE DIMENSION RESERVED FOR DATA STORAGE IS BY ABOUDIM  690
     1T',I8,' WORDS TOO SMALL.'/'     INCREASE THE DIMENSION *B* IN COMMDIM  700
     2ON /VARDIT/ PLUS THE VARIABLE IT,'/'     BECAUSE PROGRAM ALSO INTEDIM  710
     3RNALLY REQUIRES MORE STORAGE CAPACITY. ***')                      DIM  720
C                                                                       DIM  730
C                                                                       DIM  740
      GOTO(3,4),IAB                                                     DIM  750
    3 CONTINUE                                                          DIM  760
      N27 = 2                                                           DIM  770
      N40 = 40                                                          DIM  780
      IACT = 28                                                         DIM  790
      NLB = 0                                                           DIM  800
      JCLAS = 0                                                         DIM  810
C                                                                       DIM  820
CARD V1                                                                 DIM  830
C                                                                       DIM  840
CFZJ042                                                       09.09.05  DIM  850
      READ (NS,40) N26,MMAF,MBATCH,MSTOB,JTYP,MREP,JABOX,KMAZ           DIM  860
C                                                                       DIM  870
      KMAT = KMATD                                                      DIM  880
      IF(MMAF .LE. 0) MMAF = 1                                          DIM  890
      IF(KMAZ .EQ. 0) KMAZ = 50                                         DIM  900
      KMAZ = KMAZ + 1                                                   DIM  910
      KOMAX = KMAZ                                                      DIM  920
      JCLAS = JCLAS + 2 * JTYP                                          DIM  930
      NBOX = MBATCH                                                     DIM  940
      MBOX = MBATCH + MSTOB                                             DIM  950
      IF(MBOX .EQ. 0) MBOX = 1                                          DIM  960
      JMAT = KMAT                                                       DIM  970
      KMAT = IABS(KMAT) + IACT                                          DIM  980
      N25 = N26 - 1                                                     DIM  990
      LIMT = (N26*N25) / 2                                              DIM 1000
      IF(I3D) 5,5,6                                                     DIM 1010
    5 CONTINUE                                                          DIM 1020
      N200 = NBIR(1)                                                    DIM 1030
      N20 = NBIR(2)                                                     DIM 1040
      NDR = NBIR(3)                                                     DIM 1050
      KROT = NBIR(4)                                                    DIM 1060
      GOTO 7                                                            DIM 1070
    6 CONTINUE                                                          DIM 1080
      N200 = NTRI(1)                                                    DIM 1090
      NDR = NTRI(2)                                                     DIM 1100
      KROT = NDR                                                        DIM 1110
    7 CONTINUE                                                          DIM 1120
CFZJ042                                                       09.09.05  DIM 1130
      NXS = NDR                                                         DIM 1140
C                                                                       DIM 1150
      K42 = N200                                                        DIM 1160
      NOPILE = JCLAS - JTYP                                             DIM 1170
      JD11 = N200 + 2 + MBOX + NOPILE + JTYP + (MREP+1) + JABOX         DIM 1180
C                                                                       DIM 1190
C                                                                       DIM 1200
      M = 1                                                             DIM 1210
      VAR(M) = 'IMAT(KMAT)                              LIMAT        1' DIM 1220
      KL(M) =        KMAT                                               DIM 1230
      LI(1) = M                                                         DIM 1240
C                                                                       DIM 1250
      M = M + 1                                                         DIM 1260
      VAR(M) = 'AINO(KMAT)                              LAINO      107' DIM 1270
      KL(M) =        KMAT                                               DIM 1280
      LI(107) = M                                                       DIM 1290
C                                                                       DIM 1300
      M = M + 1                                                         DIM 1310
      VAR(M) = 'XREPRO(KMAT)                            LXREP       55' DIM 1320
      KL(M) =          KMAT                                             DIM 1330
      LI(55) = M                                                        DIM 1340
C                                                                       DIM 1350
      M = M + 1                                                         DIM 1360
      VAR(M) = 'NHOTD(KMAT)                             LNHOD       90' DIM 1370
      KL(M) =         KMAT                                              DIM 1380
      LI(90) = M                                                        DIM 1390
C                                                                       DIM 1400
      M = M + 1                                                         DIM 1410
      VAR(M) = 'DOR(100,KMAT)                           LDOR       103' DIM 1420
      KL(M) =      100*KMAT                                             DIM 1430
      LI(103) = M                                                       DIM 1440
C                                                                       DIM 1450
      M = M + 1                                                         DIM 1460
      VAR(M) = 'AWT(IACT)                               LAWT       151' DIM 1470
      KL(M) =       IACT                                                DIM 1480
      LI(151) = M                                                       DIM 1490
C                                                                       DIM 1500
      M = M + 1                                                         DIM 1510
      VAR(M) = 'GS(IACT)                                LGS        152' DIM 1520
      KL(M) =      IACT                                                 DIM 1530
      LI(152) = M                                                       DIM 1540
C                                                                       DIM 1550
      M = M + 1                                                         DIM 1560
      VAR(M) = 'FICOMP(IACT)                            LFICO      154' DIM 1570
      KL(M) =          IACT                                             DIM 1580
      LI(154) = M                                                       DIM 1590
C                                                                       DIM 1600
      M = M + 1                                                         DIM 1610
      VAR(M) = 'IDFISS(IACT)                            LIDFI      155' DIM 1620
      KL(M) =          IACT                                             DIM 1630
      LI(155) = M                                                       DIM 1640
C                                                                       DIM 1650
      M = M + 1                                                         DIM 1660
      VAR(M) = 'DMAT(IACT+1,4,4)                        LDMAT      150' DIM 1670
      KL(M) =       (IACT+1)*4*4                                        DIM 1680
      LI(150) = M                                                       DIM 1690
C                                                                       DIM 1700
      M = M + 1                                                         DIM 1710
      VAR(M) = 'RPHI(N26)                               LRPH        66' DIM 1720
      KL(M) =        N26                                                DIM 1730
      LI(66) = M                                                        DIM 1740
C                                                                       DIM 1750
      M = M + 1                                                         DIM 1760
      VAR(M) = 'A0(N26)                                 LANU        68' DIM 1770
      KL(M) =      N26                                                  DIM 1780
      LI(68) = M                                                        DIM 1790
C                                                                       DIM 1800
      M = M + 1                                                         DIM 1810
      VAR(M) = 'A1(N26)                                 LA1         69' DIM 1820
      KL(M) =      N26                                                  DIM 1830
      LI(69) = M                                                        DIM 1840
C                                                                       DIM 1850
      M = M + 1                                                         DIM 1860
      VAR(M) = 'A2(N26)                                 LA2         70' DIM 1870
      KL(M) =      N26                                                  DIM 1880
      LI(70) = M                                                        DIM 1890
C                                                                       DIM 1900
      M = M + 1                                                         DIM 1910
      VAR(M) = 'A3(N26)                                 LA3         71' DIM 1920
      KL(M) =      N26                                                  DIM 1930
      LI(71) = M                                                        DIM 1940
C                                                                       DIM 1950
      M = M + 1                                                         DIM 1960
      VAR(M) = 'A4(N26)                                 LA4         72' DIM 1970
      KL(M) =      N26                                                  DIM 1980
      LI(72) = M                                                        DIM 1990
C                                                                       DIM 2000
      M = M + 1                                                         DIM 2010
      VAR(M) = 'A5(N26)                                 LA5         73' DIM 2020
      KL(M) =      N26                                                  DIM 2030
      LI(73) = M                                                        DIM 2040
C                                                                       DIM 2050
      M = M + 1                                                         DIM 2060
      VAR(M) = 'BUG(N26)                                LBUG        79' DIM 2070
      KL(M) =       N26                                                 DIM 2080
      LI(79) = M                                                        DIM 2090
C                                                                       DIM 2100
      M = M + 1                                                         DIM 2110
      VAR(M) = 'FEEO(N26)                               LFEEO       83' DIM 2120
      KL(M) =        N26                                                DIM 2130
      LI(83) = M                                                        DIM 2140
C                                                                       DIM 2150
      M = M + 1                                                         DIM 2160
      VAR(M) = 'BUK(N26)                                LBUK        85' DIM 2170
      KL(M) =       N26                                                 DIM 2180
      LI(85) = M                                                        DIM 2190
C                                                                       DIM 2200
      M = M + 1                                                         DIM 2210
      VAR(M) = 'PHIN(N26)                               LPHIN      179' DIM 2220
      KL(M) =        N26                                                DIM 2230
      LI(179) = M                                                       DIM 2240
C                                                                       DIM 2250
      M = M + 1                                                         DIM 2260
      VAR(M) = 'BUCKL(N26,5)                            LBUCL       95' DIM 2270
      KL(M) =         N26*5                                             DIM 2280
      LI(95) = M                                                        DIM 2290
C                                                                       DIM 2300
      M = M + 1                                                         DIM 2310
      VAR(M) = 'PARPHI(N26,5)                           LPARP       67' DIM 2320
      KL(M) =          N26*5                                            DIM 2330
      LI(67) = M                                                        DIM 2340
C                                                                       DIM 2350
      M = M + 1                                                         DIM 2360
      VAR(M) = 'FEE(N26,150)                            LFEE        82' DIM 2370
      KL(M) =       N26*150                                             DIM 2380
      LI(82) = M                                                        DIM 2390
C                                                                       DIM 2400
CFZJ011 Increase dimensions of arrays SGA, SGTR, FKEN         01.12.03  DIM 2410
      M = M + 1                                                         DIM 2420
      VAR(M) = 'SGA(20,N26)                             LSGA        91' DIM 2430
      KL(M) =       20*N26                                              DIM 2440
      LI(91) = M                                                        DIM 2450
C                                                                       DIM 2460
      M = M + 1                                                         DIM 2470
      VAR(M) = 'SGTR(20,N26,N26)                        LSGTR       92' DIM 2480
      KL(M) =        20*N26*N26                                         DIM 2490
      LI(92) = M                                                        DIM 2500
C                                                                       DIM 2510
      M = M + 1                                                         DIM 2520
      VAR(M) = 'V1(201,N26,3)                           LV1         93' DIM 2530
      KL(M) =      201*N26*3                                            DIM 2540
      LI(93) = M                                                        DIM 2550
C                                                                       DIM 2560
      M = M + 1                                                         DIM 2570
      VAR(M) = 'FKEN(20,N26)                            LFKEN       94' DIM 2580
      KL(M) =        20*N26                                             DIM 2590
      LI(94) = M                                                        DIM 2600
C                                                                       DIM 2610
      M = M + 1                                                         DIM 2620
      VAR(M) = 'B(KMAT+2,N26*2+2)                       LB         101' DIM 2630
      KL(M) =    (KMAT+2)*(N26*2+2)                                     DIM 2640
      LI(101) = M                                                       DIM 2650
C                                                                       DIM 2660
CFZJ042                                                       09.09.05  DIM 2670
      M = M + 1                                                         DIM 2680
      LA0 = NDR*KMAT*N26                                                DIM 2690
      VAR(M) = 'OUSIG(LA0), LA0=NDR*KMAT*N26            LOUSI        3' DIM 2700
      KL(M) =         LA0                                               DIM 2710
      LI(3) = M                                                         DIM 2720
C                                                                       DIM 2730
      M = M + 1                                                         DIM 2740
      VAR(M) = 'TOSIG(LA0)                              LTOSI        4' DIM 2750
      KL(M) =         LA0                                               DIM 2760
      LI(4) = M                                                         DIM 2770
C                                                                       DIM 2780
      M = M + 1                                                         DIM 2790
      VAR(M) = 'ABSIG(LA0)                              LABSI        5' DIM 2800
      KL(M) =         LA0                                               DIM 2810
      LI(5) = M                                                         DIM 2820
C                                                                       DIM 2830
CFZJ042                                                       09.09.05  DIM 2840
      M = M + 1                                                         DIM 2850
      LF0 = NDR*IACT*N26                                                DIM 2860
      VAR(M) = 'FISIG(LF0), LF0=NDR*IACT*N26            LFISI        6' DIM 2870
      KL(M) =         LF0                                               DIM 2880
      LI(6) = M                                                         DIM 2890
C                                                                       DIM 2900
      M = M + 1                                                         DIM 2910
      VAR(M) = 'XNU(LF0)                                LXNU         7' DIM 2920
      KL(M) =       LF0                                                 DIM 2930
      LI(7) = M                                                         DIM 2940
C                                                                       DIM 2950
      M = M + 1                                                         DIM 2960
      VAR(M) = 'SN2N(LF0)                               LSN2N      178' DIM 2970
      KL(M) =        LF0                                                DIM 2980
      LI(178) = M                                                       DIM 2990
C                                                                       DIM 3000
CFZJ042                                                       09.09.05  DIM 3010
      M = M + 1                                                         DIM 3020
      LT0 = NDR*KMAT*LIMT                                               DIM 3030
      VAR(M) = 'TRSIG(LT0), LT0=NDR*KMAT*LIMT           LTRSI      180' DIM 3040
      KL(M) =         LT0                                               DIM 3050
      LI(180) = M                                                       DIM 3060
C                                                                       DIM 3070
      M = M + 1                                                         DIM 3080
      VAR(M) = 'NHOT(N200)                              LNHOT       28' DIM 3090
      KL(M) =        N200                                               DIM 3100
      LI(28) = M                                                        DIM 3110
C                                                                       DIM 3120
      M = M + 1                                                         DIM 3130
      VAR(M) = 'VOL(N200)                               LVOL        29' DIM 3140
      KL(M) =       N200                                                DIM 3150
      LI(29) = M                                                        DIM 3160
C                                                                       DIM 3170
      M = M + 1                                                         DIM 3180
      VAR(M) = 'VPART(N200)                             LVPAR       30' DIM 3190
      KL(M) =         N200                                              DIM 3200
      LI(30) = M                                                        DIM 3210
C                                                                       DIM 3220
      M = M + 1                                                         DIM 3230
      VAR(M) = 'XXXNEW(N200)                            LXXXN       34' DIM 3240
      KL(M) =          N200                                             DIM 3250
      LI(34) = M                                                        DIM 3260
C                                                                       DIM 3270
      M = M + 1                                                         DIM 3280
      VAR(M) = 'YYYNEW(N200)                            LYYYN       35' DIM 3290
      KL(M) =          N200                                             DIM 3300
      LI(35) = M                                                        DIM 3310
C                                                                       DIM 3320
      M = M + 1                                                         DIM 3330
      VAR(M) = 'CRNEW(N200)                             LCRNE       36' DIM 3340
      KL(M) =         N200                                              DIM 3350
      LI(36) = M                                                        DIM 3360
C                                                                       DIM 3370
      M = M + 1                                                         DIM 3380
      VAR(M) = 'DENIOD(N200)                            LDENI       37' DIM 3390
      KL(M) =          N200                                             DIM 3400
      LI(37) = M                                                        DIM 3410
C                                                                       DIM 3420
      M = M + 1                                                         DIM 3430
      VAR(M) = 'YIELDX(N200)                            LYIEX       38' DIM 3440
      KL(M) =          N200                                             DIM 3450
      LI(38) = M                                                        DIM 3460
C                                                                       DIM 3470
      M = M + 1                                                         DIM 3480
      VAR(M) = 'YIELDJ(N200)                            LYIEJ       39' DIM 3490
      KL(M) =          N200                                             DIM 3500
      LI(39) = M                                                        DIM 3510
C                                                                       DIM 3520
      M = M + 1                                                         DIM 3530
      VAR(M) = 'HM(N200)                                LHM         40' DIM 3540
      KL(M) =      N200                                                 DIM 3550
      LI(40) = M                                                        DIM 3560
C                                                                       DIM 3570
      M = M + 1                                                         DIM 3580
      VAR(M) = 'THBURN(N200)                            LTHBU       41' DIM 3590
      KL(M) =          N200                                             DIM 3600
      LI(41) = M                                                        DIM 3610
C                                                                       DIM 3620
      M = M + 1                                                         DIM 3630
      VAR(M) = 'FADOS3(N200)                            LFADO       42' DIM 3640
      KL(M) =          N200                                             DIM 3650
      LI(42) = M                                                        DIM 3660
C                                                                       DIM 3670
      M = M + 1                                                         DIM 3680
      VAR(M) = 'NRG(N200)                               LNRG        48' DIM 3690
      KL(M) =       N200                                                DIM 3700
      LI(48) = M                                                        DIM 3710
C                                                                       DIM 3720
      M = M + 1                                                         DIM 3730
      VAR(M) = 'NOPOW(N200)                             LNOPO       51' DIM 3740
      KL(M) =         N200                                              DIM 3750
      LI(51) = M                                                        DIM 3760
C                                                                       DIM 3770
      M = M + 1                                                         DIM 3780
      VAR(M) = 'HMETAL(N200)                            LHMET       52' DIM 3790
      KL(M) =          N200                                             DIM 3800
      LI(52) = M                                                        DIM 3810
C                                                                       DIM 3820
      M = M + 1                                                         DIM 3830
      VAR(M) = 'TCHG2(N200)                             LTCH2       54' DIM 3840
      KL(M) =         N200                                              DIM 3850
      LI(54) = M                                                        DIM 3860
C                                                                       DIM 3870
      M = M + 1                                                         DIM 3880
      VAR(M) = 'KRESHZ(N200)                            LKRES       56' DIM 3890
      KL(M) =          N200                                             DIM 3900
      LI(56) = M                                                        DIM 3910
C                                                                       DIM 3920
      M = M + 1                                                         DIM 3930
      VAR(M) = 'LRZN(N200)                              LLRZN       57' DIM 3940
      KL(M) =        N200                                               DIM 3950
      LI(57) = M                                                        DIM 3960
C                                                                       DIM 3970
      M = M + 1                                                         DIM 3980
      VAR(M) = 'NTYP2(N200)                             LNTY2       58' DIM 3990
      KL(M) =         N200                                              DIM 4000
      LI(58) = M                                                        DIM 4010
C                                                                       DIM 4020
      M = M + 1                                                         DIM 4030
      VAR(M) = 'NNEU(N200)                              LNNEU      100' DIM 4040
      KL(M) =        N200                                               DIM 4050
      LI(100) = M                                                       DIM 4060
C                                                                       DIM 4070
      M = M + 1                                                         DIM 4080
      VAR(M) = 'NRY(N200)                               LNRY       102' DIM 4090
      KL(M) =       N200                                                DIM 4100
      LI(102) = M                                                       DIM 4110
C                                                                       DIM 4120
      M = M + 1                                                         DIM 4130
      VAR(M) = 'FIRATE(N200)                            LFIRA      104' DIM 4140
      KL(M) =          N200                                             DIM 4150
      LI(104) = M                                                       DIM 4160
C                                                                       DIM 4170
      M = M + 1                                                         DIM 4180
      VAR(M) = 'ABSXE(N200)                             LABSX      105' DIM 4190
      KL(M) =         N200                                              DIM 4200
      LI(105) = M                                                       DIM 4210
C                                                                       DIM 4220
CFZJ042                                                       09.09.05  DIM 4230
      M = M + 1                                                         DIM 4240
      VAR(M) = 'AGEFAC(N200)                            LAGEF       26' DIM 4250
      KL(M) =          N200                                             DIM 4260
      LI(26) = M                                                        DIM 4270
C                                                                       DIM 4280
      M = M + 1                                                         DIM 4290
      VAR(M) = 'DEN(KMAT,N200)                          LDEN        31' DIM 4300
      KL(M) =       KMAT*N200                                           DIM 4310
      LI(31) = M                                                        DIM 4320
C                                                                       DIM 4330
      M = M + 1                                                         DIM 4340
      VAR(M) = 'SS(N26,N200)                            LSS         32' DIM 4350
      KL(M) =      N26*N200                                             DIM 4360
      LI(32) = M                                                        DIM 4370
C                                                                       DIM 4380
      M = M + 1                                                         DIM 4390
      VAR(M) = 'FISMAC(N26,N200)                        LFISM       33' DIM 4400
      KL(M) =          N26*N200                                         DIM 4410
      LI(33) = M                                                        DIM 4420
C                                                                       DIM 4430
      M = M + 1                                                         DIM 4440
      VAR(M) = 'HMNULL(IACT,N200)                       LHMNU       53' DIM 4450
      KL(M) =          IACT*N200                                        DIM 4460
      LI(53) = M                                                        DIM 4470
C                                                                       DIM 4480
      M = M + 1                                                         DIM 4490
      VAR(M) = 'GM(IACT,N200)                           LGM        153' DIM 4500
      KL(M) =      IACT*N200                                            DIM 4510
      LI(153) = M                                                       DIM 4520
C                                                                       DIM 4530
      M = M + 1                                                         DIM 4540
      VAR(M) = 'TCHG1(N200+N40)                         LTCH1       59' DIM 4550
      KL(M) =         N200+N40                                          DIM 4560
      LI(59) = M                                                        DIM 4570
C                                                                       DIM 4580
      M = M + 1                                                         DIM 4590
      VAR(M) = 'NALT(N200+N40)                          LNALT       60' DIM 4600
      KL(M) =        N200+N40                                           DIM 4610
      LI(60) = M                                                        DIM 4620
C                                                                       DIM 4630
      M = M + 1                                                         DIM 4640
      VAR(M) = 'NTYP1(N200+N40)                         LNTY1       61' DIM 4650
      KL(M) =         N200+N40                                          DIM 4660
      LI(61) = M                                                        DIM 4670
C                                                                       DIM 4680
      M = M + 1                                                         DIM 4690
      VAR(M) = 'VCHG(N200+N40)                          LVCHG      110' DIM 4700
      KL(M) =        N200+N40                                           DIM 4710
      LI(110) = M                                                       DIM 4720
C                                                                       DIM 4730
      M = M + 1                                                         DIM 4740
      VAR(M) = 'U3IN(N200+N40)                          LU3IN      123' DIM 4750
      KL(M) =        N200+N40                                           DIM 4760
      LI(123) = M                                                       DIM 4770
C                                                                       DIM 4780
      M = M + 1                                                         DIM 4790
      VAR(M) = 'U3EX(N200+N40)                          LU3EX      124' DIM 4800
      KL(M) =        N200+N40                                           DIM 4810
      LI(124) = M                                                       DIM 4820
C                                                                       DIM 4830
      M = M + 1                                                         DIM 4840
      VAR(M) = 'U5IN(N200+N40)                          LU5IN      125' DIM 4850
      KL(M) =        N200+N40                                           DIM 4860
      LI(125) = M                                                       DIM 4870
C                                                                       DIM 4880
      M = M + 1                                                         DIM 4890
      VAR(M) = 'U5EX(N200+N40)                          LU5EX      126' DIM 4900
      KL(M) =        N200+N40                                           DIM 4910
      LI(126) = M                                                       DIM 4920
C                                                                       DIM 4930
      M = M + 1                                                         DIM 4940
      VAR(M) = 'THIN(N200+N40)                          LTHIN      127' DIM 4950
      KL(M) =        N200+N40                                           DIM 4960
      LI(127) = M                                                       DIM 4970
C                                                                       DIM 4980
      M = M + 1                                                         DIM 4990
      VAR(M) = 'THEX(N200+N40)                          LTHEX      128' DIM 5000
      KL(M) =        N200+N40                                           DIM 5010
      LI(128) = M                                                       DIM 5020
C                                                                       DIM 5030
      M = M + 1                                                         DIM 5040
      VAR(M) = 'U8IN(N200+N40)                          LU8IN      129' DIM 5050
      KL(M) =        N200+N40                                           DIM 5060
      LI(129) = M                                                       DIM 5070
C                                                                       DIM 5080
      M = M + 1                                                         DIM 5090
      VAR(M) = 'U8EX(N200+N40)                          LU8EX      130' DIM 5100
      KL(M) =        N200+N40                                           DIM 5110
      LI(130) = M                                                       DIM 5120
C                                                                       DIM 5130
      M = M + 1                                                         DIM 5140
      VAR(M) = 'SM1(N200+N40)                           LSM1       131' DIM 5150
      KL(M) =       N200+N40                                            DIM 5160
      LI(131) = M                                                       DIM 5170
C                                                                       DIM 5180
      M = M + 1                                                         DIM 5190
      VAR(M) = 'SM2(N200+N40)                           LSM2       132' DIM 5200
      KL(M) =       N200+N40                                            DIM 5210
      LI(132) = M                                                       DIM 5220
C                                                                       DIM 5230
      M = M + 1                                                         DIM 5240
      VAR(M) = 'U6IN(N200+N40)                          LU6IN      133' DIM 5250
      KL(M) =        N200+N40                                           DIM 5260
      LI(133) = M                                                       DIM 5270
C                                                                       DIM 5280
      M = M + 1                                                         DIM 5290
      VAR(M) = 'U6EX(N200+N40)                          LU6EX      134' DIM 5300
      KL(M) =        N200+N40                                           DIM 5310
      LI(134) = M                                                       DIM 5320
C                                                                       DIM 5330
      M = M + 1                                                         DIM 5340
      VAR(M) = 'PU39IN(N200+N40)                        LPU9I      135' DIM 5350
      KL(M) =          N200+N40                                         DIM 5360
      LI(135) = M                                                       DIM 5370
C                                                                       DIM 5380
      M = M + 1                                                         DIM 5390
      VAR(M) = 'PU39EX(N200+N40)                        LPU9E      136' DIM 5400
      KL(M) =          N200+N40                                         DIM 5410
      LI(136) = M                                                       DIM 5420
C                                                                       DIM 5430
      M = M + 1                                                         DIM 5440
      VAR(M) = 'PU40IN(N200+N40)                        LPU0I      137' DIM 5450
      KL(M) =          N200+N40                                         DIM 5460
      LI(137) = M                                                       DIM 5470
C                                                                       DIM 5480
      M = M + 1                                                         DIM 5490
      VAR(M) = 'PU40EX(N200+N40)                        LPU0E      138' DIM 5500
      KL(M) =          N200+N40                                         DIM 5510
      LI(138) = M                                                       DIM 5520
C                                                                       DIM 5530
      M = M + 1                                                         DIM 5540
      VAR(M) = 'PU41IN(N200+N40)                        LPU1I      139' DIM 5550
      KL(M) =          N200+N40                                         DIM 5560
      LI(139) = M                                                       DIM 5570
C                                                                       DIM 5580
      M = M + 1                                                         DIM 5590
      VAR(M) = 'PU41EX(N200+N40)                        LPU1E      140' DIM 5600
      KL(M) =          N200+N40                                         DIM 5610
      LI(140) = M                                                       DIM 5620
C                                                                       DIM 5630
      M = M + 1                                                         DIM 5640
      VAR(M) = 'PU42IN(N200+N40)                        LPU2I      141' DIM 5650
      KL(M) =          N200+N40                                         DIM 5660
      LI(141) = M                                                       DIM 5670
C                                                                       DIM 5680
      M = M + 1                                                         DIM 5690
      VAR(M) = 'PU42EX(N200+N40)                        LPU2E      142' DIM 5700
      KL(M) =          N200+N40                                         DIM 5710
      LI(142) = M                                                       DIM 5720
C                                                                       DIM 5730
      M = M + 1                                                         DIM 5740
      VAR(M) = 'PA33IN(N200+N40)                        LPA3I      156' DIM 5750
      KL(M) =          N200+N40                                         DIM 5760
      LI(156) = M                                                       DIM 5770
C                                                                       DIM 5780
      M = M + 1                                                         DIM 5790
      VAR(M) = 'PA33EX(N200+N40)                        LPA3E      157' DIM 5800
      KL(M) =          N200+N40                                         DIM 5810
      LI(157) = M                                                       DIM 5820
C                                                                       DIM 5830
      M = M + 1                                                         DIM 5840
      VAR(M) = 'U4IN(N200+N40)                          LU4IN      158' DIM 5850
      KL(M) =        N200+N40                                           DIM 5860
      LI(158) = M                                                       DIM 5870
C                                                                       DIM 5880
      M = M + 1                                                         DIM 5890
      VAR(M) = 'U4EX(N200+N40)                          LU4EX      159' DIM 5900
      KL(M) =        N200+N40                                           DIM 5910
      LI(159) = M                                                       DIM 5920
C                                                                       DIM 5930
      M = M + 1                                                         DIM 5940
      VAR(M) = 'NP37IN(N200+N40)                        LNP7I      160' DIM 5950
      KL(M) =          N200+N40                                         DIM 5960
      LI(160) = M                                                       DIM 5970
C                                                                       DIM 5980
      M = M + 1                                                         DIM 5990
      VAR(M) = 'NP37EX(N200+N40)                        LNP7E      161' DIM 6000
      KL(M) =          N200+N40                                         DIM 6010
      LI(161) = M                                                       DIM 6020
C                                                                       DIM 6030
      M = M + 1                                                         DIM 6040
      VAR(M) = 'PU38IN(N200+N40)                        LPU8I      162' DIM 6050
      KL(M) =          N200+N40                                         DIM 6060
      LI(162) = M                                                       DIM 6070
C                                                                       DIM 6080
      M = M + 1                                                         DIM 6090
      VAR(M) = 'PU38EX(N200+N40)                        LPU8E      163' DIM 6100
      KL(M) =          N200+N40                                         DIM 6110
      LI(163) = M                                                       DIM 6120
C                                                                       DIM 6130
      M = M + 1                                                         DIM 6140
      VAR(M) = 'AM41IN(N200+N40)                        LAM1I      164' DIM 6150
      KL(M) =          N200+N40                                         DIM 6160
      LI(164) = M                                                       DIM 6170
C                                                                       DIM 6180
      M = M + 1                                                         DIM 6190
      VAR(M) = 'AM41EX(N200+N40)                        LAM1E      165' DIM 6200
      KL(M) =          N200+N40                                         DIM 6210
      LI(165) = M                                                       DIM 6220
C                                                                       DIM 6230
      M = M + 1                                                         DIM 6240
      VAR(M) = 'AM2MIN(N200+N40)                        LA2MI      166' DIM 6250
      KL(M) =          N200+N40                                         DIM 6260
      LI(166) = M                                                       DIM 6270
C                                                                       DIM 6280
      M = M + 1                                                         DIM 6290
      VAR(M) = 'AM2MEX(N200+N40)                        LA2ME      167' DIM 6300
      KL(M) =          N200+N40                                         DIM 6310
      LI(167) = M                                                       DIM 6320
C                                                                       DIM 6330
      M = M + 1                                                         DIM 6340
      VAR(M) = 'AM42IN(N200+N40)                        LAM2I      168' DIM 6350
      KL(M) =          N200+N40                                         DIM 6360
      LI(168) = M                                                       DIM 6370
C                                                                       DIM 6380
      M = M + 1                                                         DIM 6390
      VAR(M) = 'AM42EX(N200+N40)                        LAM2E      169' DIM 6400
      KL(M) =          N200+N40                                         DIM 6410
      LI(169) = M                                                       DIM 6420
C                                                                       DIM 6430
      M = M + 1                                                         DIM 6440
      VAR(M) = 'AM43IN(N200+N40)                        LAM3I      170' DIM 6450
      KL(M) =          N200+N40                                         DIM 6460
      LI(170) = M                                                       DIM 6470
C                                                                       DIM 6480
      M = M + 1                                                         DIM 6490
      VAR(M) = 'AM43EX(N200+N40)                        LAM3E      171' DIM 6500
      KL(M) =          N200+N40                                         DIM 6510
      LI(171) = M                                                       DIM 6520
C                                                                       DIM 6530
      M = M + 1                                                         DIM 6540
      VAR(M) = 'CM42IN(N200+N40)                        LCM2I      172' DIM 6550
      KL(M) =          N200+N40                                         DIM 6560
      LI(172) = M                                                       DIM 6570
C                                                                       DIM 6580
      M = M + 1                                                         DIM 6590
      VAR(M) = 'CM42EX(N200+N40)                        LCM2E      173' DIM 6600
      KL(M) =          N200+N40                                         DIM 6610
      LI(173) = M                                                       DIM 6620
C                                                                       DIM 6630
      M = M + 1                                                         DIM 6640
      VAR(M) = 'CM43IN(N200+N40)                        LCM3I      174' DIM 6650
      KL(M) =          N200+N40                                         DIM 6660
      LI(174) = M                                                       DIM 6670
C                                                                       DIM 6680
      M = M + 1                                                         DIM 6690
      VAR(M) = 'CM43EX(N200+N40)                        LCM3E      175' DIM 6700
      KL(M) =          N200+N40                                         DIM 6710
      LI(175) = M                                                       DIM 6720
C                                                                       DIM 6730
      M = M + 1                                                         DIM 6740
      VAR(M) = 'CM44IN(N200+N40)                        LCM4I      176' DIM 6750
      KL(M) =          N200+N40                                         DIM 6760
      LI(176) = M                                                       DIM 6770
C                                                                       DIM 6780
      M = M + 1                                                         DIM 6790
      VAR(M) = 'CM44EX(N200+N40)                        LCM4E      177' DIM 6800
      KL(M) =          N200+N40                                         DIM 6810
      LI(177) = M                                                       DIM 6820
C                                                                       DIM 6830
      M = M + 1                                                         DIM 6840
      VAR(M) = 'AIN(IACT,N200+N40)                      LAIN       108' DIM 6850
      KL(M) =       IACT*(N200+N40)                                     DIM 6860
      LI(108) = M                                                       DIM 6870
C                                                                       DIM 6880
      M = M + 1                                                         DIM 6890
      VAR(M) = 'AEX(IACT,N200+N40)                      LAEX       109' DIM 6900
      KL(M) =       IACT*(N200+N40)                                     DIM 6910
      LI(109) = M                                                       DIM 6920
C                                                                       DIM 6930
      M = M + 1                                                         DIM 6940
      VAR(M) = 'UMREG(NDR)                              LUMRE        8' DIM 6950
      KL(M) =         NDR                                               DIM 6960
      LI(8) = M                                                         DIM 6970
C                                                                       DIM 6980
      M = M + 1                                                         DIM 6990
      VAR(M) = 'VOLREG(NDR)                             LVOLR        9' DIM 7000
      KL(M) =          NDR                                              DIM 7010
      LI(9) = M                                                         DIM 7020
C                                                                       DIM 7030
      M = M + 1                                                         DIM 7040
      VAR(M) = 'IREG(NDR)                               LIREG       10' DIM 7050
      KL(M) =        NDR                                                DIM 7060
      LI(10) = M                                                        DIM 7070
C                                                                       DIM 7080
      M = M + 1                                                         DIM 7090
      VAR(M) = 'NBURN(NDR)                              LNBUR       12' DIM 7100
      KL(M) =         NDR                                               DIM 7110
      LI(12) = M                                                        DIM 7120
C                                                                       DIM 7130
      M = M + 1                                                         DIM 7140
      VAR(M) = 'PINMIN(NDR)                             LPINI       21' DIM 7150
      KL(M) =          NDR                                              DIM 7160
      LI(21) = M                                                        DIM 7170
C                                                                       DIM 7180
      M = M + 1                                                         DIM 7190
      VAR(M) = 'PINMAX(NDR)                             LPINA       22' DIM 7200
      KL(M) =          NDR                                              DIM 7210
      LI(22) = M                                                        DIM 7220
C                                                                       DIM 7230
      M = M + 1                                                         DIM 7240
      VAR(M) = 'NPOIS(NDR)                              LNPOI       23' DIM 7250
      KL(M) =         NDR                                               DIM 7260
      LI(23) = M                                                        DIM 7270
C                                                                       DIM 7280
      M = M + 1                                                         DIM 7290
      VAR(M) = 'VFREG(NDR)                              LVFRE       24' DIM 7300
      KL(M) =         NDR                                               DIM 7310
      LI(24) = M                                                        DIM 7320
C                                                                       DIM 7330
      M = M + 1                                                         DIM 7340
      VAR(M) = 'MIX(NDR)                                LMIX       183' DIM 7350
      KL(M) =       NDR                                                 DIM 7360
      LI(183) = M                                                       DIM 7370
C                                                                       DIM 7380
CFZJ042                                                       09.09.05  DIM 7390
      M = M + 1                                                         DIM 7400
      VAR(M) = 'VREG(NDR)                               LVREG       27' DIM 7410
      KL(M) =        NDR                                                DIM 7420
      LI(27) = M                                                        DIM 7430
C                                                                       DIM 7440
CFZJ042                                                       09.09.05  DIM 7450
      M = M + 1                                                         DIM 7460
      VAR(M) = 'VRP(NDR)                                LVRP        47' DIM 7470
      KL(M) =       NDR                                                 DIM 7480
      LI(47) = M                                                        DIM 7490
C                                                                       DIM 7500
CFZJ042                                                       09.09.05  DIM 7510
      M = M + 1                                                         DIM 7520
      VAR(M) = 'ZLTH(NDR)                               LZLTH       25' DIM 7530
      KL(M) =        NDR                                                DIM 7540
      LI(25) = M                                                        DIM 7550
C                                                                       DIM 7560
      M = M + 1                                                         DIM 7570
      VAR(M) = 'NDES(NDR)                               LNDES       63' DIM 7580
      KL(M) =        NDR                                                DIM 7590
      LI(63) = M                                                        DIM 7600
C                                                                       DIM 7610
      M = M + 1                                                         DIM 7620
      VAR(M) = 'TEMZUT(NDR)                             LTEMZ       64' DIM 7630
      KL(M) =          NDR                                              DIM 7640
      LI(64) = M                                                        DIM 7650
C                                                                       DIM 7660
      M = M + 1                                                         DIM 7670
      VAR(M) = 'TFU(NDR)                                LTFU        97' DIM 7680
      KL(M) =       NDR                                                 DIM 7690
      LI(97) = M                                                        DIM 7700
C                                                                       DIM 7710
      M = M + 1                                                         DIM 7720
      VAR(M) = 'TMO(NDR)                                LTMO        98' DIM 7730
      KL(M) =       NDR                                                 DIM 7740
      LI(98) = M                                                        DIM 7750
C                                                                       DIM 7760
      M = M + 1                                                         DIM 7770
      VAR(M) = 'TRAT(NDR)                               LTRAT       99' DIM 7780
      KL(M) =        NDR                                                DIM 7790
      LI(99) = M                                                        DIM 7800
C                                                                       DIM 7810
      M = M + 1                                                         DIM 7820
      VAR(M) = 'DSP(NDR)                                LDSP       106' DIM 7830
      KL(M) =       NDR                                                 DIM 7840
      LI(106) = M                                                       DIM 7850
C                                                                       DIM 7860
      M = M + 1                                                         DIM 7870
      VAR(M) = 'COSSHM(NDR)                             LCOSS      145' DIM 7880
      KL(M) =          NDR                                              DIM 7890
      LI(145) = M                                                       DIM 7900
C                                                                       DIM 7910
      M = M + 1                                                         DIM 7920
      VAR(M) = 'KDES(NDR)                               LKDES      148' DIM 7930
      KL(M) =        NDR                                                DIM 7940
      LI(148) = M                                                       DIM 7950
C                                                                       DIM 7960
      M = M + 1                                                         DIM 7970
      VAR(M) = 'TCELS(5,NDR)                            LTCEL       65' DIM 7980
      KL(M) =         5*NDR                                             DIM 7990
      LI(65) = M                                                        DIM 8000
C                                                                       DIM 8010
      M = M + 1                                                         DIM 8020
      VAR(M) = 'AW(3,NDR)                               LAW         13' DIM 8030
      KL(M) =      3*NDR                                                DIM 8040
      LI(13) = M                                                        DIM 8050
C                                                                       DIM 8060
      M = M + 1                                                         DIM 8070
      VAR(M) = 'VLP(3,NDR)                              LVLP        14' DIM 8080
      KL(M) =       3*NDR                                               DIM 8090
      LI(14) = M                                                        DIM 8100
C                                                                       DIM 8110
      M = M + 1                                                         DIM 8120
      VAR(M) = 'WLP(3,NDR)                              LWLP        15' DIM 8130
      KL(M) =       3*NDR                                               DIM 8140
      LI(15) = M                                                        DIM 8150
C                                                                       DIM 8160
      M = M + 1                                                         DIM 8170
      VAR(M) = 'RHO(3,NDR)                              LRHO        16' DIM 8180
      KL(M) =       3*NDR                                               DIM 8190
      LI(16) = M                                                        DIM 8200
C                                                                       DIM 8210
      M = M + 1                                                         DIM 8220
      VAR(M) = 'CFRAC(2,NDR)                            LCFRA       18' DIM 8230
      KL(M) =         2*NDR                                             DIM 8240
      LI(18) = M                                                        DIM 8250
C                                                                       DIM 8260
      M = M + 1                                                         DIM 8270
      VAR(M) = 'POISM(2,NDR)                            LPOIM       19' DIM 8280
      KL(M) =         2*NDR                                             DIM 8290
      LI(19) = M                                                        DIM 8300
C                                                                       DIM 8310
      M = M + 1                                                         DIM 8320
      VAR(M) = 'POISL(2,NDR)                            LPOIL       20' DIM 8330
      KL(M) =         2*NDR                                             DIM 8340
      LI(20) = M                                                        DIM 8350
C                                                                       DIM 8360
CFZJ042                                                       09.09.05  DIM 8370
      M = M + 1                                                         DIM 8380
      VAR(M) = 'BUCKS(N26,NDR)                          LBUCK       62' DIM 8390
      KL(M) =         N26*NDR                                           DIM 8400
      LI(62) = M                                                        DIM 8410
C                                                                       DIM 8420
      M = M + 1                                                         DIM 8430
      VAR(M) = 'RPHIAV(N26,NDR)                         LRPHI       11' DIM 8440
      KL(M) =          N26*NDR                                          DIM 8450
      LI(11) = M                                                        DIM 8460
C                                                                       DIM 8470
      M = M + 1                                                         DIM 8480
      VAR(M) = 'SSLUMP(N26,NDR)                         LSSLU       17' DIM 8490
      KL(M) =          N26*NDR                                          DIM 8500
      LI(17) = M                                                        DIM 8510
C                                                                       DIM 8520
      M = M + 1                                                         DIM 8530
      VAR(M) = 'SCTR(NDR,LIMT)                          LSCTR      181' DIM 8540
      KL(M) =        NDR*LIMT                                           DIM 8550
      LI(181) = M                                                       DIM 8560
C                                                                       DIM 8570
      M = M + 1                                                         DIM 8580
      VAR(M) = 'NOPOWK(KROT)                            LNOPK        2' DIM 8590
      KL(M) =          KROT                                             DIM 8600
      LI(2) = M                                                         DIM 8610
C                                                                       DIM 8620
      M = M + 1                                                         DIM 8630
      VAR(M) = 'XSIGS(KROT,N26)                         LXSIS       43' DIM 8640
      KL(M) =         KROT*N26                                          DIM 8650
      LI(43) = M                                                        DIM 8660
C                                                                       DIM 8670
      M = M + 1                                                         DIM 8680
      VAR(M) = 'XTOTL(KROT,N26)                         LXTOT       44' DIM 8690
      KL(M) =         KROT*N26                                          DIM 8700
      LI(44) = M                                                        DIM 8710
C                                                                       DIM 8720
      M = M + 1                                                         DIM 8730
      VAR(M) = 'XSIGA(KROT,N26)                         LXSIA       45' DIM 8740
      KL(M) =         KROT*N26                                          DIM 8750
      LI(45) = M                                                        DIM 8760
C                                                                       DIM 8770
      M = M + 1                                                         DIM 8780
      VAR(M) = 'XNFIS(KROT,N26)                         LXNFI       46' DIM 8790
      KL(M) =         KROT*N26                                          DIM 8800
      LI(46) = M                                                        DIM 8810
C                                                                       DIM 8820
      M = M + 1                                                         DIM 8830
      VAR(M) = 'CNU(KROT,N26)                           LCNU        96' DIM 8840
      KL(M) =       KROT*N26                                            DIM 8850
      LI(96) = M                                                        DIM 8860
C                                                                       DIM 8870
      M = M + 1                                                         DIM 8880
      VAR(M) = 'XSIGTR(KROT,LIMT)                       LXSIG      182' DIM 8890
      KL(M) =          KROT*LIMT                                        DIM 8900
      LI(182) = M                                                       DIM 8910
C                                                                       DIM 8920
      M = M + 1                                                         DIM 8930
      VAR(M) = 'AK(MMAF)                                LAK        111' DIM 8940
      KL(M) =      MMAF                                                 DIM 8950
      LI(111) = M                                                       DIM 8960
C                                                                       DIM 8970
      M = M + 1                                                         DIM 8980
      VAR(M) = 'DK(MMAF)                                LDK        112' DIM 8990
      KL(M) =      MMAF                                                 DIM 9000
      LI(112) = M                                                       DIM 9010
C                                                                       DIM 9020
      M = M + 1                                                         DIM 9030
      VAR(M) = 'FK(MMAF)                                LFK        113' DIM 9040
      KL(M) =      MMAF                                                 DIM 9050
      LI(113) = M                                                       DIM 9060
C                                                                       DIM 9070
      M = M + 1                                                         DIM 9080
      VAR(M) = 'BK(MMAF)                                LBK        114' DIM 9090
      KL(M) =      MMAF                                                 DIM 9100
      LI(114) = M                                                       DIM 9110
C                                                                       DIM 9120
      M = M + 1                                                         DIM 9130
      VAR(M) = '$K(MMAF)                                L$K        115' DIM 9140
      KL(M) =      MMAF                                                 DIM 9150
      LI(115) = M                                                       DIM 9160
C                                                                       DIM 9170
      M = M + 1                                                         DIM 9180
      VAR(M) = 'EKWH(MMAF)                              LEKWH      116' DIM 9190
      KL(M) =        MMAF                                               DIM 9200
      LI(116) = M                                                       DIM 9210
C                                                                       DIM 9220
      M = M + 1                                                         DIM 9230
      VAR(M) = 'ET(MMAF)                                LET        117' DIM 9240
      KL(M) =      MMAF                                                 DIM 9250
      LI(117) = M                                                       DIM 9260
C                                                                       DIM 9270
      M = M + 1                                                         DIM 9280
      VAR(M) = 'ORE1(MMAF)                              LORE1      118' DIM 9290
      KL(M) =        MMAF                                               DIM 9300
      LI(118) = M                                                       DIM 9310
C                                                                       DIM 9320
      M = M + 1                                                         DIM 9330
      VAR(M) = 'ORE2(MMAF)                              LORE2      119' DIM 9340
      KL(M) =        MMAF                                               DIM 9350
      LI(119) = M                                                       DIM 9360
C                                                                       DIM 9370
      M = M + 1                                                         DIM 9380
      VAR(M) = 'SEP1(MMAF)                              LSEP1      120' DIM 9390
      KL(M) =        MMAF                                               DIM 9400
      LI(120) = M                                                       DIM 9410
C                                                                       DIM 9420
      M = M + 1                                                         DIM 9430
      VAR(M) = 'SEP2(MMAF)                              LSEP2      121' DIM 9440
      KL(M) =        MMAF                                               DIM 9450
      LI(121) = M                                                       DIM 9460
C                                                                       DIM 9470
      M = M + 1                                                         DIM 9480
      VAR(M) = 'UUKOST(MMAF)                            LUUKO      122' DIM 9490
      KL(M) =          MMAF                                             DIM 9500
      LI(122) = M                                                       DIM 9510
CFZJ043                                                       23.09.05  DIM 9520
C                                                                       DIM 9530
      M = M + 1                                                         DIM 9540
      VAR(M) = 'IBOX(MBOX), MBOX=MBATCH+MSTOB           LIBOX      147' DIM 9550
      KL(M) =        MBOX                                               DIM 9560
      LI(147) = M                                                       DIM 9570
C                                                                       DIM 9580
      M = M + 1                                                         DIM 9590
      VAR(M) = 'RVCB(MBOX), MBOX=MBATCH+MSTOB           LRVCB      149' DIM 9600
      KL(M) =        MBOX                                               DIM 9610
      LI(149) = M                                                       DIM 9620
C                                                                       DIM 9630
      M = M + 1                                                         DIM 9640
      VAR(M) = 'JAD11(JD11)                             LJAD1      146' DIM 9650
      KL(M) =         JD11                                              DIM 9660
      LI(146) = M                                                       DIM 9670
C                                                                       DIM 9680
      KA(1) = 1                                                         DIM 9690
      WRITE (NT,10)                                                     DIM 9700
      DO 1 I=1,M                                                        DIM 9710
        IF(I .EQ. 1) GOTO 11                                            DIM 9720
        KA(I) = KA(I-1) + KL(I-1)                                       DIM 9730
   11   WRITE (NT,20) I,VAR(I),KA(I),KL(I)                              DIM 9740
    1 CONTINUE                                                          DIM 9750
      NENDP = KA(M) + KL(M) - 1                                         DIM 9760
      WRITE (NT,30) NENDP                                               DIM 9770
      KNU = IV - NENDP                                                  DIM 9780
      IF(KNU) 8,4,4                                                     DIM 9790
    8 CONTINUE                                                          DIM 9800
      WRITE (6,70) IABS(KNU)                                            DIM 9810
      STOP                                                              DIM 9820
C                                                                       DIM 9830
C                                                                       DIM 9840
    4 CONTINUE                                                          DIM 9850
C                                                                       DIM 9860
      IF(I3D .GT. 0) GOTO 99                                            DIM 9870
      N = 1                                                             DIM 9880
      VAT(N) = 'VL(N200)                                IVL          1' DIM 9890
      KY(N) =      N200                                                 DIM 9900
      LZ(1) = N                                                         DIM 9910
C                                                                       DIM 9920
      N = N + 1                                                         DIM 9930
      VAT(N) = 'BUR(N200)                               IBUR         2' DIM 9940
      KY(N) =       N200                                                DIM 9950
      LZ(2) = N                                                         DIM 9960
C                                                                       DIM 9970
      N = N + 1                                                         DIM 9980
      VAT(N) = 'DOS(N200)                               IDOS         3' DIM 9990
      KY(N) =       N200                                                DIM10000
      LZ(3) = N                                                         DIM10010
C                                                                       DIM10020
      N = N + 1                                                         DIM10030
      VAT(N) = 'POW(N200)                               IPOW         4' DIM10040
      KY(N) =       N200                                                DIM10050
      LZ(4) = N                                                         DIM10060
C                                                                       DIM10070
      N = N + 1                                                         DIM10080
      VAT(N) = 'JZON(N200)                              IJZON        5' DIM10090
      KY(N) =        N200                                               DIM10100
      LZ(5) = N                                                         DIM10110
CFZJ042                                                       09.09.05  DIM10120
C                                                                       DIM10130
      N = N + 1                                                         DIM10140
      VAT(N) = 'POWL(N200)                              IPOWL        7' DIM10150
      KY(N) =        N200                                               DIM10160
      LZ(7) = N                                                         DIM10170
C                                                                       DIM10180
      N = N + 1                                                         DIM10190
      VAT(N) = 'NOPOW(N200)                             INOPO       11' DIM10200
      KY(N) =         N200                                              DIM10210
      LZ(11) = N                                                        DIM10220
CFZJ042                                                       09.09.05  DIM10230
C                                                                       DIM10240
      N = N + 1                                                         DIM10250
      VAT(N) = 'DOSNRX(N200)                            IDOSN       15' DIM10260
      KY(N) =          N200                                             DIM10270
      LZ(15) = N                                                        DIM10280
C                                                                       DIM10290
      N = N + 1                                                         DIM10300
      VAT(N) = 'IZON(N200)                              IIZON       16' DIM10310
      KY(N) =        N200                                               DIM10320
      LZ(16) = N                                                        DIM10330
C                                                                       DIM10340
      N = N + 1                                                         DIM10350
      VAT(N) = 'FIMA(N200)                              IFIMA       17' DIM10360
      KY(N) =        N200                                               DIM10370
      LZ(17) = N                                                        DIM10380
C                                                                       DIM10390
      N = N + 1                                                         DIM10400
      VAT(N) = 'TK(N200)                                ITK         18' DIM10410
      KY(N) =      N200                                                 DIM10420
      LZ(18) = N                                                        DIM10430
C                                                                       DIM10440
      N = N + 1                                                         DIM10450
      VAT(N) = 'RK(N200)                                IRK         19' DIM10460
      KY(N) =      N200                                                 DIM10470
      LZ(19) = N                                                        DIM10480
C                                                                       DIM10490
      N = N + 1                                                         DIM10500
      VAT(N) = 'POV(N200)                               IPOV        20' DIM10510
      KY(N) =       N200                                                DIM10520
      LZ(20) = N                                                        DIM10530
C                                                                       DIM10540
      N = N + 1                                                         DIM10550
      VAT(N) = 'NRY(N200)                               INRY        22' DIM10560
      KY(N) =       N200                                                DIM10570
      LZ(22) = N                                                        DIM10580
C                                                                       DIM10590
      N = N + 1                                                         DIM10600
      VAT(N) = 'QNW(N200)                               IQNW        23' DIM10610
      KY(N) =       N200                                                DIM10620
      LZ(23) = N                                                        DIM10630
C                                                                       DIM10640
      N = N + 1                                                         DIM10650
      VAT(N) = 'HM(N200)                                IHM         24' DIM10660
      KY(N) =      N200                                                 DIM10670
      LZ(24) = N                                                        DIM10680
C                                                                       DIM10690
CFZJ042                                                       09.09.05  DIM10700
      N = N + 1                                                         DIM10710
      VAT(N) = 'DOSD(N200)                              IDOSD      230' DIM10720
      KY(N) =        N200                                               DIM10730
      LZ(230) = N                                                       DIM10740
C                                                                       DIM10750
CFZJ042                                                       09.09.05  DIM10760
      N = N + 1                                                         DIM10770
      VAT(N) = 'PBET(N200)                              IPBET      231' DIM10780
      KY(N) =        N200                                               DIM10790
      LZ(231) = N                                                       DIM10800
C                                                                       DIM10810
CFZJ042                                                       09.09.05  DIM10820
      N = N + 1                                                         DIM10830
      VAT(N) = 'TEML(3,N200)                            ITEML        8' DIM10840
      KY(N) =        3*N200                                             DIM10850
      LZ(8) = N                                                         DIM10860
C                                                                       DIM10870
      N = N + 1                                                         DIM10880
      VAT(N) = 'ZF(7,N200)                              IZF         21' DIM10890
      KY(N) =      7*N200                                               DIM10900
      LZ(21) = N                                                        DIM10910
C                                                                       DIM10920
      N = N + 1                                                         DIM10930
      VAT(N) = 'V(15,N200)                              IV           9' DIM10940
      KY(N) =     15*N200                                               DIM10950
      LZ(9) = N                                                         DIM10960
C                                                                       DIM10970
      N = N + 1                                                         DIM10980
      VAT(N) = 'PF(15,N200)                             IPF         10' DIM10990
      KY(N) =      15*N200                                              DIM11000
      LZ(10) = N                                                        DIM11010
C                                                                       DIM11020
      N = N + 1                                                         DIM11030
      VAT(N) = 'TB(50,N200)                             ITB        212' DIM11040
      KY(N) =      50*N200                                              DIM11050
      LZ(212) = N                                                       DIM11060
C                                                                       DIM11070
      N = N + 1                                                         DIM11080
      VAT(N) = 'PGES(50,N200)                           IPGES      213' DIM11090
      KY(N) =        50*N200                                            DIM11100
      LZ(213) = N                                                       DIM11110
C                                                                       DIM11120
      N = N + 1                                                         DIM11130
      VAT(N) = 'ZD(7,50,N200)       ("ZF" IN DECHEAT)   IZD        214' DIM11140
      KY(N) =      7*50*N200                                            DIM11150
      LZ(214) = N                                                       DIM11160
C                                                                       DIM11170
      N = N + 1                                                         DIM11180
      VAT(N) = 'NCOLA(NDR)                              INCOL       13' DIM11190
      KY(N) =         NDR                                               DIM11200
      LZ(13) = N                                                        DIM11210
C                                                                       DIM11220
      N = N + 1                                                         DIM11230
      VAT(N) = 'VCOLA(NDR)                              IVCOL       14' DIM11240
      KY(N) =         NDR                                               DIM11250
      LZ(14) = N                                                        DIM11260
C                                                                       DIM11270
      N = N + 1                                                         DIM11280
      VAT(N) = 'IREG(NDR)                               IIREG       28' DIM11290
      KY(N) =        NDR                                                DIM11300
      LZ(28) = N                                                        DIM11310
CFZJ042                                                       09.09.05  DIM11320
C                                                                       DIM11330
CFZJ042                                                       09.09.05  DIM11340
      N = N + 1                                                         DIM11350
      VAT(N) = 'POWPART(NDR,15)                         IPOWP      226' DIM11360
      KY(N) =           NDR*15                                          DIM11370
      LZ(226) = N                                                       DIM11380
C                                                                       DIM11390
CFZJ042                                                       09.09.05  DIM11400
      N = N + 1                                                         DIM11410
      VAT(N) = 'VOLPART(NDR,15)                         IVOLP      227' DIM11420
      KY(N) =           NDR*15                                          DIM11430
      LZ(227) = N                                                       DIM11440
C                                                                       DIM11450
CFZJ042                                                       09.09.05  DIM11460
      N = N + 1                                                         DIM11470
      VAT(N) = 'DOSPART(NDR,15)                         IDOSP      232' DIM11480
      KY(N) =           NDR*15                                          DIM11490
      LZ(232) = N                                                       DIM11500
CFZJ042                                                       09.09.05  DIM11510
C                                                                       DIM11520
      N = N + 1                                                         DIM11530
      VAT(N) = 'NKOM(KMAZ)                              INKOM       32' DIM11540
      KY(N) =        KMAZ                                               DIM11550
      LZ(32) = N                                                        DIM11560
C                                                                       DIM11570
      N = N + 1                                                         DIM11580
      VAT(N) = 'KART(KMAZ)                              IKART       34' DIM11590
      KY(N) =        KMAZ                                               DIM11600
      LZ(34) = N                                                        DIM11610
CFZJ042                                                       09.09.05  DIM11620
C                                                                       DIM11630
      N = N + 1                                                         DIM11640
      VAT(N) = 'NHZON(KMAZ)                             INHZO       45' DIM11650
      KY(N) =         KMAZ                                              DIM11660
      LZ(45) = N                                                        DIM11670
C                                                                       DIM11680
      N = N + 1                                                         DIM11690
      VAT(N) = 'IKO(KMAZ)                               IIKO        46' DIM11700
      KY(N) =       KMAZ                                                DIM11710
      LZ(46) = N                                                        DIM11720
C                                                                       DIM11730
      N = N + 1                                                         DIM11740
      VAT(N) = 'QX(KMAZ)                                IQX         47' DIM11750
      KY(N) =      KMAZ                                                 DIM11760
      LZ(47) = N                                                        DIM11770
CFZJ042                                                       09.09.05  DIM11780
C                                                                       DIM11790
      N = N + 1                                                         DIM11800
      VAT(N) = 'HEPS(KMAZ)                              IHEPS       52' DIM11810
      KY(N) =        KMAZ                                               DIM11820
      LZ(52) = N                                                        DIM11830
C                                                                       DIM11840
      N = N + 1                                                         DIM11850
      VAT(N) = 'HKUG(KMAZ)                              IHKUG       53' DIM11860
      KY(N) =        KMAZ                                               DIM11870
      LZ(53) = N                                                        DIM11880
C                                                                       DIM11890
      N = N + 1                                                         DIM11900
      VAT(N) = 'IFHET(KMAZ)                             IIFHE       58' DIM11910
      KY(N) =         KMAZ                                              DIM11920
      LZ(58) = N                                                        DIM11930
C                                                                       DIM11940
      N = N + 1                                                         DIM11950
      VAT(N) = 'IFWKT(KMAZ)                             IIFWK       59' DIM11960
      KY(N) =         KMAZ                                              DIM11970
      LZ(59) = N                                                        DIM11980
C                                                                       DIM11990
      N = N + 1                                                         DIM12000
      VAT(N) = 'IFLT(KMAZ)                              IIFLT       60' DIM12010
      KY(N) =        KMAZ                                               DIM12020
      LZ(60) = N                                                        DIM12030
C                                                                       DIM12040
      N = N + 1                                                         DIM12050
      VAT(N) = 'WPR(KMAZ)                               IWPR        61' DIM12060
      KY(N) =       KMAZ                                                DIM12070
      LZ(61) = N                                                        DIM12080
C                                                                       DIM12090
      N = N + 1                                                         DIM12100
      VAT(N) = 'DOST(KMAZ)         ("DOS" IN THERMIX)   JDOS        62' DIM12110
      KY(N) =        KMAZ                                               DIM12120
      LZ(62) = N                                                        DIM12130
C                                                                       DIM12140
      N = N + 1                                                         DIM12150
      VAT(N) = 'LAM(KMAZ)                               ILAM        66' DIM12160
      KY(N) =       KMAZ                                                DIM12170
      LZ(66) = N                                                        DIM12180
C                                                                       DIM12190
      N = N + 1                                                         DIM12200
      VAT(N) = 'TVOR(KMAZ)                              ITVOR       67' DIM12210
      KY(N) =        KMAZ                                               DIM12220
      LZ(67) = N                                                        DIM12230
C                                                                       DIM12240
      N = N + 1                                                         DIM12250
      VAT(N) = 'ALP(KMAZ)                               IALP        68' DIM12260
      KY(N) =       KMAZ                                                DIM12270
      LZ(68) = N                                                        DIM12280
C                                                                       DIM12290
      N = N + 1                                                         DIM12300
      VAT(N) = 'REF(KMAZ)                               IRE         69' DIM12310
      KY(N) =       KMAZ                                                DIM12320
      LZ(69) = N                                                        DIM12330
C                                                                       DIM12340
      N = N + 1                                                         DIM12350
      VAT(N) = 'IFTV(KMAZ)                              IIFTV       70' DIM12360
      KY(N) =        KMAZ                                               DIM12370
      LZ(70) = N                                                        DIM12380
C                                                                       DIM12390
      N = N + 1                                                         DIM12400
      VAT(N) = 'RHO(KMAZ)                               IRHO        72' DIM12410
      KY(N) =       KMAZ                                                DIM12420
      LZ(72) = N                                                        DIM12430
C                                                                       DIM12440
      N = N + 1                                                         DIM12450
      VAT(N) = 'C(KMAZ)                                 IC          73' DIM12460
      KY(N) =     KMAZ                                                  DIM12470
      LZ(73) = N                                                        DIM12480
C                                                                       DIM12490
      N = N + 1                                                         DIM12500
      VAT(N) = 'EPS(KMAZ)                               IEPS        74' DIM12510
      KY(N) =       KMAZ                                                DIM12520
      LZ(74) = N                                                        DIM12530
C                                                                       DIM12540
      N = N + 1                                                         DIM12550
      VAT(N) = 'IDIR(KMAZ)                              IIDIR       75' DIM12560
      KY(N) =        KMAZ                                               DIM12570
      LZ(75) = N                                                        DIM12580
C                                                                       DIM12590
      N = N + 1                                                         DIM12600
      VAT(N) = 'IFANIS(KMAZ)                            IIFAN       76' DIM12610
      KY(N) =          KMAZ                                             DIM12620
      LZ(76) = N                                                        DIM12630
C                                                                       DIM12640
      N = N + 1                                                         DIM12650
      VAT(N) = 'NTVAR(KMAZ)                             INTVA       79' DIM12660
      KY(N) =         KMAZ                                              DIM12670
      LZ(79) = N                                                        DIM12680
C                                                                       DIM12690
      N = N + 1                                                         DIM12700
      VAT(N) = 'IKOM(KMAZ)                              IIKOM       80' DIM12710
      KY(N) =        KMAZ                                               DIM12720
      LZ(80) = N                                                        DIM12730
C                                                                       DIM12740
      N = N + 1                                                         DIM12750
      VAT(N) = 'KSTR(KMAZ)                              IKSTR      119' DIM12760
      KY(N) =        KMAZ                                               DIM12770
      LZ(119) = N                                                       DIM12780
C                                                                       DIM12790
      N = N + 1                                                         DIM12800
      VAT(N) = 'VK(KMAZ)                                IVK        124' DIM12810
      KY(N) =      KMAZ                                                 DIM12820
      LZ(124) = N                                                       DIM12830
C                                                                       DIM12840
      N = N + 1                                                         DIM12850
      VAT(N) = 'TKT(KMAZ)                               ITKT       186' DIM12860
      KY(N) =       KMAZ                                                DIM12870
      LZ(186) = N                                                       DIM12880
C                                                                       DIM12890
      N = N + 1                                                         DIM12900
      VAT(N) = 'JUG(KMAZ)                               IJUG       187' DIM12910
      KY(N) =       KMAZ                                                DIM12920
      LZ(187) = N                                                       DIM12930
C                                                                       DIM12940
      N = N + 1                                                         DIM12950
      VAT(N) = 'QQUEL1(KMAZ)                            IQQUE      188' DIM12960
      KY(N) =          KMAZ                                             DIM12970
      LZ(188) = N                                                       DIM12980
C                                                                       DIM12990
      N = N + 1                                                         DIM13000
      VAT(N) = 'BUINS(KMAZ)                             IBUIN      189' DIM13010
      KY(N) =         KMAZ                                              DIM13020
      LZ(189) = N                                                       DIM13030
C                                                                       DIM13040
      N = N + 1                                                         DIM13050
      VAT(N) = 'QSPEI1(KMAZ)                            IQSP1      190' DIM13060
      KY(N) =          KMAZ                                             DIM13070
      LZ(190) = N                                                       DIM13080
C                                                                       DIM13090
      N = N + 1                                                         DIM13100
      VAT(N) = 'QKONS1(KMAZ)                            IQKO1      192' DIM13110
      KY(N) =          KMAZ                                             DIM13120
      LZ(192) = N                                                       DIM13130
C                                                                       DIM13140
      N = N + 1                                                         DIM13150
      VAT(N) = 'TKOMI(KMAZ)                             ITKOI      195' DIM13160
      KY(N) =         KMAZ                                              DIM13170
      LZ(195) = N                                                       DIM13180
C                                                                       DIM13190
      N = N + 1                                                         DIM13200
      VAT(N) = 'TKOMA(KMAZ)                             ITKOA      196' DIM13210
      KY(N) =         KMAZ                                              DIM13220
      LZ(196) = N                                                       DIM13230
C                                                                       DIM13240
      N = N + 1                                                         DIM13250
      VAT(N) = 'VKOM(KMAZ)                              IVKOM      197' DIM13260
      KY(N) =        KMAZ                                               DIM13270
      LZ(197) = N                                                       DIM13280
C                                                                       DIM13290
      N = N + 1                                                         DIM13300
      VAT(N) = 'ITK(KMAZ)                               IITK       221' DIM13310
      KY(N) =       KMAZ                                                DIM13320
      LZ(221) = N                                                       DIM13330
C                                                                       DIM13340
      N = N + 1                                                         DIM13350
      VAT(N) = 'DI(KMAZ,5)                              IDI         54' DIM13360
      KY(N) =      KMAZ*5                                               DIM13370
      LZ(54) = N                                                        DIM13380
C                                                                       DIM13390
      N = N + 1                                                         DIM13400
      VAT(N) = 'NHMAT1(KMAZ,5)                          INHM1       55' DIM13410
      KY(N) =          KMAZ*5                                           DIM13420
      LZ(55) = N                                                        DIM13430
C                                                                       DIM13440
      N = N + 1                                                         DIM13450
      VAT(N) = 'NHMAT2(KMAZ,5)                          INHM2       56' DIM13460
      KY(N) =          KMAZ*5                                           DIM13470
      LZ(56) = N                                                        DIM13480
C                                                                       DIM13490
      N = N + 1                                                         DIM13500
      VAT(N) = 'XFWQZ(KMAZ,5)                           IXFWQ       57' DIM13510
      KY(N) =         KMAZ*5                                            DIM13520
      LZ(57) = N                                                        DIM13530
C                                                                       DIM13540
      N = N + 1                                                         DIM13550
      VAT(N) = 'VOLK(KMAZ,5)                            IVOLK      120' DIM13560
      KY(N) =        KMAZ*5                                             DIM13570
      LZ(120) = N                                                       DIM13580
C                                                                       DIM13590
      N = N + 1                                                         DIM13600
      VAT(N) = 'VOLS(KMAZ,5)                            IVOLS      121' DIM13610
      KY(N) =        KMAZ*5                                             DIM13620
      LZ(121) = N                                                       DIM13630
C                                                                       DIM13640
      N = N + 1                                                         DIM13650
      VAT(N) = 'IFFUEL(KMAZ,5)                          IIFFU      122' DIM13660
      KY(N) =          KMAZ*5                                           DIM13670
      LZ(122) = N                                                       DIM13680
C                                                                       DIM13690
      N = N + 1                                                         DIM13700
      VAT(N) = 'VOLZ(KMAZ,5)                            IVOLZ      123' DIM13710
      KY(N) =        KMAZ*5                                             DIM13720
      LZ(123) = N                                                       DIM13730
C                                                                       DIM13740
      N = N + 1                                                         DIM13750
      VAT(N) = 'XFAK(KMAZ,5)                            IXFAK      125' DIM13760
      KY(N) =        KMAZ*5                                             DIM13770
      LZ(125) = N                                                       DIM13780
C                                                                       DIM13790
      N = N + 1                                                         DIM13800
      VAT(N) = 'WWK(KMAZ,5)                             IWWK       129' DIM13810
      KY(N) =       KMAZ*5                                              DIM13820
      LZ(129) = N                                                       DIM13830
C                                                                       DIM13840
      N = N + 1                                                         DIM13850
      VAT(N) = 'QSPEIZ(KMAZ,5)                          IQSPE      183' DIM13860
      KY(N) =          KMAZ*5                                           DIM13870
      LZ(183) = N                                                       DIM13880
C                                                                       DIM13890
      N = N + 1                                                         DIM13900
      VAT(N) = 'QKONVZ(KMAZ,5)                          IQKON      184' DIM13910
      KY(N) =          KMAZ*5                                           DIM13920
      LZ(184) = N                                                       DIM13930
C                                                                       DIM13940
      N = N + 1                                                         DIM13950
      VAT(N) = 'QNUKLZ(KMAZ,5)                          IQNUK      185' DIM13960
      KY(N) =          KMAZ*5                                           DIM13970
      LZ(185) = N                                                       DIM13980
CFZJ042                                                       09.09.05  DIM13990
C                                                                       DIM14000
      N = N + 1                                                         DIM14010
      VAT(N) = 'ZEIV(10,KMAZ)                           IZEIV       77' DIM14020
      KY(N) =        10*KMAZ                                            DIM14030
      LZ(77) = N                                                        DIM14040
C                                                                       DIM14050
      N = N + 1                                                         DIM14060
      VAT(N) = 'TKV(10,KMAZ)                            ITKV        78' DIM14070
      KY(N) =       10*KMAZ                                             DIM14080
      LZ(78) = N                                                        DIM14090
C                                                                       DIM14100
      N = N + 1                                                         DIM14110
      VAT(N) = 'RADP(IMAZ)                              IRADP       29' DIM14120
      KY(N) =        IMAZ                                               DIM14130
      LZ(29) = N                                                        DIM14140
C                                                                       DIM14150
      N = N + 1                                                         DIM14160
      VAT(N) = 'DR(IMAZ)                                IDR         64' DIM14170
      KY(N) =      IMAZ                                                 DIM14180
      LZ(64) = N                                                        DIM14190
C                                                                       DIM14200
      N = N + 1                                                         DIM14210
      VAT(N) = 'MFR(IMAZ)                               IMFR        86' DIM14220
      KY(N) =       IMAZ                                                DIM14230
      LZ(86) = N                                                        DIM14240
C                                                                       DIM14250
      N = N + 1                                                         DIM14260
      VAT(N) = 'RE(IMAZ)                                JRE         95' DIM14270
      KY(N) =      IMAZ                                                 DIM14280
      LZ(95) = N                                                        DIM14290
C                                                                       DIM14300
      N = N + 1                                                         DIM14310
      VAT(N) = 'RAD(IMAZ+1)                             IRAD        30' DIM14320
      KY(N) =       IMAZ+1                                              DIM14330
      LZ(30) = N                                                        DIM14340
C                                                                       DIM14350
      N = N + 1                                                         DIM14360
      VAT(N) = 'NSL(IMAZ,19)                            INSL       117' DIM14370
      KY(N) =       IMAZ*19                                             DIM14380
      LZ(117) = N                                                       DIM14390
C                                                                       DIM14400
      N = N + 1                                                         DIM14410
      VAT(N) = 'NSR(IMAZ,19)                            INSR       118' DIM14420
      KY(N) =       IMAZ*19                                             DIM14430
      LZ(118) = N                                                       DIM14440
C                                                                       DIM14450
      N = N + 1                                                         DIM14460
      VAT(N) = 'PHIP(NMAZ)                              IPHIP       63' DIM14470
      KY(N) =        NMAZ                                               DIM14480
      LZ(63) = N                                                        DIM14490
C                                                                       DIM14500
      N = N + 1                                                         DIM14510
      VAT(N) = 'DPH(NMAZ)                               IDPH        65' DIM14520
      KY(N) =       NMAZ                                                DIM14530
      LZ(65) = N                                                        DIM14540
C                                                                       DIM14550
      N = N + 1                                                         DIM14560
      VAT(N) = 'MFZ(NMAZ)                               IMFZ        87' DIM14570
      KY(N) =       NMAZ                                                DIM14580
      LZ(87) = N                                                        DIM14590
C                                                                       DIM14600
      N = N + 1                                                         DIM14610
      VAT(N) = 'PHE(NMAZ)                               IPHE        96' DIM14620
      KY(N) =       NMAZ                                                DIM14630
      LZ(96) = N                                                        DIM14640
C                                                                       DIM14650
      N = N + 1                                                         DIM14660
      VAT(N) = 'WG(NMAZ)                                IWG         97' DIM14670
      KY(N) =      NMAZ                                                 DIM14680
      LZ(97) = N                                                        DIM14690
C                                                                       DIM14700
      N = N + 1                                                         DIM14710
      VAT(N) = 'WS(NMAZ)                                IWS         98' DIM14720
      KY(N) =      NMAZ                                                 DIM14730
      LZ(98) = N                                                        DIM14740
C                                                                       DIM14750
      N = N + 1                                                         DIM14760
      VAT(N) = 'XE(NMAZ)                                IXE         99' DIM14770
      KY(N) =      NMAZ                                                 DIM14780
      LZ(99) = N                                                        DIM14790
C                                                                       DIM14800
      N = N + 1                                                         DIM14810
      VAT(N) = 'ZDOS(NMAZ)                              IZDOS      111' DIM14820
      KY(N) =        NMAZ                                               DIM14830
      LZ(111) = N                                                       DIM14840
C                                                                       DIM14850
      N = N + 1                                                         DIM14860
      VAT(N) = 'ZD(NMAZ)                                IZD        112' DIM14870
      KY(N) =      NMAZ                                                 DIM14880
      LZ(112) = N                                                       DIM14890
C                                                                       DIM14900
      N = N + 1                                                         DIM14910
      VAT(N) = 'ZH(NMAZ)                                IZH        113' DIM14920
      KY(N) =      NMAZ                                                 DIM14930
      LZ(113) = N                                                       DIM14940
C                                                                       DIM14950
      N = N + 1                                                         DIM14960
      VAT(N) = 'FAKZ1(NMAZ)                             IFAK1      193' DIM14970
      KY(N) =         NMAZ                                              DIM14980
      LZ(193) = N                                                       DIM14990
C                                                                       DIM15000
      N = N + 1                                                         DIM15010
      VAT(N) = 'FAKZ2(NMAZ)                             IFAK2      194' DIM15020
      KY(N) =         NMAZ                                              DIM15030
      LZ(194) = N                                                       DIM15040
C                                                                       DIM15050
      N = N + 1                                                         DIM15060
      VAT(N) = 'PHI(NMAZ+1)                             IPHI        31' DIM15070
      KY(N) =       NMAZ+1                                              DIM15080
      LZ(31) = N                                                        DIM15090
C                                                                       DIM15100
      N = N + 1                                                         DIM15110
      VAT(N) = 'VQ(NMAZ+1)                              IVQ        101' DIM15120
      KY(N) =      NMAZ+1                                               DIM15130
      LZ(101) = N                                                       DIM15140
C                                                                       DIM15150
      N = N + 1                                                         DIM15160
      VAT(N) = 'ISO(NMAZ,19)                            IISO       115' DIM15170
      KY(N) =       NMAZ*19                                             DIM15180
      LZ(115) = N                                                       DIM15190
C                                                                       DIM15200
      N = N + 1                                                         DIM15210
      VAT(N) = 'ISU(NMAZ,19)                            IISU       116' DIM15220
      KY(N) =       NMAZ*19                                             DIM15230
      LZ(116) = N                                                       DIM15240
C                                                                       DIM15250
      N = N + 1                                                         DIM15260
      VAT(N) = 'KOM(IMAZ,NMAZ)                          IKOMP       33' DIM15270
      KY(N) =       IMAZ*NMAZ                                           DIM15280
      LZ(33) = N                                                        DIM15290
C                                                                       DIM15300
      N = N + 1                                                         DIM15310
      VAT(N) = 'T(IMAZ,NMAZ)                            IT          35' DIM15320
      KY(N) =     IMAZ*NMAZ                                             DIM15330
      LZ(35) = N                                                        DIM15340
C                                                                       DIM15350
      N = N + 1                                                         DIM15360
      VAT(N) = 'WI(IMAZ,NMAZ)                           IWI         36' DIM15370
      KY(N) =      IMAZ*NMAZ                                            DIM15380
      LZ(36) = N                                                        DIM15390
C                                                                       DIM15400
      N = N + 1                                                         DIM15410
      VAT(N) = 'WT(IMAZ,NMAZ)                           IWT         37' DIM15420
      KY(N) =      IMAZ*NMAZ                                            DIM15430
      LZ(37) = N                                                        DIM15440
C                                                                       DIM15450
      N = N + 1                                                         DIM15460
      VAT(N) = 'TQ(IMAZ,NMAZ)                           ITQ         38' DIM15470
      KY(N) =      IMAZ*NMAZ                                            DIM15480
      LZ(38) = N                                                        DIM15490
C                                                                       DIM15500
      N = N + 1                                                         DIM15510
      VAT(N) = 'IFBER(IMAZ,NMAZ)                        IIFBE       39' DIM15520
      KY(N) =         IMAZ*NMAZ                                         DIM15530
      LZ(39) = N                                                        DIM15540
C                                                                       DIM15550
      N = N + 1                                                         DIM15560
      VAT(N) = 'WQ(IMAZ,NMAZ)                           IWQ         89' DIM15570
      KY(N) =      IMAZ*NMAZ                                            DIM15580
      LZ(89) = N                                                        DIM15590
C                                                                       DIM15600
      N = N + 1                                                         DIM15610
      VAT(N) = 'WQR(IMAZ,NMAZ)                          IWQR        90' DIM15620
      KY(N) =       IMAZ*NMAZ                                           DIM15630
      LZ(90) = N                                                        DIM15640
C                                                                       DIM15650
      N = N + 1                                                         DIM15660
      VAT(N) = 'E(IMAZ,NMAZ)                            IE          91' DIM15670
      KY(N) =     IMAZ*NMAZ                                             DIM15680
      LZ(91) = N                                                        DIM15690
C                                                                       DIM15700
      N = N + 1                                                         DIM15710
      VAT(N) = 'BU1(IMAZ,NMAZ)                          IBU1        92' DIM15720
      KY(N) =       IMAZ*NMAZ                                           DIM15730
      LZ(92) = N                                                        DIM15740
CFZJ042                                                       09.09.05  DIM15750
C                                                                       DIM15760
      N = N + 1                                                         DIM15770
      VAT(N) = 'DOSI(IMAZ,NMAZ)                         IDOSI      100' DIM15780
      KY(N) =        IMAZ*NMAZ                                          DIM15790
      LZ(100) = N                                                       DIM15800
C                                                                       DIM15810
      N = N + 1                                                         DIM15820
      VAT(N) = 'TFLU(IMAZ,NMAZ)                         ITFLU      102' DIM15830
      KY(N) =        IMAZ*NMAZ                                          DIM15840
      LZ(102) = N                                                       DIM15850
C                                                                       DIM15860
      N = N + 1                                                         DIM15870
      VAT(N) = 'DU(IMAZ,NMAZ)                           IDU        103' DIM15880
      KY(N) =      IMAZ*NMAZ                                            DIM15890
      LZ(103) = N                                                       DIM15900
C                                                                       DIM15910
      N = N + 1                                                         DIM15920
      VAT(N) = 'TGRAT(IMAZ,NMAZ)  ("TGRA" IN THERMIX)   JTGRA      104' DIM15930
      KY(N) =         IMAZ*NMAZ                                         DIM15940
      LZ(104) = N                                                       DIM15950
C                                                                       DIM15960
      N = N + 1                                                         DIM15970
      VAT(N) = 'TFUL(IMAZ,NMAZ)                         ITFUL      105' DIM15980
      KY(N) =        IMAZ*NMAZ                                          DIM15990
      LZ(105) = N                                                       DIM16000
CFZJ042                                                       09.09.05  DIM16010
C                                                                       DIM16020
      N = N + 1                                                         DIM16030
      VAT(N) = 'AU(IMAZ,NMAZ)                           IAU        114' DIM16040
      KY(N) =      IMAZ*NMAZ                                            DIM16050
      LZ(114) = N                                                       DIM16060
C                                                                       DIM16070
      N = N + 1                                                         DIM16080
      VAT(N) = 'BU(IMAZ,NMAZ)                           IBU        126' DIM16090
      KY(N) =      IMAZ*NMAZ                                            DIM16100
      LZ(126) = N                                                       DIM16110
C                                                                       DIM16120
      N = N + 1                                                         DIM16130
      VAT(N) = 'BR(IMAZ,NMAZ)                           IBR        127' DIM16140
      KY(N) =      IMAZ*NMAZ                                            DIM16150
      LZ(127) = N                                                       DIM16160
C                                                                       DIM16170
      N = N + 1                                                         DIM16180
      VAT(N) = 'WL(IMAZ,NMAZ)                           IWL        130' DIM16190
      KY(N) =      IMAZ*NMAZ                                            DIM16200
      LZ(130) = N                                                       DIM16210
C                                                                       DIM16220
      N = N + 1                                                         DIM16230
      VAT(N) = 'AR(IMAZ,NMAZ)                           IAR        131' DIM16240
      KY(N) =      IMAZ*NMAZ                                            DIM16250
      LZ(131) = N                                                       DIM16260
C                                                                       DIM16270
      N = N + 1                                                         DIM16280
      VAT(N) = 'CU(IMAZ,NMAZ)                           ICU        182' DIM16290
      KY(N) =      IMAZ*NMAZ                                            DIM16300
      LZ(182) = N                                                       DIM16310
CFZJ042                                                       09.09.05  DIM16320
C                                                                       DIM16330
      N = N + 1                                                         DIM16340
      VAT(N) = 'STRAHL(IMAZ,NMAZ)                       ISTRA      220' DIM16350
      KY(N) =          IMAZ*NMAZ                                        DIM16360
      LZ(220) = N                                                       DIM16370
C                                                                       DIM16380
      N = N + 1                                                         DIM16390
      VAT(N) = 'PDTHERM(IMAZ,NMAZ)                      IPDTH       88' DIM16400
      KY(N) =           IMAZ*NMAZ                                       DIM16410
      LZ(88) = N                                                        DIM16420
C                                                                       DIM16430
CFZJ023                                                       29.01.04  DIM16440
      N = N + 1                                                         DIM16450
      VAT(N) = 'FFTHX(IMAZ,NMAZ)                        IFFTH      224' DIM16460
      KY(N) =         IMAZ*NMAZ                                         DIM16470
      LZ(224) = N                                                       DIM16480
C                                                                       DIM16490
CFZJ024                                                       02.02.04  DIM16500
      N = N + 1                                                         DIM16510
      VAT(N) = 'TCOND(IMAZ,NMAZ)                        ITCON      225' DIM16520
      KY(N) =         IMAZ*NMAZ                                         DIM16530
      LZ(225) = N                                                       DIM16540
C                                                                       DIM16550
      N = N + 1                                                         DIM16560
      VAT(N) = 'KOC(IMAZ,NMAZ)                          IKOC       222' DIM16570
      KY(N) =       IMAZ*NMAZ                                           DIM16580
      LZ(222) = N                                                       DIM16590
C                                                                       DIM16600
      N = N + 1                                                         DIM16610
      VAT(N) = 'KG(IMAZ,NMAZ)                           IKG        223' DIM16620
      KY(N) =      IMAZ*NMAZ                                            DIM16630
      LZ(223) = N                                                       DIM16640
C                                                                       DIM16650
CFZJ042                                                       09.09.05  DIM16660
      N = N + 1                                                         DIM16670
      VAT(N) = 'POWPT(IMAZ,NMAZ,15)                     IPOWT      228' DIM16680
      KY(N) =         IMAZ*NMAZ*15                                      DIM16690
      LZ(228) = N                                                       DIM16700
C                                                                       DIM16710
CFZJ042                                                       09.09.05  DIM16720
      N = N + 1                                                         DIM16730
      VAT(N) = 'VOLPT(IMAZ,NMAZ,15)                     IVOLT      229' DIM16740
      KY(N) =         IMAZ*NMAZ*15                                      DIM16750
      LZ(229) = N                                                       DIM16760
C                                                                       DIM16770
CFZJ042                                                       09.09.05  DIM16780
      N = N + 1                                                         DIM16790
      VAT(N) = 'DOSPT(IMAZ,NMAZ,15)                     IDOST      233' DIM16800
      KY(N) =         IMAZ*NMAZ*15                                      DIM16810
      LZ(233) = N                                                       DIM16820
C                                                                       DIM16830
      N = N + 1                                                         DIM16840
      VAT(N) = 'RT(ICO+1)                               IRT        108' DIM16850
      KY(N) =      ICO+1                                                DIM16860
      LZ(108) = N                                                       DIM16870
C                                                                       DIM16880
      N = N + 1                                                         DIM16890
      VAT(N) = 'RVT(ICO*2)                              IRVT       110' DIM16900
      KY(N) =       ICO*2                                               DIM16910
      LZ(110) = N                                                       DIM16920
C                                                                       DIM16930
      N = N + 1                                                         DIM16940
      VAT(N) = 'ZT(NCO+1)                               IZT        109' DIM16950
      KY(N) =      NCO+1                                                DIM16960
      LZ(109) = N                                                       DIM16970
C                                                                       DIM16980
      N = N + 1                                                         DIM16990
      VAT(N) = 'IFBH(ICO,NCO)                           IIFBH       43' DIM17000
      KY(N) =        ICO*NCO                                            DIM17010
      LZ(43) = N                                                        DIM17020
C                                                                       DIM17030
      N = N + 1                                                         DIM17040
      VAT(N) = 'KKB(ICO,NCO)                            IKKB        44' DIM17050
      KY(N) =       ICO*NCO                                             DIM17060
      LZ(44) = N                                                        DIM17070
C                                                                       DIM17080
      N = N + 1                                                         DIM17090
      VAT(N) = 'ZKUG(ICO,NCO)                           IZKUG      176' DIM17100
      KY(N) =        ICO*NCO                                            DIM17110
      LZ(176) = N                                                       DIM17120
C                                                                       DIM17130
      N = N + 1                                                         DIM17140
      VAT(N) = 'WQN(ICO,NCO)                            IWQN       179' DIM17150
      KY(N) =       ICO*NCO                                             DIM17160
      LZ(179) = N                                                       DIM17170
C                                                                       DIM17180
      N = N + 1                                                         DIM17190
      VAT(N) = 'WQK(ICO,NCO)                            IWQK       180' DIM17200
      KY(N) =       ICO*NCO                                             DIM17210
      LZ(180) = N                                                       DIM17220
CFZJ042                                                       09.09.05  DIM17230
C                                                                       DIM17240
      N = N + 1                                                         DIM17250
      VAT(N) = 'THALT(ICO,NCO,5)                        ITHAL      178' DIM17260
      KY(N) =         ICO*NCO*5                                         DIM17270
      LZ(178) = N                                                       DIM17280
CFZJ042                                                       09.09.05  DIM17290
C                                                                       DIM17300
CFZJ042                                                       09.09.05  DIM17310
      N = N + 1                                                         DIM17320
      VAT(N) = 'WKAPH(ICO,NCO,5)                        IWKAP      177' DIM17330
      KY(N) =         ICO*NCO*5                                         DIM17340
      LZ(177) = N                                                       DIM17350
C                                                                       DIM17360
CFZJ042                                                       09.09.05  DIM17370
      N = N + 1                                                         DIM17380
      VAT(N) = 'THETNEW(ICO,NCO,5,15)                   ITHEN      234' DIM17390
      KY(N) =           ICO*NCO*5*15                                    DIM17400
      LZ(234) = N                                                       DIM17410
C                                                                       DIM17420
      N = N + 1                                                         DIM17430
      VAT(N) = 'DROHR(KOMAX)                            IDROH      132' DIM17440
      KY(N) =         KOMAX                                             DIM17450
      LZ(132) = N                                                       DIM17460
C                                                                       DIM17470
      N = N + 1                                                         DIM17480
      VAT(N) = 'QTV(KOMAX)                              IQTV       133' DIM17490
      KY(N) =       KOMAX                                               DIM17500
      LZ(133) = N                                                       DIM17510
C                                                                       DIM17520
      N = N + 1                                                         DIM17530
      VAT(N) = 'LTV(KOMAX)                              ILTV       134' DIM17540
      KY(N) =       KOMAX                                               DIM17550
      LZ(134) = N                                                       DIM17560
C                                                                       DIM17570
      N = N + 1                                                         DIM17580
      VAT(N) = 'XGEO(KOMAX)                             IXGEO      135' DIM17590
      KY(N) =        KOMAX                                              DIM17600
      LZ(135) = N                                                       DIM17610
C                                                                       DIM17620
      N = N + 1                                                         DIM17630
      VAT(N) = 'NRHLR(KOMAX)                            INRHL      136' DIM17640
      KY(N) =         KOMAX                                             DIM17650
      LZ(136) = N                                                       DIM17660
C                                                                       DIM17670
      N = N + 1                                                         DIM17680
      VAT(N) = 'PVOR(KOMAX)                             IPVOR      142' DIM17690
      KY(N) =        KOMAX                                              DIM17700
      LZ(142) = N                                                       DIM17710
C                                                                       DIM17720
      N = N + 1                                                         DIM17730
      VAT(N) = 'IFBR(KOMAX)                             IIFBR      143' DIM17740
      KY(N) =        KOMAX                                              DIM17750
      LZ(143) = N                                                       DIM17760
C                                                                       DIM17770
      N = N + 1                                                         DIM17780
      VAT(N) = 'ALPHA(KOMAX)                            IALPH      144' DIM17790
      KY(N) =         KOMAX                                             DIM17800
      LZ(144) = N                                                       DIM17810
C                                                                       DIM17820
      N = N + 1                                                         DIM17830
      VAT(N) = 'IFBQ(KOMAX)                             IIFBQ      145' DIM17840
      KY(N) =        KOMAX                                              DIM17850
      LZ(145) = N                                                       DIM17860
C                                                                       DIM17870
      N = N + 1                                                         DIM17880
      VAT(N) = 'ALFISO(KOMAX)                           IALFI      146' DIM17890
      KY(N) =          KOMAX                                            DIM17900
      LZ(146) = N                                                       DIM17910
C                                                                       DIM17920
      N = N + 1                                                         DIM17930
      VAT(N) = 'EPSIL(KOMAX)                            IEPSI      157' DIM17940
      KY(N) =         KOMAX                                             DIM17950
      LZ(157) = N                                                       DIM17960
C                                                                       DIM17970
      N = N + 1                                                         DIM17980
      VAT(N) = 'XKON(KOMAX)                             IXKON      158' DIM17990
      KY(N) =        KOMAX                                              DIM18000
      LZ(158) = N                                                       DIM18010
C                                                                       DIM18020
      N = N + 1                                                         DIM18030
      VAT(N) = 'DHYD(KOMAX)                             IDHYD      159' DIM18040
      KY(N) =        KOMAX                                              DIM18050
      LZ(159) = N                                                       DIM18060
C                                                                       DIM18070
      N = N + 1                                                         DIM18080
      VAT(N) = 'STZUK(KOMAX)                            ISTZU      160' DIM18090
      KY(N) =         KOMAX                                             DIM18100
      LZ(160) = N                                                       DIM18110
C                                                                       DIM18120
      N = N + 1                                                         DIM18130
      VAT(N) = 'TFLVOR(KOMAX)                           ITFLV      161' DIM18140
      KY(N) =          KOMAX                                            DIM18150
      LZ(161) = N                                                       DIM18160
C                                                                       DIM18170
      N = N + 1                                                         DIM18180
      VAT(N) = 'IFZST(KOMAX)                            IIFZS      162' DIM18190
      KY(N) =         KOMAX                                             DIM18200
      LZ(162) = N                                                       DIM18210
C                                                                       DIM18220
      N = N + 1                                                         DIM18230
      VAT(N) = 'IFZTF(KOMAX)                            IIFZT      163' DIM18240
      KY(N) =         KOMAX                                             DIM18250
      LZ(163) = N                                                       DIM18260
C                                                                       DIM18270
      N = N + 1                                                         DIM18280
      VAT(N) = 'VOL(KOMAX)                              IVOL       164' DIM18290
      KY(N) =       KOMAX                                               DIM18300
      LZ(164) = N                                                       DIM18310
C                                                                       DIM18320
      N = N + 1                                                         DIM18330
      VAT(N) = 'XZSUM(KOMAX)                            IXZSU      171' DIM18340
      KY(N) =         KOMAX                                             DIM18350
      LZ(171) = N                                                       DIM18360
C                                                                       DIM18370
      N = N + 1                                                         DIM18380
      VAT(N) = 'XNSUM1(KOMAX)                           IXNSU      172' DIM18390
      KY(N) =          KOMAX                                            DIM18400
      LZ(172) = N                                                       DIM18410
C                                                                       DIM18420
      N = N + 1                                                         DIM18430
      VAT(N) = 'XKSUM(KOMAX)                            IXKSU       25' DIM18440
      KY(N) =         KOMAX                                             DIM18450
      LZ(25) = N                                                        DIM18460
C                                                                       DIM18470
      N = N + 1                                                         DIM18480
      VAT(N) = 'XKINT(KOMAX)                            IXKIN       26' DIM18490
      KY(N) =         KOMAX                                             DIM18500
      LZ(26) = N                                                        DIM18510
C                                                                       DIM18520
      N = N + 1                                                         DIM18530
      VAT(N) = 'XKSUMA(KOMAX)                           IXKSA       27' DIM18540
      KY(N) =          KOMAX                                            DIM18550
      LZ(27) = N                                                        DIM18560
C                                                                       DIM18570
      N = N + 1                                                         DIM18580
      VAT(N) = 'TFMIT(KOMAX)                            ITFMI       83' DIM18590
      KY(N) =         KOMAX                                             DIM18600
      LZ(83) = N                                                        DIM18610
C                                                                       DIM18620
      N = N + 1                                                         DIM18630
      VAT(N) = 'XH(KOMAX)                               IXH         85' DIM18640
      KY(N) =      KOMAX                                                DIM18650
      LZ(85) = N                                                        DIM18660
C                                                                       DIM18670
      N = N + 1                                                         DIM18680
      VAT(N) = 'R(KONI)                                 IR          81' DIM18690
      KY(N) =     KONI                                                  DIM18700
      LZ(81) = N                                                        DIM18710
C                                                                       DIM18720
      N = N + 1                                                         DIM18730
      VAT(N) = 'FZQ(KONI)                               IFZQ       149' DIM18740
      KY(N) =       KONI                                                DIM18750
      LZ(149) = N                                                       DIM18760
C                                                                       DIM18770
      N = N + 1                                                         DIM18780
      VAT(N) = 'DRK(KONI)            ("DR" IN KONVEK)   IDRK       151' DIM18790
      KY(N) =       KONI                                                DIM18800
      LZ(151) = N                                                       DIM18810
C                                                                       DIM18820
      N = N + 1                                                         DIM18830
      VAT(N) = 'FZQ1(KONI)                              IFZQ1      153' DIM18840
      KY(N) =        KONI                                               DIM18850
      LZ(153) = N                                                       DIM18860
C                                                                       DIM18870
      N = N + 1                                                         DIM18880
      VAT(N) = 'RP(KONI)                                IRP        156' DIM18890
      KY(N) =      KONI                                                 DIM18900
      LZ(156) = N                                                       DIM18910
C                                                                       DIM18920
      N = N + 1                                                         DIM18930
      VAT(N) = 'XKP(KONI*2)                             IXKP       218' DIM18940
      KY(N) =       KONI*2                                              DIM18950
      LZ(218) = N                                                       DIM18960
C                                                                       DIM18970
      N = N + 1                                                         DIM18980
      VAT(N) = 'ROGP(KONI*2)                            IROGP      219' DIM18990
      KY(N) =        KONI*2                                             DIM19000
      LZ(219) = N                                                       DIM19010
C                                                                       DIM19020
      N = N + 1                                                         DIM19030
      VAT(N) = 'IOKUL(KONI,4)                           IIOKU      168' DIM19040
      KY(N) =         KONI*4                                            DIM19050
      LZ(168) = N                                                       DIM19060
C                                                                       DIM19070
      N = N + 1                                                         DIM19080
      VAT(N) = 'IUKUL(KONI,4)                           IIUKU      169' DIM19090
      KY(N) =         KONI*4                                            DIM19100
      LZ(169) = N                                                       DIM19110
C                                                                       DIM19120
      N = N + 1                                                         DIM19130
      VAT(N) = 'XKUL(KONI,4)                            IXKUL      170' DIM19140
      KY(N) =        KONI*4                                             DIM19150
      LZ(170) = N                                                       DIM19160
C                                                                       DIM19170
      N = N + 1                                                         DIM19180
      VAT(N) = 'IOVER(KONI,30)                          IIOVE      165' DIM19190
      KY(N) =         KONI*30                                           DIM19200
      LZ(165) = N                                                       DIM19210
C                                                                       DIM19220
      N = N + 1                                                         DIM19230
      VAT(N) = 'IUVER(KONI,30)                          IIUVE      166' DIM19240
      KY(N) =         KONI*30                                           DIM19250
      LZ(166) = N                                                       DIM19260
C                                                                       DIM19270
      N = N + 1                                                         DIM19280
      VAT(N) = 'XVER(KONI,30)                           IXVER      167' DIM19290
      KY(N) =        KONI*30                                            DIM19300
      LZ(167) = N                                                       DIM19310
C                                                                       DIM19320
      N = N + 1                                                         DIM19330
      VAT(N) = 'SUMRG(KONI,30)                          ISUMR      206' DIM19340
      KY(N) =         KONI*30                                           DIM19350
      LZ(206) = N                                                       DIM19360
C                                                                       DIM19370
      N = N + 1                                                         DIM19380
      VAT(N) = 'SUMXK(KONI,30)                          ISUMX      207' DIM19390
      KY(N) =         KONI*30                                           DIM19400
      LZ(207) = N                                                       DIM19410
C                                                                       DIM19420
      N = N + 1                                                         DIM19430
      VAT(N) = 'IPAR(KONI*2,10)                         IIPAR      215' DIM19440
      KY(N) =        KONI*2*10                                          DIM19450
      LZ(215) = N                                                       DIM19460
C                                                                       DIM19470
      N = N + 1                                                         DIM19480
      VAT(N) = 'JPAR(KONI*2,10)                         IJPAR      216' DIM19490
      KY(N) =        KONI*2*10                                          DIM19500
      LZ(216) = N                                                       DIM19510
C                                                                       DIM19520
      N = N + 1                                                         DIM19530
      VAT(N) = 'NPAR(KONI*2,10)                         INPAR      217' DIM19540
      KY(N) =        KONI*2*10                                          DIM19550
      LZ(217) = N                                                       DIM19560
C                                                                       DIM19570
      N = N + 1                                                         DIM19580
      VAT(N) = 'Z(KONN)                                 IZ          82' DIM19590
      KY(N) =     KONN                                                  DIM19600
      LZ(82) = N                                                        DIM19610
C                                                                       DIM19620
      N = N + 1                                                         DIM19630
      VAT(N) = 'DZ(KONN)                                IDZ         84' DIM19640
      KY(N) =      KONN                                                 DIM19650
      LZ(84) = N                                                        DIM19660
C                                                                       DIM19670
      N = N + 1                                                         DIM19680
      VAT(N) = 'ZP(KONN)                                IZPK       155' DIM19690
      KY(N) =      KONN                                                 DIM19700
      LZ(155) = N                                                       DIM19710
C                                                                       DIM19720
      N = N + 1                                                         DIM19730
      VAT(N) = 'STRBEZ(KONN)                            ISTRB      174' DIM19740
      KY(N) =          KONN                                             DIM19750
      LZ(174) = N                                                       DIM19760
C                                                                       DIM19770
      N = N + 1                                                         DIM19780
      VAT(N) = 'IFQROW(KONN)                            IFQRO      175' DIM19790
      KY(N) =          KONN                                             DIM19800
      LZ(175) = N                                                       DIM19810
C                                                                       DIM19820
      N = N + 1                                                         DIM19830
      VAT(N) = 'IFB(KONN,KONI)                          IIFB       137' DIM19840
      KY(N) =       KONN*KONI                                           DIM19850
      LZ(137) = N                                                       DIM19860
C                                                                       DIM19870
      N = N + 1                                                         DIM19880
      VAT(N) = 'XKK(KONN,KONI)                          IXKK       138' DIM19890
      KY(N) =       KONN*KONI                                           DIM19900
      LZ(138) = N                                                       DIM19910
C                                                                       DIM19920
      N = N + 1                                                         DIM19930
      VAT(N) = 'MR(KONN,KONI)                           IMR        139' DIM19940
      KY(N) =      KONN*KONI                                            DIM19950
      LZ(139) = N                                                       DIM19960
C                                                                       DIM19970
      N = N + 1                                                         DIM19980
      VAT(N) = 'P(KONN,KONI)                            IP         140' DIM19990
      KY(N) =     KONN*KONI                                             DIM20000
      LZ(140) = N                                                       DIM20010
C                                                                       DIM20020
      N = N + 1                                                         DIM20030
      VAT(N) = 'KON(KONN,KONI)      ("KOM" IN KONVEK)   IKON       141' DIM20040
      KY(N) =       KONN*KONI                                           DIM20050
      LZ(141) = N                                                       DIM20060
C                                                                       DIM20070
      N = N + 1                                                         DIM20080
      VAT(N) = 'ROGG(KONN,KONI)                         IROGG      147' DIM20090
      KY(N) =        KONN*KONI                                          DIM20100
      LZ(147) = N                                                       DIM20110
C                                                                       DIM20120
      N = N + 1                                                         DIM20130
      VAT(N) = 'TKO(KONN,KONI)        ("T" IN KONVEK)   ITKO       148' DIM20140
      KY(N) =       KONN*KONI                                           DIM20150
      LZ(148) = N                                                       DIM20160
C                                                                       DIM20170
      N = N + 1                                                         DIM20180
      VAT(N) = 'FRQ(KONN,KONI)                          IFRQ       150' DIM20190
      KY(N) =       KONN*KONI                                           DIM20200
      LZ(150) = N                                                       DIM20210
C                                                                       DIM20220
      N = N + 1                                                         DIM20230
      VAT(N) = 'RHOK(KONN,KONI)     ("RHO" IN KONVEK)   IRHOK      152' DIM20240
      KY(N) =        KONN*KONI                                          DIM20250
      LZ(152) = N                                                       DIM20260
C                                                                       DIM20270
      N = N + 1                                                         DIM20280
      VAT(N) = 'FRQ1(KONN,KONI)                         IFRQ1      154' DIM20290
      KY(N) =        KONN*KONI                                          DIM20300
      LZ(154) = N                                                       DIM20310
C                                                                       DIM20320
      N = N + 1                                                         DIM20330
      VAT(N) = 'TFL(KONN,KONI)                          ITFL       173' DIM20340
      KY(N) =       KONN*KONI                                           DIM20350
      LZ(173) = N                                                       DIM20360
C                                                                       DIM20370
      N = N + 1                                                         DIM20380
      VAT(N) = 'MZ(KONN,KONI)                           IMZ        198' DIM20390
      KY(N) =      KONN*KONI                                            DIM20400
      LZ(198) = N                                                       DIM20410
C                                                                       DIM20420
      N = N + 1                                                         DIM20430
      VAT(N) = 'STROM(KONN,KONI)                        ISTRO      199' DIM20440
      KY(N) =         KONN*KONI                                         DIM20450
      LZ(199) = N                                                       DIM20460
C                                                                       DIM20470
      N = N + 1                                                         DIM20480
      VAT(N) = 'HG(KONN,KONI)                           IHG        200' DIM20490
      KY(N) =      KONN*KONI                                            DIM20500
      LZ(200) = N                                                       DIM20510
C                                                                       DIM20520
      N = N + 1                                                         DIM20530
      VAT(N) = 'HF(KONN,KONI)                           IHF        201' DIM20540
      KY(N) =      KONN*KONI                                            DIM20550
      LZ(201) = N                                                       DIM20560
CFZJ042                                                       09.09.05  DIM20570
C                                                                       DIM20580
      N = N + 1                                                         DIM20590
      VAT(N) = 'XKR(KONN,KONI)                          IXKR       204' DIM20600
      KY(N) =       KONN*KONI                                           DIM20610
      LZ(204) = N                                                       DIM20620
C                                                                       DIM20630
      N = N + 1                                                         DIM20640
      VAT(N) = 'XKZ(KONN,KONI)                          IXKZ       205' DIM20650
      KY(N) =       KONN*KONI                                           DIM20660
      LZ(205) = N                                                       DIM20670
C                                                                       DIM20680
      N = N + 1                                                         DIM20690
      VAT(N) = 'LAMTUR(KONN,KONI)                       ILAMT      208' DIM20700
      KY(N) =          KONN*KONI                                        DIM20710
      LZ(208) = N                                                       DIM20720
C                                                                       DIM20730
      N = N + 1                                                         DIM20740
      VAT(N) = 'FELD(KONN,KONI)                         IFELD       71' DIM20750
      KY(N) =        KONN*KONI                                          DIM20760
      LZ(71) = N                                                        DIM20770
C                                                                       DIM20780
      N = N + 1                                                         DIM20790
      VAT(N) = 'ALGA(KONN,KONI,2)                       IALGA      209' DIM20800
      KY(N) =        KONN*KONI*2                                        DIM20810
      LZ(209) = N                                                       DIM20820
C                                                                       DIM20830
      KX(1) = 1                                                         DIM20840
      WRITE (NT,50)                                                     DIM20850
      DO 2 I=1,N                                                        DIM20860
        IF(I .EQ. 1) GOTO 22                                            DIM20870
        KX(I) = KX(I-1) + KY(I-1)                                       DIM20880
   22   WRITE (NT,20) I,VAT(I),KX(I),KY(I)                              DIM20890
    2 CONTINUE                                                          DIM20900
      NENDPT = KX(N) + KY(N) - 1                                        DIM20910
      WRITE (NT,60) NENDPT                                              DIM20920
      KNU = IT - NENDPT                                                 DIM20930
      IF(KNU) 9,99,99                                                   DIM20940
    9 CONTINUE                                                          DIM20950
      WRITE (6,80) IABS(KNU)                                            DIM20960
      STOP                                                              DIM20970
C                                                                       DIM20980
   99 RETURN                                                            DIM20990
      END                                                               DIM21000
      SUBROUTINE ADMAIN(IMAT)                                           ADM   10
C     READ ADAGE-LIB                                                    ADM   20
      COMMON /FLUXN/ TOCAP(120),FISS(120),DIS(120),ILITE,IACT,IFP,ITOT, ADM   30
     1 NON,INPT                                                         ADM   40
C                                                                       ADM   50
      COMMON /VSDA4/ NISO(30),IGAM,NRGAM(30),IDORG(200),THEB10,RESB10,  ADM   60
     1 RFAST                                                            ADM   70
C                                                                       ADM   80
      COMMON /OUT/ NUCL(120)                                            ADM   90
C                                                                       ADM  100
      COMMON /NUKDAT/ DDLAM(30),IIU(30),FFB1(30),FFP(30),FFP1(30),      ADM  110
     1 FFT(30),FFA(30),FFSF(30),FFNG1(30),FFN2N1(30),QMEV(30)           ADM  120
C                                                                       ADM  130
CFZJ031                                                       28.05.04  ADM  140
      COMMON /VSDATA/ NRKONZ,KFISS,IDUMMY,DELT,NB10,NDUMMY,AKONZ(30),   ADM  150
     1 MATNR(30),NR(30),FLAX(33),THERMA(30),RESA(30),FASTV(30),         ADM  160
     2 THERMF(30),RESF(30),FASF(30),FASAB(30),SIGMA(33,30),SIGMF(33,30) ADM  170
C                                                                       ADM  180
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    ADM  190
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    ADM  200
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIADM  210
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D             ADM  220
C                                                                       ADM  230
      DIMENSION IMAT(KMAT)                                              ADM  240
C                                                                       ADM  250
C                                                                       ADM  260
      L = 63                                                            ADM  270
      ILITE = 0                                                         ADM  280
      IFP = 0                                                           ADM  290
      DO 3 I=1,IACT                                                     ADM  300
        READ (L,100) NUCL(I),DDLAM(I),IIU(I),FFB1(I),FFP(I),FFP1(I),    ADM  310
     1   FFT(I),FFA(I),FFSF(I),QMEV(I)                                  ADM  320
        FFNG1(I) = 0.                                                   ADM  330
        FFN2N1(I) = 0.                                                  ADM  340
    3 CONTINUE                                                          ADM  350
      FFNG1(3) = 0.512                                                  ADM  360
      FFNG1(21) = 0.162                                                 ADM  370
      REWIND L                                                          ADM  380
      ITOT = IACT + IFP                                                 ADM  390
      NB10 = IACT + 1                                                   ADM  400
C     WRITE(6,300)                                                      ADM  410
      DO 4 I=1,IACT                                                     ADM  420
        NISO(I) = IDORG(NRGAM(I))                                       ADM  430
        NIS = IABS(NISO(I))                                             ADM  440
        IMAT(I) = NRGAM(I)                                              ADM  450
C       WRITE (6,400) I,NRGAM(I),NIS                                    ADM  460
    4 CONTINUE                                                          ADM  470
      RETURN                                                            ADM  480
C                                                                       ADM  490
  100 FORMAT (I7,E9.2,I2,5F5.3,E9.2,F6.3)                               ADM  500
  300 FORMAT (/////,1X,'VSOP-NO',2X,'GAM-NO',3X,'ADAGE-NO'//)           ADM  510
  400 FORMAT (2I8,I11)                                                  ADM  520
      END                                                               ADM  530