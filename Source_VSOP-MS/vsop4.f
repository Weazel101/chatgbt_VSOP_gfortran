      SUBROUTINE CH4(MAFIA,IMAT,BUCKS,NDES,TEMZUT,JAD11,KDES,CONC)      CH4   10
CFZJ034                                                       31.08.04  CH4   20
C                                                                       CH4   30
C     GAM    (MIT BUCKLING-RUECKKOPPLUNG)                               CH4   40
C                                                                       CH4   50
CFZJ035                                                       14.09.04  CH4   60
CFZJ063                                                       26.07.11  CH4   70
      DIMENSION D(108),B(108),SIGU(1),EN(250),GAMN(250),GAMGAM(250),    CH4   80
     1L(250),GAMT(99),FAC3(99),FAC1(99),SIGZ(99),ZONE(9),TERM(9),AT(108)CH4   90
     2,LP(108),T(1),AR(1),FACT(1),AL1(69),AFSS(69),AMAR(69),ADUM(5100), CH4  100
     3 BST(69),CEG(33),LGBN(35),CGM(33),CTL(33),CRAM(33),CD(33),CSAR(33)CH4  110
     4 ,CX(33),CNSQ(33),APHI(69),AJ(69),DB0(528),DB1(528),DBN(528),     CH4  120
     5 DBIN(528),SIG(2,1),COEF1(1,99),BETA(1,50),GK(1,99),ZI(1,99),     CH4  130
     6 Z(2,1),SAR(9,68),ACS(4,2346),PLCK(2,69),GDJ(2,70),FTM(528),      CH4  140
     7 TAU(69),THRM(69),SECM(69),FIRM(69),ANSQ(68),LOL(4),LA(4),LD(4),  CH4  150
     8 PSFS(70),FSN(18),RB0(33),RB1(33),RBIN(33),RBN(33),TTR(33),TPB(33)CH4  160
     9 ,G(9,33,14),F(4158),IMAT(KMAT),BUCKS(N26,NXS),NDES(NXS),         CH4  170
     X TEMZUT(NXS),CONC(KMAT),JAD11(JD11),KDES(NXS)                     CH4  180
C                                                                       CH4  190
CFZJ059                                                       04.11.09  CH4  200
      CHARACTER*4 BCDW(18)                                              CH4  210
C                                                                       CH4  220
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    CH4  230
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    CH4  240
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PICH4  250
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 CH4  260
C                                                                       CH4  270
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), CH4  280
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10CH4  290
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11CH4  300
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         CH4  310
C                                                                       CH4  320
      COMMON /BLOCKG/ K,D,B,AU,F,JGM,SIG,SIGU,EN,GAMN,GAMGAM,L,GAMT,FAC3CH4  330
     1 ,FAC1,SIGZ,COEF1,BETA,GK,ZI,Z,ZONE,TERM,AT,SAR,LP,T,AR,FACT,Al1, CH4  340
     2 AFSS,AMAR,ACS,CEG,LGBN,CD,ANSQ,LOL,LA,LD,PSFS,PLCK,FSN,GDJ,RB0,  CH4  350
     3 RB1,RBIN,RBN,TTR,TPB,NOAG,NOBG,ADELL,MICR,NOI,MTP,WAG,TKN,ADUM,  CH4  360
     4 BXCX                                                             CH4  370
C                                                                       CH4  380
CFZJ035                                                       14.09.04  CH4  390
CFZJ063                                                       26.07.11  CH4  400
      COMMON /GRENZE/ NURTHM,THERGR,IDGAM,IDZUT,IDTHER,NGAM,IDESIN,JJGM,CH4  410
     1 MSTU,MGHUS,NNOBG,NZT(10,9),IZUT(10,2,10),SIRA(68),NIRPI,NID,NDA30CH4  420
C                                                                       CH4  430
CFZJ031                                                       28.05.04  CH4  440
CFZJ062                                                       04.05.11  CH4  450
      COMMON /ORIGEN/ LOB,NOR,VOR(100),DUMM(7),FSP(33)                  CH4  460
C                                                                       CH4  470
      COMMON /VARDIM/ A(8000000)                                        CH4  480
C                                                                       CH4  490
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       CH4  500
C                                                                       CH4  510
      COMMON /FLUXN/ DU(361),IACT                                       CH4  520
C                                                                       CH4  530
CFZJ035                                                       14.09.04  CH4  540
CFZJ063                                                       26.07.11  CH4  550
      COMMON /RI/ SM(10,9,2),IZUT1(10,9,10),IZUT2(10,9,10),SMC          CH4  560
C                                                                       CH4  570
      COMMON /FLEX/ DUMMY(6),ERR                                        CH4  580
C                                                                       CH4  590
CFZJ055                                                       25.09.07  CH4  600
C                                                                       CH4  610
      EQUIVALENCE(G(1,1,1),F(1)),(BST(1),F(1)),(F(70),APHI(1)),         CH4  620
     1 (F(140),AJ(1)),(FIRM(1),F(210)),(F(280),SECM(1)),(F(350),THRM(1))CH4  630
     2 ,(F(420),TAU(1)),(CGM(1),F(500)),(CTL(1),F(540)),(F(580),CRAM(1))CH4  640
     3 ,(CSAR(1),F(620)),(F(660),CX(1)),(CNSQ(1),F(700)),(FTM(1),F(800))CH4  650
     4 ,(DB0(1),F(1350)),(DB1(1),F(1900)),(DBIN(1),F(2450)),            CH4  660
     5 (DBN(1),F(3000)),(LI(3),LOUSI),(LI(4),LTOSI),(LI(5),LABSI),      CH4  670
     6 (LI(6),LFISI),(LI(7),LXNU),(LI(63),LNDES),(LI(178),LSN2N),       CH4  680
     7 (LI(180),LTRSI)                                                  CH4  690
C                                                                       CH4  700
C                                                                       CH4  710
      NT = JTPE3                                                        CH4  720
      IPRIN(7) = IPRIN(7) + 1                                           CH4  730
      NIRPI = IPRIN(7)                                                  CH4  740
      MAFIA = 6                                                         CH4  750
      IF(NIRPI .GT. NXS) RETURN                                         CH4  760
      JSATZ = NIRPI + 2                                                 CH4  770
      NXT11 = JAD11(JSATZ)                                              CH4  780
      READ (NDA11,REC=NXT11) (CONC(M),M=1,KMAT)                         CH4  790
      NXT11 = NXT11 + 1                                                 CH4  800
      JSATZ = 2 + KDES(NIRPI)                                           CH4  810
      NXT10 = JAD10(JSATZ)                                              CH4  820
      READ (NDA10,REC=NXT10) NOBG,(CEG(I),I=1,NOBG),(LGBN(I),I=1,NOBG), CH4  830
     1 (AFSS(I),I=1,68)                                                 CH4  840
      NXT10 = NXT10 + 1                                                 CH4  850
      ADELL = 0.25                                                      CH4  860
      NOAG = NOBG - 1                                                   CH4  870
      NOI = KMAT                                                        CH4  880
      DO 1010 I=1,68                                                    CH4  890
        PLCK(1,I) = 0.0                                                 CH4  900
        PLCK(2,I) = 0.0                                                 CH4  910
        GDJ(1,I) = 0.0                                                  CH4  920
        GDJ(2,I) = 0.0                                                  CH4  930
        AMAR(I) = 0.0                                                   CH4  940
        AL1(I) = 0.0                                                    CH4  950
        ANSQ(I) = 0.0                                                   CH4  960
 1010 CONTINUE                                                          CH4  970
      DO 1011 I=1,2346                                                  CH4  980
        ACS(1,I) = 0.0                                                  CH4  990
        ACS(2,I) = 0.0                                                  CH4 1000
        ACS(3,I) = 0.0                                                  CH4 1010
        ACS(4,I) = 0.0                                                  CH4 1020
 1011 CONTINUE                                                          CH4 1030
      D2 = 0.                                                           CH4 1040
      DO 13 M=1,KMAT                                                    CH4 1050
        IF(IMAT(M) .EQ. 10) MU5 = M                                     CH4 1060
        D2 = D2 + CONC(M)                                               CH4 1070
        IF(M .EQ. IACT) D1 = D2                                         CH4 1080
   13 CONTINUE                                                          CH4 1090
      D3 = D2 * ERR                                                     CH4 1100
      IF(D1 .GT. D3) GOTO 12                                            CH4 1110
      CONC(MU5) = D3                                                    CH4 1120
   12 CONTINUE                                                          CH4 1130
      DO 201 LZ=1,KMAT                                                  CH4 1140
        NAUSF = 0                                                       CH4 1150
CFZJ059                                                       04.11.09  CH4 1160
        READ (NDA10,REC=NXT10) ADEN,SSON,(BCDW(I),I=1,18),(PSFS(I),I=1, CH4 1170
     1   68),NID,LTOT,IWA,IWF,IWR,(LOL(I),LA(I),LD(I),I=1,4)            CH4 1180
        NXT10 = NXT10 + 1                                               CH4 1190
C                                                                       CH4 1200
        CALL WRDA(IREAD,NDA10,NXT10,L10,ADUM,LTOT)                      CH4 1210
C                                                                       CH4 1220
CFZJ035                                                       14.09.04  CH4 1230
CFZJ059                                                       04.11.09  CH4 1240
CFZJ063                                                       26.07.11  CH4 1250
        IF(NID .NE. 6 .AND. NID .NE. 12 .AND. NID .NE. 15 .AND. NID .NE.CH4 1260
     1   17 .AND. NID .NE. 133 .AND. NID .NE. 178 .AND. NID .NE. 181    CH4 1270
     2   .AND. NID .NE. 182 .AND. NID .NE. 184) GOTO 39                 CH4 1280
        IF(NGAM .NE. NDA30) GOTO 39                                     CH4 1290
        ISOT = 1                                                        CH4 1300
        IF(NID .EQ. 12) ISOT = 2                                        CH4 1310
CFZJ035                                                       14.09.04  CH4 1320
CFZJ063                                                       26.07.11  CH4 1330
        IF(NID .EQ. 15) ISOT = 3                                        CH4 1340
        IF(NID .EQ. 17) ISOT = 4                                        CH4 1350
        IF(NID .EQ. 133) ISOT = 5                                       CH4 1360
        IF(NID .EQ. 178) ISOT = 6                                       CH4 1370
        IF(NID .EQ. 181) ISOT = 7                                       CH4 1380
        IF(NID .EQ. 182) ISOT = 8                                       CH4 1390
        IF(NID .EQ. 184) ISOT = 9                                       CH4 1400
        IDENT = IDZUT                                                   CH4 1410
        IPRI = IPRIN(1)                                                 CH4 1420
C                                                                       CH4 1430
C     USE RESONANCE CALCULATIONS FROM ZUT-DGL                           CH4 1440
C                                                                       CH4 1450
        IF(IDESIN .EQ. 0) GOTO 4447                                     CH4 1460
        DO 2000 I=1,IACT                                                CH4 1470
          IF(IMAT(I) .EQ. 6) ITH = I                                    CH4 1480
          IF(IMAT(I) .EQ. 12) IU8 = I                                   CH4 1490
CFZJ035                                                       14.09.04  CH4 1500
CFZJ063                                                       26.07.11  CH4 1510
          IF(IMAT(I) .EQ. 15) IPU0 = I                                  CH4 1520
          IF(IMAT(I) .EQ. 17) IPU2 = I                                  CH4 1530
          IF(IMAT(I) .EQ. 133) INP7 = I                                 CH4 1540
          IF(IMAT(I) .EQ. 178) IAM1 = I                                 CH4 1550
          IF(IMAT(I) .EQ. 181) IAM3 = I                                 CH4 1560
          IF(IMAT(I) .EQ. 182) ICM2 = I                                 CH4 1570
          IF(IMAT(I) .EQ. 184) ICM4 = I                                 CH4 1580
 2000   CONTINUE                                                        CH4 1590
        IF(ISOT .EQ. 1) SMC = CONC(ITH)                                 CH4 1600
        IF(ISOT .EQ. 2) SMC = CONC(IU8)                                 CH4 1610
CFZJ035                                                       14.09.04  CH4 1620
CFZJ063                                                       26.07.11  CH4 1630
        IF(ISOT .EQ. 3) SMC = CONC(IPU0)                                CH4 1640
        IF(ISOT .EQ. 4) SMC = CONC(IPU2)                                CH4 1650
        IF(ISOT .EQ. 5) SMC = CONC(INP7)                                CH4 1660
        IF(ISOT .EQ. 6) SMC = CONC(IAM1)                                CH4 1670
        IF(ISOT .EQ. 7) SMC = CONC(IAM3)                                CH4 1680
        IF(ISOT .EQ. 8) SMC = CONC(ICM2)                                CH4 1690
        IF(ISOT .EQ. 9) SMC = CONC(ICM4)                                CH4 1700
C                                                                       CH4 1710
        CALL RESONA(IPRI,NDES,KL(LNDES),TEMZUT,NT)                      CH4 1720
C                                                                       CH4 1730
        IDENT = 0                                                       CH4 1740
        DO 44432 I=1,68                                                 CH4 1750
          SAR(ISOT,I) = SIRA(I) / ADELL                                 CH4 1760
44432   CONTINUE                                                        CH4 1770
 4447   CONTINUE                                                        CH4 1780
        DO 853 I=1,68                                                   CH4 1790
          ADUM(I) = ADUM(I) + SAR(ISOT,I)                               CH4 1800
  853   CONTINUE                                                        CH4 1810
   39   CONTINUE                                                        CH4 1820
        DO 451 I=1,KMAT                                                 CH4 1830
          J = I                                                         CH4 1840
          MANU = IMAT(I)                                                CH4 1850
          IF(NID-MANU) 451,25,451                                       CH4 1860
  451   CONTINUE                                                        CH4 1870
   25   ADEN = CONC(J)                                                  CH4 1880
        DO 517 I=1,68                                                   CH4 1890
          PSFS(I) = PSFS(I) * ADEN                                      CH4 1900
          IF(PSFS(I) .LT. ERR) PSFS(I) = 0.                             CH4 1910
  517   CONTINUE                                                        CH4 1920
C                                                                       CH4 1930
C     DOES THIS NUCLIDE CAUSE FISSION -  IWF=1(YES)    ,IWF=0(NO)       CH4 1940
C                                                                       CH4 1950
        IF(IWA-1) 158,579,579                                           CH4 1960
  579   DO 559 I=1,68                                                   CH4 1970
          AMAR(I) = AMAR(I) + ADUM(I) * PSFS(I)                         CH4 1980
  559   CONTINUE                                                        CH4 1990
        NAUSF = 68                                                      CH4 2000
C                                                                       CH4 2010
C     THIS INFORMATION IS USED ONLY IN AVERAGING                        CH4 2020
C                                                                       CH4 2030
        IF(IWF-1) 158,157,157                                           CH4 2040
  157   DO 161 I=1,68                                                   CH4 2050
          AL1(I) = AL1(I) + ADUM(I+68) * PSFS(I)                        CH4 2060
          ANSQ(I) = ANSQ(I) + ADUM(I+68) * ADUM(I+136) * PSFS(I)        CH4 2070
  161   CONTINUE                                                        CH4 2080
        NAUSF = 204                                                     CH4 2090
C                                                                       CH4 2100
C     SET-UP SCATTERING ANDINELASTIC AND N-2N MATRICES                  CH4 2110
C                                                                       CH4 2120
  158   DO 170 IJ=1,4                                                   CH4 2130
          IF(LOL(IJ)-1) 169,165,165                                     CH4 2140
  165     KT = 1                                                        CH4 2150
          KZQ = 0                                                       CH4 2160
          IF(IJ-3) 5807,5801,5801                                       CH4 2170
 5801     KZQ = IJ - 2                                                  CH4 2180
 5807     KZ = 0                                                        CH4 2190
          KB = LD(IJ)                                                   CH4 2200
          MAL = LA(IJ)                                                  CH4 2210
          DO 163 I=1,MAL                                                CH4 2220
            IF(KZQ) 5814,5814,5811                                      CH4 2230
 5811       KZ = KZ + 1                                                 CH4 2240
            MXUP = NAUSF + KZ                                           CH4 2250
            PLCK(KZQ,I) = PLCK(KZQ,I) + ADUM(MXUP) * PSFS(I)            CH4 2260
            GOTO 8863                                                   CH4 2270
 5814       KZ = KZ + 1                                                 CH4 2280
            MXUP = NAUSF + KZ                                           CH4 2290
            GDJ(IJ,I) = GDJ(IJ,I) + ADUM(MXUP) * PSFS(I)                CH4 2300
 8863       KT = KT + 1                                                 CH4 2310
            KB = KB + 1                                                 CH4 2320
            IF(KB-69) 167,167,168                                       CH4 2330
  168       KB = 69                                                     CH4 2340
  167       DO 163 J=KT,KB                                              CH4 2350
              KP = ((I-1)*(136-I)) / 2 + J - 1                          CH4 2360
              KZ = KZ + 1                                               CH4 2370
              MXUP = NAUSF + KZ                                         CH4 2380
              ACS(IJ,KP) = ACS(IJ,KP) + ADUM(MXUP) * PSFS(I)            CH4 2390
  163     CONTINUE                                                      CH4 2400
  169     NAUSF = NAUSF + LOL(IJ)                                       CH4 2410
  170   CONTINUE                                                        CH4 2420
  201 CONTINUE                                                          CH4 2430
C                                                                       CH4 2440
C     CALCULATE THE TOTAL CROSS-SECTIONS FOR EACH GROUP                 CH4 2450
C                                                                       CH4 2460
      JJ = 1                                                            CH4 2470
      KK = 68                                                           CH4 2480
      DO 70 J=1,68                                                      CH4 2490
        D(J) = AL1(J)                                                   CH4 2500
        AL1(J) = 0.0                                                    CH4 2510
        AST = 0.0                                                       CH4 2520
        DO 176 I=JJ,KK                                                  CH4 2530
          AST = AST + ACS(1,I) + ACS(2,I) + ACS(3,I)                    CH4 2540
  176   CONTINUE                                                        CH4 2550
        BST(J) = AST + AMAR(J)                                          CH4 2560
        JJ = KK + 1                                                     CH4 2570
        KK = KK + 68 - J                                                CH4 2580
   70 CONTINUE                                                          CH4 2590
      DO 8800 I=1,NOAG                                                  CH4 2600
        AL1(I) = 0.                                                     CH4 2610
 8800 CONTINUE                                                          CH4 2620
C                                                                       CH4 2630
C     SETZE DIE BUCKLINGS                                               CH4 2640
C                                                                       CH4 2650
      BUMIN = ERR                                                       CH4 2660
      DO 8831 I=1,NOAG                                                  CH4 2670
        IF(IBUCK .EQ. 0 .OR. ABS(BUCKS(I,NIRPI)) .LT. BUMIN)            CH4 2680
     1   BUCKS(I,NIRPI) = BUMIN                                         CH4 2690
        AL1(I) = -BUCKS(I,NIRPI)                                        CH4 2700
 8831 CONTINUE                                                          CH4 2710
C                                                                       CH4 2720
C     P-1 CALCULATION                                                   CH4 2730
C                                                                       CH4 2740
      CALL PONE                                                         CH4 2750
C                                                                       CH4 2760
C     READ IN L-S  AVERAGE CROSS SECTIONS                               CH4 2770
C                                                                       CH4 2780
      CALL CSAV(IMAT,A(KA(LOUSI)),KL(LOUSI),A(KA(LTOSI)),A(KA(LABSI)),  CH4 2790
     1 A(KA(LFISI)),KL(LFISI),A(KA(LXNU)),KDES,A(KA(LSN2N)),A(KA(LTRSI))CH4 2800
     2 ,KL(LTRSI))                                                      CH4 2810
C                                                                       CH4 2820
      NURTHM = 0                                                        CH4 2830
      MAFIA = 5                                                         CH4 2840
      RETURN                                                            CH4 2850
      END                                                               CH4 2860
      SUBROUTINE RESONA(IPRI,NDES,NXS,TEMZUT,NT)                        RES   10
C                                                                       RES   20
      DIMENSION NRSAZ(37),DENHOM(2),SISP(68),INHALT(34,2),S(2,68),TS(2),RES   30
     1 SIRAN(68,2),NDES(NXS),TEMZUT(NXS)                                RES   40
C                                                                       RES   50
CFZJ035                                                       14.09.04  RES   60
CFZJ063                                                       26.07.11  RES   70
      COMMON /GRENZE/ NURTHM,THERGR,IDGAM,IDZUT,IDTHER,NGAM,IDESIN,JJGM,RES   80
     1 MSTU,MGHUS,NNOBG,NZT(10,9),IZUT(10,2,10),SIRA(68),NIRPI,NID,NDA30RES   90
C                                                                       RES  100
CFZJ035                                                       14.09.04  RES  110
CFZJ063                                                       26.07.11  RES  120
      COMMON /RI/ SM(10,9,2),IZUT1(10,9,10),IZUT2(10,9,10),SMC          RES  130
C                                                                       RES  140
CFZJ055                                                       25.09.07  RES  150
CFZJ042                                                       09.09.05  RES  160
      COMMON /IWAGA/ IWAGAM                                             RES  170
C                                                                       RES  180
   31 FORMAT (' SPECTRUM ZONE:',I3,', NUCLIDE:',I3,', FUEL DESIGN NO.:',RES  190
     1 I2,', TEMPERATURE:',E12.5,' CELSIUS, HOMOGENIZED ATOM DENSITY:', RES  200
     2 E12.5,','/' RESONANCE INTEGRAL:',E12.5,' BARN')                  RES  210
  101 FORMAT ('1',10('*'),'DATA SET IS SPECIFIED IN INPUT WITH ID NO',I6RES  220
     1 ,' CASE TERMINATED',10('*'))                                     RES  230
  103 FORMAT (' RECORD NO',I4,' ID NO',I6,' CONTAINS RESONANCE DATA FOR RES  240
     1NUCLIDE',I5,' :'/' GRP.  RES.SIGM....')                           RES  250
  104 FORMAT (4(I4,E15.6,6X))                                           RES  260
  106 FORMAT (' FOR NUCLIDE',I4,' THE FOLLOWING RESONANCE INTEGRALS ARE RES  270
     1USED:'/' (GRP., RES.SIGM....)')                                   RES  280
  109 FORMAT (' ')                                                      RES  290
 9000 FORMAT (/' ***WARNING***  ABSORBER CONCENTRATION EXCEEDS THE RANGERES  300
     1 COVERED BY THE AVAILABLE RESONANCE CROSS SECTIONS.'/' (NUCLIDE:',RES  310
     2 I3,' ACTUAL CONCENTRATION:',E12.5,' COVERED RANGE:',E12.5,' - ', RES  320
     3 E12.5,')'/)                                                      RES  330
C                                                                       RES  340
C                                                                       RES  350
      M6 = 6                                                            RES  360
      DO 200 M2=1,2                                                     RES  370
        NAE25 = 1                                                       RES  380
        NA = NAE25                                                      RES  390
        NUCLID = NID                                                    RES  400
        NEI25 = NGAM                                                    RES  410
        IF(IDESIN .EQ. 0) GOTO 19                                       RES  420
        NRA = 0                                                         RES  430
        IF(NUCLID .EQ. 6) NRA = 1                                       RES  440
        IF(NUCLID .EQ. 12) NRA = 2                                      RES  450
CFZJ035                                                       14.09.04  RES  460
CFZJ063                                                       26.07.11  RES  470
        IF(NUCLID .EQ. 15) NRA = 3                                      RES  480
        IF(NUCLID .EQ. 17) NRA = 4                                      RES  490
        IF(NUCLID .EQ. 133) NRA = 5                                     RES  500
        IF(NUCLID .EQ. 178) NRA = 6                                     RES  510
        IF(NUCLID .EQ. 181) NRA = 7                                     RES  520
        IF(NUCLID .EQ. 182) NRA = 8                                     RES  530
        IF(NUCLID .EQ. 184) NRA = 9                                     RES  540
        IF(NRA .EQ. 0) GOTO 108                                         RES  550
   19   CONTINUE                                                        RES  560
        READ (NEI25,REC=NAE25) KENNNR,MAXSAE,LIBIN,LIBSAE,NGGR          RES  570
        NAE25 = NAE25 + 1                                               RES  580
        DO 10 N=1,NGGR                                                  RES  590
          SIRA(N) = 0.                                                  RES  600
   10   CONTINUE                                                        RES  610
        IF(IDESIN .EQ. 0) GOTO 23                                       RES  620
        EPST = 0.001                                                    RES  630
        TKEL = 273.                                                     RES  640
        TMIL = 10000.                                                   RES  650
        LENNNR = IDZUT                                                  RES  660
        TS(1) = -TKEL                                                   RES  670
        TS(2) = TMIL                                                    RES  680
        NDESI = NDES(NIRPI)                                             RES  690
        T = TEMZUT(NIRPI)                                               RES  700
        NZ = NZT(NDESI,NRA)                                             RES  710
        DO 20 M=1,37                                                    RES  720
          NRSAZ(M) = 0                                                  RES  730
   20   CONTINUE                                                        RES  740
        DO 21 M=1,NZ                                                    RES  750
          IF(M2 .EQ. 1) NRSAZ(M) = IZUT1(NDESI,NRA,M)                   RES  760
          IF(M2 .EQ. 2) NRSAZ(M) = IZUT2(NDESI,NRA,M)                   RES  770
   21   CONTINUE                                                        RES  780
        DO 22 K=1,2                                                     RES  790
          DO 22 N=1,NGGR                                                RES  800
            S(K,N) = 0.                                                 RES  810
   22   CONTINUE                                                        RES  820
   23   CONTINUE                                                        RES  830
        IF(KENNNR . EQ. LENNNR)  GOTO 11                                RES  840
        WRITE (M6,101) LENNNR                                           RES  850
        STOP                                                            RES  860
   11   CONTINUE                                                        RES  870
        DO 77 LL=1,NZ                                                   RES  880
          NAE25 = 2                                                     RES  890
          DO 76 L1=1,LIBSAE                                             RES  900
            READ (NEI25,REC=NAE25) ((INHALT(L,K),L=1,LIBIN),K=1,2)      RES  910
            NAE25 = NAE25 + 1                                           RES  920
            DO 76 LLL=1,LIBIN                                           RES  930
              IF(INHALT(LLL,1) .EQ. NRSAZ(LL)) GOTO 77                  RES  940
   76     CONTINUE                                                      RES  950
          WRITE(6,*) 'RESONANCE CROSS SECTIONS MISSING'                 RES  960
          STOP                                                          RES  970
   77   CONTINUE                                                        RES  980
        NZASAE = 0                                                      RES  990
        DO 80 I=1,LIBIN                                                 RES 1000
          IF(NRSAZ(I) .GT. 0) NZASAE = NZASAE + 1                       RES 1010
   80   CONTINUE                                                        RES 1020
        IF(NZASAE .LE. 0) GOTO 108                                      RES 1030
        NZSAE = NZASAE                                                  RES 1040
        DO 71 I=1,LIBSAE                                                RES 1050
          NA = NA + 1                                                   RES 1060
          NAE25 = NA                                                    RES 1070
          READ (NEI25,REC=NAE25) ((INHALT(L,K),L=1,LIBIN),K=1,2)        RES 1080
          NAE25 = NAE25 + 1                                             RES 1090
          DO 72 J=1,LIBIN                                               RES 1100
            IF(INHALT(J,2) .NE. NUCLID) GOTO 73                         RES 1110
            DO 82 II=1,NZASAE                                           RES 1120
              IF(INHALT(J,1) .EQ. NRSAZ(II)) GOTO 83                    RES 1130
   82       CONTINUE                                                    RES 1140
            GOTO 73                                                     RES 1150
   83       CONTINUE                                                    RES 1160
            NZSAE = NZSAE - 1                                           RES 1170
            IFALNR = INHALT(J,1)                                        RES 1180
            NAE25 = 1 + LIBSAE + (I-1) * LIBIN + J                      RES 1190
            READ (NEI25,REC=NAE25) (SISP(N),N=1,NGGR),TRI,DENHOM(M2)    RES 1200
            NAE25 = NAE25 + 1                                           RES 1210
            TRI = TRI - TKEL                                            RES 1220
            IF(IDESIN .LE. 0) GOTO 25                                   RES 1230
            IT = 1                                                      RES 1240
            IF(TRI .GT. T) IT = 2                                       RES 1250
            Q = T - TS(IT)                                              RES 1260
            IF(ABS(Q) .LT. EPST) GOTO 26                                RES 1270
            Q = (T-TRI) / Q                                             RES 1280
            IF(Q .GT. 1.) GOTO 26                                       RES 1290
            TS(IT) = TRI                                                RES 1300
            DO 24 N=1,NGGR                                              RES 1310
              S(IT,N) = SISP(N)                                         RES 1320
   24       CONTINUE                                                    RES 1330
            GOTO 26                                                     RES 1340
   25       CONTINUE                                                    RES 1350
            WRITE (M6,103) NAE25,IFALNR,NUCLID                          RES 1360
            DO 105 N1=1,17                                              RES 1370
              N2 = N1 + 17                                              RES 1380
              N3 = N2 + 17                                              RES 1390
              N4 = N3 + 17                                              RES 1400
              WRITE (M6,104) N1,SISP(N1),N2,SISP(N2),N3,SISP(N3),N4,    RES 1410
     1         SISP(N4)                                                 RES 1420
  105       CONTINUE                                                    RES 1430
            DO 70 N=1,NGGR                                              RES 1440
              SIRA(N) = SIRA(N) + SISP(N)                               RES 1450
   70       CONTINUE                                                    RES 1460
   26       CONTINUE                                                    RES 1470
            IF(NZSAE .EQ. 0) GOTO 75                                    RES 1480
   73       CONTINUE                                                    RES 1490
   72     CONTINUE                                                      RES 1500
   71   CONTINUE                                                        RES 1510
   75   CONTINUE                                                        RES 1520
        IF(IDESIN .LE. 0) GOTO 30                                       RES 1530
        IBUCH = 0                                                       RES 1540
        IF(TS(1) .LE. -TKEL) IBUCH = IBUCH - 1                          RES 1550
        IF(TS(2) .GE. TMIL) IBUCH = IBUCH + 1                           RES 1560
        IF(IBUCH .EQ. 0) GOTO 28                                        RES 1570
        I1 = (3+IBUCH) / 2                                              RES 1580
        I2 = (3-IBUCH) / 2                                              RES 1590
        DO 27 N=1,NGGR                                                  RES 1600
          S(I1,N) = S(I2,N)                                             RES 1610
   27   CONTINUE                                                        RES 1620
        TS(I1) = TS(I2)                                                 RES 1630
   28   CONTINUE                                                        RES 1640
        TT = TS(2) - TS(1)                                              RES 1650
        IF(ABS(TT) .GT. EPST) TT = (T-TS(1)) / TT                       RES 1660
        DO 201 N=1,NGGR                                                 RES 1670
          SIRAN(N,M2) = S(1,N) * (1.-TT) + S(2,N) * TT                  RES 1680
  201   CONTINUE                                                        RES 1690
        IF(SM(NDESI,NRA,M2) .GE. 0.) GOTO 200                           RES 1700
        SM(NDESI,NRA,M2) = DENHOM(M2)                                   RES 1710
  200 CONTINUE                                                          RES 1720
      SM0 = SM(NDESI,NRA,1) + SM(NDESI,NRA,2)                           RES 1730
      IF(SM0 .EQ. 0. .OR. SMC .EQ. 0.) GOTO 202                         RES 1740
CFZJ042                                                       09.09.05  RES 1750
      IF(IWAGAM .EQ. 1) GOTO 202                                        RES 1760
      IF(SMC .GE. SM(NDESI,NRA,2) .AND. SMC .LE. SM(NDESI,NRA,1)) GOTO  RES 1770
     1 202                                                              RES 1780
      WRITE(NT,9000) NUCLID,SMC,(SM(NDESI,NRA,I),I=2,1,-1)              RES 1790
CFZJ042                                                       09.09.05  RES 1800
      IWAGAM = 1                                                        RES 1810
  202 CONTINUE                                                          RES 1820
      SMA = SMC                                                         RES 1830
      IF(SMC .LT. SM(NDESI,NRA,2)) SMC = SM(NDESI,NRA,2)                RES 1840
      IF(SMC .GT. SM(NDESI,NRA,1)) SMC = SM(NDESI,NRA,1)                RES 1850
      DSM = SM(NDESI,NRA,1) - SM(NDESI,NRA,2)                           RES 1860
      DSMR = 1.E-5 * SM(NDESI,NRA,1)                                    RES 1870
      RINP = 0.                                                         RES 1880
      IF(DSM .GT. DSMR) RINP = (SM(NDESI,NRA,1)-SMC) / DSM              RES 1890
      RESINT = 0.                                                       RES 1900
      DO 29 N=1,NGGR                                                    RES 1910
        SIRA(N) = SIRAN(N,1) + RINP * (SIRAN(N,2)-SIRAN(N,1))           RES 1920
        RESINT = RESINT + SIRA(N)                                       RES 1930
   29 CONTINUE                                                          RES 1940
      IF(IPRI.NE.-1) WRITE (NT,31) NIRPI,NUCLID,NDESI,T,SMA,RESINT      RES 1950
   30 CONTINUE                                                          RES 1960
      IF(IPRI .LT. 1) GOTO 108                                          RES 1970
      WRITE (M6,106) NUCLID                                             RES 1980
      DO 107 N1=1,17                                                    RES 1990
        N2 = N1 + 17                                                    RES 2000
        N3 = N2 + 17                                                    RES 2010
        N4 = N3 + 17                                                    RES 2020
        WRITE (M6,104) N1,SIRA(N1),N2,SIRA(N2),N3,SIRA(N3),N4,SIRA(N4)  RES 2030
  107 CONTINUE                                                          RES 2040
      WRITE (M6,109)                                                    RES 2050
  108 CONTINUE                                                          RES 2060
      RETURN                                                            RES 2070
      END                                                               RES 2080
      SUBROUTINE CSAV(IMAT,OUSIG,LA0,TOSIG,ABSIG,FISIG,LF0,XNU,KDES,SN2NCSA   10
     1 ,TRSIG,LT0)                                                      CSA   20
C                                                                       CSA   30
CFZJ035                                                       14.09.04  CSA   40
CFZJ063                                                       26.07.11  CSA   50
      DIMENSION D(108),B(108),SIGU(1),EN(250),GAMN(250),GAMGAM(250),    CSA   60
     1L(250),GAMT(99),FAC3(99),FAC1(99),SIGZ(99),ZONE(9),TERM(9),AT(108)CSA   70
     2,LP(108),T(1),AR(1),FACT(1),AL1(69),AFSS(69),AMAR(69),ADUM(5100), CSA   80
     3 BST(69),CEG(33),LGBN(35),CGM(33),CTL(33),CRAM(33),CD(33),CSAR(33)CSA   90
     4 ,CX(33),CNSQ(33),APHI(69),AJ(69),DB0(528),DB1(528),DBN(528),     CSA  100
     5 DBIN(528),SIG(2,1),COEF1(1,99),BETA(1,50),GK(1,99),ZI(1,99),     CSA  110
     6 Z(2,1),SAR(9,68),ACS(4,2346),PLCK(2,69),GDJ(2,70),FTM(528),      CSA  120
     7 TAU(69),THRM(69),SECM(69),FIRM(69),ANSQ(68),LOL(4),LA(4),LD(4),  CSA  130
     8 PSFS(70),FSN(18),RB0(33),RB1(33),RBIN(33),RBN(33),TTR(33),TPB(33)CSA  140
     9 ,G(9,33,14),F(4158),XTJ(33),GN2N(33),FN2N(69),IMAT(KMAT),        CSA  150
     X OUSIG(LA0),TOSIG(LA0),ABSIG(LA0),FISIG(LF0),XNU(LF0),KDES(NXS),  CSA  160
     Y SN2N(LF0),TRSIG(LT0)                                             CSA  170
C                                                                       CSA  180
CFZJ059                                                       04.11.09  CSA  190
      CHARACTER*4 BCDW(18)                                              CSA  200
C                                                                       CSA  210
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    CSA  220
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    CSA  230
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PICSA  240
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D,NLAYP,ITTT, CSA  250
     4 LIMT                                                             CSA  260
C                                                                       CSA  270
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), CSA  280
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10CSA  290
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11CSA  300
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         CSA  310
C                                                                       CSA  320
      COMMON /BLOCKG/ K,D,B,AU,F,JGM,SIG,SIGU,EN,GAMN,GAMGAM,L,GAMT,FAC3CSA  330
     1 ,FAC1,SIGZ,COEF1,BETA,GK,ZI,Z,ZONE,TERM,AT,SAR,LP,T,AR,FACT,Al1, CSA  340
     2 AFSS,AMAR,ACS,CEG,LGBN,CD,ANSQ,LOL,LA,LD,PSFS,PLCK,FSN,GDJ,RB0,  CSA  350
     3 RB1,RBIN,RBN,TTR,TPB,NOAG,NOBG,ADELL,MICR,NOI,MTP,WAG,TKN,ADUM,  CSA  360
     4 BXCX                                                             CSA  370
C                                                                       CSA  380
CFZJ062                                                       04.05.11  CSA  390
      COMMON /ORIGEN/ LOB,NOR,VOR(100)                                  CSA  400
C                                                                       CSA  410
CFZJ055                                                       25.09.07  CSA  420
C                                                                       CSA  430
      COMMON /FLUXN/ DU(361),IACT                                       CSA  440
C                                                                       CSA  450
      EQUIVALENCE(G(1,1,1),F(1)),(BST(1),F(1)),(F(70),APHI(1)),         CSA  460
     1 (F(140),AJ(1)),(FIRM(1),F(210)),(F(280),SECM(1)),(F(350),THRM(1))CSA  470
     2 ,(F(420),TAU(1)),(CGM(1),F(500)),(CTL(1),F(540)),(F(580),CRAM(1))CSA  480
     3 ,(CSAR(1),F(620)),(F(660),CX(1)),(CNSQ(1),F(700)),(FTM(1),F(800))CSA  490
     4 ,(DB0(1),F(1350)),(DB1(1),F(1900)),(DBIN(1),F(2450)),            CSA  500
     5 (DBN(1),F(3000))                                                 CSA  510
C                                                                       CSA  520
C                                                                       CSA  530
      NIRPI = IPRIN(7)                                                  CSA  540
      JSATZ = 2 + KDES(NIRPI)                                           CSA  550
      NXT10 = JAD10(JSATZ)                                              CSA  560
      NOTG = NOBG + 1                                                   CSA  570
      LGBN(NOTG) = 70                                                   CSA  580
      APHI(69) = 0.                                                     CSA  590
      PSFS(69) = 0.                                                     CSA  600
      PSFS(70) = 0.                                                     CSA  610
      AJ(69) = 0.0                                                      CSA  620
      FN2N(69)=0.                                                       CSA  630
      PLCK(1,69)=0.                                                     CSA  640
      PLCK(2,69)=0.                                                     CSA  650
C                                                                       CSA  660
C     CALCULATE NOW FOR THE BROAD GROUPS (NOAG BROAD GROUPS)            CSA  670
C                                                                       CSA  680
      DO 4201 K=1,33                                                    CSA  690
        CGM(K) = 0.0                                                    CSA  700
        CTL(K) = 0.0                                                    CSA  710
        CX(K) = 0.0                                                     CSA  720
 4201 CONTINUE                                                          CSA  730
      DO 110 K=1,NOBG                                                   CSA  740
        LL = LGBN(K)                                                    CSA  750
        LU = LGBN(K+1) - 1                                              CSA  760
        DO 100 J=LL,LU                                                  CSA  770
          CGM(K) = CGM(K) + APHI(J)                                     CSA  780
          CTL(K) = CTL(K) + AJ(J)                                       CSA  790
          CX(K) = CX(K) + AFSS(J)                                       CSA  800
  100   CONTINUE                                                        CSA  810
  110 CONTINUE                                                          CSA  820
      DO 3018 J=1,NOAG                                                  CSA  830
        FAC3(J) = 1.0                                                   CSA  840
 3018 CONTINUE                                                          CSA  850
      READ (NDA10,REC=NXT10) NOBG,(CEG(I),I=1,NOBG),(LGBN(I),I=1,NOBG), CSA  860
     1 (AFSS(I),I=1,68)                                                 CSA  870
      NXT10 = NXT10 + 1                                                 CSA  880
      NOINOI = NOI                                                      CSA  890
      DO 521 LBJ=1,NOINOI                                               CSA  900
CFZJ059                                                       04.11.09  CSA  910
        READ (NDA10,REC=NXT10) DUMY,SSON,(BCDW(I),I=1,18),(PSFS(I),I=1, CSA  920
     1   68),NID,LTOT,IWA,IWF,IWR,(LOL(I),LA(I),LD(I),I=1,4)            CSA  930
        NXT10 = NXT10 + 1                                               CSA  940
C                                                                       CSA  950
        CALL WRDA(IREAD,NDA10,NXT10,L10,ADUM,LTOT)                      CSA  960
C                                                                       CSA  970
CFZJ035                                                       14.09.04  CSA  980
CFZJ063                                                       26.07.11  CSA  990
        IF(IWR .NE. 1 .AND. NID .NE. 15 .AND. NID .NE. 17 .AND. NID .NE.CSA 1000
     1   133 .AND. NID .NE. 178 .AND. NID .NE. 181 .AND. NID .NE. 182   CSA 1010
     2   .AND. NID .NE. 184) GOTO 700                                   CSA 1020
        ISOT = 1                                                        CSA 1030
        IF(NID .EQ. 12) ISOT = 2                                        CSA 1040
CFZJ035                                                       14.09.04  CSA 1050
CFZJ063                                                       26.07.11  CSA 1060
        IF(NID .EQ. 15) ISOT = 3                                        CSA 1070
        IF(NID .EQ. 17) ISOT = 4                                        CSA 1080
        IF(NID .EQ. 133) ISOT = 5                                       CSA 1090
        IF(NID .EQ. 178) ISOT = 6                                       CSA 1100
        IF(NID .EQ. 181) ISOT = 7                                       CSA 1110
        IF(NID .EQ. 182) ISOT = 8                                       CSA 1120
        IF(NID .EQ. 184) ISOT = 9                                       CSA 1130
        DO 699 I=1,68                                                   CSA 1140
          ADUM(I) = ADUM(I) + SAR(ISOT,I)                               CSA 1150
  699   CONTINUE                                                        CSA 1160
  700   CONTINUE                                                        CSA 1170
        NAUSF = 0                                                       CSA 1180
        DO 701 J=1,2346                                                 CSA 1190
          ACS(1,J) = 0.0                                                CSA 1200
          ACS(2,J) = 0.0                                                CSA 1210
          ACS(3,J) = 0.0                                                CSA 1220
          ACS(4,J) = 0.0                                                CSA 1230
  701   CONTINUE                                                        CSA 1240
        DO 5841 I=1,68                                                  CSA 1250
          GDJ(1,I) = 0.0                                                CSA 1260
          B(I) = 0.0                                                    CSA 1270
          EN(I) = 0.0                                                   CSA 1280
          GAMT(I) = 0.0                                                 CSA 1290
          GAMN(I) = 0.0                                                 CSA 1300
          FN2N(I) = 0.0                                                 CSA 1310
          GDJ(2,I) = 0.0                                                CSA 1320
          PLCK(1,I) = 0.0                                               CSA 1330
          PLCK(2,I) = 0.0                                               CSA 1340
 5841   CONTINUE                                                        CSA 1350
        DO 855 K=1,69                                                   CSA 1360
          FIRM(K) = 0.0                                                 CSA 1370
          SECM(K) = 0.0                                                 CSA 1380
  855   CONTINUE                                                        CSA 1390
        DO 856 K=1,33                                                   CSA 1400
          RB0(K) = 0.0                                                  CSA 1410
          RB1(K) = 0.0                                                  CSA 1420
          RBN(K) = 0.0                                                  CSA 1430
          RBIN(K) = 0.0                                                 CSA 1440
          TTR(K) = 0.0                                                  CSA 1450
          XTJ(K) = 0.0                                                  CSA 1460
          GN2N(K) = 0.0                                                 CSA 1470
          CSAR(K) = 0.0                                                 CSA 1480
  856   CONTINUE                                                        CSA 1490
        DO 858 K=1,LIMT                                                 CSA 1500
          DB0(K) = 0.0                                                  CSA 1510
          DB1(K) = 0.0                                                  CSA 1520
          DBIN(K) = 0.0                                                 CSA 1530
          DBN(K) = 0.0                                                  CSA 1540
  858   CONTINUE                                                        CSA 1550
        DO 4 I=1,IACT                                                   CSA 1560
          N2N = IMAT(I)                                                 CSA 1570
          IF(N2N .EQ. NID .AND. N2N .NE. 179) GOTO 5                    CSA 1580
    4   CONTINUE                                                        CSA 1590
        GOTO 3                                                          CSA 1600
    5   CONTINUE                                                        CSA 1610
        KB = LD(2)                                                      CSA 1620
        KT = 0                                                          CSA 1630
        MAL = LA(2)                                                     CSA 1640
        MXUP = 204 + LOL(1)                                             CSA 1650
        DO 2 I=1,MAL                                                    CSA 1660
          KT = KT + 1                                                   CSA 1670
          KB = KB + 1                                                   CSA 1680
          DO 1 J=KT,KB                                                  CSA 1690
            FN2N(I) = FN2N(I) + ADUM(J+MXUP)                            CSA 1700
    1     CONTINUE                                                      CSA 1710
          MXUP = MXUP + KB - KT                                         CSA 1720
    2   CONTINUE                                                        CSA 1730
    3   CONTINUE                                                        CSA 1740
        IF(IWA-1) 806,808,808                                           CSA 1750
  808   IF(IWF-1) 702,703,703                                           CSA 1760
  703   NAUSF = 136                                                     CSA 1770
  702   NAUSF = NAUSF + 68                                              CSA 1780
  806   DO 770 IJ=1,4                                                   CSA 1790
          IF(LOL(IJ)-1) 769,765,765                                     CSA 1800
  765     KT = 1                                                        CSA 1810
          KZQ = 0                                                       CSA 1820
          IF(IJ-3) 5857,5851,5851                                       CSA 1830
 5851     KZQ = IJ - 2                                                  CSA 1840
 5857     KZ = 0                                                        CSA 1850
          KB = LD(IJ)                                                   CSA 1860
          MAL = LA(IJ)                                                  CSA 1870
          DO 763 I=1,MAL                                                CSA 1880
            IF(KZQ) 5864,5864,5861                                      CSA 1890
 5861       KZ = KZ + 1                                                 CSA 1900
            MXUP = NAUSF + KZ                                           CSA 1910
            PLCK(KZQ,I) = PLCK(KZQ,I) + ADUM(MXUP) * PSFS(I)            CSA 1920
            GOTO 5863                                                   CSA 1930
 5864       KZ = KZ + 1                                                 CSA 1940
            MXUP = NAUSF + KZ                                           CSA 1950
            GDJ(IJ,I) = GDJ(IJ,I) + ADUM(MXUP) * PSFS(I)                CSA 1960
 5863       KT = KT + 1                                                 CSA 1970
            KB = KB + 1                                                 CSA 1980
            IF(KB-69) 767,767,768                                       CSA 1990
  768       KB = 69                                                     CSA 2000
  767       DO 763 J=KT,KB                                              CSA 2010
              KZ = KZ + 1                                               CSA 2020
              KP = ((I-1)*(136-I)) / 2 + J - 1                          CSA 2030
              MXUP = NAUSF + KZ                                         CSA 2040
              ACS(IJ,KP) = ADUM(MXUP) * PSFS(I)                         CSA 2050
  763     CONTINUE                                                      CSA 2060
  769     NAUSF = NAUSF + LOL(IJ)                                       CSA 2070
  770   CONTINUE                                                        CSA 2080
        DO 610 K=1,NOBG                                                 CSA 2090
          LL = LGBN(K)                                                  CSA 2100
          LU = LGBN(K+1) - 1                                            CSA 2110
          IF(IWA-1) 809,632,632                                         CSA 2120
  632     DO 600 J=LL,LU                                                CSA 2130
            XTJ(K) = XTJ(K) + ADUM(J) * AJ(J) * PSFS(J)                 CSA 2140
            CSAR(K) = CSAR(K) + ADUM(J) * APHI(J) * PSFS(J)             CSA 2150
            GN2N(K) = GN2N(K) + FN2N(J) * APHI(J) * PSFS(J)             CSA 2160
            IF(IWF-1) 600,666,666                                       CSA 2170
  666       FIRM(K) = FIRM(K) + ADUM(J+68) * APHI(J) * PSFS(J)          CSA 2180
            SECM(K) = SECM(K) + ADUM(J+68) * ADUM(J+136) * APHI(J) *    CSA 2190
     1       PSFS(J)                                                    CSA 2200
  600     CONTINUE                                                      CSA 2210
C                                                                       CSA 2220
C     CALCULATE SOME OF THE GROUP CONSTANTS                             CSA 2230
C                                                                       CSA 2240
          CSAR(K) = CSAR(K) / CGM(K)                                    CSA 2250
          GN2N(K) = GN2N(K) / CGM(K)                                    CSA 2260
          FIRM(K) = FIRM(K) / CGM(K)                                    CSA 2270
          SECM(K) = SECM(K) / CGM(K)                                    CSA 2280
          IF(FIRM(K)) 1000,1000,1001                                    CSA 2290
 1001     CONTINUE                                                      CSA 2300
          EN(K) = SECM(K) / FIRM(K)                                     CSA 2310
 1000     CONTINUE                                                      CSA 2320
          GAMN(K) = CSAR(K) - FIRM(K)                                   CSA 2330
C                                                                       CSA 2340
C     CALCULATE THE BROAD GROUP MATRICES IN GROUP SCATTERING            CSA 2350
C                                                                       CSA 2360
  809     LH = LL + 1                                                   CSA 2370
          IF(LH-LU) 2093,2093,2098                                      CSA 2380
 2093     DO 5871 I=LH,LU                                               CSA 2390
            LBST = I - 1                                                CSA 2400
            DO 5871 LZ=LL,LBST                                          CSA 2410
              KP = ((LZ-1)*(136-LZ)) / 2 + I - 1                        CSA 2420
              XTJ(K) = XTJ(K) + (ACS(1,KP)+ACS(2,KP)+ACS(3,KP)) * AJ(LZ)CSA 2430
              RB0(K) = RB0(K) + ACS(3,KP) * APHI(LZ)                    CSA 2440
              RB1(K) = RB1(K) + ACS(4,KP) * AJ(LZ)                      CSA 2450
              RBN(K) = RBN(K) + ACS(2,KP) * APHI(LZ)                    CSA 2460
              RBIN(K) = RBIN(K) + ACS(1,KP) * APHI(LZ)                  CSA 2470
 5871     CONTINUE                                                      CSA 2480
 2098     DO 5873 I=LL,LU                                               CSA 2490
            XTJ(K) = XTJ(K) + (GDJ(1,I)+GDJ(2,I)+PLCK(1,I)) * AJ(I)     CSA 2500
            RBIN(K) = RBIN(K) + GDJ(1,I) * APHI(I)                      CSA 2510
            RBN(K) = RBN(K) + GDJ(2,I) * APHI(I)                        CSA 2520
            RB0(K) = RB0(K) + PLCK(1,I) * APHI(I)                       CSA 2530
            RB1(K) = RB1(K) + PLCK(2,I) * AJ(I)                         CSA 2540
 5873     CONTINUE                                                      CSA 2550
          RB0(K) = RB0(K) / CGM(K)                                      CSA 2560
          IF(CTL(K) .EQ. 0.0) GOTO 200                                  CSA 2570
          RB1(K) = RB1(K) / CTL(K)                                      CSA 2580
          GOTO 201                                                      CSA 2590
  200     CONTINUE                                                      CSA 2600
          RB1(K) = 0.0                                                  CSA 2610
  201     CONTINUE                                                      CSA 2620
          RBN(K) = RBN(K) / CGM(K)                                      CSA 2630
          RBIN(K) = RBIN(K) / CGM(K)                                    CSA 2640
          TTR(K) = RB0(K) + RBIN(K) + 2.0 * RBN(K)                      CSA 2650
          NX = K - 1                                                    CSA 2660
          IF(K-1) 610,610,602                                           CSA 2670
  602     DO 608 M=1,NX                                                 CSA 2680
            MK = ((M-1)*(NOAG*2-M)) / 2 + K - 1                         CSA 2690
            LH = LGBN(M)                                                CSA 2700
            LT = LGBN(M+1) - 1                                          CSA 2710
            DO 604 I=LL,LU                                              CSA 2720
              DO 604 LZ=LH,LT                                           CSA 2730
                KP = ((LZ-1)*(136-LZ)) / 2 + I - 1                      CSA 2740
                XTJ(M) = XTJ(M) + (ACS(1,KP)+ACS(2,KP)+ACS(3,KP)) *     CSA 2750
     1           AJ(LZ)                                                 CSA 2760
                DB0(MK) = DB0(MK) + ACS(3,KP) * APHI(LZ)                CSA 2770
                DB1(MK) = DB1(MK) + ACS(4,KP) * AJ(LZ)                  CSA 2780
                DBIN(MK) = DBIN(MK) + ACS(1,KP) * APHI(LZ)              CSA 2790
                DBN(MK) = DBN(MK) + ACS(2,KP) * APHI(LZ)                CSA 2800
  604       CONTINUE                                                    CSA 2810
            DB0(MK) = DB0(MK) / CGM(M)                                  CSA 2820
            IF(CTL(M) .EQ. 0.0) GOTO 210                                CSA 2830
            DB1(MK) = DB1(MK) / CTL(M)                                  CSA 2840
            GOTO 211                                                    CSA 2850
  210       CONTINUE                                                    CSA 2860
            DB1(MK) = 0.0                                               CSA 2870
  211       CONTINUE                                                    CSA 2880
            DBIN(MK) = DBIN(MK) / CGM(M)                                CSA 2890
            DBN(MK) = DBN(MK) / CGM(M)                                  CSA 2900
  608     CONTINUE                                                      CSA 2910
  610   CONTINUE                                                        CSA 2920
        TPB(1) = RB1(1)                                                 CSA 2930
        DO 5893 K=2,NOAG                                                CSA 2940
          TPB(K) = 0.0                                                  CSA 2950
          NK = K - 1                                                    CSA 2960
          DO 5891 I=1,NK                                                CSA 2970
            MK = ((I-1)*(NOAG*2-I)) / 2 + K - 1                         CSA 2980
            TPB(K) = TPB(K) + DB1(MK) * CTL(I)                          CSA 2990
 5891     CONTINUE                                                      CSA 3000
          IF(CTL(K) .EQ. 0.0) GOTO 220                                  CSA 3010
          TPB(K) = TPB(K) / CTL(K) + RB1(K)                             CSA 3020
          GOTO 5893                                                     CSA 3030
  220     CONTINUE                                                      CSA 3040
          TPB(K) = RB1(K)                                               CSA 3050
 5893   CONTINUE                                                        CSA 3060
C                                                                       CSA 3070
C     CALCULATE TOTAL REMOVAL SIGMA                                     CSA 3080
C                                                                       CSA 3090
        JJ = 1                                                          CSA 3100
        KK = NOAG                                                       CSA 3110
        DO 672 J=1,NOAG                                                 CSA 3120
          IF(CTL(J) .NE. 0.) XTJ(J) = XTJ(J) / CTL(J)                   CSA 3130
          AST = 0.0                                                     CSA 3140
          DO 676 I=JJ,KK                                                CSA 3150
            AST = AST + DB0(I) + DBIN(I) + DBN(I)                       CSA 3160
  676     CONTINUE                                                      CSA 3170
          GAMT(J) = AST                                                 CSA 3180
          BST(J) = AST + CSAR(J)                                        CSA 3190
          IF(CTL(J) .NE. 0.) GOTO 673                                   CSA 3200
          TPB(J) = FAC3(J) * (BST(J)+RB0(J)+RBIN(J)+RBN(J)) - TPB(J) /  CSA 3210
     1     3.0                                                          CSA 3220
          GOTO 674                                                      CSA 3230
  673     TPB(J) = XTJ(J) - TPB(J) / 3.0                                CSA 3240
          IF(TPB(J) .LT. 0.)                                            CSA 3250
     1     TPB(J) = FAC3(J) * (BST(J)+RB0(J)+RBIN(J)+RBN(J))            CSA 3260
  674     CONTINUE                                                      CSA 3270
          JJ = KK + 1                                                   CSA 3280
          KK = KK + NOAG - J                                            CSA 3290
  672   CONTINUE                                                        CSA 3300
C                                                                       CSA 3310
C     CALC. TRANSFER MATRIX                                             CSA 3320
C                                                                       CSA 3330
        DO  683 K=1,LIMT                                                CSA 3340
          FTM(K) = DBIN(K) + DB0(K) + 2. * DBN(K)                       CSA 3350
  683   CONTINUE                                                        CSA 3360
        DO 680 M=1,KMAT                                                 CSA 3370
          LB = M                                                        CSA 3380
          MANU = IMAT(M)                                                CSA 3390
          IF(MANU .EQ. NID) GOTO 685                                    CSA 3400
  680   CONTINUE                                                        CSA 3410
  685   CONTINUE                                                        CSA 3420
        LBJJ = ((IPRIN(7)-1)*KMAT+LB-1) * N26                           CSA 3430
        LBJF = ((IPRIN(7)-1)*IACT+LB-1) * N26                           CSA 3440
        LBJT = ((IPRIN(7)-1)*KMAT+LB-1) * LIMT                          CSA 3450
        DO 1511 I=1,NOAG                                                CSA 3460
          TOSIG(I+LBJJ) = TPB(I)                                        CSA 3470
          ABSIG(I+LBJJ) = CSAR(I)                                       CSA 3480
          OUSIG(I+LBJJ) = GAMT(I)                                       CSA 3490
          IF(LB .GT. IACT) GOTO 1511                                    CSA 3500
          FISIG(I+LBJF) = SECM(I)                                       CSA 3510
          XNU(I+LBJF) = EN(I)                                           CSA 3520
          SN2N(I+LBJF) = GN2N(I)                                        CSA 3530
 1511   CONTINUE                                                        CSA 3540
        DO 1512 I=1,LIMT                                                CSA 3550
          TRSIG(I+LBJT) = FTM(I)                                        CSA 3560
 1512   CONTINUE                                                        CSA 3570
  521 CONTINUE                                                          CSA 3580
      RETURN                                                            CSA 3590
      END                                                               CSA 3600
      SUBROUTINE PONE                                                   PON   10
C                                                                       PON   20
C     CALCULATE THE FLUXES AND THE CURRENTS                             PON   30
C                                                                       PON   40
CFZJ035                                                       14.09.04  PON   50
CFZJ063                                                       26.07.11  PON   60
      DIMENSION D(108),B(108),SIGU(1),EN(250),GAMN(250),GAMGAM(250),    PON   70
     1L(250),GAMT(99),FAC3(99),FAC1(99),SIGZ(99),ZONE(9),TERM(9),AT(108)PON   80
     2,LP(108),T(1),AR(1),FACT(1),AL1(69),AFSS(69),AMAR(69),ADUM(5100), PON   90
     3 BST(69),CEG(33),LGBN(35),CGM(33),CTL(33),CRAM(33),CD(33),CSAR(33)PON  100
     4 ,CX(33),CNSQ(33),APHI(69),AJ(69),DB0(528),DB1(528),DBN(528),     PON  110
     5 DBIN(528),SIG(2,1),COEF1(1,99),BETA(1,50),GK(1,99),ZI(1,99),     PON  120
     6 Z(2,1),SAR(9,68),ACS(4,2346),PLCK(2,69),GDJ(2,70),FTM(528),      PON  130
     7 TAU(69),THRM(69),SECM(69),FIRM(69),ANSQ(68),LOL(4),LA(4),LD(4),  PON  140
     8 PSFS(70),FSN(18),RB0(33),RB1(33),RBIN(33),RBN(33),TTR(33),TPB(33)PON  150
     9 ,G(9,33,14),F(4158)                                              PON  160
C                                                                       PON  170
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    PON  180
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    PON  190
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIPON  200
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 PON  210
C                                                                       PON  220
      COMMON /BLOCKG/ K,D,B,AU,F,JGM,SIG,SIGU,EN,GAMN,GAMGAM,L,GAMT,FAC3PON  230
     1 ,FAC1,SIGZ,COEF1,BETA,GK,ZI,Z,ZONE,TERM,AT,SAR,LP,T,AR,FACT,Al1, PON  240
     2 AFSS,AMAR,ACS,CEG,LGBN,CD,ANSQ,LOL,LA,LD,PSFS,PLCK,FSN,GDJ,RB0,  PON  250
     3 RB1,RBIN,RBN,TTR,TPB,NOAG,NOBG,ADELL,MICR,NOI,MTP,WAG,TKN,ADUM,  PON  260
     4 BXCX                                                             PON  270
C                                                                       PON  280
      COMMON /FLEX/ DUMMY(6),ERR                                        PON  290
C                                                                       PON  300
      EQUIVALENCE(G(1,1,1),F(1)),(JTPE3,M6),(JTPE5,N4),(BST(1),F(1)),   PON  310
     1 (F(70),APHI(1)),(F(140),AJ(1)),(FIRM(1),F(210)),(F(280),SECM(1)),PON  320
     2 (F(350),THRM(1)),(F(420),TAU(1)),(CGM(1),F(500)),(CTL(1),F(540)),PON  330
     3 (F(580),CRAM(1)),(CSAR(1),F(620)),(F(660),CX(1)),(CNSQ(1),F(700))PON  340
     4 ,(FTM(1),F(800)),(DB0(1),F(1350)),(DB1(1),F(1900)),              PON  350
     5 (DBIN(1),F(2450)),(DBN(1),F(3000))                               PON  360
C                                                                       PON  370
   72 FORMAT (1H0/35H GROUP           NET LEAKAGE TERM  /1H0)           PON  380
   73 FORMAT (I5,E26.6)                                                 PON  390
  211 FORMAT (1H0/7H  GROUP,15H           FLUX,30H                  CURRPON  400
     1ENT TERM,10X,'  GROUP',11X,'FLUX',18X,'CURRENT TERM')             PON  410
  215 FORMAT (I6,E20.8,E26.8,10X,I6,E20.8,E26.8)                        PON  420
  218 FORMAT (/' +++ CAUTION +++'/' +++ FLUX OF GROUP',I3,' .LE. 0. (=',PON  430
     1 E13.5,')     THE CODE MAKES THE EPITHERMAL BUCKLING ZERO. +++'/) PON  440
  668 FORMAT (1H1)                                                      PON  450
 8011 FORMAT (1H0,72H THE FLUXES AND CURRENT TERMS ARE CALCULATED USING PON  460
     1THE P-1 APPROXIMATION)                                            PON  470
C                                                                       PON  480
C                                                                       PON  490
      BX0 = 0.0                                                         PON  500
      BW = 0.0                                                          PON  510
      BX1 = 0.0                                                         PON  520
      IPRIN1 = 0                                                        PON  530
      AL1(NOBG) = AL1(NOAG)                                             PON  540
      IF(IPRIN(1) .LT. 4) GOTO 8838                                     PON  550
      IPRIN1 = 1                                                        PON  560
      WRITE (M6,8011)                                                   PON  570
      WRITE (M6,72)                                                     PON  580
 8838 CONTINUE                                                          PON  590
C                                                                       PON  600
C     SET-UP  L,S (AL1) FOR FINE GROUP STRUCTURE                        PON  610
C     THE AL1 GOES IN FROM HIGHEST ENERGY TO LOWEST                     PON  620
C                                                                       PON  630
      M = 1                                                             PON  640
      NOTG = NOBG + 1                                                   PON  650
      LGBN(NOTG) = 70                                                   PON  660
      DO 60 K=1,NOBG                                                    PON  670
        IF(IPRIN1 .EQ. 1) WRITE (M6,73) K,AL1(K)                        PON  680
        JK = NOTG - K                                                   PON  690
        DO 61 I=M,69                                                    PON  700
          JI = 70 - I                                                   PON  710
          IF(JI-LGBN(JK)) 59,62,62                                      PON  720
   62     CONTINUE                                                      PON  730
          AL1(JI) = AL1(JK)                                             PON  740
   61   CONTINUE                                                        PON  750
   59   M = I                                                           PON  760
   60 CONTINUE                                                          PON  770
      DO 180 J=1,68                                                     PON  780
        QTH = 3.0 * (BST(J)+PLCK(1,J)+GDJ(1,J)+GDJ(2,J)) - PLCK(2,J)    PON  790
  182   CONTINUE                                                        PON  800
        XXXXX = BST(J) - GDJ(2,J) - AL1(J)                              PON  810
        APHI(J) = BX0 + BW + AFSS(J) / ADELL - BX1 / QTH                PON  820
        IF(XXXXX .NE. 0.) APHI(J) = APHI(J) / XXXXX                     PON  830
        IF(APHI(J) .LT. 1.E+5*ERR .OR. APHI(J) .GT. 1.E-5/ERR) THEN     PON  840
         AL1(J) = ERR                                                   PON  850
         GOTO 182                                                       PON  860
        ENDIF                                                           PON  870
        AJ(J) = BX1 / QTH - AL1(J) * APHI(J)                            PON  880
C                                                                       PON  890
C     CALCULATE  BX0(J+1),BW(J+1) AND BX1(J+1)                          PON  900
C                                                                       PON  910
        BX0 = 0.0                                                       PON  920
        BX1 = 0.0                                                       PON  930
        BW = 0.0                                                        PON  940
        KJ = 68                                                         PON  950
        K = J                                                           PON  960
        DO 180 I=1,J                                                    PON  970
          BX0 = BX0 + ACS(3,K) * APHI(I)                                PON  980
          BW = BW + (ACS(1,K)+2.0*ACS(2,K)) * APHI(I)                   PON  990
          BX1 = BX1 + ACS(4,K) * AJ(I)                                  PON 1000
          KJ = KJ - 1                                                   PON 1010
          K = K + KJ                                                    PON 1020
  180 CONTINUE                                                          PON 1030
C                                                                       PON 1040
C     NORMALIZE FLUXES AND ADJUST CURRENT TERMS                         PON 1050
C                                                                       PON 1060
      TOT = 0.0                                                         PON 1070
      DO 8403 J=1,68                                                    PON 1080
        TOT = TOT + APHI(J)                                             PON 1090
 8403 CONTINUE                                                          PON 1100
      TOT = TOT * ADELL                                                 PON 1110
      DO 8023 J=1,68                                                    PON 1120
        APHI(J) = APHI(J) / TOT                                         PON 1130
        IF(APHI(J) .LT. ERR) APHI(J) = ERR                              PON 1140
        AJ(J) = AJ(J) / TOT                                             PON 1150
        IF(ABS(AJ(J)) .LT. ERR) AJ(J) = SIGN(ERR,AJ(J))                 PON 1160
 8023 CONTINUE                                                          PON 1170
      IF(IPRIN(1)-2) 500,501,501                                        PON 1180
  501 CONTINUE                                                          PON 1190
C                                                                       PON 1200
C     WRITE OUT FLUX AND CURRENTS                                       PON 1210
C                                                                       PON 1220
      WRITE (M6,668)                                                    PON 1230
      WRITE (M6,211)                                                    PON 1240
      DO 65 I=1,34                                                      PON 1250
        J = I + 34                                                      PON 1260
        WRITE (M6,215) I,APHI(I),AJ(I),J,APHI(J),AJ(J)                  PON 1270
   65 CONTINUE                                                          PON 1280
  500 CONTINUE                                                          PON 1290
      IF(IPRIN(5)) 2001,2000,2001                                       PON 1300
 2001 WRITE (N4) IPRIN(15),IPRIN(12),IPRIN(7),(APHI(I),I=1,68)          PON 1310
 2000 CONTINUE                                                          PON 1320
      RETURN                                                            PON 1330
      END                                                               PON 1340