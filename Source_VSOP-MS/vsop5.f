      SUBROUTINE CH5(MAFIA,IMAT,OUSIG,LA0,TOSIG,ABSIG,FISIG,LF0,XNU,    CH5   10
     1 TCELS,NHOTD,JAD11,CONC,VF,NTYSP)                                 CH5   20
CFZJ034                                                       31.08.04  CH5   30
C                                                                       CH5   40
C     SET UP MATRIX AND CALCULATE SPECTRUM                              CH5   50
C                                                                       CH5   60
      DIMENSION E(96),S(96),SIGA(96),DELTA(96),GFLUX(96),PHI(96),XA(96),CH5   70
     1 XF(96),XS(96),SIGN(96),SIGTR(96),SIGS(96),SELS(97),DID(18),      CH5   80
     2 BEG(51),LBON(51),NDIV(96),ST(96),VEC(5),TIN(50),XTR(96),FINU(96),CH5   90
     3 BBS(98),EKVR(96,96),TIM(96,96),DIF(6,50),IDKER(2,10),PHICAP(96), CH5  100
     4 PHU(96),IMAT(KMAT),OUSIG(LA0),TOSIG(LA0),ABSIG(LA0),FISIG(LF0),  CH5  110
     5 XNU(LF0),TCELS(5,NXS),CONC(KMAT),NHOTD(KMAT),VF(KMAT,10),        CH5  120
     6 NTYSP(NXS),JAD11(JD11)                                           CH5  130
C                                                                       CH5  140
      REAL*8 DTE                                                        CH5  150
C                                                                       CH5  160
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    CH5  170
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    CH5  180
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PICH5  190
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 CH5  200
C                                                                       CH5  210
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), CH5  220
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10CH5  230
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11CH5  240
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         CH5  250
C                                                                       CH5  260
      COMMON /BLOCKT/ DELT,NOIT,TIN,SIGA,SIGN,SIGS,SIGTR,EKVR,PHI,E,BN, CH5  270
     1 SELS,DID,BEG,LBON,NDIV,DIF,TIM,FMIC,NEVR,NBG,NALPHA,NKER,MNBG,   CH5  280
     2 FINU,DELTA,KDIV,XA,XS,XF,XTR,S,T,UB,DFO,IDT,VEC                  CH5  290
C                                                                       CH5  300
CFZJ035                                                       14.09.04  CH5  310
CFZJ063                                                       26.07.11  CH5  320
      COMMON /GRENZE/ NURTHM,THERGR,IDGAM,IDZUT,IDTHER,NGAM,IDESIN,JJGM,CH5  330
     1 MSTU,MGHUS,NNOBG,NZT(10,9),IZUT(10,2,10),SIRA(68),NIRPI,NID,NDA30CH5  340
     2 ,MDUM                                                            CH5  350
C                                                                       CH5  360
      COMMON /TERFLU/ KANN,NUTTE,NEUSPK,NSAEFL,A39,B39,C39,A40,B40,C40, CH5  370
     1 AD39,AD40,AD39R,AD40R,SSPU39,SSPU40                              CH5  380
C                                                                       CH5  390
      COMMON /FLUXN/ D(361),IACT                                        CH5  400
C                                                                       CH5  410
CFZJ055                                                       25.09.07  CH5  420
C                                                                       CH5  430
      COMMON /BUC/ BU(6,200)                                            CH5  440
C                                                                       CH5  450
      CHARACTER*4 BU                                                    CH5  460
C                                                                       CH5  470
      EQUIVALENCE(JTPE3,N6),(JTPE5,N4)                                  CH5  480
C                                                                       CH5  490
    7 FORMAT ('1')                                                      CH5  500
  186 FORMAT (30H0CONVERGENCE CRITERION NOT MET)                        CH5  510
  192 FORMAT ('  NO',6X,'ENERGY POINT',14X,'FLUX',6X,'GIVEN SOURCE',6X, CH5  520
     1 'CALC. SOURCE',9X,'NORM FLUX',7X,'NORMFLUX(U)'//(I4,6E18.6))     CH5  530
  193 FORMAT (//' THE * NORM FLUX * IS NORMALIZED TO ONE FOR THE ENERGY CH5  540
     1RANGE 0-1.86 EV CORRESPONDING TO ENERGY POINTS 1-94 ')            CH5  550
 3412 FORMAT (I5,41H ITERATIONS WERE REQUIRED FOR CONVERGENCE,'. LAMBDA-CH5  560
     1THERMAL =',E12.6)                                                 CH5  570
CFZJ059                                                       04.11.09  CH5  580
 5006 FORMAT (1H0,4A4/(5E12.4))                                         CH5  590
 5008 FORMAT (63H  NU*FISSION   TRANSPORT  ABSORPTION SCATTER OUT NEUTS/CH5  600
     1FISSION )                                                         CH5  610
 5009 FORMAT (1H1,90X,'SPECTRUM CALCULATION',I3)                        CH5  620
 5015 FORMAT (//'*** AVERAGE THERMAL ETA*F =',G15.6,' ***',' TOTAL SIGMACH5  630
     1-ABS =',G15.6,' TOTAL SIGMA-NUFIS=',G15.6)                        CH5  640
 5052 FORMAT (17H FLUX DUMP ON B3 )                                     CH5  650
 8841 FORMAT (34H0BUT HERE ARE THE FLUXES AS OF NOW/28H0GROUP      UNCONCH5  660
     1VERGED FLUX/(I5,E21.8))                                           CH5  670
C                                                                       CH5  680
C                                                                       CH5  690
      KMA = KMAT                                                        CH5  700
      NIRPI = IPRIN(7)                                                  CH5  710
      JSATZ = NIRPI + 2                                                 CH5  720
      NXT11 = JAD11(JSATZ)                                              CH5  730
      READ (NDA11,REC=NXT11) (CONC(M),M=1,KMAT)                         CH5  740
      NXT11 = NXT11 + 1                                                 CH5  750
C                                                                       CH5  760
      IF(KANN .GT. 0) CALL THERMS(CONC,NTYSP,TCELS,VF)                  CH5  770
C                                                                       CH5  780
      IF(NUTTE .GT. 0) GOTO 5000                                        CH5  790
C                                                                       CH5  800
C     THERMALIZATION                                                    CH5  810
C                                                                       CH5  820
      JSATZ = 20 + NIRPI                                                CH5  830
      NXT10 = JAD10(JSATZ)                                              CH5  840
      READ (NDA10,REC=NXT10) NEVR,NALPHA,NKER,NBG,MNBG,NRQ,TOM,WAT,HAT, CH5  850
     1 EPSI,(E(I),I=1,NEVR),((IDKER(J,I),J=1,2),I=1,10),BEG(1),LBON(1), CH5  860
     2 LBON(2),LBON(3),(NHOTD(I),I=1,KMAT),A39,B39,C39,A40,B40,C40,     CH5  870
     3 AD39R,AD40R                                                      CH5  880
      NXT10 = NXT10 + 1                                                 CH5  890
C                                                                       CH5  900
C     ZERO OUT SOME THINGS                                              CH5  910
C                                                                       CH5  920
      DO 901 I=1,NEVR                                                   CH5  930
        BBS(I) = 0.0                                                    CH5  940
        ST(I) = 0.0                                                     CH5  950
        SIGTR(I) = 0.0                                                  CH5  960
        SIGA(I) = 0.0                                                   CH5  970
  901 CONTINUE                                                          CH5  980
      NEVRPR = NEVR - 1                                                 CH5  990
      DO 1200 I=1,NEVR                                                  CH5 1000
        DO 1200 J=1,NEVR                                                CH5 1010
          EKVR(I,J) = 0.                                                CH5 1020
 1200 CONTINUE                                                          CH5 1030
C                                                                       CH5 1040
C     LOWER ENERGY BOUNDARY 5.E-04 EV AND UPPER BOUNDARY 2.05 EV        CH5 1050
C                                                                       CH5 1060
      DELTA(1) = E(2) * 0.5                                             CH5 1070
      PHU(1) = DELTA(1) / ALOG(0.5*(E(2)+E(1))/(5.E-04))                CH5 1080
      DO 65 J=2,NEVRPR                                                  CH5 1090
        DELTA(J) = (E(J+1)-E(J-1)) * 0.5                                CH5 1100
        PHU(J) = DELTA(J) / ALOG((E(J+1)+E(J))/(E(J)+E(J-1)))           CH5 1110
   65 CONTINUE                                                          CH5 1120
      DELTA(NEVR) = E(NEVR) - E(NEVRPR)                                 CH5 1130
      PHU(NEVR) = DELTA(NEVR) / ALOG((2.05)/(E(NEVR)+E(NEVRPR))*2.0)    CH5 1140
      IF(NALPHA) 20,20,11                                               CH5 1150
   11 CONTINUE                                                          CH5 1160
      AD39 = CONC(16)                                                   CH5 1170
      SSPU39 = A39 + B39 * (AD39-AD39R) + C39 * (AD39-AD39R)**2         CH5 1180
      SSPU39 = 1. / SQRT(SSPU39)                                        CH5 1190
      AD40 = CONC(17)                                                   CH5 1200
      SSPU40 = A40 + B40 * (AD40-AD40R) + C40 * (AD40-AD40R)**2         CH5 1210
      SSPU40 = 1. / SQRT(SSPU40)                                        CH5 1220
      NABS = IPRIN(14)                                                  CH5 1230
      DO 10 J=1,NALPHA                                                  CH5 1240
        READ (NDA10,REC=NXT10) IDT,(DID(I),I=1,18),ADEN,UB,DFO,SSF,     CH5 1250
     1   (SELS(I),I=1,NEVR),(XA(I),I=1,NEVR),(XS(I),I=1,NEVR),          CH5 1260
     2   (XF(I),I=1,NEVR),(XTR(I),I=1,NEVR)                             CH5 1270
        NXT10 = NXT10 + 1                                               CH5 1280
        DO 1501 I=1,NALPHA                                              CH5 1290
          MANU = IMAT(I)                                                CH5 1300
          IF(IDT-MANU) 1501,1502,1501                                   CH5 1310
 1501   CONTINUE                                                        CH5 1320
 1502   ADEN = CONC(I)                                                  CH5 1330
        IF(I .GT. NABS) GOTO 9003                                       CH5 1340
        DO 9002 I=31,41                                                 CH5 1350
          SELS(I) = SELS(I) * SSPU39                                    CH5 1360
 9002   CONTINUE                                                        CH5 1370
        DO 9005 I=73,81                                                 CH5 1380
          SELS(I) = SELS(I) * SSPU40                                    CH5 1390
 9005   CONTINUE                                                        CH5 1400
 9003   CONTINUE                                                        CH5 1410
C                                                                       CH5 1420
        IF(KANN .GT. 0) CALL ESEL(J,VF,KMA)                             CH5 1430
C                                                                       CH5 1440
        DO 2017 I=1,NEVR                                                CH5 1450
          SELS(I) = SELS(I) * ADEN                                      CH5 1460
 2017   CONTINUE                                                        CH5 1470
C                                                                       CH5 1480
C     SET-UP SIGMA-A                                                    CH5 1490
C                                                                       CH5 1500
        DO 2007 I=1,NEVR                                                CH5 1510
          SIGTR(I) = SIGTR(I) + SELS(I) * XTR(I)                        CH5 1520
          SIGA(I) = SIGA(I) + XA(I) * SELS(I)                           CH5 1530
 2007   CONTINUE                                                        CH5 1540
   10 CONTINUE                                                          CH5 1550
   20 CONTINUE                                                          CH5 1560
      DO 1206 KIM=1,NKER                                                CH5 1570
        READ (NDA10,REC=NXT10) IDT,(DID(I),I=1,18),ADEN,UB,SSF,FSX,     CH5 1580
     1   (SELS(I),I=1,NEVR),(XA(K),K=1,NEVR),(XS(K),K=1,NEVR),          CH5 1590
     2   (XTR(K),K=1,NEVR),(S(K),K=1,NEVR)                              CH5 1600
        NXT10 = NXT10 + 1                                               CH5 1610
C                                                                       CH5 1620
        CALL WRDA(IREAD,NDA10,NXT10,L10,TIM,NEVR*NEVR)                  CH5 1630
C                                                                       CH5 1640
        DO 1504 I=1,NKER                                                CH5 1650
          JI = IDKER(1,I)                                               CH5 1660
          MANU = IDKER(2,I)                                             CH5 1670
          IF(IDT-MANU) 1504,1505,1504                                   CH5 1680
 1504   CONTINUE                                                        CH5 1690
 1505   CONTINUE                                                        CH5 1700
C                                                                       CH5 1710
        IF(KANN .GT. 0) CALL ESEL(JI,VF,KMA)                            CH5 1720
C                                                                       CH5 1730
        ADEN = CONC(JI)                                                 CH5 1740
        DO 4053 I=1,NEVR                                                CH5 1750
          SELS(I) = SELS(I) * ADEN                                      CH5 1760
 4053   CONTINUE                                                        CH5 1770
        DO 4073 I=1,NEVR                                                CH5 1780
          ST(I) = ST(I) + S(I) * ADEN * FSX                             CH5 1790
          SIGTR(I) = SIGTR(I) + SELS(I) * XTR(I)                        CH5 1800
          SIGA(I) = SIGA(I) + XA(I) * SELS(I)                           CH5 1810
 4073   CONTINUE                                                        CH5 1820
        DO 1251 I=1,NEVR                                                CH5 1830
          DO 1251 J=1,NEVR                                              CH5 1840
            EKVR(I,J) = EKVR(I,J) + TIM(I,J) * SELS(I)                  CH5 1850
 1251   CONTINUE                                                        CH5 1860
 1206 CONTINUE                                                          CH5 1870
      TOM = TOM * 8.61640E-5                                            CH5 1880
      DO 8427 K=1,NEVR                                                  CH5 1890
        PHICAP(K) = 0.                                                  CH5 1900
        PHI(K) = E(K) * EXP(-E(K)/TOM)                                  CH5 1910
 8427 CONTINUE                                                          CH5 1920
C                                                                       CH5 1930
C     A FAST WAY TO FIND FLUXES                                         CH5 1940
C                                                                       CH5 1950
      DO 427 J=1,NEVR                                                   CH5 1960
        TE = EKVR(J,J) * DELTA(J)                                       CH5 1970
        DTE = -DBLE(TE)                                                 CH5 1980
        DO 425 K=1,NEVR                                                 CH5 1990
          TE = EKVR(J,K) * DELTA(K)                                     CH5 2000
          DTE = DTE + DBLE(TE)                                          CH5 2010
  425   CONTINUE                                                        CH5 2020
        TE = SNGL(DTE)                                                  CH5 2030
        EKVR(J,J) = TE + SIGA(J) + BBS(J) / (3.0*SIGTR(J))              CH5 2040
  427 CONTINUE                                                          CH5 2050
C                                                                       CH5 2060
C     SET-UP THE MATRIX TO BE INVERTED                                  CH5 2070
C                                                                       CH5 2080
      DO 301 K=1,NEVR                                                   CH5 2090
        DO 301 J=1,NEVR                                                 CH5 2100
          IF(J-K) 302,301,302                                           CH5 2110
  302     EKVR(J,K) = -EKVR(J,K) * DELTA(J)                             CH5 2120
  301 CONTINUE                                                          CH5 2130
      SOT = 0.0                                                         CH5 2140
      DO 888 K=1,NEVR                                                   CH5 2150
        GFLUX(K) = (SIGA(K)+BBS(K)/(3.0*SIGTR(K))) * DELTA(K)           CH5 2160
        SOT = SOT + ST(K) * DELTA(K)                                    CH5 2170
        S(K) = ST(K)                                                    CH5 2180
  888 CONTINUE                                                          CH5 2190
      PICA = 0.                                                         CH5 2200
      DO 180 KDY=1,400                                                  CH5 2210
        SMT = 0.0                                                       CH5 2220
        DO 171 K=1,NEVR                                                 CH5 2230
          SMT = SMT + GFLUX(K) * PHI(K)                                 CH5 2240
  171   CONTINUE                                                        CH5 2250
        CAT = SOT / SMT                                                 CH5 2260
        SAT = CAT * WAT                                                 CH5 2270
        DO 6550 K=1,NEVR                                                CH5 2280
          IF(HAT .GT. 0.) PICA = HAT * PHICAP(K)                        CH5 2290
          PHICAP(K) = PICA + PHI(K) * SAT                               CH5 2300
 6550   CONTINUE                                                        CH5 2310
        DO 6551 K=1,NEVR                                                CH5 2320
          TE = EKVR(K,K) * PHICAP(K)                                    CH5 2330
          DTE = -DBLE(TE)                                               CH5 2340
          DO 172 J=1,NEVR                                               CH5 2350
            TE = EKVR(J,K) * PHICAP(J)                                  CH5 2360
            DTE = DTE + DBLE(TE)                                        CH5 2370
  172     CONTINUE                                                      CH5 2380
          TE = SNGL(DTE)                                                CH5 2390
          PHI(K) = (-TE+S(K)) / EKVR(K,K)                               CH5 2400
 6551   CONTINUE                                                        CH5 2410
        DO 377 J=1,NEVR                                                 CH5 2420
          JX = NEVR + 1 - J                                             CH5 2430
          IF(ABS(PHI(JX)/PHICAP(JX)-1.0)-EPSI) 377,180,180              CH5 2440
  377   CONTINUE                                                        CH5 2450
        GOTO 190                                                        CH5 2460
  180 CONTINUE                                                          CH5 2470
      WRITE (N6,186)                                                    CH5 2480
      WRITE (N6,8841) (M,PHI(M),M=1,NEVR)                               CH5 2490
C                                                                       CH5 2500
      CALL EXIT                                                         CH5 2510
C                                                                       CH5 2520
C     CALCULATE THE SOURCES FOR THESE FLUXES                            CH5 2530
C                                                                       CH5 2540
  190 DO 203 I=1,NEVR                                                   CH5 2550
        TE = 0.0                                                        CH5 2560
        DO 204 K=1,NEVR                                                 CH5 2570
          TE = TE + EKVR(K,I) * PHI(K)                                  CH5 2580
  204   CONTINUE                                                        CH5 2590
        ST(I) = TE                                                      CH5 2600
  203 CONTINUE                                                          CH5 2610
      IF(IPRIN(5)) 5050,5051,5050                                       CH5 2620
 5050 WRITE (N4) (PHI(I),I=1,NEVR)                                      CH5 2630
      WRITE (N6,5052)                                                   CH5 2640
 5051 CONTINUE                                                          CH5 2650
      WRITE (N6,3412) KDY,CAT                                           CH5 2660
C                                                                       CH5 2670
C     CALCULATE AVERAGE CROSS SECTIONS AND ARRANGE TRANSFER MATRIX      CH5 2680
C                                                                       CH5 2690
      CALL AVGS(IDKER,IMAT,OUSIG,LA0,TOSIG,ABSIG,FISIG,LF0,XNU,VF)      CH5 2700
C                                                                       CH5 2710
      RN = 1. / VEC(1)                                                  CH5 2720
      DO 5500 I=1,NEVR                                                  CH5 2730
        FINU(I) = PHI(I) * RN                                           CH5 2740
        PHU(I) = PHU(I) * FINU(I)                                       CH5 2750
 5500 CONTINUE                                                          CH5 2760
      IF(IPRIN(1) .LT. 2) GOTO 5555                                     CH5 2770
      WRITE (N6,7)                                                      CH5 2780
      WRITE (N6,192) (I,E(I),PHI(I),S(I),ST(I),FINU(I),PHU(I),I=1,NEVR) CH5 2790
      WRITE (N6,193)                                                    CH5 2800
 5555 CONTINUE                                                          CH5 2810
      IF(IPRIN(5) .NE. 0) WRITE (N4) (FINU(I),I=1,NEVR)                 CH5 2820
 5000 CONTINUE                                                          CH5 2830
      IF(IPRIN(1)-2) 5004,5004,5005                                     CH5 2840
 5005 WRITE (N6,5009) IPRIN(7)                                          CH5 2850
      WRITE (N6,5008)                                                   CH5 2860
      XNULL = 0.0                                                       CH5 2870
      AR = 0.                                                           CH5 2880
      FR = 0.                                                           CH5 2890
      NFP = 10                                                          CH5 2900
      KJ = 1                                                            CH5 2910
      DO 5007 KI=1,KMAT                                                 CH5 2920
        IF(KJ-NFP) 5010,5010,5011                                       CH5 2930
 5011   NFP = NFP + 10                                                  CH5 2940
        WRITE (N6,5008)                                                 CH5 2950
 5010   CONTINUE                                                        CH5 2960
        IF(IPRIN(1) .EQ. 3 .AND. CONC(KI) .EQ. 0.) GOTO 5007            CH5 2970
        K = ((NIRPI-1)*KMAT+KI-1) * N26                                 CH5 2980
        IF(KI .GT. IACT) GOTO 5012                                      CH5 2990
        N = ((NIRPI-1)*IACT+KI-1) * N26                                 CH5 3000
CFZJ059                                                       04.11.09  CH5 3010
        WRITE (N6,5006) (BU(I,KI),I=1,4),(FISIG(I+N),TOSIG(I+K),        CH5 3020
     1   ABSIG(I+K),OUSIG(I+K),XNU(I+N),I=1,N26)                        CH5 3030
        FR = FR + FISIG(N26+N) * CONC(KI)                               CH5 3040
        GOTO 5016                                                       CH5 3050
 5012   CONTINUE                                                        CH5 3060
CFZJ059                                                       04.11.09  CH5 3070
        WRITE (N6,5006) (BU(I,KI),I=1,4),(XNULL,TOSIG(I+K),ABSIG(I+K),  CH5 3080
     1   OUSIG(I+K),XNULL,I=1,N26)                                      CH5 3090
 5016   AR = AR + ABSIG(N26+K) * CONC(KI)                               CH5 3100
        KJ = KJ + 1                                                     CH5 3110
 5007 CONTINUE                                                          CH5 3120
      ETAF = FR / AR                                                    CH5 3130
      WRITE (N6,5015) ETAF,AR,FR                                        CH5 3140
 5004 CONTINUE                                                          CH5 3150
      IF(IPRIN(7)-NXS) 5600,5601,5601                                   CH5 3160
 5601 CONTINUE                                                          CH5 3170
      MAFIA = 6                                                         CH5 3180
      IPRIN(7)=0                                                        CH5 3190
      RETURN                                                            CH5 3200
 5600 CONTINUE                                                          CH5 3210
      MAFIA = 4                                                         CH5 3220
      RETURN                                                            CH5 3230
      END                                                               CH5 3240
      SUBROUTINE AVGS(IDKER,IMAT,OUSIG,LA0,TOSIG,ABSIG,FISIG,LF0,XNU,VF)AVG   10
C                                                                       AVG   20
C     INTEGRATE FLUXES FOR BROAD GROUPS                                 AVG   30
C                                                                       AVG   40
      DIMENSION E(96),S(96),SIGA(96),DELTA(96),GFLUX(96),PHI(96),XA(96),AVG   50
     1 XF(96),XS(96),SIGN(96),SIGTR(96),SIGS(96),SELS(97),DID(18),      AVG   60
     2 BEG(51),LBON(51),NDIV(96),VEC(5),TIN(50),XTR(96),FINU(96),       AVG   70
     3 EKVR(96,96),TIM(96,96),DIF(6,50),IDKER(2,10),FINT(51),XIGA(96),  AVG   80
     4 XIGS(96),XIGN(96),XIGTR(96),ZIGA(51),ZIGS(51),ZIGN(51),ZIGTR(51),AVG   90
     5 IMAT(KMAT),OUSIG(LA0),TOSIG(LA0),ABSIG(LA0),FISIG(LF0),XNU(LF0), AVG  100
     6 VF(KMAT,10)                                                      AVG  110
C                                                                       AVG  120
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    AVG  130
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    AVG  140
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIAVG  150
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 AVG  160
C                                                                       AVG  170
      EQUIVALENCE(JTPE3,N6)                                             AVG  180
C                                                                       AVG  190
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), AVG  200
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10AVG  210
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11AVG  220
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         AVG  230
C                                                                       AVG  240
      COMMON /BLOCKT/ DELT,NOIT,TIN,SIGA,SIGN,SIGS,SIGTR,EKVR,PHI,E,BN, AVG  250
     1 SELS,DID,BEG,LBON,NDIV,DIF,TIM,FMIC,NEVR,NBG,NALPHA,NKER,MNBG,   AVG  260
     2 FINU,DELTA,KDIV,XA,XS,XF,XTR,S,T,UB,DFO,IDT,VEC                  AVG  270
C                                                                       AVG  280
      COMMON /TERFLU/ KANN,NUTTE,NEUSPK,NSAEFL,A39,B39,C39,A40,B40,C40, AVG  290
     1 AD39,AD40,AD39R,AD40R,SSPU39,SSPU40                              AVG  300
C                                                                       AVG  310
      COMMON /FLUXN/ D(361),IACT                                        AVG  320
C                                                                       AVG  330
CFZJ055                                                       25.09.07  AVG  340
C                                                                       AVG  350
C                                                                       AVG  360
      KMA = KMAT                                                        AVG  370
      JSATZ = 20 + IPRIN(7)                                             AVG  380
      NXT10 = JAD10(JSATZ)                                              AVG  390
      READ (NDA10,REC=NXT10) DUMY                                       AVG  400
      NXT10 = NXT10 + 1                                                 AVG  410
      DELTA(1) = E(1) * 0.5                                             AVG  420
      GFLUX(1) = PHI(1)                                                 AVG  430
      DO 107 I=2,NEVR                                                   AVG  440
        DELTA(I) = (E(I)-E(I-1)) * 0.5                                  AVG  450
        GFLUX(I) = PHI(I)                                               AVG  460
  107 CONTINUE                                                          AVG  470
C                                                                       AVG  480
C     INTEGRATE FLUXES                                                  AVG  490
C                                                                       AVG  500
      CALL GROUP(GFLUX,FINT)                                            AVG  510
C                                                                       AVG  520
      VEC(1) = FINT(1)                                                  AVG  530
C                                                                       AVG  540
C     FIND MICRO. AVERAGES / MICROSCOPIC AVERAGING / AVERAGE ABSORBERS  AVG  550
C                                                                       AVG  560
      IA = 0                                                            AVG  570
      DO 4001 KAP=1,NALPHA                                              AVG  580
        READ (NDA10,REC=NXT10) IDT,(DID(I),I=1,18),DUMY,UB,DFO,SSF,     AVG  590
     1   (SELS(I),I=1,NEVR),(SIGA(I),I=1,NEVR),(SIGS(I),I=1,NEVR),      AVG  600
     2   (SIGN(I),I=1,NEVR),(XTR(I),I=1,NEVR)                           AVG  610
        NXT10 = NXT10 + 1                                               AVG  620
        DO 9002 I=31,41                                                 AVG  630
          SELS(I) = SELS(I) * SSPU39                                    AVG  640
 9002   CONTINUE                                                        AVG  650
        DO 9005 I=73,81                                                 AVG  660
          SELS(I) = SELS(I) * SSPU40                                    AVG  670
 9005   CONTINUE                                                        AVG  680
        IA = IA + 1                                                     AVG  690
C                                                                       AVG  700
        IF(KANN .GT. 0) CALL ESEL(IA,VF,KMA)                            AVG  710
C                                                                       AVG  720
CFZJ046                                                       15.07.09  AVG  730
        IF(DFO) 3901,3901,3903                                          AVG  740
 3901   CONTINUE                                                        AVG  750
        DO 3906 I=1,NEVR                                                AVG  760
          BOB = SELS(I) * PHI(I)                                        AVG  770
          XIGA(I) = SIGA(I) * BOB                                       AVG  780
          XIGS(I) = SIGS(I) * BOB                                       AVG  790
 3906   CONTINUE                                                        AVG  800
C                                                                       AVG  810
C     NOW AVERAGE                                                       AVG  820
C                                                                       AVG  830
        CALL GROUP(XIGA,ZIGA)                                           AVG  840
C                                                                       AVG  850
        CALL GROUP(XIGS,ZIGS)                                           AVG  860
C                                                                       AVG  870
        DO 3907 I=1,NBG                                                 AVG  880
          SIGA(I) = ZIGA(I) / FINT(I)                                   AVG  890
          SIGS(I) = ZIGS(I) / FINT(I)                                   AVG  900
 3907   CONTINUE                                                        AVG  910
        DO 3922 I=1,NBG                                                 AVG  920
          XF(I) = 0.0                                                   AVG  930
CFZJ046                                                       15.07.09  AVG  940
 3922   CONTINUE                                                        AVG  950
        DFO = 0.0                                                       AVG  960
        GOTO 4002                                                       AVG  970
 3903   CONTINUE                                                        AVG  980
        DO 3909 I=1,NEVR                                                AVG  990
          BOB = SELS(I) * PHI(I)                                        AVG 1000
          XIGA(I) = SIGA(I) * BOB                                       AVG 1010
          XIGS(I) = SIGS(I) * BOB                                       AVG 1020
          XIGN(I) = SIGN(I) * BOB                                       AVG 1030
 3909   CONTINUE                                                        AVG 1040
C                                                                       AVG 1050
C     NOW AVERAGE                                                       AVG 1060
C                                                                       AVG 1070
        CALL GROUP(XIGA,ZIGA)                                           AVG 1080
C                                                                       AVG 1090
        CALL GROUP(XIGS,ZIGS)                                           AVG 1100
C                                                                       AVG 1110
        CALL GROUP(XIGN,ZIGN)                                           AVG 1120
C                                                                       AVG 1130
        DO 3913 I=1,NBG                                                 AVG 1140
          SIGA(I) = ZIGA(I) / FINT(I)                                   AVG 1150
          SIGS(I) = ZIGS(I) / FINT(I)                                   AVG 1160
          SIGN(I) = ZIGN(I) / FINT(I)                                   AVG 1170
 3913   CONTINUE                                                        AVG 1180
        DO 3927 I=1,NBG                                                 AVG 1190
          XA(I) = SIGA(I) - SIGN(I)                                     AVG 1200
          XF(I) = DFO * SIGN(I)                                         AVG 1210
CFZJ046                                                       15.07.09  AVG 1220
 3927   CONTINUE                                                        AVG 1230
C                                                                       AVG 1240
C     ARRANGE SIGMAS IN ORDER OF V.S.O.P. LIBRARY                       AVG 1250
C                                                                       AVG 1260
 4002   CONTINUE                                                        AVG 1270
        DO 4003 I=1,KMAT                                                AVG 1280
          JP = I                                                        AVG 1290
          MANU = IMAT(I)                                                AVG 1300
          IF(MANU .EQ. IDT) GOTO 4004                                   AVG 1310
 4003   CONTINUE                                                        AVG 1320
 4004   CONTINUE                                                        AVG 1330
        I = N26                                                         AVG 1340
        JAP = ((IPRIN(7)-1)*KMAT+JP-1) * N26                            AVG 1350
        TOSIG(I+JAP) = SIGTR(1)                                         AVG 1360
        ABSIG(I+JAP) = SIGA(1)                                          AVG 1370
        OUSIG(I+JAP) = 0.0                                              AVG 1380
        IF(JP .GT. IACT) GOTO 4001                                      AVG 1390
        JFI = ((IPRIN(7)-1)*IACT+JP-1) * N26                            AVG 1400
        FISIG(I+JFI) = XF(1)                                            AVG 1410
        XNU(I+JFI) = DFO                                                AVG 1420
 4001 CONTINUE                                                          AVG 1430
C                                                                       AVG 1440
C     AVERAGE KERNEL BEARING QUANTITIES                                 AVG 1450
C                                                                       AVG 1460
      DO 7001 JAN=1,NKER                                                AVG 1470
        READ (NDA10,REC=NXT10) IDT,(DID(I),I=1,18),DUMY,UB,SSF,FSX,     AVG 1480
     1   (SELS(I),I=1,NEVR),(SIGA(I),I=1,NEVR),(SIGS(I),I=1,NEVR),      AVG 1490
     2   (SIGTR(I),I=1,NEVR),(XS(K),K=1,NEVR)                           AVG 1500
        NXT10 = NXT10 + 1                                               AVG 1510
C                                                                       AVG 1520
        CALL WRDA(IREAD,NDA10,NXT10,L10,TIM,NEVR*NEVR)                  AVG 1530
C                                                                       AVG 1540
        IA = IA + 1                                                     AVG 1550
C                                                                       AVG 1560
        IF(KANN .GT. 0) CALL ESEL(IA,VF,KMA)                            AVG 1570
C                                                                       AVG 1580
        DO 5211 J=1,NEVR                                                AVG 1590
          BOB = PHI(J) * SELS(J)                                        AVG 1600
          XIGA(J) = SIGA(J) * BOB                                       AVG 1610
          XIGS(J) = SIGS(J) * BOB                                       AVG 1620
          XIGTR(J) = SIGTR(J) * BOB                                     AVG 1630
 5211   CONTINUE                                                        AVG 1640
C                                                                       AVG 1650
        CALL GROUP(XIGA,ZIGA)                                           AVG 1660
C                                                                       AVG 1670
        CALL GROUP(XIGS,ZIGS)                                           AVG 1680
C                                                                       AVG 1690
        CALL GROUP(XIGTR,ZIGTR)                                         AVG 1700
C                                                                       AVG 1710
        DO 5213 J=1,NBG                                                 AVG 1720
          SIGA(J) = ZIGA(J) / FINT(J)                                   AVG 1730
          SIGS(J) = ZIGS(J) / FINT(J)                                   AVG 1740
          SIGTR(J) = ZIGTR(J) / FINT(J)                                 AVG 1750
 5213   CONTINUE                                                        AVG 1760
        DO 365 J=1,MNBG                                                 AVG 1770
          XA(J) = 0.0                                                   AVG 1780
          XF(J) = SIGA(J)                                               AVG 1790
  365   CONTINUE                                                        AVG 1800
        DO 7002 I=1,NKER                                                AVG 1810
          JP = IDKER(1,I)                                               AVG 1820
          MANU = IDKER(2,I)                                             AVG 1830
          IF(MANU .EQ. IDT) GOTO 7003                                   AVG 1840
 7002   CONTINUE                                                        AVG 1850
 7003   CONTINUE                                                        AVG 1860
        JAP = ((IPRIN(7)-1)*KMAT+JP-1) * N26                            AVG 1870
        I = N26                                                         AVG 1880
        TOSIG(I+JAP) = SIGTR(1)                                         AVG 1890
        ABSIG(I+JAP) = SIGA(1)                                          AVG 1900
        OUSIG(I+JAP) = 0.0                                              AVG 1910
 7001 CONTINUE                                                          AVG 1920
      RETURN                                                            AVG 1930
      END                                                               AVG 1940
      SUBROUTINE GROUP(RAT,GFLUX)                                       GRO   10
C                                                                       GRO   20
C     DIMENSIONS                                                        GRO   30
C                                                                       GRO   40
      DIMENSION E(96),S(96),SIGA(96),DELTA(96),GFLUX(51),PHI(96),XA(96),GRO   50
     1 XF(96),XS(96),SIGN(96),SIGTR(96),SIGS(96),SELS(97),DID(18),      GRO   60
     2 BEG(51),LBON(51),NDIV(96),VEC(5),TIN(50),XTR(96),FINU(96),       GRO   70
     3 EKVR(96,96),TIM(96,96),DIF(6,50),PHICAP(96),RAT(96)              GRO   80
C                                                                       GRO   90
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    GRO  100
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    GRO  110
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIGRO  120
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 GRO  130
C                                                                       GRO  140
      COMMON /BLOCKT/ DELT,NOIT,TIN,SIGA,SIGN,SIGS,SIGTR,EKVR,PHI,E,BN, GRO  150
     1 SELS,DID,BEG,LBON,NDIV,DIF,TIM,FMIC,NEVR,NBG,NALPHA,NKER,MNBG,   GRO  160
     2 FINU,DELTA,KDIV,XA,XS,XF,XTR,S,T,UB,DFO,IDT,VEC                  GRO  170
C                                                                       GRO  180
C                                                                       GRO  190
      PHICAP(1) = RAT(1) * DELTA(1)                                     GRO  200
      DO 201 K=2,NEVR                                                   GRO  210
        PHICAP(K) = (RAT(K-1)+RAT(K)) * DELTA(K)                        GRO  220
  201 CONTINUE                                                          GRO  230
C                                                                       GRO  240
C     GROUP THE INTEGRATED FINE GROUPS / GET GROUP FLUXES               GRO  250
C                                                                       GRO  260
      DO 2271 K=1,MNBG                                                  GRO  270
        JS = LBON(K)                                                    GRO  280
        JF = LBON(K+1) - 1                                              GRO  290
        GFLUX(K) = 0.0                                                  GRO  300
        DO 2268 J=JS,JF                                                 GRO  310
          IF(PHICAP(J) .LT. 1.0E-38) GOTO 2268                          GRO  320
          GFLUX(K) = GFLUX(K) + PHICAP(J)                               GRO  330
 2268   CONTINUE                                                        GRO  340
 2271 CONTINUE                                                          GRO  350
      RETURN                                                            GRO  360
      END                                                               GRO  370
      SUBROUTINE THERMS(CONC,NTYSP,TCELS,VF)                            HER   10
C                                                                       HER   20
C     P U  -  T H E R M O S                                             HER   30
C                                                                       HER   40
      REAL*8 COD(10)                                                    HER   50
C                                                                       HER   60
      DOUBLE PRECISION RNEWD,RNI,QRATD                                  HER   70
C                                                                       HER   80
CFZJ046                                                       26.09.06  HER   90
      DIMENSION VVH(10),COP(2),COA(10),XF(30),XT(30),XS(30),XA(30),     HER  100
     1 SP(30),CONCTA(10),XAM(10,30),XTM(10,30),S(10,30),H(20,30),       HER  110
     2 R(20,30),Q(20,30),PP(30,30),RO(20),RN(20),BETA(21),FNA(100),     HER  120
     3 FNB(100),JKER(100),IKER(5,20),NSRK(5),TCLIB(100),JDL10(100),     HER  130
     4 G(1200),SFT(50,30),CONC(KMAT),NTYSP(NXS),TCELS(5,NXS),VF(KMAT,10)HER  140
     5 ,IDSELF(200),RI(20)                                              HER  150
C                                                                       HER  160
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    HER  170
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    HER  180
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIHER  190
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 HER  200
C                                                                       HER  210
      COMMON /BLOCKT/ T(20,20,30),P(30,30,10)                           HER  220
C                                                                       HER  230
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), HER  240
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10HER  250
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11HER  260
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         HER  270
C                                                                       HER  280
      COMMON /BLOCRT/ ID,NBER,IX,MX,NX,ICOAT,MY,IBRENN,SS96(96,10),     HER  290
     1 F(20,30),MTBL(20),VOL(20),V(30),DV(30),VV(30),VDV(30),MG(30),    HER  300
     2 COZ(30,2),GIM(30,10),ALBEDO(30),LEAKT                            HER  310
C                                                                       HER  320
      COMMON /TERFLU/ KANN,NUTTE,NEUSPK,NSAEFL,A39,B39,C39,A40,B40,C40, HER  330
     1 AD39,AD40,AD39R,AD40R,SSPU39,SSPU40                              HER  340
C                                                                       HER  350
CFZJ035                                                       14.09.04  HER  360
CFZJ063                                                       26.07.11  HER  370
      COMMON /GRENZE/ NURTHM,THERGR,IDGAM,IDZUT,IDTHER,NGAM,IDESIN,JJGM,HER  380
     1 MSTU,MGHUS,NNOBG,NZT(10,9),IZUT(10,2,10),SIRA(68),NIRPI,NID,NDA30HER  390
     2 ,NKER                                                            HER  400
C                                                                       HER  410
      COMMON /VARDIM/ A(8000000)                                        HER  420
C                                                                       HER  430
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       HER  440
C                                                                       HER  450
CFZJ055                                                       25.09.07  HER  460
CFZJ046                                                       26.09.06  HER  470
C                                                                       HER  480
      EQUIVALENCE(G(1),R(1,1)),(G(601),Q(1,1)),(JTPE3,N6,NTOUT),(LI(1), HER  490
     1 LIMAT),(LI(3),LOUSI),(LI(4),LTOSI),(LI(5),LABSI),(LI(6),LFISI),  HER  500
     2 (LI(7),LXNU ),(LI(25),LZLTH),(LI(145),LCOSS)                     HER  510
C                                                                       HER  520
    2 FORMAT (/2I6/3(10E12.5/))                                         HER  530
  100 FORMAT (///40X,'T H E R M O S   SPECTRUM CALCULATION',20X,'SPECTRUHER  540
     1M ZONE',I4,//' ATOM DENSITIES ZONES NO  1 -',I2/)                 HER  550
  103 FORMAT (2I5,10E12.5)                                              HER  560
  361 FORMAT (' ... JSATZ ',I4,' ON DATA SET ',I4,' STARTS ON RECORD NO'HER  570
     1 ,I6,' ... ')                                                     HER  580
 1035 FORMAT (13H LARGEST RES= ,E12.5,13H    MEAN RES= ,E12.5,9H   N(V*)HER  590
     1= ,E12.5)                                                         HER  600
 2020 FORMAT (7H1ITCNT=,I5,8H RENORM=,E12.5,5H EPS= ,E12.5)             HER  610
 2030 FORMAT (11H0I,N,F(N,I))                                           HER  620
 2031 FORMAT (7(2I3,E12.4))                                             HER  630
 9909 FORMAT ('0NUMBER OF THERMOS-ITERATIONS:',I6,' CONVERGENCE:',E12.5,HER  640
     1 ' ALBEDO:',E12.5)                                                HER  650
99987 FORMAT (' FOR THE NUCLIDE',I3,', TEMPERATURE=',E12.5,' CELSIUS, THHER  660
     1ERM.SIGM. ARE INTERPOLATED BETWEEN THE SETS',I5,' AND',I5)        HER  670
99988 FORMAT (' MATERIAL ....',I5,' USES SCATTERING MATRIX NO',I6)      HER  680
99992 FORMAT (30(I4,2X,2E14.6/))                                        HER  690
99993 FORMAT (//' SELFSHIELDING FACTORS  FOR COATED PARTICLES'//' GROUP HER  700
     1       KERNEL       COATING')                                     HER  710
C                                                                       HER  720
C                                                                       HER  730
      IPR1 = IPRIN(1)                                                   HER  740
      IPR7 = IPRIN(7)                                                   HER  750
      IF(IPR7 .EQ. 1) NEUSPK = NEUSPK + 1                               HER  760
      JSATZ = 17                                                        HER  770
      NXT10 = JAD10(JSATZ)                                              HER  780
CFZJ012   New identification numbers for THERMOS-cell         02.12.03  HER  790
C         definitions for the spectrum zones                  02.12.03  HER  800
      READ (NDA10,REC=NXT10) NBER,(NTYSP(I),I=1,NXS)                    HER  810
      NXT10 = NXT10 + 1                                                 HER  820
   10 CONTINUE                                                          HER  830
      READ (NDA10,REC=NXT10) NBE                                        HER  840
      IF(NBE .LT. 0) GOTO 11                                            HER  850
      READ (NDA10,REC=NXT10) NBE,NGEOM,TKG,NX,MX,MY,MTBL,BETA,VVH,IBRENNHER  860
     1 ,ICOAT,COA,FNA,FNB,RI,RN,RO,VOL,DH,NORMP,NFU,NSTT0,LEAKT,ALBEDO  HER  870
      NXT10 = NXT10 + 1                                                 HER  880
C                                                                       HER  890
      CALL WRDA(IREAD,NDA10,NXT10,L10,VF,KMAT*10)                       HER  900
C                                                                       HER  910
      LT = 1                                                            HER  920
      GOTO 12                                                           HER  930
   11 CONTINUE                                                          HER  940
      READ (NDA10,REC=NXT10) NBE,NGEOM,TKG,NX,MX,MY,MTBL,BETA,VVH,IBRENNHER  950
     1 ,ICOAT,COA,FNA,FNB,RI,RN,RO,VOL,DH,NORMP,NFU,NSTT0,LEAKT,ALBEDO, HER  960
     2 LJ,LT,(IDSELF(N),N=1,KMAT),((SFT(L,J),J=1,LT),L=1,LJ)            HER  970
      NXT10 = NXT10 + 1                                                 HER  980
C                                                                       HER  990
      CALL WRDA(IREAD,NDA10,NXT10,L10,VF,KMAT*10)                       HER 1000
C                                                                       HER 1010
      NBE = IABS(NBE)                                                   HER 1020
   12 CONTINUE                                                          HER 1030
      IF(NBER .GT. 1 .AND. NBE .NE. NTYSP(IPR7)) GOTO 10                HER 1040
      JSATZ = 16                                                        HER 1050
      NXT10 = JAD10(JSATZ)                                              HER 1060
      READ (NDA10,REC=NXT10) NSTR10,NSTRDL,MERK2,KKER,(TCLIB(K),JDL10(K)HER 1070
     1 ,K=1,KKER),NUCT                                                  HER 1080
      NXT10 = NXT10 + 1                                                 HER 1090
      READ (NDA10,REC=NXT10) IX,(V(I),DV(I),VV(I),VDV(I),MG(I),I=1,IX), HER 1100
     1 NABS,NKER,NCL,((IKER(I,J),I=1,NKER),J=1,NCL),(JKER(K),K=1,KKER), HER 1110
     2 NIS,TKELV                                                        HER 1120
      NXT10 = NXT10 + 1                                                 HER 1130
      IF(IPRIN(1) .GE. 3) WRITE (N6,100) IPR7,MX                        HER 1140
      DO 19 M=1,10                                                      HER 1150
        DO 19 J=1,IX                                                    HER 1160
          XAM(M,J) = 0.0                                                HER 1170
          XTM(M,J) = 0.0                                                HER 1180
          S(M,J) = 0.0                                                  HER 1190
          DO 19 I=1,IX                                                  HER 1200
            P(I,J,M) = 0.0                                              HER 1210
   19 CONTINUE                                                          HER 1220
      FUE = 0.                                                          HER 1230
      DO 20 M=1,MX                                                      HER 1240
        FUE = FUE + VVH(M)                                              HER 1250
   20 CONTINUE                                                          HER 1260
      VODFL = RO(NX) / (FUE*(NGEOM+2))                                  HER 1270
      IF(ICOAT .LE. 0) GOTO 161                                         HER 1280
      BET1 = COA(3) * (COA(1)/COA(2))**3                                HER 1290
      BET2 = COA(3) + COA(4) * (1.-COA(3)) - BET1                       HER 1300
  161 CONTINUE                                                          HER 1310
      NN10 = NXT10                                                      HER 1320
      NKE = 0                                                           HER 1330
      DO 649 N=1,KMAT                                                   HER 1340
        NKM = 0                                                         HER 1350
        IF(N .LE. NABS) GOTO 22                                         HER 1360
        NKE = NKE + 1                                                   HER 1370
        TC = TCELS(NKE,IPR7)                                            HER 1380
        TMIN = -TKELV                                                   HER 1390
        TMAX = 10000.                                                   HER 1400
        KMIN = 0                                                        HER 1410
        KMAX = 0                                                        HER 1420
        TKE = 0.                                                        HER 1430
        DO 29 J=1,1050                                                  HER 1440
          G(J) = 0.                                                     HER 1450
   29   CONTINUE                                                        HER 1460
        DO 31 J=1,NCL                                                   HER 1470
          DO 31 K=1,KKER                                                HER 1480
            IF(IKER(NKE,J) .NE. JKER(K)) GOTO 31                        HER 1490
            TCK = TCLIB(K)                                              HER 1500
            IF(TCK .GT. TC .OR. TCK .LE. TMIN) GOTO 30                  HER 1510
            KMIN = K                                                    HER 1520
            TMIN = TCK                                                  HER 1530
   30       CONTINUE                                                    HER 1540
            IF(TCK .LT. TC .OR. TCK .GE. TMAX) GOTO 31                  HER 1550
            KMAX = K                                                    HER 1560
            TMAX = TCK                                                  HER 1570
   31   CONTINUE                                                        HER 1580
        IF(KMIN .EQ. 0) KMIN = KMAX                                     HER 1590
        IF(KMAX .EQ. 0) KMAX = KMIN                                     HER 1600
        TT = TCLIB(KMAX) - TCLIB(KMIN)                                  HER 1610
        IF(TT .NE. 0.) TT = (TC-TCLIB(KMIN)) / TT                       HER 1620
        NXT10 = JDL10(KMIN)                                             HER 1630
        NKM = 1                                                         HER 1640
   22   CONTINUE                                                        HER 1650
        READ (NDA10,REC=NXT10) NN,IDT,IY,L,(XA(I),I=1,IX),(XS(I),I=1,IX)HER 1660
     1   ,(SP(I),I=1,IX),((PP(I,J),J=1,IY),I=1,IY),(XF(I),I=1,IX),      HER 1670
     2   (XT(I),I=1,IX),DFO,UB,TK                                       HER 1680
        NXT10 = NXT10 + 1                                               HER 1690
CFZJ046                                                       26.09.06  HER 1700
        IF(IPRIN(1) .EQ. 4) WRITE(N6,2) NN,IDT,(XA(I),I=1,IX)           HER 1710
        IF(N .GT. NABS) L = NABS + NKE                                  HER 1720
        IF(LT .LE. 1) GOTO 44                                           HER 1730
        LL = IDSELF(N)                                                  HER 1740
        IF(LL .GT. 0) GOTO 41                                           HER 1750
        GOTO 44                                                         HER 1760
   41   CONTINUE                                                        HER 1770
        DO 42 I=1,LT                                                    HER 1780
          C = SFT(LL,I)                                                 HER 1790
          XA(I) = XA(I) * C                                             HER 1800
          XS(I) = XS(I) * C                                             HER 1810
CFZJ046                                                       26.09.06  HER 1820
          XT(I) = XT(I) * C                                             HER 1830
          IF(IY .LT. IX) GOTO 42                                        HER 1840
          DO 43 J=1,IY                                                  HER 1850
            PP(I,J) = PP(I,J) * C                                       HER 1860
   43     CONTINUE                                                      HER 1870
   42   CONTINUE                                                        HER 1880
   44   CONTINUE                                                        HER 1890
        NKM = NKM + 1                                                   HER 1900
        IF(NKM .LE. 1) GOTO 35                                          HER 1910
        IX2 = IX * 2                                                    HER 1920
        IX4 = IX * (IX+3)                                               HER 1930
        IX5 = IX + IX4                                                  HER 1940
        TT1 = TT                                                        HER 1950
        IF(NKM .GT. 2) GOTO 33                                          HER 1960
        NXT10 = JDL10(KMAX)                                             HER 1970
        TT1 = 1.                                                        HER 1980
        ID1 = IDT                                                       HER 1990
   33   CONTINUE                                                        HER 2000
        TT2 = 1. - TT1                                                  HER 2010
        DO 34 I=1,IX                                                    HER 2020
          G(I) = TT1 * XA(I) + TT2 * G(I)                               HER 2030
          G(I+IX) = TT1 * XS(I) + TT2 * G(I+IX)                         HER 2040
          G(I+IX2) = TT1 * SP(I) + TT2 * G(I+IX2)                       HER 2050
          G(I+IX4) = TT1 * XF(I) + TT2 * G(I+IX4)                       HER 2060
          G(I+IX5) = TT1 * XT(I) + TT2 * G(I+IX5)                       HER 2070
          IX3 = IX * (I+2)                                              HER 2080
          DO 34 J=1,IX                                                  HER 2090
            G(J+IX3) = TT1 * PP(I,J) + TT2 * G(J+IX3)                   HER 2100
   34   CONTINUE                                                        HER 2110
        TKE = TT1 * TK + TT2 * TKE                                      HER 2120
        IF(NKM .EQ. 2) GOTO 22                                          HER 2130
        ID2 = IDT                                                       HER 2140
        IDT = NKE                                                       HER 2150
        IX5 = IX5 + IX                                                  HER 2160
        NXT10 = MERK2 + (NKE-1) * NSTRDL                                HER 2170
        NSRK(NKE) = NXT10                                               HER 2180
        IDT = NKE                                                       HER 2190
        WRITE (NDA10,REC=NXT10) NN,IDT,IY,L,(G(I),I=1,IX5),DFO,UB,TKE   HER 2200
        NXT10 = NXT10 + 1                                               HER 2210
        IF(IPRIN(1) .LT. 3) GOTO 9901                                   HER 2220
        WRITE (N6,99987) L,TC,ID1,ID2                                   HER 2230
 9901   CONTINUE                                                        HER 2240
        NKM = 0                                                         HER 2250
        NXT10 = NSRK(NKE)                                               HER 2260
        GOTO 22                                                         HER 2270
   35   CONTINUE                                                        HER 2280
        IF(N .GT. NABS .AND. IPRIN(1) .GE. 3) WRITE (N6,99988) L,IDT    HER 2290
        IF(NKE .EQ. NSTT0 .AND. NSTT0 .GT. 0) TKG = TKG * TK / TKELV    HER 2300
        DO 7 M=1,MY                                                     HER 2310
          CONCTA(M) = 0.                                                HER 2320
    7   CONTINUE                                                        HER 2330
        DO 6 M=1,MX                                                     HER 2340
          IF(VF(N,M) .GT. 0.) CONCTA(M) = CONC(L) * VF(N,M) / VVH(M)    HER 2350
    6   CONTINUE                                                        HER 2360
        CONZ2 = 0.                                                      HER 2370
        IF(ICOAT .LE. 0 .OR. VF(N,MY) .GE. 1.) GOTO 429                 HER 2380
        CONZ2 = CONCTA(ICOAT) * (1.-VF(N,MY)) / BET2                    HER 2390
        CONCTA(MY) = CONZ2                                              HER 2400
  429   CONTINUE                                                        HER 2410
C                                                                       HER 2420
C     MAKROSKOPISCHE  W - Q                                             HER 2430
C                                                                       HER 2440
        DO 450 M=1,MX                                                   HER 2450
          IF(CONCTA(M)) 430,450,430                                     HER 2460
  430     DO 440 J=1,IX                                                 HER 2470
            XAM(M,J) = XAM(M,J) + CONCTA(M) * XA(J)                     HER 2480
            XTM(M,J) = XTM(M,J) + CONCTA(M) * (XS(J)+XA(J))             HER 2490
            S(M,J) = S(M,J) + CONCTA(M) * SP(J) / V(J)                  HER 2500
            IF(M .NE. ICOAT .OR. CONZ2 .EQ. 0) GOTO 432                 HER 2510
            XAM(MY,J) = XAM(MY,J) + CONZ2 * XA(J)                       HER 2520
            XTM(MY,J) = XTM(MY,J) + CONZ2 * (XS(J)+XA(J))               HER 2530
  432       CONTINUE                                                    HER 2540
            IF(IY .LT. IX) GOTO 440                                     HER 2550
            DO 441 I=1,IX                                               HER 2560
              IF(M .NE. ICOAT .OR. CONZ2 .EQ. 0) GOTO 433               HER 2570
              P(I,J,MY) = P(I,J,MY) + CONZ2 * PP(I,J) / V(I)            HER 2580
  433         CONTINUE                                                  HER 2590
              P(I,J,M) = P(I,J,M) + CONCTA(M) * PP(I,J) / V(I)          HER 2600
  441       CONTINUE                                                    HER 2610
  440     CONTINUE                                                      HER 2620
  450   CONTINUE                                                        HER 2630
        IF(IPRIN(1) .LT. 3) GOTO 649                                    HER 2640
        WRITE (N6,103) L,IDT,(CONCTA(M),M=1,MY)                         HER 2650
  649 CONTINUE                                                          HER 2660
      IF(ICOAT .EQ. 0) GOTO 651                                         HER 2670
      DO 1650 J=1,IX                                                    HER 2680
        COA(7) = XTM(MY,J)                                              HER 2690
        COA(6) = (XTM(ICOAT,J)-COA(7)*BET2) / BET1                      HER 2700
        DO 1651 I=1,7                                                   HER 2710
          COP(2) = 0.                                                   HER 2720
          COP(1) = COA(I)                                               HER 2730
          COD(I) = DBLE(COP(1))                                         HER 2740
 1651   CONTINUE                                                        HER 2750
C                                                                       HER 2760
        CALL COPAMA(COD)                                                HER 2770
C                                                                       HER 2780
        COZ(J,1) = SNGL(COD(6))                                         HER 2790
        COZ(J,2) = SNGL(COD(7))                                         HER 2800
        SCHM2 = XAM(MY,J) * BET2                                        HER 2810
        XAM(ICOAT,J) = (XAM(ICOAT,J)-SCHM2) * COZ(J,1) + SCHM2 *        HER 2820
     1   COZ(J,2)                                                       HER 2830
        SCHM2 = XTM(MY,J) * BET2                                        HER 2840
        XTM(ICOAT,J) = (XTM(ICOAT,J)-SCHM2) * COZ(J,1) + SCHM2 *        HER 2850
     1   COZ(J,2)                                                       HER 2860
        DO 1652 I=1,IX                                                  HER 2870
          SCHM2 = P(I,J,MY) * BET2                                      HER 2880
          P(I,J,ICOAT) = (P(I,J,ICOAT)-SCHM2) * COZ(J,1) + SCHM2 *      HER 2890
     1     COZ(J,2)                                                     HER 2900
 1652   CONTINUE                                                        HER 2910
 1650 CONTINUE                                                          HER 2920
      IF(IPRIN(1) .LT. 3) GOTO 651                                      HER 2930
      WRITE (N6,99993)                                                  HER 2940
      WRITE (6,99992) (J,COZ(J,1),COZ(J,2),J=1,30)                      HER 2950
  651 CONTINUE                                                          HER 2960
      DO 251 I=1,NX                                                     HER 2970
        DO 251 J=1,NX                                                   HER 2980
          DO 251 K=1,IX                                                 HER 2990
            T(I,J,K) = 0.                                               HER 3000
  251 CONTINUE                                                          HER 3010
      NEGFL = 0                                                         HER 3020
  900 CONTINUE                                                          HER 3030
C                                                                       HER 3040
      CALL GEOV(NGEOM,RI,RO,XTM,VODFL,A(KA(LZLTH)),S,NEGFL)             HER 3050
C                                                                       HER 3060
C     EHEMALS SUBROUTINE ITER                                           HER 3070
C                                                                       HER 3080
      ITCNT = -1                                                        HER 3090
C                                                                       HER 3100
      CALL RELAX(ITCNT,RNEW,OVER,EPS,ITMAX,RENORM,IPR1)                 HER 3110
C                                                                       HER 3120
CFZJ060                                                       26.07.10  HER 3130
      DO 130 I=1,IX                                                     HER 3140
        DO 130 K=1,NX                                                   HER 3150
          F(K,I) = 0.                                                   HER 3160
          DO 120 N=1,NX                                                 HER 3170
            M = MTBL(N)                                                 HER 3180
            F(K,I) = F(K,I) + XTM(M,I) * T(N,K,I) * VOL(N) / VOL(K)     HER 3190
  120     CONTINUE                                                      HER 3200
          F(K,I) = F(K,I) * V(I)                                        HER 3210
  130 CONTINUE                                                          HER 3220
      D = 0.0                                                           HER 3230
      DO 170 I=1,IX                                                     HER 3240
        DO 170 N=1,NX                                                   HER 3250
          M = MTBL(N)                                                   HER 3260
          D = D + F(N,I) * S(M,I) * VOL(N) * DV(I)                      HER 3270
  170 CONTINUE                                                          HER 3280
      IF(D) 175,185,175                                                 HER 3290
  175 CONTINUE                                                          HER 3300
      D = 1. / D                                                        HER 3310
      DO 180 I=1,IX                                                     HER 3320
        DO 180 M=1,MX                                                   HER 3330
          S(M,I) = S(M,I) * D                                           HER 3340
  180 CONTINUE                                                          HER 3350
      FVSTR = D / (V(IX)+0.5*DV(IX))**2                                 HER 3360
  185 CONTINUE                                                          HER 3370
CFZJ060                                                       26.07.10  HER 3380
      DO 200 I=1,IX                                                     HER 3390
        DO 200 N=1,NX                                                   HER 3400
          Q(N,I) = 0.                                                   HER 3410
          M = MTBL(N)                                                   HER 3420
          DO 200 J=1,IX                                                 HER 3430
            Q(N,I) = Q(N,I) + F(N,J) * P(J,I,M) * DV(J)                 HER 3440
  200 CONTINUE                                                          HER 3450
      DO 210 I=1,IX                                                     HER 3460
        DO 210 N=1,NX                                                   HER 3470
          M = MTBL(N)                                                   HER 3480
          Q(N,I) = (V(I)*XTM(M,I)*DV(I)-Q(N,I)) * VOL(N)                HER 3490
  210 CONTINUE                                                          HER 3500
      JSATZ = 20                                                        HER 3510
      IF(NEUSPK .EQ. 1) GOTO 240                                        HER 3520
      IF(NEGFL .EQ. 1) THEN                                             HER 3530
        NEGFL = 0                                                       HER 3540
        GOTO 240                                                        HER 3550
      ENDIF                                                             HER 3560
      IF(JAD10(JSATZ) .EQ. 0) GOTO 240                                  HER 3570
      NXT10 = JAD10(JSATZ) + NSAEFL * (IPR7-1)                          HER 3580
      READ (NDA10,REC=NXT10) F                                          HER 3590
      NXT10 = NXT10 + 1                                                 HER 3600
      NEWSPK = 1                                                        HER 3610
      GOTO 261                                                          HER 3620
  240 D = 0.                                                            HER 3630
      NEWSPK = 0                                                        HER 3640
      DO 250 I=1,IX                                                     HER 3650
        B = VV(I)                                                       HER 3660
        B = B * EXP(-B/TKG)                                             HER 3670
        DO 250 N=1,NX                                                   HER 3680
          F(N,I) = B                                                    HER 3690
          D = D + B * Q(N,I)                                            HER 3700
  250 CONTINUE                                                          HER 3710
C                                                                       HER 3720
      IF(D .LE. 0.) CALL ESTOP(101)                                     HER 3730
C                                                                       HER 3740
      D = 1. / D                                                        HER 3750
      DO 260 I=1,IX                                                     HER 3760
        DO 260 N=1,NX                                                   HER 3770
          F(N,I) = D * F(N,I)                                           HER 3780
  260 CONTINUE                                                          HER 3790
  261 CONTINUE                                                          HER 3800
      QBAR = 0.                                                         HER 3810
      DO 270 I=1,IX                                                     HER 3820
        DO 270 N=1,NX                                                   HER 3830
          QBAR = QBAR + Q(N,I)**2                                       HER 3840
  270 CONTINUE                                                          HER 3850
      RNEW = 0.                                                         HER 3860
      ITCNT=0                                                           HER 3870
C                                                                       HER 3880
      CALL RELAX(ITCNT,RNEW,OVER,EPS,ITMAX,RENORM,IPR1)                 HER 3890
C                                                                       HER 3900
  280 IF(ITCNT .GT. (ITMAX-0) .AND. NEWSPK .EQ. 1) GOTO 240             HER 3910
      IF(ITCNT-ITMAX) 300,360,360                                       HER 3920
  300 DO 310 I=1,IX                                                     HER 3930
        DO 310 K=1,NX                                                   HER 3940
          M = MTBL(K)                                                   HER 3950
          H(K,I) = S(M,I)                                               HER 3960
          DO 310 J=1,IX                                                 HER 3970
            H(K,I) = H(K,I) + P(I,J,M) * F(K,J)                         HER 3980
  310 CONTINUE                                                          HER 3990
      D = 0.                                                            HER 4000
      DO 330 I=1,IX                                                     HER 4010
        DO 330 N=1,NX                                                   HER 4020
          R(N,I) = 0.                                                   HER 4030
          K = 1                                                         HER 4040
  319     R(N,I) = R(N,I) + H(K,I) * T(N,K,I)                           HER 4050
          K = K + 1                                                     HER 4060
          IF(K-NX) 319,319,320                                          HER 4070
  320     CONTINUE                                                      HER 4080
          D = D + R(N,I) * Q(N,I)                                       HER 4090
  330 CONTINUE                                                          HER 4100
      RENORM = 1. / D                                                   HER 4110
      DO 340 I=1,IX                                                     HER 4120
        DO 340 N=1,NX                                                   HER 4130
          R(N,I) = RENORM * R(N,I) - F(N,I)                             HER 4140
  340 CONTINUE                                                          HER 4150
      ITCNT = ITCNT + 1                                                 HER 4160
      RNEWD = 0.D0                                                      HER 4170
      ROOTQ = 1. / SQRT(QBAR)                                           HER 4180
      DO 345 I=1,IX                                                     HER 4190
        DO 345 N=1,NX                                                   HER 4200
          RNI = DBLE(R(N,I))                                            HER 4210
          QRAT = ROOTQ * Q(N,I)                                         HER 4220
          QRATD = DBLE(QRAT)                                            HER 4230
          RNEWD = RNEWD + (QRATD*RNI)**2                                HER 4240
  345 CONTINUE                                                          HER 4250
      RNEW = SNGL(DSQRT(RNEWD))                                         HER 4260
C                                                                       HER 4270
      CALL RELAX(ITCNT,RNEW,OVER,EPS,ITMAX,RENORM,IPR1)                 HER 4280
C                                                                       HER 4290
      DO 3120 I=1,IX                                                    HER 4300
        DO 3120 N=1,NX                                                  HER 4310
          F(N,I) = F(N,I) + OVER * R(N,I)                               HER 4320
 3120 CONTINUE                                                          HER 4330
      DO 350 I=1,IX                                                     HER 4340
        DO 350 N=1,NX                                                   HER 4350
          IF(ABS(R(N,I))-ABS(EPS*F(N,I))) 350,350,280                   HER 4360
  350 CONTINUE                                                          HER 4370
  360 CONTINUE                                                          HER 4380
      JSATZ = 20                                                        HER 4390
      IF(JAD10(JSATZ) .NE. 0) GOTO 363                                  HER 4400
      JAD10(JSATZ) = JSUM10                                             HER 4410
      NSAEFL = JSUM10                                                   HER 4420
  363 CONTINUE                                                          HER 4430
      NXT10 = JAD10(JSATZ)                                              HER 4440
      NXT10 = NXT10 + NSAEFL * (IPR7-1)                                 HER 4450
      WRITE (NDA10,REC=NXT10) F                                         HER 4460
      NXT10 = NXT10 + 1                                                 HER 4470
      IF(JAD10(JSATZ) .NE. JSUM10) GOTO 362                             HER 4480
      NSAEFL = NXT10 - NSAEFL                                           HER 4490
      JSUM10 = NXT10 + NSAEFL * (NXS-1)                                 HER 4500
      WRITE (N6,361) JSATZ, NDA10,JAD10(JSATZ)                          HER 4510
  362 CONTINUE                                                          HER 4520
      ROLD = 0.                                                         HER 4530
      DO 410 I=1,IX                                                     HER 4540
        DO 410 N=1,NX                                                   HER 4550
          IF(ABS(R(N,I))-ROLD) 410,410,400                              HER 4560
  400     ROLD = ABS(R(N,I))                                            HER 4570
          ROLDV = ROLD / ABS(F(N,I))                                    HER 4580
  410 CONTINUE                                                          HER 4590
      NB = 1                                                            HER 4600
      ND = 7                                                            HER 4610
      IF(IPRIN(1).NE.-1) WRITE (N6,9909) ITCNT,ROLDV,ALBEDO(1)          HER 4620
      IF(IPRIN(1) .LT. 3) GOTO 1050                                     HER 4630
 1010 IF(NX-ND) 1020,1030,1030                                          HER 4640
 1020 ND = NX                                                           HER 4650
 1030 WRITE (NTOUT,2020) ITCNT,RENORM,EPS                               HER 4660
      WRITE (NTOUT,1035) ROLD,RNEW,FVSTR                                HER 4670
      WRITE (NTOUT,2030)                                                HER 4680
      DO 1031 I=1,IX                                                    HER 4690
        WRITE (NTOUT,2031) (I,N,F(N,I),N=NB,ND)                         HER 4700
 1031 CONTINUE                                                          HER 4710
      IF(NX-ND) 1050,1050,1040                                          HER 4720
 1040 NB = NB + 7                                                       HER 4730
      ND = ND + 7                                                       HER 4740
      GOTO 1010                                                         HER 4750
 1050 CONTINUE                                                          HER 4760
      NXT10 = NN10                                                      HER 4770
C                                                                       HER 4780
      CALL EDIT(NABS,NSRK,NORMP,IDSELF,SFT,LT,A(KA(LIMAT)),A(KA(LOUSI)),HER 4790
     1 KL(LOUSI),A(KA(LTOSI)),A(KA(LABSI)),A(KA(LFISI)),KL(LFISI),      HER 4800
     2 A(KA(LXNU)),VF,A(KA(LCOSS)),NEGFL)                               HER 4810
      IF(NEGFL .EQ. 1) GOTO 900                                         HER 4820
C                                                                       HER 4830
      RETURN                                                            HER 4840
      END                                                               HER 4850
      SUBROUTINE EDIT(NABS,NSRK,NORMP,IDSELF,SFT,LT,IMAT,OUSIG,LA0,TOSIGEDI   10
     1 ,ABSIG,FISIG,LF0,XNU,VF,COSSHM,NEGFL)                            EDI   20
C                                                                       EDI   30
      DIMENSION NSRK(5),GIMY(5),SSHM(5),SSF(10),FIME(30),FIM(10),       EDI   40
     1 SVOL(10),SFT(50,30),IMAT(KMAT),OUSIG(LA0),TOSIG(LA0),ABSIG(LA0), EDI   50
     2 FISIG(LF0),XNU(LF0),VF(KMAT,10),COSSHM(NXS),IDSELF(200)          EDI   60
C                                                                       EDI   70
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    EDI   80
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    EDI   90
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIEDI  100
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 EDI  110
C                                                                       EDI  120
      COMMON /BLOCKT/ XA(30),XS(30),SP(30),XF(30),XT(30),PP(30,30)      EDI  130
C                                                                       EDI  140
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), EDI  150
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10EDI  160
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11EDI  170
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         EDI  180
C                                                                       EDI  190
      COMMON /BLOCRT/ ID,NBER,IZ,MZ,NZ,ICOAT,MY,JBRENN,SS96(96,10),     EDI  200
     1 F(20,30),MTBL(20),VOL(20),V(30),DV(30),VV(30),VDV(30),MG(30),    EDI  210
     2 COZ(30,2),GIM(30,10),ALBEDO(30),LEAKT                            EDI  220
C                                                                       EDI  230
      COMMON /FLUXN/ D(361),IACT                                        EDI  240
C                                                                       EDI  250
CFZJ055                                                       25.09.07  EDI  260
C                                                                       EDI  270
      EQUIVALENCE(JTPE3,N6,NTOUT)                                       EDI  280
C                                                                       EDI  290
  222 FORMAT ('1THERMAL FLUX '/8H0K,FI(K))                              EDI  300
  235 FORMAT (I6,E16.4)                                                 EDI  310
  236 FORMAT (22H AVERAGE ZONE FLUXES  ,5(I6,E13.4)/22X,5(I6,E13.4))    EDI  320
  237 FORMAT (22H0AVERAGE CELL FLUX    ,6('.'),E13.4)                   EDI  330
  242 FORMAT (I3,I4,'-',I2,E14.5,3X,11F9.5)                             EDI  340
  245 FORMAT ('1 SELFSHIELDING-FACTORS'//' GRP  FINE GRPS   ENERGY      EDI  350
     1SSF ZONE  1 -',I2)                                                EDI  360
  555 FORMAT ('0SSF, RELATIVE TO FLUX IN FUEL ZONE:',5(I3,E13.5)/36X,   EDI  370
     1 5(I3,E13.5))                                                     EDI  380
  556 FORMAT (' SSF, RELATIVE TO AVERAGE CELL FLUX:',5(I3,E13.5)/36X,   EDI  390
     1 5(I3,E13.5))                                                     EDI  400
  557 FORMAT (' SELFSHIELDING DUE TO COATED PARTICLE STRUCTURE OF FUEL:'EDI  410
     1 ,2E15.6)                                                         EDI  420
  690 FORMAT ('1 NEUTRON FLUXES FOR AVERAGE VALUES: ')                  EDI  430
  691 FORMAT (I3,3X,10E12.5)                                            EDI  440
CFZJ046                                                       26.09.06  EDI  450
  692 FORMAT (I6,4E15.6)                                                EDI  460
  693 FORMAT ('1 AVERAGED THERMAL CROSS SECTIONS'/'   MAT           SIGAEDI  470
     1           SIGS        NU*SIGF           SIGTR')                  EDI  480
 5004 FORMAT (I5,10E12.5)                                               EDI  490
 5005 FORMAT ('1',30X,' **  FLUXES NORMALIZED TO 1  **'/' GROUP         EDI  500
     1 ZONE 1 -',I2/' NORMALIZED PER ZONE')                             EDI  510
 5006 FORMAT ('0NORMALIZED PER CELL')                                   EDI  520
 5007 FORMAT ('0TOTAL CELL FLUX ........:',E15.6/' TOTAL ZONE FLUXES ...EDI  530
     1...:',10F9.5)                                                     EDI  540
C                                                                       EDI  550
C                                                                       EDI  560
      IX = IZ                                                           EDI  570
      MX = MZ                                                           EDI  580
      NX = NZ                                                           EDI  590
      IBRENN = JBRENN                                                   EDI  600
      V186 = SQRT(1.86/0.0253)                                          EDI  610
      DO 1056 I=1,IX                                                    EDI  620
        IF(V(I) .LE. V186) IW = I                                       EDI  630
        DO 1055 N=1,NX                                                  EDI  640
          F(N,I) = F(N,I) * VDV(I) * VOL(N)                             EDI  650
 1055   CONTINUE                                                        EDI  660
        DO 1056 K=1,MX                                                  EDI  670
          GIM(I,K) = 0.                                                 EDI  680
 1056 CONTINUE                                                          EDI  690
      GGIM = 0.                                                         EDI  700
      GVOL = 0.                                                         EDI  710
      DO 600 M=1,MX                                                     EDI  720
        SVOL(M) = 0.                                                    EDI  730
        FIM(M) = 0.                                                     EDI  740
  600 CONTINUE                                                          EDI  750
      DO 605 N=1,NX                                                     EDI  760
        FIME(N) = 0.                                                    EDI  770
        M = MTBL(N)                                                     EDI  780
        SVOL(M) = SVOL(M) + VOL(N)                                      EDI  790
        DO 601 I=1,IW                                                   EDI  800
          FIME(N) = FIME(N) + F(N,I)                                    EDI  810
          GIM(I,M) = GIM(I,M) + F(N,I)                                  EDI  820
  601   CONTINUE                                                        EDI  830
        FIME(N) = FIME(N) / VOL(N)                                      EDI  840
  605 CONTINUE                                                          EDI  850
      DO 602 M=1,MX                                                     EDI  860
        GVOL = GVOL + SVOL(M)                                           EDI  870
        DO 602 I=1,IW                                                   EDI  880
          GGIM = GGIM + GIM(I,M)                                        EDI  890
          GIM(I,M) = GIM(I,M) / SVOL(M)                                 EDI  900
  602 CONTINUE                                                          EDI  910
      GGIM = GGIM / GVOL                                                EDI  920
      IF(NORMP .GT. 0) GGIM = FIME(NORMP)                               EDI  930
      DO 603 M=1,MX                                                     EDI  940
        DO 603 I=1,IW                                                   EDI  950
          GIM(I,M) = GIM(I,M) / GGIM                                    EDI  960
  603 CONTINUE                                                          EDI  970
      IF(IPRIN(1) .LT. 3) GOTO 9000                                     EDI  980
      WRITE (N6,690)                                                    EDI  990
 9000 CONTINUE                                                          EDI 1000
      DO 604 I=1,IW                                                     EDI 1010
        IF(IPRIN(1) .LT. 3) GOTO 8999                                   EDI 1020
        WRITE (N6,691) I,(GIM(I,M),M=1,MX)                              EDI 1030
 8999   CONTINUE                                                        EDI 1040
        DO 604 M=1,MX                                                   EDI 1050
        IF(GIM(I,M) .LT. 0.) NEGFL = 1                                  EDI 1060
        IF(NEGFL .EQ. 1) RETURN                                         EDI 1070
          FIM(M) = FIM(M) + GIM(I,M)                                    EDI 1080
  604 CONTINUE                                                          EDI 1090
      IF(IPRIN(1) .LT. 2) GOTO 9001                                     EDI 1100
      WRITE (N6,691) MX,(FIM(M),M=1,MX)                                 EDI 1110
 9001 CONTINUE                                                          EDI 1120
      IF(IPRIN(1) .GE. 1) WRITE (N6,693)                                EDI 1130
      NKE = 0                                                           EDI 1140
      DO 610 N=1,KMAT                                                   EDI 1150
        IF(N .LE. NABS) GOTO 21                                         EDI 1160
        NKE = NKE + 1                                                   EDI 1170
        NXT10 = NSRK(NKE)                                               EDI 1180
   21   CONTINUE                                                        EDI 1190
        READ (NDA10,REC=NXT10) NN,IDT,IY,L,(XA(I),I=1,IX),(XS(I),I=1,IX)EDI 1200
     1   ,(SP(I),I=1,IX),((PP(I,J),J=1,IY),I=1,IY),(XF(I),I=1,IX),      EDI 1210
     2   (XT(I),I=1,IX),DFO,UB                                          EDI 1220
        NXT10 = NXT10 + 1                                               EDI 1230
        LLL = 0                                                         EDI 1240
        IF(LT .LE. 1) GOTO 44                                           EDI 1250
        LLL = IDSELF(N)                                                 EDI 1260
   44   CONTINUE                                                        EDI 1270
        XXA = 0.                                                        EDI 1280
        XXS = 0.                                                        EDI 1290
        XXF = 0.                                                        EDI 1300
        XXT = 0.                                                        EDI 1310
        DO 613 M=1,MX                                                   EDI 1320
          IF(VF(N,M) .LE. 0.0) GOTO 613                                 EDI 1330
          DO 611 I=1,IW                                                 EDI 1340
            C = 1.                                                      EDI 1350
            IF(LLL .GT. 0 .AND. NKE .LE. 0) C = SFT(LLL,I)              EDI 1360
            SFCP = 1.                                                   EDI 1370
            IF(MY .GT. MX) SFCP = VF(N,MY) * COZ(I,1) + (1.-VF(N,MY)) * EDI 1380
     1       COZ(I,2)                                                   EDI 1390
            GVSFCP = GIM(I,M) * VF(N,M) * C                             EDI 1400
            IF(M .EQ. IBRENN) GVSFCP = GVSFCP * SFCP                    EDI 1410
            XXA = XXA + XA(I) * GVSFCP                                  EDI 1420
            XXS = XXS + XS(I) * GVSFCP                                  EDI 1430
            XXF = XXF + XF(I) * GVSFCP                                  EDI 1440
            XXT = XXT + XT(I) * GVSFCP                                  EDI 1450
  611     CONTINUE                                                      EDI 1460
  613   CONTINUE                                                        EDI 1470
CFZJ046                                                       26.09.06  EDI 1480
        XXF = XXF * DFO                                                 EDI 1490
        IF(IPRIN(1) .LT. 1) GOTO 9002                                   EDI 1500
        M = IDT                                                         EDI 1510
CFZJ046                                                       26.09.06  EDI 1520
        WRITE (N6,692) M,XXA,XXS,XXF,XXT                                EDI 1530
 9002   CONTINUE                                                        EDI 1540
C                                                                       EDI 1550
C     ARRANGE SIGMAS IN ORDER OF V.S.O.P. LIBRARY                       EDI 1560
C                                                                       EDI 1570
        IF(N .GT. NABS) GOTO 7003                                       EDI 1580
        DO 4003 I=1,KMAT                                                EDI 1590
          JP = I                                                        EDI 1600
          MANU = IMAT(I)                                                EDI 1610
          IF(MANU .EQ. IDT) GOTO 4004                                   EDI 1620
 4003   CONTINUE                                                        EDI 1630
 4004   CONTINUE                                                        EDI 1640
        I = N26                                                         EDI 1650
        JAP = ((IPRIN(7)-1)*KMAT+JP-1) * N26                            EDI 1660
CFZJ046                                                       26.09.06  EDI 1670
        TOSIG(I+JAP) = XXT                                              EDI 1680
        ABSIG(I+JAP) = XXA                                              EDI 1690
        OUSIG(I+JAP) = 0.0                                              EDI 1700
        IF(JP .GT. IACT) GOTO 4001                                      EDI 1710
        JFI = ((IPRIN(7)-1)*IACT+JP-1) * N26                            EDI 1720
        FISIG(I+JFI) = XXF                                              EDI 1730
        XNU(I+JFI) = DFO                                                EDI 1740
 4001   CONTINUE                                                        EDI 1750
        GOTO 610                                                        EDI 1760
 7003   CONTINUE                                                        EDI 1770
        I = N26                                                         EDI 1780
        JAP = ((IPRIN(7)-1)*KMAT+ N-1) * N26                            EDI 1790
CFZJ046                                                       26.09.06  EDI 1800
        TOSIG(I+JAP) = XXT                                              EDI 1810
        ABSIG(I+JAP) = XXA                                              EDI 1820
        OUSIG(I+JAP) = 0.0                                              EDI 1830
  610 CONTINUE                                                          EDI 1840
      DO 612 I=1,IX                                                     EDI 1850
        DO 612 M=1,MX                                                   EDI 1860
          GIM(I,M) = 0.                                                 EDI 1870
  612 CONTINUE                                                          EDI 1880
      IF(IPRIN(1) .LT. 4) GOTO 9003                                     EDI 1890
      WRITE (NTOUT,222)                                                 EDI 1900
 9003 CONTINUE                                                          EDI 1910
      IA = IW + 1                                                       EDI 1920
      DO 224 N=1,NX                                                     EDI 1930
        DO 223 I=IA,IX                                                  EDI 1940
          FIME(N) = FIME(N) + F(N,I) / VOL(N)                           EDI 1950
  223   CONTINUE                                                        EDI 1960
        IF(IPRIN(1) .LT. 4) GOTO 224                                    EDI 1970
        WRITE (NTOUT,235) N,FIME(N)                                     EDI 1980
  224 CONTINUE                                                          EDI 1990
      GVOL = 0.                                                         EDI 2000
      K = 1                                                             EDI 2010
      FIM(K) = 0.                                                       EDI 2020
      SVOL(K) = 0.                                                      EDI 2030
      DO 228 N=1,NX                                                     EDI 2040
        M = MTBL(N)                                                     EDI 2050
        IF(M-K) 251,226,227                                             EDI 2060
  226   FIM(K) = FIM(K) + FIME(N) * VOL(N)                              EDI 2070
        SVOL(K) = SVOL(K) + VOL(N)                                      EDI 2080
        DO 5011 I=1,IX                                                  EDI 2090
          GIM(I,K) = GIM(I,K) + F(N,I)                                  EDI 2100
 5011   CONTINUE                                                        EDI 2110
        GOTO 228                                                        EDI 2120
  227   GVOL = GVOL + SVOL(K)                                           EDI 2130
        FIM(K) = FIM(K) / SVOL(K)                                       EDI 2140
        K = K + 1                                                       EDI 2150
        FIM(K) = 0.                                                     EDI 2160
        SVOL(K) = 0.                                                    EDI 2170
        GOTO 226                                                        EDI 2180
  228 CONTINUE                                                          EDI 2190
      GVOL = GVOL + SVOL(K)                                             EDI 2200
      FIM(K) = FIM(K) / SVOL(K)                                         EDI 2210
      FIMZ = 0.                                                         EDI 2220
      COKERN = 0.                                                       EDI 2230
      COCOAT = 0.                                                       EDI 2240
      DO 229 M=1,MX                                                     EDI 2250
        K = M                                                           EDI 2260
        GIMY(K) = 0.                                                    EDI 2270
        SSHM(K) = FIM(K) / FIM(IBRENN)                                  EDI 2280
        SVFI = SVOL(K) * FIM(K)                                         EDI 2290
        FIMZ = FIMZ + SVFI                                              EDI 2300
        DO 5012 I=1,IX                                                  EDI 2310
          GIM(I,K) = GIM(I,K) / SVFI                                    EDI 2320
          GIMY(K) = GIMY(K) + GIM(I,K)                                  EDI 2330
          IF(K .NE. ICOAT) GOTO 5020                                    EDI 2340
          COKERN = COKERN + GIM(I,ICOAT) * COZ(I,1)                     EDI 2350
          COCOAT = COCOAT + GIM(I,ICOAT) * COZ(I,2)                     EDI 2360
 5020     CONTINUE                                                      EDI 2370
          GIM(I,K) = GIM(I,K) / VDV(I)                                  EDI 2380
 5012   CONTINUE                                                        EDI 2390
  229 CONTINUE                                                          EDI 2400
      FIMZ = FIMZ / GVOL                                                EDI 2410
      IF(IPRIN(1) .LT. 2) GOTO 9004                                     EDI 2420
      WRITE (NTOUT,5005) MX                                             EDI 2430
      DO 5014 I=1,IX                                                    EDI 2440
        J = I                                                           EDI 2450
        WRITE (N6,5004) I,(GIM(J,K),K=1,MX)                             EDI 2460
 5014 CONTINUE                                                          EDI 2470
      GIMZ = 0.                                                         EDI 2480
      DO 5013 K=1,MX                                                    EDI 2490
        FAKTO = FIM(K) / (GVOL*FIMZ)                                    EDI 2500
        DO 5013 I=1,IX                                                  EDI 2510
          GIM(I,K) = GIM(I,K) * FAKTO                                   EDI 2520
          GIMZ = GIMZ + GIM(I,K) * VDV(I) * SVOL(K)                     EDI 2530
 5013 CONTINUE                                                          EDI 2540
      WRITE (NTOUT,5006)                                                EDI 2550
      DO 5015 I=1,IX                                                    EDI 2560
        J = I                                                           EDI 2570
        WRITE (N6,5004) I,(GIM(J,K),K=1,MX)                             EDI 2580
 5015 CONTINUE                                                          EDI 2590
      WRITE (NTOUT,5007) GIMZ,(GIMY(K),K=1,MX)                          EDI 2600
      WRITE (NTOUT,237) FIMZ                                            EDI 2610
      WRITE (NTOUT,236) (K,FIM(K),K=1,MX)                               EDI 2620
 9004 CONTINUE                                                          EDI 2630
      IF(IPRIN(1) .NE. -1) WRITE (NTOUT,555) (K,SSHM(K),K=1,MX)         EDI 2640
      DO 1099 M=1,MX                                                    EDI 2650
        SSHM(M) = FIM(M) / FIMZ                                         EDI 2660
 1099 CONTINUE                                                          EDI 2670
      I7 = IPRIN(7)                                                     EDI 2680
      IF(ICOAT .GT. 0) COSSHM(I7) = COKERN * SSHM(ICOAT)                EDI 2690
      IF(ICOAT .LE. 0) COSSHM(I7) = SSHM(IBRENN)                        EDI 2700
      IF(IPRIN(1) .NE. -1) WRITE (NTOUT,556) (K,SSHM(K),K=1,MX)         EDI 2710
      IF(ICOAT .GT. 0 .AND. IPRIN(1) .NE. -1) WRITE (NTOUT,557) COKERN, EDI 2720
     1 COCOAT                                                           EDI 2730
      IF(IPRIN(1) .LT. 2) GOTO 251                                      EDI 2740
      WRITE (NTOUT,245) MX                                              EDI 2750
      I2 = 0                                                            EDI 2760
      DO 238 I=1,IX                                                     EDI 2770
        K = 1                                                           EDI 2780
        SVOL(K) = 0.                                                    EDI 2790
        FIME(I) = 0.                                                    EDI 2800
        DO 239 N=1,NX                                                   EDI 2810
          M = MTBL(N)                                                   EDI 2820
          IF(M-K) 251,240,241                                           EDI 2830
  241     FIM(K) = FIME(I) / SVOL(K)                                    EDI 2840
          K = K + 1                                                     EDI 2850
          FIME(I) = 0.                                                  EDI 2860
          SVOL(K) = 0.                                                  EDI 2870
  240     CONTINUE                                                      EDI 2880
          FIME(I) = FIME(I) + F(N,I) / VDV(I)                           EDI 2890
          SVOL(K) = SVOL(K) + VOL(N)                                    EDI 2900
  239   CONTINUE                                                        EDI 2910
        FIM(K) = FIME(I) / SVOL(K)                                      EDI 2920
        FIMZ = 0.                                                       EDI 2930
        DO 400 K=1,MX                                                   EDI 2940
          FIMZ = FIMZ + FIM(K) * SVOL(K)                                EDI 2950
  400   CONTINUE                                                        EDI 2960
        FIMZ = FIMZ / GVOL                                              EDI 2970
        DO 300 K=1,MX                                                   EDI 2980
          SSF(K) = FIM(K) / FIMZ                                        EDI 2990
  300   CONTINUE                                                        EDI 3000
        IF(ICOAT .LE. 0) GOTO 302                                       EDI 3010
        SSF(ICOAT) = SSF(ICOAT) * COZ(I,1)                              EDI 3020
        SSF(MY) = SSF(ICOAT) * COZ(I,2)                                 EDI 3030
  302   CONTINUE                                                        EDI 3040
        I1 = I2 + 1                                                     EDI 3050
        I2 = I1 + MG(I)                                                 EDI 3060
        DO 301 K=1,MY                                                   EDI 3070
          DO 301 N=I1,I2                                                EDI 3080
            SS96(N,K) = SSF(K)                                          EDI 3090
  301   CONTINUE                                                        EDI 3100
        PEN = 0.0253 * VV(I)                                            EDI 3110
        WRITE (N6,242) I,I1,I2,PEN,(SSF(K),K=1,MY)                      EDI 3120
  238 CONTINUE                                                          EDI 3130
  251 CONTINUE                                                          EDI 3140
      RETURN                                                            EDI 3150
      END                                                               EDI 3160
      SUBROUTINE ESEL(J,VF,KMAT)                                        ESE   10
C                                                                       ESE   20
C     ERSETZE SELFSHIELDING-FAKTOREN                                    ESE   30
C                                                                       ESE   40
      DIMENSION E(96),S(96),SIGA(96),DELTA(96),PHI(96),XA(96),XF(96),   ESE   50
     1 XS(96),SIGN(96),SIGTR(96),SIGS(96),SELS(97),DID(18),BEG(51),     ESE   60
     2 LBON(51),NDIV(96),VEC(5),TIN(50),XTR(96),FINU(96),EKVR(96,96),   ESE   70
     3 TIM(96,96),DIF(6,50),VF(KMAT,10)                                 ESE   80
C                                                                       ESE   90
      COMMON /BLOCKT/ DELT,NOIT,TIN,SIGA,SIGN,SIGS,SIGTR,EKVR,PHI,E,BN, ESE  100
     1 SELS,DID,BEG,LBON,NDIV,DIF,TIM,FMIC,NEVR,NBG,NALPHA,NKER,MNBG,   ESE  110
     2 FINU,DELTA,KDIV,XA,XS,XF,XTR,S,T,UB,DFO,IDT,VEC                  ESE  120
C                                                                       ESE  130
      COMMON /BLOCRT/ ID,NBER,IX,MX,NX,ICOAT,MY,IBRENN,SS96(96,10),     ESE  140
     1 F(20,30),MTBL(20),VOL(20),V(30),DV(30),VV(30),VDV(30),MG(30),    ESE  150
     2 COZ(30,2),GIM(30,10),ALBEDO(30),LEAKT                            ESE  160
C                                                                       ESE  170
C                                                                       ESE  180
      MM = 0                                                            ESE  190
      JCOAT = 0                                                         ESE  200
      DO 2 M=1,MX                                                       ESE  210
        IF(VF(J,M) .NE. 1.) GOTO 1                                      ESE  220
        MM = M                                                          ESE  230
        GOTO 4                                                          ESE  240
    1   CONTINUE                                                        ESE  250
        IF(VF(J,M) .GT. 0.) GOTO 6                                      ESE  260
    2 CONTINUE                                                          ESE  270
C                                                                       ESE  280
C     MATERIAL KAM BEI ZELLRECHNUNG NICHT VOR                           ESE  290
C                                                                       ESE  300
      DO 3 I=1,NEVR                                                     ESE  310
        SELS(I) = 1.                                                    ESE  320
    3 CONTINUE                                                          ESE  330
      GOTO 9                                                            ESE  340
    4 CONTINUE                                                          ESE  350
C                                                                       ESE  360
C     MATERIAL IST NUR IN THERMOS-ZELLE MM VORHANDEN                    ESE  370
C                                                                       ESE  380
      IF(MM .EQ. ICOAT .AND. VF(J,MY) .NE. 1.) JCOAT = 1                ESE  390
      DO 5 I=1,NEVR                                                     ESE  400
        SELS(I) = SS96(I,MM)                                            ESE  410
        IF(JCOAT .GT. 0) SELS(I) = SS96(I,MY) + (SS96(I,ICOAT)-         ESE  420
     1   SS96(I,MY)) * VF(J,MY)                                         ESE  430
    5 CONTINUE                                                          ESE  440
      GOTO 9                                                            ESE  450
    6 CONTINUE                                                          ESE  460
C                                                                       ESE  470
C     MATERIAL IST IN MEHREREN THERMOS-ZONEN DRIN                       ESE  480
C                                                                       ESE  490
      IF(ICOAT .GT. 0 .AND. VF(J,MY) .NE. 1.) JCOAT = 1                 ESE  500
      DO 7 I=1,NEVR                                                     ESE  510
        SELS(I) = 0.                                                    ESE  520
        DO 7 M=1,MX                                                     ESE  530
          IF(VF(J,M) .EQ. 0.) GOTO 7                                    ESE  540
          SS = SS96(I,M)                                                ESE  550
          IF(M .EQ. ICOAT .AND. JCOAT .GT. 0) SS = SS96(I,MY) +         ESE  560
     1     (SS96(I,ICOAT)-SS96(I,MY)) * VF(J,MY)                        ESE  570
          SELS(I) = SELS(I) + SS * VF(J,M)                              ESE  580
    7 CONTINUE                                                          ESE  590
    9 CONTINUE                                                          ESE  600
      RETURN                                                            ESE  610
      END                                                               ESE  620
      SUBROUTINE RELAX(ITCNT,RNEW,OVER,EPS,ITMAX,RENORM,IPR1)           REL   10
C                                                                       REL   20
      DIMENSION RBS(20)                                                 REL   30
C                                                                       REL   40
CFZJ055                                                       25.09.07  REL   50
C                                                                       REL   60
      COMMON /BLOCK1/ IPRIN(15)                                         REL   70
C                                                                       REL   80
      DATA NTOUT/6/                                                     REL   90
C                                                                       REL  100
   50 FORMAT (17H1EARLIEST EXTRAP=,I3/12H NO. TESTED=,I3/11H INCREMENT=,REL  110
     1 I3/9H MAX ITS=,I3/11H CONV CRIT=,1PE12.5/16H OVERRELAXATION=,    REL  120
     2 1PE12.5/13H EXTRAP CRIT=,1PE12.5/12H MAX EXTRAP=,1PE12.5/8H FACTOREL  130
     3R=,1PE12.5)                                                       REL  140
   70 FORMAT (37H   IT    RENORM    RMS RES      RATIO)                 REL  150
 2920 FORMAT (I6,F10.5,1PE13.4,0PF9.4)                                  REL  160
 3085 FORMAT (' EXTRAPOLATION AT',I4,' BY',F12.4,' MU=',F8.5,7H   LAM=, REL  170
     1 F8.5,9H   OMEGA=,F8.5)                                           REL  180
C                                                                       REL  190
C                                                                       REL  200
      IF(ITCNT) 35,100,110                                              REL  210
  110 IF(ITCNT-1) 120,120,2900                                          REL  220
  120 ROLD = RNEW * 6.4                                                 REL  230
      GOTO 2900                                                         REL  240
   35 EPS = 5.E-5                                                       REL  250
      RATIO = 1.0                                                       REL  260
      RELC = 1.7                                                        REL  270
      EPSG = .05                                                        REL  280
      OVERX = 50.                                                       REL  290
      FACTOR = 1.                                                       REL  300
      ITBG = 30                                                         REL  310
      LCMX = 5                                                          REL  320
      ITDM = 10                                                         REL  330
      ITMAX = 300                                                       REL  340
      IPT = IPR1 - 3                                                    REL  350
      RELCA = RELC                                                      REL  360
      IF(IPR1 .LT. 4) GOTO 90                                           REL  370
      WRITE (NTOUT,50) ITBG,LCMX,ITDM,ITMAX,EPS,RELC,EPSG,OVERX,FACTOR  REL  380
   90 CONTINUE                                                          REL  390
      IF(IPT) 3110,3110,60                                              REL  400
   60 WRITE (NTOUT,70)                                                  REL  410
      GOTO 3110                                                         REL  420
  100 LCNT = 0                                                          REL  430
      RELC = RELCA                                                      REL  440
      GOTO 3110                                                         REL  450
 2900 CONTINUE                                                          REL  460
      IF(ROLD .NE. 0.0) RATIO = RNEW / ROLD                             REL  470
      ROLD = RNEW                                                       REL  480
      IF(IPT) 2990,2990,2910                                            REL  490
 2910 WRITE (NTOUT,2920) ITCNT,RENORM,RNEW,RATIO                        REL  500
 2990 IF(ITCNT-ITBG) 3100,3000,3000                                     REL  510
 3000 LCNT = LCNT + 1                                                   REL  520
      RBS(LCNT) = 1.                                                    REL  530
      IF(RATIO .NE. 1.) RBS(LCNT) = 1. / (1.-RATIO)                     REL  540
      IF(LCNT-LCMX) 3100,3020,3020                                      REL  550
 3020 LCMXP = LCMX - 1                                                  REL  560
      DO 3040 M=1,LCMXP                                                 REL  570
        IF(ABS((RBS(M)-RBS(LCMXP))/RBS(LCMXP))-EPSG) 3040,3040,3050     REL  580
 3040 CONTINUE                                                          REL  590
      GOTO 3060                                                         REL  600
 3050 LCNT = 0                                                          REL  610
      GOTO 3100                                                         REL  620
 3060 OVER = FACTOR * RBS(LCMXP)                                        REL  630
      IF(OVER-OVERX) 3080,3080,3070                                     REL  640
 3070 OVER = OVERX                                                      REL  650
 3080 ALAM = (RATIO+RELC-1.) / RELC                                     REL  660
      RELCB = 2. / (2.-ALAM)                                            REL  670
      IF(RELCB-2.) 3082,3083,3083                                       REL  680
 3082 RELC = RELCB                                                      REL  690
 3083 IF(IPRIN(1) .GT. 0) WRITE (NTOUT,3085) ITCNT,OVER,RATIO,ALAM,RELC REL  700
      ITBG = ITCNT + ITDM + 1                                           REL  710
      LCNT = 0                                                          REL  720
      GOTO 3110                                                         REL  730
 3100 OVER = RELC                                                       REL  740
 3110 RETURN                                                            REL  750
      END                                                               REL  760
      SUBROUTINE COPAMA(COD)                                            COP   10
C                                                                       COP   20
      IMPLICIT REAL*8 (A-H,O-Z)                                         COP   30
C                                                                       COP   40
      DIMENSION SR(10),RRO2(10),CA(10),D(10),COD(10)                    COP   50
C                                                                       COP   60
C                                                                       COP   70
      R1 = COD(1)                                                       COP   80
      R2 = COD(2)                                                       COP   90
      F = COD(3)                                                        COP  100
      ALPH = COD(4)                                                     COP  110
      SI1 = COD(6)                                                      COP  120
      SI2 = COD(7)                                                      COP  130
      IF(SI1 .EQ. 0.) GOTO 99                                           COP  140
      BLF = 1.                                                          COP  150
C                                                                       COP  160
C     HERSTELLEN DER RECHENGROESSEN                                     COP  170
C                                                                       COP  180
      CNUMI = 0.D0                                                      COP  190
      DO 10 I=1,10                                                      COP  200
        SR(I) = 0.D0                                                    COP  210
        CA(I) = 0.D0                                                    COP  220
        D(I) = 0.D0                                                     COP  230
        RRO2(I) = 0.D0                                                  COP  240
   10 CONTINUE                                                          COP  250
      SR(1) = SI1 * R1                                                  COP  260
      SR(2) = SI2 * R1                                                  COP  270
      SR(3) = SI2 * R2                                                  COP  280
      RRO2(1) = (R2/R1)**2                                              COP  290
C                                                                       COP  300
C     HERSTELLEN DER WAHRSCHEINLICHKEITEN A                             COP  310
C                                                                       COP  320
      UND = 2.D0                                                        COP  330
      D(1) = SR(2)                                                      COP  340
      D(2) = SR(1)                                                      COP  350
      D(4) = RRO2(1)                                                    COP  360
      ALF = BLF - ALPH                                                  COP  370
      DALF = DEXP(-SR(3)*2.*ALPH/(3.*F))                                COP  380
      D(6) = ALF                                                        COP  390
C                                                                       COP  400
      CALL NUMINT(UND,D,CNUMA)                                          COP  410
C                                                                       COP  420
      C1 = CNUMA                                                        COP  430
      CA(1) = SR(1)                                                     COP  440
C                                                                       COP  450
      CALL ANINTF(CA)                                                   COP  460
C                                                                       COP  470
C     JETZT DAS SI3                                                     COP  480
C                                                                       COP  490
      UND = 1.D0                                                        COP  500
      D(6) = ALF                                                        COP  510
C                                                                       COP  520
      CALL NUMINT(UND,D,CNUMI)                                          COP  530
C                                                                       COP  540
      CA(1) = SR(3) * ALF                                               COP  550
      CA(2) = 1.D0 / RRO2(1)                                            COP  560
C                                                                       COP  570
      CALL ANINTF(CA)                                                   COP  580
C                                                                       COP  590
      CA(6) = CNUMI * DALF**2                                           COP  600
      CA(7) = CNUMA * DALF                                              COP  610
      CA(4) = CA(4) * DALF**2                                           COP  620
      CA(5) = 1.- CA(4)                                                 COP  630
      CA(8) = CA(4) + CA(6) * CA(2)                                     COP  640
      CA(9) = CA(5) - CA(6) * CA(2)                                     COP  650
      CA(7) = CA(9) - CA(7) * CA(2)                                     COP  660
      CA(10) = DLOG(1.D0/CA(8))                                         COP  670
      A9C = CA(9)                                                       COP  680
      QSIG3 = 1. - C1 * DALF * CA(2) / A9C                              COP  690
      SI3 = CA(10) * 3. * F / (4.*R2)                                   COP  700
      SI31 = SI3 * (1.-QSIG3)                                           COP  710
      SI32 = SI3 - SI31                                                 COP  720
      RO3 = (R1/R2)**3                                                  COP  730
      SI31S = SI1 * F * RO3                                             COP  740
      SI32S = SI2 * (F*(1.-RO3)+ ALPH*(1.-F))                           COP  750
      SF1 = SI31 / SI31S                                                COP  760
      IF(SI32S .NE. 0.) GOTO 98                                         COP  770
      SF2 = 1.                                                          COP  780
      GOTO 97                                                           COP  790
   98 CONTINUE                                                          COP  800
      SF2 = SI32 / SI32S                                                COP  810
   97 CONTINUE                                                          COP  820
      COD(6) = SF1                                                      COP  830
      COD(7) = SF2                                                      COP  840
   99 CONTINUE                                                          COP  850
      RETURN                                                            COP  860
      END                                                               COP  870
      SUBROUTINE NUMINT(UND,D,CNUMI)                                    MIN   10
C                                                                       MIN   20
      IMPLICIT REAL*8 (A-H,O-Z)                                         MIN   30
C                                                                       MIN   40
      DIMENSION D(10),V(6),Y(5),X(6),IK(4)                              MIN   50
C                                                                       MIN   60
      DATA KFAOP$/6/                                                    MIN   70
C                                                                       MIN   80
   12 FORMAT (49H OVER 600 SIMP-INTEGR-ITERATIONS FOR ONE INTEGRAL)     MIN   90
C                                                                       MIN  100
C                                                                       MIN  110
C     NUMERISCHE INTEGRATION                                            MIN  120
C                                                                       MIN  130
      IND = IFIX(SNGL(UND))                                             MIN  140
      CNUMI = 0.D0                                                      MIN  150
      ALF = D(6)                                                        MIN  160
      L = 1                                                             MIN  170
      X(6) = 1.                                                         MIN  180
      X(1) = 0.                                                         MIN  190
      FSIMP = 0.                                                        MIN  200
      D1ALF = DMAX1(D(1)*4.D-6,1.0D-10)                                 MIN  210
      EPSI1 = DMIN1(D1ALF,1.0D-7)                                       MIN  220
      EPSI2 = EPSI1 * 1.0D-2                                            MIN  230
      SCHRIT = 0.25                                                     MIN  240
      DX1 = 0.                                                          MIN  250
      DO 2 I=1,4                                                        MIN  260
        IK(I) = 0                                                       MIN  270
    2 CONTINUE                                                          MIN  280
    1 CONTINUE                                                          MIN  290
      FPROV = 0.                                                        MIN  300
C                                                                       MIN  310
      CALL SIMP2(X,Y,FSIMP,EPSI1,EPSI2,SCHRIT,L,IA,IS,DX1,IK,F1SIM,FPROVMIN  320
     1 )                                                                MIN  330
C                                                                       MIN  340
      GOTO(10,3),L                                                      MIN  350
    3 DO 4 I=1,5                                                        MIN  360
        V(I) = 0.D0                                                     MIN  370
        V(I) = X(I)                                                     MIN  380
    4 CONTINUE                                                          MIN  390
C                                                                       MIN  400
C     BERECHNUNG DES INTEGRANDEN                                        MIN  410
C                                                                       MIN  420
      DO 9 I=IA,5,IS                                                    MIN  430
        D(6) = DSQRT(1.D0-V(I))                                         MIN  440
        D(7) = DSQRT(D(4)-V(I))                                         MIN  450
        DEX = D(1) * (ALF*D(7)-D(6))                                    MIN  460
        IF(DEX-130.) 31,31,32                                           MIN  470
   31   D(8) = DEXP(-DEX)                                               MIN  480
        GOTO 33                                                         MIN  490
   32   D(8) = 0.D0                                                     MIN  500
   33   CONTINUE                                                        MIN  510
        DEX = 2.D0 * D(2) * D(6)                                        MIN  520
        IF(DEX-130.) 21,21,22                                           MIN  530
   21   D(9) = D(8) / DEXP(DEX)                                         MIN  540
        GOTO 23                                                         MIN  550
   22   D(9) = 0.D0                                                     MIN  560
   23   CONTINUE                                                        MIN  570
        GOTO(5,6,6),IND                                                 MIN  580
    5   D(10) = D(8) * D(9)                                             MIN  590
        GOTO 8                                                          MIN  600
    6   D(10) = D(8) - D(9)                                             MIN  610
        GOTO(8,8,7),IND                                                 MIN  620
    7   D(10) = D(10) / DEXP(D(3)*(DSQRT(D(5)-V(I))-D(7)))              MIN  630
    8   Y(I) = D(10)                                                    MIN  640
    9 CONTINUE                                                          MIN  650
      IK(4) = IK(4) + 1                                                 MIN  660
      IF(IK(4)-600) 1,1,11                                              MIN  670
   10 CNUMI = FSIMP                                                     MIN  680
      RETURN                                                            MIN  690
   11 WRITE (KFAOP$,12)                                                 MIN  700
      RETURN                                                            MIN  710
      END                                                               MIN  720
      SUBROUTINE ANINTF(CA)                                             ANI   10
C                                                                       ANI   20
      IMPLICIT REAL*8 (A-H,O-Z)                                         ANI   30
C                                                                       ANI   40
      DIMENSION CA(10)                                                  ANI   50
C                                                                       ANI   60
C                                                                       ANI   70
C     ANALYTISCH INTEGRIERTE FUNKTION                                   ANI   80
C                                                                       ANI   90
      C = 2. * CA(1)                                                    ANI  100
      CQ = C**2                                                         ANI  110
      IF(CA(2) .EQ. 0.) GOTO 1                                          ANI  120
      CQ = CQ * (1.-CA(2))                                              ANI  130
      C = DSQRT(CQ)                                                     ANI  140
    1 CONTINUE                                                          ANI  150
      IF(C .LT. 0.004) GOTO 2                                           ANI  160
      IF(C .GT. 130.) C = -1.                                           ANI  170
      CA(4) = (1.-(1.+C)/DEXP(C)) / (CA(1)**2*2.)                       ANI  180
      CA(5) = 1. - CA(4)                                                ANI  190
      GOTO 3                                                            ANI  200
    2 CONTINUE                                                          ANI  210
      CA(5) = CA(2) + (1.-CA(2)) * (C/6.-CQ/16.+C*CQ/60.) * 4.          ANI  220
      CA(4) = 1. - CA(5)                                                ANI  230
    3 CONTINUE                                                          ANI  240
      RETURN                                                            ANI  250
      END                                                               ANI  260
      SUBROUTINE SIMP2(X,Y,FSIMP,EPSI1,EPSI2,SCHRIT,L,IA,IS,DX1,IK,F1SIMIMP   10
     1 ,FPROV)                                                          IMP   20
C                                                                       IMP   30
C  SUBROUTINE SIMPSON INTEGRATION                                       IMP   40
C  X = STUTZPUNKT                                                       IMP   50
C  Y = ZUGEHOERIGER FUNKTIONSWERT                                       IMP   60
C  SCHRIT = SCHRITTWEITE                                                IMP   70
C  EPSI1 = MAX. RELATIVER FEHLER, EPSI2 = MIN. RELATIVER FEHLER         IMP   80
C  FSIMP = INTEGRALWERT MIT SCHRITTWEITE DX/2                           IMP   90
C  DIE SHRITTWEITE DES ERSTEN SCHRITTES WIRD GESPEICHERT AUF S C H R I TIMP  100
C  UND STEHT BEI DER EVTL. NAECHSTEN KURVE MIT GLEICHEM STEIGUNGSVERLAUFIMP  110
C  WIEDER ZUR VERFUEGUNG.                                               IMP  120
C                                                                       IMP  130
      IMPLICIT REAL*8 (A-H,O-Z)                                         IMP  140
C                                                                       IMP  150
      DIMENSION X(6),Y(5),IK(4)                                         IMP  160
C                                                                       IMP  170
CFZJ055                                                       25.09.07  IMP  180
C                                                                       IMP  190
   31 FORMAT (' SIMP-INTEGRATION STEP WIDTH DX =',D12.6,', EPSI1 =',    IMP  200
     1 D12.6,' WILL BE ENLARGED BY FACTOR 10')                          IMP  210
C                                                                       IMP  220
C                                                                       IMP  230
      DX = DX1                                                          IMP  240
      MM = IK(1)                                                        IMP  250
      KSI = IK(2)                                                       IMP  260
      LAUF = IK(3)                                                      IMP  270
      K = KSI                                                           IMP  280
      GOTO(111,2),L                                                     IMP  290
  111 L = 2                                                             IMP  300
      K = 1                                                             IMP  310
      LAUF = 1                                                          IMP  320
      FSIMP = 0.                                                        IMP  330
      F1SIM = 0.                                                        IMP  340
      DX = SCHRIT                                                       IMP  350
      MM = 1                                                            IMP  360
      VFAKT = 1.                                                        IMP  370
   14 GOTO(114,214),MM                                                  IMP  380
  214 MM = 1                                                            IMP  390
      EPSI1 = EPSI1 / VFAKT                                             IMP  400
      EPSI2 = EPSI2 / VFAKT                                             IMP  410
      VFAKT = 1.                                                        IMP  420
  114 IA = 1                                                            IMP  430
      IS = 1                                                            IMP  440
      DO 5 I=1,2                                                        IMP  450
        X(I+1) = X(I) + DX                                              IMP  460
        X(I+3) = X(I) + DX / 2.                                         IMP  470
    5 CONTINUE                                                          IMP  480
   88 KSI = K                                                           IMP  490
      DX1 = DX                                                          IMP  500
      IK(1) = MM                                                        IMP  510
      IK(2) = KSI                                                       IMP  520
      IK(3) = LAUF                                                      IMP  530
      IK(4) = IK(4) + 1                                                 IMP  540
      RETURN                                                            IMP  550
    2 D1 = DX / 3.                                                      IMP  560
      D2 = D1 / 2.                                                      IMP  570
      A1 = Y(1) + 4. * Y(2) + Y(3)                                      IMP  580
      A2 = A1 - Y(2) - Y(2) + 4. * (Y(4)+Y(5))                          IMP  590
      F1 = D1 * A1                                                      IMP  600
      F2 = D2 * A2                                                      IMP  610
      ABW = 0.                                                          IMP  620
      IF(F1 .EQ. F2) GOTO 30                                            IMP  630
      F12 = DMAX1(DABS(F1),DABS(F2),DABS(F2+FSIMP),1.0D-7,DABS(FPROV*DX)IMP  640
     1 )                                                                IMP  650
      ABW = DABS((F1-F2)/F12)                                           IMP  660
   30 CONTINUE                                                          IMP  670
      IF(ABW-EPSI1) 10,11,11                                            IMP  680
   11 DX = DX / 2.                                                      IMP  690
      IF(DABS(DX)/2. .GT. 1.0D-11) GOTO 113                             IMP  700
      DX = DX * 2.                                                      IMP  710
      WRITE (6,31) DX,EPSI1                                             IMP  720
      EPSI1 = EPSI1 * 10.                                               IMP  730
      EPSI2 = EPSI2 * 10.                                               IMP  740
      MM = 2                                                            IMP  750
      VFAKT = VFAKT * 10.                                               IMP  760
      GOTO 30                                                           IMP  770
  113 K = 2                                                             IMP  780
      X(3) = X(2)                                                       IMP  790
      Y(3) = Y(2)                                                       IMP  800
      X(2) = X(4)                                                       IMP  810
      Y(2) = Y(4)                                                       IMP  820
      X(4) = X(1) + DX / 2.                                             IMP  830
      X(5) = X(2) + DX / 2.                                             IMP  840
      IA = 4                                                            IMP  850
      IS = 1                                                            IMP  860
      GOTO 88                                                           IMP  870
   10 IF(ABW-EPSI2) 12,12,13                                            IMP  880
   12 GOTO(1,13),K                                                      IMP  890
    1 DX = 2. * DX                                                      IMP  900
      IF((X(6)-X(3))/(DX*(1.+1.0D-14)) .LE. 1.0)  GOTO 9                IMP  910
      X(4) = X(2)                                                       IMP  920
      Y(4) = Y(2)                                                       IMP  930
      X(2) = X(3)                                                       IMP  940
      Y(2) = Y(3)                                                       IMP  950
      X(3) = X(2) + DX                                                  IMP  960
      X(5) = X(2) + DX / 2.                                             IMP  970
      IA = 3                                                            IMP  980
      IS = 2                                                            IMP  990
      GOTO 88                                                           IMP 1000
    9 K = 2                                                             IMP 1010
      DX = (X(6)-X(1)) / 2.                                             IMP 1020
      GOTO 14                                                           IMP 1030
   13 K = 1                                                             IMP 1040
      GOTO(130,230),LAUF                                                IMP 1050
  130 LAUF = 2                                                          IMP 1060
      SCHRIT = DX                                                       IMP 1070
  230 FSIMP = FSIMP + F2                                                IMP 1080
      F1SIM = F1SIM + F1                                                IMP 1090
      X(1) = X(3)                                                       IMP 1100
      R = X(6) - X(1)                                                   IMP 1110
      IF(R/(2.0*DX) .GE. 1.0) GOTO 14                                   IMP 1120
      EINS = 1.                                                         IMP 1130
      IF(R*DSIGN(EINS,DX) .LT. 1.0D-14) GOTO 17                         IMP 1140
      DX = R / 2.                                                       IMP 1150
      GOTO 14                                                           IMP 1160
   17 L = 1                                                             IMP 1170
      GOTO 88                                                           IMP 1180
      END                                                               IMP 1190
      SUBROUTINE GEOV(NGEOM,RI,RO,XTM,VODFL,ZLTH,S,NEGFL)               EOV   10
C                                                                       EOV   20
      IMPLICIT REAL*8 (A-H,O-Z)                                         EOV   30
C                                                                       EOV   40
      REAL*4 POWER,PI,RINN,T,P,SS96,F,VOL,V,DV,VV,VDV,COZ,GIM,ALBEDO,   EOV   50
     1 DUM,A1,X1,RI,RO,XTM,VODFL,ZLTH,S                                 EOV   60
C                                                                       EOV   70
      DIMENSION XTM(10,30),Q(20,30),RA(20),PE(20),RO(20),FNA(100),      EOV   80
     1 FNB(100),ZLTH(NXS),ANORM(20),RISY(20),RI(20),GEW(10),STU(10),    EOV   90
     2 S(10,30),H(20,30),R(20,30),ENT(20,20),C(20,20),DX(20,600),       EOV  100
     3 DL(20,600)                                                       EOV  110
C                                                                       EOV  120
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    EOV  130
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    EOV  140
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIEOV  150
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 EOV  160
C                                                                       EOV  170
      EQUIVALENCE(JTPE3,NT)                                             EOV  180
C                                                                       EOV  190
      COMMON /BLOCKT/ T(20,20,30),P(30,30,10)                           EOV  200
C                                                                       EOV  210
      COMMON /BLOCRT/ ID,NBER,IX,MX,NX,ICOAT,MY,IBRENN,SS96(96,10),     EOV  220
     1 F(20,30),MTBL(20),VOL(20),V(30),DV(30),VV(30),VDV(30),MG(30),    EOV  230
     2 COZ(30,2),GIM(30,10),ALBEDO(30),LEAKT                            EOV  240
C                                                                       EOV  250
CFZJ055                                                       25.09.07  EOV  260
C                                                                       EOV  270
C *** THERMOS TRANSPORTKERN *** NEEF (ZYL.) - GERWIN/SCHERER (KUG.) *** EOV  280
C                                                                       EOV  290
C                                                                       EOV  300
      WEGMIN = 1.E-4                                                    EOV  310
      ISTG = 5                                                          EOV  320
      GEW(1) = 0.236926885                                              EOV  330
      GEW(2) = 0.478628670                                              EOV  340
      GEW(3) = 0.568888889                                              EOV  350
      GEW(4) = GEW(2)                                                   EOV  360
      GEW(5) = GEW(1)                                                   EOV  370
      STU(1) = 0.906179846                                              EOV  380
      STU(2) = 0.538469310                                              EOV  390
      STU(3) = 0.                                                       EOV  400
      STU(4) = -STU(2)                                                  EOV  410
      STU(5) = -STU(1)                                                  EOV  420
C                                                                       EOV  430
C     NGEOM = 1    SPHERICAL GEOMETRY                                   EOV  440
C     NGEOM .NE. 1 CYLINDRICAL GEOMETRY                                 EOV  450
C                                                                       EOV  460
      IF(IBUCK .LE. 0 .OR. IBUCK .EQ. 3) GOTO 499                       EOV  470
      IF(JN-NXE+IPRIN(15)) 499,497,497                                  EOV  480
  497 CONTINUE                                                          EOV  490
      NIRPI = IPRIN(7)                                                  EOV  500
      ALF = VODFL * ZLTH(NIRPI)                                         EOV  510
      ZLT = (1.-ALF) / (1.+ALF)                                         EOV  520
      IF(NEGFL .EQ. 1) ZLT = 1.                                         EOV  530
      DO 498 I=1,IX                                                     EOV  540
        ALBEDO(I) = ZLT                                                 EOV  550
  498 CONTINUE                                                          EOV  560
      IF(IPRIN(1) .GT. 0) WRITE (NT,8000) NIRPI,ZLT                     EOV  570
  499 CONTINUE                                                          EOV  580
      NXR = NX                                                          EOV  590
      OBER = 2. * 3.14159265 * RO(NX)                                   EOV  600
      IF(NGEOM .EQ. 1) OBER = OBER * 2. * RO(NX)                        EOV  610
C                                                                       EOV  620
C     BESTIMMUNG DES GEOM. WEGES DX(N,L) IN DEN GEBIETEN N FUER DIE     EOV  630
C     STUETZSTELLEN L                                                   EOV  640
C                                                                       EOV  650
      DO 9000 N=1,NXR                                                   EOV  660
        IE = N * ISTG                                                   EOV  670
        IA = IE - ISTG + 1                                              EOV  680
        ROP = RO(N)                                                     EOV  690
        RIP = RI(N)                                                     EOV  700
        IF(NGEOM .EQ. 0) GOTO 9001                                      EOV  710
        ROP = ROP * ROP                                                 EOV  720
        RIP = RIP * RIP                                                 EOV  730
 9001   DELTR = ROP - RIP                                               EOV  740
        DO 9000 L=IA,IE                                                 EOV  750
          ILL = L + ISTG * (1-N)                                        EOV  760
          RS = 0.5 * DELTR * (STU(ILL)+1.) + RIP                        EOV  770
          Y = 0.                                                        EOV  780
          DO 9000 NN=1,NXR                                              EOV  790
            IF(NGEOM) 9002,9002,9003                                    EOV  800
 9002       RON = RO(NN)                                                EOV  810
            RSQ = RS * RS                                               EOV  820
            ROQ = RON * RON                                             EOV  830
            GOTO 9004                                                   EOV  840
 9003       RON = RO(NN) * RO(NN)                                       EOV  850
            ROQ = RON                                                   EOV  860
            RSQ = RS                                                    EOV  870
 9004       IF(RON .LT. RS) GOTO 9010                                   EOV  880
            DX(NN,L) = SQRT(ROQ-RSQ) - Y                                EOV  890
            Y = Y + DX(NN,L)                                            EOV  900
            GOTO 9000                                                   EOV  910
 9010       DX(NN,L) = 0.                                               EOV  920
 9000 CONTINUE                                                          EOV  930
      DO 6000 I=1,IX                                                    EOV  940
        DO 6000 N=1,NX                                                  EOV  950
          DO 6000 K=1,NX                                                EOV  960
            T(N,K,I) = 0.                                               EOV  970
 6000 CONTINUE                                                          EOV  980
C                                                                       EOV  990
C     BEGINN DER DO-SCHLEIFE ENERGIE I                                  EOV 1000
C                                                                       EOV 1010
      DO 3310 I=1,IX                                                    EOV 1020
        DO 6037 N=1,NX                                                  EOV 1030
          DO 6037 K=1,NX                                                EOV 1040
            C(N,K) = 0.                                                 EOV 1050
            ENT(N,K) = 0.                                               EOV 1060
 6037   CONTINUE                                                        EOV 1070
C                                                                       EOV 1080
C     OPTISCHER WEG DL(K,L)                                             EOV 1090
C                                                                       EOV 1100
        LSTOP = NXR * ISTG                                              EOV 1110
        DO 3085 K=1,NXR                                                 EOV 1120
          RISY(K) = 0.                                                  EOV 1130
          M = MTBL(K)                                                   EOV 1140
          DO 3085 L=1,LSTOP                                             EOV 1150
            DL(K,L) = DX(K,L) * XTM(M,I)                                EOV 1160
 3085   CONTINUE                                                        EOV 1170
C                                                                       EOV 1180
C     BEGINN DER DO-SCHLEIFE QUELLGEBIET K                              EOV 1190
C                                                                       EOV 1200
        DO 3190 K=1,NX                                                  EOV 1210
          KIND = (K-1) * ISTG + 1                                       EOV 1220
          MK = MTBL(K)                                                  EOV 1230
          XTMK = XTM(MK,I)                                              EOV 1240
          KP1 = K + 1                                                   EOV 1250
C                                                                       EOV 1260
C     BEGINN DER DO-SCHLEIFE ZIEL N, N .GE. K                           EOV 1270
C                                                                       EOV 1280
          DO 3191 N=K,NX                                                EOV 1290
            NIND = (N-1) * ISTG + 1                                     EOV 1300
            PT = 0.                                                     EOV 1310
            PTR = 0.                                                    EOV 1320
            PR = 0.                                                     EOV 1330
            MN = MTBL(N)                                                EOV 1340
            XTMN = XTM(MN,I)                                            EOV 1350
C                                                                       EOV 1360
C     FALLUNTERSCHEIDUNGEN                                              EOV 1370
C                                                                       EOV 1380
            IF(DL(N,NIND) .LT. WEGMIN) GOTO 9100                        EOV 1390
            IF(DL(K,KIND) .LT. WEGMIN) GOTO 9110                        EOV 1400
            IF(N .EQ. K) GOTO 9120                                      EOV 1410
            IFALL = 1                                                   EOV 1420
            SVK = 1. / (XTMK*VOL(K))                                    EOV 1430
            GOTO 9200                                                   EOV 1440
 9100       IF(N .EQ. K) GOTO 9130                                      EOV 1450
            IF(DL(K,KIND) .LT. WEGMIN) GOTO 9140                        EOV 1460
            IFALL = 3                                                   EOV 1470
            SVK = 1. / (XTMK*VOL(N))                                    EOV 1480
            GOTO 9200                                                   EOV 1490
 9110       IFALL = 2                                                   EOV 1500
            SVK = 1. / VOL(K)                                           EOV 1510
            GOTO 9200                                                   EOV 1520
 9120       IFALL = 5                                                   EOV 1530
            SVK = 1. / (XTMK*VOL(K))                                    EOV 1540
            GOTO 9200                                                   EOV 1550
 9130       IFALL = 6                                                   EOV 1560
            SVK = 1. / VOL(K)                                           EOV 1570
            GOTO 9200                                                   EOV 1580
 9140       IFALL = 4                                                   EOV 1590
            SVK = 1. / VOL(N)                                           EOV 1600
 9200       GOTO(9210,9210,9220,9220,9210,9210),IFALL                   EOV 1610
 9220       ENT(N,K) = ENT(N-1,K)                                       EOV 1620
C                                                                       EOV 1630
C     BEGINN DER DO-SCHLEIFEN GAUSSINTEGRATION KK UND L                 EOV 1640
C                                                                       EOV 1650
 9210       DO 3192 KK=1,K                                              EOV 1660
              IE = KK * ISTG                                            EOV 1670
              IA = IE - ISTG + 1                                        EOV 1680
              ROP = RO(KK)                                              EOV 1690
              RIP = RI(KK)                                              EOV 1700
              IF(NGEOM .EQ. 0) GOTO 9211                                EOV 1710
              ROP = ROP * ROP                                           EOV 1720
              RIP = RIP * RIP                                           EOV 1730
 9211         DELTR = ROP - RIP                                         EOV 1740
              PTS = 0.                                                  EOV 1750
              PTRS = 0.                                                 EOV 1760
              PRS = 0.                                                  EOV 1770
              DO 3193 L=IA,IE                                           EOV 1780
                ILL = L + ISTG * (1-KK)                                 EOV 1790
                Y = GEW(ILL)                                            EOV 1800
C                                                                       EOV 1810
C     BESTIMMUNG DER OPTISCHEN WEGE TAU                                 EOV 1820
C                                                                       EOV 1830
                TMNK = 0.                                               EOV 1840
                TPNKM = 0.                                              EOV 1850
                TMNKM = 0.                                              EOV 1860
                TPNMKM = 0.                                             EOV 1870
                IKK = 0                                                 EOV 1880
                IF(K .EQ. 1 .OR. K .EQ. KK) IKK = 1                     EOV 1890
                GOTO(41,42,43,44,45,46),IFALL                           EOV 1900
   41           NN = N                                                  EOV 1910
                GOTO 76                                                 EOV 1920
   43           NN = N - 1                                              EOV 1930
   76           DO 70 M=KP1,NN                                          EOV 1940
                  TMNK = TMNK + DL(M,L)                                 EOV 1950
   70           CONTINUE                                                EOV 1960
   45           TY = 0.                                                 EOV 1970
                DO 71 M=1,K                                             EOV 1980
                  TY = TY + DL(M,L)                                     EOV 1990
   71           CONTINUE                                                EOV 2000
                TPNK = TMNK + 2. * TY                                   EOV 2010
                IF(IKK) 90,72,90                                        EOV 2020
   72           TPNKM = TPNK - DL(K,L)                                  EOV 2030
                TMNKM = TMNK + DL(K,L)                                  EOV 2040
                TPNMKM = TPNKM - DL(N,L)                                EOV 2050
                GOTO 90                                                 EOV 2060
   42           NN = N                                                  EOV 2070
                GOTO 79                                                 EOV 2080
   44           NN = N - 1                                              EOV 2090
   79           DO 73 M=KP1,NN                                          EOV 2100
                  TMNK = TMNK + DL(M,L)                                 EOV 2110
   73           CONTINUE                                                EOV 2120
                IF(IKK) 57,74,57                                        EOV 2130
   74           TY = 0.                                                 EOV 2140
                DO 75 M=1,K                                             EOV 2150
                  TY = TY + DL(M,L)                                     EOV 2160
   75           CONTINUE                                                EOV 2170
                TPNKM = TMNK + 2. * TY - DL(K,L)                        EOV 2180
                GOTO 90                                                 EOV 2190
   57           TPNKM = TMNK                                            EOV 2200
                GOTO 90                                                 EOV 2210
   46           CONTINUE                                                EOV 2220
                IF(IKK) 90,77,90                                        EOV 2230
   77           KM1 = K - 1                                             EOV 2240
                DO 78 M=1,KM1                                           EOV 2250
                  TPNMKM = TPNMKM + DL(M,L)                             EOV 2260
   78           CONTINUE                                                EOV 2270
                TPNMKM = 2. * TPNMKM                                    EOV 2280
   90           GOTO(51,52,53,54,55,56),IFALL                           EOV 2290
   51           TPNKM = F3(TPNKM,NGEOM)                                 EOV 2300
                TPNK = F3(TPNK,NGEOM)                                   EOV 2310
                TMNK = F3(TMNK,NGEOM)                                   EOV 2320
                TMNKM = F3(TMNKM,NGEOM)                                 EOV 2330
                PTS = PTS + Y * (TPNKM-TPNK+TMNK-TMNKM)                 EOV 2340
                GOTO 3193                                               EOV 2350
   52           TPNKM = F2(TPNKM,NGEOM)                                 EOV 2360
                TMNK = F2(TMNK,NGEOM)                                   EOV 2370
                PTS = PTS + Y * (TPNKM+TMNK) * DX(K,L)                  EOV 2380
                GOTO 3193                                               EOV 2390
   53           TPNKM = F2(TPNKM,NGEOM)                                 EOV 2400
                TPNK = F2(TPNK,NGEOM)                                   EOV 2410
                TMNK = F2(TMNK,NGEOM)                                   EOV 2420
                TMNKM = F2(TMNKM,NGEOM)                                 EOV 2430
                PTRS = PTRS + Y * (TPNKM-TPNK+TMNK-TMNKM) * DX(N,L)     EOV 2440
                GOTO 3193                                               EOV 2450
   54           TPNKM = F1(TPNKM,NGEOM)                                 EOV 2460
                TMNK = F1(TMNK,NGEOM)                                   EOV 2470
                PTRS = PTRS + Y * (TPNKM+TMNK) * DX(K,L) * DX(N,L)      EOV 2480
                GOTO 3193                                               EOV 2490
   55           TPNKM = F3(TPNKM,NGEOM)                                 EOV 2500
                TPNMKM = F3(TPNMKM,NGEOM)                               EOV 2510
                TPNK = F3(TPNK,NGEOM)                                   EOV 2520
                TMNK = F3(TMNK,NGEOM)                                   EOV 2530
                TMNKM = F3(TMNKM,NGEOM)                                 EOV 2540
                PTS = PTS + Y * (TPNKM-TPNK+TMNK-TMNKM)                 EOV 2550
                ROK = 2. * RO(K)                                        EOV 2560
                IF(NGEOM .EQ. 1) ROK = 0.78539816 * ROK * ROK           EOV 2570
                PRS = PRS + Y * (XTMK*VOL(K)/ROK+(TPNK+TPNMKM-2.*TPNKM- EOV 2580
     1           2.*(TMNK-TMNKM)))                                      EOV 2590
                GOTO 3193                                               EOV 2600
   56           TPNMK1 = F2(TPNMKM,NGEOM)                               EOV 2610
                TP1 = 1.                                                EOV 2620
                TPNMKM = F1(TPNMKM,NGEOM)                               EOV 2630
                TP = F1(0D0,NGEOM)                                      EOV 2640
                PTS = PTS + Y * (TPNMK1+TP1) * DX(K,L)                  EOV 2650
                PTRS = PTRS + Y * (TPNMKM+TP) * DX(K,L) * DX(N,L)       EOV 2660
 3193         CONTINUE                                                  EOV 2670
              GOTO(81,81,83,83,85,86),IFALL                             EOV 2680
   83         PTR = PTR + PTRS * DELTR                                  EOV 2690
              GOTO 3192                                                 EOV 2700
   85         PR = PR + PRS * DELTR                                     EOV 2710
              GOTO 81                                                   EOV 2720
   86         PTR = PTR + PTRS * DELTR                                  EOV 2730
   81         PT = PT + PTS * DELTR                                     EOV 2740
 3192       CONTINUE                                                    EOV 2750
            GOTO(91,91,93,93,95,96),IFALL                               EOV 2760
   93       PTR = SVK * PTR                                             EOV 2770
            IF(NGEOM .EQ. 1) PTR = PTR * 1.5707963                      EOV 2780
            T(N,K,I) = PTR                                              EOV 2790
            GOTO 3191                                                   EOV 2800
   95       PR = SVK * PR                                               EOV 2810
            IF(NGEOM .EQ. 1) PR = PR * 1.5707963                        EOV 2820
            RISY(K) = PR                                                EOV 2830
            GOTO 91                                                     EOV 2840
   96       PTR = SVK * PTR                                             EOV 2850
            IF(NGEOM .EQ. 1) PTR = PTR * 1.5707963                      EOV 2860
            T(N,K,I) = PTR                                              EOV 2870
            MK = MTBL(K)                                                EOV 2880
            RISY(K) = PTR * XTM(MK,I)                                   EOV 2890
   91       PT = PT * SVK                                               EOV 2900
            IF(NGEOM .EQ. 1) PT = PT * 1.5707963                        EOV 2910
            ENT(N,K) = PT                                               EOV 2920
 3191     CONTINUE                                                      EOV 2930
 3190   CONTINUE                                                        EOV 2940
        IF(IPRIN(1) .LE. 4) GOTO 4000                                   EOV 2950
        WRITE (NT,20) I                                                 EOV 2960
        DO 21 K=1,NX                                                    EOV 2970
          WRITE (NT,22) (N,K,ENT(N,K),N=K,NX)                           EOV 2980
   21   CONTINUE                                                        EOV 2990
 4000   DO 4030 K=1,NX                                                  EOV 3000
          DO 4030 N=K,NX                                                EOV 3010
            IF(N-K) 4040,4040,4050                                      EOV 3020
 4050       IF(T(N,K,I) .NE. 0.) GOTO 4051                              EOV 3030
            C(N,K) = ENT(N-1,K) - ENT(N,K)                              EOV 3040
            GOTO 4030                                                   EOV 3050
 4051       M = MTBL(N)                                                 EOV 3060
            C(N,K) = T(N,K,I) * XTM(M,I) * VOL(N) / VOL(K)              EOV 3070
            GOTO 4030                                                   EOV 3080
 4040       C(N,K) = RISY(K)                                            EOV 3090
 4030   CONTINUE                                                        EOV 3100
        IF(IPRIN(1) .LE. 4) GOTO 4001                                   EOV 3110
        WRITE (NT,23) I                                                 EOV 3120
        DO 24 K=1,NX                                                    EOV 3130
          WRITE (NT,22) (N,K,C(N,K),N=K,NX)                             EOV 3140
   24   CONTINUE                                                        EOV 3150
C                                                                       EOV 3160
C     REZIPROZITAET UND NORMIERUNG                                      EOV 3170
C                                                                       EOV 3180
 4001   IF(NX .EQ. 1) GOTO 67                                           EOV 3190
        NX1 = NX - 1                                                    EOV 3200
        DO 60 K=1,NX1                                                   EOV 3210
          KIND = (K-1) * ISTG + 1                                       EOV 3220
          MK = MTBL(K)                                                  EOV 3230
          SK = XTM(MK,I)                                                EOV 3240
          KP1 = K + 1                                                   EOV 3250
          DO 60 N=KP1,NX                                                EOV 3260
            NIND = (N-1) * ISTG + 1                                     EOV 3270
            IF(DL(K,KIND) .GT. 0.) GOTO 61                              EOV 3280
            C(K,N) = 0.                                                 EOV 3290
            GOTO 60                                                     EOV 3300
   61       MN = MTBL(N)                                                EOV 3310
            IF(DL(N,NIND) .GT. 0.) GOTO 60                              EOV 3320
            C(K,N) = T(N,K,I) * SK                                      EOV 3330
            IF(C(K,N) .LT. 0.) C(K,N) = 0.                              EOV 3340
   60   CONTINUE                                                        EOV 3350
   67   DO 5201 K=1,NX                                                  EOV 3360
          KIND = (K-1) * ISTG + 1                                       EOV 3370
          TY = 0.                                                       EOV 3380
          KM1 = K - 1                                                   EOV 3390
          MK = MTBL(K)                                                  EOV 3400
          KK = K                                                        EOV 3410
          IF(KM1) 5210,5220,5210                                        EOV 3420
 5210     IF(DL(K,KIND)) 5230,5230,5240                                 EOV 3430
 5230     KK = 1                                                        EOV 3440
          GOTO 5220                                                     EOV 3450
 5240     DO 5250 M=1,KM1                                               EOV 3460
            IF(C(M,K) .LT. 0.) C(M,K) = 0.                              EOV 3470
            TY = TY + C(M,K)                                            EOV 3480
 5250     CONTINUE                                                      EOV 3490
 5220     TY = 1. - TY                                                  EOV 3500
          AFAKT = 0.                                                    EOV 3510
          DO 5260 M=KK,NX                                               EOV 3520
            IF(C(M,K) .LT. 0.) C(M,K) = 0.                              EOV 3530
            AFAKT = AFAKT + C(M,K)                                      EOV 3540
 5260     CONTINUE                                                      EOV 3550
          TY = TY / (AFAKT+ENT(NX,K))                                   EOV 3560
          ANORM(K) = TY                                                 EOV 3570
          DO 5270 M=KK,NX                                               EOV 3580
            C(M,K) = C(M,K) * TY                                        EOV 3590
 5270     CONTINUE                                                      EOV 3600
          ENT(NX,K) = ENT(NX,K) * TY                                    EOV 3610
          KP1 = K + 1                                                   EOV 3620
          IF(KP1 .GT. NX) GOTO 5201                                     EOV 3630
          DO 5200 N=KP1,NX                                              EOV 3640
            MN = MTBL(N)                                                EOV 3650
            SN = XTM(MN,I)                                              EOV 3660
            IF(C(K,N)) 5285,5280,5200                                   EOV 3670
 5280       IF(SN .EQ. 0.) GOTO 5285                                    EOV 3680
            C(K,N) = C(N,K) * VOL(K) * XTM(MK,I) / (VOL(N)*SN)          EOV 3690
            GOTO 5200                                                   EOV 3700
 5285       C(K,N) = 0.                                                 EOV 3710
 5200     CONTINUE                                                      EOV 3720
 5201   CONTINUE                                                        EOV 3730
        IF(IPRIN(1) .LE. 4) GOTO 4271                                   EOV 3740
        WRITE (NT,62) I                                                 EOV 3750
        DO 63 K=1,NX                                                    EOV 3760
          WRITE (NT,22) (N,K,C(N,K),N=1,NX)                             EOV 3770
   63   CONTINUE                                                        EOV 3780
        WRITE (NT,65) I                                                 EOV 3790
        WRITE (NT,22) (NX,K,ENT(NX,K),K=1,NX)                           EOV 3800
        WRITE (NT,64)                                                   EOV 3810
        WRITE (NT,6043) (I,K,ANORM(K),K=1,NX)                           EOV 3820
C                                                                       EOV 3830
CFZJ060                                                       26.07.10  EOV 3840
 4271   CONTINUE                                                        EOV 3850
        DO 3250 K=1,NX                                                  EOV 3860
          KIND = (K-1) * ISTG + 1                                       EOV 3870
          M = MTBL(K)                                                   EOV 3880
          RA(K) = 4. * VOL(K) * XTM(M,I) * ENT(NX,K) / OBER             EOV 3890
 3250   CONTINUE                                                        EOV 3900
        IF(IPRIN(1) .LE. 4) GOTO 4272                                   EOV 3910
        WRITE (NT,3242)                                                 EOV 3920
        WRITE (NT,3243) (K,RA(K),K=1,NX)                                EOV 3930
 4272   T1 = 0.                                                         EOV 3940
        DO 3260 K=1,NX                                                  EOV 3950
          T1 = T1 + RA(K)                                               EOV 3960
 3260   CONTINUE                                                        EOV 3970
        IF(T1 .GT. 1.) T1 = 1.                                          EOV 3980
        T1 = 1. - (1.-T1) * ALBEDO(I)                                   EOV 3990
C                                                                       EOV 4000
        IF(T1 .LE. 0.) CALL ESTOP(100)                                  EOV 4010
C                                                                       EOV 4020
        DO 3280 K=1,NX                                                  EOV 4030
          DO 3280 N=1,NX                                                EOV 4040
            C(N,K) = C(N,K) + ALBEDO(I) * ENT(NX,K) * RA(N) / T1        EOV 4050
 3280   CONTINUE                                                        EOV 4060
        IF(IPRIN(1) .LE. 4) GOTO 4273                                   EOV 4070
        WRITE (NT,3281)                                                 EOV 4080
        DO 3282 K=1,NX                                                  EOV 4090
          WRITE (NT,22) (N,K,C(N,K),N=1,NX)                             EOV 4100
 3282   CONTINUE                                                        EOV 4110
 4273   CONTINUE                                                        EOV 4120
        DO 3300 K=1,NX                                                  EOV 4130
          DO 3300 N=1,NX                                                EOV 4140
            M = MTBL(N)                                                 EOV 4150
            IF(XTM(M,I) .EQ. 0.) GOTO 3304                              EOV 4160
            T(N,K,I) = C(N,K) * VOL(K) / (XTM(M,I)*VOL(N))              EOV 4170
 3305       T(K,N,I) = T(N,K,I) * VOL(N) / VOL(K)                       EOV 4180
            GOTO 3300                                                   EOV 4190
 3304       MK = MTBL(K)                                                EOV 4200
            IF(XTM(MK,I) .EQ. 0.) GOTO 3305                             EOV 4210
            T(K,N,I) = C(K,N) * VOL(N) / (XTM(MK,I)*VOL(K))             EOV 4220
            T(N,K,I) = T(K,N,I) * VOL(K) / VOL(N)                       EOV 4230
 3300   CONTINUE                                                        EOV 4240
        IF(IPRIN(1) .LE. 4) GOTO 4274                                   EOV 4250
        WRITE (NT,3301) I                                               EOV 4260
        DO 3303 K=1,NX                                                  EOV 4270
          WRITE (NT,22) (N,K,T(N,K,I),N=1,NX)                           EOV 4280
 3303   CONTINUE                                                        EOV 4290
 4274   CONTINUE                                                        EOV 4300
 3310 CONTINUE                                                          EOV 4310
      DO 8860 I=1,IX                                                    EOV 4320
        DO 8860 K=1,NX                                                  EOV 4330
          Q(K,I) = 0.                                                   EOV 4340
          DO 8840 N=1,NX                                                EOV 4350
            M = MTBL(N)                                                 EOV 4360
            Q(K,I) = Q(K,I) + VOL(N) * XTM(M,I) * T(N,K,I)              EOV 4370
 8840     CONTINUE                                                      EOV 4380
          Q(K,I) = Q(K,I) / VOL(K)                                      EOV 4390
 8860 CONTINUE                                                          EOV 4400
      IF(IPRIN(1) .LE. 4) GOTO 321                                      EOV 4410
      WRITE (NT,8865) ((K,I,Q(K,I),K=1,NX),I=1,IX)                      EOV 4420
      DO 336 M=1,MX                                                     EOV 4430
        WRITE (NT,3000) M,(I,S(M,I),I=1,IX)                             EOV 4440
  336 CONTINUE                                                          EOV 4450
      DO 331 M=1,MX                                                     EOV 4460
        WRITE (NT,3001) M,((I,J,P(I,J,M),J=1,IX),I=1,IX)                EOV 4470
  331 CONTINUE                                                          EOV 4480
      DO 332 I=1,IX                                                     EOV 4490
        WRITE (NT,3002) I,((N,K,T(N,K,I),K=1,NX),N=1,NX)                EOV 4500
  332 CONTINUE                                                          EOV 4510
      DO 334 M=1,MX                                                     EOV 4520
        WRITE (NT,3004) M,(I,XTM(M,I),I=1,IX)                           EOV 4530
  334 CONTINUE                                                          EOV 4540
  321 CONTINUE                                                          EOV 4550
      IF(IPRIN(1) .EQ. -1) RETURN                                       EOV 4560
      FNORM = 0.                                                        EOV 4570
      FNORM1 = 0.                                                       EOV 4580
      DO 390 I=1,IX                                                     EOV 4590
        VVD = V(I) * DV(I)                                              EOV 4600
        DO 390 N=1,NX                                                   EOV 4610
          M = MTBL(N)                                                   EOV 4620
          SV = S(M,I) * VOL(N) * VVD                                    EOV 4630
          FNORM1 = FNORM1 + Q(N,I) * SV                                 EOV 4640
          FNORM = FNORM + SV                                            EOV 4650
  390 CONTINUE                                                          EOV 4660
      FNORM = FNORM / FNORM1                                            EOV 4670
      WRITE (NT,392) FNORM                                              EOV 4680
      RETURN                                                            EOV 4690
C                                                                       EOV 4700
   20 FORMAT (/'  ESCAPE PROB., E-GR:',I3,' N,K,P(N<-K,I)'/)            EOV 4710
   22 FORMAT (6(I5,I3,E12.5))                                           EOV 4720
   23 FORMAT (/'  COLL. PR., E.GR.:',I3,'  ,N,K,C(N,K)'/)               EOV 4730
   62 FORMAT (/' NORM.COLL.Pr., E.GR.: ',I3,'  ,N,K,C(N,K)'/)           EOV 4740
   64 FORMAT (/' NORMALIZATION FACTORS, I, K, NORM(K):'/)               EOV 4750
   65 FORMAT (/' NORMALIZED P(NX,K,I),E.GR.:',I3/)                      EOV 4760
  392 FORMAT (/' EPITHERMAL NEUTRON SOURCE :',1PE15.4)                  EOV 4770
 3000 FORMAT (//'  REGION NR.  ',I2/' I,S(M,I):'/(6(I3,E12.5,2X)))      EOV 4780
 3001 FORMAT (//'  REGION NR.  ',I2/' I,J,P(I<-J,M):'/(6(2I3,E12.5,2X)))EOV 4790
 3002 FORMAT (//'  REGION NR.  ',I2/' N,K,T(N<-K,I):'/(6(2I3,E12.5,2X)))EOV 4800
 3004 FORMAT (//'  REGION NR.  ',I2/' I,XTM(M,I):'/(6(I3,E12.5,2X)))    EOV 4810
 3242 FORMAT (/' COLL. PROB. F. NEUTR. FROM NX+1   K,RA(K)'/)           EOV 4820
 3243 FORMAT (6(I8,E12.5))                                              EOV 4830
 3281 FORMAT (/' COLL. PROB. F. ALBEDO-BOUND.  N,K,C(N,K)'/)            EOV 4840
 3301 FORMAT (/' TRANSPORT KERNEL     N ,K, T(N<-K,I)  I=',I3/)         EOV 4850
 6043 FORMAT (10(2I3,1X,F6.4))                                          EOV 4860
 8000 FORMAT (/' ALBEDO (SPECTRUM ZONE:',I3,' ) = ',E12.5/)             EOV 4870
 8865 FORMAT (11H1K,I,Q(K,I)/(7(2I3,F9.5)))                             EOV 4880
      END                                                               EOV 4890
      FUNCTION F1(X,NGEOM)                                              F1    10
C                                                                       F1    20
C     KI1(X),EXP(-X)                                                    F1    30
C                                                                       F1    40
      IMPLICIT REAL*8 (A-H,O-Z)                                         F1    50
C                                                                       F1    60
C                                                                       F1    70
      IF(NGEOM .EQ. 0) GOTO 10                                          F1    80
C                                                                       F1    90
      F1 = EXP(-X)                                                      F1   100
C                                                                       F1   110
      RETURN                                                            F1   120
   10 CONTINUE                                                          F1   130
      IF(X .GT. 10.) GOTO 11                                            F1   140
      A = 0.24379971D-7 + X * (0.1467588796D-3+X*(0.05736867829+X*      F1   150
     1 (2.838501203+X*(24.28269118+X*(41.441359+X*(12.14837747))))))    F1   160
      B = 0.155208395D-7 + X * (0.9353689972D-4+X*(0.03696900541+X*     F1   170
     1 (1.930093672+X*(19.71226041+X*(52.67424655+X*(43.95298186+X*     F1   180
     2 (9.693354952)))))))                                              F1   190
C                                                                       F1   200
      F1 = SQRT(1.+X) * EXP(-X) * A / B                                 F1   210
C                                                                       F1   220
      RETURN                                                            F1   230
C                                                                       F1   240
   11 F1 = 0.                                                           F1   250
C                                                                       F1   260
      RETURN                                                            F1   270
      END                                                               F1   280
      FUNCTION F2(X,NGEOM)                                              F2    10
C                                                                       F2    20
C     KI2(X),EXP(-X)                                                    F2    30
C                                                                       F2    40
      IMPLICIT REAL*8 (A-H,O-Z)                                         F2    50
C                                                                       F2    60
C                                                                       F2    70
      IF(NGEOM .EQ. 0) GOTO 100                                         F2    80
C                                                                       F2    90
      F2 = EXP(-X)                                                      F2   100
C                                                                       F2   110
      RETURN                                                            F2   120
  100 CONTINUE                                                          F2   130
      IF(X .GT. 10.) GOTO 11                                            F2   140
      IF(X .EQ. 0.) GOTO 10                                             F2   150
      CB = EXP(-X)                                                      F2   160
C                                                                       F2   170
      F2 = CB / (1.+X*(0.454891+X*(-0.155068+X*(0.5191562D-1+X*         F2   180
     1 (-0.9179453D-2+X*0.6300736D-3)))))                               F2   190
C                                                                       F2   200
      RETURN                                                            F2   210
C                                                                       F2   220
   10 F2 = 1.                                                           F2   230
C                                                                       F2   240
      RETURN                                                            F2   250
C                                                                       F2   260
   11 F2 = 0.                                                           F2   270
C                                                                       F2   280
      RETURN                                                            F2   290
      END                                                               F2   300
      FUNCTION F3(X,NGEOM)                                              F3    10
C                                                                       F3    20
C     KI3(X),EXP(-X)                                                    F3    30
C                                                                       F3    40
      IMPLICIT REAL*8 (A-H,O-Z)                                         F3    50
C                                                                       F3    60
C                                                                       F3    70
      IF(NGEOM .EQ. 0) GOTO 10                                          F3    80
C                                                                       F3    90
      F3 = EXP(-X)                                                      F3   100
C                                                                       F3   110
      RETURN                                                            F3   120
   10 CONTINUE                                                          F3   130
      IF(X .GT. 10.) GOTO 6                                             F3   140
      IF(X .EQ. 0.) GOTO 1                                              F3   150
      IF(X .LE. 0.1) GOTO 2                                             F3   160
      IF(X .LE. 0.4) GOTO 3                                             F3   170
      IF(X .LE. 1.) GOTO 4                                              F3   180
      IF(X .LE. 2.5) GOTO 5                                             F3   190
      Y = 1. / (X+3.25)                                                 F3   200
      SY = SQRT(Y)                                                      F3   210
      CB = 1.268445824 * SY * EXP(-X)                                   F3   220
C                                                                       F3   230
      F3 = CB / (1.012074180+Y*(-0.325432D-3+Y*(-1.1646323+Y*(1.3873864 F3   240
     1 -Y*4.4655208))))                                                 F3   250
C                                                                       F3   260
      RETURN                                                            F3   270
C                                                                       F3   280
    2 F3 = 0.7366554521 / (0.9379388841+X*(1.194191634+X*(0.588245154+X F3   290
     1 *(0.570337193+X*(-1.5791166+X*4.292469)))))                      F3   300
C                                                                       F3   310
      RETURN                                                            F3   320
C                                                                       F3   330
    3 F3 = 0.5714977571 / (0.7276787064+X*(0.9254690857+X*(0.4741520763 F3   340
     1 +X*(0.250820355+X*(-0.025930075+X*0.055707999)))))               F3   350
C                                                                       F3   360
      RETURN                                                            F3   370
C                                                                       F3   380
    4 F3 = 0.3272473766 / (0.4166740874+X*(0.5295655111+X*(0.2754273045 F3   390
     1 +X*(0.1283775092+X*(0.0119191487+X*0.0139209543)))))             F3   400
C                                                                       F3   410
      RETURN                                                            F3   420
C                                                                       F3   430
    5 CB = (0.2215940159+X*(-0.09388379097+X*(0.0147382145-X*           F3   440
     1 0.857650032D-3)))                                                F3   450
C                                                                       F3   460
      F3 = CB / (0.2826723681+X*(0.2356320335+X*(0.06340205186+X*       F3   470
     1 0.01360032364)))                                                 F3   480
C                                                                       F3   490
      RETURN                                                            F3   500
C                                                                       F3   510
    1 F3 = 0.7366554521 / 0.9379388841                                  F3   520
C                                                                       F3   530
      RETURN                                                            F3   540
C                                                                       F3   550
    6 F3 = 0.                                                           F3   560
C                                                                       F3   570
      RETURN                                                            F3   580
      END                                                               F3   590
      SUBROUTINE ESTOP(K)                                               EST   10
C                                                                       EST   20
C     PRINT ERROR STOPS                                                 EST   30
C                                                                       EST   40
    1 FORMAT(/' *** PROGRAM STOPS BECAUSE OF ERROR NO. ',I4,' ***')     EST   50
C                                                                       EST   60
C                                                                       EST   70
      NTOUT = 6                                                         EST   80
      WRITE (NTOUT,1) K                                                 EST   90
      STOP                                                              EST  100
      END                                                               EST  110