      SUBROUTINE FLUX(NRGN,E1,B1,B2,B3,B4,B5,SCAC,CONC,P1,P2,DCONB,DCONRFLU   10
     1 ,SOUR,SCAT,PTSA,NCOMP,F1,SS1,SS2,SS3,SS4,SS5,SSC,SIG,HOL,ONEOV,  FLU   20
     2 PVOL,NJJR, BBND,BND,XI,XL,XII,IVX,JVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1FLU   30
     3 ,IVZ,KVZ,NSETVX,IOVX,IOVZ,A,MEMORY,AIO,IX3738,SPAR,BIEMS,NCRP,   FLU   40
     4 NSPA,ZONEN,NNXTRA,NVO)                                           FLU   50
C                                                                       FLU   60
CFLUX --095 ***CITATION*** EIGENVALUE-FLUX FOR 1,2-D/ CF-EIGN           FLU   70
C                                                                       FLU   80
      REAL*8 SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2S,XLL1,D8FLU   90
     1 ,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK,B5LK,D1, FLU  100
     2 D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,YRDB,SPR50FLU  110
     3 ,XLAST,P2,SCAT,SOUR,DU,B1,PNM,PNM0,PNM1,PNM2,BET(211),DEL(211),  FLU  120
     4 XII,XLAMDA,XAAMDA,TXX1,ZPDB,XMU3                                 FLU  130
C                                                                       FLU  140
      COMMON /ADUBP/ SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2SFLU  150
     1 ,XLL1,D8,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK, FLU  160
     2 B5LK,D1,D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,  FLU  170
     3 YRDB,SPR50,XLAST                                                 FLU  180
C                                                                       FLU  190
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,FLU  200
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   FLU  210
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), FLU  220
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    FLU  230
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    FLU  240
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   FLU  250
     6 IXPUT(9999),XPUT(9999)                                           FLU  260
C                                                                       FLU  270
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   FLU  280
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XELKFLU  290
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    FLU  300
     3 VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2,BETTA,SAMXI,IX25,IX28,I,J,KB,K,  FLU  310
     4 ITMAX,ITIME,BAT(211),DAL(211)                                    FLU  320
C                                                                       FLU  330
      COMMON /AKADD/ KAY(1),K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13, FLU  340
     1 K131,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,    FLU  350
     2 K28,K29,K30,K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,K41,K42,K43, FLU  360
     3 K44,K45,K46,K47,K48,K49,K50,K51,K52,K53,K54,K55,K56,K57,K58,     FLU  370
     4 K59,K60,K61,K62,K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,     FLU  380
     5 K74,K75,K76,K77,K78,K79,K80,K81,K82,K83,K84,K85,K86,K87,K88,     FLU  390
     6 K89,K90,K91,K92,K93,K94,K95,K96,K97,K98,K99,K100,NDATA,KNRGN,    FLU  400
     7 KNCOMP,KPVOL,KRVOL,MEMVRY,MEMX                                   FLU  410
C                                                                       FLU  420
      COMMON /BLOCK1/ IDUM(17),JTPE3                                    FLU  430
C                                                                       FLU  440
CFZJ055                                                       25.09.07  FLU  450
C                                                                       FLU  460
      COMMON /MU/ MU4                                                   FLU  470
C                                                                       FLU  480
      EQUIVALENCE(JTPE3,NT)                                             FLU  490
C                                                                       FLU  500
      DIMENSION E1(LVX,KVX),B1(MVX,KVX),B2(MVX,KVX),B3(MVX,KVX),        FLU  510
     1 SCAC(KVX,MVX,KVX),CONC(NVX,MVX),B4(MVX,KVX),B5(MVX, KVX),        FLU  520
     2 P1(JVX,IVX),P2(JVX,IVX,KVX),DCONB(JVX,IVXP1,IOVX),               FLU  530
     3 DCONR(JVXP1,IVZ,IOVZ),NRGN(JVX,IVX),SOUR(JVX,IVX),SCAT(JVX,IVX), FLU  540
     4 PTSA(JVX,IVX,IOVX),NCOMP(LVX),F1(KVX,MVX),SS1(KVX,NVX,NSETVX),   FLU  550
     5 SS2(KVX,NVX,NSETVX),SS3(KVX,NVX,NSETVX),SS4(KVX,NVX,NSETVX),     FLU  560
     6 SS5(KVX,NVX,NSETVX),SSC(KVX,KVX,NVX),HOL(NVX,NSETVX,10),         FLU  570
     7 ONEOV(KVX,NSETVX),PVOL(LVX),NJJR(NVX,NSETVX),SIG(KVX,MVX,10),    FLU  580
     8 BBND(KVX),BND(6,KVX),XI(KVX),XL(6,KVX),XII(KVX),AIO(IX3738),     FLU  590
     9 A(MEMORY),SPAR(NCRP,NSPA),BIEMS(KVX),ZONEN(NVO),                 FLU  600
     X NNXTRA(NVX,NSETVX)                                               FLU  610
C                                                                       FLU  620
C                                                                       FLU  630
      ISTART = ICLOCK(0)                                                FLU  640
      XLAMDA = XLAMDB                                                   FLU  650
      IX(129) = 0                                                       FLU  660
      SPARE(29) = 1.0                                                   FLU  670
      IF(IX(128) .GT. 0) SPARE(29) = 0.0                                FLU  680
      SPR50 = 1.0 / SPARE(50)                                           FLU  690
      SUMXI = 0.0                                                       FLU  700
      DO 101 K=1,KMAX                                                   FLU  710
        SUMXI = SUMXI + XI(K)                                           FLU  720
  101 CONTINUE                                                          FLU  730
      IX37 = IX(37)                                                     FLU  740
      IO19 = IX(86)                                                     FLU  750
      IO15 = IX(82)                                                     FLU  760
      IOADJ = IO15                                                      FLU  770
      IF(IX(71) .GT. 0) IOADJ = IO2                                     FLU  780
      IF(IX37 .GT. 0) REWIND IOADJ                                      FLU  790
C                                                                       FLU  800
      CALL BEGN(XL,P2,E1,XLAMDA,IVX,JVX,KVX,LVX)                        FLU  810
C                                                                       FLU  820
      XPDB = 1.0                                                        FLU  830
      YPDB = 1.0                                                        FLU  840
      BETTX = BETTA                                                     FLU  850
      IF(IX(198) .NE. 0) GOTO 102                                       FLU  860
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .EQ. 0) GOTO 103                   FLU  870
  102 CONTINUE                                                          FLU  880
      BETTX = 1.0                                                       FLU  890
      IEP = -1                                                          FLU  900
  103 CONTINUE                                                          FLU  910
      L211 = 211                                                        FLU  920
      DO 104 N=1,L211                                                   FLU  930
        BET(N) = 0.0                                                    FLU  940
        DEL(N) = 0.0                                                    FLU  950
  104 CONTINUE                                                          FLU  960
      IF(IX(5) .NE. -5) GOTO 106                                        FLU  970
      DO 105 K=1,KMAX                                                   FLU  980
        XII(K) = XI(K)                                                  FLU  990
        XI(K) = BIEMS(K)                                                FLU 1000
  105 CONTINUE                                                          FLU 1010
      IF(IX(132) .LE. 0) GOTO 106                                       FLU 1020
      IOFS = IX(84)                                                     FLU 1030
      REWIND IOFS                                                       FLU 1040
  106 CONTINUE                                                          FLU 1050
      IRR = IX(10)                                                      FLU 1060
      JRR = IX(11)                                                      FLU 1070
      KRR = IX(13)                                                      FLU 1080
      NUPDTE = 0                                                        FLU 1090
      IEND = 0                                                          FLU 1100
      KGP1 = KMAX + 1                                                   FLU 1110
      PNM0 = 0.0                                                        FLU 1120
      PNM1 = 0.0                                                        FLU 1130
C                                                                       FLU 1140
      CALL LOOP(P2,SOUR,NRGN,XII,IVX,JVX,KVX,LVX,XLAMDA,SIG,PVOL,NCOMP, FLU 1150
     1 MVX)                                                             FLU 1160
C                                                                       FLU 1170
      IF(MU4 .EQ. 1) NIT2 = NIT                                         FLU 1180
      NDELTA = 0                                                        FLU 1190
  107 CONTINUE                                                          FLU 1200
      PNM2 = PNM1                                                       FLU 1210
      PNM1 = PNM0                                                       FLU 1220
      YADX = XADX                                                       FLU 1230
      ZPDB = YPDB                                                       FLU 1240
      YPDB = XPDB                                                       FLU 1250
      YLEK = XLEK                                                       FLU 1260
      YS1S = CS1S                                                       FLU 1270
      YS2S = CS2S                                                       FLU 1280
      YS1DB = XS1DB                                                     FLU 1290
      YS2DB = XS2DB                                                     FLU 1300
      XMU3 = 0.0                                                        FLU 1310
      XADB = 0.0                                                        FLU 1320
      XPDB = 0.0                                                        FLU 1330
      XLEK = 0.0                                                        FLU 1340
      XRDB = 0.0                                                        FLU 1350
      XS1DB = 0.0                                                       FLU 1360
      XS2DB = 0.0                                                       FLU 1370
      IF(NUAC(5) .EQ. 10) GOTO 108                                      FLU 1380
      IF(IVX .NE. 1) GOTO 108                                           FLU 1390
      IF(IX(72) .GT. 0) GOTO 108                                        FLU 1400
C                                                                       FLU 1410
      CALL WFLX(P1,P2,SOUR,SCAT,SCAC,DCONR,PTSA,NRGN,E1,BET,DEL,XII,IVX,FLU 1420
     1 JVX,KVX,LVX,IVXP1,JVXP1,IVZ,KVZ,IOVX,IOVZ,SPAR,BIEMS,NCRP,NSPA,  FLU 1430
     2 SIG,PVOL,NCOMP,MVX,AIO,IX3738,XLAMDA,XI,XL,B2,IOADJ,IOFS,KGP1)   FLU 1440
C                                                                       FLU 1450
      GOTO 109                                                          FLU 1460
  108 CONTINUE                                                          FLU 1470
C                                                                       FLU 1480
      CALL DNSD(P1,P2,SOUR,NRGN,SCAC,SCAT,XII,DCONB,DCONR,PTSA,BET,DEL, FLU 1490
     1 E1,IVX,JVX,KVX,LVX,IVXP1,JVXP1,IVZ,KVZ,IOVX,IOVZ,SPAR,BIEMS,NCRP,FLU 1500
     2 NSPA,SIG,PVOL,NCOMP,MVX,AIO,IX3738,XLAMDA,XI,XL,B2,IOADJ,IOFS,   FLU 1510
     3 KGP1)                                                            FLU 1520
C                                                                       FLU 1530
  109 CONTINUE                                                          FLU 1540
      IF(NUAC(3) .GT. 0 .OR. IX(24) .GT. 0) GOTO 113                    FLU 1550
C                                                                       FLU 1560
      CALL ABPR(P2,B1,B2,B3,B4,B5,NRGN,IVX,JVX,KVX,LVX,SIG,PVOL,NCOMP,  FLU 1570
     1 MVX)                                                             FLU 1580
C                                                                       FLU 1590
      XELK = XLEK                                                       FLU 1600
      XKEF2 = XKEF1                                                     FLU 1610
      SPARE(43) = XLAMDA                                                FLU 1620
      XPDB = XPDB * SUMXI                                               FLU 1630
      IF(YPDB .NE. ZPDB) XMU3 = (XPDB-YPDB) / (YPDB-ZPDB)               FLU 1640
      XADX = XADB+XRDB                                                  FLU 1650
C                                                                       FLU 1660
C     SEARCH OPTIONS                                                    FLU 1670
C                                                                       FLU 1680
      IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 110                       FLU 1690
C                                                                       FLU 1700
      CALL GINS(B1,B3,B4,B5,KVX,LVX,XLAMDA,MVX)                         FLU 1710
C                                                                       FLU 1720
      IF(IX(5) .EQ. -5) GOTO 112                                        FLU 1730
      GOTO 111                                                          FLU 1740
  110 CONTINUE                                                          FLU 1750
      XLAMDA = (XLEK+XADX) / XPDB                                       FLU 1760
      XKEF1 = 1.0 / XLAMDA                                              FLU 1770
      VRGK1 = ABS(XKEF2/XKEF1-1.0)                                      FLU 1780
  111 CONTINUE                                                          FLU 1790
      XABS = XADX + XLAMDA * XS1DB                                      FLU 1800
      PROD = XPDB + XLAMDA * XS2DB                                      FLU 1810
      SPARE(48) = XS1DB                                                 FLU 1820
      SPARE(49) = XS2DB                                                 FLU 1830
      GOTO 113                                                          FLU 1840
  112 CONTINUE                                                          FLU 1850
      PROD = XPDB                                                       FLU 1860
      XABS = XADX                                                       FLU 1870
      IF(IX(132) .GT. 0) REWIND IOFS                                    FLU 1880
  113 CONTINUE                                                          FLU 1890
      IF(IX37 .GT. 0) REWIND IOADJ                                      FLU 1900
      IF(BETTA-1.0) 114,116,114                                         FLU 1910
  114 IF(IEP) 115,116,116                                               FLU 1920
  115 IEP = 1                                                           FLU 1930
  116 CONTINUE                                                          FLU 1940
      IX(134) = 0                                                       FLU 1950
      IF(NUAC(3) .GT. 0 .OR. IX(24) .GT. 0) GOTO 120                    FLU 1960
      XABT = ABS(XABS)                                                  FLU 1970
      IF(XABT .LT. 1.0E+38 .AND. XABT .GT. 1.0E-10) GOTO 120            FLU 1980
      UP = 1.0E-30                                                      FLU 1990
      IF(XABT .LT. 1.0E-10) UP = 1.0E+30                                FLU 2000
      IF(NI3 .LE. 3) GOTO 120                                           FLU 2010
      IF(VRGP2 .EQ. 0.0) GOTO 120                                       FLU 2020
      IF((VRGP1/VRGP2) .GE. 1.0) GOTO 120                               FLU 2030
      DO 119 K=1,KMAX                                                   FLU 2040
        DO 118 I=1,IMAX                                                 FLU 2050
          DO 117 J=1,JMAX                                               FLU 2060
            P2(J,I,K) = P2(J,I,K) * UP                                  FLU 2070
  117     CONTINUE                                                      FLU 2080
  118   CONTINUE                                                        FLU 2090
  119 CONTINUE                                                          FLU 2100
      PNM1 = PNM1 * UP                                                  FLU 2110
      PNM2 = PNM2 * UP                                                  FLU 2120
      IX(134) = 1                                                       FLU 2130
      IF(IX(5) .NE. -5) GOTO 120                                        FLU 2140
      XLAMDA = XLAMDA * UP                                              FLU 2150
  120 CONTINUE                                                          FLU 2160
      P2RR = P2(JRR,IRR,KRR)                                            FLU 2170
      IF(P2RR .GT. 0.0 .AND. P2RR .LT. 1.0E+38) GOTO 121                FLU 2180
      WRITE (IOUT,1003) JRR,IRR,KRR,P2RR                                FLU 2190
      IF(P2RR .LE. 0.0 .AND. NUAC(18) .GT. 0) GOTO 121                  FLU 2200
C                                                                       FLU 2210
      CALL EXIT                                                         FLU 2220
C                                                                       FLU 2230
  121 CONTINUE                                                          FLU 2240
      NUPDTE = NUPDTE + 1                                               FLU 2250
      IF(NSRH(11) .EQ. 0) NUPDTE = -1                                   FLU 2260
C                                                                       FLU 2270
      CALL LOOP(P2,SOUR,NRGN,XII,IVX,JVX,KVX,LVX,XLAMDA,SIG,PVOL,NCOMP, FLU 2280
     1 MVX)                                                             FLU 2290
C                                                                       FLU 2300
      IF(RMN .GE. 0.0) GOTO 122                                         FLU 2310
      IF(NUAC(18) .GT. 0) GOTO 122                                      FLU 2320
      WRITE (IOUT,1003)                                                 FLU 2330
C                                                                       FLU 2340
      CALL EXIT                                                         FLU 2350
C                                                                       FLU 2360
  122 CONTINUE                                                          FLU 2370
      PNM0 = P2(JRR,IRR,KRR)                                            FLU 2380
      P2DOM = 2.0 * PNM1 - PNM2 - PNM0                                  FLU 2390
      IF(P2DOM .NE. 0.0) PNM = (PNM0-PNM1) / P2DOM                      FLU 2400
      SPARE(35) = PNM                                                   FLU 2410
      IF(IX(5) .GE. 2 .AND. IX(73) .EQ. 2) GOTO 135                     FLU 2420
C                                                                       FLU 2430
      CALL EXTR                                                         FLU 2440
C                                                                       FLU 2450
      IX(129) = 0                                                       FLU 2460
      XT1 = SPARE(31)                                                   FLU 2470
      XT2 = SPARE(32)                                                   FLU 2480
      XT3 = SPARE(33)                                                   FLU 2490
      IF(IX(32)) 135,135,123                                            FLU 2500
  123 CONTINUE                                                          FLU 2510
      IEP = -1                                                          FLU 2520
      REWIND IO19                                                       FLU 2530
      DO 128 KT1=1,KMAX                                                 FLU 2540
        IF(IX(24) .GT. 0) GOTO 124                                      FLU 2550
        K = KT1                                                         FLU 2560
        GOTO 125                                                        FLU 2570
  124   K = KGP1 - KT1                                                  FLU 2580
  125   CONTINUE                                                        FLU 2590
        READ (IO19) P1                                                  FLU 2600
        DO 127 I=1,IMAX                                                 FLU 2610
          DO 126 J=1,JMAX                                               FLU 2620
            T1 = P1(J,I)                                                FLU 2630
            IF(IX(134) .EQ. 1) T1 = T1 * UP                             FLU 2640
            P2(J,I,K) = P2(J,I,K) + (P2(J,I,K)- T1) * EXFC1             FLU 2650
  126     CONTINUE                                                      FLU 2660
  127   CONTINUE                                                        FLU 2670
  128 CONTINUE                                                          FLU 2680
      REWIND IO19                                                       FLU 2690
      IF(NUAC(3) .GT. 0 .OR. IX(24) .GT. 0) GOTO 134                    FLU 2700
      D1 = XPDB + (XPDB-YPDB) * EXFC1                                   FLU 2710
      D2 = XADX + (XADX-YADX) * EXFC1                                   FLU 2720
      D3 = XLEK + (XLEK-YLEK) * EXFC1                                   FLU 2730
C                                                                       FLU 2740
C     SEARCH OPTIONS                                                    FLU 2750
C                                                                       FLU 2760
      IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 131                       FLU 2770
      IF(IX(5) .EQ. -5) GOTO 132                                        FLU 2780
      D6 = XS1DB + (XS1DB-YS1DB) * EXFC1                                FLU 2790
      D7 = XS2DB + (XS2DB-YS2DB) * EXFC1                                FLU 2800
      D8 = XLAMDA                                                       FLU 2810
      TL = D2 + D3 + D8 * D6                                            FLU 2820
      XKEF1 = (D1+D8*D7) / TL                                           FLU 2830
      XAAMDA = (SPR50*D1-D2-D3) / (D6-SPR50*D7)                         FLU 2840
      TXX1 = SPARE(51)                                                  FLU 2850
      IF(SPARE(51) .GT. 0.0) GOTO 129                                   FLU 2860
      XLAMDA = DMAX1(XAAMDA,TXX1)                                       FLU 2870
      GOTO 130                                                          FLU 2880
  129 CONTINUE                                                          FLU 2890
      XLAMDA = DMIN1(XAAMDA,TXX1)                                       FLU 2900
  130 CONTINUE                                                          FLU 2910
      GOTO 134                                                          FLU 2920
  131 CONTINUE                                                          FLU 2930
      XLAMDA = (D3+D2) / D1                                             FLU 2940
      XKEF1 = 1.0 / XLAMDA                                              FLU 2950
      GOTO 134                                                          FLU 2960
  132 CONTINUE                                                          FLU 2970
      D8 = (D3+D2-D1) / SPARE(88)                                       FLU 2980
      IF(D8 .GE. 0.0) GOTO 133                                          FLU 2990
      IF(D8 .LT. XLAMDA) GOTO 134                                       FLU 3000
  133 XLAMDA = D8                                                       FLU 3010
  134 CONTINUE                                                          FLU 3020
C                                                                       FLU 3030
      CALL LOOP(P2,SOUR,NRGN,XII,IVX,JVX,KVX,LVX,XLAMDA,SIG,PVOL,NCOMP, FLU 3040
     1 MVX)                                                             FLU 3050
C                                                                       FLU 3060
  135 CONTINUE                                                          FLU 3070
      IF(NUAC(3)) 137,137,136                                           FLU 3080
  136 CONTINUE                                                          FLU 3090
C                                                                       FLU 3100
      CALL RDUE(SCAC,RESLM,RESSA,P2,NRGN,SOUR,DCONR,DCONB,XI,IVX,JVX,KVXFLU 3110
     1 ,LVX,IVXP1,JVXP1,IVZ,KVZ,IOVX,IOVZ,A,MEMORY,AIO,IX3738,XLAMDA,SIGFLU 3120
     2 ,PVOL,NCOMP,MVX)                                                 FLU 3130
C                                                                       FLU 3140
  137 CONTINUE                                                          FLU 3150
      INOW = ICLOCK(0)                                                  FLU 3160
      XWACH = (FLOAT(INOW)-FLOAT(ISTART)) / 60.                         FLU 3170
      SPARE(66) = XWACH                                                 FLU 3180
      T1 = ABS(2.0*VRG1)                                                FLU 3190
      T2 = ABS(VRGABS)                                                  FLU 3200
      IF(T1-T2) 139,139,138                                             FLU 3210
  138 VRGABS = T1                                                       FLU 3220
  139 CONTINUE                                                          FLU 3230
C                                                                       FLU 3240
      CALL RQED(IX(101),IND)                                            FLU 3250
C                                                                       FLU 3260
      IF(IND .NE. 0) GOTO 140                                           FLU 3270
C                                                                       FLU 3280
      CALL ITED(XLAMDA,XMU3,BETTX,XT1,XT2,XT3)                          FLU 3290
C                                                                       FLU 3300
  140 CONTINUE                                                          FLU 3310
      IF(IX(5) .GE. 2 .AND. IX(73) .EQ. 2) GOTO 169                     FLU 3320
      IWACH = (INOW-ISTART) / 60                                        FLU 3330
      IS1 = 0                                                           FLU 3340
      IF(IX(6)) 143,143,142                                             FLU 3350
  142 NR = 16                                                           FLU 3360
      GOTO 161                                                          FLU 3370
  143 CONTINUE                                                          FLU 3380
      IF(IEND .GT. 0) GOTO 151                                          FLU 3390
      IF(IX(5) .NE. 1) GOTO 145                                         FLU 3400
      UPSIG = 0                                                         FLU 3410
      IF(NUPDTE .LT. NSRH(11)) GOTO 145                                 FLU 3420
      IF(NIIT .GT. 0) GOTO 145                                          FLU 3430
      NGOTO = 1                                                         FLU 3440
      NUPDTE = 0                                                        FLU 3450
  144 CONTINUE                                                          FLU 3460
C                                                                       FLU 3470
      CALL CNST(NRGN,SCAC,DCONB,DCONR,F1,SIG,PTSA,NCOMP,PVOL,BBND,BND,  FLU 3480
     1 IVX,JVX,KVX,LVX,MVX,IVXP1,JVXP1,IVZ,KVZ,IOVX,IOVZ,A,MEMORY,AIO,  FLU 3490
     2 IX3738)                                                          FLU 3500
C                                                                       FLU 3510
      IX(129) = 1                                                       FLU 3520
      UPSIG = 1                                                         FLU 3530
      GOTO(145,150),NGOTO                                               FLU 3540
  145 CONTINUE                                                          FLU 3550
      BETTX = BETTA                                                     FLU 3560
      IF(IEP .LT. 0) BETTX = 1.0                                        FLU 3570
      IF(NI3 .LT. 3) GOTO 146                                           FLU 3580
      IF(VRGP1 .GE. EPI1) GOTO 146                                      FLU 3590
      IF(VRGK1 .GE. EPI2) GOTO 146                                      FLU 3600
      GOTO 148                                                          FLU 3610
  146 IF(IWACH .GE. ITIME) GOTO 147                                     FLU 3620
      NDELTA = NIT - NIT2                                               FLU 3630
      IF(NDELTA .GE. ITMAX) GOTO 147                                    FLU 3640
      GOTO 107                                                          FLU 3650
  147 IS1 = 1                                                           FLU 3660
  148 CONTINUE                                                          FLU 3670
      NIT2 = NIT                                                        FLU 3680
C                                                                       FLU 3690
C     SEARCH OPTION                                                     FLU 3700
C                                                                       FLU 3710
      IF(IX(5) .NE. 1) GOTO 152                                         FLU 3720
      IF(UPSIG .EQ. 1) GOTO 150                                         FLU 3730
      NGOTO = 2                                                         FLU 3740
      GOTO 144                                                          FLU 3750
  150 IF(NSRH(11) .EQ. 0) GOTO 151                                      FLU 3760
      IEND = 1                                                          FLU 3770
      GOTO 107                                                          FLU 3780
  151 CONTINUE                                                          FLU 3790
  152 CONTINUE                                                          FLU 3800
      IF(IX(24) .EQ. 0) SPARE(56) = XLEK + XABS                         FLU 3810
C                                                                       FLU 3820
      CALL RQED(IX(101),IND)                                            FLU 3830
C                                                                       FLU 3840
      IF(IND .EQ. 0) GOTO 153                                           FLU 3850
C                                                                       FLU 3860
      CALL ITED(XLAMDA,XMU3,BETTX,XT1,XT2,XT3)                          FLU 3870
C                                                                       FLU 3880
  153 CONTINUE                                                          FLU 3890
C                                                                       FLU 3900
C     SEARCH OPTIONS                                                    FLU 3910
C                                                                       FLU 3920
      IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 154                       FLU 3930
      IF(IX(5) .EQ. -5) GOTO 156                                        FLU 3940
      GOTO 157                                                          FLU 3950
  154 CONTINUE                                                          FLU 3960
      IF(IX(24) .EQ. 0) GOTO 155                                        FLU 3970
      WRITE (IOUT,1004) XWACH                                           FLU 3980
      GOTO 157                                                          FLU 3990
  155 CONTINUE                                                          FLU 4000
      WRITE (NT,1005) XWACH,NDELTA+1                                    FLU 4010
      GOTO 157                                                          FLU 4020
  156 WRITE (IOUT,1000) XWACH,NDELTA+1                                  FLU 4030
  157 CONTINUE                                                          FLU 4040
      IF(IS1) 163,163,159                                               FLU 4050
  159 CONTINUE                                                          FLU 4060
      IF(NGC(15) .EQ. 0) GOTO 162                                       FLU 4070
      IF(NGC(15) .EQ. 2) GOTO 160                                       FLU 4080
      IF(VRGP2 .EQ. 0.0) GOTO 162                                       FLU 4090
      IF((VRGP1/VRGP2) .GE. 1.0) GOTO 160                               FLU 4100
      GOTO 162                                                          FLU 4110
  160 NR = 13                                                           FLU 4120
  161 WRITE (IOUT,1006) NR                                              FLU 4130
C                                                                       FLU 4140
      CALL EXIT                                                         FLU 4150
C                                                                       FLU 4160
  162 CONTINUE                                                          FLU 4170
      WRITE (IOUT,1001)                                                 FLU 4180
C                                                                       FLU 4190
      CALL EXIT                                                         FLU 4200
C                                                                       FLU 4210
  163 CONTINUE                                                          FLU 4220
      IF(NUAC(3) .EQ. 0) GOTO 164                                       FLU 4230
      WRITE (IOUT,1002)                                                 FLU 4240
C                                                                       FLU 4250
      CALL EXIT                                                         FLU 4260
C                                                                       FLU 4270
  164 IF(NGC(18)) 166,165,165                                           FLU 4280
  165 CONTINUE                                                          FLU 4290
      IF(IX(5) .EQ. -5) GOTO 166                                        FLU 4300
C                                                                       FLU 4310
      CALL RDUE(SCAC,RESLM,RESSA,P2,NRGN,SOUR,DCONR,DCONB,XI,IVX,JVX,KVXFLU 4320
     1 ,LVX,IVXP1,JVXP1,IVZ,KVZ,IOVX,IOVZ,A,MEMORY,AIO,IX3738,XLAMDA,SIGFLU 4330
     2 ,PVOL,NCOMP,MVX)                                                 FLU 4340
C                                                                       FLU 4350
      WRITE (NT,1008) RESSA,RESLM                                       FLU 4360
  166 CONTINUE                                                          FLU 4370
      XLAMDB = XLAMDA                                                   FLU 4380
      IF(IX(5) .NE. -5) GOTO 168                                        FLU 4390
      DO 167 K=1,KMAX                                                   FLU 4400
        BIEMS(K) = XI(K)                                                FLU 4410
        XI(K) = XII(K)                                                  FLU 4420
  167 CONTINUE                                                          FLU 4430
  168 CONTINUE                                                          FLU 4440
  169 CONTINUE                                                          FLU 4450
      RETURN                                                            FLU 4460
C                                                                       FLU 4470
 1000 FORMAT (1H0,'END OF FIXED SOURCE CALCULATION - ITERATION TIME',   FLU 4480
     1 0PF7.3,' MINUTES - NO. OF ITERATIONS =',I5)                      FLU 4490
 1001 FORMAT (1H0/1H0,'**********WARNING - FLUX CALCULATION NOT CONVERGEFLU 4500
     1D**********  PROGRAM STOPS.'/1H0/1H0)                             FLU 4510
 1002 FORMAT (1H0,'FLUX CALCULATION WAS DONE WITH THE RESIDUES - CALLINGFLU 4520
     1 EXIT NOW')                                                       FLU 4530
 1003 FORMAT (1H0,'ERROR STOP NUMBER 12',3I4,1PE13.5)                   FLU 4540
 1004 FORMAT (1H0,'END OF ADJOINT CALCULATION - ITERATION TIME',0PF7.3, FLU 4550
     1 ' MINUTES')                                                      FLU 4560
 1005 FORMAT (1H0,'END OF EIGENVALUE CALCULATION - ITERATION TIME',     FLU 4570
     1 0PF7.3,' MINUTES - NO. OF ITERATIONS =',I5)                      FLU 4580
 1006 FORMAT (1H0,'ERROR STOP',I3)                                      FLU 4590
 1008 FORMAT (1H0,'CONVERGENCE INDICATION BY MINIMIZING THE SUM OF THE SFLU 4600
     1QUARES OF THE RESIDUES - RELATIVE ABSORPTION',F11.7,'   K',F11.7) FLU 4610
      END                                                               FLU 4620
      SUBROUTINE KLUX(NRGNE,E1,B1,B2,B3,B4,B5,SCAC,CONC,P1E,P2E,DCONBE, KLU   10
     1 DCONRE,DCONBK,SOURE,SCATE,PTSAE,NCOMP,F1,SS1,SS2,SS3,SS4,SS5,SSC,KLU   20
     2 SIG,HOL,ONEOV,PVOL,NJJR,BBND,BND,XI,XL,XII,IVX,JVX,KBVX,KVX,LVX, KLU   30
     3 MVX,NVX,IVXP1,JVXP1,KBVXP1,IVZ,KVZ,NSETVX,JIVX,JIP1VX,JP1IXZ,IOVXKLU   40
     4 ,IOVZ,A,MEMORY,AIO,IX3738,SPAR,BIEMS,NCRP,NSPA,ZONEN,NNXTRA,NVO) KLU   50
C                                                                       KLU   60
CKLUX --111 ***CITATION*** EIGENVALUE-FLUX FOR 3-D/ CF-EIGN             KLU   70
C                                                                       KLU   80
      REAL*8 SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2S,XLL1,D8KLU   90
     1 ,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK,B5LK,D1, KLU  100
     2 D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,YRDB,SPR50KLU  110
     3 ,XLAST,B1,XLAMDA,XAAMDA,XII,TXX1,ZPDB,XMU3                       KLU  120
C                                                                       KLU  130
      COMMON /ADUBP/ SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2SKLU  140
     1 ,XLL1,D8,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK, KLU  150
     2 B5LK,D1,D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,  KLU  160
     3 YRDB,SPR50,XLAST                                                 KLU  170
C                                                                       KLU  180
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KLU  190
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KLU  200
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KLU  210
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KLU  220
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KLU  230
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KLU  240
     6 IXPUT(9999),XPUT(9999)                                           KLU  250
C                                                                       KLU  260
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   KLU  270
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XELKKLU  280
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    KLU  290
     3 VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2,BETTA,SAMXI,IX25,IX28,I,J,KB,K,  KLU  300
     4 ITMAX,ITIME,BET(211),DEL(211)                                    KLU  310
C                                                                       KLU  320
      COMMON /AKADD/ KAY(1),K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13, KLU  330
     1 K131,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,    KLU  340
     2 K28,K29,K30,K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,K41,K42,K43, KLU  350
     3 K44,K45,K46,K47,K48,K49,K50,K51,K52,K53,K54,K55,K56,K57,K58,     KLU  360
     4 K59,K60,K61,K62,K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,     KLU  370
     5 K74,K75,K76,K77,K78,K79,K80,K81,K82,K83,K84,K85,K86,K87,K88,     KLU  380
     6 K89,K90,K91,K92,K93,K94,K95,K96,K97,K98,K99,K100,NDATA,KNRGN,    KLU  390
     7 KNCOMP,KPVOL,KRVOL,MEMVRY,MEMX                                   KLU  400
C                                                                       KLU  410
CFZJ055                                                       25.09.07  KLU  420
C                                                                       KLU  430
      COMMON /MU/ MU4                                                   KLU  440
C                                                                       KLU  450
      COMMON /BLOCK1/ IDUM(17),JTPE3                                    KLU  460
C                                                                       KLU  470
      EQUIVALENCE(JTPE3,NT)                                             KLU  480
C                                                                       KLU  490
      DIMENSION NRGNE(JVX,IVX,KBVX),E1(LVX,KVX),B1(MVX,KVX),B2(MVX,KVX),KLU  500
     1 B3(MVX,KVX),SCAC(KVX,MVX,KVX),CONC(NVX,MVX),P1E(JIVX,KBVX),      KLU  510
     2 P2E(JIVX,KBVX,KVX),DCONBE(JIP1VX,KBVX,IOVX),                     KLU  520
     3 DCONRE(JP1IXZ,KBVX,IOVZ),DCONBK(JIVX,KBVXP1,IOVX),               KLU  530
     4 SOURE(JVX,IVX,KBVX),SCATE(JVX,IVX,KBVX),PTSAE(JIVX,KBVX,IOVX),   KLU  540
     5 B4(MVX,KVX),B5(MVX,KVX),NCOMP(LVX),F1(KVX,MVX),                  KLU  550
     6 SS1(KVX,NVX,NSETVX),SS2(KVX,NVX,NSETVX),SS3(KVX,NVX,NSETVX),     KLU  560
     7 SS4(KVX,NVX,NSETVX),SS5(KVX,NVX,NSETVX),SSC(KVX,KVX,NVX),        KLU  570
     8 HOL(NVX,NSETVX,10),ONEOV(KVX,NSETVX),PVOL(LVX),NJJR(NVX,NSETVX), KLU  580
     9 SIG(KVX,MVX,10),BBND(KVX),BND(6,KVX),XI(KVX),XL(6,KVX),XII(KVX), KLU  590
     X A(MEMORY),AIO(IX3738),SPAR(NCRP,NSPA),BIEMS(KVX),ZONEN(NVO),     KLU  600
     Y NNXTRA(NVX,NSETVX)                                               KLU  610
C                                                                       KLU  620
C                                                                       KLU  630
      ISTART = ICLOCK(0)                                                KLU  640
      XLAMDA = XLAMDB                                                   KLU  650
      IX(129) = 0                                                       KLU  660
      SPARE(29) = 1.0                                                   KLU  670
      IF(IX(128) .GT. 0) SPARE(29) = 0.0                                KLU  680
      SPR50 = 1.0 / SPARE(50)                                           KLU  690
      SUMXI = 0.0                                                       KLU  700
      DO 101 K=1,KMAX                                                   KLU  710
        SUMXI = SUMXI + XI(K)                                           KLU  720
  101 CONTINUE                                                          KLU  730
      IX37 = IX(37)                                                     KLU  740
      IO19 = IX(86)                                                     KLU  750
      IO15 = IX(82)                                                     KLU  760
      IOADJ = IO15                                                      KLU  770
      IF(IX(71) .GT. 0) IOADJ = IO2                                     KLU  780
      IF(IX37 .GT. 0) REWIND IOADJ                                      KLU  790
C                                                                       KLU  800
      CALL KEGN(XL,P2E,E1,XLAMDA,IVX,JVX,KBVX,KVX,LVX,JIVX)             KLU  810
C                                                                       KLU  820
      XPDB = 1.0                                                        KLU  830
      YPDB = 1.0                                                        KLU  840
      BETTX = BETTA                                                     KLU  850
      IF(IX(198) .NE. 0) GOTO 102                                       KLU  860
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .EQ. 0) GOTO 103                   KLU  870
  102 CONTINUE                                                          KLU  880
      BETTX = 1.0                                                       KLU  890
      IEP = -1                                                          KLU  900
  103 CONTINUE                                                          KLU  910
      IF(IX(5) .NE. -5) GOTO 105                                        KLU  920
      DO 104 K=1,KMAX                                                   KLU  930
        XII(K) = XI(K)                                                  KLU  940
        XI(K) = BIEMS(K)                                                KLU  950
  104 CONTINUE                                                          KLU  960
      IF(IX(132) .LE. 0) GOTO 105                                       KLU  970
      IOFS = IX(84)                                                     KLU  980
      REWIND IOFS                                                       KLU  990
  105 CONTINUE                                                          KLU 1000
      IRR = IX(10)                                                      KLU 1010
      JRR = IX(11)                                                      KLU 1020
      KBRR = IX(12)                                                     KLU 1030
      KRR = IX(13)                                                      KLU 1040
      NRR = (IRR-1) * JVX + JRR                                         KLU 1050
      NUPDTE = 0                                                        KLU 1060
      IEND = 0                                                          KLU 1070
      KGP1 = KMAX + 1                                                   KLU 1080
      PNM0 = 0.0                                                        KLU 1090
      PNM1 = 0.0                                                        KLU 1100
C                                                                       KLU 1110
      CALL KOOP(P2E,SOURE,NRGNE,XII,IVX,JVX,KBVX,KVX,LVX,JIVX,XLAMDA,SIGKLU 1120
     1 ,PVOL,NCOMP,MVX)                                                 KLU 1130
C                                                                       KLU 1140
      IF(MU4 .EQ. 1) NIT2 = NIT                                         KLU 1150
      NDELTA = 0                                                        KLU 1160
  106 CONTINUE                                                          KLU 1170
      PNM2 = PNM1                                                       KLU 1180
      PNM1 = PNM0                                                       KLU 1190
      YADX = XADX                                                       KLU 1200
      ZPDB = YPDB                                                       KLU 1210
      YPDB = XPDB                                                       KLU 1220
      YLEK = XLEK                                                       KLU 1230
      YS1S = CS1S                                                       KLU 1240
      YS2S = CS2S                                                       KLU 1250
      YS1DB = XS1DB                                                     KLU 1260
      YS2DB = XS2DB                                                     KLU 1270
      XMU3 = 0.0                                                        KLU 1280
      XADB = 0.0                                                        KLU 1290
      XPDB = 0.0                                                        KLU 1300
      XLEK = 0.0                                                        KLU 1310
      XRDB = 0.0                                                        KLU 1320
      XS1DB = 0.0                                                       KLU 1330
      XS2DB = 0.0                                                       KLU 1340
C                                                                       KLU 1350
      CALL KNSD(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,SOURE,NRGNE,XII,   KLU 1360
     1 SCAC,P1E,E1,IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,LVX,JIVX,JIP1VX, KLU 1370
     2 JP1IXZ,IOVX,IOVZ,SPAR,BIEMS,NCRP,NSPA,SIG,PVOL,NCOMP,MVX,AIO,    KLU 1380
     3 IX3738,XLAMDA,XI,XL,B2,IOADJ,IOFS,KGP1)                          KLU 1390
C                                                                       KLU 1400
      IF(NUAC(3) .GT. 0 .OR. IX(24) .GT. 0) GOTO 110                    KLU 1410
C                                                                       KLU 1420
      CALL KBPR(P2E,B1,B2,B3,B4,B5,NRGNE,JVX,IVX,KBVX,KVX,LVX,JIVX,SIG, KLU 1430
     1 PVOL,NCOMP,MVX)                                                  KLU 1440
C                                                                       KLU 1450
      XELK = XLEK                                                       KLU 1460
      XKEF2 = XKEF1                                                     KLU 1470
      SPARE(43) = XLAMDA                                                KLU 1480
      XPDB = XPDB * SUMXI                                               KLU 1490
      IF(YPDB .NE. ZPDB) XMU3 = (XPDB-YPDB) / (YPDB-ZPDB)               KLU 1500
      XADX = XADB + XRDB                                                KLU 1510
C                                                                       KLU 1520
C     SEARCH OPTIONS                                                    KLU 1530
C                                                                       KLU 1540
      IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 107                       KLU 1550
C                                                                       KLU 1560
      CALL GINS(B1,B3,B4,B5,KVX,LVX,XLAMDA,MVX)                         KLU 1570
C                                                                       KLU 1580
      IF(IX(5) .EQ. -5) GOTO 109                                        KLU 1590
      GOTO 108                                                          KLU 1600
  107 CONTINUE                                                          KLU 1610
      XLAMDA = (XLEK+XADX) / XPDB                                       KLU 1620
      XKEF1 = 1.0 / XLAMDA                                              KLU 1630
      VRGK1 = ABS(XKEF2/XKEF1-1.0)                                      KLU 1640
  108 CONTINUE                                                          KLU 1650
      XABS = XADX + XLAMDA * XS1DB                                      KLU 1660
      PROD = XPDB + XLAMDA * XS2DB                                      KLU 1670
      SPARE(48) = XS1DB                                                 KLU 1680
      SPARE(49) = XS2DB                                                 KLU 1690
      GOTO 110                                                          KLU 1700
  109 CONTINUE                                                          KLU 1710
      PROD = XPDB                                                       KLU 1720
      XABS = XADX                                                       KLU 1730
      IF(IX(132) .GT. 0) REWIND IOFS                                    KLU 1740
  110 CONTINUE                                                          KLU 1750
      IF(IX37 .GT. 0) REWIND IOADJ                                      KLU 1760
      IF(BETTA-1.0) 111,113,111                                         KLU 1770
  111 IF(IEP) 112,113,113                                               KLU 1780
  112 IEP = 1                                                           KLU 1790
  113 CONTINUE                                                          KLU 1800
      IX(134) = 0                                                       KLU 1810
      IF(NUAC(3) .GT. 0 .OR. IX(24) .GT. 0) GOTO 119                    KLU 1820
      XABT = ABS(XABS)                                                  KLU 1830
      IF(XABT .LT. 1.0E+38 .AND. XABT .GT. 1.0E-10) GOTO 119            KLU 1840
      UP = 1.0E-30                                                      KLU 1850
      IF(XABT .LT. 1.0E-10) UP = 1.0E+30                                KLU 1860
      IF(NI3 .LE. 3) GOTO 119                                           KLU 1870
      IF(VRGP2 .EQ. 0.0) GOTO 119                                       KLU 1880
      IF((VRGP1/VRGP2) .GE. 1.0) GOTO 119                               KLU 1890
      DO 118 K=1,KMAX                                                   KLU 1900
        DO 117 KB=1,KBMAX                                               KLU 1910
          N1 = 0                                                        KLU 1920
          DO 116 I=1,IMAX                                               KLU 1930
            DO 115 J=1,JMAX                                             KLU 1940
              N1 = N1 + 1                                               KLU 1950
              P2E(N1,KB,K) = P2E(N1,KB,K) * UP                          KLU 1960
  115       CONTINUE                                                    KLU 1970
  116     CONTINUE                                                      KLU 1980
  117   CONTINUE                                                        KLU 1990
  118 CONTINUE                                                          KLU 2000
      PNM1 = PNM1 * UP                                                  KLU 2010
      PNM2 = PNM2 * UP                                                  KLU 2020
      IX(134) = 1                                                       KLU 2030
      IF(IX(5) .NE. -5) GOTO 119                                        KLU 2040
      XLAMDA = XLAMDA * UP                                              KLU 2050
  119 CONTINUE                                                          KLU 2060
      P2RR = P2E(NRR,KBRR,KRR)                                          KLU 2070
      IF(P2RR .GT. 0.0 .AND. P2RR .LT. 1.0E+38) GOTO 120                KLU 2080
      WRITE (IOUT,1002) JRR,IRR,KBRR,KRR,P2RR                           KLU 2090
      IF(P2RR .LE. 0.0 .AND. NUAC(18) .GT. 0) GOTO 120                  KLU 2100
C                                                                       KLU 2110
      CALL EXIT                                                         KLU 2120
C                                                                       KLU 2130
  120 CONTINUE                                                          KLU 2140
      NUPDTE = NUPDTE + 1                                               KLU 2150
      IF(NSRH(11) .EQ. 0) NUPDTE = -1                                   KLU 2160
C                                                                       KLU 2170
      CALL KOOP(P2E,SOURE,NRGNE,XII,IVX,JVX,KBVX,KVX,LVX,JIVX,XLAMDA,SIGKLU 2180
     1 ,PVOL,NCOMP,MVX)                                                 KLU 2190
C                                                                       KLU 2200
      IF(RMN .GE. 0.0) GOTO 122                                         KLU 2210
      WRITE (IOUT,1002)                                                 KLU 2220
      IF(NUAC(18) .GT. 0) GOTO 122                                      KLU 2230
C                                                                       KLU 2240
      CALL EXIT                                                         KLU 2250
C                                                                       KLU 2260
  122 CONTINUE                                                          KLU 2270
      PNM0 = P2E(NRR,KBRR,KRR)                                          KLU 2280
      P2DOM = 2.0 * PNM1 - PNM2 - PNM0                                  KLU 2290
      IF(P2DOM .NE. 0.0) PNM = (PNM0-PNM1) / P2DOM                      KLU 2300
      SPARE(35) = PNM                                                   KLU 2310
      IF(IX(5) .GE. 2 .AND. IX(73) .EQ. 2) GOTO 136                     KLU 2320
C                                                                       KLU 2330
      CALL EXTR                                                         KLU 2340
C                                                                       KLU 2350
      IX(129) = 0                                                       KLU 2360
      XT1 = SPARE(31)                                                   KLU 2370
      XT2 = SPARE(32)                                                   KLU 2380
      XT3 = SPARE(33)                                                   KLU 2390
      IF(IX(32)) 136,136,123                                            KLU 2400
  123 CONTINUE                                                          KLU 2410
      REWIND IO19                                                       KLU 2420
      IEP = -1                                                          KLU 2430
      DO 129 KT1=1,KMAX                                                 KLU 2440
        IF(IX(24) .GT. 0) GOTO 124                                      KLU 2450
        K = KT1                                                         KLU 2460
        GOTO 125                                                        KLU 2470
  124   K = KGP1 - KT1                                                  KLU 2480
  125   CONTINUE                                                        KLU 2490
        READ (IO19) P1E                                                 KLU 2500
        DO 128 KB=1,KBMAX                                               KLU 2510
          N1 = 0                                                        KLU 2520
          DO 127 I=1,IMAX                                               KLU 2530
            DO 126 J=1,JMAX                                             KLU 2540
              N1 = N1 + 1                                               KLU 2550
              T1 = P1E(N1,KB)                                           KLU 2560
              IF(IX(134) .EQ. 1) T1 = T1 * UP                           KLU 2570
              P2E(N1,KB,K) = P2E(N1,KB,K) + (P2E(N1,KB,K)-T1) * EXFC1   KLU 2580
  126       CONTINUE                                                    KLU 2590
  127     CONTINUE                                                      KLU 2600
  128   CONTINUE                                                        KLU 2610
  129 CONTINUE                                                          KLU 2620
      REWIND IO19                                                       KLU 2630
      IF(NUAC(3) .GT. 0 .OR. IX(24) .GT. 0) GOTO 135                    KLU 2640
      D1 = XPDB + (XPDB-YPDB) * EXFC1                                   KLU 2650
      D2 = XADX + (XADX-YADX) * EXFC1                                   KLU 2660
      D3 = XLEK + (XLEK-YLEK) * EXFC1                                   KLU 2670
C                                                                       KLU 2680
C     SEARCH OPTIONS                                                    KLU 2690
C                                                                       KLU 2700
      IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 132                       KLU 2710
      IF(IX(5) .EQ. -5) GOTO 133                                        KLU 2720
      D6 = XS1DB + (XS1DB-YS1DB) * EXFC1                                KLU 2730
      D7 = XS2DB + (XS2DB-YS2DB) * EXFC1                                KLU 2740
      D8 = XLAMDA                                                       KLU 2750
      TL = D2 + D3 + D8 * D6                                            KLU 2760
      XKEF1 = (D1+D8*D7) / TL                                           KLU 2770
      XAAMDA = (SPR50*D1-D2-D3) / (D6-SPR50*D7)                         KLU 2780
      TXX1 = SPARE(51)                                                  KLU 2790
      IF(SPARE(51) .GT. 0.0) GOTO 130                                   KLU 2800
      XLAMDA = DMAX1(XAAMDA,TXX1)                                       KLU 2810
      GOTO 131                                                          KLU 2820
  130 XLAMDA = DMIN1(XAAMDA,TXX1)                                       KLU 2830
  131 CONTINUE                                                          KLU 2840
      GOTO 135                                                          KLU 2850
  132 CONTINUE                                                          KLU 2860
      XLAMDA = (D3+D2) / D1                                             KLU 2870
      XKEF1 = 1.0 / XLAMDA                                              KLU 2880
      GOTO 135                                                          KLU 2890
  133 CONTINUE                                                          KLU 2900
      D8 = (D3+D2-D1) / SPARE(88)                                       KLU 2910
      IF(D8 .GE. 0.0) GOTO 134                                          KLU 2920
      IF(D8 .LT. XLAMDA) GOTO 135                                       KLU 2930
  134 XLAMDA = D8                                                       KLU 2940
  135 CONTINUE                                                          KLU 2950
C                                                                       KLU 2960
      CALL KOOP(P2E,SOURE,NRGNE,XII,IVX,JVX,KBVX,KVX,LVX,JIVX,XLAMDA,SIGKLU 2970
     1 ,PVOL,NCOMP,MVX)                                                 KLU 2980
C                                                                       KLU 2990
  136 CONTINUE                                                          KLU 3000
      IF(NUAC(3)) 138,138,137                                           KLU 3010
  137 CONTINUE                                                          KLU 3020
C                                                                       KLU 3030
      CALL KDUE(SCAC,RESLM,RESSA,P2E,NRGNE,SOURE,DCONRE,DCONBE,DCONBK,XIKLU 3040
     1 ,IVX,JVX,KBVX,KVX,LVX,IVXP1,JVXP1,KBVXP1,IVZ,KVZ,JIVX,JIP1VX,    KLU 3050
     2 JP1IXZ,IOVX,IOVZ,A,MEMORY,AIO,IX3738, XLAMDA,SIG,PVOL,NCOMP,MVX) KLU 3060
C                                                                       KLU 3070
  138 CONTINUE                                                          KLU 3080
      INOW = ICLOCK(0)                                                  KLU 3090
      XWACH = (FLOAT(INOW)-FLOAT(ISTART)) / 60.                         KLU 3100
      SPARE(66) = XWACH                                                 KLU 3110
      T1 = ABS(2.0*VRG1)                                                KLU 3120
      T2 = ABS(VRGABS)                                                  KLU 3130
      IF(T1-T2) 140,140,139                                             KLU 3140
  139 VRGABS = T1                                                       KLU 3150
  140 CONTINUE                                                          KLU 3160
C                                                                       KLU 3170
      CALL RQED(IX(101),IND)                                            KLU 3180
C                                                                       KLU 3190
      IF(IND .NE. 0) GOTO 141                                           KLU 3200
C                                                                       KLU 3210
      CALL ITED(XLAMDA,XMU3,BETTX,XT1,XT2,XT3)                          KLU 3220
C                                                                       KLU 3230
  141 CONTINUE                                                          KLU 3240
      IF(IX(5) .GE. 2 .AND. IX(73) .EQ. 2) GOTO 169                     KLU 3250
      IWACH = (INOW-ISTART) / 60                                        KLU 3260
      IS1 = 0                                                           KLU 3270
      IF(IX(6)) 144,144,143                                             KLU 3280
  143 NR = 16                                                           KLU 3290
      GOTO 161                                                          KLU 3300
  144 CONTINUE                                                          KLU 3310
      IF(IEND .GT. 0) GOTO 152                                          KLU 3320
      IF(IX(5) .NE. 1) GOTO 146                                         KLU 3330
      UPSIG = 0                                                         KLU 3340
      IF(NUPDTE .LT. NSRH(11)) GOTO 146                                 KLU 3350
      IF(NIIT .GT. 0) GOTO 146                                          KLU 3360
      NGOTO = 1                                                         KLU 3370
      NUPDTE = 0                                                        KLU 3380
  145 CONTINUE                                                          KLU 3390
C                                                                       KLU 3400
      CALL KNST(NRGNE,SCAC,DCONBE,DCONRE,DCONBK,F1,SIG,PTSAE,NCOMP,PVOL,KLU 3410
     1 BBND,BND,IVX,JVX,KBVX,KVX,LVX,MVX,IVXP1,JVXP1,KBVXP1,IVZ,KVZ,JIVXKLU 3420
     2 ,JIP1VX,JP1IXZ,IOVX,IOVZ,A,MEMORY,AIO,IX3738)                    KLU 3430
C                                                                       KLU 3440
      IX(129) = 1                                                       KLU 3450
      UPSIG = 1                                                         KLU 3460
      GOTO(146,151),NGOTO                                               KLU 3470
  146 CONTINUE                                                          KLU 3480
      BETTX = BETTA                                                     KLU 3490
      IF(IEP .LT. 0) BETTX = 1.0                                        KLU 3500
      IF(NI3 .LT. 3) GOTO 147                                           KLU 3510
      IF(VRGP1 .GE. EPI1) GOTO 147                                      KLU 3520
      IF(VRGK1 .GE. EPI2) GOTO 147                                      KLU 3530
      GOTO 149                                                          KLU 3540
  147 IF(IWACH .GE. ITIME) GOTO 148                                     KLU 3550
      NDELTA = NIT - NIT2                                               KLU 3560
      IF(NDELTA .GE. ITMAX) GOTO 148                                    KLU 3570
      GOTO 106                                                          KLU 3580
  148 IS1 = 1                                                           KLU 3590
  149 CONTINUE                                                          KLU 3600
      NIT2 = NIT                                                        KLU 3610
C                                                                       KLU 3620
C     SEARCH OPTION                                                     KLU 3630
C                                                                       KLU 3640
      IF(IX(5) .NE. 1) GOTO 153                                         KLU 3650
      IF(UPSIG .EQ. 1) GOTO 151                                         KLU 3660
      NGOTO = 2                                                         KLU 3670
      GOTO 145                                                          KLU 3680
  151 IF(NSRH(11) .EQ. 0) GOTO 152                                      KLU 3690
      IEND = 1                                                          KLU 3700
      GOTO 106                                                          KLU 3710
  152 CONTINUE                                                          KLU 3720
  153 CONTINUE                                                          KLU 3730
      IF(IX(24) .EQ. 0) SPARE(56) = XLEK + XABS                         KLU 3740
C                                                                       KLU 3750
      CALL RQED(IX(101),IND)                                            KLU 3760
C                                                                       KLU 3770
      IF(IND .EQ. 0) GOTO 154                                           KLU 3780
C                                                                       KLU 3790
      CALL ITED(XLAMDA,XMU3,BETTX,XT1,XT2,XT3)                          KLU 3800
C                                                                       KLU 3810
  154 CONTINUE                                                          KLU 3820
C                                                                       KLU 3830
C     SEARCH OPTIONS                                                    KLU 3840
C                                                                       KLU 3850
      IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 155                       KLU 3860
      IF(IX(5) .EQ. -5) GOTO 157                                        KLU 3870
      GOTO 158                                                          KLU 3880
  155 CONTINUE                                                          KLU 3890
      IF(IX(24) .EQ. 0) GOTO 156                                        KLU 3900
      WRITE (IOUT,1005) XWACH                                           KLU 3910
      GOTO 158                                                          KLU 3920
  156 CONTINUE                                                          KLU 3930
      WRITE (NT,1006) XWACH,NDELTA+1                                    KLU 3940
      GOTO 158                                                          KLU 3950
  157 WRITE (IOUT,1000) XWACH,NDELTA+1                                  KLU 3960
  158 CONTINUE                                                          KLU 3970
      IF(IS1) 163,163,159                                               KLU 3980
  159 CONTINUE                                                          KLU 3990
      IF(NGC(15) .EQ. 0) GOTO 162                                       KLU 4000
      IF(NGC(15) .EQ. 2) GOTO 160                                       KLU 4010
      IF(VRGP2 .EQ. 0.0) GOTO 162                                       KLU 4020
      IF((VRGP1/VRGP2) .GE. 1.0) GOTO 160                               KLU 4030
      GOTO 162                                                          KLU 4040
  160 NR = 13                                                           KLU 4050
  161 WRITE (IOUT,1007) NR                                              KLU 4060
C                                                                       KLU 4070
      CALL EXIT                                                         KLU 4080
C                                                                       KLU 4090
  162 CONTINUE                                                          KLU 4100
      WRITE (IOUT,1010)                                                 KLU 4110
C                                                                       KLU 4120
      CALL EXIT                                                         KLU 4130
C                                                                       KLU 4140
  163 CONTINUE                                                          KLU 4150
      IF(NUAC(3) .EQ. 0) GOTO 164                                       KLU 4160
      WRITE (IOUT,1001)                                                 KLU 4170
C                                                                       KLU 4180
      CALL EXIT                                                         KLU 4190
C                                                                       KLU 4200
  164 IF(NGC(18)) 166,165,165                                           KLU 4210
  165 CONTINUE                                                          KLU 4220
      IF(IX(5) .EQ. -5) GOTO 166                                        KLU 4230
C                                                                       KLU 4240
      CALL KDUE(SCAC,RESLM,RESSA,P2E,NRGNE,SOURE,DCONRE,DCONBE,DCONBK,XIKLU 4250
     1 ,IVX,JVX,KBVX,KVX,LVX,IVXP1,JVXP1,KBVXP1,IVZ,KVZ,JIVX,JIP1VX,    KLU 4260
     2 JP1IXZ,IOVX,IOVZ,A,MEMORY,AIO,IX3738,XLAMDA,SIG,PVOL,NCOMP,MVX)  KLU 4270
C                                                                       KLU 4280
      WRITE (NT,1009) RESSA,RESLM                                       KLU 4290
  166 CONTINUE                                                          KLU 4300
      XLAMDB = XLAMDA                                                   KLU 4310
      IF(IX(5) .NE. -5) GOTO 168                                        KLU 4320
      DO 167 K=1,KMAX                                                   KLU 4330
        BIEMS(K) = XI(K)                                                KLU 4340
        XI(K) = XII(K)                                                  KLU 4350
  167 CONTINUE                                                          KLU 4360
  168 CONTINUE                                                          KLU 4370
  169 CONTINUE                                                          KLU 4380
      RETURN                                                            KLU 4390
C                                                                       KLU 4400
 1000 FORMAT (1H0,'END OF FIXED SOURCE CALCULATION - ITERATION TIME',   KLU 4410
     1 0PF7.3,' MINUTES - NO. OF ITERATIONS =',I5)                      KLU 4420
 1001 FORMAT (1H0,'FLUX CALCULATION WAS DONE WITH THE RESIDUES - NOW CALKLU 4430
     1L EXIT')                                                          KLU 4440
 1002 FORMAT (1H0,'ERROR STOP NUMBER 12',4I4,1PE13.5)                   KLU 4450
 1005 FORMAT (1H0,'END OF ADJOINT CALCULATION - ITERATION TIME',0PF7.3, KLU 4460
     1 ' MINUTES')                                                      KLU 4470
 1006 FORMAT (1H0,'END OF EIGENVALUE CALCULATION - ITERATION TIME',     KLU 4480
     1 0PF7.3,' MINUTES - NO. OF ITERATIONS =',I5)                      KLU 4490
 1007 FORMAT (1H0,'ERROR STOP',I3)                                      KLU 4500
 1009 FORMAT (1H0,'CONVERGENCE INDICATION BY MINIMIZING THE SUM OF THE SKLU 4510
     1QUARES OF THE RESIDUES - RELATIVE ABSORPTION',F11.7,'   K',F11.7) KLU 4520
 1010 FORMAT (1H0/1H0,'**********WARNING - FLUX CALCULATION NOT CONVERGEKLU 4530
     1D**********  PROGRAM STOPS.'/1H0/1H0)                             KLU 4540
      END                                                               KLU 4550
      SUBROUTINE RODI(NRGNE,NCOMP,JVX,IVX,KBVX,LVX,MJJR,NSETVX)         ROD   10
C                                                                       ROD   20
CRODI --42.1 ***CITATION*** SECTION 030 INPUT ROUTINE     /CF-IPTM      ROD   30
C                                                                       ROD   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,ROD   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   ROD   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), ROD   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    ROD   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    ROD   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   ROD  100
     6 IXPUT(9999),XPUT(9999)                                           ROD  110
C                                                                       ROD  120
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           ROD  130
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    ROD  140
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),ROD  150
     3 NCLASS(9999),NDP(9999)                                           ROD  160
C                                                                       ROD  170
CFZJ055                                                       25.09.07  ROD  180
C                                                                       ROD  190
      DIMENSION ICRP(24),ICRPS(24),NRGNE(JVX,IVX,KBVX),NCOMP(LVX),      ROD  200
     1 MJJR(9999,NSETVX),IDPSR(200),IDRLR(200),ITEMP(200)               ROD  210
C                                                                       ROD  220
      EQUIVALENCE(IDPSR(1),XTR1(1)),(IDRLR(1),XTR2(1))                  ROD  230
C                                                                       ROD  240
C                                                                       ROD  250
      WRITE (IOUT,1002)                                                 ROD  260
      IO10 = IX(77)                                                     ROD  270
      REWIND IO10                                                       ROD  280
  100 READ (IO10,END=101)                                               ROD  290
      GOTO 100                                                          ROD  300
  101 BACKSPACE IO10                                                    ROD  310
      REWIND IO3                                                        ROD  320
      NERR = 0                                                          ROD  330
      NIDR = 0                                                          ROD  340
      NROC = 0                                                          ROD  350
      DO 102 I=1,200                                                    ROD  360
        IDPSR(I) = 0                                                    ROD  370
        IDRLR(I) = 0                                                    ROD  380
  102 CONTINUE                                                          ROD  390
      NGEOM = NUAC(5)                                                   ROD  400
C                                                                       ROD  410
CARD 30-2                                                               ROD  420
C                                                                       ROD  430
      READ (IOIN,1000) (ICRP(I),I=1,24)                                 ROD  440
C                                                                       ROD  450
      WRITE (IOUT,1003) (ICRP(I),I=1,24)                                ROD  460
      IF(ICRP(1) .EQ. 0) GOTO 104                                       ROD  470
      IF(ICRP(2) .EQ. 0) GOTO 104                                       ROD  480
      NN = 0                                                            ROD  490
      DO 103 I=3,12                                                     ROD  500
        IF(ICRP(I) .EQ. 0) GOTO 103                                     ROD  510
        NN = NN + 1                                                     ROD  520
        IF(ICRP(I) .LT. 1 .OR. ICRP(I) .GT. 200) NERR = 6               ROD  530
  103 CONTINUE                                                          ROD  540
      IF(NN .GT. 0 .AND. NERR .EQ. 0) GOTO 104                          ROD  550
      WRITE (IOUT,1007)                                                 ROD  560
  104 CONTINUE                                                          ROD  570
      IF(ICRP(1) .NE. 0) GOTO 109                                       ROD  580
C                                                                       ROD  590
CARD(S) 30-3                                                            ROD  600
C                                                                       ROD  610
  105 READ (IOIN,1000) IDCR1,IDCR2,IDJL,IDJR,IDIT,IDIB,IDPF,IDPB        ROD  620
C                                                                       ROD  630
      IF(IDCR1 .EQ. 0) GOTO 109                                         ROD  640
      WRITE (IOUT,1003) IDCR1,IDCR2,IDJL,IDJR,IDIT,IDIB,IDPF,IDPB       ROD  650
      NIDR = NIDR + 1                                                   ROD  660
C                                                                       ROD  670
      CALL MYSH(IDJL,IDJR,IDIT,IDIB,IDPF,IDPB,JMAX,IMAX,KBMAX,NGEOM,IND,ROD  680
     1 IOUT)                                                            ROD  690
C                                                                       ROD  700
      IF(IND .GT. 0) NERR = 1                                           ROD  710
      LR = NRGNE(IDJL,IDIT,IDPF)                                        ROD  720
      DO 108 KB=IDPF,IDPB                                               ROD  730
        DO 107 I=IDIT,IDIB                                              ROD  740
          DO 106 J=IDJL,IDJR                                            ROD  750
            IF(NRGNE(J,I,KB) .EQ. LR) GOTO 106                          ROD  760
            NERR = 3                                                    ROD  770
            WRITE (IOUT,1004) IDCR1                                     ROD  780
  106     CONTINUE                                                      ROD  790
  107   CONTINUE                                                        ROD  800
  108 CONTINUE                                                          ROD  810
      IDPSR(NIDR) = IDCR1                                               ROD  820
      IDRLR(NIDR) = LR                                                  ROD  830
      GOTO 105                                                          ROD  840
  109 CONTINUE                                                          ROD  850
  110 CONTINUE                                                          ROD  860
      NSPEC = 0                                                         ROD  870
      I2 = 0                                                            ROD  880
  111 I1 = I2 + 1                                                       ROD  890
      I2 = I1 + 23                                                      ROD  900
C                                                                       ROD  910
CARD(S) 30-4                                                            ROD  920
C                                                                       ROD  930
      READ (IOIN,1000) (NXTR1(I),I=I1,I2)                               ROD  940
C                                                                       ROD  950
      IF(NXTR1(1) .EQ. 0) GOTO 123                                      ROD  960
      IF(NXTR1(I2) .EQ. 0 .AND. NXTR1(I2-1) .EQ. 0) GOTO 112            ROD  970
      GOTO 111                                                          ROD  980
  112 CONTINUE                                                          ROD  990
      I3 = 3                                                            ROD 1000
      I4 = I2 - 1                                                       ROD 1010
      DO 113 I=I3,I4                                                    ROD 1020
        IF(NXTR1(I) .NE. 0) GOTO 113                                    ROD 1030
        NSPEC = NSPEC + 1                                               ROD 1040
        IF(NXTR1(I+1) .NE. 0) GOTO 113                                  ROD 1050
        IT = I - 1                                                      ROD 1060
        GOTO 114                                                        ROD 1070
  113 CONTINUE                                                          ROD 1080
      NERR = 2                                                          ROD 1090
  114 CONTINUE                                                          ROD 1100
      WRITE (IOUT,1001) (NXTR1(I),I=1,IT)                               ROD 1110
      IF(ICRP(1) .NE. 0) GOTO 118                                       ROD 1120
      DO 117 I=3,IT                                                     ROD 1130
        LR = NXTR1(I)                                                   ROD 1140
        IF(LR .EQ. 0) GOTO 117                                          ROD 1150
        DO 115 II=1,NIDR                                                ROD 1160
          IF(IDPSR(II) .NE. LR) GOTO 115                                ROD 1170
          LRR = IDRLR(II)                                               ROD 1180
          GOTO 116                                                      ROD 1190
  115   CONTINUE                                                        ROD 1200
        NERR = 4                                                        ROD 1210
        WRITE (IOUT,1005) LR                                            ROD 1220
        GOTO 117                                                        ROD 1230
  116   NXTR1(I) = LRR                                                  ROD 1240
  117 CONTINUE                                                          ROD 1250
      IF(ICRP(24) .GT. 0) WRITE (IOUT,1001) (NXTR1(I),I=1,IT)           ROD 1260
  118 CONTINUE                                                          ROD 1270
      IF(ICRP(24) .GT. 0) WRITE (IOUT,1001) NSPEC,IT                    ROD 1280
      IF(ICRP(1) .EQ. 0) GOTO 122                                       ROD 1290
      DO 119 I=2,IT                                                     ROD 1300
        IF(NXTR1(I) .LE. MMAX) GOTO 119                                 ROD 1310
        NERR = 9                                                        ROD 1320
        WRITE (IOUT,1010) NXTR1(I),MMAX                                 ROD 1330
  119 CONTINUE                                                          ROD 1340
      IF(ICRP(2) .EQ. 0) GOTO 122                                       ROD 1350
      DO 121 I=2,IT                                                     ROD 1360
        M = NXTR1(I)                                                    ROD 1370
        IF(M .EQ. 0) GOTO 121                                           ROD 1380
        NS = NXSET(M)                                                   ROD 1390
        NR = NXODR(NS)                                                  ROD 1400
        DO 120 N=3,12                                                   ROD 1410
          NN = ICRP(N)                                                  ROD 1420
          IF(NN .EQ. 0) GOTO 120                                        ROD 1430
          IF(MJJR(NN,NR) .GT. 0) GOTO 120                               ROD 1440
          WRITE (IOUT,1009) NN,M                                        ROD 1450
  120   CONTINUE                                                        ROD 1460
  121 CONTINUE                                                          ROD 1470
  122 CONTINUE                                                          ROD 1480
      NROC = NROC + 1                                                   ROD 1490
      ITEMP(NROC) = NXTR1(1)                                            ROD 1500
      WRITE (IO3) NXTR1(1),IT,NSPEC,(NXTR1(I),I=1,IT)                   ROD 1510
      GOTO 110                                                          ROD 1520
  123 CONTINUE                                                          ROD 1530
      REWIND IO3                                                        ROD 1540
C                                                                       ROD 1550
CARD 30-5                                                               ROD 1560
C                                                                       ROD 1570
      READ (IOIN,1000) (ICRPS(I),I=1,24)                                ROD 1580
C                                                                       ROD 1590
      WRITE (IOUT,1001) (ICRPS(I),I=1,24)                               ROD 1600
      IF(NDPL(2) .LE. 24) GOTO 124                                      ROD 1610
      NERR = 7                                                          ROD 1620
  124 IF(NDPL(1) .LE. 1) GOTO 126                                       ROD 1630
      IF(NDPL(3) .LE. 24) GOTO 125                                      ROD 1640
      NERR = 7                                                          ROD 1650
  125 IF(NDPL(1) .LE. 2) GOTO 126                                       ROD 1660
      IF(NDPL(4) .LE. 24) GOTO 126                                      ROD 1670
      NERR = 7                                                          ROD 1680
  126 CONTINUE                                                          ROD 1690
      IF(NERR .EQ. 7) WRITE (IOUT,1008)                                 ROD 1700
      DO 129 I=1,24                                                     ROD 1710
        IC = ICRPS(I)                                                   ROD 1720
        IF(IC .EQ. 0) GOTO 129                                          ROD 1730
        DO 127 II=1,NROC                                                ROD 1740
          IF(ITEMP(II) .EQ. IC) GOTO 128                                ROD 1750
  127   CONTINUE                                                        ROD 1760
        NERR = 5                                                        ROD 1770
        WRITE (IOUT,1006) IC                                            ROD 1780
  128   CONTINUE                                                        ROD 1790
  129 CONTINUE                                                          ROD 1800
      IS = 0                                                            ROD 1810
      ID = 30                                                           ROD 1820
      IC = NROC + 2                                                     ROD 1830
      WRITE (IO10) ID,IC,NROC,IS,IS                                     ROD 1840
      WRITE (IO10) (ICRP(I),I=1,24)                                     ROD 1850
      WRITE (IO10) (ICRPS(I),I=1,24)                                    ROD 1860
      DO 130 II=1,NROC                                                  ROD 1870
        READ (IO3) ID,IT,IS,(NXTR1(I),I=1,IT)                           ROD 1880
        WRITE (IO10) ID,IT,IS,(NXTR1(I),I=1,IT)                         ROD 1890
  130 CONTINUE                                                          ROD 1900
      END FILE IO10                                                     ROD 1910
      REWIND IO10                                                       ROD 1920
      REWIND IO3                                                        ROD 1930
      IF(NERR .GT. 0) NER(53) = 53                                      ROD 1940
      RETURN                                                            ROD 1950
C                                                                       ROD 1960
 1000 FORMAT (24I3)                                                     ROD 1970
 1001 FORMAT (1H0,24I3)                                                 ROD 1980
 1002 FORMAT (1H0/1H0,'CONTROL ROD POSITION - INPUT SECTION 030')       ROD 1990
 1003 FORMAT (1H ,24I3)                                                 ROD 2000
 1004 FORMAT (1H0,'ALL POINTS IN SPEC',I5,' ARE NOT IN THE SAME REGION')ROD 2010
 1005 FORMAT (1H0,'A REGION ID IS MISSING FOR',I5)                      ROD 2020
 1006 FORMAT (1H0,'THERE IS NO DATA FOR PROGRAM SPEC',I5)               ROD 2030
 1007 FORMAT (1H0,'ERROR IN NUCLIDE NUMBERS')                           ROD 2040
 1008 FORMAT (1H0,'DEPLETION TIME STEPS PER CYCLE MAY NOT EXCEED 24 FOR ROD 2050
     1THIS OPTION')                                                     ROD 2060
 1009 FORMAT (1H ,'***WARNING*** NUCLIDE NUMBER',I5,' NOT IN ZONE',I5)  ROD 2070
 1010 FORMAT (1H0,'ZONE',I5,' IS GREATER THAN',I5)                      ROD 2080
      END                                                               ROD 2090
      SUBROUTINE RSTR(NTITE,A,MEMORY)                                   RST   10
C                                                                       RST   20
CRSTR --059-***CITATION***-READS-RESTART-TAPE/CF-IPTM                   RST   30
C                                                                       RST   40
C     NTITE SET TO 0 IF RESTART STATICS ONLY.                           RST   50
C                  1 IF RESTART ATEND OF SOME CYCLE                     RST   60
C                    OR CONTINUE A CYCLE .GT. 1.                        RST   70
C                 -1 IF RESTART DEPLETION ONLY PROBLEM                  RST   80
C                    OR CONTINUE FIRST CYCLE.                           RST   90
C                                                                       RST  100
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,RST  110
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   RST  120
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), RST  130
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    RST  140
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    RST  150
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   RST  160
     6 IXPUT(9999),XPUT(9999)                                           RST  170
C                                                                       RST  180
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  RST  190
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), RST  200
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)RST  210
     3 ,PDK(211)                                                        RST  220
C                                                                       RST  230
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   RST  240
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKRST  250
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    RST  260
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  RST  270
     4 ITMAX,ITIME,BET(211),DEL(211)                                    RST  280
C                                                                       RST  290
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           RST  300
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    RST  310
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),RST  320
     3 NCLASS(9999),NDP(9999)                                           RST  330
C                                                                       RST  340
      COMMON /AVDLM/ IVDLM(1),IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1, RST  350
     1 KBVXP1,NSETVX,NVO,IVO,IVZ,KVZ,NCRP,NSPA,N3DDIM,NBLOCK,NUO,JIVX,  RST  360
     2 JIP1VX,JP1IXZ,IOVX,IOVZ                                          RST  370
C                                                                       RST  380
      COMMON /AKADD/ KAY(1),K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13, RST  390
     1 K131,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,    RST  400
     2 K28,K29,K30,K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,K41,K42,K43, RST  410
     3 K44,K45,K46,K47,K48,K49,K50,K51,K52,K53,K54,K55,K56,K57,K58,     RST  420
     4 K59,K60,K61,K62,K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,     RST  430
     5 K74,K75,K76,K77,K78,K79,K80,K81,K82,K83,K84,K85,K86,K87,K88,     RST  440
     6 K89,K90,K91,K92,K93,K94,K95,K96,K97,K98,K99,K100,NDATA,KNRGN,    RST  450
     7 KNCOMP,KPVOL,KRVOL,MEMVRY,MEMX                                   RST  460
C                                                                       RST  470
      COMMON /ASRCH/ BSRCH(30),XK1,XK2,XK3,XN1,XN2,XN3,DELK1,DELK2,DELK3RST  480
     1 ,BATTY,DRV,TBF,GWC,EK2,RCCM,DNDK(5),NSC(5),NSCN,NXZ,NXN,NXM,NXS, RST  490
     2 INIL,INIU,INID                                                   RST  500
C                                                                       RST  510
      COMMON /MU/ MU4                                                   RST  520
C                                                                       RST  530
      COMMON /IFLUX/ IFLX,YFLX(211)                                     RST  540
C                                                                       RST  550
      DIMENSION A(MEMORY)                                               RST  560
C                                                                       RST  570
C                                                                       RST  580
      XMI3 = XMIS(3)                                                    RST  590
      XMI4 = XMIS(4)                                                    RST  600
      SPA100 = SPARE(100)                                               RST  610
      REWIND IO1                                                        RST  620
      WRITE (IO1)(TITL1(I),I=1,18),(TITL2(I),I=1,18),(NGC(I),I=1,24),   RST  630
     1 (IEDG(I),I=1,24),(ITMX(I),I=1,24),(GLIM(I),I=1,6),IX(5),SPARE(50)RST  640
      END FILE IO1                                                      RST  650
      REWIND IO1                                                        RST  660
      REWIND IO2                                                        RST  670
      IO10 = IX(77)                                                     RST  680
      IO12 = IX(79)                                                     RST  690
      REWIND IO12                                                       RST  700
      IO13 = IX(80)                                                     RST  710
      IOR = IO13                                                        RST  720
      REWIND IOR                                                        RST  730
      NGC2 = NGC(2)                                                     RST  740
      MEMTRY = 0                                                        RST  750
      READ (IOR) II                                                     RST  760
      IF(II .EQ. 999) GOTO 102                                          RST  770
  101 WRITE (IOUT,1000) II,NGC2,MEMTRY,MEMORY                           RST  780
C                                                                       RST  790
      CALL EXIT                                                         RST  800
C                                                                       RST  810
  102 CONTINUE                                                          RST  820
      READ (IOR) N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N13,N14,MEMTRY          RST  830
      READ (IOR) (TITL1(I),I=1,18),(TITL2(I),I=1,18)                    RST  840
      IF(MU4 .EQ. 1) WRITE (IOUT,1001) (TITL1(I),I=1,18)                RST  850
      IF(MU4 .EQ. 1) WRITE (IOUT,1002) (TITL2(I),I=1,18)                RST  860
      IF(MEMTRY .NE. MEMORY) GOTO 101                                   RST  870
      READ (IOR) BLSUB,TITL1,TITL2,IMAX,JMAX,KBMAX,KMAX,LMAX,MMAX,NMAX, RST  880
     1 IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,IOUT,IOSIG,IOFLX, RST  890
     2 IO1,IO2,IO3,IO4,IO7,NER,IX,INNO,NGC,IEDG,ITMX,TIMX,GLIM,NDPL,    RST  900
     3 IEDP1,IEDP2,IEDP3,DPLH,NUAC,EPI,XMIS,NSRH,XSRH1,XTR1,XTR2,NXTR1, RST  910
     4 NXTR2,SPARE,IXPUT,XPUT                                           RST  920
      XMIS(3) = XMI3                                                    RST  930
      XMIS(4) = XMI4                                                    RST  940
      READ (IOR) BMESH,NREGI,NREGJ,NREGKB,XSHI,XSHJ,XSHKB,MSHI,MSHJ,    RST  950
     1 MSHKB,Y,YY,X,XX,Z,ZZ,ZONVOL,AVZPD,PDI,PDJ,PDK                    RST  960
      READ (IOR) BFLUX,KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,ISTART,IEP,RST  970
     1 VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEK,RMX,RMN,  RST  980
     2 XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,VRGABS,LO3,  RST  990
     3 LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,ITMAX,ITIME, RST 1000
     4 BET,DEL                                                          RST 1010
      READ (IOR) BBURN,NSIG1,NSIG2,NSIG3,N1N2R,NSIG4,NSIG5,NSIG6,NJM,   RST 1020
     1 NJMM,NJNQ,NCH,NZON,NXSET,NXODR,IDXSET,NCLASS,NDP                 RST 1030
      READ (IOR) IVDLM,IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1,KBVXP1, RST 1040
     1 NSETVX,NVO,IVO,IVZ,KVZ,NCRP,NSPA,N3DDIM,NBLOCK,NUO,JIVX,JIP1VX,  RST 1050
     2 JP1IXZ,IOVX,IOVZ                                                 RST 1060
      READ (IOR) KAY,K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13,K131,K14RST 1070
     1 ,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,K28,K29,K30,RST 1080
     2 K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,K41,K42,K43,K44,K45,K46, RST 1090
     3 K47,K48,K49,K50,K51,K52,K53,K54,K55,K56,K57,K58,K59,K60,K61,K62, RST 1100
     4 K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,K74,K75,K76,K77,K78, RST 1110
     5 K79,K80,K81,K82,K83,K84,K85,K86,K87,K88,K89,K90,K91,K92,K93,K94, RST 1120
     6 K95,K96,K97,K98,K99,K100,NDATA,KNRGN,KNCOMP,KPVOL,KRVOL,MEMVRY,  RST 1130
     7 MEMX                                                             RST 1140
      READ (IOR) BSRCH,XK1,XK2,XK3,XN1,XN2,XN3,DELK1,DELK2,DELK3,BATTY, RST 1150
     1 DRV,TBF,GWC,EK2,RCCM,DNDK,NSC,NSCN,NXZ,NXN,NXM,NXS,INIL,INIU,INIDRST 1160
      READ (IOR) (A(I1),I1=K8,N8)                                       RST 1170
      READ (IOR) (A(I1),I1=K50,N9)                                      RST 1180
      READ (IOR) (A(I1),I1=KNRGN,N10)                                   RST 1190
      READ (IOR) (A(I1),I1=KNCOMP,MEMORY)                               RST 1200
      READ (IOR) (A(I1),I1=K45,N14)                                     RST 1210
      REWIND IO10                                                       RST 1220
      IREAD = IOR                                                       RST 1230
      IRITE = IO10                                                      RST 1240
C                                                                       RST 1250
      CALL TRAN(A(K17),MVX,NVX,IREAD,IRITE,A(K30),A(K31),A(K32),A(K33), RST 1260
     1 NSETVX)                                                          RST 1270
C                                                                       RST 1280
      END FILE IO10                                                     RST 1290
      REWIND IO10                                                       RST 1300
      IO19 = IX(86)                                                     RST 1310
      REWIND IO19                                                       RST 1320
      NL = K42 - 1                                                      RST 1330
      WRITE (IO19) (A(I),I=K30,NL)                                      RST 1340
      END FILE IO19                                                     RST 1350
      REWIND IO19                                                       RST 1360
      IO26 = IX(93)                                                     RST 1370
      REWIND IO26                                                       RST 1380
      READ (IOR) II                                                     RST 1390
      IF(II .NE. 0) GOTO 101                                            RST 1400
      READ (IOR) N12,NKM,(IX(I),I=1,200),(SPARE(I),I=1,200),(A(I),I=K14,RST 1410
     1 N12)                                                             RST 1420
      SPARE(100) = SPA100                                               RST 1430
      WRITE (IO2) (A(I),I=K17,N12)                                      RST 1440
      READ (IOR) N11,N22, BFLUX,KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,  RST 1450
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKRST 1460
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    RST 1470
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  RST 1480
     4 ITMAX,ITIME,BET,DEL,SPARE(39),(A(I),I=K24,N11)                   RST 1490
      WRITE (IO26) N11,(A(I),I=K24,N11)                                 RST 1500
      END FILE IO2                                                      RST 1510
      REWIND IO2                                                        RST 1520
      END FILE IO26                                                     RST 1530
      REWIND IO26                                                       RST 1540
      IF(NGC(1) .GT. 0) GOTO 103                                        RST 1550
      REWIND IOR                                                        RST 1560
      NTITE = 0                                                         RST 1570
      IX19 = 1                                                          RST 1580
      GOTO 104                                                          RST 1590
  103 CONTINUE                                                          RST 1600
      IX19 = NGC2                                                       RST 1610
      NTITE = 1                                                         RST 1620
      READ (IOR) II                                                     RST 1630
      BACKSPACE IOR                                                     RST 1640
      IF(NGC2 .GT. 0 .OR. II .EQ. -11111) IX(2) = 0                     RST 1650
      IF(NGC2 .GT. 0 .OR. II .GT. 0 .OR. II .EQ. -11111) GOTO 106       RST 1660
      IF(NGC2 .GT. 0 .AND. II .LT. 0) GOTO 101                          RST 1670
      NTITE = -1                                                        RST 1680
      IX19 = II                                                         RST 1690
C                                                                       RST 1700
      IF(NDPL(11) .GT. 0) CALL RODO(A(KNCOMP),LVX)                      RST 1710
C                                                                       RST 1720
      READ (IOR) II                                                     RST 1730
      READ (IOR) N12,NKM,(IX(I),I=1,200),(SPARE(I),I=1,200),(A(I),I=K14,RST 1740
     1 N12),(A(I),I=KNCOMP,NKM)                                         RST 1750
      SPARE(100) = SPA100                                               RST 1760
      BACKSPACE IOR                                                     RST 1770
      BACKSPACE IOR                                                     RST 1780
  104 CONTINUE                                                          RST 1790
      IF(NGC(19) .GT. 0) GOTO 106                                       RST 1800
      IF(NGC(1) .EQ. 0) READ (IO2) (A(I),I=K17,N12)                     RST 1810
      NZN2 = 0                                                          RST 1820
      WRITE (IO12) MMAX,MMAX,MMAX,MMAX,MMAX                             RST 1830
      DO 105 M = 1,MMAX                                                 RST 1840
        NACT = NXSET(M)                                                 RST 1850
        MS1 = NXODR(NACT)                                               RST 1860
        NZN1 = NZN2 + 1                                                 RST 1870
        NZN2 = NZN2 + NJM(MS1) * NZON(M)                                RST 1880
        J1 = NZN1 + K18 - 1                                             RST 1890
        J2 = NZN2 + K18 - 1                                             RST 1900
        WRITE (IO12) NZN1,NZN2                                          RST 1910
        WRITE (IO12) (A(I),I=J1,J2)                                     RST 1920
  105 CONTINUE                                                          RST 1930
      END FILE IO12                                                     RST 1940
      REWIND IO12                                                       RST 1950
      REWIND IO2                                                        RST 1960
  106 CONTINUE                                                          RST 1970
      READ (IO19) (A(I),I=K30,NL)                                       RST 1980
      REWIND IO19                                                       RST 1990
      READ (IO1) (TITL1(I),I=1,18),(TITL2(I),I=1,18),(NGC(I),I=1,24),   RST 2000
     1 (IEDG(I),I=1,24),(ITMX(I),I=1,24),(GLIM(I),I=1,6),IX(5),SPARE(50)RST 2010
      REWIND IO1                                                        RST 2020
      IFLX = 0                                                          RST 2030
      IF(IEDG(10) .EQ. 1) IFLX = 1                                      RST 2040
      IF(IEDG(10) .EQ. 2) IFLX = 2                                      RST 2050
      IX(19) = IX19                                                     RST 2060
      IX(22) = 0                                                        RST 2070
      IX(39) = 0                                                        RST 2080
      IX(198) = NTITE                                                   RST 2090
      RETURN                                                            RST 2100
C                                                                       RST 2110
 1000 FORMAT (1H0,'ERROR STOP 27 IN SUB. RSTR',10X,10I8)                RST 2120
 1001 FORMAT (1H0,'THE CASE TITLE ON THE RESTART TAPE IS'/1H0,18A4)     RST 2130
 1002 FORMAT (1H ,18A4)                                                 RST 2140
      END                                                               RST 2150
      SUBROUTINE MYSH(JL,JR,IT,IB,KBF,KBB,JMAX,IMAX,KBMAX,N5,IND,IOUT)  MYS   10
C                                                                       MYS   20
CMYSH --60.4 ***CITATION*** CHECKS MESH POINT SPECIFICATIONS /CF-RODI   MYS   30
C                                                                       MYS   40
C     CHECK MESH SPECIFICATIONS                                         MYS   50
C                                                                       MYS   60
C                                                                       MYS   70
      IND = 0                                                           MYS   80
      IF(JL .GE. 1 .AND. JL .LE. JMAX) GOTO 100                         MYS   90
      GOTO 111                                                          MYS  100
  100 IF(JR .GE. 1 .AND. JR .LE. JMAX) GOTO 101                         MYS  110
      GOTO 111                                                          MYS  120
  101 IF(JL .LE. JR) GOTO 102                                           MYS  130
      GOTO 111                                                          MYS  140
  102 IF(N5 .LE. 5) GOTO 108                                            MYS  150
      IF(IT .GE. 1 .AND. IT .LE. IMAX) GOTO 103                         MYS  160
      GOTO 111                                                          MYS  170
  103 IF(IB .GE. 1 .AND. IB .LE. IMAX) GOTO 104                         MYS  180
      GOTO 111                                                          MYS  190
  104 IF(IT .LE. IB) GOTO 105                                           MYS  200
      GOTO 111                                                          MYS  210
  105 IF(N5 .LE. 10) GOTO 109                                           MYS  220
      IF(KBF .GE. 1 .AND. KBF .LE. KBMAX) GOTO 106                      MYS  230
      GOTO 111                                                          MYS  240
  106 IF(KBB .GE. 1 .AND. KBB .LE. KBMAX) GOTO 107                      MYS  250
      GOTO 111                                                          MYS  260
  107 IF(KBF .LE. KBB) GOTO 110                                         MYS  270
      GOTO 111                                                          MYS  280
  108 IT = 1                                                            MYS  290
      IB = 1                                                            MYS  300
  109 KBF = 1                                                           MYS  310
      KBB = 1                                                           MYS  320
  110 CONTINUE                                                          MYS  330
      RETURN                                                            MYS  340
  111 CONTINUE                                                          MYS  350
      IND = 1                                                           MYS  360
      WRITE (IOUT,1000) JL,JR,IT,IB,KBF,KBB                             MYS  370
      GO TO 110                                                         MYS  380
C                                                                       MYS  390
 1000 FORMAT (1H0,'SOMETHING IS WRONG WITH MESH SPECS',6I5)             MYS  400
      END                                                               MYS  410
      SUBROUTINE RODO(NCOMP,LVX)                                        ODO   10
C                                                                       ODO   20
CRODO --60.5 ***CITATION*** TRANSFERS SECTION 030 DATA   /CF-IPTM,RSTR  ODO   30
C                           FROM IO10 TO IO35 AND ADDS NCOMP            ODO   40
C                                                                       ODO   50
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,ODO   60
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   ODO   70
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), ODO   80
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    ODO   90
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    ODO  100
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   ODO  110
     6 IXPUT(9999),XPUT(9999)                                           ODO  120
C                                                                       ODO  130
CFZJ055                                                       25.09.07  ODO  140
C                                                                       ODO  150
      DIMENSION NCOMP(LVX)                                              ODO  160
C                                                                       ODO  170
C                                                                       ODO  180
      IO10 = IX(77)                                                     ODO  190
      IO35 = IX(141)                                                    ODO  200
      REWIND IO10                                                       ODO  210
      REWIND IO35                                                       ODO  220
  100 READ (IO10,END=102) I1,I2,I3                                      ODO  230
      IF(I1 .EQ. 30) GOTO 103                                           ODO  240
      IF(I2 .EQ. 0) GOTO 100                                            ODO  250
      DO 101 I=1,I2                                                     ODO  260
        READ (IO10)                                                     ODO  270
  101 CONTINUE                                                          ODO  280
      GOTO 100                                                          ODO  290
  102 WRITE (IOUT,1000)                                                 ODO  300
C                                                                       ODO  310
      CALL EXIT                                                         ODO  320
C                                                                       ODO  330
  103 CONTINUE                                                          ODO  340
      NROC = I3                                                         ODO  350
      WRITE (IO35) (NCOMP(L),L=1,LMAX)                                  ODO  360
      WRITE (IO35) NROC,NROC,NROC,NROC,NROC                             ODO  370
      READ (IO10) (NXTR1(I),I=1,24)                                     ODO  380
      WRITE (IO35) (NXTR1(I),I=1,24)                                    ODO  390
      READ (IO10) (NXTR1(I),I=1,24)                                     ODO  400
      WRITE (IO35) (NXTR1(I),I=1,24)                                    ODO  410
      DO 104 II=1,NROC                                                  ODO  420
        READ (IO10) I1,I2,I3,(NXTR1(I),I=1,I2)                          ODO  430
        WRITE (IO35) I1,I2,I3,(NXTR1(I),I=1,I2)                         ODO  440
  104 CONTINUE                                                          ODO  450
      END FILE IO35                                                     ODO  460
      REWIND IO35                                                       ODO  470
      REWIND IO10                                                       ODO  480
      RETURN                                                            ODO  490
C                                                                       ODO  500
 1000 FORMAT (1H0,'NO SECTION 30 DATA ON LOG 10 (RODO)')                ODO  510
      END                                                               ODO  520
      SUBROUTINE RODX(CONC,MJJR,NCOMP,P2,P2E,NRGNE,BBND,NVX,MVX,NSETVX, ODX   10
     1 LVX,JVX,IVX,KBVX,JIVX,KVX)                                       ODX   20
C                                                                       ODX   30
CRODX --94.1 ***CITATION*** CONTROLS ROD MOVEMENT         /CF-CALR      ODX   40
C                                                                       ODX   50
      REAL*8 P2                                                         ODX   60
C                                                                       ODX   70
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,ODX   80
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   ODX   90
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), ODX  100
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    ODX  110
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    ODX  120
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   ODX  130
     6 IXPUT(9999),XPUT(9999)                                           ODX  140
C                                                                       ODX  150
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           ODX  160
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    ODX  170
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),ODX  180
     3 NCLASS(9999),NDP(9999)                                           ODX  190
C                                                                       ODX  200
CFZJ055                                                       25.09.07  ODX  210
C                                                                       ODX  220
      DIMENSION CONC(NVX,MVX),MJJR(9999,NSETVX),NCOMP(LVX),             ODX  230
     1 P2(JVX,IVX,KVX),P2E(JIVX,KBVX,KVX),NRGNE(JVX,IVX,KBVX),BBND(KVX),ODX  240
     2 ICRP(24),ICRPS(24)                                               ODX  250
C                                                                       ODX  260
C                                                                       ODX  270
      IPSN = IX(2)                                                      ODX  280
      IO35 = IX(141)                                                    ODX  290
      REWIND IO35                                                       ODX  300
      IF(IPSN .GT. 1) READ (IO35)                                       ODX  310
      IF(IPSN .LE. 1) READ (IO35) (NCOMP(L),L=1,LMAX)                   ODX  320
      READ (IO35) NROC                                                  ODX  330
      READ (IO35) (ICRP(I),I=1,24)                                      ODX  340
      READ (IO35) (ICRPS(I),I=1,24)                                     ODX  350
      IPS = ICRPS(IPSN)                                                 ODX  360
      IF(IPS .EQ. 0) GOTO 133                                           ODX  370
      DO 100 N=1,NROC                                                   ODX  380
        READ (IO35) IPST,IT,NSPEC,(NXTR1(I),I=1,IT)                     ODX  390
        IF(IPST .EQ. IPS) GOTO 101                                      ODX  400
  100 CONTINUE                                                          ODX  410
      WRITE (IOUT,1000) IPS                                             ODX  420
      STOP                                                              ODX  430
  101 CONTINUE                                                          ODX  440
      IF(ICRP(1) .NE. 0) GOTO 106                                       ODX  450
C                                                                       ODX  460
C     REGION BASIS                                                      ODX  470
C                                                                       ODX  480
      WRITE (IOUT,1001)                                                 ODX  490
      ITT = IT + 1                                                      ODX  500
      IT2 = IT - 2                                                      ODX  510
      DO 105 II=1,IT2                                                   ODX  520
        I = ITT - II                                                    ODX  530
        LO = NXTR1(I)                                                   ODX  540
        IF(NXTR1(I-1) .EQ. 0 .OR. I-1 .EQ. 2) GOTO 102                  ODX  550
        LN = NXTR1(I-1)                                                 ODX  560
        MN = NCOMP(LN)                                                  ODX  570
        GOTO 103                                                        ODX  580
  102   CONTINUE                                                        ODX  590
        IF(NXTR1(2) .EQ. 0) GOTO 104                                    ODX  600
        MN = NXTR1(2)                                                   ODX  610
  103   CONTINUE                                                        ODX  620
        MT = NCOMP(LO)                                                  ODX  630
        NCOMP(LO) = MN                                                  ODX  640
        IF(ICRP(24) .GT. 0) WRITE (IOUT,1002) LO,NCOMP(LO),MN           ODX  650
  104   CONTINUE                                                        ODX  660
  105 CONTINUE                                                          ODX  670
      GOTO 119                                                          ODX  680
  106 CONTINUE                                                          ODX  690
      IF(ICRP(2) .GT. 0) GOTO 113                                       ODX  700
C                                                                       ODX  710
C     ZONE BASIS                                                        ODX  720
C                                                                       ODX  730
      WRITE (IOUT,1003)                                                 ODX  740
      ITT = IT + 1                                                      ODX  750
      IT2 = IT - 2                                                      ODX  760
      DO 111 II=1,IT2                                                   ODX  770
        I = ITT - II                                                    ODX  780
        MO = NXTR1(I)                                                   ODX  790
        IF(NXTR1(I-1) .EQ. 0 .OR. I-1 .EQ. 2) GOTO 107                  ODX  800
        MN = NXTR1(I-1)                                                 ODX  810
        GOTO 108                                                        ODX  820
  107   CONTINUE                                                        ODX  830
        IF(NXTR1(2) .EQ. 0) GOTO 110                                    ODX  840
        MN = NXTR1(2)                                                   ODX  850
  108   CONTINUE                                                        ODX  860
        DO 109 L=1,LMAX                                                 ODX  870
          IF(NCOMP(L) .NE. MO) GOTO 109                                 ODX  880
          MT = NCOMP(L)                                                 ODX  890
          NCOMP(L) = -MN                                                ODX  900
          IF(ICRP(24) .LE. 0) GOTO 109                                  ODX  910
          WRITE (IOUT,1002) L,NCOMP(L),MT                               ODX  920
  109   CONTINUE                                                        ODX  930
  110   CONTINUE                                                        ODX  940
  111 CONTINUE                                                          ODX  950
      DO 112 L=1,LMAX                                                   ODX  960
        NCOMP(L) = IABS(NCOMP(L))                                       ODX  970
  112 CONTINUE                                                          ODX  980
      GOTO 119                                                          ODX  990
  113 CONTINUE                                                          ODX 1000
C                                                                       ODX 1010
C     ZONE CONCENTRATION BASIS                                          ODX 1020
C                                                                       ODX 1030
      WRITE (IOUT,1004)                                                 ODX 1040
      ITT = IT + 1                                                      ODX 1050
      IT2 = IT - 2                                                      ODX 1060
      DO 118 II=1,IT2                                                   ODX 1070
        I = ITT - II                                                    ODX 1080
        MO = NXTR1(I)                                                   ODX 1090
        IF(NXTR1(I-1) .EQ. 0 .OR. I-1 .EQ. 2) GOTO 114                  ODX 1100
        MN = NXTR1(I-1)                                                 ODX 1110
        GOTO 115                                                        ODX 1120
  114   CONTINUE                                                        ODX 1130
        IF(NXTR1(2) .EQ. 0) GOTO 117                                    ODX 1140
        MN = NXTR1(2)                                                   ODX 1150
  115   CONTINUE                                                        ODX 1160
        DO 116 N=3,12                                                   ODX 1170
          IC = ICRP(N)                                                  ODX 1180
          IF(IC .EQ. 0) GOTO 116                                        ODX 1190
          NSN = NXSET(MN)                                               ODX 1200
          NRN = NXODR(NSN)                                              ODX 1210
          NN = MJJR(IC,NRN)                                             ODX 1220
          NSO = NXSET(MO)                                               ODX 1230
          NRO = NXODR(NSO)                                              ODX 1240
          NO = MJJR(IC,NRO)                                             ODX 1250
          IF(NN .EQ. 0 .OR. NO .EQ. 0) GOTO 116                         ODX 1260
          CT = CONC(NO,MO)                                              ODX 1270
          CONC(NO,MO) = CONC(NN,MN)                                     ODX 1280
          IF(ICRP(24) .GT. 0) WRITE (IOUT,1005) IC,NSN,NRN,NN,NSO,NRO,NOODX 1290
     1     ,MO,MN,CONC(NO,MO),CT                                        ODX 1300
  116   CONTINUE                                                        ODX 1310
  117   CONTINUE                                                        ODX 1320
  118 CONTINUE                                                          ODX 1330
      GOTO 133                                                          ODX 1340
  119 CONTINUE                                                          ODX 1350
      N17 = NUAC(17)                                                    ODX 1360
      IF(N17 .EQ. 0) GOTO 132                                           ODX 1370
C                                                                       ODX 1380
C     MODIFY STARTING FLUX IF INTERNAL BLACK ABSORBER PRESENT           ODX 1390
C                                                                       ODX 1400
      N5 = NUAC(5)                                                      ODX 1410
      DO 123 K=1,KMAX                                                   ODX 1420
        DO 122 KB=1,KBMAX                                               ODX 1430
          N1 = 0                                                        ODX 1440
          DO 121 I=1,IMAX                                               ODX 1450
            DO 120 J=1,JMAX                                             ODX 1460
              N1 = N1 + 1                                               ODX 1470
              IF(N5 .LE. 0) P = P2(J,I,K)                               ODX 1480
              IF(N5 .GT. 0) P = P2E(N1,KB,K)                            ODX 1490
              IF(P .EQ. 0.0) GOTO 120                                   ODX 1500
              PNO = P                                                   ODX 1510
              GOTO 124                                                  ODX 1520
  120       CONTINUE                                                    ODX 1530
  121     CONTINUE                                                      ODX 1540
  122   CONTINUE                                                        ODX 1550
  123 CONTINUE                                                          ODX 1560
      GOTO 132                                                          ODX 1570
  124 CONTINUE                                                          ODX 1580
      DO 131 KB=1,KBMAX                                                 ODX 1590
        N1 = 0                                                          ODX 1600
        DO 130 I=1,IMAX                                                 ODX 1610
          DO 129 J=1,JMAX                                               ODX 1620
            N1 = N1 + 1                                                 ODX 1630
            L = NRGNE(J,I,KB)                                           ODX 1640
            M = NCOMP(L)                                                ODX 1650
            DO 128 K=1,KMAX                                             ODX 1660
              IF(N5 .LE. 10) P = P2(J,I,K)                              ODX 1670
              IF(N5 .GT. 10) P = P2E(N1,KB,K)                           ODX 1680
              IF(M .NE. N17) GOTO 126                                   ODX 1690
              IF(XMIS(2) .GE. 0.0) GOTO 125                             ODX 1700
              IF(BBND(K) .EQ. 0.0) GOTO 126                             ODX 1710
  125         P = 0.0                                                   ODX 1720
              GOTO 127                                                  ODX 1730
  126         IF(P .EQ. 0.0) P = PNO                                    ODX 1740
  127         IF(N5 .LE. 10) P2(J,I,K) = P                              ODX 1750
              IF(N5 .GT. 10) P2E(N1,KB,K) = P                           ODX 1760
  128       CONTINUE                                                    ODX 1770
  129     CONTINUE                                                      ODX 1780
  130   CONTINUE                                                        ODX 1790
  131 CONTINUE                                                          ODX 1800
  132 CONTINUE                                                          ODX 1810
      IF(ICRP(24) .GT. 0) WRITE (IOUT,1006) (NCOMP(L),L=1,LMAX)         ODX 1820
  133 CONTINUE                                                          ODX 1830
      REWIND IO35                                                       ODX 1840
      RETURN                                                            ODX 1850
C                                                                       ODX 1860
 1000 FORMAT (1H0,'NO DATA FOR CONTROL ROD PROGRAM SPEC',I5)            ODX 1870
 1001 FORMAT (1H0,'CONTROL ROD PROGRAM - REGION BASIS')                 ODX 1880
 1002 FORMAT (1H ,10I5)                                                 ODX 1890
 1003 FORMAT (1H0,'CONTROL ROD PROGRAM - ZONE BASIS')                   ODX 1900
 1004 FORMAT (1H0,'CONTROL ROD PROGRAM - ZONE CONC BASIS')              ODX 1910
 1005 FORMAT (1H ,9I5,2E15.6)                                           ODX 1920
 1006 FORMAT (1H0,24I5)                                                 ODX 1930
      END                                                               ODX 1940
      SUBROUTINE WSTR(NCYLE,A,MEMORY)                                   WST   10
C                                                                       WST   20
CWSTR --130-***CITATION***-WRITES-RESTART-TAPE/CF-CALR                  WST   30
C                                                                       WST   40
C     IF(NCYLE .EQ. 999) WRITE COMMON AND FIXED DATA                    WST   50
C     IF(NCYLE .EQ. 0)   WRITE CONC. AND FLUX                           WST   60
C     IF(NCYLE .LT. 0)   WRITE CONC.                                    WST   70
C                                                                       WST   80
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,WST   90
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   WST  100
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), WST  110
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    WST  120
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    WST  130
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   WST  140
     6 IXPUT(9999),XPUT(9999)                                           WST  150
C                                                                       WST  160
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  WST  170
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), WST  180
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)WST  190
     3 ,PDK(211)                                                        WST  200
C                                                                       WST  210
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   WST  220
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKWST  230
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    WST  240
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  WST  250
     4 ITMAX,ITIME,BET(211),DEL(211)                                    WST  260
C                                                                       WST  270
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           WST  280
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    WST  290
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),WST  300
     4 NCLASS(9999),NDP(9999)                                           WST  310
C                                                                       WST  320
      COMMON /AVDLM/ IVDLM(1),IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1, WST  330
     1 KBVXP1,NSETVX,NVO,IVO,IVZ,KVZ,NCRP,NSPA,N3DDIM,NBLOCK,NUO,JIVX,  WST  340
     2 JIP1VX,JP1IXZ,IOVX,IOVZ                                          WST  350
C                                                                       WST  360
      COMMON /AKADD/ KAY(1),K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13, WST  370
     1 K131,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,    WST  380
     2 K28,K29,K30,K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,K41,K42,K43, WST  390
     3 K44,K45,K46,K47,K48,K49,K50,K51,K52,K53,K54,K55,K56,K57,K58,     WST  400
     4 K59,K60,K61,K62,K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,     WST  410
     5 K74,K75,K76,K77,K78,K79,K80,K81,K82,K83,K84,K85,K86,K87,K88,     WST  420
     6 K89,K90,K91,K92,K93,K94,K95,K96,K97,K98,K99,K100,NDATA,KNRGN,    WST  430
     7 KNCOMP,KPVOL,KRVOL,MEMVRY,MEMX                                   WST  440
C                                                                       WST  450
      COMMON /ASRCH/ BSRCH(30),XK1,XK2,XK3,XN1,XN2,XN3,DELK1,DELK2,DELK3WST  460
     1 ,BATTY,DRV,TBF,GWC,EK2,RCCM,DNDK(5),NSC(5),NSCN,NXZ,NXN,NXM,NXS, WST  470
     2 INIL,INIU,INID                                                   WST  480
C                                                                       WST  490
CFZJ055                                                       25.09.07  WST  500
C                                                                       WST  510
      DIMENSION A(MEMORY)                                               WST  520
C                                                                       WST  530
C                                                                       WST  540
      IO10 = IX(77)                                                     WST  550
      IO12 = IX(79)                                                     WST  560
      REWIND IO12                                                       WST  570
      IO13 = IX(80)                                                     WST  580
      IOR = IO13                                                        WST  590
      WRITE (IOR) NCYLE,NCYLE,NCYLE,NCYLE,NCYLE                         WST  600
      IF(NCYLE .EQ. 999) GOTO 106                                       WST  610
      J2 = K19 - 1                                                      WST  620
      IF(NGC(19) .GT. 0 .OR. IX(199) .EQ. 0) GOTO 102                   WST  630
      IO11 = IX(78)                                                     WST  640
      REWIND IO11                                                       WST  650
      N1 = K41 - 1                                                      WST  660
      WRITE (IO11) (A(I),I=K24,N1)                                      WST  670
      END FILE IO11                                                     WST  680
      REWIND IO11                                                       WST  690
      INCR = K18 - 1                                                    WST  700
      READ (IO12) MVVX                                                  WST  710
      DO 101 M=1,MVVX                                                   WST  720
        READ (IO12) NZN1,NZN2                                           WST  730
        J1 = NZN1 + INCR                                                WST  740
        J2 = NZN2 + INCR                                                WST  750
        READ (IO12) (A(I),I=J1,J2)                                      WST  760
  101 CONTINUE                                                          WST  770
      REWIND IO12                                                       WST  780
  102 CONTINUE                                                          WST  790
      NKM = KPVOL - 1                                                   WST  800
      WRITE (IOR) J2,NKM,(IX(I),I=1,200),(SPARE(I),I=1,200),(A(I),I=K14,WST  810
     1 J2),(A(I),I=KNCOMP,NKM)                                          WST  820
      N1 = K41 - 1                                                      WST  830
      N2 = 498                                                          WST  840
      IF(NGC(19) .GT. 0 .OR. IX(199) .EQ. 0) GOTO 104                   WST  850
      READ (IO11) (A(I),I=K24,N1)                                       WST  860
      REWIND IO11                                                       WST  870
  104 CONTINUE                                                          WST  880
      IF(NCYLE .NE. 0) GOTO 105                                         WST  890
      WRITE (IOR) N1,N2, BFLUX,KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   WST  900
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKWST  910
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    WST  920
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  WST  930
     4 ITMAX,ITIME,BET,DEL,SPARE(39),(A(I),I=K24,N1)                    WST  940
  105 CONTINUE                                                          WST  950
      END FILE IOR                                                      WST  960
      BACKSPACE IOR                                                     WST  970
      GOTO 107                                                          WST  980
  106 N1 = 21939                                                        WST  990
      N2 = 23130                                                        WST 1000
      N3 = 498                                                          WST 1010
      N4 = 80522                                                        WST 1020
      N5 = 26                                                           WST 1030
      N6 = 0                                                            WST 1040
      N7 = 109                                                          WST 1050
      N13 = 63                                                          WST 1060
      N8 = K34 - 1                                                      WST 1070
      N9 = K52 - 1                                                      WST 1080
      N14 = K64 - 1                                                     WST 1090
      N10 = KNRGN + JVX * IVX * KBVX - 1                                WST 1100
      WRITE (IOR) N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N13,N14,MEMORY         WST 1110
      WRITE (IOR) (TITL1(I),I=1,18),(TITL2(I),I=1,18)                   WST 1120
      WRITE (IOR) BLSUB,TITL1,TITL2,IMAX,JMAX,KBMAX,KMAX,LMAX,MMAX,NMAX,WST 1130
     1 IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,IOUT,IOSIG,IOFLX, WST 1140
     2 IO1,IO2,IO3,IO4,IO7,NER,IX,INNO,NGC,IEDG,ITMX,TIMX,GLIM,NDPL,    WST 1150
     3 IEDP1,IEDP2,IEDP3,DPLH,NUAC,EPI,XMIS,NSRH,XSRH1,XTR1,XTR2,NXTR1, WST 1160
     4 NXTR2,SPARE,IXPUT,XPUT                                           WST 1170
      WRITE (IOR) BMESH,NREGI,NREGJ,NREGKB,XSHI,XSHJ,XSHKB,MSHI,MSHJ,   WST 1180
     1 MSHKB,Y,YY,X,XX,Z,ZZ,ZONVOL,AVZPD,PDI,PDJ,PDK                    WST 1190
      WRITE (IOR) BFLUX,KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,ISTART,IEPWST 1200
     1 ,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEK,RMX,RMN, WST 1210
     2 XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,VRGABS,LO3,  WST 1220
     3 LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,ITMAX,ITIME, WST 1230
     4 BET,DEL                                                          WST 1240
      WRITE (IOR) BBURN,NSIG1,NSIG2,NSIG3,N1N2R,NSIG4,NSIG5,NSIG6,NJM,  WST 1250
     1 NJMM,NJNQ,NCH,NZON,NXSET,NXODR,IDXSET,NCLASS,NDP                 WST 1260
      WRITE (IOR) IVDLM,IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1,KBVXP1,WST 1270
     1 NSETVX,NVO,IVO,IVZ,KVZ,NCRP,NSPA,N3DDIM,NBLOCK,NUO,JIVX,JIP1VX,  WST 1280
     2 JP1IXZ,IOVX,IOVZ                                                 WST 1290
      WRITE (IOR) KAY,K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13,K131,  WST 1300
     1 K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,K28,K29, WST 1310
     2 K30,K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,K41,K42,K43,K44,K45, WST 1320
     3 K46,K47,K48,K49,K50,K51,K52,K53,K54,K55,K56,K57,K58,K59,K60,K61, WST 1330
     4 K62,K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,K74,K75,K76,K77, WST 1340
     5 K78,K79,K80,K81,K82,K83,K84,K85,K86,K87,K88,K89,K90,K91,K92,K93, WST 1350
     6 K94,K95,K96,K97,K98,K99,K100,NDATA,KNRGN,KNCOMP,KPVOL,KRVOL,     WST 1360
     7 MEMVRY,MEMX                                                      WST 1370
      WRITE (IOR) BSRCH,XK1,XK2,XK3,XN1,XN2,XN3,DELK1,DELK2,DELK3,BATTY,WST 1380
     1 DRV,TBF,GWC,EK2,RCCM,DNDK,NSC,NSCN,NXZ,NXN,NXM,NXS,INIL,INIU,INIDWST 1390
      WRITE (IOR) (A(I1),I1=K8,N8)                                      WST 1400
      WRITE (IOR) (A(I1),I1=K50,N9)                                     WST 1410
      WRITE (IOR) (A(I1),I1=KNRGN,N10)                                  WST 1420
      WRITE (IOR) (A(I1),I1=KNCOMP,MEMORY)                              WST 1430
      WRITE (IOR) (A(I1),I1=K45,N14)                                    WST 1440
      LIM = K18 - 1                                                     WST 1450
      REWIND IO1                                                        WST 1460
      WRITE (IO1) (A(I),I=K17,LIM)                                      WST 1470
      END FILE IO1                                                      WST 1480
      REWIND IO1                                                        WST 1490
      REWIND IO10                                                       WST 1500
      IREAD = IO10                                                      WST 1510
      IRITE = IOR                                                       WST 1520
C                                                                       WST 1530
      CALL KRAN(A(K17),MVX,NVX,IREAD,IRITE,A(K30),A(K31),A(K32),A(K33), WST 1540
     1 NSETVX)                                                          WST 1550
C                                                                       WST 1560
      REWIND IO10                                                       WST 1570
      END FILE IOR                                                      WST 1580
      BACKSPACE IOR                                                     WST 1590
      READ (IO1) (A(I),I=K17,LIM)                                       WST 1600
      REWIND IO1                                                        WST 1610
  107 CONTINUE                                                          WST 1620
      RETURN                                                            WST 1630
      END                                                               WST 1640
      SUBROUTINE MESH(RVOL,PVOL,LVX)                                    MES   10
C                                                                       MES   20
CMESH --029 ***CITATION*** REGION VOLUMES AND MESH SPACING/ CF-IPTM     MES   30
C                                                                       MES   40
      REAL*8 XDEL,ADR,YDELSQ,XNIPTS,XNJPTS,XNKPTS,TEMP,ZXYR,YDIST,XDIST,MES   50
     1 ZDIST,YDEL,ZDEL,AXYR,XDIST1,XDIST2,YDIST1,YDIST2,XNTOPS,U1,U2,H, MES   60
     2 H1,H2,U3                                                         MES   70
C                                                                       MES   80
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,MES   90
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   MES  100
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), MES  110
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    MES  120
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    MES  130
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   MES  140
     6 IXPUT(9999),XPUT(9999)                                           MES  150
C                                                                       MES  160
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  MES  170
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), MES  180
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)MES  190
     3 ,PDK(211)                                                        MES  200
C                                                                       MES  210
      COMMON /IFLUX/ IFLX,YFLX(211),XFLX(211)                           MES  220
C                                                                       MES  230
      DIMENSION RVOL(LVX),PVOL(LVX),NX(6),TX(6)                         MES  240
C                                                                       MES  250
C                                                                       MES  260
C     SET NXY TO SAME NUMBER AS THE DIMENSION OF XX AND YY              MES  270
C                                                                       MES  280
      NXY = 211                                                         MES  290
      IMAX = 1                                                          MES  300
      KBMAX = 1                                                         MES  310
      NDIM = IX(25)                                                     MES  320
      NGEM = IX(26)                                                     MES  330
      MGEM = 0                                                          MES  340
      H1 = 6 * MSHI(1) * MSHJ(1)                                        MES  350
      H2 = XSHI(1) * XSHJ(1)                                            MES  360
      H = DSQRT(H2/H1)                                                  MES  370
      IF(NGEM .EQ. 8 .OR. NGEM .EQ. 12) MGEM = 8                        MES  380
      JMAX = 0                                                          MES  390
      DO 101 J=1,NREGJ                                                  MES  400
        JMAX = JMAX + MSHJ(J)                                           MES  410
  101 CONTINUE                                                          MES  420
      IF(NDIM-2) 107,102,102                                            MES  430
  102 IMAX = 0                                                          MES  440
      DO 104 I=1,NREGI                                                  MES  450
        IMAX = IMAX + MSHI(I)                                           MES  460
  104 CONTINUE                                                          MES  470
      IF(NDIM-2) 107,107,105                                            MES  480
  105 KBMAX = 0                                                         MES  490
      DO 106 KB=1,NREGKB                                                MES  500
        KBMAX = KBMAX + MSHKB(KB)                                       MES  510
  106 CONTINUE                                                          MES  520
  107 NRGNO = 0                                                         MES  530
      MESHGE = 0                                                        MES  540
      DO 125 KBR=1,NREGKB                                               MES  550
        NKBPTS = MSHKB(KBR)                                             MES  560
        U3 = NKBPTS                                                     MES  570
        ZDIST = XSHKB(KBR)                                              MES  580
        YDIST2 = 0.D+0                                                  MES  590
        DO 124 IR=1,NREGI                                               MES  600
          NIPTS = MSHI(IR)                                              MES  610
          U1 = NIPTS                                                    MES  620
          YDIST = XSHI(IR)                                              MES  630
          YDIST1 = YDIST2                                               MES  640
          YDIST2 = YDIST2 + XSHI(IR)                                    MES  650
          XDIST2 = 0.D+0                                                MES  660
          DO 123 JR=1,NREGJ                                             MES  670
            XDIST1 = XDIST2                                             MES  680
            XDIST2 = XDIST2 + XSHJ(JR)                                  MES  690
            NJPTS = MSHJ(JR)                                            MES  700
            U2 = NJPTS                                                  MES  710
            XDIST = XSHJ(JR)                                            MES  720
            NTOPTS = NKBPTS * NIPTS * NJPTS                             MES  730
            XNTOPS = FLOAT(NTOPTS)                                      MES  740
            NRGNO = NRGNO + 1                                           MES  750
            GOTO(108,109,110,111,112,113,114,115,116,117,118,119,120,121MES  760
     1       ),NGEM                                                     MES  770
  108       AXYR = XDIST                                                MES  780
            GOTO 122                                                    MES  790
  109       AXYR = 3.141593 * (XDIST2**2-XDIST1**2)                     MES  800
            GOTO 122                                                    MES  810
  110       AXYR = 4.0 * 3.141593 / 3.0 * (XDIST2**3-XDIST1**3)         MES  820
            GOTO 122                                                    MES  830
  111       AXYR = 1.0                                                  MES  840
            GOTO 122                                                    MES  850
  112       GOTO 122                                                    MES  860
  113       AXYR = YDIST * XDIST                                        MES  870
            GOTO 122                                                    MES  880
  114       AXYR = 3.141593 * (XDIST2**2-XDIST1**2) * YDIST             MES  890
            GOTO 122                                                    MES  900
  115       AXYR = XDIST * (YDIST2**2-YDIST1**2) / 2.0                  MES  910
            GOTO 122                                                    MES  920
  116       AXYR = 0.3608439 * ((XDIST/U2)**2+(YDIST/U1)**2+0.4*XDIST*  MES  930
     1       YDIST/(U2*U1)) * XNTOPS                                    MES  940
            GOTO 122                                                    MES  950
  117       CONTINUE                                                    MES  960
            AXYR = 0.8660254040 * XDIST * YDIST                         MES  970
C           SQRT(3)/2 = 0.8660254040                                    MES  980
            GOTO 122                                                    MES  990
  118       AXYR = ZDIST * YDIST * XDIST                                MES 1000
            GOTO 122                                                    MES 1010
  119       AXYR = 0.5 * ZDIST * XDIST * (YDIST2**2-YDIST1**2)          MES 1020
            GOTO 122                                                    MES 1030
  120       AXYR = 0.3608439 * ZDIST * ((XDIST/U2)**2+(YDIST/U1)**2+0.4*MES 1040
     1       XDIST*YDIST/(U2*U1)) * XNTOPS / U3                         MES 1050
            GOTO 122                                                    MES 1060
  121       CONTINUE                                                    MES 1070
            AXYR = 0.8660254040 * XDIST * YDIST * ZDIST                 MES 1080
  122       RVOL(NRGNO) = AXYR                                          MES 1090
            PVOL(NRGNO) = AXYR / XNTOPS                                 MES 1100
            IF(NGEM .NE. 9 .OR. NGEM .NE. 13) GOTO 123                  MES 1110
            IF(NRGNO .EQ. 1) GOTO 123                                   MES 1120
            IF(PVOL(1) .NE. PVOL(NRGNO)) MESHGE = 1                     MES 1130
  123     CONTINUE                                                      MES 1140
  124   CONTINUE                                                        MES 1150
  125 CONTINUE                                                          MES 1160
      KB = 0                                                            MES 1170
      ZZ(1) = 0.0                                                       MES 1180
      ZXYR = 0.D+0                                                      MES 1190
      ADR = 0.0                                                         MES 1200
      DO 133 KBR=1,NREGKB                                               MES 1210
        NKBPTS = MSHKB(KBR)                                             MES 1220
        XNKPTS = FLOAT(NKBPTS)                                          MES 1230
        ZDEL = XSHKB(KBR) / XNKPTS                                      MES 1240
        DO 132 KNR=1,NKBPTS                                             MES 1250
          KB = KB + 1                                                   MES 1260
          ZXYR = ZXYR + ZDEL                                            MES 1270
          IF(KB-1) 126,126,127                                          MES 1280
  126     AXYR = 0.5D+0 * ZDEL                                          MES 1290
          GOTO 130                                                      MES 1300
  127     IF(KNR-1) 128,128,129                                         MES 1310
  128     AXYR = AXYR + 0.5 * (ZDEL+ADR)                                MES 1320
          GOTO 130                                                      MES 1330
  129     AXYR = AXYR + ZDEL                                            MES 1340
  130     ZZ(KB+1) = ZXYR                                               MES 1350
          Z(KB) = AXYR                                                  MES 1360
          IF(KNR-NKBPTS) 132,131,131                                    MES 1370
  131     ADR = ZDEL                                                    MES 1380
  132   CONTINUE                                                        MES 1390
  133 CONTINUE                                                          MES 1400
      NREGIJ = NREGI                                                    MES 1410
      IF(MGEM-8) 135,134,135                                            MES 1420
  134 NREGIJ = NREGJ                                                    MES 1430
  135 I = 0                                                             MES 1440
      YY(1) = 0.0                                                       MES 1450
      ZXYR = 0.D+0                                                      MES 1460
      ADR = 0.0                                                         MES 1470
      DO 147 IR=1,NREGIJ                                                MES 1480
        IF(MGEM-8) 136,137,136                                          MES 1490
  136   NIPTS = MSHI(IR)                                                MES 1500
        XNIPTS = FLOAT(NIPTS)                                           MES 1510
        XDEL = XSHI(IR) / XNIPTS                                        MES 1520
        GOTO 138                                                        MES 1530
  137   NIPTS = MSHJ(IR)                                                MES 1540
        XNIPTS = FLOAT(NIPTS)                                           MES 1550
        XDEL = XSHJ(IR) / XNIPTS                                        MES 1560
  138   DO 146 INR=1,NIPTS                                              MES 1570
          I = I + 1                                                     MES 1580
          IF((NGEM .NE. 10) .AND. (NGEM .NE. 14)) GOTO 139              MES 1590
          Y(I) = YY(I) + H                                              MES 1600
          YY(I+1) = YY(I) + 3.0 * H                                     MES 1610
          PDI(I) = YY(I+1) - H                                          MES 1620
          GOTO 146                                                      MES 1630
  139     CONTINUE                                                      MES 1640
          ZXYR = ZXYR + XDEL                                            MES 1650
          IF(I-1) 140,140,141                                           MES 1660
  140     AXYR = 0.5D+0 * XDEL                                          MES 1670
          GOTO 144                                                      MES 1680
  141     IF(INR-1) 142,142,143                                         MES 1690
  142     AXYR = AXYR + 0.5 * (XDEL+ADR)                                MES 1700
          GOTO 144                                                      MES 1710
  143     AXYR = AXYR + XDEL                                            MES 1720
  144     Y(I) = AXYR                                                   MES 1730
          YY(I+1) = ZXYR                                                MES 1740
          IF(INR-NIPTS) 146,145,145                                     MES 1750
  145     ADR = XDEL                                                    MES 1760
  146   CONTINUE                                                        MES 1770
  147 CONTINUE                                                          MES 1780
      ZTEMP = 1.0 / 3.0                                                 MES 1790
      J = 0                                                             MES 1800
      XX(1) = 0.0                                                       MES 1810
      ZXYR = 0.D+0                                                      MES 1820
      ZDEL1 = ZZ(2) - ZZ(1)                                             MES 1830
      YDEL1 = YY(2) - YY(1)                                             MES 1840
      IF(NGEM-5) 148,148,149                                            MES 1850
  148 T1 = 1.0                                                          MES 1860
      GOTO 152                                                          MES 1870
  149 IF(NGEM-11) 150,151,151                                           MES 1880
  150 T1 = YDEL1                                                        MES 1890
      GOTO 152                                                          MES 1900
  151 T1 = YDEL1 * ZDEL1                                                MES 1910
  152 CONTINUE                                                          MES 1920
      YDELSQ = YY(2)**2 - YY(1)**2                                      MES 1930
      NREGIJ = NREGJ                                                    MES 1940
      IF(MGEM-8) 154,153,154                                            MES 1950
  153 NREGIJ = NREGI                                                    MES 1960
  154 DO 164 JR=1,NREGIJ                                                MES 1970
        IF(MGEM .EQ. 8) GOTO 155                                        MES 1980
        NJPTS = MSHJ(JR)                                                MES 1990
        TEMP = PVOL(JR) / T1                                            MES 2000
        XNJPTS = FLOAT(NJPTS)                                           MES 2010
        XDEL = XSHJ(JR) / XNJPTS                                        MES 2020
        GOTO 156                                                        MES 2030
  155   NJPTS = MSHI(JR)                                                MES 2040
        JS = (JR-1) * NREGJ + 1                                         MES 2050
        TEMP = PVOL(JS) / T1                                            MES 2060
        XNJPTS = FLOAT(NJPTS)                                           MES 2070
        XDEL = XSHI(JR) / XNJPTS                                        MES 2080
  156   DO 163 JNR=1,NJPTS                                              MES 2090
          J = J + 1                                                     MES 2100
          GOTO(157,158,161,163,163,157,158,159,157,160,157,159,157,160),MES 2110
     1     NGEM                                                         MES 2120
  157     ZXYR = ZXYR + XDEL                                            MES 2130
          GOTO 162                                                      MES 2140
  158     ZXYR = DSQRT(TEMP/3.141593+ZXYR**2)                           MES 2150
          GOTO 162                                                      MES 2160
  159     ZXYR = DSQRT(2.0*TEMP+ZXYR **2)                               MES 2170
          GOTO 162                                                      MES 2180
  160     CONTINUE                                                      MES 2190
          X(J) = XX(J) + H                                              MES 2200
          XX(J+1) = X(J) + H                                            MES 2210
          GOTO 163                                                      MES 2220
  161     ZXYR = (3.0*TEMP/(4.0*3.141593)+ZXYR**3)**ZTEMP               MES 2230
  162     CONTINUE                                                      MES 2240
          XX(J+1) = ZXYR                                                MES 2250
  163   CONTINUE                                                        MES 2260
  164 CONTINUE                                                          MES 2270
      IF(NGEM .EQ. 10 .OR. NGEM .EQ. 14) GOTO 171                       MES 2280
      JXLM = JMAX                                                       MES 2290
      IF(MGEM .EQ. 8) JXLM = IMAX                                       MES 2300
      DO 168 J=1,JXLM                                                   MES 2310
        GOTO(165,166,167,168,168,165,166,166,165,168,165,166,165,168),  MES 2320
     1   NGEM                                                           MES 2330
  165   X(J) = (XX(J+1)+XX(J)) * 0.5                                    MES 2340
        GOTO 168                                                        MES 2350
  166   X(J) = SQRT((XX(J+1)**2+XX(J)**2)*0.5)                          MES 2360
        GOTO 168                                                        MES 2370
  167   X(J) = ((XX(J+1)**3+XX(J)**3)*0.5)**ZTEMP                       MES 2380
  168 CONTINUE                                                          MES 2390
      IF(MGEM-8) 171,169,171                                            MES 2400
  169 DO 170 N=1,NXY                                                    MES 2410
        TEMP1 = XX(N)                                                   MES 2420
        TEMP2 = YY(N)                                                   MES 2430
        XX(N) = TEMP2                                                   MES 2440
        YY(N) = TEMP1                                                   MES 2450
        TEMP1 = X(N)                                                    MES 2460
        TEMP2 = Y(N)                                                    MES 2470
        X(N) = TEMP2                                                    MES 2480
        Y(N) = TEMP1                                                    MES 2490
  170 CONTINUE                                                          MES 2500
  171 CONTINUE                                                          MES 2510
      IMXP1 = IMAX + 1                                                  MES 2520
      JMXP1 = JMAX + 1                                                  MES 2530
      KBMXP1 = KBMAX + 1                                                MES 2540
      GOTO(172,174,175,175,175,176,179,180,181,182,183,187,188,189),NGEMMES 2550
  172 WRITE (IOUT,1005) XX(JMAX+1)                                      MES 2560
  173 WRITE (IOUT,1006)                                                 MES 2570
      WRITE (IOUT,1007) (MSHJ(J),XSHJ(J),J=1,NREGJ)                     MES 2580
      WRITE (IOUT,1008) JMAX                                            MES 2590
      WRITE (IOUT,1009)                                                 MES 2600
      WRITE (IOUT,1010) (J,XX(J),J=2,JMXP1)                             MES 2610
      WRITE (IOUT,1011)                                                 MES 2620
      WRITE (IOUT,1010) (J,X(J),J=1,JMAX)                               MES 2630
      GOTO 190                                                          MES 2640
  174 WRITE (IOUT,1012) XX(JMAX+1)                                      MES 2650
      GOTO 173                                                          MES 2660
  175 WRITE (IOUT,1013) XX(JMAX+1)                                      MES 2670
      GOTO 173                                                          MES 2680
  176 WRITE (IOUT,1014) XX(JMAX+1),YY(IMAX+1)                           MES 2690
  177 WRITE (IOUT,1006)                                                 MES 2700
      WRITE (IOUT,1007) (MSHJ(J),XSHJ(J),J=1,NREGJ)                     MES 2710
      WRITE (IOUT,1015)                                                 MES 2720
      WRITE (IOUT,1007) (MSHI(I),XSHI(I),I=1,NREGI)                     MES 2730
      WRITE (IOUT,1016) JMAX,IMAX                                       MES 2740
      WRITE (IOUT,1009)                                                 MES 2750
      WRITE (IOUT,1010) (J,XX(J),J=2,JMXP1)                             MES 2760
      WRITE (IOUT,1017)                                                 MES 2770
      WRITE (IOUT,1010) (I,YY(I),I=2,IMXP1)                             MES 2780
      WRITE (IOUT,1011)                                                 MES 2790
      WRITE (IOUT,1010) (J,X(J),J=1,JMAX)                               MES 2800
      IF(NGEM .NE. 10) GOTO 178                                         MES 2810
      WRITE (IOUT,1000)                                                 MES 2820
      WRITE (IOUT,1001) (I,Y(I),PDI(I),I=1,IMAX)                        MES 2830
      GOTO 190                                                          MES 2840
  178 CONTINUE                                                          MES 2850
      WRITE (IOUT,1017)                                                 MES 2860
      WRITE (IOUT,1010) (I,Y(I),I=1,IMAX)                               MES 2870
      DO 2001 I=1,IMAX                                                  MES 2880
        YFLX(I) = Y(I)                                                  MES 2890
 2001 CONTINUE                                                          MES 2900
      DO 2002 J=1,JMAX                                                  MES 2910
        XFLX(J) = X(J)                                                  MES 2920
 2002 CONTINUE                                                          MES 2930
      GOTO 190                                                          MES 2940
  179 WRITE (IOUT,1018) XX(JMAX+1),YY(IMAX+1)                           MES 2950
      GOTO 177                                                          MES 2960
  180 WRITE (IOUT,1019) XX(JMAX+1),YY(IMAX+1)                           MES 2970
      GOTO 177                                                          MES 2980
  181 WRITE (IOUT,1020) XX(JMAX+1),YY(IMAX+1)                           MES 2990
      GOTO 177                                                          MES 3000
  182 CONTINUE                                                          MES 3010
      WRITE (IOUT,1002) XX(JMAX+1),YY(IMAX+1)                           MES 3020
      GOTO 177                                                          MES 3030
  183 WRITE (IOUT,1021) XX(JMAX+1),YY(IMAX+1),ZZ(KBMAX+1)               MES 3040
  184 WRITE (IOUT,1006)                                                 MES 3050
      WRITE (IOUT,1007) (MSHJ(J),XSHJ(J),J=1,NREGJ)                     MES 3060
      WRITE (IOUT,1015)                                                 MES 3070
      WRITE (IOUT,1007) (MSHI(I),XSHI(I),I=1,NREGI)                     MES 3080
      WRITE (IOUT,1022)                                                 MES 3090
      WRITE (IOUT,1007) (MSHKB(I),XSHKB(I),I=1,NREGKB)                  MES 3100
      WRITE (IOUT,1023) JMAX,IMAX,KBMAX                                 MES 3110
      WRITE (IOUT,1009)                                                 MES 3120
      WRITE (IOUT,1010) (J,XX(J),J=2,JMXP1)                             MES 3130
      WRITE (IOUT,1017)                                                 MES 3140
      WRITE (IOUT,1010) (I,YY(I),I=2,IMXP1)                             MES 3150
      WRITE (IOUT,1024)                                                 MES 3160
      WRITE (IOUT,1010) (KB,ZZ(KB),KB=2,KBMXP1)                         MES 3170
      WRITE (IOUT,1011)                                                 MES 3180
      WRITE (IOUT,1010) (J,X(J),J=1,JMAX)                               MES 3190
      IF(NGEM .NE. 14) GOTO 185                                         MES 3200
      WRITE (IOUT,1000)                                                 MES 3210
      WRITE (IOUT,1001) (I,Y(I),PDI(I),I=1,IMAX)                        MES 3220
      GOTO 186                                                          MES 3230
  185 CONTINUE                                                          MES 3240
      WRITE (IOUT,1017)                                                 MES 3250
      WRITE (IOUT,1010) (I,Y(I),I=1,IMAX)                               MES 3260
  186 CONTINUE                                                          MES 3270
      WRITE (IOUT,1024)                                                 MES 3280
      WRITE (IOUT,1010) (KB,Z(KB),KB=1,KBMAX)                           MES 3290
      GOTO 190                                                          MES 3300
  187 WRITE (IOUT,1025) XX(JMAX+1),YY(IMAX+1),ZZ(KBMAX+1)               MES 3310
      GOTO 184                                                          MES 3320
  188 WRITE (IOUT,1026) XX(JMAX+1),YY(IMAX+1),ZZ(KBMAX+1)               MES 3330
      GOTO 184                                                          MES 3340
  189 CONTINUE                                                          MES 3350
      WRITE (IOUT,1003) XX(JMAX+1),YY(IMAX+1),ZZ(KBMAX+1)               MES 3360
      GOTO 184                                                          MES 3370
  190 CONTINUE                                                          MES 3380
      DO 191 J=1,JMAX                                                   MES 3390
        IF(XX(J)-XX(J+1)) 191,194,194                                   MES 3400
  191 CONTINUE                                                          MES 3410
      DO 192 I=1,IMAX                                                   MES 3420
        IF(YY(I)-YY(I+1)) 192,194,194                                   MES 3430
  192 CONTINUE                                                          MES 3440
      DO 193 KB=1,KBMAX                                                 MES 3450
        IF(ZZ(KB)-ZZ(KB+1)) 193,193,194                                 MES 3460
  193 CONTINUE                                                          MES 3470
      GOTO 195                                                          MES 3480
  194 NER(3) = 3                                                        MES 3490
  195 IF(MESHGE .EQ. 0) GOTO 196                                        MES 3500
      WRITE (IOUT,1027)                                                 MES 3510
  196 CONTINUE                                                          MES 3520
      IF((NGEM .NE. 10) .AND. (NGEM .NE. 14)) GOTO 200                  MES 3530
      DO 199 I=1,NREGI                                                  MES 3540
        T1 = 2 * MSHI(I)                                                MES 3550
        T2 = XSHI(I)                                                    MES 3560
        DO 198 J=1,NREGJ                                                MES 3570
          T3 = MSHJ(J)                                                  MES 3580
          T4 = XSHJ(J)                                                  MES 3590
          T5 = (T1*T4) / (T2*T3)                                        MES 3600
          T6 = ABS(T5-1.0)                                              MES 3610
          IF(T6 .LE. 0.00001) GOTO 197                                  MES 3620
          WRITE (IOUT,1004) I,J,T1,T4,T3,T2,T5                          MES 3630
  197     CONTINUE                                                      MES 3640
  198   CONTINUE                                                        MES 3650
  199 CONTINUE                                                          MES 3660
  200 CONTINUE                                                          MES 3670
      IF(NGC(7) .EQ. 0) GOTO 201                                        MES 3680
      IO32 = IX(138)                                                    MES 3690
      REWIND IO32                                                       MES 3700
      WRITE (IO32) (TITL1(I),I=1,18),(TITL2(I),I=1,18),NUAC(5),JMAX,IMAXMES 3710
     1 ,KBMAX,NGC(21),NGC(7),(X(J),J=1,JMAX),(Y(I),I=1,IMAX),(Z(KB),KB=1MES 3720
     2 ,KBMAX)                                                          MES 3730
      END FILE IO32                                                     MES 3740
      REWIND IO32                                                       MES 3750
  201 CONTINUE                                                          MES 3760
      RETURN                                                            MES 3770
C                                                                       MES 3780
 1000 FORMAT (1H0,'    I   ODD J  EVEN J')                              MES 3790
 1001 FORMAT (1H ,I5,2F8.3,I5,2F8.3,I5,2F8.3,I5,2F8.3,I5,2F8.3,I5,2F8.3)MES 3800
 1002 FORMAT (2H0 ,'TWO DIMENSIONAL TRIANGULAR GEOMETRY (X,Y)',2H  ,'WIDMES 3810
     1TH',1PE14.6,2H  ,'HEIGHT',E14.6//)                                MES 3820
 1003 FORMAT (2H0 ,'THREE DIMENSIONAL TRIANGULAR GEOMETRY (X,Y,Z)',2H  ,MES 3830
     1 'WIDTH',1PE14.6,2H  ,'HEIGHT',E14.6,2H  ,'DEPTH',E14.6//)        MES 3840
 1004 FORMAT (1H0,'******WARNING--TRIANGLES NOT EQULATERAL FOR HORIZONTAMES 3850
     1L-VERTICAL REGION',2I3/1H ,'2*I, WIDTH, J, HEIGHT, AND (2*I*WIDTH)MES 3860
     2/(J*HEIGHT) ARE',5(1PE13.5))                                      MES 3870
 1005 FORMAT (2H0 ,'ONE DIMENSIONAL SLAB GEOMETRY (X)',2H  ,'WIDTH',    MES 3880
     1 1PE14.6//)                                                       MES 3890
 1006 FORMAT (2H0,'REGION SPECIFICATIONS'/4H    ,'PTS  REGION WIDTH')   MES 3900
 1007 FORMAT (1H ,I6,1PE14.6,I6,E14.6,I6,E14.6,I6,E14.6,I6,E14.6,I6,    MES 3910
     1 E14.6)                                                           MES 3920
 1008 FORMAT (2H0 ,'X-DIR. POINTS',I4)                                  MES 3930
 1009 FORMAT (2H0,'DISTANCES TO MESH INTERVAL',1H ,'INTERFACES'/1H0,'   MES 3940
     1 J   DIST.')                                                      MES 3950
 1010 FORMAT (1H ,I5,F8.3,I5,F8.3,I5,F8.3,I5,F8.3,I5,F8.3,I5,F8.3,I5,   MES 3960
     1 F8.3,I5,F8.3,I5,F8.3)                                            MES 3970
 1011 FORMAT (2H0,'DISTANCES TO FLUX POINTS'/1H0,'    J   DIST.')       MES 3980
 1012 FORMAT (2H0,'ONE DIMENSIONAL CYLINDRICAL GEOMETRY (R)',2H  ,'WIDTHMES 3990
     1',1PE14.6//)                                                      MES 4000
 1013 FORMAT (2H0,'ONE DIMENSIONAL SPHERICAL GEOMETRY (R)',2H  ,'WIDTH',MES 4010
     1 1PE14.6//)                                                       MES 4020
 1014 FORMAT (2H0,'TWO DIMENSIONAL SLAB GEOMETRY (X,Y)',2H  ,'WIDTH',   MES 4030
     1 1PE14.6,2H  ,'HEIGHT',E14.6//)                                   MES 4040
 1015 FORMAT (4H0   ,'PTS  REGION HEIGHT')                              MES 4050
 1016 FORMAT (2H0,'X-DIR. POINTS',I4,9H         ,'Y-DIR. POINTS',I4)    MES 4060
 1017 FORMAT (1H0,'    I   DIST.')                                      MES 4070
 1018 FORMAT (2H0,'TWO DIMENSIONAL CYLINDRICAL GEOMETRY (R,Z)',2H  ,'WIDMES 4080
     1TH',1PE14.6,2H  ,'HEIGHT',E14.6//)                                MES 4090
 1019 FORMAT (2H0,'TWO DIMENSIONAL CIRCULAR GEOMETRY (R,THETA)',2H  ,'WIMES 4100
     1DTH',1PE14.6,2H  ,'HEIGHT',E14.6//)                               MES 4110
 1020 FORMAT (2H0,'TWO DIMENSIONAL HEXAGIONAL GEOMETRY (X,Y)',2H  ,'WIDTMES 4120
     1H',1PE14.6,2H  ,'HEIGHT',E14.6//)                                 MES 4130
 1021 FORMAT (2H0,'THREE DIMENSIONAL SLAB GEOMETRY (X,Y,Z)',2H  ,'WIDTH'MES 4140
     1 ,1PE14.6,2H  ,'HEIGHT',E14.6,2H  ,'DEPTH',E14.6//)               MES 4150
 1022 FORMAT (4H0   ,'PTS  REGION DEPTH')                               MES 4160
 1023 FORMAT (2H0,'X-DIR. POINTS',I4,9H         ,'Y-DIR. POINTS',I4,9H  MES 4170
     1       ,'Z-DIR. POINTS',I4)                                       MES 4180
 1024 FORMAT (1H0,'    KB   DIST')                                      MES 4190
 1025 FORMAT (2H0,'THREE DIMENSIONAL CYLINDRICAL GEOMETRY (R,THETA,Z)', MES 4200
     1 2H  ,'WIDTH',1PE14.6,2H  ,'HEIGHT',E14.6,2H  ,'DEPTH',E14.6//)   MES 4210
 1026 FORMAT (2H0,'THREE DIMENSIONAL HEXAGIONAL GEOMETRY (X,Y,Z)',2H  , MES 4220
     1 'WIDTH',1PE14.6,2H  ,'HEIGHT',E14.6,2H  ,'DEPTH',E14.6//)        MES 4230
 1027 FORMAT (1H0,'*****WARNING--REGION VOLUMES NOT EQUAL IN HEX GEOM.')MES 4240
      END                                                               MES 4250
      SUBROUTINE OVER(NRGN,NCOMP,RVOL,PVOL,IVX,JVX,KBVX,LSZ,NNN1,N222)  OVE   10
C                                                                       OVE   20
COVER --034 ***CITATION*** READS OVERLAY SECTION 006/ CF-IPTM           OVE   30
C                                                                       OVE   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,OVE   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   OVE   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), OVE   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    OVE   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    OVE   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   OVE  100
     6 IXPUT(9999),XPUT(9999)                                           OVE  110
C                                                                       OVE  120
CFZJ055                                                       25.09.07  OVE  130
C                                                                       OVE  140
      DIMENSION NRGN(JVX,IVX,KBVX),NCOMP(LSZ),RVOL(LSZ),PVOL(LSZ),      OVE  150
     1 NOVER(6,9999),JL(3),JR(3),IT(3),IB(3),KBF(3),KBB(3)              OVE  160
C                                                                       OVE  170
C                                                                       OVE  180
      WRITE (IOUT,1000)                                                 OVE  190
      IF(NUAC(5) .GT. 5) GOTO 100                                       OVE  200
      IGO = 1                                                           OVE  210
      GOTO 102                                                          OVE  220
  100 IF(NUAC(5) .GT. 10) GOTO 101                                      OVE  230
      IGO = 2                                                           OVE  240
      GOTO 102                                                          OVE  250
  101 IGO = 3                                                           OVE  260
  102 LENTRY = 0                                                        OVE  270
      READ (IOIN,1001) M,N                                              OVE  280
      IF(M .EQ. 0) GOTO 137                                             OVE  290
      WRITE (IOUT,1002) M                                               OVE  300
  103 READ (IOIN,1001) (JL(I),JR(I),IT(I),IB(I),KBF(I),KBB(I),I=1,3)    OVE  310
      DO 104 I=1,3                                                      OVE  320
        IF(JL(I) .EQ. 0) GOTO 105                                       OVE  330
        LENTRY = LENTRY + 1                                             OVE  340
        NOVER(1,LENTRY) = JL(I)                                         OVE  350
        NOVER(2,LENTRY) = JR(I)                                         OVE  360
        NOVER(3,LENTRY) = IT(I)                                         OVE  370
        NOVER(4,LENTRY) = IB(I)                                         OVE  380
        NOVER(5,LENTRY) = KBF(I)                                        OVE  390
        NOVER(6,LENTRY) = KBB(I)                                        OVE  400
  104 CONTINUE                                                          OVE  410
      GOTO 103                                                          OVE  420
  105 DO 116 L=1,LENTRY                                                 OVE  430
        IF((NOVER(1,L) .GT. 0) .AND. (NOVER(1,L) .LE. JMAX)) GOTO 106   OVE  440
        NER(6) = 6                                                      OVE  450
  106   IF((NOVER(2,L) .GT. 0) .AND. (NOVER(2,L) .LE. JMAX)) GOTO 107   OVE  460
        NER(6) = 6                                                      OVE  470
  107   IF(NOVER(1,L) .LE. NOVER(2,L)) GOTO 108                         OVE  480
        NER(6) = 6                                                      OVE  490
  108   GOTO(116,109,109),IGO                                           OVE  500
  109   IF((NOVER(3,L) .GT. 0) .AND. (NOVER(3,L) .LE. IMAX)) GOTO 110   OVE  510
        NER(6) = 6                                                      OVE  520
  110   IF((NOVER(4,L) .GT. 0) .AND. (NOVER(4,L) .LE. IMAX)) GOTO 111   OVE  530
        NER(6) = 6                                                      OVE  540
  111   IF(NOVER(3,L) .LE. NOVER(4,L)) GOTO 112                         OVE  550
        NER(6) = 6                                                      OVE  560
  112   GOTO(116,116,113),IGO                                           OVE  570
  113   IF((NOVER(5,L) .GT. 0) .AND. (NOVER(5,L) .LE. KBMAX)) GOTO 114  OVE  580
        NER(6) = 6                                                      OVE  590
  114   IF((NOVER(6,L) .GT. 0) .AND. (NOVER(6,L) .LE. KBMAX)) GOTO 115  OVE  600
        NER(6) = 6                                                      OVE  610
  115   IF(NOVER(5,L) .LE. NOVER(6,L)) GOTO 116                         OVE  620
        NER(6) = 6                                                      OVE  630
  116 CONTINUE                                                          OVE  640
      GOTO(117,118,119),IGO                                             OVE  650
  117 WRITE (IOUT,1003) (NOVER(1,L),NOVER(2,L),L=1,LENTRY)              OVE  660
      GOTO 120                                                          OVE  670
  118 WRITE (IOUT,1003) (NOVER(1,L),NOVER(2,L),NOVER(3,L),NOVER(4,L),L=1OVE  680
     1 ,LENTRY)                                                         OVE  690
      GOTO 120                                                          OVE  700
  119 WRITE (IOUT,1003) (NOVER(1,L),NOVER(2,L),NOVER(3,L),NOVER(4,L),   OVE  710
     1 NOVER(5,L),NOVER(6,L),L=1,LENTRY)                                OVE  720
  120 IF(NER(6) .NE. 6) GOTO 121                                        OVE  730
      WRITE (IOUT,1004)                                                 OVE  740
C                                                                       OVE  750
      CALL EXIT                                                         OVE  760
C                                                                       OVE  770
  121 CONTINUE                                                          OVE  780
      I1 = 1                                                            OVE  790
      I2 = 1                                                            OVE  800
      KB1 = 1                                                           OVE  810
      KB2 = 1                                                           OVE  820
      DO 136 LE=1,LENTRY                                                OVE  830
        J1 = NOVER(1,LE)                                                OVE  840
        J2 = NOVER(2,LE)                                                OVE  850
        GOTO(124,122,122),IGO                                           OVE  860
  122   I1 = NOVER(3,LE)                                                OVE  870
        I2 = NOVER(4,LE)                                                OVE  880
        GOTO(124,124,123),IGO                                           OVE  890
  123   KB1 = NOVER(5,LE)                                               OVE  900
        KB2 = NOVER(6,LE)                                               OVE  910
  124   DO 135 KB=KB1,KB2                                               OVE  920
          DO 134 I=I1,I2                                                OVE  930
            DO 133 J=J1,J2                                              OVE  940
              IF(N .EQ. 0) GOTO 798                                     OVE  950
              IF(KB .EQ. KB1 .AND. I .EQ. I1 .AND. J .EQ. J1) GOTO 125  OVE  960
              GOTO 799                                                  OVE  970
  798         CONTINUE                                                  OVE  980
              IF(LE .EQ. 1 .AND. KB .EQ. KB1 .AND. I .EQ. I1 .AND. J    OVE  990
     1         .EQ. J1) GOTO 125                                        OVE 1000
  799         CONTINUE                                                  OVE 1010
              L2 = NRGN(J,I,KB)                                         OVE 1020
              VOL2 = PVOL(L2)                                           OVE 1030
              T1 = ABS(VOL2/VOL1-1.0)                                   OVE 1040
              IF(T1 .GE. 1.0E-6) GOTO 125                               OVE 1050
              IF(IND .EQ. 0) GOTO 132                                   OVE 1060
              GOTO 130                                                  OVE 1070
  125         CONTINUE                                                  OVE 1080
              L1 = NRGN(J,I,KB)                                         OVE 1090
              VOL1 = PVOL(L1)                                           OVE 1100
              IND = 0                                                   OVE 1110
              IF(N .GT. 0) GOTO 800                                     OVE 1120
              DO 126 KKBB=1,KBMAX                                       OVE 1130
                LREG = NRGN(J,I,KKBB)                                   OVE 1140
                MM = NCOMP(LREG)                                        OVE 1150
                IF(MM .NE. M) GOTO 126                                  OVE 1160
                VOTM = PVOL(LREG)                                       OVE 1170
                IF(ABS(VOTM/VOL1-1.0) .LT. 1.0E-6) GOTO 131             OVE 1180
  126         CONTINUE                                                  OVE 1190
              DO 127 IIB=1,IMAX                                         OVE 1200
                LREG = NRGN(J,IIB,KB)                                   OVE 1210
                MM = NCOMP(LREG)                                        OVE 1220
                IF(MM .NE. M) GOTO 127                                  OVE 1230
                VOTM = PVOL(LREG)                                       OVE 1240
                IF(ABS(VOTM/VOL1-1.0) .LT. 1.0E-6) GOTO 131             OVE 1250
  127         CONTINUE                                                  OVE 1260
              DO 128 JJB=1,JMAX                                         OVE 1270
                LREG = NRGN(JJB,I,KB)                                   OVE 1280
                MM = NCOMP(LREG)                                        OVE 1290
                IF(MM .NE. M) GOTO 128                                  OVE 1300
                VOTM = PVOL(LREG)                                       OVE 1310
                IF(ABS(VOTM/VOL1-1.0) .LT. 1.0E-6) GOTO 131             OVE 1320
  128         CONTINUE                                                  OVE 1330
  800         CONTINUE                                                  OVE 1340
              LMAX = LMAX + 1                                           OVE 1350
              NNN1 = NNN1 + 1                                           OVE 1360
              IND = 1                                                   OVE 1370
              IF(NNN1 .LT. N222) GOTO 129                               OVE 1380
              WRITE (IOUT,1005)                                         OVE 1390
C                                                                       OVE 1400
              CALL EXIT                                                 OVE 1410
C                                                                       OVE 1420
  129         CONTINUE                                                  OVE 1430
              NCOMP(LMAX) = M                                           OVE 1440
              PVOL(LMAX) = VOL1                                         OVE 1450
  130         CONTINUE                                                  OVE 1460
              NRGN(J,I,KB) = LMAX                                       OVE 1470
              GOTO 133                                                  OVE 1480
  131         CONTINUE                                                  OVE 1490
              NCOMP(LREG) = M                                           OVE 1500
  132         CONTINUE                                                  OVE 1510
              NRGN(J,I,KB) = LREG                                       OVE 1520
  133       CONTINUE                                                    OVE 1530
  134     CONTINUE                                                      OVE 1540
  135   CONTINUE                                                        OVE 1550
  136 CONTINUE                                                          OVE 1560
      GOTO 102                                                          OVE 1570
  137 MMAX = 0                                                          OVE 1580
      DO 138 L=1,LMAX                                                   OVE 1590
        RVOL(L) = 0.0                                                   OVE 1600
        MMAX = MAX0(MMAX,NCOMP(L))                                      OVE 1610
  138 CONTINUE                                                          OVE 1620
      DO 141 KB=1,KBVX                                                  OVE 1630
        DO 140 I=1,IVX                                                  OVE 1640
          DO 139 J=1,JVX                                                OVE 1650
            L = NRGN(J,I,KB)                                            OVE 1660
            RVOL(L) = RVOL(L) + PVOL(L)                                 OVE 1670
  139     CONTINUE                                                      OVE 1680
  140   CONTINUE                                                        OVE 1690
  141 CONTINUE                                                          OVE 1700
      RETURN                                                            OVE 1710
C                                                                       OVE 1720
 1000 FORMAT (1H0,'MESH OVERLAY INPUT')                                 OVE 1730
 1001 FORMAT (18I4)                                                     OVE 1740
 1002 FORMAT (1H0,'ZONE NUMBER',I3)                                     OVE 1750
 1003 FORMAT (1H ,36I3)                                                 OVE 1760
 1004 FORMAT (1H0,'ERROR STOP NUMBER 6')                                OVE 1770
 1005 FORMAT (1H0,'ERROR STOP NUMBER 0'/1H ,'STORAGE EXCEEDED IN OVERLAYOVE 1780
     1 CALCULATION')                                                    OVE 1790
      END                                                               OVE 1800
      SUBROUTINE POUT(P2,UTIL,IND,SOURE,IVX,JVX,KBVX,KVX,PDTHERM,FFTHX) POU   10
C                                                                       POU   20
CPOUT --137 ***CITATION*** PRINT 1,2-D FLUXES/ CF-OUTC                  POU   30
C                                                                       POU   40
C     IND = 0---FOWARD FLUX OUTPUT                                      POU   50
C     IND = 1---POINT POWER OUTPUT                                      POU   60
C     IND = 2---POINT NEUTRON DENSITY OUTPUT                            POU   70
C     IND = 3---POINT NEUTRON ABSORPTION RATE IN A NUCLIDE OUTPUT       POU   80
C     IND = 4---ADJOINT FLUX OUTPUT                                     POU   90
C     IND = 5---IMPORTANCE MAP OF UNIT CHANGE IN NUCLIDE                POU  100
C     IND = 6---RELATIVE IMPORTANCE MAP FOR NUCLIDE                     POU  110
C     IND = 7---IMPORTANCE MAP OF 1/V CROSS SECTION                     POU  120
C     IND = 8---CUMULATIVE HEAT GENERATION RATE                         POU  130
C     IND = 9---TEMPERATURE OR POWER COEFFICIENT                        POU  140
C     IND = 10--DAMAGE RATE                                             POU  150
C                                                                       POU  160
      REAL*8 P2                                                         POU  170
C                                                                       POU  180
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,POU  190
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   POU  200
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), POU  210
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    POU  220
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    POU  230
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   POU  240
     6 IXPUT(9999),XPUT(9999)                                           POU  250
C                                                                       POU  260
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   POU  270
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKPOU  280
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    POU  290
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  POU  300
     4 ITMAX,ITIME,BET(211),DEL(211)                                    POU  310
C                                                                       POU  320
      COMMON /MU/ MU4                                                   POU  330
C                                                                       POU  340
      COMMON /IFLUX/ IFLX,YFLX(211),XFLX(211),IPOW                      POU  350
C                                                                       POU  360
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             POU  370
C                                                                       POU  380
      COMMON /PDTX/ PDTHX                                               POU  390
C                                                                       POU  400
      DIMENSION P2(JVX,IVX,KVX),UTIL(JVX,IVX,KBVX),SOURE(JVX,IVX,KBVX), POU  410
     1 PDTHERM(IMAZ,NMAZ),FFTHX(IMAZ,NMAZ)                              POU  420
C                                                                       POU  430
C                                                                       POU  440
      IS1 = 0                                                           POU  450
      IS2 = 0                                                           POU  460
      IPG = 0                                                           POU  470
      IF(IND .EQ. 9) GOTO 104                                           POU  480
      IX50 = 0                                                          POU  490
      N3 = 3                                                            POU  500
      IF(IND .EQ. 0 .OR. IND .EQ. 4) GOTO 101                           POU  510
      N = IMAX * JMAX                                                   POU  520
      IF(IMAX .GT. 25 .OR. N .GT. 550) GOTO 101                         POU  530
      IX50 = 1                                                          POU  540
      IF(IMAX .LE. 15 .AND. JMAX .LE. 11) N3 = 4                        POU  550
      IF(IX(50) .GT. 0) GOTO 102                                        POU  560
  101 CONTINUE                                                          POU  570
      IF(MU4 .EQ. 1) WRITE (IOUT,1001) (TITL1(I),I=1,18)                POU  580
      IF(MU4 .EQ. 1) WRITE (IOUT,1002) (TITL2(I),I=1,18)                POU  590
      IF(IX50 .EQ. 0) GOTO 103                                          POU  600
  102 CONTINUE                                                          POU  610
      IX(50) = IX(50) + 1                                               POU  620
      IF(IX(50) .EQ. N3) GOTO 101                                       POU  630
  103 CONTINUE                                                          POU  640
      IF(IX(50) .EQ. N3) IPG = 1                                        POU  650
      IF(IX(50) .EQ. N3) IX(50) = 0                                     POU  660
      KLAX = KMAX                                                       POU  670
      IF(IND .EQ. 0 .OR. IND .EQ. 4) GOTO 105                           POU  680
  104 CONTINUE                                                          POU  690
      KLAX = 1                                                          POU  700
  105 DO 136 K=1,KLAX                                                   POU  710
        IF(IND .EQ. 9) GOTO 106                                         POU  720
        IF(PDTHX .NE. 1.) WRITE (IOUT,1003)                             POU  730
  106   CONTINUE                                                        POU  740
        IF(IND .NE. 8) GOTO 113                                         POU  750
        NGO = NGC(21)                                                   POU  760
        IF(NGO .EQ. 0) NGO = 1                                          POU  770
        GOTO(107,108,109,110,111,112),NGO                               POU  780
  107   WRITE (IOUT,1004)                                               POU  790
        GOTO 122                                                        POU  800
  108   WRITE (IOUT,1005)                                               POU  810
        GOTO 122                                                        POU  820
  109   WRITE (IOUT,1006)                                               POU  830
        GOTO 122                                                        POU  840
  110   WRITE (IOUT,1007)                                               POU  850
        GOTO 122                                                        POU  860
  111   WRITE (IOUT,1008)                                               POU  870
        GOTO 122                                                        POU  880
  112   WRITE (IOUT,1009)                                               POU  890
        GOTO 122                                                        POU  900
  113   CONTINUE                                                        POU  910
        IF(IND .EQ. 9) GOTO 122                                         POU  920
        IF(IND .EQ. 7) GOTO 115                                         POU  930
        IF(IND .EQ. 6) GOTO 114                                         POU  940
        IF(IND .NE. 5) GOTO 116                                         POU  950
        WRITE (IOUT,1010) IXPUT(34),SPARE(91)                           POU  960
        GOTO 122                                                        POU  970
  114   WRITE (IOUT,1011) IXPUT(35)                                     POU  980
        GOTO 122                                                        POU  990
  115   WRITE (IOUT,1012)                                               POU 1000
        GOTO 122                                                        POU 1010
  116   CONTINUE                                                        POU 1020
        IF(IND .NE. 4) GOTO 117                                         POU 1030
        WRITE (IOUT,1013) K                                             POU 1040
        GOTO 122                                                        POU 1050
  117   CONTINUE                                                        POU 1060
        NUD = IX(165)                                                   POU 1070
        IF(IND .EQ. 10) WRITE (IOUT,1000) NUD                           POU 1080
        IF(IND .EQ. 10) GOTO 122                                        POU 1090
        IF(IND .LT. 3) GOTO 118                                         POU 1100
        WRITE (IOUT,1014) IX(117)                                       POU 1110
        GOTO 122                                                        POU 1120
  118   CONTINUE                                                        POU 1130
        IF(IND-1) 120,121,119                                           POU 1140
  119   WRITE (IOUT,1015)                                               POU 1150
        GOTO 122                                                        POU 1160
  120   IF(IEDG(10) .NE. 0 .AND. PDTHX .NE. 1.) WRITE (IOUT,1016) K     POU 1170
        GOTO 122                                                        POU 1180
  121   IF(PDTHX .NE. 1.) WRITE (IOUT,1017)                             POU 1190
  122   CONTINUE                                                        POU 1200
        N1J = JMAX                                                      POU 1210
        N2J = 0                                                         POU 1220
  123   N1I = IMAX                                                      POU 1230
        N2I = 0                                                         POU 1240
        IF(N1J-11) 124,124,125                                          POU 1250
  124   N1L = N2J + 1                                                   POU 1260
        N2J = JMAX                                                      POU 1270
        IS1 = 1                                                         POU 1280
        GOTO 126                                                        POU 1290
  125   N1L = N2J + 1                                                   POU 1300
        N2J = N2J + 11                                                  POU 1310
        N1J = N1J - 11                                                  POU 1320
  126   CONTINUE                                                        POU 1330
        IF(N1I-50) 127,127,128                                          POU 1340
  127   N2L = N2I + 1                                                   POU 1350
        N2I = IMAX                                                      POU 1360
        IS2 = 1                                                         POU 1370
        GOTO 129                                                        POU 1380
  128   N2L = N2I + 1                                                   POU 1390
        N2I = N2I + 50                                                  POU 1400
        N1I = N1I - 50                                                  POU 1410
  129   IF(PDTHX .EQ. 0.) WRITE (IOUT,1018) (J,J=N1L,N2J)               POU 1420
        DO 133 I=N2L,N2I                                                POU 1430
          IF(IND .EQ. 9) GOTO 132                                       POU 1440
          IF(IND .EQ. 5 .OR. IND .EQ. 6 .OR. IND .EQ. 7 .OR. IND .EQ. 8)POU 1450
     1     GOTO 132                                                     POU 1460
          IF(IND .EQ. 0 .OR. IND .EQ. 4) GOTO 131                       POU 1470
          IF(PDTHX .EQ. 0.) WRITE (IOUT,1019) I,(UTIL(J,I,1)*XMIS(4),   POU 1480
     1     J=N1L,N2J)                                                   POU 1490
          GOTO 133                                                      POU 1500
  131     IF(PDTHX .EQ. 0.) WRITE (IOUT,1019) I,(P2(J,I,K),J=N1L,N2J)   POU 1510
          GOTO 133                                                      POU 1520
  132     CONTINUE                                                      POU 1530
          WRITE (IOUT,1019) I,(SOURE(J,I,1),J=N1L,N2J)                  POU 1540
  133   CONTINUE                                                        POU 1550
        IF(IS2) 126,126,134                                             POU 1560
  134   IS2 = 0                                                         POU 1570
        IF(IS1) 123,123,135                                             POU 1580
  135   IS1 = 0                                                         POU 1590
  136 CONTINUE                                                          POU 1600
      IF(PDTHX .GT. 0.) GOTO 2010                                       POU 1610
      IF(IND .NE. 0) GOTO 2010                                          POU 1620
      GOTO(2005,2006),IFLX                                              POU 1630
 2005 CONTINUE                                                          POU 1640
      GOTO 2010                                                         POU 1650
 2006 CONTINUE                                                          POU 1660
      WRITE (68,2007) (XFLX(J),J=1,JMAX)                                POU 1670
      WRITE (68,1003)                                                   POU 1680
      DO 2002 K=1,KMAX                                                  POU 1690
        DO 2002 I=1,IMAX                                                POU 1700
          WRITE (68,2003) YFLX(I),(P2(J,I,K),J=1,JMAX)                  POU 1710
 2002 CONTINUE                                                          POU 1720
 2010 CONTINUE                                                          POU 1730
CFZJ023 create array FFTHX                                    29.01.04  POU 1740
CFZJ040 Initialization of array FFTHX                         12.01.05  POU 1750
      DO 504 I=1,NMAZ                                                   POU 1760
        DO 504 J=1,IMAZ                                                 POU 1770
          FFTHX(J,I) = 0.                                               POU 1780
  504 CONTINUE                                                          POU 1790
      DO 500 I=1,IMAX                                                   POU 1800
        DO 500 J=1,JMAX                                                 POU 1810
          PDTHERM(J,I) = UTIL(J,I,1)*XMIS(4)                            POU 1820
          IF(PDTHX .EQ. 1.) FFTHX(J,I) = P2(J,I,1)                      POU 1830
  500 CONTINUE                                                          POU 1840
      IF(PDTHX .NE. 1) GOTO 503                                         POU 1850
      DO 501 J=1,JMAX                                                   POU 1860
        FFTHX(J,IMAX+1) = FFTHX(J,IMAX)                                 POU 1870
  501 CONTINUE                                                          POU 1880
      DO 502 I=1,IMAX+1                                                 POU 1890
        FFTHX(JMAX+1,I) = FFTHX(JMAX,I)                                 POU 1900
  502 CONTINUE                                                          POU 1910
  503 CONTINUE                                                          POU 1920
      IF(PDTHX .GT. 0.) GOTO 2020                                       POU 1930
      IF(IND .NE. 1) GOTO 2020                                          POU 1940
      GOTO(2020,2035),IPOW                                              POU 1950
 2035 CONTINUE                                                          POU 1960
      WRITE (80,2007) (XFLX(J),J=1,JMAX)                                POU 1970
      WRITE (80,1003)                                                   POU 1980
      DO 2036 I=1,IMAX                                                  POU 1990
        WRITE (80,2003) YFLX(I),(PDTHERM(J,I),J=1,JMAX)                 POU 2000
 2036 CONTINUE                                                          POU 2010
 2020 CONTINUE                                                          POU 2020
      IF(IPG .EQ. 0) GOTO 137                                           POU 2030
      WRITE (IOUT,1001) (TITL1(I),I=1,18)                               POU 2040
      WRITE (IOUT,1002) (TITL2(I),I=1,18)                               POU 2050
  137 CONTINUE                                                          POU 2060
C                                                                       POU 2070
C     NOTE SEQUENCING 2000 - 2200 FOLLOWING                             POU 2080
C                                                                       POU 2090
      IF(NGC(7) .EQ. 0) GOTO 400                                        POU 2100
      IF(IND .EQ. 1 .OR. IND .EQ. 8) GOTO 150                           POU 2110
      GOTO 400                                                          POU 2120
  150 CONTINUE                                                          POU 2130
      IF(NGC(7) .EQ. 1 .AND. IND .EQ. 8) GOTO 400                       POU 2140
      IO32 = IX(138)                                                    POU 2150
      READ (IO32)                                                       POU 2160
      IJDG14 = IEDG(14) + IXPUT(14)                                     POU 2170
      IF(NGC(7) .GT. 1 .AND. IND .EQ. 8) GOTO 200                       POU 2180
      WRITE (IO32) SPARE(12),SPARE(100),XKEF1                           POU 2190
      WRITE (IO32) (((UTIL(J,I,KB),J=1,JMAX),I=1,IMAX),KB=1,KBMAX)      POU 2200
      END FILE IO32                                                     POU 2210
      REWIND IO32                                                       POU 2220
      GOTO 400                                                          POU 2230
  200 CONTINUE                                                          POU 2240
      IF(IJDG14 .EQ. 0) WRITE (IO32) SPARE(12),SPARE(100),XKEF1         POU 2250
      IF(IJDG14 .EQ. 0) GOTO 300                                        POU 2260
      READ (IO32)                                                       POU 2270
      READ (IO32)                                                       POU 2280
  300 CONTINUE                                                          POU 2290
      WRITE (IO32) (((SOURE(J,I,KB),J=1,JMAX),I=1,IMAX),KB=1,KBMAX)     POU 2300
      END FILE IO32                                                     POU 2310
      REWIND IO32                                                       POU 2320
  400 CONTINUE                                                          POU 2330
      RETURN                                                            POU 2340
C                                                                       POU 2350
 1000 FORMAT (1H0,'POINT DAMAGE RATE (DISPLACEMENT/CC-SEC) FOR NUCLIDE',POU 2360
     1 I5)                                                              POU 2370
 1001 FORMAT (1H1,18A4)                                                 POU 2380
 1002 FORMAT (1H ,18A4)                                                 POU 2390
 1003 FORMAT (1H0)                                                      POU 2400
 1004 FORMAT (1H0,'CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROPOU 2410
     1M LEFT TO RIGHT')                                                 POU 2420
 1005 FORMAT (1H0,'CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROPOU 2430
     1M RIGHT TO LEFT')                                                 POU 2440
 1006 FORMAT (1H0,'CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROPOU 2450
     1M TOP TO BOTTOM')                                                 POU 2460
 1007 FORMAT (1H0,'CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROPOU 2470
     1M BOTTOM TO TOP')                                                 POU 2480
 1008 FORMAT (1H0,'CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROPOU 2490
     1M FRONT TO BACK')                                                 POU 2500
 1009 FORMAT (1H0,'CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROPOU 2510
     1M BACK TO FRONT')                                                 POU 2520
 1010 FORMAT (1H0,'IMPORTANCE MAP FOR UNIT CHANGE (DELTA-K/K DELTA-N) INPOU 2530
     1 NUCLIDE',I3,'  VOLUME INTEGRAL IS',1PE13.5)                      POU 2540
 1011 FORMAT (1H0,'RELATIVE IMPORTANCE MAP(N DELTA-K/K DELTA-N) FOR NUCLPOU 2550
     1IDE',I3)                                                          POU 2560
 1012 FORMAT (1H0,'IMPORTANCE MAP OF 1/V CROSS SECTION')                POU 2570
 1013 FORMAT (1H0,' GROUP',I3,' ADJOINT FLUX')                          POU 2580
 1014 FORMAT (1H0,'POINT NEUTRON ABSORPTION RATE(N/CC-SEC) IN NUCLIDE', POU 2590
     1 I4)                                                              POU 2600
 1015 FORMAT (1H0,' POINT NEUTRON DENSITY (N/CC)')                      POU 2610
 1016 FORMAT (1H0,' GROUP',I3,' FLUX')                                  POU 2620
 1017 FORMAT (1H0,' POINT POWER DISTRIBUTION (WATTS/CC)')               POU 2630
 1018 FORMAT (1H0,I10,10I11)                                            POU 2640
 1019 FORMAT (1H ,I3,1PE12.3,10E11.3)                                   POU 2650
 2003 FORMAT (200E12.5)                                                 POU 2660
 2007 FORMAT (12X,200E12.5)                                             POU 2670
      END                                                               POU 2680
      SUBROUTINE KOUT(P2E,UTILE,IND,SOURE,IVX,JVX,KBVX,KVX,JIVX)        KOU   10
C                                                                       KOU   20
CKOUT --138 ***CITATION*** PRINT 3-D FLUXES/ CF-OUTC                    KOU   30
C                                                                       KOU   40
C     IND = 0---FOWARD FLUX OUTPUT                                      KOU   50
C     IND = 1---POINT POWER OUTPUT                                      KOU   60
C     IND = 2---POINT NEUTRON DENSITY OUTPUT                            KOU   70
C     IND = 3---POINT NEUTRON ABSORPTION RATE IN A NUCLIDE              KOU   80
C     IND = 4---ADJOINT FLUX OUTPUT                                     KOU   90
C     IND = 5---IMPORTANCE MAP OF UNIT CHANGE IN NUCLIDE                KOU  100
C     IND = 6---RELATIVE IMPORTANCE MAP FOR NUCLIDE                     KOU  110
C     IND = 7---IMPORTANCE MAP OF 1/V CROSS SECTION                     KOU  120
C     IND = 8---CUMULATIVE HEAT GENERATION RATE                         KOU  130
C     IND = 9---TEMPERATURE OR POWER COEFFICIENT                        KOU  140
C     IND = 10--DAMAGE RATE                                             KOU  150
C                                                                       KOU  160
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KOU  170
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KOU  180
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KOU  190
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KOU  200
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KOU  210
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KOU  220
     6 IXPUT(9999),XPUT(9999)                                           KOU  230
C                                                                       KOU  240
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   KOU  250
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKKOU  260
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    KOU  270
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  KOU  280
     4 ITMAX,ITIME,BET(211),DEL(211)                                    KOU  290
C                                                                       KOU  300
      DIMENSION P2E(JIVX,KBVX,KVX),UTILE(JVX,IVX,KBVX),                 KOU  310
     1 SOURE(JVX,IVX,KBVX)                                              KOU  320
C                                                                       KOU  330
C                                                                       KOU  340
      IS1 = 0                                                           KOU  350
      IS2 = 0                                                           KOU  360
      IF(IND .EQ. 9) GOTO 101                                           KOU  370
      WRITE (IOUT,1001) (TITL1(I),I=1,18)                               KOU  380
      WRITE (IOUT,1002) (TITL2(I),I=1,18)                               KOU  390
      KLAX = KMAX                                                       KOU  400
      IF(IND .EQ. 0 .OR. IND .EQ. 4) GOTO 102                           KOU  410
  101 CONTINUE                                                          KOU  420
      KLAX = 1                                                          KOU  430
  102 DO 134 K=1,KLAX                                                   KOU  440
        IF(IND .EQ. 9) GOTO 103                                         KOU  450
        WRITE (IOUT,1003)                                               KOU  460
  103   CONTINUE                                                        KOU  470
        IF(IND .NE. 8) GOTO 110                                         KOU  480
        NGO = NGC(21)                                                   KOU  490
        IF(NGO .EQ. 0) NGO = 1                                          KOU  500
        GOTO(104,105,106,107,108,109),NGO                               KOU  510
  104   WRITE (IOUT,1004)                                               KOU  520
        GOTO 119                                                        KOU  530
  105   WRITE (IOUT,1005)                                               KOU  540
        GOTO 119                                                        KOU  550
  106   WRITE (IOUT,1006)                                               KOU  560
        GOTO 119                                                        KOU  570
  107   WRITE (IOUT,1007)                                               KOU  580
        GOTO 119                                                        KOU  590
  108   WRITE (IOUT,1008)                                               KOU  600
        GOTO 119                                                        KOU  610
  109   WRITE (IOUT,1009)                                               KOU  620
        GOTO 119                                                        KOU  630
  110   CONTINUE                                                        KOU  640
        IF(IND .EQ. 9) GOTO 119                                         KOU  650
        IF(IND .EQ. 7) GOTO 112                                         KOU  660
        IF(IND .EQ. 6) GOTO 111                                         KOU  670
        IF(IND .NE. 5) GOTO 113                                         KOU  680
        WRITE (IOUT,1010) IXPUT(34),SPARE(91)                           KOU  690
        GOTO 119                                                        KOU  700
  111   WRITE (IOUT,1011) IXPUT(35)                                     KOU  710
        GOTO 119                                                        KOU  720
  112   WRITE (IOUT,1012)                                               KOU  730
        GOTO 119                                                        KOU  740
  113   CONTINUE                                                        KOU  750
        IF(IND .NE. 4) GOTO 114                                         KOU  760
        WRITE (IOUT,1013) K                                             KOU  770
        GOTO 119                                                        KOU  780
  114   CONTINUE                                                        KOU  790
        NUD = IX(165)                                                   KOU  800
        IF(IND .EQ. 10) WRITE (IOUT,1000) NUD                           KOU  810
        IF(IND .EQ. 10) GOTO 119                                        KOU  820
        IF(IND .LT. 3) GOTO 115                                         KOU  830
        WRITE (IOUT,1014) IX(117)                                       KOU  840
        GOTO 119                                                        KOU  850
  115   CONTINUE                                                        KOU  860
        IF(IND-1) 117,118,116                                           KOU  870
  116   WRITE (IOUT,1015)                                               KOU  880
        GOTO 119                                                        KOU  890
  117   WRITE (IOUT,1016) K                                             KOU  900
        GOTO 119                                                        KOU  910
  118   WRITE (IOUT,1017)                                               KOU  920
  119   CONTINUE                                                        KOU  930
        DO 133 KB=1,KBMAX                                               KOU  940
          WRITE (IOUT,1018) KB                                          KOU  950
          N1J = JMAX                                                    KOU  960
          N2J = 0                                                       KOU  970
  120     N1I = IMAX                                                    KOU  980
          N2I = 0                                                       KOU  990
          IF(N1J-11) 121,121,122                                        KOU 1000
  121     N1L = N2J + 1                                                 KOU 1010
          N2J = JMAX                                                    KOU 1020
          IS1 = 1                                                       KOU 1030
          GOTO 123                                                      KOU 1040
  122     N1L = N2J + 1                                                 KOU 1050
          N2J = N2J + 11                                                KOU 1060
          N1J = N1J - 11                                                KOU 1070
  123     CONTINUE                                                      KOU 1080
          IF(N1I-50) 124,124,125                                        KOU 1090
  124     N2L = N2I + 1                                                 KOU 1100
          N2I = IMAX                                                    KOU 1110
          IS2 = 1                                                       KOU 1120
          GOTO 126                                                      KOU 1130
  125     N2L = N2I + 1                                                 KOU 1140
          N2I = N2I + 50                                                KOU 1150
          N1I = N1I - 50                                                KOU 1160
  126     WRITE (IOUT,1019) (J,J=N1L,N2J)                               KOU 1170
          DO 130 I=N2L,N2I                                              KOU 1180
            N1 = (I-1) * JVX                                            KOU 1190
            IF(IND .EQ. 9) GOTO 129                                     KOU 1200
            IF(IND .EQ. 5 .OR. IND .EQ. 6 .OR. IND .EQ. 7 .OR. IND .EQ. KOU 1210
     1       8) GOTO 129                                                KOU 1220
            IF(IND .EQ. 0 .OR. IND .EQ. 4) GOTO 128                     KOU 1230
            WRITE (IOUT,1020) I,(UTILE(J,I,KB ),J=N1L,N2J)              KOU 1240
            GOTO 130                                                    KOU 1250
  128       WRITE (IOUT,1020) I,(P2E(N1+J,KB,K),J=N1L,N2J)              KOU 1260
            GOTO 130                                                    KOU 1270
  129       CONTINUE                                                    KOU 1280
            WRITE (IOUT,1020) I,(SOURE(J,I,KB),J=N1L,N2J)               KOU 1290
  130     CONTINUE                                                      KOU 1300
          IF(IS2) 123,123,131                                           KOU 1310
  131     IS2 = 0                                                       KOU 1320
          IF(IS1) 120,120,132                                           KOU 1330
  132     IS1 = 0                                                       KOU 1340
  133   CONTINUE                                                        KOU 1350
  134 CONTINUE                                                          KOU 1360
      IF(IND .NE. 0) GOTO 2010                                          KOU 1370
      DO 2001 K=1,KMAX                                                  KOU 1380
        WRITE (53) ((P2E(JI,KB,K),JI=1,JIVX),KB=1,KBMAX)                KOU 1390
 2001 CONTINUE                                                          KOU 1400
 2010 CONTINUE                                                          KOU 1410
      IF(IND .NE. 1) GOTO 2020                                          KOU 1420
      WRITE (54) (((UTILE(J,I,KB),J=1,JMAX),I=1,IMAX),KB=1,KBMAX)       KOU 1430
 2020 CONTINUE                                                          KOU 1440
C                                                                       KOU 1450
C     NOTE SEQUENCING 2000 - 2200 FOLLOWING                             KOU 1460
C                                                                       KOU 1470
      IF(NGC(7) .EQ. 0) GOTO 400                                        KOU 1480
      IF(IND .EQ. 1 .OR. IND .EQ. 8) GOTO 150                           KOU 1490
      GOTO 400                                                          KOU 1500
  150 CONTINUE                                                          KOU 1510
      IF(NGC(7) .EQ. 1 .AND. IND .EQ. 8) GOTO 400                       KOU 1520
      IO32 = IX(138)                                                    KOU 1530
      READ (IO32)                                                       KOU 1540
      IJDG14 = IEDG(14) + IXPUT(14)                                     KOU 1550
      IF(NGC(7) .GT. 1 .AND. IND .EQ. 8) GOTO 200                       KOU 1560
      WRITE (IO32) SPARE(12),SPARE(100),XKEF1                           KOU 1570
      WRITE (IO32) (((UTILE(J,I,KB),J=1,JMAX),I=1,IMAX),KB=1,KBMAX)     KOU 1580
      END FILE IO32                                                     KOU 1590
      REWIND IO32                                                       KOU 1600
      GOTO 400                                                          KOU 1610
  200 CONTINUE                                                          KOU 1620
      IF(IJDG14 .EQ. 0) WRITE (IO32) SPARE(12),SPARE(100),XKEF1         KOU 1630
      IF(IJDG14 .EQ. 0) GOTO 300                                        KOU 1640
      READ (IO32)                                                       KOU 1650
      READ (IO32)                                                       KOU 1660
  300 CONTINUE                                                          KOU 1670
      WRITE (IO32) (((SOURE(J,I,KB),J=1,JMAX),I=1,IMAX),KB=1,KBMAX)     KOU 1680
      END FILE IO32                                                     KOU 1690
      REWIND IO32                                                       KOU 1700
  400 CONTINUE                                                          KOU 1710
      RETURN                                                            KOU 1720
C                                                                       KOU 1730
 1000 FORMAT (1H0,'POINT DAMAGE RATE (DISPLACEMENT/CC-SEC) FOR NUCLIDE',KOU 1740
     1 I5)                                                              KOU 1750
 1001 FORMAT (1H1,18A4)                                                 KOU 1760
 1002 FORMAT (1H ,18A4)                                                 KOU 1770
 1003 FORMAT (1H0)                                                      KOU 1780
 1004 FORMAT (1H0,'CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROKOU 1790
     1M LEFT TO RIGHT')                                                 KOU 1800
 1005 FORMAT (1H0,'CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROKOU 1810
     1M RIGHT TO LEFT')                                                 KOU 1820
 1006 FORMAT (1H0,'CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROKOU 1830
     1M TOP TO BOTTOM')                                                 KOU 1840
 1007 FORMAT (1H0,'CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROKOU 1850
     1M BOTTOM TO TOP')                                                 KOU 1860
 1008 FORMAT (1H0,'CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROKOU 1870
     1M FRONT TO BACK')                                                 KOU 1880
 1009 FORMAT (1H0,'CUMULATIVE HEAT GENERATION RATE(MWT/CM**2) - FLOW FROKOU 1890
     1M BACK TO FRONT')                                                 KOU 1900
 1010 FORMAT (1H0,'IMPORTANCE MAP FOR UNIT CHANGE (DELTA-K/K DELTA-N) INKOU 1910
     1 NUCLIDE',I3,'  VOLUME INTEGRAL IS',1PE13.5)                      KOU 1920
 1011 FORMAT (1H0,'RELATIVE IMPORTANCE MAP(N DELTA-K/K DELTA-N) FOR NUCLKOU 1930
     1IDE',I3)                                                          KOU 1940
 1012 FORMAT (1H0,'IMPORTANCE MAP OF 1/V CROSS SECTION')                KOU 1950
 1013 FORMAT (1H0,' GROUP',I3,' ADJOINT FLUX')                          KOU 1960
 1014 FORMAT (1H0,'POINT NEUTRON ABSORPTION RATE(N/CC-SEC) IN NUCLIDE', KOU 1970
     1 I4)                                                              KOU 1980
 1015 FORMAT (1H0,' POINT NEUTRON DENSITY (N/CC)')                      KOU 1990
 1016 FORMAT (1H0,' GROUP',I3,' FLUX')                                  KOU 2000
 1017 FORMAT (1H0,' POINT POWER DISTRIBUTION (WATTS/CC)')               KOU 2010
 1018 FORMAT (1H0,' PLANE NUMBER',I3)                                   KOU 2020
 1019 FORMAT (1H0,I10,10I11)                                            KOU 2030
 1020 FORMAT (1H ,I3,1PE12.3,10E11.3)                                   KOU 2040
      END                                                               KOU 2050
      SUBROUTINE KNFX(P2E,NRGNE,NCOMP,BBND,IVX,JVX,KBVX,KVX,LVX,JIVX)   KNF   10
C                                                                       KNF   20
CKNFX --094 ***CITATION*** INITIAL FLUX FOR 3-D/ CF- EIGN               KNF   30
C                                                                       KNF   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KNF   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KNF   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KNF   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KNF   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KNF   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KNF  100
     6 IXPUT(9999),XPUT(9999)                                           KNF  110
C                                                                       KNF  120
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   KNF  130
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKKNF  140
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    KNF  150
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  KNF  160
     4 ITMAX,ITIME,BET(211),DEL(211)                                    KNF  170
C                                                                       KNF  180
CFZJ055                                                       25.09.07  KNF  190
C                                                                       KNF  200
      DIMENSION P2E(JIVX,KBVX,KVX),NRGNE(JVX,IVX,KBVX),NCOMP(LVX),      KNF  210
     1 BBND(KVX)                                                        KNF  220
C                                                                       KNF  230
C                                                                       KNF  240
      IF(IX(24) .EQ. 0) REWIND IOFLX                                    KNF  250
      IF(IX(24) .GT. 0) GOTO 101                                        KNF  260
      IF(IX(39) .NE. 0) GOTO 101                                        KNF  270
      IF(IX(22) .NE. 0) GOTO 101                                        KNF  280
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .LT. 2) GOTO 106                   KNF  290
      IF(NGC(2) .NE. 0) GOTO 101                                        KNF  300
      IF(IX(16) .GT. 0) GOTO 106                                        KNF  310
      IF(IX(3) .GT. 1) GOTO 106                                         KNF  320
  101 CONTINUE                                                          KNF  330
      IF(NGC(6) .LE. 1) GOTO 134                                        KNF  340
      WRITE (6,1001)                                                    KNF  350
      REWIND IOFLX                                                      KNF  360
      JIPROD = JMAX * IMAX                                              KNF  370
      DO 131 K=1,KMAX                                                   KNF  380
        READ (IOFLX) ((P2E(N1,KB,K),N1=1,JIPROD),KB=1,KBMAX)            KNF  390
  131 CONTINUE                                                          KNF  400
      READ (IOFLX) XLAMDA                                               KNF  410
      WRITE (6,1002) XLAMDA                                             KNF  420
      GOTO 106                                                          KNF  430
  134 CONTINUE                                                          KNF  440
      XLPL = 1.0E-30                                                    KNF  450
      IF(IX(24) .GT. 0 .AND. PROD .LE. 10.0) XLPL = 1.0                 KNF  460
      DO 105 KB=1,KBMAX                                                 KNF  470
        DO 104 I=1,IMAX                                                 KNF  480
          NN1= (I-1) * JVX                                              KNF  490
          DO 103 J=1,JMAX                                               KNF  500
            N1= NN1 + J                                                 KNF  510
            DO 102 K=1,KMAX                                             KNF  520
              T1 = 1.0E+12                                              KNF  530
              IF(IX(24) .GT. 0) T1 = P2E(N1,KB,1) * XLPL                KNF  540
              P2E(N1,KB,K) = T1                                         KNF  550
  102       CONTINUE                                                    KNF  560
  103     CONTINUE                                                      KNF  570
  104   CONTINUE                                                        KNF  580
  105 CONTINUE                                                          KNF  590
  106 CONTINUE                                                          KNF  600
      DO 110 K=1,KMAX                                                   KNF  610
        DO 109 KB=1,KBMAX                                               KNF  620
          N1 = 0                                                        KNF  630
          DO 108 I=1,IMAX                                               KNF  640
            DO 107 J=1,JMAX                                             KNF  650
              N1= N1 + 1                                                KNF  660
              IF(P2E(N1,KB,K) .EQ. 0.0) GOTO 107                        KNF  670
              T2 = P2E(N1,KB,K)                                         KNF  680
              GOTO 111                                                  KNF  690
  107       CONTINUE                                                    KNF  700
  108     CONTINUE                                                      KNF  710
  109   CONTINUE                                                        KNF  720
  110 CONTINUE                                                          KNF  730
      WRITE (IOUT,1000)                                                 KNF  740
C                                                                       KNF  750
      CALL EXIT                                                         KNF  760
C                                                                       KNF  770
  111 CONTINUE                                                          KNF  780
      DO 115 K=1,KMAX                                                   KNF  790
        DO 114 KB=1,KBMAX                                               KNF  800
          N1 = 0                                                        KNF  810
          DO 113 I=1,IMAX                                               KNF  820
            DO 112 J=1,JMAX                                             KNF  830
              N1 = N1 + 1                                               KNF  840
              IF(P2E(N1,KB,K) .EQ. 0.0) P2E(N1,KB,K) = T2               KNF  850
  112       CONTINUE                                                    KNF  860
  113     CONTINUE                                                      KNF  870
  114   CONTINUE                                                        KNF  880
  115 CONTINUE                                                          KNF  890
      DO 123 KB=1,KBMAX                                                 KNF  900
        DO 122 I=1,IMAX                                                 KNF  910
          NN1 = (I-1) * JVX                                             KNF  920
          DO 121 J=1,JMAX                                               KNF  930
            N1 = NN1 + J                                                KNF  940
            L = NRGNE(J,I,KB)                                           KNF  950
            M = NCOMP(L)                                                KNF  960
            DO 120 K=1,KMAX                                             KNF  970
              IF(M-NUAC(17)) 120,116,120                                KNF  980
  116         IF(XMIS(2)) 117,118,118                                   KNF  990
  117         IF(BBND(K)) 118,120,118                                   KNF 1000
  118         P2E(N1,KB,K) = 0.0                                        KNF 1010
  120       CONTINUE                                                    KNF 1020
  121     CONTINUE                                                      KNF 1030
  122   CONTINUE                                                        KNF 1040
  123 CONTINUE                                                          KNF 1050
      IF(IX(39) .NE. 0) GOTO 124                                        KNF 1060
      IF(IX(22) .NE. 0) GOTO 124                                        KNF 1070
      IF(NGC(2) .GT. 0 .AND. NUAC(2) .EQ. 0) GOTO 130                   KNF 1080
  124 CONTINUE                                                          KNF 1090
      IGOTO = 2                                                         KNF 1100
      IF(IX(24) .GT. 0) IGOTO = 1                                       KNF 1110
      BETTA = XMIS(6)                                                   KNF 1120
      VRGK2 = BETTA                                                     KNF 1130
      SPARE(39) = BETTA                                                 KNF 1140
      IF(IX(31)) 126,126,129                                            KNF 1150
  126 IF(IMAX-1) 127,127,128                                            KNF 1160
  127 BETTA = 1.0                                                       KNF 1170
      GOTO 129                                                          KNF 1180
  128 ARG1 = 3.141593 / FLOAT(IMXP1)                                    KNF 1190
      ARG2 = 3.141593 / FLOAT(KBMXP1)                                   KNF 1200
      ARG3 = 3.141593 / FLOAT(JMXP1)                                    KNF 1210
      XBET = (COS(ARG1)+COS(ARG2)) / (3.0-COS(ARG3))                    KNF 1220
      YBET = AMAX1(XBET,0.95)                                           KNF 1230
      T1 = KMAX                                                         KNF 1240
      T2 = (T1+11.0) / 12.0                                             KNF 1250
      ZBET = YBET**T2                                                   KNF 1260
      BETTA = 2.0 / (1.0+SQRT(1.0-ZBET**2))                             KNF 1270
  129 CONTINUE                                                          KNF 1280
  130 CONTINUE                                                          KNF 1290
      RETURN                                                            KNF 1300
C                                                                       KNF 1310
 1000 FORMAT (1H0,'ERROR STOP NUMBER 11')                               KNF 1320
 1001 FORMAT ('0******* INITIAL FLUX FROM TAPE/DISK IOFLX (=9) *******'/KNF 1330
     1 /)                                                               KNF 1340
 1002 FORMAT ('0XLAMDA',1PE15.5)                                        KNF 1350
      END                                                               KNF 1360
      SUBROUTINE CALR(A,RATE,NTITE,NDX,MDX,IX3738,MVZ,PF,ISIZ)          CAL   10
C                                                                       CAL   20
CCALR --005 ***CITATION*** ENTIRE PROBLEM CONTROL/ CF-INPT              CAL   30
C                                                                       CAL   40
      REAL*8 FLOTR                                                      CAL   50
C                                                                       CAL   60
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,CAL   70
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   CAL   80
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), CAL   90
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    CAL  100
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    CAL  110
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   CAL  120
     6 IXPUT(9999),XPUT(9999)                                           CAL  130
C                                                                       CAL  140
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  CAL  150
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), CAL  160
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)CAL  170
     3 ,PDK(211)                                                        CAL  180
C                                                                       CAL  190
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   CAL  200
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKCAL  210
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    CAL  220
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  CAL  230
     4 ITMAX,ITIME,BET(211),DEL(211)                                    CAL  240
C                                                                       CAL  250
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           CAL  260
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    CAL  270
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),CAL  280
     3 NCLASS(9999),NDP(9999)                                           CAL  290
C                                                                       CAL  300
      COMMON /AKADD/ KAY(1),K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13, CAL  310
     1 K131,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,    CAL  320
     2 K28,K29,K30,K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,K41,K42,K43, CAL  330
     3 K44,K45,K46,K47,K48,K49,K50,K51,K52,K53,K54,K55,K56,K57,K58,     CAL  340
     4 K59,K60,K61,K62,K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,     CAL  350
     5 K74,K75,K76,K77,K78,K79,K80,K81,K82,K83,K84,K85,K86,K87,K88,     CAL  360
     6 K89,K90,K91,K92,K93,K94,K95,K96,K97,K98,K99,K100,NDATA,KNRGN,    CAL  370
     7 KNCOMP,KPVOL,KRVOL,MEMVRY,MEMX                                   CAL  380
C                                                                       CAL  390
      COMMON /AVDLM/ IVDLM(1),IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1, CAL  400
     1 KBVXP1,NSETVX,NVO,IVO,IVZ,KVZ,NCRP,NSPA,N3DDIM,NBLOCK,JIVX,JIP1VXCAL  410
     2 ,JP1IXZ,IOVX,IOVZ                                                CAL  420
C                                                                       CAL  430
      COMMON /CMARY/ MEMARY,IMN,MNI,IJLMN,NMLJI,IY(50),BX(50),TITL(36)  CAL  440
C                                                                       CAL  450
CFZJ056                                                       06.11.07  CAL  460
      COMMON /COOPD/ FLOTR(9999),INTGR(200)                             CAL  470
C                                                                       CAL  480
      COMMON /AMISS/ ITMIS(20),AITMIS(20)                               CAL  490
C                                                                       CAL  500
CFZJ055                                                       25.09.07  CAL  510
C                                                                       CAL  520
      DIMENSION A(1),RATE(NDX,MDX,10),PF(ISIZ)                          CAL  530
C                                                                       CAL  540
C                                                                       CAL  550
      ENDE = 0.                                                         CAL  560
      MEMORY = MEMVRY                                                   CAL  570
      IX(172) = 0                                                       CAL  580
      IX172 = 0                                                         CAL  590
      IO19 = IX(86)                                                     CAL  600
      KK1 = K19 + NVX * MVX * 10                                        CAL  610
      KK2 = K61 + NVO                                                   CAL  620
      KL1 = KK1 - K23                                                   CAL  630
      KL2 = KK2 - K23                                                   CAL  640
      KL3 = MAX0(KL1,KL2)                                               CAL  650
      KL4 = MAX0(KK1,KK2)                                               CAL  660
      DO 100 N=1,20                                                     CAL  670
        ITMIS(N) = 0                                                    CAL  680
        AITMIS(N) = 0.0                                                 CAL  690
  100 CONTINUE                                                          CAL  700
C                                                                       CAL  710
C     IF(NTITE .EQ. 1)  RESTART OR CONTINUE SOME CYCLE.                 CAL  720
C     IF(NTITE .EQ. 0)  NO RESTART OR RESTART STATICS PROBLEM.          CAL  730
C     IF(NTITE .EQ. -1) RESTART DEPLETION ONLY OR CONT. FIRST CYCLE.    CAL  740
C                                                                       CAL  750
      IO13 = IX(80)                                                     CAL  760
      NCCYL = 999                                                       CAL  770
C                                                                       CAL  780
      IF(NGC(3) .GT. 0 .AND. NTITE .EQ. 0) CALL WSTR(NCCYL,A,MEMORY)    CAL  790
C                                                                       CAL  800
      NRCYC = 1                                                         CAL  810
      NCYCLE = 1                                                        CAL  820
      MAC4 = 1                                                          CAL  830
      IF(NDPL(1) .NE. 0) NCYCLE = NDPL(1)                               CAL  840
      INNOO = 1                                                         CAL  850
      IF(INNO(91) .EQ. 0 .OR. INNO(93) .EQ. 0) INNOO = 0                CAL  860
      IF(INNOO .EQ. 0) GOTO 102                                         CAL  870
C                                                                       CAL  880
C     WRITE FIXED FUEL MANAGEMENT INTERFACE DATA                        CAL  890
C                                                                       CAL  900
      IF(NGC(1) .LE. 0) GOTO 102                                        CAL  910
      IX(171) = INTGR(29)                                               CAL  920
      IF(IX(171) .LE. 0) GOTO 101                                       CAL  930
      IO33 = IX(139)                                                    CAL  940
      IO34 = IX(140)                                                    CAL  950
      IOOR = IO33                                                       CAL  960
      IOOW = IO34                                                       CAL  970
      REWIND IOOR                                                       CAL  980
      REWIND IOOW                                                       CAL  990
  101 CONTINUE                                                          CAL 1000
      ITMIS(1) = 0                                                      CAL 1010
      MAC2 = INTGR(22)                                                  CAL 1020
      IF(MAC2 .GT. 0) NCYCLE = MAC2                                     CAL 1030
      IF(INTGR(24) .GT. 0) MAC4 = INTGR(24)                             CAL 1040
      IF(INTGR(35) .LE. 0) GOTO 102                                     CAL 1050
      NRCYC = INTGR(35) + 1                                             CAL 1060
  102 CONTINUE                                                          CAL 1070
      I11 = ICLOCK(0)                                                   CAL 1080
      NCY = 1                                                           CAL 1090
      IQUE = 1                                                          CAL 1100
      NRC = 1                                                           CAL 1110
      NNST = NGC(2)                                                     CAL 1120
      IF(NGC(2) .EQ. 0) GOTO 103                                        CAL 1130
      INTGR(51) = 0                                                     CAL 1140
      NCY = IABS(IX(19))                                                CAL 1150
      IF(INNOO .GT. 0) IQUE = INTGR(19)                                 CAL 1160
      IF(INNOO .GT. 0) NRC = INTGR(46)                                  CAL 1170
      IF(NTITE .LE. 0) GOTO 103                                         CAL 1180
      IF(NGC(2) .LT. 0) GOTO 103                                        CAL 1190
      GOTO 161                                                          CAL 1200
  103 CONTINUE                                                          CAL 1210
C                                                                       CAL 1220
C     START EQUILIBRIUM LOOP                                            CAL 1230
C     DO 174 IQUE = 1,MAC4                                              CAL 1240
C                                                                       CAL 1250
  104 CONTINUE                                                          CAL 1260
C                                                                       CAL 1270
C     START DEPLETION CYCLE LOOP                                        CAL 1280
C     DO 168 NCY = 1,NCYCLE                                             CAL 1290
C                                                                       CAL 1300
  105 CONTINUE                                                          CAL 1310
      IF(NDPL(12) .EQ. 0) GOTO 106                                      CAL 1320
      IF(NCY .EQ. 1) GOTO 106                                           CAL 1330
      NRSET1 = 1                                                        CAL 1340
      NRSET2 = 0                                                        CAL 1350
  106 CONTINUE                                                          CAL 1360
      ICUP1 = ICLOCK(0)                                                 CAL 1370
C                                                                       CAL 1380
      CALL WATCH(ENDE)                                                  CAL 1390
C                                                                       CAL 1400
      IRAL1 = IFIX(ENDE)                                                CAL 1410
      IX(16) = 0                                                        CAL 1420
      IX(99) = 0                                                        CAL 1430
      LSTEP = 0                                                         CAL 1440
      IX(3) = NCY                                                       CAL 1450
      I21 = ICLOCK(0)                                                   CAL 1460
      U1 = SPARE(1)                                                     CAL 1470
      U2 = SPARE(2)                                                     CAL 1480
      U9 = SPARE(9)                                                     CAL 1490
      U10 = SPARE(10)                                                   CAL 1500
      U11 = SPARE(11)                                                   CAL 1510
      U12 = SPARE(12)                                                   CAL 1520
      U13 = SPARE(13)                                                   CAL 1530
      U26 = SPARE(26)                                                   CAL 1540
      U74 = SPARE(74)                                                   CAL 1550
      U75 = SPARE(75)                                                   CAL 1560
C                                                                       CAL 1570
C     START REPEAT CYCLE LOOP                                           CAL 1580
C     DO 164 NRC = 1,NRCYC                                              CAL 1590
C                                                                       CAL 1600
  107 CONTINUE                                                          CAL 1610
      IF(NDPL(12) .EQ. 0) GOTO 108                                      CAL 1620
      IF(NRCYC .EQ. 1) GOTO 108                                         CAL 1630
      IF(NRC .EQ. 1) GOTO 108                                           CAL 1640
      NRSET1 = 2                                                        CAL 1650
      NRSET2 = 0                                                        CAL 1660
  108 CONTINUE                                                          CAL 1670
      FLOTR(29) = 10000.0                                               CAL 1680
      FLOTR(30) = 10000.0                                               CAL 1690
      IF(NGC(1) .EQ. 0) GOTO 109                                        CAL 1700
      IF(NGC(2) .EQ. 0) GOTO 109                                        CAL 1710
      IF(NTITE .LT. 0) GOTO 123                                         CAL 1720
      IF(NGC(2) .LT. 0 .AND. NTITE .EQ. 1) GOTO 123                     CAL 1730
  109 CONTINUE                                                          CAL 1740
      IX(14) = 0                                                        CAL 1750
      IX(8) = NRC                                                       CAL 1760
      IND1 = 0                                                          CAL 1770
C                                                                       CAL 1780
      CALL HOWE(NCY,IND1,IND2,IHW,ITX,ITM)                              CAL 1790
C                                                                       CAL 1800
      N1C1S = 0                                                         CAL 1810
      IF(IND2 .EQ. 0) GOTO 123                                          CAL 1820
      N1C1S = 1                                                         CAL 1830
      IX(5) = IHW                                                       CAL 1840
      ITMAX = ITX                                                       CAL 1850
      ITIME = ITM                                                       CAL 1860
      IX(75) = 0                                                        CAL 1870
      IX(73) = 0                                                        CAL 1880
      IGO = 1                                                           CAL 1890
      IX(71) = 0                                                        CAL 1900
      NDO = 1                                                           CAL 1910
      NDOUBL = 1                                                        CAL 1920
      IF(IX(5) .GT. 1) NDOUBL = NSRH(3)                                 CAL 1930
C                                                                       CAL 1940
C     START INITIAL REACTIVITY LOOP                                     CAL 1950
C     DO 117 NDO = 1,NDOUBL                                             CAL 1960
C                                                                       CAL 1970
      IX(127) = ICLOCK(0)                                               CAL 1980
  110 CONTINUE                                                          CAL 1990
      IX(73) = IX(73) + 1                                               CAL 2000
  111 CONTINUE                                                          CAL 2010
C                                                                       CAL 2020
      CALL EIGN(A,A(K24),A(K24),NTITE,IVX,JVX,KVX,JIVX,KBVX,IX3738)     CAL 2030
C                                                                       CAL 2040
      IX(198) = 1                                                       CAL 2050
      NTITE = 0                                                         CAL 2060
      IX(16) = 1                                                        CAL 2070
      GOTO(112,119),IGO                                                 CAL 2080
  112 CONTINUE                                                          CAL 2090
      IDJ = 0                                                           CAL 2100
      IF(IX(75) .EQ. 1) GOTO 117                                        CAL 2110
      IF(IX(70) .EQ. 0) GOTO 116                                        CAL 2120
      IF(IX(70) .LT. 0) GOTO 113                                        CAL 2130
      WRITE (IOUT,1005)                                                 CAL 2140
      GOTO 114                                                          CAL 2150
  113 WRITE (IOUT,1012)                                                 CAL 2160
  114 CONTINUE                                                          CAL 2170
      IF(NGC(15) .EQ. 0) GOTO 117                                       CAL 2180
      IF(NGC(15) .EQ. 2) GOTO 115                                       CAL 2190
      IF(IX(126) .EQ. 0) GOTO 117                                       CAL 2200
  115 WRITE (IOUT,1006)                                                 CAL 2210
C                                                                       CAL 2220
      CALL EXIT                                                         CAL 2230
C                                                                       CAL 2240
  116 CONTINUE                                                          CAL 2250
      NDO = NDO + 1                                                     CAL 2260
      IF(NDO .LE. NDOUBL) GOTO 110                                      CAL 2270
  117 CONTINUE                                                          CAL 2280
      T1 = 1.0 / SPARE(18)                                              CAL 2290
      IF(INTGR(35) .GT. 0 .AND. INTGR(36) .EQ. 3) GOTO 118              CAL 2300
      IF(T1 .GT. GLIM(1) .OR. T1 .LT. GLIM(2)) GOTO 178                 CAL 2310
      IF(IX(5) .LE. 0) GOTO 118                                         CAL 2320
      IF(SPARE(58) .GT. GLIM(3)) GOTO 178                               CAL 2330
  118 CONTINUE                                                          CAL 2340
      IF(IX(23) .EQ. 0) GOTO 119                                        CAL 2350
      IX(24) = 1                                                        CAL 2360
      IDJ = 1                                                           CAL 2370
      IF(IX(37) .GT. 0) IX(71) = 1                                      CAL 2380
      IGO = 2                                                           CAL 2390
      GOTO 111                                                          CAL 2400
  119 CONTINUE                                                          CAL 2410
      IF(KL1 .LE. 0 .OR. IDJ .GT. 0) GOTO 120                           CAL 2420
      REWIND IO19                                                       CAL 2430
      WRITE (IO19) (A(I),I=K23,KK1)                                     CAL 2440
      END FILE IO19                                                     CAL 2450
      REWIND IO19                                                       CAL 2460
  120 CONTINUE                                                          CAL 2470
      IX(71) = 0                                                        CAL 2480
      INDTOR = 0                                                        CAL 2490
C                                                                       CAL 2500
      CALL OUTC(A,A(K17),A(K40),A(K38),A(K14),A(K19),A(K52),A(K16),     CAL 2510
     1 A(K48),A(K23),NVX,MVX,KVX,IX3738,INDTOR,MVZ,A(K39))              CAL 2520
C                                                                       CAL 2530
      IF(KL1 .LE. 0 .OR. IDJ .GT. 0) GOTO 121                           CAL 2540
      READ (IO19) (A(I),I=K23,KK1)                                      CAL 2550
      REWIND IO19                                                       CAL 2560
  121 CONTINUE                                                          CAL 2570
      NCCYL = 0                                                         CAL 2580
C                                                                       CAL 2590
      IF(NGC(1) .EQ. 0 .AND. NGC(3) .GT. 0) CALL WSTR(NCCYL,A,MEMORY)   CAL 2600
C                                                                       CAL 2610
C     STATICS BYPASS                                                    CAL 2620
C                                                                       CAL 2630
      IF(NGC(1) .EQ. 0) GOTO 177                                        CAL 2640
      IF(NCY .GT. 1) GOTO 122                                           CAL 2650
C                                                                       CAL 2660
      IF(NGC(3) .GT. 0 .AND. NGC(2) .EQ. 0) CALL WSTR(NCCYL,A,MEMORY)   CAL 2670
C                                                                       CAL 2680
  122 CONTINUE                                                          CAL 2690
      NGC(2) = 0                                                        CAL 2700
  123 CONTINUE                                                          CAL 2710
      IF(INNOO .EQ. 0) GOTO 125                                         CAL 2720
C                                                                       CAL 2730
C     WRITE START OF CYCLE CONCENTRATIONS ON IO24                       CAL 2740
C                                                                       CAL 2750
      IO24 = IX(91)                                                     CAL 2760
      IO12 = IX(79)                                                     CAL 2770
      REWIND IO24                                                       CAL 2780
      IXND = 0                                                          CAL 2790
      REWIND IO12                                                       CAL 2800
  125 CONTINUE                                                          CAL 2810
      IF(NCY-2) 126,127,128                                             CAL 2820
  126 NSTEP = NDPL(2)                                                   CAL 2830
      GOTO 129                                                          CAL 2840
  127 NSTEP = NDPL(3)                                                   CAL 2850
      GOTO 129                                                          CAL 2860
  128 NSTEP = NDPL(4)                                                   CAL 2870
  129 CONTINUE                                                          CAL 2880
      NSTEP = MAX0(1,NSTEP)                                             CAL 2890
C                                                                       CAL 2900
C     START DEPLETION TIME STEP LOOP                                    CAL 2910
C     DO 157 NST = 1,NSTEP                                              CAL 2920
C                                                                       CAL 2930
      NST = 1                                                           CAL 2940
      IF(NNST .LT. 0) NST = IX(2)                                       CAL 2950
      NNST = 0                                                          CAL 2960
  130 CONTINUE                                                          CAL 2970
      IF(NDPL(12) .EQ. 0) GOTO 131                                      CAL 2980
      IF(LSTEP .GT. 0) GOTO 131                                         CAL 2990
      NRSET1 = 3                                                        CAL 3000
      NRSET2 = NST                                                      CAL 3010
  131 CONTINUE                                                          CAL 3020
      IF(IX(172) .EQ. 0) GOTO 133                                       CAL 3030
      READ (IOOR,END=132) PF,XLAMDA,BETTA,SPARE(45),SPARE(46)           CAL 3040
      GOTO 133                                                          CAL 3050
  132 CONTINUE                                                          CAL 3060
      IX(172) = 0                                                       CAL 3070
  133 CONTINUE                                                          CAL 3080
      ICPUT1 = ICLOCK(0)                                                CAL 3090
C                                                                       CAL 3100
      CALL WATCH(ENDE)                                                  CAL 3110
C                                                                       CAL 3120
      IRELT1 = IFIX(ENDE)                                               CAL 3130
      IX(2) = NST                                                       CAL 3140
C                                                                       CAL 3150
      IF(NDPL(11) .GT. 0) CALL RODX(A(K17),A(K10),A(KNCOMP),A(K24),     CAL 3160
     1 A(K24),A(KNRGN),A(K50),NVX,MVX,NSETVX,LVX,JVX,IVX,KBVX,JIVX,KVX) CAL 3170
C                                                                       CAL 3180
C     NEED SOMETHING TO TELL IF NRTSP = ITMX(4)                         CAL 3190
C                                                                       CAL 3200
      NRTSP = 1                                                         CAL 3210
C                                                                       CAL 3220
C     START REPEAT TIME STEP LOOP                                       CAL 3230
C                                                                       CAL 3240
      I31 = ICLOCK(0)                                                   CAL 3250
      DO 146 NRT=1,NRTSP                                                CAL 3260
        IX(9) = NRT                                                     CAL 3270
        IND1 = 1                                                        CAL 3280
C                                                                       CAL 3290
        CALL HOWE(NCY,IND1,IND2,IHW,ITX,ITM)                            CAL 3300
C                                                                       CAL 3310
        IX(5) = IHW                                                     CAL 3320
        ITMAX = ITX                                                     CAL 3330
        ITIME = ITM                                                     CAL 3340
        IX(75) = 0                                                      CAL 3350
        IX(73) = 0                                                      CAL 3360
        IGO = 1                                                         CAL 3370
        IX(71) = 0                                                      CAL 3380
        NDO = 1                                                         CAL 3390
        NDOUBL = 1                                                      CAL 3400
        IF(IX(5) .GT. 1) NDOUBL = NSRH(3)                               CAL 3410
C                                                                       CAL 3420
C     START CYCLE REACTIVITY LOOP                                       CAL 3430
C       DO 141 NDO = 1,NDOUBL                                           CAL 3440
C                                                                       CAL 3450
        IX(127) = ICLOCK(0)                                             CAL 3460
  134   CONTINUE                                                        CAL 3470
        IX(73) = IX(73) + 1                                             CAL 3480
  135   CONTINUE                                                        CAL 3490
C                                                                       CAL 3500
        CALL EIGN(A,A(K24),A(K24),NTITE,IVX,JVX,KVX,JIVX,KBVX,IX3738)   CAL 3510
C                                                                       CAL 3520
        IX(198) = 1                                                     CAL 3530
        NTITE = 0                                                       CAL 3540
        NGC(2) = 0                                                      CAL 3550
        IX(16) = 0                                                      CAL 3560
        GOTO(136,143),IGO                                               CAL 3570
  136   CONTINUE                                                        CAL 3580
        TF1 = FLOTR(29)                                                 CAL 3590
        TF2 = FLOTR(30)                                                 CAL 3600
        TM1 = AMIN1(XKEF1,TF1)                                          CAL 3610
        TM2 = AMIN1(SPARE(52),TF2)                                      CAL 3620
        FLOTR(29) = TM1                                                 CAL 3630
        FLOTR(30) = TM2                                                 CAL 3640
        IF(IX(75) .EQ. 1) GOTO 141                                      CAL 3650
        IF(IX(70) .EQ. 0) GOTO 140                                      CAL 3660
        IF(IX(70) .LT. 0) GOTO 137                                      CAL 3670
        WRITE (IOUT,1007)                                               CAL 3680
        GOTO 138                                                        CAL 3690
  137   WRITE (IOUT,1013)                                               CAL 3700
  138   CONTINUE                                                        CAL 3710
        IF(NGC(15) .EQ. 0) GOTO 141                                     CAL 3720
        IF(NGC(15) .EQ. 2) GOTO 139                                     CAL 3730
        IF(IX(126) .EQ. 0) GOTO 141                                     CAL 3740
  139   WRITE (IOUT,1008)                                               CAL 3750
C                                                                       CAL 3760
        CALL EXIT                                                       CAL 3770
C                                                                       CAL 3780
  140   CONTINUE                                                        CAL 3790
        NDO = NDO + 1                                                   CAL 3800
        IF(NDO .LE. NDOUBL) GOTO 134                                    CAL 3810
  141   CONTINUE                                                        CAL 3820
        T1 = 1.0 / SPARE(18)                                            CAL 3830
        IF(INTGR(35) .GT. 0 .AND. INTGR(36) .EQ. 3) GOTO 142            CAL 3840
        IF(T1 .GT. GLIM(1) .OR. T1 .LT. GLIM(2)) GOTO 178               CAL 3850
        IF(IX(5) .LE. 0) GOTO 142                                       CAL 3860
  142   CONTINUE                                                        CAL 3870
        IF(LSTEP .NE. 1) GOTO 143                                       CAL 3880
        IF(NDPL(5) .GE. 0) GOTO 143                                     CAL 3890
        IX(24) = 1                                                      CAL 3900
        IF(IX(37) .GT. 0) IX(71) = 1                                    CAL 3910
        IGO = 2                                                         CAL 3920
        GOTO 135                                                        CAL 3930
  143   CONTINUE                                                        CAL 3940
        IF(KL3 .LE. 0) GOTO 144                                         CAL 3950
        REWIND IO19                                                     CAL 3960
        WRITE (IO19) (A(I),I=K23,KL4)                                   CAL 3970
        END FILE IO19                                                   CAL 3980
        REWIND IO19                                                     CAL 3990
  144   CONTINUE                                                        CAL 4000
        INDTOR = 1                                                      CAL 4010
        IF(LSTEP .EQ. 1) INDTOR = 0                                     CAL 4020
C                                                                       CAL 4030
        CALL OUTC(A,A(K17),A(K40),A(K38),A(K14),A(K19),A(K52),A(K16),   CAL 4040
     1   A(K48),A(K23),NVX,MVX,KVX,IX3738,INDTOR,MVZ,A(K39))            CAL 4050
C                                                                       CAL 4060
        IF(LSTEP .EQ. 1) GOTO 149                                       CAL 4070
        SPARE(14) = XKEF1                                               CAL 4080
        IX(4) = 0                                                       CAL 4090
        IF(IX(15) .GT. 0) IX(4) = 1                                     CAL 4100
        IF(IX(4) .GT. 0) GOTO 147                                       CAL 4110
        I32 = ICLOCK(0)                                                 CAL 4120
        II3 = (I32-I31) / 60                                            CAL 4130
        IF(II3 .LT. ITMX(22)) GOTO 146                                  CAL 4140
        WRITE (IOUT,1014)                                               CAL 4150
        IF(NGC(15) .EQ. 2) GOTO 179                                     CAL 4160
        GOTO 147                                                        CAL 4170
  146 CONTINUE                                                          CAL 4180
  147 CONTINUE                                                          CAL 4190
      IX(41) = 0                                                        CAL 4200
      IF(NST .EQ. NSTEP .OR. IX(4) .EQ. 1) IX(41) = 1                   CAL 4210
C                                                                       CAL 4220
      CALL RQED(IX(102),IND)                                            CAL 4230
C                                                                       CAL 4240
      IX(14) = IX(14) + 1                                               CAL 4250
  149 CONTINUE                                                          CAL 4260
      IF(LSTEP .EQ. 1 .AND. NDPL(5) .LT. 1) GOTO 150                    CAL 4270
      IF(KL3 .LE. 0) GOTO 150                                           CAL 4280
      READ (IO19) (A(I),I=K23,KL4)                                      CAL 4290
      REWIND IO19                                                       CAL 4300
  150 CONTINUE                                                          CAL 4310
      IF(LSTEP .EQ. 1) GOTO 151                                         CAL 4320
      IF(NCY .GT. 1 .OR. NST .GT. 1 .OR. IQUE .GT. 1) GOTO 151          CAL 4330
      IF(N1C1S .GT. 0) GOTO 151                                         CAL 4340
      NCCYL = 0                                                         CAL 4350
C                                                                       CAL 4360
      IF(NGC(3) .GT. 0) CALL WSTR(NCCYL,A,MEMORY)                       CAL 4370
C                                                                       CAL 4380
  151 CONTINUE                                                          CAL 4390
      IX(2) = IX(2) + 1                                                 CAL 4400
      IF(IX(171) .LE. 0) GOTO 154                                       CAL 4410
      IF(IX172 .GT. 0) GOTO 153                                         CAL 4420
      IF(MAC4 .EQ. 1) GOTO 152                                          CAL 4430
      IF(IQUE .LT. IX(171)) GOTO 154                                    CAL 4440
      GOTO 153                                                          CAL 4450
  152 CONTINUE                                                          CAL 4460
      IF(NCY .LT. IX(171)) GOTO 154                                     CAL 4470
  153 CONTINUE                                                          CAL 4480
      WRITE (IOOW) PF,XLAMDA,BETTA,SPARE(45),SPARE(46)                  CAL 4490
      IX172 = 1                                                         CAL 4500
  154 CONTINUE                                                          CAL 4510
      NCCYL = -NCY                                                      CAL 4520
      IF(NGC(3) .EQ. 0) GOTO 155                                        CAL 4530
C                                                                       CAL 4540
      CALL WSTR(NCCYL,A,MEMORY)                                         CAL 4550
C                                                                       CAL 4560
      BACKSPACE IO13                                                    CAL 4570
      BACKSPACE IO13                                                    CAL 4580
  155 CONTINUE                                                          CAL 4590
      IF(LSTEP .EQ. 1) GOTO 159                                         CAL 4600
      IF(IX(4) .GT. 0) GOTO 157                                         CAL 4610
      IF(SPARE(2) .LT. DPLH(1)) GOTO 156                                CAL 4620
      WRITE (IOUT,1011) NCY                                             CAL 4630
      GOTO 157                                                          CAL 4640
  156 CONTINUE                                                          CAL 4650
      NST = NST + 1                                                     CAL 4660
      IX2 = IX(2) - 1                                                   CAL 4670
      ICPUT2 = ICLOCK(0)                                                CAL 4680
C                                                                       CAL 4690
      CALL WATCH(ENDE)                                                  CAL 4700
C                                                                       CAL 4710
      IRELT2 = IFIX(ENDE)                                               CAL 4720
      CPUT = (FLOAT(ICPUT2)-FLOAT(ICPUT1)) / 60.                        CAL 4730
      RELT = (FLOAT(IRELT2)-FLOAT(IRELT1)) / 60.                        CAL 4740
      WRITE (IOUT,1009) IX2,CPUT,RELT                                   CAL 4750
      IF(NST .LE. NSTEP) GOTO 130                                       CAL 4760
  157 CONTINUE                                                          CAL 4770
      IF(NGC(3) .EQ. 0) GOTO 158                                        CAL 4780
      IF(INNOO .GT. 0) GOTO 158                                         CAL 4790
      READ(IO13)                                                        CAL 4800
      READ(IO13)                                                        CAL 4810
  158 CONTINUE                                                          CAL 4820
      IX(16) = 0                                                        CAL 4830
      IX(4) = 0                                                         CAL 4840
      IF(NDPL(5) .EQ. 0) GOTO 159                                       CAL 4850
      IX(99) = 1                                                        CAL 4860
      LSTEP = 1                                                         CAL 4870
      GOTO 130                                                          CAL 4880
  159 CONTINUE                                                          CAL 4890
      IF(IX172 .LE. 0) GOTO 160                                         CAL 4900
      END FILE IOOW                                                     CAL 4910
      REWIND IOOW                                                       CAL 4920
      REWIND IOOR                                                       CAL 4930
      IOT1 = IOOR                                                       CAL 4940
      IOOR = IOOW                                                       CAL 4950
      IOOW = IOT1                                                       CAL 4960
      IX(172) = 1                                                       CAL 4970
  160 CONTINUE                                                          CAL 4980
      IX(99) = 0                                                        CAL 4990
      LSTEP = 0                                                         CAL 5000
      IF(INNOO .EQ. 0) GOTO 165                                         CAL 5010
      ITMIS(1) = ITMIS(1) + 1                                           CAL 5020
C                                                                       CAL 5030
C     WRITE END OF CYCLE CONCENTRATIONS ON IO24                         CAL 5040
C                                                                       CAL 5050
      IO24 = IX(91)                                                     CAL 5060
      IO12 = IX(79)                                                     CAL 5070
      IXND = 1                                                          CAL 5080
      REWIND IO12                                                       CAL 5090
      END FILE IO24                                                     CAL 5100
      REWIND IO24                                                       CAL 5110
  161 CONTINUE                                                          CAL 5120
      REWIND IO1                                                        CAL 5130
      WRITE (IO1) (A(I),I=1,MEMORY)                                     CAL 5140
      END FILE IO1                                                      CAL 5150
      REWIND IO1                                                        CAL 5160
      INTGR(11) = 0                                                     CAL 5170
      IO22 = IX(89)                                                     CAL 5180
      INTGR(12) = IO22                                                  CAL 5190
      INTGR(71) = NCY                                                   CAL 5200
      INTGR(13) = IO13                                                  CAL 5210
      INTGR(14) = NGC(2)                                                CAL 5220
      INTGR(15) = NGC(3)                                                CAL 5230
      INTGR(77) = 0                                                     CAL 5240
      INTGR(19) = IQUE                                                  CAL 5250
      INTGR(20) = IOUT                                                  CAL 5260
      INTGR(46) = NRC                                                   CAL 5270
      INTGR(47) = 0                                                     CAL 5280
      INTGR(49) = 0                                                     CAL 5290
      IF(INTGR(35) .EQ. 0) GOTO 162                                     CAL 5300
      IF(NGC(2) .NE. 0) GOTO 162                                        CAL 5310
      FLOTR(28) = FLOTR(21)                                             CAL 5320
      IF(INTGR(36) .EQ. 1) FLOTR(21) = XKEF1                            CAL 5330
      IF(INTGR(36) .EQ. 2) FLOTR(21) = SPARE(52)                        CAL 5340
      IF(INTGR(36) .EQ. 3) FLOTR(21) = SPARE(2)                         CAL 5350
      IF(INTGR(36) .EQ. 4) FLOTR(21) = FLOTR(29)                        CAL 5360
      IF(INTGR(36) .EQ. 5) FLOTR(21) = FLOTR(30)                        CAL 5370
  162 CONTINUE                                                          CAL 5380
      IF(NCY .EQ. NCYCLE .AND. NGC(2) .EQ. 0) INTGR(77) = 1             CAL 5390
      READ (IO1) (A(I),I=1,MEMORY)                                      CAL 5400
      REWIND IO1                                                        CAL 5410
      IO12 = IX(79)                                                     CAL 5420
      I22 = ICLOCK(0)                                                   CAL 5430
      II2 = (I22-I21) / 60                                              CAL 5440
      IF(II2 .LT. ITMX(23)) GOTO 163                                    CAL 5450
      WRITE (IOUT,1015)                                                 CAL 5460
      IF(NGC(15) .EQ. 2) GOTO 179                                       CAL 5470
      GOTO 164                                                          CAL 5480
  163 CONTINUE                                                          CAL 5490
      NRC = NRC + 1                                                     CAL 5500
      IF(INTGR(47) .LE. 0) GOTO 164                                     CAL 5510
      IF(NRC .GT. NRCYC) GOTO 164                                       CAL 5520
      IX(2) = 0                                                         CAL 5530
      IX(198) = 0                                                       CAL 5540
      SPARE(1 ) = U1                                                    CAL 5550
      SPARE(2 ) = U2                                                    CAL 5560
      SPARE(9 ) = U9                                                    CAL 5570
      SPARE(10) = U10                                                   CAL 5580
      SPARE(11) = U11                                                   CAL 5590
      SPARE(12) = U12                                                   CAL 5600
      SPARE(13) = U13                                                   CAL 5610
      SPARE(26) = U26                                                   CAL 5620
      SPARE(74) = U74                                                   CAL 5630
      SPARE(75) = U75                                                   CAL 5640
      GOTO 107                                                          CAL 5650
  164 CONTINUE                                                          CAL 5660
      NRC = 1                                                           CAL 5670
C                                                                       CAL 5680
      CALL CYCR                                                         CAL 5690
C                                                                       CAL 5700
      IF(NGC(3) .EQ. 0) GOTO 165                                        CAL 5710
      WRITE (IO13) (IX(I),I=1,200),(SPARE(I),I=1,200)                   CAL 5720
  165 CONTINUE                                                          CAL 5730
      IF(INTGR(35) .LE. 0) GOTO 166                                     CAL 5740
      IF(INTGR(49) .GT. 0) GOTO 166                                     CAL 5750
      WRITE (IOUT,1000)                                                 CAL 5760
      IF(INTGR(41) .GT. 0) GOTO 175                                     CAL 5770
  166 CONTINUE                                                          CAL 5780
      IX(2) = 0                                                         CAL 5790
      ICUP2 = ICLOCK(0)                                                 CAL 5800
C                                                                       CAL 5810
      CALL WATCH(ENDE)                                                  CAL 5820
C                                                                       CAL 5830
      IRAL2 = IFIX(ENDE)                                                CAL 5840
      CUP = (FLOAT(ICUP2)-FLOAT(ICUP1)) / 60.                           CAL 5850
      RAL = (FLOAT(IRAL2)-FLOAT(IRAL1)) / 60.                           CAL 5860
      WRITE (IOUT,1002) NCY,CUP,RAL                                     CAL 5870
      NCY = NCY + 1                                                     CAL 5880
      IF(NCY .GT. 1 .AND. DPLH(2) .NE. 0) SPARE(100) = DPLH(2)          CAL 5890
      NGC(2) = 0                                                        CAL 5900
      I12 = ICLOCK(0)                                                   CAL 5910
      II1 = (I12-I11) / 60                                              CAL 5920
      IF(II1 .LT. ITMX(24)) GOTO 167                                    CAL 5930
      WRITE (IOUT,1016)                                                 CAL 5940
      MAC4 = 1                                                          CAL 5950
      IF(NGC(15) .EQ. 2) GOTO 179                                       CAL 5960
      GOTO 168                                                          CAL 5970
  167 CONTINUE                                                          CAL 5980
      IF(INTGR(77) .GT. 0 .AND. INTGR(35) .LT. 0) GOTO 168              CAL 5990
      IF(NCY .LE. NCYCLE) GOTO 105                                      CAL 6000
  168 CONTINUE                                                          CAL 6010
      NCY = 1                                                           CAL 6020
      IX(16) = 0                                                        CAL 6030
      SPARE(1) = 0.0                                                    CAL 6040
      SPARE(2) = 0.0                                                    CAL 6050
      SPARE(9) = 0.0                                                    CAL 6060
      SPARE(10) = 0.0                                                   CAL 6070
      SPARE(11) = 0.0                                                   CAL 6080
      SPARE(12) = 0.0                                                   CAL 6090
      SPARE(13) = 0.0                                                   CAL 6100
      SPARE(26) = 0.0                                                   CAL 6110
      SPARE(74) = 0.0                                                   CAL 6120
      SPARE(75) = 0.0                                                   CAL 6130
      IX(198) = 0                                                       CAL 6140
      IF(INTGR(55) .GT. 0) GOTO 177                                     CAL 6150
      IF(NGC(3) .EQ. 0 .OR. MAC4 .LE. 1 .OR. IQUE .EQ. MAC4) GOTO 173   CAL 6160
      REWIND IO13                                                       CAL 6170
  169 CONTINUE                                                          CAL 6180
      READ (IO13,END=171) ISIGNL                                        CAL 6190
      IF(ISIGNL .NE. -11111) GOTO 169                                   CAL 6200
      DO 170 N=1,8                                                      CAL 6210
        READ (IO13,END=171)                                             CAL 6220
  170 CONTINUE                                                          CAL 6230
      GOTO 172                                                          CAL 6240
  171 CONTINUE                                                          CAL 6250
      WRITE (IOUT,1003)                                                 CAL 6260
C                                                                       CAL 6270
      CALL EXIT                                                         CAL 6280
C                                                                       CAL 6290
  172 CONTINUE                                                          CAL 6300
      IF(IX(199) .GT. 0) READ (IO13,END=171)                            CAL 6310
  173 CONTINUE                                                          CAL 6320
      IQUE = IQUE + 1                                                   CAL 6330
      IF(IQUE .LE. MAC4) GOTO 104                                       CAL 6340
      IF(MAC4 .EQ. 1) GOTO 177                                          CAL 6350
      WRITE (IOUT,1004)                                                 CAL 6360
      IF(INTGR(26) .EQ. 0) GOTO 177                                     CAL 6370
  175 CONTINUE                                                          CAL 6380
      WRITE (IOUT,1001)                                                 CAL 6390
      IF(NGC(3) .EQ. 0) GOTO 176                                        CAL 6400
      END FILE IO13                                                     CAL 6410
      REWIND IO13                                                       CAL 6420
  176 CONTINUE                                                          CAL 6430
C                                                                       CAL 6440
      CALL EXIT                                                         CAL 6450
C                                                                       CAL 6460
  177 CONTINUE                                                          CAL 6470
      GOTO 180                                                          CAL 6480
  178 WRITE (IOUT,1010) T1                                              CAL 6490
C                                                                       CAL 6500
      CALL EXIT                                                         CAL 6510
C                                                                       CAL 6520
  179 CONTINUE                                                          CAL 6530
  180 CONTINUE                                                          CAL 6540
      RETURN                                                            CAL 6550
C                                                                       CAL 6560
 1000 FORMAT (1H0,'REPEAT CYCLE CONDITIONS NOT MET')                    CAL 6570
 1001 FORMAT (1H0,'OPTION TO TERMINATE IS ON')                          CAL 6580
 1002 FORMAT (1H0,'CYCLE',I4,'  REQUIRED',0PF7.3,'  MINUTES CPU TIME, ANCAL 6590
     1D',F7.3,'  MINUTES CLOCK TIME')                                   CAL 6600
 1003 FORMAT (1H0,'ERROR STOP 913')                                     CAL 6610
 1004 FORMAT (1H0,'EQUILIBRIUM CYCLE CONDITIONS NOT MET')               CAL 6620
 1005 FORMAT (1H0,'INITIAL REACTIVITY LOOP ITERATION COUNT EXCEEDED')   CAL 6630
 1006 FORMAT (1H ,'ERROR STOP 13')                                      CAL 6640
 1007 FORMAT (1H0,' CYCLE REACTIVITY LOOP ITERATION COUNT EXCEEDED')    CAL 6650
 1008 FORMAT (1H ,'ERROR STOP 13')                                      CAL 6660
 1009 FORMAT (1H0,'TIME STEP',I3,'  REQUIRED',0PF7.3,'  MINUTES CPU TIMECAL 6670
     1, AND',F7.3,'  MINUTES CLOCK TIME')                               CAL 6680
 1010 FORMAT (1H0,'THE MULTIPLICATION FACTOR ',1PE13.5,1H ,'EXCEEDS THE CAL 6690
     1RESTRAINTS'/1H ,'SEE GLIM1,GLIM2, CARD C6, SECTION 4.4.6')        CAL 6700
 1011 FORMAT (1H0,'MAXIMUM EXPOSURE TIME FOR CYCLE',I3,1H ,'EXCEEDED')  CAL 6710
 1012 FORMAT (1H0,'INITIAL REACTIVITY LOOP COMPUTER TIME LIMIT EXCEEDED'CAL 6720
     1 )                                                                CAL 6730
 1013 FORMAT (1H0,'CYCLE REACTIVITY LOOP COMPUTER TIME LIMIT EXCEEDED') CAL 6740
 1014 FORMAT (1H0,'REPEAT TIME STEP LOOP COMPUTER TIME LIMIT EXCEEDED') CAL 6750
 1015 FORMAT (1H0,'REPEAT CYCLE LOOP COMPUTER TIME LIMIT EXCEEDED')     CAL 6760
 1016 FORMAT (1H0,'TOTAL COMPUTER TIME LIMIT EXCEEDED')                 CAL 6770
      END                                                               CAL 6780