      SUBROUTINE CNST(NRGN,SCAC,DCONB,DCONR,F1,SIG,PTSA,NCOMP,PVOL,BBND,CNS   10
     1 BND,IVX,JVX,KVX,LVX,MVX,IVXP1,JVXP1,IVZ,KVZ,IOVX,IOVZ,A,MEMORY,  CNS   20
     2 AIO,IX3738)                                                      CNS   30
C                                                                       CNS   40
CCNST --101 ***CITATION*** EQ. CNSTS. FOR 1,2-D/ CF-EIGN                CNS   50
C                                                                       CNS   60
      REAL*8 TPTSA,D1,D2,D3,D4,D5,D6,D7                                 CNS   70
C                                                                       CNS   80
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,CNS   90
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   CNS  100
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), CNS  110
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    CNS  120
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    CNS  130
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   CNS  140
     6 IXPUT(9999),XPUT(9999)                                           CNS  150
C                                                                       CNS  160
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  CNS  170
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), CNS  180
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)CNS  190
     3 ,PDK(211)                                                        CNS  200
C                                                                       CNS  210
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   CNS  220
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKCNS  230
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    CNS  240
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  CNS  250
     4 ITMAX,ITIME,BET(211),DEL(211)                                    CNS  260
C                                                                       CNS  270
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           CNS  280
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    CNS  290
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),CNS  300
     3 NCLASS(9999),NDP(9999)                                           CNS  310
C                                                                       CNS  320
      COMMON /MU/ MU4                                                   CNS  330
C                                                                       CNS  340
CFZJ055                                                       25.09.07  CNS  350
C                                                                       CNS  360
CFZJ031                                                       28.05.04  CNS  370
      DIMENSION NRGN(JVX,IVX),SCAC(KVX,MVX,KVX),DCONB(JVX,IVXP1,IOVX),  CNS  380
     1 DCONR(JVXP1,IVZ,IOVZ),F1(KVX,MVX),SIG(KVX,MVX,10),               CNS  390
     2 PTSA(JVX,IVX,IOVX),NCOMP(LVX),PVOL(LVX),BBND(KVX),BND(6,KVX),    CNS  400
     3 AIO(IX3738),A(MEMORY),B(2,33,9999)                               CNS  410
C                                                                       CNS  420
C                                                                       CNS  430
C     INRB = 1  ORDINARY                                                CNS  440
C     INRB = 2  PERIODIC(REPEATING)                                     CNS  450
C     INRB = 3  90 DEGREE ROTATIONAL                                    CNS  460
C     INRB = 4  180 DEGREE ROTATIONAL                                   CNS  470
C                                                                       CNS  480
C     HIER WIRD DIE BEHRENSKORREKTUR GEMACHT                            CNS  490
C                                                                       CNS  500
C     B(1,..,..) ENTSPRICHT X-RICHTUNG UND INDEX J                      CNS  510
C     B(2,..,..) ENTSPRICHT Y-RICHTUNG UND INDEX I                      CNS  520
C                                                                       CNS  530
C     FUER RZ-GEOMETRIE IST R=X UND Z=Y                                 CNS  540
C                                                                       CNS  550
C     B(..,K,L) IST D(BEHRENSKORRIGIERT))/D(UNKORR.) IN GRUPPE K UND    CNS  560
C     ZONE L                                                            CNS  570
C                                                                       CNS  580
 1000 FORMAT (1H0,'ERROR STOP',4I6)                                     CNS  590
 1001 FORMAT (1H0,'DIAGONAL SYMMETRY OPTION DENIED')                    CNS  600
 1002 FORMAT (//' PLEASE ENLARGE GROUP DIMENSION OF FIELD B IN SR CNST')CNS  610
 1003 FORMAT (//' PLEASE ENLARGE ZONE DIMENSION OF FIELD B IN SR CNST') CNS  620
 1004 FORMAT (18I4)                                                     CNS  630
 1005 FORMAT (//'  JZMI=',I5,'  JZMA=',I5,' IRICHT=',I5,' IRIC=',I5/'  SCNS  640
     1OMETHING WRONG WHILE READING BEHRENS CORRECTION')                 CNS  650
 1006 FORMAT (6E12.0)                                                   CNS  660
 1007 FORMAT (////'   THE DIFFUSION COEFFICIENT HAS BEEN MULTIPLIED WITHCNS  670
     1 THE FOLLOWING FACTORS'/)                                         CNS  680
 1008 FORMAT (//'   ZONE',I4,' TO',I4,'  GROUP    X-DIRECTION    Y-DIRECCNS  690
     1TION'/)                                                           CNS  700
 1009 FORMAT (20X,I4,4X,1PE12.5,3X,E12.5)                               CNS  710
 1101 FORMAT (///' FOR NUAC(5) =',I3,' B(1,..,..) MUST BE = B(2,..,..)  CNS  720
     1  -->  *** STOP ***')                                             CNS  730
C                                                                       CNS  740
C                                                                       CNS  750
      IF(KMAX .GT. 33) WRITE (IOUT,1002)                                CNS  760
      IF(MMAX .GT. 9999) WRITE (IOUT,1003)                              CNS  770
      IF(KMAX .GT. 33) STOP                                             CNS  780
      IF(MMAX .GT. 9999) STOP                                           CNS  790
      DO 300 I=1,2                                                      CNS  800
        DO 300 K=1,KMAX                                                 CNS  810
          DO 300 M=1,MMAX                                               CNS  820
            B(I,K,M) = 1.0                                              CNS  830
  300 CONTINUE                                                          CNS  840
C                                                                       CNS  850
C     EINLESEN VON B                                                    CNS  860
C     IRICHT  IRIC                                                      CNS  870
C       0           EIN WERT FUER X UND Y                               CNS  880
C       1       1   NUR X                                               CNS  890
C       1       2   BEIDE,ERST X DANN Y                                 CNS  900
C       2       2   NUR Y                                               CNS  910
C                                                                       CNS  920
      ISCHR = 1                                                         CNS  930
      IF(NGC(24)) 305,320,320                                           CNS  940
  305 READ (IOIN,1004) JZMI,JZMA,IRICHT,IRIC                            CNS  950
      IF(JZMI) 310,320,330                                              CNS  960
  330 IF(IRICHT .GT. 2 .OR. IRICHT .LT. 0) GOTO 310                     CNS  970
      IF(IRIC .GT. 2 .OR. IRIC .LT. 0) GOTO 310                         CNS  980
      IF(IRIC .LT. IRICHT) GOTO 310                                     CNS  990
      IF(JZMI .LE. JZMA) GOTO 331                                       CNS 1000
  310 WRITE (IOUT,1005) JZMI,JZMA,IRICHT,IRIC                           CNS 1010
      STOP                                                              CNS 1020
  331 IF(IRICHT .GT. 0) GOTO 401                                        CNS 1030
      READ (IOIN,1006) (B(1,K,JZMI),K=1,KMAX)                           CNS 1040
      GOTO 402                                                          CNS 1050
  401 DO 335 I=IRICHT,IRIC                                              CNS 1060
        READ (IOIN,1006) (B(I,K,JZMI),K=1,KMAX)                         CNS 1070
  335 CONTINUE                                                          CNS 1080
  402 DO 340 I=1,2                                                      CNS 1090
        II = I                                                          CNS 1100
        IF(IRICHT .EQ. 0) II = 1                                        CNS 1110
        DO 340 J=JZMI,JZMA                                              CNS 1120
          DO 340 K=1,KMAX                                               CNS 1130
            B(I,K,J) = B(II,K,JZMI)                                     CNS 1140
  340 CONTINUE                                                          CNS 1150
      IF(ISCHR .EQ. 1 .AND. MU4 .EQ. 1) WRITE (IOUT,1007)               CNS 1160
      ISCHR = 0                                                         CNS 1170
      IF(MU4 .EQ. 1) WRITE (IOUT,1008) JZMI,JZMA                        CNS 1180
      DO 350 K=1,KMAX                                                   CNS 1190
        IF(MU4 .EQ. 1) WRITE (IOUT,1009) K,B(1,K,JZMI),B(2,K,JZMI)      CNS 1200
  350 CONTINUE                                                          CNS 1210
      GOTO 305                                                          CNS 1220
  320 CONTINUE                                                          CNS 1230
      IF(NUAC(5) .NE. 9 .AND. NUAC(5) .NE. 10) GOTO 500                 CNS 1240
      DO 510 K=1,KMAX                                                   CNS 1250
        DO 510 M=1,MMAX                                                 CNS 1260
          IF(B(1,K,M) .EQ. B(2,K,M)) GOTO 510                           CNS 1270
          WRITE (IOUT,1101) NUAC(5)                                     CNS 1280
          STOP                                                          CNS 1290
  510 CONTINUE                                                          CNS 1300
  500 CONTINUE                                                          CNS 1310
      INRB = IX(72) + 1                                                 CNS 1320
      KMAXP1 = KMAX + 1                                                 CNS 1330
      IO14 = IX(81)                                                     CNS 1340
      REWIND IO14                                                       CNS 1350
      GOTO(107,107,102,100),INRB                                        CNS 1360
  100 I = IVX / 2                                                       CNS 1370
      DO 101 J=1,I                                                      CNS 1380
        K = IVX + 1 - J                                                 CNS 1390
        TAL = (YY(J)-Y(J)) / (Y(K)-YY(K+1))                             CNS 1400
        IF(NUAC(5) .EQ. 10) TAL = (PDI(I)-YY(I)) / (YY(K+1)-Y(K))       CNS 1410
        EPICH = ABS(TAL-1.0)                                            CNS 1420
        IF(EPICH .GT. 1.0E-4) GOTO 103                                  CNS 1430
  101 CONTINUE                                                          CNS 1440
      GOTO 107                                                          CNS 1450
  102 CONTINUE                                                          CNS 1460
      IDA = IVX                                                         CNS 1470
      IF(NUAC(5) .EQ. 10) IDA = 2 * IVX                                 CNS 1480
      IF(JVX .EQ. IDA) GOTO 104                                         CNS 1490
  103 JB = 32                                                           CNS 1500
      WRITE (IOUT,1000) JB,I,J                                          CNS 1510
C                                                                       CNS 1520
      CALL EXIT                                                         CNS 1530
C                                                                       CNS 1540
  104 CONTINUE                                                          CNS 1550
      J = 1                                                             CNS 1560
      DO 106 I=1,IVX                                                    CNS 1570
        TAL = Y(I) / X(I)                                               CNS 1580
        IF(NUAC(5) .NE. 10 .OR. I .EQ. 1) GOTO 105                      CNS 1590
        J = J + 2                                                       CNS 1600
        TAL = (Y(I)-PDI(I-1)) / (X(J)-X(J-1))                           CNS 1610
  105   EPICH = ABS(TAL-1.0)                                            CNS 1620
        IF(EPICH .GT. 1.0E-4) GOTO 103                                  CNS 1630
  106 CONTINUE                                                          CNS 1640
  107 CONTINUE                                                          CNS 1650
      IX37 = IX(37)                                                     CNS 1660
      IO15 = IX(82)                                                     CNS 1670
      IOADJ = IO15                                                      CNS 1680
      IF(IX(71) .GT. 0) IOADJ = IO2                                     CNS 1690
      IF(IX37 .GT. 0) REWIND IOADJ                                      CNS 1700
      IF(IX(71) .GT. 0) IX(5) = IX(17)                                  CNS 1710
      EPICH = 1.0E-5                                                    CNS 1720
      SPARE(51) = -1.0E+30                                              CNS 1730
      SPARE(57) = -1.0E+30                                              CNS 1740
      N17 = NUAC(17)                                                    CNS 1750
      SQUIRE = 1.1547005                                                CNS 1760
      IF(N17) 114,114,108                                               CNS 1770
  108 IF(XMIS(2)) 114,109,109                                           CNS 1780
  109 DO 113 K=1,KMAX                                                   CNS 1790
        IF(XMIS(2)) 113,110,111                                         CNS 1800
  110   T1 = 0.4692                                                     CNS 1810
        GOTO 112                                                        CNS 1820
  111   T1 = XMIS(2)                                                    CNS 1830
  112   BBND(K) = T1                                                    CNS 1840
  113 CONTINUE                                                          CNS 1850
  114 CONTINUE                                                          CNS 1860
      NGEM = IX(26)                                                     CNS 1870
      PI = 3.141593                                                     CNS 1880
      IF(IX37 .GT. 0) GOTO 121                                          CNS 1890
      DO 117 K=1,IOVX                                                   CNS 1900
        DO 116 I=1,IVXP1                                                CNS 1910
          DO 115 J=1,JVX                                                CNS 1920
            DCONB(J,I,K) = 0.0                                          CNS 1930
  115     CONTINUE                                                      CNS 1940
  116   CONTINUE                                                        CNS 1950
  117 CONTINUE                                                          CNS 1960
      DO 120 K=1,IOVZ                                                   CNS 1970
        DO 119 I=1,IVZ                                                  CNS 1980
          DO 118 J=1,JVXP1                                              CNS 1990
            DCONR(J,I,K) = 0.0                                          CNS 2000
  118     CONTINUE                                                      CNS 2010
  119   CONTINUE                                                        CNS 2020
  120 CONTINUE                                                          CNS 2030
  121 CONTINUE                                                          CNS 2040
      SPARE(98) = 0.0                                                   CNS 2050
      DO 134 K=1,KMAX                                                   CNS 2060
        READ (IO14) ((F1(KK,M),KK=1,KMAX),M=1,MMAX)                     CNS 2070
        DO 133 M=1,MMAX                                                 CNS 2080
          IF(M-N17) 128,123,128                                         CNS 2090
  123     IF(XMIS(2)) 124,125,125                                       CNS 2100
  124     IF(BBND(K)) 125,128,125                                       CNS 2110
  125     DO 126 L=1,10                                                 CNS 2120
            SIG(K,M,L) = 0.0                                            CNS 2130
  126     CONTINUE                                                      CNS 2140
          DO 127 KK=1,KMAX                                              CNS 2150
            SCAC(K,M,KK) = 0.0                                          CNS 2160
  127     CONTINUE                                                      CNS 2170
          GOTO 133                                                      CNS 2180
  128     CONTINUE                                                      CNS 2190
          SIG(K,M,9) = SIG(K,M,1) * SIG(K,M,6)                          CNS 2200
          SPARE(98) = SPARE(98) + SIG(K,M,8)                            CNS 2210
C                                                                       CNS 2220
C     SEARCH OPTIONS                                                    CNS 2230
C                                                                       CNS 2240
          IF(IX(5) .NE. -2) GOTO 131                                    CNS 2250
          IF(IX(44) .EQ. 0 .AND. IX(49) .EQ. 0) GOTO 130                CNS 2260
          IF(IX(49) .GT. 0) GOTO 129                                    CNS 2270
          IF(M .EQ. IX(44) .OR. M .EQ. IX(45) .OR. M .EQ. IX(46) .OR. M CNS 2280
     1     .EQ. IX(47) .OR. M .EQ. IX(48)) GOTO 130                     CNS 2290
          GOTO 131                                                      CNS 2300
  129     IF(IX(49) .NE. NCLASS(M)) GOTO 131                            CNS 2310
  130     SIG(K,M,5) = SIG(K,M,9)                                       CNS 2320
          SIG(K,M,9) = 0.0                                              CNS 2330
  131     CONTINUE                                                      CNS 2340
          DO 132 KK=1,KMAX                                              CNS 2350
            SCAC(K,M,KK) = F1(KK,M)                                     CNS 2360
  132     CONTINUE                                                      CNS 2370
  133   CONTINUE                                                        CNS 2380
  134 CONTINUE                                                          CNS 2390
      REWIND IO14                                                       CNS 2400
      T1 = 0.0                                                          CNS 2410
      DO 141 K=1,KMAX                                                   CNS 2420
        DO 140 M=1,MMAX                                                 CNS 2430
          TPTSA = 0.D+0                                                 CNS 2440
          DO 135 KK=1,KMAX                                              CNS 2450
            TPTSA = TPTSA + SCAC(K,M,KK)                                CNS 2460
  135     CONTINUE                                                      CNS 2470
          SIG(K,M,2) = TPTSA                                            CNS 2480
          TPTSA = TPTSA + SIG(K,M,3) + SIG(K,M,9)                       CNS 2490
          SIG(K,M,10) = TPTSA                                           CNS 2500
          SIG(K,M,3) = SIG(K,M,10) - SIG(K,M,2) - SIG(K,M,9)            CNS 2510
          IF(SIG(K,M,10) .EQ. 0.0 .OR. SIG(K,M,5) .EQ. 0.0) GOTO 136    CNS 2520
          SPARE(51) = AMAX1(SPARE(51),SIG(K,M,5)/SIG(K,M,10))           CNS 2530
C                                                                       CNS 2540
C     SEARCH OPTIONS                                                    CNS 2550
C                                                                       CNS 2560
  136     CONTINUE                                                      CNS 2570
          IF(IX(5) .EQ. -5) GOTO 139                                    CNS 2580
          IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 139                   CNS 2590
          IF(IX(44) .EQ. 0 .AND. IX(49) .EQ. 0) GOTO 138                CNS 2600
          IF(IX(49) .GT. 0) GOTO 137                                    CNS 2610
          IF(M .EQ. IX(44) .OR. M .EQ. IX(45) .OR. M .EQ. IX(46) .OR. M CNS 2620
     1     .EQ. IX(47) .OR. M .EQ. IX(48)) GOTO 138                     CNS 2630
          GOTO 139                                                      CNS 2640
  137     IF(IX(49) .NE. NCLASS(M)) GOTO 139                            CNS 2650
  138     T1 = T1 + SIG(K,M,5)                                          CNS 2660
  139     CONTINUE                                                      CNS 2670
  140   CONTINUE                                                        CNS 2680
  141 CONTINUE                                                          CNS 2690
C                                                                       CNS 2700
C     SEARCH OPTIONS                                                    CNS 2710
C                                                                       CNS 2720
      IF(IX(5) .EQ. -5) GOTO 143                                        CNS 2730
      IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 143                       CNS 2740
      IF(T1 .NE. 0.0) GOTO 143                                          CNS 2750
      JB = 16                                                           CNS 2760
      WRITE (IOUT,1000) JB                                              CNS 2770
C                                                                       CNS 2780
      CALL EXIT                                                         CNS 2790
C                                                                       CNS 2800
  143 CONTINUE                                                          CNS 2810
      DO 243 KT1=1,KMAX                                                 CNS 2820
        K = KT1                                                         CNS 2830
        N = KT1                                                         CNS 2840
        IF(IX37 .EQ. 0) GOTO 149                                        CNS 2850
        N = 1                                                           CNS 2860
        IF(IX(71) .GT. 0) K = KMAXP1 - KT1                              CNS 2870
        DO 145 I=1,IVXP1                                                CNS 2880
          DO 144 J=1,JVX                                                CNS 2890
            DCONB(J,I,1) = 0.0                                          CNS 2900
  144     CONTINUE                                                      CNS 2910
  145   CONTINUE                                                        CNS 2920
        DO 148 K1=1,IOVZ                                                CNS 2930
          DO 147 I=1,IVX                                                CNS 2940
            DO 146 J=1,JVXP1                                            CNS 2950
              DCONR(J,I,K1)= 0.0                                        CNS 2960
  146       CONTINUE                                                    CNS 2970
  147     CONTINUE                                                      CNS 2980
  148   CONTINUE                                                        CNS 2990
  149   CONTINUE                                                        CNS 3000
        DO 230 I=1,IMAX                                                 CNS 3010
          T1 = YY(I+1) - YY(I)                                          CNS 3020
          DELTT = 0.0                                                   CNS 3030
          IF(I-1) 151,151,150                                           CNS 3040
  150     DELTT = YY(I) - Y(I-1)                                        CNS 3050
          IF(NGEM .EQ. 10) DELTT = YY(I) - PDI(I-1)                     CNS 3060
  151     DELT = Y(I) - YY(I)                                           CNS 3070
          DELB = YY(I+1) - Y(I)                                         CNS 3080
          IF(NGEM .EQ. 10) DELB = YY(I+1) - PDI(I)                      CNS 3090
          DELBB = 0.0                                                   CNS 3100
          IF(I-IMAX) 152,153,153                                        CNS 3110
  152     DELBB = Y(I+1) - YY(I+1)                                      CNS 3120
  153     CONTINUE                                                      CNS 3130
          DO 229 J=1,JMAX                                               CNS 3140
            N2 = J                                                      CNS 3150
            NRN = NRGN(J,I)                                             CNS 3160
            NOE = J - (J/2) * 2                                         CNS 3170
            DELLL = 0.0                                                 CNS 3180
            IF(J-1) 155,155,154                                         CNS 3190
  154       DELLL = XX(J) - X(J-1)                                      CNS 3200
            IF(NGEM .EQ. 10) N2 = N2 - 1                                CNS 3210
  155       DELL = X(J) - XX(J)                                         CNS 3220
            DELR = XX(J+1) - X(J)                                       CNS 3230
            DELRR = 0.0                                                 CNS 3240
            IF(J-JMAX) 156,157,157                                      CNS 3250
  156       DELRR = X(J+1) - XX(J+1)                                    CNS 3260
  157       CONTINUE                                                    CNS 3270
            GOTO(163,164,166,169,169,163,164,165,158,167),NGEM          CNS 3280
  158       TAT = SQUIRE * DELT                                         CNS 3290
            TAL = SQUIRE * DELL                                         CNS 3300
            TAR = DELR + DELL                                           CNS 3310
            TAB = DELB + DELT                                           CNS 3320
            TD1 = X(J+1) - X(J)                                         CNS 3330
            TD2 = Y(I) - Y(I-1)                                         CNS 3340
            IF(I .EQ. 1) TD2 = 2 * Y(I)                                 CNS 3350
            IF(J .EQ. JMAX) TD1 = TD2                                   CNS 3360
            DELHT = 0.5 * SQRT(TD1**2+TD2**2-TD1*TD2)                   CNS 3370
            DELHB = DELHT                                               CNS 3380
            TAR = TAL                                                   CNS 3390
            TAB = TAT                                                   CNS 3400
            TAH = SQUIRE * DELHT                                        CNS 3410
            MH = 0                                                      CNS 3420
            IF(J-JMAX) 160,162,162                                      CNS 3430
  160       IF(I-1) 162,162,161                                         CNS 3440
  161       NHN = NRGN(J+1,I-1)                                         CNS 3450
            MH = NCOMP(NHN)                                             CNS 3460
  162       GOTO 169                                                    CNS 3470
  163       TAL = T1                                                    CNS 3480
            TAR = T1                                                    CNS 3490
            TAT = XX(J+1) - XX(J)                                       CNS 3500
            TAB = TAT                                                   CNS 3510
            GOTO 169                                                    CNS 3520
  164       TAL = 2.0 * PI * XX(J) * T1                                 CNS 3530
            TAR = 2.0 * PI * XX(J+1) * T1                               CNS 3540
            TAT = PI * XX(J+1)**2                                       CNS 3550
            IF(XX(J) .NE. 0.) TAT = TAT - PI * XX(J)**2                 CNS 3560
            TAB = TAT                                                   CNS 3570
            GOTO 169                                                    CNS 3580
  165       TAL = T1                                                    CNS 3590
            TAR = T1                                                    CNS 3600
            TAT = YY(I) * (XX(J+1)-XX(J))                               CNS 3610
            TAB = YY(I+1) * (XX(J+1)-XX(J))                             CNS 3620
            DELR = DELR * Y(I)                                          CNS 3630
            DELRR = DELRR * Y(I)                                        CNS 3640
            DELL = DELL * Y(I)                                          CNS 3650
            DELLL = DELLL * Y(I)                                        CNS 3660
            GOTO 169                                                    CNS 3670
  166       TAL = 0.                                                    CNS 3680
            IF(XX(J) .NE. 0.) TAL = 4.0 *PI * XX(J)**2                  CNS 3690
            TAR = 4.0 * PI * XX(J+1)**2                                 CNS 3700
            TAT = 0.0                                                   CNS 3710
            TAB = 0.0                                                   CNS 3720
            GOTO 169                                                    CNS 3730
  167       CONTINUE                                                    CNS 3740
            TAL = 3.46410 * (X(J)-XX(J))                                CNS 3750
C           2 * SQRT(3) = 3.46410                                       CNS 3760
            TAR = 3.46410 * (XX(J+1)-X(J))                              CNS 3770
            IF(NOE .EQ. 0) GOTO 168                                     CNS 3780
            TAT = 3.46410 * (Y(I)-YY(I))                                CNS 3790
            TAB = 0.0                                                   CNS 3800
            GOTO 169                                                    CNS 3810
  168       CONTINUE                                                    CNS 3820
            TAB = 3.46410 * (YY(I+1)-PDI(I))                            CNS 3830
            TAT = 0.0                                                   CNS 3840
  169       CONTINUE                                                    CNS 3850
            M = NCOMP(NRN)                                              CNS 3860
            IF(I .EQ. 1) GOTO 170                                       CNS 3870
            NRNT = NRGN(J,I-1)                                          CNS 3880
            IF(NGEM .EQ. 10 .AND. J .LT. JMAX) NRNT = NRGN(J+1,I-1)     CNS 3890
            MT = NCOMP(NRNT)                                            CNS 3900
  170       IF(J .EQ. 1) GOTO 171                                       CNS 3910
            NRNL = NRGN(J-1,I)                                          CNS 3920
            ML = NCOMP(NRNL)                                            CNS 3930
            GOTO 172                                                    CNS 3940
  171       IF(INRB .NE. 2) GOTO 172                                    CNS 3950
            NRNL = NRGN(JMAX,I)                                         CNS 3960
            ML = NCOMP(NRNL)                                            CNS 3970
  172       IF(I .EQ. IMAX) GOTO 173                                    CNS 3980
            NRNB = NRGN(J,I+1)                                          CNS 3990
            IF(NGEM .EQ. 10 .AND. J .GT. 1) NRNB = NRGN(J-1,I+1)        CNS 4000
            MB = NCOMP(NRNB)                                            CNS 4010
            GOTO 174                                                    CNS 4020
  173       IF(INRB .NE. 3) GOTO 174                                    CNS 4030
            NRNB = NRGN(JVX,J)                                          CNS 4040
            IF(NGEM .EQ. 10) NRNB = NRGN(JVX,J/2)                       CNS 4050
            MB = NCOMP(NRNB)                                            CNS 4060
            DELB = XX(JVXP1) - X(JVX)                                   CNS 4070
  174       IF(J .EQ. JMAX) GOTO 175                                    CNS 4080
            NRNR = NRGN(J+1,I)                                          CNS 4090
            MR = NCOMP(NRNR)                                            CNS 4100
            GOTO 179                                                    CNS 4110
  175       GOTO(179,176,177,178),INRB                                  CNS 4120
  176       CONTINUE                                                    CNS 4130
            NRNR = NRGN(1,I)                                            CNS 4140
            MR = NCOMP(NRNR)                                            CNS 4150
            DELRR = X(1)                                                CNS 4160
            IF(NGEM .EQ. 8) DELRR = DELRR * Y(I)                        CNS 4170
            GOTO 179                                                    CNS 4180
  177       CONTINUE                                                    CNS 4190
            NRNR = NRGN(I,IVX)                                          CNS 4200
            IF(NGEM .EQ. 10) NRNR = NRGN(2*I,IVX)                       CNS 4210
            MR = NCOMP(NRNR)                                            CNS 4220
            DELRR = YY(IVXP1) - Y(IVX)                                  CNS 4230
            IF(NGEM .EQ. 10) DELRR = YY(IVXP1) - PDI(IVX)               CNS 4240
            GOTO 179                                                    CNS 4250
  178       CONTINUE                                                    CNS 4260
            NRNR = NRGN(JVX,IVXP1-I)                                    CNS 4270
            MR = NCOMP(NRNR)                                            CNS 4280
            DELRR = DELR                                                CNS 4290
  179       CONTINUE                                                    CNS 4300
            IF(M .NE. N17) GOTO 180                                     CNS 4310
            IF(XMIS(2) .GE. 0.0) GOTO 196                               CNS 4320
            IF(BBND(K) .NE. 0.0) GOTO 196                               CNS 4330
  180       IF(I .GT. 1) GOTO 181                                       CNS 4340
C           DCONB(J,I,N) = BND(2,K) * TAT / (1.0+DELT*BND(2,K)/         CNS 4350
C            SIG(K,M,1))                                                CNS 4360
            DCONB(J,I,N) = BND(2,K) * TAT / (1.0+DELT*BND(2,K)/         CNS 4370
     1       SIG(K,M,1)/B(2,K,M))                                       CNS 4380
  181       IF(J .GT. 1) GOTO 182                                       CNS 4390
            IF(INRB .EQ. 2) GOTO 182                                    CNS 4400
C           DCONR(J,I,N) = BND(1,K) * TAL / (1.0+DELL*BND(1,K)/         CNS 4410
C            SIG(K,M,1))                                                CNS 4420
            DCONR(J,I,N) = BND(1,K) * TAL / (1.0+DELL*BND(1,K)/         CNS 4430
     1       SIG(K,M,1)/B(1,K,M))                                       CNS 4440
  182       IF(I .LT. IMAX) GOTO 183                                    CNS 4450
C           DCONB(N2,I+1,N) = BND(4,K) * TAB / (1.0+DELB*BND(4,K)/      CNS 4460
C            SIG(K,M,1))                                                CNS 4470
            DCONB(N2,I+1,N) = BND(4,K) * TAB / (1.0+DELB*BND(4,K)/      CNS 4480
     1       SIG(K,M,1)/B(2,K,M))                                       CNS 4490
            IF(DCONB(N2,I+1,N) .EQ. 0.0) DCONB(N2,I+1,N) = 4096.0E-13   CNS 4500
  183       IF(J .LT. JMAX) GOTO 184                                    CNS 4510
            IF(INRB .GT. 1) GOTO 184                                    CNS 4520
C           DCONR(J+1,I,N) = BND(3,K) * TAR / (1.0+DELR*BND(3,K)/       CNS 4530
C            SIG(K,M,1))                                                CNS 4540
            DCONR(J+1,I,N) = BND(3,K) * TAR / (1.0+DELR*BND(3,K)/       CNS 4550
     1       SIG(K,M,1)/B(1,K,M))                                       CNS 4560
            IF(DCONR(J+1,I,N) .EQ. 0.0) DCONR(J+1,I,N) = 4096.0E-13     CNS 4570
            GOTO 185                                                    CNS 4580
C 184       DCONR(J+1,I,N) = SIG(K,MR,1) * TAR / (DELRR+SIG(K,MR,1)*DELRCNS 4590
C            /SIG(K,M,1))                                               CNS 4600
  184       DCONR(J+1,I,N) = B(1,K,MR) * SIG(K,MR,1) * TAR / (DELRR+    CNS 4610
     1       B(1,K,MR)*SIG(K,MR,1)*DELR/(B(1,K,M)*SIG(K,M,1)))          CNS 4620
            IF(DCONR(J+1,I,N) .EQ. 0.0) DCONR(J+1,I,N) = 4096.0E-13     CNS 4630
  185       IF(I .GE. IMAX) GOTO 186                                    CNS 4640
C           DCONB(N2,I+1,N) = TAB * SIG(K,MB,1) / (DELBB+DELB*          CNS 4650
C            SIG(K,MB,1)/SIG(K,M,1))                                    CNS 4660
            DCONB(N2,I+1,N) = TAB * B(2,K,MB) * SIG(K,MB,1) / (DELBB+   CNS 4670
     1       DELB*B(2,K,MB)*SIG(K,MB,1)/(B(2,K,M)*SIG(K,M,1)))          CNS 4680
            IF(DCONB(N2,I+1,N) .EQ. 0.0) DCONB(N2,I+1,N) = 4096.0E-13   CNS 4690
  186       CONTINUE                                                    CNS 4700
            IF(N17 .LE. 0) GOTO 196                                     CNS 4710
            IF(XMIS(2) .GE. 0.0) GOTO 187                               CNS 4720
            IF(BBND(K) .EQ. 0.0) GOTO 196                               CNS 4730
  187       IF(IMAX .LE. 1) GOTO 192                                    CNS 4740
            IF(I .NE. IMAX) GOTO 189                                    CNS 4750
            IF(INRB .NE. 3) GOTO 190                                    CNS 4760
            IF(MB .NE. N17) GOTO 190                                    CNS 4770
C           DCONB(J,I+1,N) = BBND(K) * TAB / (1.0+DELB*BBND(K)/         CNS 4780
C            SIG(K,M,1))                                                CNS 4790
            DCONB(J,I+1,N) = BBND(K) * TAB / (1.0+DELB*BBND(K)/         CNS 4800
     1       SIG(K,M,1)/B(2,K,M))                                       CNS 4810
            IF(DCONB(J,I+1,N) .EQ. 0.0) DCONB(J,I+1,N) = 4096.0E-13     CNS 4820
            IF(NGEM .EQ. 10) GOTO 188                                   CNS 4830
            DCONR(JVXP1,I,N) = DCONB(J,I+1,N)                           CNS 4840
            GOTO 190                                                    CNS 4850
  188       CONTINUE                                                    CNS 4860
            IF((J/2)*2 .NE. J) GOTO 190                                 CNS 4870
            DCONR(JVXP1,J/2,N) = DCONB(N2,I+1,N)                        CNS 4880
            GOTO 190                                                    CNS 4890
  189       IF(MB .NE. N17) GOTO 190                                    CNS 4900
C           DCONB(N2,I+1,N) = BBND(K) * TAB / (1.0+DELB*BBND(K)/        CNS 4910
C            SIG(K,M,1))                                                CNS 4920
            DCONB(N2,I+1,N) = BBND(K) * TAB / (1.0+DELB*BBND(K)/        CNS 4930
     1       SIG(K,M,1)/B(2,K,M))                                       CNS 4940
            IF(DCONB(N2,I+1,N) .EQ. 0.0) DCONB(N2,I+1,N) = 4096.0E-13   CNS 4950
  190       IF(I .EQ. 1) GOTO 192                                       CNS 4960
            IF(MT .NE. N17) GOTO 192                                    CNS 4970
C           DCONB(J,I,N) = BBND(K) * TAT / (1.0+DELT*BBND(K)/SIG(K,M,1))CNS 4980
            DCONB(J,I,N) = BBND(K) * TAT / (1.0+DELT*BBND(K)/SIG(K,M,1) CNS 4990
     1       /B(2,K,M))                                                 CNS 5000
  192       CONTINUE                                                    CNS 5010
            IF(JMAX .LE. 1) GOTO 196                                    CNS 5020
            IF(J .LT. JMAX) GOTO 193                                    CNS 5030
            IF(INRB .LE. 1) GOTO 195                                    CNS 5040
  193       IF(MR .NE. N17) GOTO 194                                    CNS 5050
C           DCONR(J+1,I,N) = BBND(K) * TAR / (1.0+DELR*BBND(K)/         CNS 5060
C            SIG(K,M,1))                                                CNS 5070
            DCONR(J+1,I,N) = BBND(K) * TAR / (1.0+DELR*BBND(K)/         CNS 5080
     1       SIG(K,M,1)/B(1,K,M))                                       CNS 5090
            IF(DCONR(J+1,I,N) .EQ. 0.0) DCONR(J+1,I,N) = 4096.0E-13     CNS 5100
            IF(INRB .EQ. 4) DCONR(JVXP1,IVXP1-I,N) = DCONR(JVXP1,I,N)   CNS 5110
  194       IF(J .GT. 1) GOTO 195                                       CNS 5120
            IF(INRB .NE. 2) GOTO 196                                    CNS 5130
            IF(ML .NE. N17) GOTO 196                                    CNS 5140
C           DCONR(JVXP1,I,N) = BBND(K) * TAL / (1.0+DELL*BBND(K)/       CNS 5150
C            SIG(K,M,1))                                                CNS 5160
            DCONR(JVXP1,I,N) = BBND(K) * TAL / (1.0+DELL*BBND(K)/       CNS 5170
     1       SIG(K,M,1)/B(1,K,M))                                       CNS 5180
            IF(DCONR(JVXP1,I,N) .EQ. 0.0) DCONR(JVXP1,I,N) = 4096.0E-13 CNS 5190
            GOTO 196                                                    CNS 5200
  195       IF(ML .NE. N17) GOTO 196                                    CNS 5210
C           DCONR(J,I,N) = BBND(K) * TAL / (1.0+DELL*BBND(K)/SIG(K,M,1))CNS 5220
            DCONR(J,I,N) = BBND(K) * TAL / (1.0+DELL*BBND(K)/SIG(K,M,1)/CNS 5230
     1       B(1,K,M))                                                  CNS 5240
            IF(DCONR(J,I,N) .EQ. 0.0) DCONR(J,I,N) = 4096.0E-13         CNS 5250
  196       IF(NGEM .NE. 9) GOTO 228                                    CNS 5260
            KKK = IOVX + N                                              CNS 5270
            IF(N17) 198,198,199                                         CNS 5280
  198       CONTINUE                                                    CNS 5290
            IF(J .EQ. JMAX) GOTO 209                                    CNS 5300
            IF(I .EQ. 1) GOTO 224                                       CNS 5310
C           DCONR(J+1,I,KKK) = TAH * SIG(K,MH,1) / (DELHT+DELHB*        CNS 5320
C            SIG(K,MH,1)/SIG(K,M,1))                                    CNS 5330
            DCONR(J+1,I,KKK) = TAH * SIG(K,MH,1) * B(1,K,MH) / (DELHT+  CNS 5340
     1       DELHB*SIG(K,MH,1)*B(1,K,MH)/SIG(K,M,1)/B(1,K,M))           CNS 5350
            GOTO 208                                                    CNS 5360
  199       IF(XMIS(2)) 200,202,202                                     CNS 5370
  200       IF(BBND(K)) 198,201,202                                     CNS 5380
  201       DCONR(J+1,I,KKK) = 0                                        CNS 5390
            GOTO 208                                                    CNS 5400
  202       IF(MH-N17) 205,203,205                                      CNS 5410
  203       IF(M-N17) 204,201,204                                       CNS 5420
C 204       DCONR(J+1,I,KKK) = BBND(K) * TAH / (1.0+DELHT*BBND(K)/      CNS 5430
C            SIG(K,M,1))                                                CNS 5440
  204       DCONR(J+1,I,KKK) = BBND(K) * TAH / (1.0+DELHT*BBND(K)/      CNS 5450
     1       SIG(K,M,1)/B(1,K,M))                                       CNS 5460
            GOTO 208                                                    CNS 5470
  205       IF(M-N17) 198,206,198                                       CNS 5480
  206       IF(MH) 201,201,207                                          CNS 5490
C 207       DCONR(J+1,I,KKK) = BBND(K) * TAH / (1.0+DELHB*BBND(K)/      CNS 5500
C            SIG(K,MH,1))                                               CNS 5510
  207       DCONR(J+1,I,KKK) = BBND(K) * TAH / (1.0+DELHB*BBND(K)/      CNS 5520
     1       SIG(K,MH,1)/B(1,K,MH))                                     CNS 5530
  208       IF(J-JMAX) 216,209,209                                      CNS 5540
  209       DCONR(J+1,I,KKK) = 0                                        CNS 5550
            IF(I-1) 210,210,214                                         CNS 5560
  210       IF(DCONR(J+1,I,N)-4096.0E-13) 211,212,211                   CNS 5570
  211       DCONR(J+1,I,N) = 1.5 * DCONR(J+1,I,N) / SQUIRE              CNS 5580
  212       IF(DCONB(J,I,N)-4096.0E-13) 213,224,213                     CNS 5590
  213       DCONB(J,I,N) = 1.5 * DCONB(J,I,N) / SQUIRE                  CNS 5600
            GOTO 224                                                    CNS 5610
  214       IF(DCONR(J+1,I,N)-4096.0E-13) 215,216,215                   CNS 5620
  215       DCONR(J+1,I,N) = 2.0 * DCONR(J+1,I,N) / SQUIRE              CNS 5630
  216       IF(I-IMAX) 224,217,217                                      CNS 5640
  217       DCONR(J,I+1,KKK) = 0                                        CNS 5650
            IF(J-1) 218,218,222                                         CNS 5660
  218       IF(DCONR(J,I,N)-4096.0E-13) 219,220,219                     CNS 5670
  219       DCONR(J,I,N) = 1.5 * DCONR(J,I,N) / SQUIRE                  CNS 5680
  220       IF(DCONB(J,I+1,N)-4096.0E-13) 221,224,221                   CNS 5690
  221       DCONB(J,I+1,N) = 1.5 * DCONB(J,I+1,N) / SQUIRE              CNS 5700
            GOTO 224                                                    CNS 5710
  222       IF(DCONB(J,I+1,N)-4096.0E-13) 223,224,223                   CNS 5720
  223       DCONB(J,I+1,N) = 2.0 * DCONB(J,I+1,N) / SQUIRE              CNS 5730
  224       IF(I-1) 225,225,226                                         CNS 5740
  225       DCONR(J+1,I,KKK) = 0                                        CNS 5750
            IF(J .NE. JMAX) DCONB(J,1,N) = 2.0 * DCONB(J,1,N) / SQUIRE  CNS 5760
  226       IF(J-1) 227,227,228                                         CNS 5770
  227       DCONR(J,I+1,KKK) = 0                                        CNS 5780
            IF(I .NE. IMAX) DCONR(1,I,N) = 2.0 * DCONR(1,I,N) / SQUIRE  CNS 5790
  228       CONTINUE                                                    CNS 5800
  229     CONTINUE                                                      CNS 5810
  230   CONTINUE                                                        CNS 5820
        GOTO(237,235,233,231),INRB                                      CNS 5830
  231   I = IVX / 2                                                     CNS 5840
        DO 232 LL=1,I                                                   CNS 5850
          DCONR(JVXP1,IVXP1-LL,N) = DCONR(JVXP1,LL,N)                   CNS 5860
  232   CONTINUE                                                        CNS 5870
        GOTO 237                                                        CNS 5880
  233   DCONR(JVXP1,IVX,N) = 4096.0E-13                                 CNS 5890
        LJ = 1                                                          CNS 5900
        DO 234 LL=1,IVX                                                 CNS 5910
          DCONB(LJ,IVXP1,N) = DCONR(JVXP1,LL,N)                         CNS 5920
          LJ = LJ + 1                                                   CNS 5930
          IF(NGEM .EQ. 10) LJ = LJ + 1                                  CNS 5940
  234   CONTINUE                                                        CNS 5950
        GOTO 237                                                        CNS 5960
  235   DO 236 I=1,IVX                                                  CNS 5970
          DCONR(1,I,N) = DCONR(JVXP1,I,N)                               CNS 5980
  236   CONTINUE                                                        CNS 5990
  237   CONTINUE                                                        CNS 6000
        DO 242 I=1,IMAX                                                 CNS 6010
          DO 241 J=1,JMAX                                               CNS 6020
            NRN = NRGN(J,I)                                             CNS 6030
            M = NCOMP(NRN)                                              CNS 6040
            D1 = DCONB(J,I,N)                                           CNS 6050
            D2 = DCONB(J,I+1,N)                                         CNS 6060
            IF(NGEM .NE. 10) GOTO 238                                   CNS 6070
            D2 = 0.0                                                    CNS 6080
            IF(J .EQ. 1) GOTO 238                                       CNS 6090
            D2 = DCONB(J-1,I+1,N)                                       CNS 6100
  238       CONTINUE                                                    CNS 6110
            D3 = DCONR(J,I,N)                                           CNS 6120
            D4 = DCONR(J+1,I,N)                                         CNS 6130
            D7 = SIG(K,M,10)                                            CNS 6140
            TPTSA = D1 + D2 + D3 + D4 + D7 * PVOL(NRN)                  CNS 6150
            IF(NGEM .NE. 9) GOTO 239                                    CNS 6160
            D5 = DCONR(J,I+1,KKK)                                       CNS 6170
            D6 = DCONR(J+1,I,KKK)                                       CNS 6180
            TPTSA = TPTSA + D5 + D6                                     CNS 6190
  239       CONTINUE                                                    CNS 6200
            PTSA(J,I,N) = TPTSA                                         CNS 6210
            IF((TPTSA .EQ. 0.0) .OR. (SIG(K,M,5) .EQ. 0.0)) GOTO 240    CNS 6220
            SPARE(57) = AMAX1(SPARE(57),SIG(K,M,5)*PVOL(NRN)/           CNS 6230
     1       PTSA(J,I,N))                                               CNS 6240
  240       CONTINUE                                                    CNS 6250
  241     CONTINUE                                                      CNS 6260
  242   CONTINUE                                                        CNS 6270
        IF(IX37 .EQ. 0) GOTO 243                                        CNS 6280
        WRITE (IOADJ) AIO                                               CNS 6290
  243 CONTINUE                                                          CNS 6300
      IF(IX37 .EQ. 0) GOTO 244                                          CNS 6310
      END FILE IOADJ                                                    CNS 6320
      REWIND IOADJ                                                      CNS 6330
  244 CONTINUE                                                          CNS 6340
C                                                                       CNS 6350
C     DIAGONAL SYMMETRY CHECKOUT                                        CNS 6360
C                                                                       CNS 6370
      IF(IX(71) .GT. 0) GOTO 262                                        CNS 6380
      IF(NUAC(8)) 247,262,245                                           CNS 6390
  245 IF(IMAX-JMAX) 246,254,246                                         CNS 6400
  246 NUAC(8) = 0                                                       CNS 6410
      WRITE (IOUT,1001)                                                 CNS 6420
      GOTO 261                                                          CNS 6430
  247 II = IMAX / 2                                                     CNS 6440
      DO 253 N=1,KMAX                                                   CNS 6450
        K = N                                                           CNS 6460
        IF(IX37 .EQ. 0) GOTO 248                                        CNS 6470
        K = 1                                                           CNS 6480
        READ (IO15) AIO                                                 CNS 6490
  248   CONTINUE                                                        CNS 6500
        DO 252 I=1,II                                                   CNS 6510
          L = IMAX - I + 1                                              CNS 6520
          DO 251 J=1,JMAX                                               CNS 6530
            M = JMAX - J + 1                                            CNS 6540
            MT = NRGN(J,I)                                              CNS 6550
            ML = NRGN(M,L)                                              CNS 6560
            IF(NCOMP(MT)-NCOMP(ML)) 246,249,246                         CNS 6570
  249       CONTINUE                                                    CNS 6580
            IF(PTSA(J,I,K) .EQ. 0.0) GOTO 250                           CNS 6590
            IF(ABS(PTSA(M,L,K)/PTSA(J,I,K)-1.0)-EPICH) 250,246,246      CNS 6600
  250       CONTINUE                                                    CNS 6610
  251     CONTINUE                                                      CNS 6620
  252   CONTINUE                                                        CNS 6630
  253 CONTINUE                                                          CNS 6640
      GOTO 261                                                          CNS 6650
  254 CONTINUE                                                          CNS 6660
      DO 260 N=1,KMAX                                                   CNS 6670
        K = N                                                           CNS 6680
        IF(IX37 .EQ. 0) GOTO 255                                        CNS 6690
        K = 1                                                           CNS 6700
        READ (IO15) AIO                                                 CNS 6710
  255   CONTINUE                                                        CNS 6720
        DO 259 I=1,IMAX                                                 CNS 6730
          DO 258 J=I,JMAX                                               CNS 6740
            MT = NRGN(J,I)                                              CNS 6750
            ML = NRGN(I,J)                                              CNS 6760
            IF(NCOMP(MT)-NCOMP(ML)) 246,256,246                         CNS 6770
  256       CONTINUE                                                    CNS 6780
            IF(PTSA(I,J,K) .EQ. 0.0) GOTO 257                           CNS 6790
            IF(ABS(PTSA(J,I,K)/PTSA(I,J,K)-1.0)-EPICH) 257,246,246      CNS 6800
  257       CONTINUE                                                    CNS 6810
  258     CONTINUE                                                      CNS 6820
  259   CONTINUE                                                        CNS 6830
  260 CONTINUE                                                          CNS 6840
      NUAC(20) = 1                                                      CNS 6850
  261 CONTINUE                                                          CNS 6860
      IF(IX37 .GT. 0) REWIND IO15                                       CNS 6870
  262 CONTINUE                                                          CNS 6880
      IF(SPARE(51) .NE. 0.0) SPARE(51) = -1.0 / SPARE(51)               CNS 6890
      IF(SPARE(57) .NE. 0.0) SPARE(57) = -1.0 / SPARE(57)               CNS 6900
      IF(IX(71) .GT. 0) IX(5)= 0                                        CNS 6910
      RETURN                                                            CNS 6920
      END                                                               CNS 6930
      SUBROUTINE RDUE(SCAC,RESLM,RESSA,P2,NRGN,SOUR,DCONR,DCONB,XI,IVX, RDU   10
     1 JVX,KVX,LVX,IVXP1,JVXP1,IVZ,KVZ,IOVX,IOVZ,A,MEMORY,AIO,IX3738,   RDU   20
     2 XLAMDA,SIG,PVOL,NCOMP,MVX)                                       RDU   30
C                                                                       RDU   40
CRDUE --103 ***CITATION*** RESIDUE CALC. FOR 1,2-D/ CF-FLUX             RDU   50
C                                                                       RDU   60
      REAL*8 P2,P2UT,P2UB,P2UL,P2UR,P2UH,P2UI,T1,TT1,TT2,TT3,TT4,TT5,TT6RDU   70
     1 ,XLA,SOUR,XLAMDA,XLD,TTT1,TTT2,TTT3,TTT4                         RDU   80
C                                                                       RDU   90
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,RDU  100
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   RDU  110
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), RDU  120
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    RDU  130
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    RDU  140
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   RDU  150
     6 IXPUT(9999),XPUT(9999)                                           RDU  160
C                                                                       RDU  170
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   RDU  180
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKRDU  190
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    RDU  200
     3 VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  RDU  210
     4 ITMAX,ITIME,BET(211),DEL(211)                                    RDU  220
C                                                                       RDU  230
      DIMENSION SCAC(KVX,MVX,KVX),P2(JVX,IVX, KVX),NRGN(JVX,IVX),       RDU  240
     1 SOUR(JVX,IVX),DCONR(JVXP1,IVZ,IOVZ),DCONB(JVX,IVXP1,IOVX),XI(KVX)RDU  250
     2 ,AIO(IX3738),A(MEMORY),SIG(KVX,MVX,10),PVOL(LVX),NCOMP(LVX)      RDU  260
C                                                                       RDU  270
C                                                                       RDU  280
C     INRB = 1  ORDINARY                                                RDU  290
C     INRB = 2  PERIODIC(REPEATING)                                     RDU  300
C     INRB = 3  90 DEGREE ROTATIONAL                                    RDU  310
C     INRB = 4  180 DEGREE ROTATIONAL                                   RDU  320
C                                                                       RDU  330
      INRB = IX(72) + 1                                                 RDU  340
      KMAXP1 = KMAX + 1                                                 RDU  350
      IX37 = IX(37)                                                     RDU  360
      IO15 = IX(82)                                                     RDU  370
      IOADJ = IO15                                                      RDU  380
      IF(IX(71) .GT. 0) IOADJ = IO2                                     RDU  390
      IF(IX37 .GT. 0) REWIND IOADJ                                      RDU  400
      TTT1 = 0.0                                                        RDU  410
      TTT2 = 0.0                                                        RDU  420
      TTT3 = 0.0                                                        RDU  430
      TTT4 = 0.0                                                        RDU  440
      IF(IX(24) .EQ. 0 .OR. IX(17) .EQ. 0) GOTO 100                     RDU  450
      XLA = 1.0 / SPARE(50)                                             RDU  460
      XLD = XLAMDA                                                      RDU  470
      IF(IX(17) .GE. 1) XLD = 0.0                                       RDU  480
      GOTO 103                                                          RDU  490
  100 CONTINUE                                                          RDU  500
C                                                                       RDU  510
C********SEARCH OPTIONS                                                 RDU  520
C                                                                       RDU  530
      IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 102                       RDU  540
      XLD = XLAMDA                                                      RDU  550
      IF(IX(5) .EQ. 1) XLD = 0.0                                        RDU  560
      XLA = 1.0 / SPARE(50)                                             RDU  570
      GOTO 103                                                          RDU  580
  102 XLD = 0.0                                                         RDU  590
      XLA = XLAMDA                                                      RDU  600
  103 CONTINUE                                                          RDU  610
      DO 129 KT1=1,KMAX                                                 RDU  620
        IF(IX37 .EQ. 0) GOTO 106                                        RDU  630
        READ (IOADJ) AIO                                                RDU  640
        IF(IX(71) .GT. 0) GOTO 104                                      RDU  650
        K = KT1                                                         RDU  660
        GOTO 105                                                        RDU  670
  104   K = KMAXP1 - KT1                                                RDU  680
  105   N = 1                                                           RDU  690
        GOTO 109                                                        RDU  700
  106   CONTINUE                                                        RDU  710
        IF(IX(24) .GT. 0) GOTO 107                                      RDU  720
        K = KT1                                                         RDU  730
        GOTO 108                                                        RDU  740
  107   K = KMAXP1 - KT1                                                RDU  750
  108   N = K                                                           RDU  760
  109   CONTINUE                                                        RDU  770
        DO 128 I=1,IMAX                                                 RDU  780
          DO 127 J=1,JMAX                                               RDU  790
            T1 = P2(J,I,K)                                              RDU  800
            IF(T1 .EQ. 0.) GOTO 127                                     RDU  810
            L = NRGN(J,I)                                               RDU  820
            M = NCOMP(L)                                                RDU  830
            TT5 = 0.0                                                   RDU  840
            IF(IX(24) .EQ. 0) GOTO 112                                  RDU  850
            DO 111 KK=1,KMAX                                            RDU  860
              TT5 = TT5 + SCAC(K,M,KK) * P2(J,I,KK)                     RDU  870
  111       CONTINUE                                                    RDU  880
            GOTO 114                                                    RDU  890
  112       CONTINUE                                                    RDU  900
            DO 113 KK=1,KMAX                                            RDU  910
              TT5 = TT5 + SCAC(KK,M,K) * P2(J,I,KK)                     RDU  920
  113       CONTINUE                                                    RDU  930
  114       CONTINUE                                                    RDU  940
            TT1 = (SIG(K,M,3)+SIG(K,M,9)+XLD*SIG(K,M,5)) * T1 * PVOL(L) RDU  950
            TT3 = SIG(K,M,2) * T1                                       RDU  960
            TDR = DCONR (J+1,I,N)                                       RDU  970
            IF(TDR .EQ. 4096.0E-13) TDR = 0.0                           RDU  980
            T2 = DCONB(J,I,N)                                           RDU  990
            T3 = DCONB(J,I+1,N)                                         RDU 1000
            N2 = J                                                      RDU 1010
            N3 = J                                                      RDU 1020
            IF(NUAC(5) .NE. 10) GOTO 116                                RDU 1030
            NOE = J - (J/2) * 2                                         RDU 1040
            IF(NOE .EQ. 0) GOTO 115                                     RDU 1050
            N2 = N2 + 1                                                 RDU 1060
            T3 = 0.0                                                    RDU 1070
            GOTO 116                                                    RDU 1080
  115       CONTINUE                                                    RDU 1090
            T2 = 0.0                                                    RDU 1100
            N3 = N3 - 1                                                 RDU 1110
            T3 = DCONB(N3,I+1,N)                                        RDU 1120
  116       CONTINUE                                                    RDU 1130
            IF(I. GT. 1) P2UT = P2(N2,I-1,K)                            RDU 1140
            IF(I .LT. IMAX) P2UB = P2(N3,I+1,K)                         RDU 1150
            IF(J .GT. 1) P2UL = P2(J-1,I,K)                             RDU 1160
            IF(J .LT. JMAX) P2UR = P2(J+1,I,K)                          RDU 1170
            IF(I .EQ. 1) P2UT = 0.                                      RDU 1180
            IF(I .LT. IMAX) GOTO 117                                    RDU 1190
            P2UB = 0.0                                                  RDU 1200
            IF(INRB .NE. 3) GOTO 117                                    RDU 1210
            P2UB = P2(JVX,J,K)                                          RDU 1220
            IF(NUAC(5) .EQ. 10) P2UB = P2(JVX,J/2,K)                    RDU 1230
  117       IF(J .GT. 1) GOTO 118                                       RDU 1240
            P2UL = 0.0                                                  RDU 1250
            IF(INRB .EQ. 2) P2UL = P2(JMAX,I,K)                         RDU 1260
  118       IF(J .LT. JMAX) GOTO 122                                    RDU 1270
            P2UR = 0.0                                                  RDU 1280
            GOTO(122,119,120,121),INRB                                  RDU 1290
  119       P2UR = P2(1,I,K)                                            RDU 1300
            GOTO 122                                                    RDU 1310
  120       P2UR = P2(I,IVX,K)                                          RDU 1320
            IF(NUAC(5) .EQ. 10) P2UR = P2(2*I,IVX,K)                    RDU 1330
            GOTO 122                                                    RDU 1340
  121       P2UR = P2(JVX,IVXP1-I,K)                                    RDU 1350
  122       CONTINUE                                                    RDU 1360
            IF(NUAC(5) .EQ. 9) GOTO 123                                 RDU 1370
            TT4 = (T1-P2UT) * T2 + (T1-P2UB) * T3 + (T1- P2UL) *        RDU 1380
     1       DCONR(J,I,N) + (T1-P2UR) * TDR                             RDU 1390
            GOTO 124                                                    RDU 1400
C                                                                       RDU 1410
C     2-D HEXAGONAL RESIDUES CONTRIBUTION FROM LEAKAGE TERMS            RDU 1420
C                                                                       RDU 1430
  123       KKK = N + IOVX                                              RDU 1440
            P2UH = P2(J-1,I+1,K)                                        RDU 1450
            P2UI = P2(J+1,I-1,K)                                        RDU 1460
            IF(I .EQ. 1 .OR. J .EQ. JMAX) P2UI = 0.0                    RDU 1470
            IF(J .EQ. 1 .OR. I .EQ .IMAX) P2UH = 0.0                    RDU 1480
            TT4 = (T1-P2UT) * DCONB(J,I,N) + (T1-P2UB) * DCONB(J,I+1,N) RDU 1490
     1       + (T1-P2UL) * DCONR(J,I,N) + (T1-P2UR) * TDR + (T1-P2UH) * RDU 1500
     2       DCONR(J,I+1,KKK) + (T1-P2UI) * DCONR(J+1,I,KKK)            RDU 1510
  124       CONTINUE                                                    RDU 1520
            IF(IX(24) .EQ. 0) GOTO 125                                  RDU 1530
            TT6 = SIG(K,M,4) * PVOL(L) * SOUR(J,I) * XKEF1              RDU 1540
            GOTO 126                                                    RDU 1550
  125       TT6 = XI(K) * SOUR(J,I) + XLD * SIG(K,M,8) * PVOL(L)        RDU 1560
  126       CONTINUE                                                    RDU 1570
            TT3 = PVOL(L) * (TT3-TT5)                                   RDU 1580
            TTT1 = TTT1 + TT6 * (TT4+TT1+TT3)                           RDU 1590
            TTT2 = TTT2 + TT6**2                                        RDU 1600
            TTT3 = TTT3 + TT1 * (XLA*TT6-TT4-TT3)                       RDU 1610
            TTT4 = TTT4 + TT1**2                                        RDU 1620
  127     CONTINUE                                                      RDU 1630
  128   CONTINUE                                                        RDU 1640
  129 CONTINUE                                                          RDU 1650
      IF(IX37 .GT. 0) REWIND IOADJ                                      RDU 1660
C                                                                       RDU 1670
C     RESIDUES ESTIMATE OF EIGENVALUE (1/K)                             RDU 1680
C                                                                       RDU 1690
      RESLM = TTT2 / TTT1                                               RDU 1700
C                                                                       RDU 1710
C     RESIDUES ESTIMATE OF RELATIVE ABSORPTION CROSS SECTION            RDU 1720
C                                                                       RDU 1730
      RESSA = TTT3 / TTT4                                               RDU 1740
      IF(NUAC(3) .LE. 0.) GOTO 130                                      RDU 1750
      XKEF2 = XKEF1                                                     RDU 1760
      XKEF1 = RESLM                                                     RDU 1770
      XLAMDA = 1.0 / XKEF1                                              RDU 1780
      VRGK1 = ABS(XKEF2/XKEF1-1.0)                                      RDU 1790
  130 CONTINUE                                                          RDU 1800
      RETURN                                                            RDU 1810
      END                                                               RDU 1820
      SUBROUTINE FWRD(SCAT,P2,DCONB,DCONR,PTSA,IVX,JVX,KVX,IVXP1,JVXP1, FWR   10
     1 IVZ,KVZ,BET,DEL,NRGN,E1,LVX,IOVX,IOVZ)                           FWR   20
C                                                                       FWR   30
CFWRD --105 ***CITATION*** LINE RELAXATION ON ROWS FOR 1,2-D/ CF-DNSD   FWR   40
C                                                                       FWR   50
      REAL*8 BET(211),DEL(211),T,TEMP,TMF,P2,SCAT                       FWR   60
C                                                                       FWR   70
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,FWR   80
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   FWR   90
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), FWR  100
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    FWR  110
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    FWR  120
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   FWR  130
     6 IXPUT(9999),XPUT(9999)                                           FWR  140
C                                                                       FWR  150
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   FWR  160
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKFWR  170
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    FWR  180
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  FWR  190
     4 ITMAX,ITIME,BAT(211),DAL(211)                                    FWR  200
C                                                                       FWR  210
      DIMENSION SCAT(JVX,IVX),P2(JVX,IVX,KVX),DCONB(JVX,IVXP1,IOVX),    FWR  220
     1 DCONR(JVXP1,IVZ,IOVZ),PTSA(JVX,IVX,IOVX),NRGN(JVX,IVX),          FWR  230
     2 E1(LVX,KVX)                                                      FWR  240
C                                                                       FWR  250
C                                                                       FWR  260
      INRB = IX(72) + 1                                                 FWR  270
      N = IX(20)                                                        FWR  280
      DO 143 I=1,IVX                                                    FWR  290
        IP1 = I - 1                                                     FWR  300
        IP2 = I + 1                                                     FWR  310
        DEL(1) = 0.0                                                    FWR  320
        D1 = DCONB(1,I,N)                                               FWR  330
        D2 = DCONB(1,IP2,N)                                             FWR  340
        D4 = DCONR(2,I,N)                                               FWR  350
        IF(I-1) 102,102,108                                             FWR  360
  102   IF(P2(1,1,K)) 103,104,103                                       FWR  370
  103   BET(1) = (P2(1,2,K)*D2+SCAT(1,1)) / D4                          FWR  380
        DEL(1) = D4 / (PTSA(1,1,N)+E1(1,K))                             FWR  390
        IF(INRB .EQ. 2) BET(1) = BET(1) + P2(JVX,I,K) * DCONR(1,I,N) /  FWR  400
     1   D4                                                             FWR  410
  104   DO 107 J=2,JVX                                                  FWR  420
          IF(P2(J,1,K)) 106,105,106                                     FWR  430
  105     DEL(J) = 0.0                                                  FWR  440
          GOTO 107                                                      FWR  450
  106     T = D4 * DEL(J-1)                                             FWR  460
          L = NRGN(J,1)                                                 FWR  470
          D4 = DCONR(J+1,1,N)                                           FWR  480
          BET(J) = (P2(J,2,K)*DCONB(J,2,N)+SCAT(J,1)+BET(J-1)*T) / D4   FWR  490
          DEL(J) = D4 / (PTSA(J,1,N)-T+E1(L,K))                         FWR  500
  107   CONTINUE                                                        FWR  510
        GOTO 121                                                        FWR  520
  108   IF(I-IVX) 115,109,109                                           FWR  530
  109   IF(P2(1,IVX,K)) 110,111,110                                     FWR  540
  110   BET(1) = (P2(1,IP1,K)*D1+SCAT(1,IVX)) / D4                      FWR  550
        L = NRGN(1,IVX)                                                 FWR  560
        DEL(1) = D4 / (PTSA(1,IVX,N)+E1(L,K))                           FWR  570
        IF(INRB .EQ. 2) BET(1) = BET(1) + P2(JVX,I,K) * DCONR(1,I,N) /  FWR  580
     1   D4                                                             FWR  590
        IF(INRB .EQ. 3) BET(1) = BET(1) + P2(JVX,1,K) * D2 / D4         FWR  600
  111   DO 114 J=2,JVX                                                  FWR  610
          IF(P2(J,IVX,K)) 113,112,113                                   FWR  620
  112     DEL(J) = 0.0                                                  FWR  630
          GOTO 114                                                      FWR  640
  113     T = D4 * DEL(J-1)                                             FWR  650
          L = NRGN(J,IVX)                                               FWR  660
          D4 = DCONR(J+1,IVX,N)                                         FWR  670
          BET(J) = (P2(J,IP1,K)*DCONB(J,IVX,N)+SCAT(J,IVX)+BET(J-1)*T) /FWR  680
     1     D4                                                           FWR  690
          DEL(J) = D4 / (PTSA(J,IVX,N)-T+E1(L,K))                       FWR  700
          IF(INRB .NE. 3) GOTO 114                                      FWR  710
          IF(J .EQ. JVX) GOTO 114                                       FWR  720
          BET(J) = BET(J) + P2(JVX,J,K) * DCONB(J,IVXP1,N) / D4         FWR  730
  114   CONTINUE                                                        FWR  740
        GOTO 121                                                        FWR  750
  115   IF(P2(1,I,K)) 116,117,116                                       FWR  760
  116   CONTINUE                                                        FWR  770
        L = NRGN(1,I)                                                   FWR  780
        BET(1) = (P2(1,IP1,K)*D1+P2(1,IP2,K)*D2+SCAT(1,I)) / D4         FWR  790
        DEL(1) = D4 / (PTSA(1,I,N)+E1(L,K))                             FWR  800
        IF(INRB .EQ. 2) BET(1) = BET(1) + P2(JVX,I,K) * DCONR(1,I,N) /  FWR  810
     1   D4                                                             FWR  820
  117   CONTINUE                                                        FWR  830
        DO 120 J=2,JVX                                                  FWR  840
          IF(P2(J,I,K)) 119,118,119                                     FWR  850
  118     DEL(J) = 0.0                                                  FWR  860
          GOTO 120                                                      FWR  870
  119     T = D4 * DEL(J-1)                                             FWR  880
          L = NRGN(J,I)                                                 FWR  890
          D4 = DCONR(J+1,I,N)                                           FWR  900
          BET(J) = (P2(J,IP1,K)*DCONB(J,I,N)+P2(J,IP2,K)*DCONB(J,IP2,N)+FWR  910
     1     SCAT(J,I)+BET(J-1)*T) / D4                                   FWR  920
          DEL(J) = D4 / (PTSA(J,I,N)-T+E1(L,K))                         FWR  930
  120   CONTINUE                                                        FWR  940
  121   CONTINUE                                                        FWR  950
        GOTO(125,122,123,124),INRB                                      FWR  960
  122   BET(JVX) = BET(JVX) + P2(1,I,K)                                 FWR  970
        GOTO 125                                                        FWR  980
  123   IF(I .EQ. IVX) GOTO 125                                         FWR  990
        BET(JVX) = BET(JVX) + P2(I,IVX,K)                               FWR 1000
        GOTO 125                                                        FWR 1010
  124   BET(JVX) = BET(JVX) + P2(JVX,IVXP1-I,K)                         FWR 1020
  125   TEMP = BET(JVX) * DEL(JVX)                                      FWR 1030
        T = P2(JVX,I,K)                                                 FWR 1040
        TMF = T + BETTA * (TEMP-T)                                      FWR 1050
        IF(IEP) 126,130,127                                             FWR 1060
  126   P2(JVX,I,K) = TEMP                                              FWR 1070
        GOTO 131                                                        FWR 1080
  127   IF(TMF-TEMP) 129,130,128                                        FWR 1090
  128   TMF = DMIN1(TMF,(TEMP+T))                                       FWR 1100
        GOTO 130                                                        FWR 1110
  129   TMF = DMAX1(TMF,0.5*TEMP)                                       FWR 1120
  130   CONTINUE                                                        FWR 1130
        P2(JVX,I,K) = TMF                                               FWR 1140
  131   DO 138 JJ=2,JVX                                                 FWR 1150
          J = JVXP1 - JJ                                                FWR 1160
          T = P2(J,I,K)                                                 FWR 1170
          TEMP = DEL(J) * (TEMP+BET(J))                                 FWR 1180
          TMF = T + BETTA * (TEMP-T)                                    FWR 1190
          IF(IEP) 132,136,133                                           FWR 1200
  132     P2(J,I,K) = TEMP                                              FWR 1210
          GOTO 138                                                      FWR 1220
  133     IF(TMF-TEMP) 135,136,134                                      FWR 1230
  134     TMF = DMIN1(TMF,(TEMP+T))                                     FWR 1240
          GOTO 136                                                      FWR 1250
  135     TMF = DMAX1(TMF,0.5*TEMP)                                     FWR 1260
  136     CONTINUE                                                      FWR 1270
          P2(J,I,K) = TMF                                               FWR 1280
  138   CONTINUE                                                        FWR 1290
        IF(NUAC(8)) 139,143,141                                         FWR 1300
  139   L = IVXP1 - I                                                   FWR 1310
        DO 140 J=1,JVX                                                  FWR 1320
          M = JVXP1 - J                                                 FWR 1330
          P2(M,L,K) = P2(J,I,K)                                         FWR 1340
  140   CONTINUE                                                        FWR 1350
        GOTO 143                                                        FWR 1360
  141   DO 142 J=1,JVX                                                  FWR 1370
          P2(I,J,K) = P2(J,I,K)                                         FWR 1380
  142   CONTINUE                                                        FWR 1390
  143 CONTINUE                                                          FWR 1400
      RETURN                                                            FWR 1410
      END                                                               FWR 1420
      SUBROUTINE FXRD(SCAT,P2,DCONB,DCONR,PTSA,IVX,JVX,KVX,IVXP1,JVXP1, FXR   10
     1 IVZ,KVZ,BET,DEL,NRGN,E1,LVX,IOVX,IOVZ)                           FXR   20
C                                                                       FXR   30
CFXRD --106 ***CITATION*** LINE RELAXATION ON COLS FOR 1,2-D/ CF-DNSD   FXR   40
C                                                                       FXR   50
      REAL*8 BET(211),DEL(211),T,TEMP,TMF,P2,SCAT                       FXR   60
C                                                                       FXR   70
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,FXR   80
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   FXR   90
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), FXR  100
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    FXR  110
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    FXR  120
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   FXR  130
     6 IXPUT(9999),XPUT(9999)                                           FXR  140
C                                                                       FXR  150
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   FXR  160
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKFXR  170
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    FXR  180
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  FXR  190
     4 ITMAX,ITIME,BAT(211),DAL(211)                                    FXR  200
C                                                                       FXR  210
      DIMENSION SCAT(JVX,IVX),P2(JVX,IVX,KVX),DCONB(JVX,IVXP1,IOVX),    FXR  220
     1 DCONR(JVXP1,IVZ,IOVZ),PTSA(JVX,IVX,IOVX),NRGN(JVX,IVX),          FXR  230
     2 E1(LVX,KVX)                                                      FXR  240
C                                                                       FXR  250
C                                                                       FXR  260
      INRB = IX(72) + 1                                                 FXR  270
      N = IX(20)                                                        FXR  280
      DO 142 J=1,JVX                                                    FXR  290
        JP1 = J - 1                                                     FXR  300
        JP2 = J + 1                                                     FXR  310
        DEL(1) = 0.0                                                    FXR  320
        D1 = DCONR(J,1,N)                                               FXR  330
        D2 = DCONR(JP2,1,N)                                             FXR  340
        D4 = DCONB(J,2,N)                                               FXR  350
        IF(J-1) 102,102,108                                             FXR  360
  102   IF(P2(1,1,K)) 103,104,103                                       FXR  370
  103   BET(1) = (P2(2,1,K)*D2+SCAT(1,1)) / D4                          FXR  380
        DEL(1) = D4 / (PTSA(1,1,N)+E1(1,K))                             FXR  390
  104   DO 107 I=2,IVX                                                  FXR  400
          IF(P2(1,I,K)) 106,105,106                                     FXR  410
  105     DEL(I) = 0.0                                                  FXR  420
          GOTO 107                                                      FXR  430
  106     T = D4 * DEL(I-1)                                             FXR  440
          L = NRGN(1,I)                                                 FXR  450
          D4 = DCONB(1,I+1,N)                                           FXR  460
          BET(I) = (P2(2,I,K)*DCONR(2,I,N)+ SCAT(1,I)+BET(I-1)*T) / D4  FXR  470
          DEL(I) = D4 / (PTSA(1,I,N)-T+E1(L,K))                         FXR  480
  107   CONTINUE                                                        FXR  490
        GOTO 125                                                        FXR  500
  108   IF(J-JVX) 119,109,109                                           FXR  510
  109   IF(P2(JVX,1,K)) 110,113,110                                     FXR  520
  110   BET(1) = (P2(JP1,1,K)*D1+SCAT(J,1)) / D4                        FXR  530
        L = NRGN(JVX,1)                                                 FXR  540
        DEL(1) = D4 / (PTSA(JVX,1,N)+E1(L,K))                           FXR  550
        GOTO(113,113,111,112),INRB                                      FXR  560
  111   BET(1) = BET(1) + P2(1,IVX,K) * D2 / D4                         FXR  570
        GOTO 113                                                        FXR  580
  112   BET(1) = BET(1) + P2(JVX,IVX,K) * D2 / D4                       FXR  590
  113   DO 118 I=2,IVX                                                  FXR  600
          IF(P2(JVX,I,K)) 115,114,115                                   FXR  610
  114     DEL(I) = 0.0                                                  FXR  620
          GOTO 118                                                      FXR  630
  115     T = D4 * DEL(I-1)                                             FXR  640
          L = NRGN(JVX,I)                                               FXR  650
          D4 = DCONB(J,I+1,N)                                           FXR  660
          BET(I) = (P2(JP1,I,K)*DCONR(JVX,I,N)+SCAT(JVX,I)+BET(I-1)*T) /FXR  670
     1     D4                                                           FXR  680
          DEL(I) = D4 / (PTSA(JVX,I,N)-T+E1(L,K))                       FXR  690
          GOTO(118,118,116,117),INRB                                    FXR  700
  116     BET(I) = BET(I) + P2(I,IVX,K) * DCONR(JVXP1,I,N) / D4         FXR  710
          GOTO 118                                                      FXR  720
  117     BET(I) = BET(I) + P2(JVX,IVXP1-I,K) * DCONR(JVXP1,I,N) / D4   FXR  730
  118   CONTINUE                                                        FXR  740
        GOTO 125                                                        FXR  750
  119   IF(P2(J,1,K)) 120,121,120                                       FXR  760
  120   CONTINUE                                                        FXR  770
        L = NRGN(J,1)                                                   FXR  780
        BET(1) = (P2(JP1,1,K)*D1+P2(JP2,1,K)*D2+SCAT(J,1)) / D4         FXR  790
        DEL(1) = D4 / (PTSA(J,1,N)+E1(L,K))                             FXR  800
  121   CONTINUE                                                        FXR  810
        DO 124 I=2,IVX                                                  FXR  820
          IF(P2(J,I,K)) 123,122,123                                     FXR  830
  122     DEL(I) = 0.0                                                  FXR  840
          GOTO 124                                                      FXR  850
  123     T = D4 * DEL(I-1)                                             FXR  860
          L = NRGN(J,I)                                                 FXR  870
          D4 = DCONB(J,I+1,N)                                           FXR  880
          BET(I) = (P2(JP1,I,K)*DCONR(J,I,N)+P2(JP2,I,K)*DCONR(JP2,I,N)+FXR  890
     1     SCAT(J,I)+BET(I-1)*T) / D4                                   FXR  900
          DEL(I) = D4 / (PTSA(J,I,N)-T+E1(L,K))                         FXR  910
  124   CONTINUE                                                        FXR  920
  125   CONTINUE                                                        FXR  930
        IF(INRB .NE. 3) GOTO 126                                        FXR  940
        IF(J .EQ. JVX) GOTO 126                                         FXR  950
        BET(IVX) = BET(IVX) + P2(JVX,J,K)                               FXR  960
  126   TEMP = BET(IVX) * DEL(IVX)                                      FXR  970
        T = P2(J,IVX,K)                                                 FXR  980
        TMF = T + VRGK2 * (TEMP-T)                                      FXR  990
        IF(IEP) 127,131,128                                             FXR 1000
  127   P2(J,IVX,K) = TEMP                                              FXR 1010
        GOTO 132                                                        FXR 1020
  128   IF(TMF-TEMP) 130,131,129                                        FXR 1030
  129   TMF = DMIN1(TMF,(TEMP+T))                                       FXR 1040
        GOTO 131                                                        FXR 1050
  130   TMF = DMAX1(TMF,0.5*TEMP)                                       FXR 1060
  131   CONTINUE                                                        FXR 1070
        P2(J,IVX,K) = TMF                                               FXR 1080
  132   DO 139 JJ=2,IVX                                                 FXR 1090
          I = IVXP1 - JJ                                                FXR 1100
          T = P2(J,I,K)                                                 FXR 1110
          TEMP = DEL(I) * (TEMP+BET(I))                                 FXR 1120
          TMF = T + VRGK2 * (TEMP-T)                                    FXR 1130
          IF(IEP) 133,137,134                                           FXR 1140
  133     P2(J,I,K) = TEMP                                              FXR 1150
          GOTO 139                                                      FXR 1160
  134     IF(TMF-TEMP) 136,137,135                                      FXR 1170
  135     TMF = DMIN1(TMF,(TEMP+T))                                     FXR 1180
          GOTO 137                                                      FXR 1190
  136     TMF = DMAX1(TMF,0.5*TEMP)                                     FXR 1200
  137     CONTINUE                                                      FXR 1210
          P2(J,I,K) = TMF                                               FXR 1220
  139   CONTINUE                                                        FXR 1230
        IF(NUAC(8)) 140,142,142                                         FXR 1240
  140   M = JVXP1 - J                                                   FXR 1250
        DO 141 I=1,IVX                                                  FXR 1260
          L = IVXP1 - I                                                 FXR 1270
          P2(M,L,K) = P2(J,I,K)                                         FXR 1280
  141   CONTINUE                                                        FXR 1290
  142 CONTINUE                                                          FXR 1300
      RETURN                                                            FXR 1310
      END                                                               FXR 1320
      SUBROUTINE FTRI(SCAT,P2,DCONB,DCONR,PTSA,IVX,JVX,KVX,IVXP1,JVXP1, FTR   10
     1 IVZ,KVZ,BET,DEL,NRGN,E1,LVX,IOVX,IOVZ)                           FTR   20
C                                                                       FTR   30
CFTRI 110.1  ***CITATION*** TRI. GEOM. LINE RELAX. 2-D/ CF-DNSD         FTR   40
C                                                                       FTR   50
      REAL*8 BET(211),DEL(211),T,TEMP,TMF,P2,SCAT                       FTR   60
C                                                                       FTR   70
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,FTR   80
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   FTR   90
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), FTR  100
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    FTR  110
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    FTR  120
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   FTR  130
     6 IXPUT(9999),XPUT(9999)                                           FTR  140
C                                                                       FTR  150
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   FTR  160
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKFTR  170
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    FTR  180
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  FTR  190
     4 ITMAX,ITIME,BAT(211),DAL(211)                                    FTR  200
C                                                                       FTR  210
      DIMENSION SCAT(JVX,IVX),P2(JVX,IVX,KVX),DCONB(JVX,IVXP1,IOVX),    FTR  220
     1 DCONR(JVXP1,IVZ,IOVZ),PTSA(JVX,IVX,IOVX),NRGN(JVX,IVX),          FTR  230
     2 E1(LVX,KVX)                                                      FTR  240
C                                                                       FTR  250
C                                                                       FTR  260
      INRB = IX(72) + 1                                                 FTR  270
      N = IX(20)                                                        FTR  280
      DO 143 I=1,IVX                                                    FTR  290
        IP1 = I - 1                                                     FTR  300
        IP2 = I + 1                                                     FTR  310
        DEL(1) = 0.0                                                    FTR  320
        D1 = DCONB(1,I,N)                                               FTR  330
        D4 = DCONR(2,I,N)                                               FTR  340
        IF(I-1) 102,102,108                                             FTR  350
  102   IF(P2(1,1,K)) 103,104,103                                       FTR  360
  103   BET(1) = SCAT(1,1) / D4                                         FTR  370
        DEL(1) = D4 / (PTSA(1,1,N)+E1(1,K))                             FTR  380
        IF(INRB .EQ. 2) BET(1) = BET(1) + P2(JVX,I,K) * DCONR(1,I,N) /  FTR  390
     1   D4                                                             FTR  400
  104   DO 107 J=2,JVX                                                  FTR  410
          IF(P2(J,1,K)) 106,105,106                                     FTR  420
  105     DEL(J) = 0.0                                                  FTR  430
          GOTO 107                                                      FTR  440
  106     T = D4 * DEL(J-1)                                             FTR  450
          L = NRGN(J,1)                                                 FTR  460
          D4 = DCONR(J+1,1,N)                                           FTR  470
          TEMP = P2(J-1,2,K) * DCONB(J-1,2,N)                           FTR  480
          IF(I .EQ. IMAX) TEMP = 0.0                                    FTR  490
          BET(J) = (TEMP+SCAT(J,1)+BET(J-1)*T) / D4                     FTR  500
          DEL(J) = D4 / (PTSA(J,1,N)-T+E1(L,K))                         FTR  510
  107   CONTINUE                                                        FTR  520
        GOTO 121                                                        FTR  530
  108   IF(I-IVX) 115,109,109                                           FTR  540
  109   IF(P2(1,IVX,K)) 110,111,110                                     FTR  550
  110   BET(1) = (P2(2,IP1,K)*D1+SCAT(1,IVX)) / D4                      FTR  560
        L = NRGN(1,IVX)                                                 FTR  570
        DEL(1) = D4 / (PTSA(1,IVX,N)+E1(L,K))                           FTR  580
        IF(INRB .EQ. 2) BET(1) = BET(1) + P2(JVX,I,K) * DCONR(1,I,N) /  FTR  590
     1   D4                                                             FTR  600
  111   DO 114 J=2,JVX                                                  FTR  610
          IF(P2(J,IVX,K)) 113,112,113                                   FTR  620
  112     DEL(J) = 0.0                                                  FTR  630
          GOTO 114                                                      FTR  640
  113     T = D4 * DEL(J-1)                                             FTR  650
          L = NRGN(J,IVX)                                               FTR  660
          D4 = DCONR(J+1,IVX,N)                                         FTR  670
          BET(J) = (P2(J+1,IP1,K)*DCONB(J,IVX,N)+SCAT(J,IVX)+BET(J-1)*T)FTR  680
     1     / D4                                                         FTR  690
          DEL(J) = D4 / (PTSA(J,IVX,N)-T+E1(L,K))                       FTR  700
          IF(INRB .NE. 3) GOTO 114                                      FTR  710
          IF(((J/2)*2) .NE. J) GOTO 114                                 FTR  720
          IF(J .EQ. JVX) GOTO 114                                       FTR  730
          BET(J) = BET(J) + P2(JVX,J/2,K) * DCONB(J-1,IVXP1,N) / D4     FTR  740
  114   CONTINUE                                                        FTR  750
        GOTO 121                                                        FTR  760
  115   IF(P2(1,I,K)) 116,117,116                                       FTR  770
  116   CONTINUE                                                        FTR  780
        L = NRGN(1,I)                                                   FTR  790
        BET(1) = (P2(2,IP1,K)*D1+SCAT(1,I)) / D4                        FTR  800
        DEL(1) = D4 / (PTSA(1,I,N)+E1(L,K))                             FTR  810
        IF(INRB .EQ. 2) BET(1) = BET(1) + P2(JVX,I,K) * DCONR(1,I,N) /  FTR  820
     1   D4                                                             FTR  830
  117   CONTINUE                                                        FTR  840
        DO 120 J=2,JVX                                                  FTR  850
          IF(P2(J,I,K)) 119,118,119                                     FTR  860
  118     DEL(J) = 0.0                                                  FTR  870
          GOTO 120                                                      FTR  880
  119     T = D4 * DEL(J-1)                                             FTR  890
          L = NRGN(J,I)                                                 FTR  900
          D4 = DCONR(J+1,I,N)                                           FTR  910
          BET(J) = (P2(J+1,IP1,K)*DCONB(J,I,N)+P2(J-1,IP2,K)*           FTR  920
     1     DCONB(J-1,IP2,N)+SCAT(J,I)+BET(J-1)*T) / D4                  FTR  930
          DEL(J) = D4 / (PTSA(J,I,N)-T+E1(L,K))                         FTR  940
  120   CONTINUE                                                        FTR  950
  121   CONTINUE                                                        FTR  960
        GOTO(125,122,123,124),INRB                                      FTR  970
  122   CONTINUE                                                        FTR  980
        BET(JVX) = BET(JVX) + P2(1,I,K)                                 FTR  990
        GOTO 125                                                        FTR 1000
  123   CONTINUE                                                        FTR 1010
        IF(I .EQ. IVX) GOTO 125                                         FTR 1020
        BET(JVX) = BET(JVX) + P2(2*I,IVX,K)                             FTR 1030
        GOTO 125                                                        FTR 1040
  124   CONTINUE                                                        FTR 1050
        BET(JVX) = BET(JVX) + P2(JVX,IVXP1-I,K)                         FTR 1060
  125   TEMP = BET(JVX) * DEL(JVX)                                      FTR 1070
        T = P2(JVX,I,K)                                                 FTR 1080
        TMF = T + BETTA * (TEMP-T)                                      FTR 1090
        IF(IEP) 126,130,127                                             FTR 1100
  126   P2(JVX,I,K) = TEMP                                              FTR 1110
        GOTO 131                                                        FTR 1120
  127   IF(TMF-TEMP) 129,130,128                                        FTR 1130
  128   TMF = DMIN1(TMF,(TEMP+T))                                       FTR 1140
        GOTO 130                                                        FTR 1150
  129   TMF = DMAX1(TMF,0.5*TEMP)                                       FTR 1160
  130   CONTINUE                                                        FTR 1170
        P2(JVX,I,K) = TMF                                               FTR 1180
  131   DO 138 JJ=2,JVX                                                 FTR 1190
          J = JVXP1 - JJ                                                FTR 1200
          T = P2(J,I,K)                                                 FTR 1210
          TEMP = DEL(J) * (TEMP+BET(J))                                 FTR 1220
          TMF = T + BETTA * (TEMP-T)                                    FTR 1230
          IF(IEP) 132,136,133                                           FTR 1240
  132     P2(J,I,K) = TEMP                                              FTR 1250
          GOTO 138                                                      FTR 1260
  133     IF(TMF-TEMP) 135,136,134                                      FTR 1270
  134     TMF = DMIN1(TMF,(TEMP+T))                                     FTR 1280
          GOTO 136                                                      FTR 1290
  135     TMF = DMAX1(TMF,0.5*TEMP)                                     FTR 1300
  136     CONTINUE                                                      FTR 1310
          P2(J,I,K) = TMF                                               FTR 1320
  138   CONTINUE                                                        FTR 1330
        IF(NUAC(8)) 139,143,141                                         FTR 1340
  139   L = IVXP1 - I                                                   FTR 1350
        DO 140 J=1,JVX                                                  FTR 1360
          M = JVXP1 - J                                                 FTR 1370
          P2(M,L,K) = P2(J,I,K)                                         FTR 1380
  140   CONTINUE                                                        FTR 1390
        GOTO 143                                                        FTR 1400
  141   DO 142 J=1,JVX                                                  FTR 1410
          P2(I,J,K) = P2(J,I,K)                                         FTR 1420
  142   CONTINUE                                                        FTR 1430
  143 CONTINUE                                                          FTR 1440
      RETURN                                                            FTR 1450
      END                                                               FTR 1460
      SUBROUTINE KNSD(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,SOURE,NRGNE, KNS   10
     1 XII,SCAC,P1E,E1,IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,LVX,JIVX,    KNS   20
     2 JIP1VX,JP1IXZ,IOVX,IOVZ,SPAR,BIEMS,NCRP,NSPA,SIG,PVOL,NCOMP,MVX, KNS   30
     3 AIO,IX3738,XLAMDA,XI,XL,B2,IOADJ,IOFS,KGP1)                      KNS   40
C                                                                       KNS   50
CKNSD --112 ***CITATION*** FLUX CALCULATION CONTROL FOR 3-D /CF-KLUX    KNS   60
C                                                                       KNS   70
      REAL*8 SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2S,XLL1,D8KNS   80
     1 ,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK,B5LK,D1, KNS   90
     2 D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,YRDB,SPR50KNS  100
     3 ,XLAST,XII,XLAMDA                                                KNS  110
C                                                                       KNS  120
      COMMON /ADUBP/ SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2SKNS  130
     1 ,XLL1,D8,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK, KNS  140
     2 B5LK,D1,D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,  KNS  150
     3 YRDB,SPR50,XLAST                                                 KNS  160
C                                                                       KNS  170
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KNS  180
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KNS  190
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KNS  200
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KNS  210
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KNS  220
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KNS  230
     6 IXPUT(9999),XPUT(9999)                                           KNS  240
C                                                                       KNS  250
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   KNS  260
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XELKKNS  270
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    KNS  280
     3 VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2,BETTA,SAMXI,IX25,IX28,I,J,KB,K,  KNS  290
     4 ITMAX,ITIME,BET(211),DEL(211)                                    KNS  300
C                                                                       KNS  310
      DIMENSION SCATE(JVX,IVX,KBVX),P2E(JIVX,KBVX,KVX),                 KNS  320
     1 DCONBE(JIP1VX,KBVX,IOVX),DCONRE(JP1IXZ,KBVX,IOVZ),               KNS  330
     2 DCONBK(JIVX,KBVXP1,IOVX),PTSAE(JIVX,KBVX,IOVX),                  KNS  340
     3 SOURE(JVX,IVX,KBVX),XII(KVX),SCAC(KVX,MVX,KVX),P1E(JIVX,KBVX),   KNS  350
     4 E1(LVX,KVX),NRGNE(JVX,IVX,KBVX),TSOUR(211),SPAR(NCRP,NSPA),      KNS  360
     5 BIEMS(KVX),SIG(KVX,MVX,10),PVOL(LVX),NCOMP(LVX),AIO(IX3738),     KNS  370
     6 XI(KVX),XL(6,KVX),B2(MVX,KVX)                                    KNS  380
C                                                                       KNS  390
CCCCC ********* SUBSCRIPT DEFINITIONS (KNSD E-061) ********* CCCCC      KNS  400
C    NEW         OLD            NEW         OLD                         KNS  410
C     N1         J,I                                                    KNS  420
C     N2         1,I             N5      JMAX,I                         KNS  430
C     N3 *       1,I             N7         J,IMXP1                     KNS  440
C     N4 *   JMXP1,I             N8         J,IMAX                      KNS  450
C                                 J         J,1                         KNS  460
C                                                                       KNS  470
C     INRB = 1  ORDINARY                                                KNS  480
C     INRB = 2  PERIODIC(REPEATING)                                     KNS  490
C     INRB = 3  90 DEGREE ROTATIONAL                                    KNS  500
C     INRB = 4  180 DEGREE ROTATIONAL                                   KNS  510
C                                                                       KNS  520
C                                                                       KNS  530
      INRB = IX(72) + 1                                                 KNS  540
      IO19 = IX(86)                                                     KNS  550
      IF(IX(135) .EQ. 1) REWIND IO19                                    KNS  560
      RMX = 1.0                                                         KNS  570
      RMN = 1.0                                                         KNS  580
      IX37 = IX(37)                                                     KNS  590
      DO 152 KT1=1,KMAX                                                 KNS  600
        IF(IX37 .EQ. 0) GOTO 102                                        KNS  610
        READ (IOADJ) AIO                                                KNS  620
        IF(IX(71) .GT. 0) GOTO 100                                      KNS  630
        K = KT1                                                         KNS  640
        GOTO 101                                                        KNS  650
  100   K = KGP1 - KT1                                                  KNS  660
  101   CONTINUE                                                        KNS  670
        IX(20) = 1                                                      KNS  680
        GOTO 105                                                        KNS  690
  102   CONTINUE                                                        KNS  700
        IF(IX(24) .GT. 0) GOTO 103                                      KNS  710
        K = KT1                                                         KNS  720
        GOTO 104                                                        KNS  730
  103   K = KGP1 - KT1                                                  KNS  740
  104   CONTINUE                                                        KNS  750
        IX(20) = K                                                      KNS  760
  105   CONTINUE                                                        KNS  770
        IF(IX(5) .EQ. -5) GOTO 112                                      KNS  780
        IF(IX(24) .EQ. 0) GOTO 106                                      KNS  790
        IF(IX(17) .GE. 1) GOTO 108                                      KNS  800
  106   CONTINUE                                                        KNS  810
        DO 107 L=1,LMAX                                                 KNS  820
          M = NCOMP(L)                                                  KNS  830
          E1(L,K) = XLAMDA * SIG(K,M,5) * PVOL(L)                       KNS  840
          IF(IX(24) .EQ. 0) GOTO 107                                    KNS  850
          IF((IX(17) .EQ. -2) .AND. (IX(71) .GT. 0)) E1(L,K) =          KNS  860
     1     SIG(K,M,5) * PVOL(L)                                         KNS  870
  107   CONTINUE                                                        KNS  880
  108   CONTINUE                                                        KNS  890
        IF(IX(24) .GT. 0) GOTO 111                                      KNS  900
C                                                                       KNS  910
C********SEARCH OPTIONS                                                 KNS  920
C                                                                       KNS  930
        IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 110                     KNS  940
        XII(K) = XI(K) / SPARE(50)                                      KNS  950
        GOTO 111                                                        KNS  960
  110   XII(K) = XI(K) * XLAMDA                                         KNS  970
  111   CONTINUE                                                        KNS  980
        GOTO 113                                                        KNS  990
  112   CONTINUE                                                        KNS 1000
        IF(IX(132) .GT. 0) READ (IOFS) SPAR                             KNS 1010
        BIEMS(K) = XLAMDA * XI(K)                                       KNS 1020
  113   CONTINUE                                                        KNS 1030
        NOINNR = NUAC(23)                                               KNS 1040
        IF(IX(24) .GT. 0) GOTO 114                                      KNS 1050
        KSCT1 = K - IX28                                                KNS 1060
        IF(KSCT1 .LE. 0) KSCT1 = 1                                      KNS 1070
        KSCT2 = MAX0((K-1),1)                                           KNS 1080
        IF(K .GE. KXMN8) KSCT2 = KVX                                    KNS 1090
C                                                                       KNS 1100
C     IRECV IS THE GROUP NO. WHICH CAN UPSCATTER TO GROUP 1. IT IS NOT  KNS 1110
C     BEING USED AND IS SET TO 0 IN KEGN.                               KNS 1120
C                                                                       KNS 1130
        IF(K .LT. IRECV) KSCT2 = IRECV                                  KNS 1140
        GOTO 115                                                        KNS 1150
  114   CONTINUE                                                        KNS 1160
        KSCT1 = K                                                       KNS 1170
        IF(K .GE. KXMN8) KSCT1 = KXMN8                                  KNS 1180
        KSCT2 = K + IX28                                                KNS 1190
        IF(KSCT2 .GT. KVX) KSCT2 = KVX                                  KNS 1200
        IF(K .LT. IRECV) KSCT1 = 1                                      KNS 1210
  115   CONTINUE                                                        KNS 1220
        DO 122 KB=1,KBVX                                                KNS 1230
          DO 121 I=1,IVX                                                KNS 1240
            NN1 = (I-1) * JVX                                           KNS 1250
            DO 120 J=1,JVX                                              KNS 1260
              N1 = NN1 + J                                              KNS 1270
              L = NRGNE(J,I,KB)                                         KNS 1280
              M = NCOMP(L)                                              KNS 1290
              P1E(N1,KB) = P2E(N1,KB,K)                                 KNS 1300
              IF(IX(24) .GT. 0) GOTO 117                                KNS 1310
              CKSS = 0.0                                                KNS 1320
              DO 116 KK=KSCT1,KSCT2                                     KNS 1330
                CKSS = CKSS + SCAC(KK,M,K) * P2E(N1,KB,KK)              KNS 1340
  116         CONTINUE                                                  KNS 1350
              SCATE(J,I,KB) = CKSS * PVOL(L) + SOURE(J,I,KB) * XII(K)   KNS 1360
              GOTO 119                                                  KNS 1370
  117         CKSS = SOURE(J,I,KB) * SIG(K,M,4)                         KNS 1380
              DO 118 KK= KSCT1,KSCT2                                    KNS 1390
                CKSS = CKSS + SCAC(K,M,KK) * P2E(N1,KB,KK)              KNS 1400
  118         CONTINUE                                                  KNS 1410
              SCATE(J,I,KB) = CKSS * PVOL(L)                            KNS 1420
  119         CONTINUE                                                  KNS 1430
  120       CONTINUE                                                    KNS 1440
  121     CONTINUE                                                      KNS 1450
  122   CONTINUE                                                        KNS 1460
        IF(IX(5) .NE. -5) GOTO 126                                      KNS 1470
        BM = BIEMS(K)                                                   KNS 1480
        NP = 0                                                          KNS 1490
        DO 125 KB=1,KBVX                                                KNS 1500
          DO 124 I=1,IVX                                                KNS 1510
            DO 123 J=1,JVX                                              KNS 1520
              NP = NP + 1                                               KNS 1530
              SCATE(J,I,KB) = SCATE(J,I,KB) + BM * SPAR(NP,1)           KNS 1540
  123       CONTINUE                                                    KNS 1550
  124     CONTINUE                                                      KNS 1560
  125   CONTINUE                                                        KNS 1570
  126   CONTINUE                                                        KNS 1580
        IF(IX(135) .EQ. 1) WRITE (IO19) P1E                             KNS 1590
        DO 130 INNR=1,NOINNR                                            KNS 1600
          IF(NUAC(5) .EQ. 14) GOTO 129                                  KNS 1610
          IF(IX(72) .EQ. 1) GOTO 128                                    KNS 1620
          IF(NUAC(5) .EQ. 13) GOTO 127                                  KNS 1630
C                                                                       KNS 1640
          CALL KWRD(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR,NRGNE,E1,KNS 1650
     1     LVX,IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,JIVX,JIP1VX,JP1IXZ,  KNS 1660
     2     IOVX,IOVZ)                                                   KNS 1670
C                                                                       KNS 1680
          IF(IX(72) .GT. 1) GOTO 130                                    KNS 1690
          IF(NUAC(20) .GT. -1) GOTO 130                                 KNS 1700
C                                                                       KNS 1710
          CALL KXRD(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR,NRGNE,E1,KNS 1720
     1     LVX,IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,JIVX,JIP1VX,JP1IXZ,  KNS 1730
     2     IOVX,IOVZ)                                                   KNS 1740
C                                                                       KNS 1750
          IF(NUAC(20) .NE. -1) GOTO 130                                 KNS 1760
C                                                                       KNS 1770
          CALL KZRD(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR,NRGNE,E1,KNS 1780
     1     LVX,IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,JIVX,JIP1VX,JP1IXZ,  KNS 1790
     2     IOVX,IOVZ)                                                   KNS 1800
C                                                                       KNS 1810
          GOTO 130                                                      KNS 1820
  127     CONTINUE                                                      KNS 1830
C                                                                       KNS 1840
          CALL MWRD(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR,NRGNE,E1,KNS 1850
     1     LVX,IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,JIVX,JIP1VX,JP1IXZ,  KNS 1860
     2     IOVX,IOVZ)                                                   KNS 1870
C                                                                       KNS 1880
          GOTO 130                                                      KNS 1890
C                                                                       KNS 1900
  128     CALL KPER(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR,NRGNE,E1,KNS 1910
     1     LVX,IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,JIVX,JIP1VX,JP1IXZ,  KNS 1920
     2     IOVX,IOVZ)                                                   KNS 1930
C                                                                       KNS 1940
          GOTO 130                                                      KNS 1950
  129     CONTINUE                                                      KNS 1960
C                                                                       KNS 1970
          CALL KTRI(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR,NRGNE,E1,KNS 1980
     1     LVX,IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,JIVX,JIP1VX,JP1IXZ,  KNS 1990
     2     IOVX,IOVZ)                                                   KNS 2000
C                                                                       KNS 2010
  130   CONTINUE                                                        KNS 2020
        IF(NUAC(3) .GT. 0 .OR. IX(24) .GT. 0) GOTO 147                  KNS 2030
        XLL1 = 0.0                                                      KNS 2040
        XLL2 = 0.0                                                      KNS 2050
        XLL3 = 0.0                                                      KNS 2060
        XLL4 = 0.0                                                      KNS 2070
        XLL5 = 0.0                                                      KNS 2080
        XLL6 = 0.0                                                      KNS 2090
        N = IX(20)                                                      KNS 2100
        NN3 = (IMXP1-1) * JVX                                           KNS 2110
        NN4 = (IMAX-1) * JVX                                            KNS 2120
        DO 131 M=1,MMAX                                                 KNS 2130
          B2(M,K) = 0.0                                                 KNS 2140
  131   CONTINUE                                                        KNS 2150
        DO 137 KB=1,KBMAX                                               KNS 2160
          DO 133 I=1,IMAX                                               KNS 2170
            N2 = (I-1) * JVX                                            KNS 2180
            N5 = N2 + JMAX                                              KNS 2190
            N2 = N2 + 1                                                 KNS 2200
            N3 = (I-1) * JVXP1                                          KNS 2210
            N4 = N3 + JMXP1                                             KNS 2220
            N3 = N3 + 1                                                 KNS 2230
            XLL1 = XLL1 + P2E(N2,KB,K) * DCONRE(N3,KB,N)                KNS 2240
            T1 = DCONRE(N4,KB,N)                                        KNS 2250
            IF(T1-4096.0E-13) 132,133,132                               KNS 2260
  132       XLL3 = XLL3 + P2E(N5,KB,K) * T1                             KNS 2270
  133     CONTINUE                                                      KNS 2280
          DO 136 J=1,JMAX                                               KNS 2290
            N7 = NN3 + J                                                KNS 2300
            N8 = NN4 + J                                                KNS 2310
            XLL2 = XLL2 + P2E(J,KB,K) * DCONBE(J,KB,N)                  KNS 2320
            IF(NUAC(5) .NE. 14) GOTO 134                                KNS 2330
            IF(J .EQ. 1) GOTO 136                                       KNS 2340
            N7 = N7 - 1                                                 KNS 2350
  134       CONTINUE                                                    KNS 2360
            T1 = DCONBE(N7,KB,N)                                        KNS 2370
            IF(T1-4096.0E-13) 135,136,135                               KNS 2380
  135       XLL4 = XLL4 + P2E(N8,KB,K) * T1                             KNS 2390
  136     CONTINUE                                                      KNS 2400
  137   CONTINUE                                                        KNS 2410
        N1 = 0                                                          KNS 2420
        DO 140 I=1,IMAX                                                 KNS 2430
          DO 139 J=1,JMAX                                               KNS 2440
            N1 = N1 + 1                                                 KNS 2450
            XLL5 = XLL5 + P2E(N1,1,K) * DCONBK(N1,1,N)                  KNS 2460
            T1 = DCONBK(N1,KBMXP1,N)                                    KNS 2470
            IF(T1-4096.0E-13) 138,139,138                               KNS 2480
  138       XLL6 = XLL6 + P2E(N1,KBMAX,K) * T1                          KNS 2490
  139     CONTINUE                                                      KNS 2500
  140   CONTINUE                                                        KNS 2510
        GOTO(141,142,143,144),INRB                                      KNS 2520
  141   XLEK = XLEK + XLL1 + XLL2 + XLL3 + XLL4 + XLL5 + XLL6           KNS 2530
        GOTO 145                                                        KNS 2540
  142   XLEK = XLEK + XLL2 + XLL4 + XLL5 + XLL6                         KNS 2550
        XLL1 = XLL1 - XLL3                                              KNS 2560
        XLL3 = -XLL1                                                    KNS 2570
        GOTO 145                                                        KNS 2580
  143   XLEK = XLEK + XLL1 + XLL2 + XLL5 + XLL6                         KNS 2590
        XLL4 = -XLL3                                                    KNS 2600
        GOTO 145                                                        KNS 2610
  144   XLEK = XLEK + XLL1 + XLL2 + XLL4 + XLL5 + XLL6                  KNS 2620
        XLL3 = 0.0                                                      KNS 2630
  145   CONTINUE                                                        KNS 2640
        XL(1,K) = XLL1                                                  KNS 2650
        XL(2,K) = XLL2                                                  KNS 2660
        XL(3,K) = XLL3                                                  KNS 2670
        XL(4,K) = XLL4                                                  KNS 2680
        XL(5,K) = XLL5                                                  KNS 2690
        XL(6,K) = XLL6                                                  KNS 2700
        IF(NUAC(17) .LE. 0) GOTO 146                                    KNS 2710
C                                                                       KNS 2720
        CALL KINS(P2E,B2,NRGNE,DCONBE,DCONRE,DCONBK,SCAC,XL,IVX,JVX,KBVXKNS 2730
     1   ,KVX,LVX,JVXP1,KBVXP1,JIVX,JIP1VX,JP1IXZ,IOVX,IOVZ,PVOL,NCOMP, KNS 2740
     2   MVX,IVXP1)                                                     KNS 2750
C                                                                       KNS 2760
  146   CONTINUE                                                        KNS 2770
  147   CONTINUE                                                        KNS 2780
        DO 151 KB=1,KBMAX                                               KNS 2790
          N1 = 0                                                        KNS 2800
          DO 150 I=1,IMAX                                               KNS 2810
            DO 149 J=1,JMAX                                             KNS 2820
              N1 = N1 + 1                                               KNS 2830
              TT1 = P1E(N1,KB)                                          KNS 2840
              T2 = P2E(N1,KB,K)                                         KNS 2850
              IF(TT1 .EQ. 0.0) GOTO 148                                 KNS 2860
              RATO = T2 / TT1                                           KNS 2870
              RMX = AMAX1(RMX,RATO)                                     KNS 2880
              RMN = AMIN1(RMN,RATO)                                     KNS 2890
  148         CONTINUE                                                  KNS 2900
  149       CONTINUE                                                    KNS 2910
  150     CONTINUE                                                      KNS 2920
  151   CONTINUE                                                        KNS 2930
  152 CONTINUE                                                          KNS 2940
      IF(IX(135) .EQ. 1) REWIND IO19                                    KNS 2950
      RETURN                                                            KNS 2960
      END                                                               KNS 2970
      SUBROUTINE KINS(P2E,B2,NRGNE,DCONBE,DCONRE,DCONBK,SCAC,XL,IVX,JVX,KIN   10
     1 KBVX,KVX,LVX,JVXP1,KBVXP1,JIVX,JIP1VX,JP1IXZ,IOVX,IOVZ,PVOL,NCOMPKIN   20
     2 ,MVX,IVXP1)                                                      KIN   30
C                                                                       KIN   40
CKINS --115 ***CITATION*** CALC. ROD LOSSES FOR 3D  /CF-KNSD            KIN   50
C                                                                       KIN   60
      REAL*8 SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2S,XLL1,D8KIN   70
     > ,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK,B5LK,D1, KIN   80
     > D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,YRDB,SPR50KIN   90
     > ,XLAST                                                           KIN  100
C                                                                       KIN  110
      COMMON /ADUBP/ SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2SKIN  120
     1 ,XLL1,D8,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK, KIN  130
     2 B5LK,D1,D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,  KIN  140
     3 YRDB,SPR50,XLAST                                                 KIN  150
C                                                                       KIN  160
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KIN  170
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KIN  180
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KIN  190
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KIN  200
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KIN  210
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KIN  220
     6 IXPUT(9999),XPUT(9999)                                           KIN  230
C                                                                       KIN  240
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   KIN  250
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XELKKIN  260
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    KIN  270
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SAMXI,IX25,IX28,I,J,KB,K,  KIN  280
     4 ITMAX,ITIME,BET(211),DEL(211)                                    KIN  290
C                                                                       KIN  300
      DIMENSION P2E(JIVX,KBVX,KVX),B2(MVX,KVX),NRGNE(JVX,IVX,KBVX),     KIN  310
     1 DCONBE(JIP1VX,KBVX,IOVX),DCONRE(JP1IXZ,KBVX,IOVZ),               KIN  320
     2 DCONBK(JIVX,KBVXP1,IOVX),SCAC(KVX,MVX,KVX),XL(6,KVX),PVOL(LVX),  KIN  330
     3 NCOMP(LVX)                                                       KIN  340
C                                                                       KIN  350
CCCCC ********* SUBSCRIPT DEFINITIONS (KINS E-110) ********* CCCCC      KIN  360
C    NEW         OLD            NEW         OLD                         KIN  370
C     N1         J,I             N6       J-1,I+1                       KIN  380
C     N2         J,I-1           N7       J+1,I-1                       KIN  390
C     N3         J,I+1           N8 *       J,I                         KIN  400
C     N4       J-1,I             N9 *       J,I+1                       KIN  410
C     N5       J+1,I             N10 *    J+1,I                         KIN  420
C     N11        1,I             N12      JVX,I                         KIN  430
C     N13      JVX,J             N14        I,IVX                       KIN  440
C     N15      JVX,IVXP1-I                                              KIN  450
C                                                                       KIN  460
C     INRB = 1  ORDINARY                                                KIN  470
C     INRB = 2  PERIODIC(REPEATING)                                     KIN  480
C     INRB = 3  90 DEGREE ROTATIONAL                                    KIN  490
C     INRB = 4  180 DEGREE ROTATIONAL                                   KIN  500
C                                                                       KIN  510
C                                                                       KIN  520
      INRB = IX(72) + 1                                                 KIN  530
      N = IX(20)                                                        KIN  540
      DO 146 KB=1,KBMAX                                                 KIN  550
        DO 145 I=1,IMAX                                                 KIN  560
          NN1= (I-1) * JVX                                              KIN  570
          NN2= (I-1) * JVXP1                                            KIN  580
          DO 144 J=1,JMAX                                               KIN  590
            N1 = NN1 + J                                                KIN  600
            N2 = N1 - JVX                                               KIN  610
            N3 = N1 + JVX                                               KIN  620
            N4 = N1 - 1                                                 KIN  630
            N5 = N1 + 1                                                 KIN  640
            N6 = N3 - 1                                                 KIN  650
            N7 = N2 + 1                                                 KIN  660
            N8 = NN2 + J                                                KIN  670
            N9 = N8 + JVXP1                                             KIN  680
            N10 = N8 + 1                                                KIN  690
            L = NRGNE(J,I,KB)                                           KIN  700
            M = NCOMP(L)                                                KIN  710
            IF(P2E(N1,KB,K)) 103,101,103                                KIN  720
  101       IF(XMIS(2) .GE. 0) GOTO 143                                 KIN  730
            TT5 = 0.0                                                   KIN  740
            DO 102 KK=1,KMAX                                            KIN  750
              TT5 = TT5 + SCAC(KK,M,K) * P2E(N1,KB,KK)                  KIN  760
  102       CONTINUE                                                    KIN  770
            B2(M,K) = B2(M,K) + TT5 * PVOL(L)                           KIN  780
            GOTO 143                                                    KIN  790
  103       IF(KB-1) 106,106,104                                        KIN  800
  104       IF(P2E(N1,KB-1,K)) 106,105,106                              KIN  810
  105       LF = NRGNE(J,I,KB-1)                                        KIN  820
            MF = NCOMP(LF)                                              KIN  830
            B2(MF,K) = B2(MF,K) + P2E(N1,KB,K) * DCONBK(N1,KB,N)        KIN  840
  106       IF(KB-KBMAX) 107,110,110                                    KIN  850
  107       IF(P2E(N1,KB+1,K)) 110,108,110                              KIN  860
  108       LBK = NRGNE(J,I,KB+1)                                       KIN  870
            MBK = NCOMP(LBK)                                            KIN  880
            T1 = DCONBK(N1,KB+1,N)                                      KIN  890
            IF(T1-4096.0E-13) 109,110,109                               KIN  900
  109       B2(MBK,K) = B2(MBK,K) + P2E(N1,KB,K) * T1                   KIN  910
  110       IF(I-1) 114,114,111                                         KIN  920
  111       CONTINUE                                                    KIN  930
            IF(NUAC(5) .NE. 14) GOTO 112                                KIN  940
            IF(P2E(N7,KB,K) .NE. 0) GOTO 114                            KIN  950
            LT = NRGNE(J+1,I-1,KB)                                      KIN  960
            GOTO 113                                                    KIN  970
  112       CONTINUE                                                    KIN  980
            IF(P2E(N2,KB,K) .NE. 0) GOTO 114                            KIN  990
            LT = NRGNE(J,I-1,KB)                                        KIN 1000
  113       CONTINUE                                                    KIN 1010
            MT = NCOMP(LT)                                              KIN 1020
            B2(MT,K) = B2(MT,K) + P2E(N1,KB,K) * DCONBE(N1,KB,N)        KIN 1030
  114       IF(I-IMAX) 117,115,115                                      KIN 1040
  115       IF(INRB .NE. 3) GOTO 122                                    KIN 1050
            N13 = J * JVX                                               KIN 1060
            LB = NRGNE(JVX,J,KB)                                        KIN 1070
            IF(NUAC(5) .NE. 14) GOTO 116                                KIN 1080
            IF((J/2)*2 .NE. J) GOTO 122                                 KIN 1090
            N13 = N13 / 2                                               KIN 1100
            LB = NRGNE(JVX,J/2,KB)                                      KIN 1110
  116       CONTINUE                                                    KIN 1120
            IF(P2E(N13,KB,K) .NE. 0.0) GOTO 122                         KIN 1130
            MB = NCOMP(LB)                                              KIN 1140
            T1 = DCONBE(N3,KB,N)                                        KIN 1150
            GOTO 120                                                    KIN 1160
  117       CONTINUE                                                    KIN 1170
            IF(NUAC(5) .NE. 14) GOTO 118                                KIN 1180
            IF(J .EQ. 1) GOTO 122                                       KIN 1190
            IF(P2E(N6,KB,K) .NE. 0.) GOTO 122                           KIN 1200
            LB = NRGNE(J-1,I+1,KB)                                      KIN 1210
            N3 = N3 - 1                                                 KIN 1220
            GOTO 119                                                    KIN 1230
  118       CONTINUE                                                    KIN 1240
            IF(P2E(N3,KB,K) .NE. 0.) GOTO 122                           KIN 1250
            LB = NRGNE(J,I+1,KB)                                        KIN 1260
  119       CONTINUE                                                    KIN 1270
            MB = NCOMP(LB)                                              KIN 1280
            T1 = DCONBE(N3,KB,N)                                        KIN 1290
  120       IF(T1-4096.0E-13) 121,122,121                               KIN 1300
  121       B2(MB,K) = B2(MB,K) + P2E(N1,KB,K) * T1                     KIN 1310
  122       IF(J .LE. 1) GOTO 123                                       KIN 1320
            IF(P2E(N4,KB,K) .NE. 0.0) GOTO 125                          KIN 1330
            LL = NRGNE(J-1,I,KB)                                        KIN 1340
            ML = NCOMP(LL)                                              KIN 1350
            GOTO 124                                                    KIN 1360
  123       IF(INRB .NE. 2) GOTO 125                                    KIN 1370
            N12 = NN1 + JVX                                             KIN 1380
            IF(P2E(N12,KB,K) .NE. 0.0) GOTO 125                         KIN 1390
            LL = NRGNE(JVX,I,KB)                                        KIN 1400
            ML = NCOMP(LL)                                              KIN 1410
  124       B2(ML,K) = B2(ML,K) + P2E(N1,KB,K) * DCONRE(N8,KB,N)        KIN 1420
  125       IF(J .GE. JVX) GOTO 126                                     KIN 1430
            IF(P2E(N5,KB,K) .NE. 0.0) GOTO 133                          KIN 1440
            LR = NRGNE(J+1,I,KB)                                        KIN 1450
            MR = NCOMP(LR)                                              KIN 1460
            T1 = DCONRE(N10,KB,N)                                       KIN 1470
            GOTO 131                                                    KIN 1480
  126       GOTO(133,127,128,130),INRB                                  KIN 1490
  127       CONTINUE                                                    KIN 1500
            N11 = NN1 + 1                                               KIN 1510
            IF(P2E(N11,KB,K) .NE. 0.0) GOTO 133                         KIN 1520
            LR = NRGNE(1,I,KB)                                          KIN 1530
            MR = NCOMP(LR)                                              KIN 1540
            T1 = DCONRE(N10,KB,N)                                       KIN 1550
            GOTO 131                                                    KIN 1560
  128       CONTINUE                                                    KIN 1570
            N14 = (IVX-1) * JVX + I                                     KIN 1580
            LR = NRGNE(I,IVX,KB)                                        KIN 1590
            IF(NUAC(5) .NE. 14) GOTO 129                                KIN 1600
            N14 = N14 + I                                               KIN 1610
            LR = NRGNE(2*I,IVX,KB)                                      KIN 1620
  129       CONTINUE                                                    KIN 1630
            IF(P2E(N14,KB,K) .NE. 0.0) GOTO 133                         KIN 1640
            MR = NCOMP(LR)                                              KIN 1650
            T1 = DCONRE(N10,KB,N)                                       KIN 1660
            GOTO 131                                                    KIN 1670
  130       CONTINUE                                                    KIN 1680
            N15 = (IVXP1-I) * JVX                                       KIN 1690
            IF(P2E(N15,KB,K) .NE. 0.0) GOTO 133                         KIN 1700
            LR = NRGNE(JVX,IVXP1-I,KB)                                  KIN 1710
            MR = NCOMP(LR)                                              KIN 1720
            T1 = DCONRE(N10,KB,N)                                       KIN 1730
  131       IF(T1 .EQ. 4096.0E-13) GOTO 133                             KIN 1740
            B2(MR,K) = B2(MR,K) + P2E(N1,KB,K) * T1                     KIN 1750
  133       IF(NUAC(5)-13) 143,134,143                                  KIN 1760
  134       KKK = N + IOVX                                              KIN 1770
            IF(I-1) 138,138,135                                         KIN 1780
  135       IF(J-JMAX) 136,138,138                                      KIN 1790
  136       IF(P2E(N7,KB,K)) 138,137,138                                KIN 1800
  137       LB = NRGNE(J+1,I-1,KB)                                      KIN 1810
            MB = NCOMP(LB)                                              KIN 1820
            B2(MB,K) = B2(MB,K) + P2E(N1,KB,K) * DCONRE(N10,KB,KKK)     KIN 1830
  138       IF(J-1) 142,142,139                                         KIN 1840
  139       IF(I-IMAX) 140,142,142                                      KIN 1850
  140       IF(P2E(N6,KB,K)) 142,141,142                                KIN 1860
  141       LB = NRGNE(J-1,I+1,KB)                                      KIN 1870
            MB = NCOMP(LB)                                              KIN 1880
            B2(MB,K) = B2(MB,K) + P2E(N1,KB,K) * DCONRE(N9,KB,KKK)      KIN 1890
  142       CONTINUE                                                    KIN 1900
  143       CONTINUE                                                    KIN 1910
  144     CONTINUE                                                      KIN 1920
  145   CONTINUE                                                        KIN 1930
  146 CONTINUE                                                          KIN 1940
      IF(NUAC(17) .EQ. 0) GOTO 149                                      KIN 1950
      DO 148 M=1,MMAX                                                   KIN 1960
        XRDB = XRDB + B2(M,K)                                           KIN 1970
  148 CONTINUE                                                          KIN 1980
  149 CONTINUE                                                          KIN 1990
      RETURN                                                            KIN 2000
      END                                                               KIN 2010
      SUBROUTINE KNST(NRGNE,SCAC,DCONBE,DCONRE,DCONBK,F1,SIG,PTSAE,NCOMPNST   10
     1 ,PVOL,BBND,BND,IVX,JVX,KBVX,KVX,LVX,MVX,IVXP1,JVXP1,KBVXP1,IVZ,  NST   20
     2 KVZ,JIVX,JIP1VX,JP1IXZ,IOVX,IOVZ,A,MEMORY,AIO,IX3738)            NST   30
C                                                                       NST   40
CKNST --117 ***CITATION*** EQ. CONSTANTS FOR 3-D/ CF-EIGN               NST   50
C                                                                       NST   60
      REAL*8 TPTSA,D1,D2,D3,D4,D5,D6,D7,D8,D9,DU                        NST   70
C                                                                       NST   80
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,NST   90
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   NST  100
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), NST  110
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    NST  120
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    NST  130
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   NST  140
     6 IXPUT(9999),XPUT(9999)                                           NST  150
C                                                                       NST  160
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  NST  170
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), NST  180
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)NST  190
     3 ,PDK(211)                                                        NST  200
C                                                                       NST  210
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   NST  220
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKNST  230
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    NST  240
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  NST  250
     4 ITMAX,ITIME,BET(211),DEL(211)                                    NST  260
C                                                                       NST  270
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           NST  280
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    NST  290
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),NST  300
     3 NCLASS(9999),NDP(9999)                                           NST  310
C                                                                       NST  320
CFZJ055                                                       25.09.07  NST  330
C                                                                       NST  340
      COMMON /MU/ MU4                                                   NST  350
C                                                                       NST  360
CFZJ031                                                       28.05.04  NST  370
      DIMENSION NRGNE(JVX,IVX,KBVX),SCAC(KVX,MVX,KVX),                  NST  380
     1 DCONBE(JIP1VX,KBVX,IOVX),DCONRE(JP1IXZ,KBVX,IOVZ),               NST  390
     2 DCONBK(JIVX,KBVXP1,IOVX),F1(KVX,MVX),SIG(KVX,MVX,10),            NST  400
     3 PTSAE(JIVX,KBVX,IOVX),NCOMP(LVX),PVOL(LVX),BBND(KVX),BND(6,KVX), NST  410
     4 A(MEMORY),AIO(IX3738),B(3,33,9999)                               NST  420
C                                                                       NST  430
C     ********* SUBSCRIPT DEFINITIONS (KNST E-010) *********            NST  440
C    NEW         OLD            NEW         OLD                         NST  450
C     N1         J,I             N5 *       J,I+1                       NST  460
C     N2         J,I+1           N6         J,I                         NST  470
C     N3 *       J,I             N7         M,L                         NST  480
C     N4 *     J+1,I             N8         I,J                         NST  490
C     N9 *       1,I             N10 *  JVX+1,I                         NST  500
C     N11 *  JVXP1,IVXP1-I       N12 *  JVXP1,I                         NST  510
C     N14        J,IVXP1         N15 *      1,I                         NST  520
C     N2       J-1,I+1 FOR TRIANGULAR ONLY                              NST  530
C                                                                       NST  540
C     THE * ABOVE REFERS TO INDEXING IN ARRAYS LARGER THAN JVX X IVX    NST  550
C                                                                       NST  560
C     INRB = 1  ORDINARY                                                NST  570
C     INRB = 2  PERIODIC(REPEATING)                                     NST  580
C     INRB = 3  90 DEGREE ROTATIONAL                                    NST  590
C     INRB = 4  180 DEGREE ROTATIONAL                                   NST  600
C                                                                       NST  610
C     HIER WIRD DIE BEHRENSKORREKTUR GEMACHT                            NST  620
C                                                                       NST  630
C     B(1,..,..) ENTSPRICHT X-RICHTUNG UND INDEX J                      NST  640
C     B(2,..,..) ENTSPRICHT Y-RICHTUNG UND INDEX I                      NST  650
C     B(3,..,..) ENTSPRICHT Z-RICHTUNG UND INDEX KB                     NST  660
C     B(..,K,L) IST D(BEHRENSKORRIGIERT))/D(UNKORR.) IN GRUPPE K UND    NST  670
C                                                    ZONE L             NST  680
C                                                                       NST  690
C                                                                       NST  700
      IF(KMAX .GT. 33) WRITE (IOUT,1002)                                NST  710
      IF(MMAX .GT. 9999) WRITE (IOUT,1003)                              NST  720
      IF(KMAX .GT. 33) STOP                                             NST  730
      IF(MMAX .GT. 9999) STOP                                           NST  740
      DO 300 I=1,3                                                      NST  750
        DO 300 K=1,KMAX                                                 NST  760
          DO 300 M=1,MMAX                                               NST  770
            B(I,K,M) = 1.0                                              NST  780
  300 CONTINUE                                                          NST  790
C                                                                       NST  800
C     EINLESEN VON B                                                    NST  810
C     IRICHT  IRIC                                                      NST  820
C       0           EIN WERT FUER X UND Y UND Z                         NST  830
C       1       1   NUR X                                               NST  840
C       1       2         ERST X DANN Y                                 NST  850
C       2       2   NUR Y                                               NST  860
C       1       3   ERST X , DANN Y , DANN Z                            NST  870
C       2       3   ERST Y , DANN Z , X BLEIBT MIT FAKTOR 1.0           NST  880
C       ETC...  ETC...                                                  NST  890
C                                                                       NST  900
      ISCHR = 1                                                         NST  910
      IF(NGC(24)) 305,320,320                                           NST  920
  305 READ (IOIN,1004) JZMI,JZMA,IRICHT,IRIC                            NST  930
      IF(JZMI) 310,320,330                                              NST  940
  330 IF(IRICHT .GT. 3 .OR. IRICHT .LT. 0) GOTO 310                     NST  950
      IF(IRIC .GT. 3 .OR. IRIC .LT. 0) GOTO 310                         NST  960
      IF(IRIC .LT. IRICHT) GOTO 310                                     NST  970
      IF(JZMI .LE. JZMA) GOTO 331                                       NST  980
  310 WRITE (IOUT,1005) JZMI,JZMA,IRICHT,IRIC                           NST  990
      STOP                                                              NST 1000
  331 IF(IRICHT .GT. 0) GOTO 401                                        NST 1010
      READ (IOIN,1006) (B(1,K,JZMI),K=1,KMAX)                           NST 1020
      GOTO 402                                                          NST 1030
  401 DO 335 I=IRICHT,IRIC                                              NST 1040
        READ (IOIN,1006) (B(I,K,JZMI),K=1,KMAX)                         NST 1050
  335 CONTINUE                                                          NST 1060
  402 DO 340 I=1,3                                                      NST 1070
        II = I                                                          NST 1080
        IF(IRICHT .EQ. 0) II = 1                                        NST 1090
        DO 340 J=JZMI,JZMA                                              NST 1100
          DO 340 K=1,KMAX                                               NST 1110
            B(I,K,J) = B(II,K,JZMI)                                     NST 1120
  340 CONTINUE                                                          NST 1130
      IF(ISCHR .EQ. 1 .AND. MU4 .EQ. 1) WRITE (IOUT,1007)               NST 1140
      ISCHR = 0                                                         NST 1150
      IF(MU4 .EQ. 1) WRITE (IOUT,1008) JZMI,JZMA                        NST 1160
      DO 350 K=1,KMAX                                                   NST 1170
        IF(MU4 .EQ. 1) WRITE (IOUT,1009) K,B(1,K,JZMI),B(2,K,JZMI),     NST 1180
     1   B(3,K,JZMI)                                                    NST 1190
  350 CONTINUE                                                          NST 1200
      GOTO 305                                                          NST 1210
  320 CONTINUE                                                          NST 1220
      IF(NUAC(5) .NE. 13 .AND. NUAC(5) .NE. 14) GOTO 500                NST 1230
      DO 510 K=1,KMAX                                                   NST 1240
        DO 510 M=1,MMAX                                                 NST 1250
          IF(B(1,K,M) .EQ. B(2,K,M)) GOTO 510                           NST 1260
          WRITE (IOUT,1101) NUAC(5)                                     NST 1270
          STOP                                                          NST 1280
  510 CONTINUE                                                          NST 1290
  500 CONTINUE                                                          NST 1300
      INRB = IX(72) + 1                                                 NST 1310
      KMAXP1 = KMAX + 1                                                 NST 1320
      IX37 = IX(37)                                                     NST 1330
      IX38 = IX(38)                                                     NST 1340
      IO15 = IX(82)                                                     NST 1350
      IOADJ = IO15                                                      NST 1360
      IF(IX(71) .GT. 0) IOADJ = IO2                                     NST 1370
      IF(IX37 .GT. 0) REWIND IOADJ                                      NST 1380
      IF(IX(71) .GT. 0) IX(5) = IX(17)                                  NST 1390
      IO14 = IX(81)                                                     NST 1400
      REWIND IO14                                                       NST 1410
      GOTO(107,107,102,100),INRB                                        NST 1420
  100 I = IVX / 2                                                       NST 1430
      DO 101 J=1,I                                                      NST 1440
        K = IVX + 1 - J                                                 NST 1450
        TAL = (YY(J)-Y(J)) / (Y(K)-YY(K+1))                             NST 1460
        IF(NUAC(5) .EQ. 14) TAL = (PDI(I)-YY(I)) / (YY(K+1)-Y(K))       NST 1470
        EPICH = ABS(TAL-1.0)                                            NST 1480
        IF(EPICH .GT. 1.0E-4) GOTO 103                                  NST 1490
  101 CONTINUE                                                          NST 1500
      GOTO 107                                                          NST 1510
  102 CONTINUE                                                          NST 1520
      IDA = IVX                                                         NST 1530
      IF(NUAC(5) .EQ. 14) IDA = 2 * IVX                                 NST 1540
      IF(JVX .EQ. IDA) GOTO 104                                         NST 1550
  103 CONTINUE                                                          NST 1560
      JB = 32                                                           NST 1570
      WRITE (IOUT,1000) JB,I,J                                          NST 1580
C                                                                       NST 1590
      CALL EXIT                                                         NST 1600
C                                                                       NST 1610
  104 CONTINUE                                                          NST 1620
      J = 1                                                             NST 1630
      DO 106 I=1,IVX                                                    NST 1640
        TAL = Y(I) / X(I)                                               NST 1650
        IF(NUAC(5) .NE. 14 .OR. I .EQ. 1) GOTO 105                      NST 1660
        J = J + 2                                                       NST 1670
        TAL = (Y(I)-PDI(I-1)) / (X(J)-X(J-1))                           NST 1680
  105   CONTINUE                                                        NST 1690
        EPICH = ABS(TAL-1.0)                                            NST 1700
        IF(EPICH .GT. 1.0E-4) GOTO 103                                  NST 1710
  106 CONTINUE                                                          NST 1720
  107 CONTINUE                                                          NST 1730
      EPICH = 1.0E-5                                                    NST 1740
      SPARE(51) = -1.0E+30                                              NST 1750
      SPARE(57) = -1.0E+30                                              NST 1760
      N17 = NUAC(17)                                                    NST 1770
      SQUIRE = 1.1547005                                                NST 1780
      IF(N17) 114,114,108                                               NST 1790
  108 IF(XMIS(2)) 114,109,109                                           NST 1800
  109 DO 113 K=1,KMAX                                                   NST 1810
        IF(XMIS(2)) 113,110,111                                         NST 1820
  110   T1 = 0.4692                                                     NST 1830
        GOTO 112                                                        NST 1840
  111   T1 = XMIS(2)                                                    NST 1850
  112   BBND(K) = T1                                                    NST 1860
  113 CONTINUE                                                          NST 1870
  114 CONTINUE                                                          NST 1880
      NGEM = IX(26)                                                     NST 1890
      IF(IX37 .GT. 0) GOTO 116                                          NST 1900
C                                                                       NST 1910
C     ZERO DCONBE,DCONRE,DCONBK,PTSAE IF NO I/O                         NST 1920
C                                                                       NST 1930
      NL = IX(169)                                                      NST 1940
      NU = IX(170)                                                      NST 1950
      DO 115 NIO=NL,NU                                                  NST 1960
        A(NIO) = 0.0                                                    NST 1970
  115 CONTINUE                                                          NST 1980
  116 CONTINUE                                                          NST 1990
      SPARE(98) = 0.0                                                   NST 2000
      DO 129 K=1,KMAX                                                   NST 2010
        READ (IO14) ((F1(KK,M),KK=1,KMAX),M=1,MMAX)                     NST 2020
        DO 128 M=1,MMAX                                                 NST 2030
          IF(M-N17) 123,118,123                                         NST 2040
  118     IF(XMIS(2)) 119,120,120                                       NST 2050
  119     IF(BBND(K)) 120,123,120                                       NST 2060
  120     DO 121 L=1,10                                                 NST 2070
            SIG(K,M,L) = 0.0                                            NST 2080
  121     CONTINUE                                                      NST 2090
          DO 122 KK=1,KMAX                                              NST 2100
            SCAC(K,M,KK) = 0.0                                          NST 2110
  122     CONTINUE                                                      NST 2120
          GOTO 128                                                      NST 2130
  123     CONTINUE                                                      NST 2140
          SIG(K,M,9) = SIG(K,M,1) * SIG(K,M,6)                          NST 2150
          SPARE(98) = SPARE(98) + SIG(K,M,8)                            NST 2160
C                                                                       NST 2170
C     SEARCH OPTIONS                                                    NST 2180
C                                                                       NST 2190
          IF(IX(5) .NE. -2) GOTO 126                                    NST 2200
          IF(IX(44) .EQ. 0 .AND. IX(49) .EQ. 0) GOTO 125                NST 2210
          IF(IX(49) .GT. 0) GO TO 124                                   NST 2220
          IF(M .EQ. IX(44) .OR. M .EQ. IX(45) .OR. M .EQ. IX(46) .OR. M NST 2230
     1     .EQ. IX(47) .OR. M .EQ. IX(48)) GOTO 125                     NST 2240
          GOTO 126                                                      NST 2250
  124     IF(IX(49) .NE. NCLASS(M)) GOTO 126                            NST 2260
  125     SIG(K,M,5) = SIG(K,M,9)                                       NST 2270
          SIG(K,M,9) = 0.0                                              NST 2280
  126     CONTINUE                                                      NST 2290
          DO 127 KK=1,KMAX                                              NST 2300
            SCAC(K,M,KK) = F1(KK,M)                                     NST 2310
  127     CONTINUE                                                      NST 2320
  128   CONTINUE                                                        NST 2330
  129 CONTINUE                                                          NST 2340
      REWIND IO14                                                       NST 2350
      T1 = 0.0                                                          NST 2360
      DO 136 K=1,KMAX                                                   NST 2370
        DO 135 M=1,MMAX                                                 NST 2380
          TPTSA = 0.D+0                                                 NST 2390
          DO 130 KK=1,KMAX                                              NST 2400
            TPTSA = TPTSA + SCAC(K,M,KK)                                NST 2410
  130     CONTINUE                                                      NST 2420
          SIG(K,M,2) = TPTSA                                            NST 2430
          TPTSA = TPTSA + SIG(K,M,3) + SIG(K,M,9)                       NST 2440
          SIG(K,M,10) = TPTSA                                           NST 2450
          SIG(K,M,3) = SIG(K,M,10) - SIG(K,M,2) - SIG(K,M,9)            NST 2460
          IF(SIG(K,M,10) .EQ. 0.0 .OR. SIG(K,M,5) .EQ. 0.0) GOTO 131    NST 2470
          SPARE(51) = AMAX1(SPARE(51),SIG(K,M,5)/SIG(K,M,10))           NST 2480
C                                                                       NST 2490
C     SEARCH OPTIONS                                                    NST 2500
C                                                                       NST 2510
  131     CONTINUE                                                      NST 2520
          IF(IX(5) .EQ. -5) GOTO 134                                    NST 2530
          IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 134                   NST 2540
          IF(IX(44) .EQ. 0 .AND. IX(49) .EQ. 0) GOTO 133                NST 2550
          IF(IX(49) .GT. 0) GOTO 132                                    NST 2560
          IF(M .EQ. IX(44) .OR. M .EQ. IX(45) .OR. M .EQ. IX(46) .OR. M NST 2570
     1     .EQ. IX(47) .OR. M .EQ. IX(48)) GOTO 133                     NST 2580
          GOTO 134                                                      NST 2590
  132     IF(IX(49) .NE. NCLASS(M)) GOTO 134                            NST 2600
  133     T1 = T1 + SIG(K,M,5)                                          NST 2610
  134     CONTINUE                                                      NST 2620
  135   CONTINUE                                                        NST 2630
  136 CONTINUE                                                          NST 2640
C                                                                       NST 2650
C     SEARCH OPTIONS                                                    NST 2660
C                                                                       NST 2670
      IF(IX(5) .EQ. -5) GOTO 138                                        NST 2680
      IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 138                       NST 2690
      IF(T1 .NE. 0.0) GOTO 138                                          NST 2700
      JB = 16                                                           NST 2710
      WRITE (IOUT,1000) JB                                              NST 2720
C                                                                       NST 2730
      CALL EXIT                                                         NST 2740
C                                                                       NST 2750
  138 CONTINUE                                                          NST 2760
      NEM = NGEM - 10                                                   NST 2770
      DO 250 KT1=1,KMAX                                                 NST 2780
        K = KT1                                                         NST 2790
        N = KT1                                                         NST 2800
        IF(IX37 .EQ. 0) GOTO 140                                        NST 2810
        N = 1                                                           NST 2820
        IF(IX(71) .GT. 0) K = KMAXP1 - KT1                              NST 2830
C                                                                       NST 2840
C     ZERO DCONBE,DCONRE,DCONBK,PTSAE IF I/O                            NST 2850
C                                                                       NST 2860
        DO 139 NIO=IX37,IX38                                            NST 2870
          A(NIO) = 0.0                                                  NST 2880
  139   CONTINUE                                                        NST 2890
  140   CONTINUE                                                        NST 2900
        DO 233 KB=1,KBMAX                                               NST 2910
          L = KB                                                        NST 2920
          TB = ZZ(KB+1) - ZZ(KB)                                        NST 2930
          DELFR = 0.0                                                   NST 2940
          IF(KB-1) 142,142,141                                          NST 2950
  141     DELFR = ZZ(KB) - ZZ(KB-1)                                     NST 2960
  142     DELF = Z(KB) - ZZ(KB)                                         NST 2970
          DELK = ZZ(KB+1) - Z(KB)                                       NST 2980
          DELBK = 0.0                                                   NST 2990
          IF(KB-KBMAX) 143,144,144                                      NST 3000
  143     DELBK = Z(KB+1) - ZZ(KB+1)                                    NST 3010
  144     CONTINUE                                                      NST 3020
          DO 232 I=1,IMAX                                               NST 3030
            NN1= (I-1) * JVX                                            NST 3040
            NN2= (I-1) * JVXP1                                          NST 3050
            N12 = I * JVXP1                                             NST 3060
            T1 = YY(I+1) - YY(I)                                        NST 3070
            DELTT = 0.0                                                 NST 3080
            IF(I-1) 146,146,145                                         NST 3090
  145       DELTT = YY(I) - Y(I-1)                                      NST 3100
            IF(NEM .EQ. 4) DELTT = YY(I) - PDI(I-1)                     NST 3110
  146       DELT = Y(I) - YY(I)                                         NST 3120
            DELB = YY(I+1) - Y(I)                                       NST 3130
            IF(NEM .EQ. 4) DELB = YY(I+1) - PDI(I)                      NST 3140
            DELBB = 0.0                                                 NST 3150
            IF(I-IMAX) 147,148,148                                      NST 3160
  147       DELBB = Y(I+1) - YY(I+1)                                    NST 3170
  148       CONTINUE                                                    NST 3180
            DO 231 J=1,JMAX                                             NST 3190
              NRN = NRGNE(J,I,KB)                                       NST 3200
              NOE = J - (J/2) * 2                                       NST 3210
              N1 = NN1 + J                                              NST 3220
              N2 = N1 + JVX                                             NST 3230
              N3 = NN2 + J                                              NST 3240
              N4 = N3 + 1                                               NST 3250
              N5 = N3 + JVXP1                                           NST 3260
              DELLL = 0.0                                               NST 3270
              IF(J-1) 150,150,149                                       NST 3280
  149         DELLL = XX(J) - X(J-1)                                    NST 3290
              IF(NEM .EQ. 4) N2 = N2 - 1                                NST 3300
  150         DELL = X(J) - XX(J)                                       NST 3310
              DELR = XX(J+1) - X(J)                                     NST 3320
              DELRR = 0.0                                               NST 3330
              IF(J-JMAX) 151,152,152                                    NST 3340
  151         DELRR = X(J+1) - XX(J+1)                                  NST 3350
  152         CONTINUE                                                  NST 3360
              GOTO(159,158,153,160),NEM                                 NST 3370
  153         TAT = SQUIRE * DELT * TB                                  NST 3380
              TAL = SQUIRE * DELL * TB                                  NST 3390
              TAR = DELR + DELL                                         NST 3400
              TAB = DELB + DELT                                         NST 3410
              TD1 = X(J+1) - X(J)                                       NST 3420
              TD2 = Y(I) - Y(I-1)                                       NST 3430
              IF(I .EQ. 1) TD2 = 2 * Y(I)                               NST 3440
              IF(J .EQ. JMAX) TD1 = TD2                                 NST 3450
              DELHT = 0.5 * SQRT(TD1**2+TD2**2-TD1*TD2)                 NST 3460
              DELHB = DELHT                                             NST 3470
              TAR = TAL                                                 NST 3480
              TAB = TAT                                                 NST 3490
              TAH = SQUIRE * DELHT * TB                                 NST 3500
              TABK = SQUIRE * (DELT**2+DELL**2+DELHT**2)                NST 3510
              TAF = TABK                                                NST 3520
              MH = 0                                                    NST 3530
              IF(J-JMAX) 155,157,157                                    NST 3540
  155         IF(I-1) 157,157,156                                       NST 3550
  156         NHN = NRGNE(J+1,I-1,KB)                                   NST 3560
              MH = NCOMP(NHN)                                           NST 3570
  157         GOTO 162                                                  NST 3580
  158         T2 = XX(J+1) - XX(J)                                      NST 3590
              TAL = TB * T1                                             NST 3600
              TAR = TAL                                                 NST 3610
              TAT = YY(I) * T2 * TB                                     NST 3620
              TAB = YY(I+1) * T2 * TB                                   NST 3630
              TAF = YY(I+1)**2                                          NST 3640
              IF(YY(I) .NE. 0.) TAF = TAF - YY(I)**2                    NST 3650
              TAF = TAF * 0.5 * T2                                      NST 3660
              TABK = TAF                                                NST 3670
              DELR = DELR * Y(I)                                        NST 3680
              DELRR = DELRR * Y(I)                                      NST 3690
              DELL = DELL * Y(I)                                        NST 3700
              DELLL = DELLL * Y(I)                                      NST 3710
              GOTO 162                                                  NST 3720
  159         TAL = T1 * TB                                             NST 3730
              TAR = TAL                                                 NST 3740
              TAT = (XX(J+1)-XX(J)) * TB                                NST 3750
              TAB = TAT                                                 NST 3760
              TAF = (XX(J+1)-XX(J)) * T1                                NST 3770
              TABK = TAF                                                NST 3780
              GOTO 162                                                  NST 3790
  160         CONTINUE                                                  NST 3800
              TAL = 3.46410 * (X(J)-XX(J)) * TB                         NST 3810
C             2 * SQRT(3) = 3.46410                                     NST 3820
              TAR = 3.46410 * (XX(J+1)-X(J)) * TB                       NST 3830
              TAF = PVOL(NRN) / TB                                      NST 3840
              TABK = TAF                                                NST 3850
              IF(NOE .EQ. 0) GOTO 161                                   NST 3860
              TAT = 3.46410 * (Y(I)-YY(I)) * TB                         NST 3870
              TAB = 0.0                                                 NST 3880
              GOTO 162                                                  NST 3890
  161         CONTINUE                                                  NST 3900
              TAB = 3.46410 * (YY(I+1)-PDI(I)) * TB                     NST 3910
              TAT = 0.0                                                 NST 3920
  162         CONTINUE                                                  NST 3930
              M = NCOMP(NRN)                                            NST 3940
              IF(KB .EQ. 1) GOTO 163                                    NST 3950
              NRNF = NRGNE(J,I,KB-1)                                    NST 3960
              MF = NCOMP(NRNF)                                          NST 3970
  163         IF(I .EQ. 1) GOTO 164                                     NST 3980
              NRNT = NRGNE(J,I-1,KB)                                    NST 3990
              IF(NEM .EQ. 4 .AND. J .LT. JMAX) NRNT = NRGNE(J+1,I-1,KB) NST 4000
              MT = NCOMP(NRNT)                                          NST 4010
  164         IF(J .EQ. 1) GOTO 165                                     NST 4020
              NRNL = NRGNE(J-1,I,KB)                                    NST 4030
              ML = NCOMP(NRNL)                                          NST 4040
              GOTO 166                                                  NST 4050
  165         IF(INRB .NE. 2) GOTO 166                                  NST 4060
              NRNL = NRGNE(JMAX,I,KB)                                   NST 4070
              ML = NCOMP(NRNL)                                          NST 4080
  166         IF(KB .EQ. KBMAX) GOTO 167                                NST 4090
              NRNBK = NRGNE(J,I,KB+1)                                   NST 4100
              MBK = NCOMP(NRNBK)                                        NST 4110
  167         IF(I .EQ. IMAX) GOTO 168                                  NST 4120
              NRNB = NRGNE(J,I+1,KB)                                    NST 4130
              IF(NEM .EQ. 4 .AND. J .GT. 1) NRNB = NRGNE(J-1,I+1,KB)    NST 4140
              MB = NCOMP(NRNB)                                          NST 4150
              GOTO 169                                                  NST 4160
  168         IF(INRB .NE. 3) GOTO 169                                  NST 4170
              NRNB = NRGNE(JVX,J,KB)                                    NST 4180
              IF(NGEM .EQ. 14) NRNB = NRGNE(JVX,J/2,KB)                 NST 4190
              MB = NCOMP(NRNB)                                          NST 4200
              DELB = XX(JVXP1) - X(JVX)                                 NST 4210
  169         IF(J .EQ. JMAX) GOTO 170                                  NST 4220
              NRNR = NRGNE(J+1,I,KB)                                    NST 4230
              MR = NCOMP(NRNR)                                          NST 4240
              GOTO 174                                                  NST 4250
  170         GOTO(174,171,172,173),INRB                                NST 4260
  171         CONTINUE                                                  NST 4270
              NRNR = NRGNE(1,I,KB)                                      NST 4280
              MR = NCOMP(NRNR)                                          NST 4290
              DELRR = X(1)                                              NST 4300
              IF(NGEM .EQ. 2) DELRR = DELRR * Y(I)                      NST 4310
              GOTO 174                                                  NST 4320
  172         CONTINUE                                                  NST 4330
              NRNR = NRGNE(I,IVX,KB)                                    NST 4340
              IF(NGEM .EQ. 14) NRNR = NRGNE(2*I,IVX,KB)                 NST 4350
              MR = NCOMP(NRNR)                                          NST 4360
              DELRR = YY(IVXP1) - Y(IVX)                                NST 4370
              IF(NGEM .EQ. 14) DELRR = YY(IVXP1) - PDI(IVX)             NST 4380
              GOTO 174                                                  NST 4390
  173         CONTINUE                                                  NST 4400
              NRNR = NRGNE(JVX,IVXP1-I,KB)                              NST 4410
              MR = NCOMP(NRNR)                                          NST 4420
              DELRR = DELR                                              NST 4430
  174         CONTINUE                                                  NST 4440
              IF(M .NE. N17) GOTO 175                                   NST 4450
              IF(XMIS(2) .GE. 0.0) GOTO 198                             NST 4460
              IF(BBND(K) .NE. 0.0) GOTO 198                             NST 4470
  175         IF(KB .GT. 1) GOTO 176                                    NST 4480
C             DCONBK(N1,KB,N) = BND(5,K) * TAF / (1.0+DELF*BND(5,K)/    NST 4490
C              SIG(K,M,1))                                              NST 4500
              DCONBK(N1,KB,N) = BND(5,K) * TAF / (1.0+DELF*BND(5,K)/    NST 4510
     1         SIG(K,M,1)/B(3,K,M))                                     NST 4520
  176         IF(I .GT. 1) GOTO 177                                     NST 4530
C             DCONBE(N1,KB,N) = BND(2,K) * TAT / (1.0+DELT*BND(2,K)/    NST 4540
C              SIG(K,M,1))                                              NST 4550
              DCONBE(N1,KB,N) = BND(2,K) * TAT / (1.0+DELT*BND(2,K)/    NST 4560
     1         SIG(K,M,1)/B(2,K,M))                                     NST 4570
  177         IF(J .GT. 1) GOTO 178                                     NST 4580
              IF(INRB .EQ. 2) GOTO 178                                  NST 4590
C             DCONRE(N3,KB,N) = BND(1,K) * TAL / (1.0+DELL*BND(1,K)/    NST 4600
C              SIG(K,M,1))                                              NST 4610
              DCONRE(N3,KB,N) = BND(1,K) * TAL / (1.0+DELL*BND(1,K)/    NST 4620
     1         SIG(K,M,1)/B(1,K,M))                                     NST 4630
  178         IF(KB .LT. KBMAX) GOTO 179                                NST 4640
C             DCONBK(N1,KB+1,N) = BND(6,K) * TABK / (1.0+DELK*BND(6,K)/ NST 4650
C              SIG(K,M,1))                                              NST 4660
              DCONBK(N1,KB+1,N) = BND(6,K) * TABK / (1.0+DELK*BND(6,K)/ NST 4670
     1         SIG(K,M,1)/B(3,K,M))                                     NST 4680
              IF(DCONBK(N1,KB+1,N) .EQ. 0.0) DCONBK(N1,KB+1,N) =        NST 4690
     1         4096.0E-13                                               NST 4700
  179         IF(I .LT. IMAX) GOTO 180                                  NST 4710
C             DCONBE(N2,KB,N) = BND(4,K) * TAB / (1.0+DELB*BND(4,K)/    NST 4720
C              SIG(K,M,1))                                              NST 4730
              DCONBE(N2,KB,N) = BND(4,K) * TAB / (1.0+DELB*BND(4,K)/    NST 4740
     1         SIG(K,M,1)/B(2,K,M))                                     NST 4750
              IF(DCONBE(N2,KB,N) .EQ. 0.0) DCONBE(N2,KB,N) = 4096.0E-13 NST 4760
  180         IF(J .LT. JMAX) GOTO 181                                  NST 4770
              IF(INRB .GT. 1) GOTO 181                                  NST 4780
C             DCONRE(N4,KB,N) = BND(3,K) * TAR / (1.0+DELR*BND(3,K)/    NST 4790
C              SIG(K,M,1))                                              NST 4800
              DCONRE(N4,KB,N) = BND(3,K) * TAR / (1.0+DELR*BND(3,K)/    NST 4810
     1         SIG(K,M,1)/B(1,K,M))                                     NST 4820
              IF(DCONRE(N4,KB,N) .EQ. 0.0) DCONRE(N4,KB,N) = 4096.0E-13 NST 4830
              GOTO 182                                                  NST 4840
C 181         DCONRE(N4,KB,N) = TAR * SIG(K,MR,1) / (DELRR+DELR*        NST 4850
C              SIG(K,MR,1)/SIG(K,M,1))                                  NST 4860
  181         DCONRE(N4,KB,N) = TAR * SIG(K,MR,1) * B(1,K,MR) / (DELRR+ NST 4870
     1         DELR*SIG(K,MR,1)*B(1,K,MR)/(SIG(K,M,1)*B(1,K,M)))        NST 4880
              IF(DCONRE(N4,KB,N) .EQ. 0.0) DCONRE(N4,KB,N) = 4096.0E-13 NST 4890
  182         IF(I .GE. IMAX) GOTO 183                                  NST 4900
C             DCONBE(N2,KB,N) = TAB * SIG(K,MB,1) / (DELBB+DELB*        NST 4910
C              SIG(K,MB,1)/SIG(K,M,1))                                  NST 4920
              DCONBE(N2,KB,N) = TAB * SIG(K,MB,1) * B(2,K,MB) / (DELBB+ NST 4930
     1         DELB*SIG(K,MB,1)*B(2,K,MB)/(SIG(K,M,1)*B(2,K,M)))        NST 4940
              IF(DCONBE(N2,KB,N) .EQ. 0.0) DCONBE(N2,KB,N) = 4096.0E-13 NST 4950
  183         IF(KB .GE. KBMAX) GOTO 184                                NST 4960
C             DCONBK(N1,KB+1,N) = TABK * SIG(K,MBK,1) / (DELBK+DELK*    NST 4970
C              SIG(K,MBK,1)/SIG(K,M,1))                                 NST 4980
              DCONBK(N1,KB+1,N) = TABK * SIG(K,MBK,1) * B(3,K,MBK) /    NST 4990
     1         (DELBK+DELK*SIG(K,MBK,1)*B(3,K,MBK)/(SIG(K,M,1)*B(3,K,M))NST 5000
     2         )                                                        NST 5010
              IF(DCONBK(N1,KB+1,N) .EQ. 0.0) DCONBK(N1,KB+1,N) =        NST 5020
     1         4096.0E-13                                               NST 5030
  184         CONTINUE                                                  NST 5040
              IF(N17 .LE. 0) GOTO 198                                   NST 5050
              IF(XMIS(2) .GE. 0.0) GOTO 185                             NST 5060
              IF(BBND(K) .EQ. 0.0) GOTO 198                             NST 5070
  185         IF(KBMAX .LE. 1) GOTO 188                                 NST 5080
              IF(KB .EQ. KBMAX) GOTO 187                                NST 5090
              IF(MBK .NE. N17) GOTO 186                                 NST 5100
C             DCONBK(N1,KB+1,N) = BBND(K) * TABK / (1.0+DELK*BBND(K)/   NST 5110
C              SIG(K,M,1))                                              NST 5120
              DCONBK(N1,KB+1,N) = BBND(K) * TABK / (1.0+DELK*BBND(K)/   NST 5130
     1         SIG(K,M,1)/B(3,K,M))                                     NST 5140
              IF(DCONBK(N1,KB+1,N) .EQ. 0.0) DCONBK(N1,KB+1,N) =        NST 5150
     1         4096.0E-13                                               NST 5160
  186         IF(KB .EQ. 1) GOTO 188                                    NST 5170
  187         IF(MF .NE. N17) GOTO 188                                  NST 5180
C             DCONBK(N1,KB,N) = BBND(K) * TAF / (1.0+DELF*BBND(K)/      NST 5190
C              SIG(K,M,1))                                              NST 5200
              DCONBK(N1,KB,N) = BBND(K) * TAF / (1.0+DELF*BBND(K)/      NST 5210
     1         SIG(K,M,1)/B(3,K,M))                                     NST 5220
  188         CONTINUE                                                  NST 5230
              IF(IMAX .LE. 1) GOTO 193                                  NST 5240
              IF(I .NE. IMAX) GOTO 190                                  NST 5250
              IF(INRB .NE. 3) GOTO 191                                  NST 5260
              IF(MB .NE. N17) GOTO 191                                  NST 5270
C             DCONBE(N2,KB,N) = BBND(K) * TAB / (1.0+DELB*BBND(K)/      NST 5280
C              SIG(K,M,1))                                              NST 5290
              DCONBE(N2,KB,N) = BBND(K) * TAB / (1.0+DELB*BBND(K)/      NST 5300
     1         SIG(K,M,1)/B(2,K,M))                                     NST 5310
              IF(DCONBE(N2,KB,N) .EQ. 0.0) DCONBE(N2,KB,N) = 4096.0E-13 NST 5320
              IF(NGEM .EQ. 14) GOTO 189                                 NST 5330
              DCONRE(N12,KB,N) = DCONBE(N2,KB,N)                        NST 5340
              GOTO 191                                                  NST 5350
  189         CONTINUE                                                  NST 5360
              IF((J/2)*2 .NE. J) GOTO 191                               NST 5370
              N122 = (J/2-1) * JVXP1 + JVXP1                            NST 5380
              DCONRE(N122,KB,N) = DCONBE(N2,KB,N)                       NST 5390
              GOTO 191                                                  NST 5400
  190         IF(MB .NE. N17) GOTO 191                                  NST 5410
C             DCONBE(N2,KB,N) = BBND(K) * TAB / (1.0+DELB*BBND(K)/      NST 5420
C              SIG(K,M,1))                                              NST 5430
              DCONBE(N2,KB,N) = BBND(K) * TAB / (1.0+DELB*BBND(K)/      NST 5440
     1         SIG(K,M,1)/B(2,K,M))                                     NST 5450
              IF(DCONBE(N2,KB,N) .EQ. 0.0) DCONBE(N2,KB,N) = 4096.0E-13 NST 5460
  191         IF(I .EQ. 1) GOTO 193                                     NST 5470
              IF(MT .NE. N17) GOTO 193                                  NST 5480
C             DCONBE(N1,KB,N) = BBND(K) * TAT / (1.0+DELT*BBND(K)/      NST 5490
C              SIG(K,M,1))                                              NST 5500
              DCONBE(N1,KB,N) = BBND(K) * TAT / (1.0+DELT*BBND(K)/      NST 5510
     1         SIG(K,M,1)/B(2,K,M))                                     NST 5520
  193         CONTINUE                                                  NST 5530
              IF(JMAX .LE. 1) GOTO 198                                  NST 5540
              IF(J .LT. JMAX) GOTO 194                                  NST 5550
              IF(INRB .LE. 1) GOTO 197                                  NST 5560
  194         IF(MR .NE. N17) GOTO 196                                  NST 5570
C             DCONRE(N4,KB,N) = BBND(K) * TAR / (1.0+DELR*BBND(K)/      NST 5580
C              SIG(K,M,1))                                              NST 5590
              DCONRE(N4,KB,N) = BBND(K) * TAR / (1.0+DELR*BBND(K)/      NST 5600
     1         SIG(K,M,1)/B(1,K,M))                                     NST 5610
              IF(DCONRE(N4,KB,N) .EQ. 0.0) DCONRE(N4,KB,N) = 4096.0E-13 NST 5620
              IF(INRB .NE. 4) GOTO 195                                  NST 5630
              N11 = IVXP1 * JVXP1 - N12                                 NST 5640
              DCONRE(N11,KB,N) = DCONRE(N12,KB,N)                       NST 5650
  195         CONTINUE                                                  NST 5660
  196         IF(J .GT. 1) GOTO 197                                     NST 5670
              IF(INRB .NE. 2) GOTO 198                                  NST 5680
              IF(ML .NE. N17) GOTO 198                                  NST 5690
C             DCONRE(N12,KB,N) = BBND(K) * TAL / (1.0+DELL*BBND(K)/     NST 5700
C              SIG(K,M,1))                                              NST 5710
              DCONRE(N12,KB,N) = BBND(K) * TAL / (1.0+DELL*BBND(K)/     NST 5720
     1         SIG(K,M,1)/B(1,K,M))                                     NST 5730
              IF(DCONRE(N12,KB,N) .EQ. 0.0) DCONRE(N12,KB,N) =          NST 5740
     1         4096.0E-13                                               NST 5750
              GOTO 198                                                  NST 5760
  197         IF(ML .NE. N17) GOTO 198                                  NST 5770
C             DCONRE(N3,KB,N) = BBND(K) * TAL / (1.0+DELL*BBND(K)/      NST 5780
C              SIG(K,M,1))                                              NST 5790
              DCONRE(N3,KB,N) = BBND(K) * TAL / (1.0+DELL*BBND(K)/      NST 5800
     1         SIG(K,M,1)/B(1,K,M))                                     NST 5810
              IF(DCONRE(N3,KB,N) .EQ. 0.0) DCONRE(N3,KB,N) = 4096.0E-13 NST 5820
  198         CONTINUE                                                  NST 5830
              IF(NEM-3) 230,199,230                                     NST 5840
  199         KKK = IOVX + N                                            NST 5850
              IF(N17) 200,200,201                                       NST 5860
  200         CONTINUE                                                  NST 5870
              IF(J .EQ. JMAX) GOTO 211                                  NST 5880
              IF(I .EQ. 1) GOTO 226                                     NST 5890
C             DCONRE(N4,KB,KKK) = TAH * SIG(K,MH,1) / (DELHT+DELHB*     NST 5900
C              SIG(K,MH,1)/SIG(K,M,1))                                  NST 5910
              DCONRE(N4,KB,KKK) = TAH * SIG(K,MH,1) * B(1,K,MH) / (DELHTNST 5920
     1         +DELHB*SIG(K,MH,1)*B(1,K,MH)/ SIG(K,M,1)/B(1,K,M))       NST 5930
              GOTO 210                                                  NST 5940
  201         IF(XMIS(2)) 202,204,204                                   NST 5950
  202         IF(BBND(K)) 200,203,204                                   NST 5960
  203         DCONRE(N4,KB,KKK) = 0.                                    NST 5970
              GOTO 210                                                  NST 5980
  204         IF(MH-N17) 207,205,207                                    NST 5990
  205         IF(M-N17) 206,203,206                                     NST 6000
C 206         DCONRE(N4,KB,KKK) = BBND(K) * TAH / (1.0+DELHT*BBND(K)/   NST 6010
C              SIG(K,M,1))                                              NST 6020
  206         DCONRE(N4,KB,KKK) = BBND(K) * TAH / (1.0+DELHT*BBND(K)/   NST 6030
     1         SIG(K,M,1)/B(1,K,M))                                     NST 6040
              GOTO 210                                                  NST 6050
  207         IF(M-N17) 200,208,200                                     NST 6060
  208         IF(MH) 203,203,209                                        NST 6070
C 209         DCONRE(N4,KB,KKK) = BBND(K) * TAH / (1.+DELHB*BBND(K)/    NST 6080
C              SIG(K,MH,1))                                             NST 6090
  209         DCONRE(N4,KB,KKK) = BBND(K) * TAH / (1.0+DELHB*BBND(K)/   NST 6100
     1         SIG(K,MH,1)/B(1,K,MH))                                   NST 6110
  210         IF(J-JMAX) 218,211,211                                    NST 6120
  211         DCONRE(N4,KB,KKK) = 0.                                    NST 6130
              IF(I-1) 212,212,216                                       NST 6140
  212         IF(DCONRE(N4,KB,N)-4096.0E-13) 213,214,213                NST 6150
  213         DCONRE(N4,KB,N) = 1.5 * DCONRE(N4,KB,N) / SQUIRE          NST 6160
  214         IF(DCONBE(N1,KB,N)-4096.0E-13) 215,226,215                NST 6170
  215         DCONBE(N1,KB,N) = 1.5 * DCONBE(N1,KB,N) / SQUIRE          NST 6180
              GOTO 226                                                  NST 6190
  216         IF(DCONRE(N4,KB,N)-4096.0E-13) 217,218,217                NST 6200
  217         DCONRE(N4,KB,N) = 2.0 * DCONRE(N4,KB,N) / SQUIRE          NST 6210
  218         IF(I-IMAX) 226,219,219                                    NST 6220
  219         DCONRE(N5,KB,KKK) = 0.                                    NST 6230
              IF(J-1) 220,220,224                                       NST 6240
  220         IF(DCONRE(N3,KB,N)-4096.0E-13) 221,222,221                NST 6250
  221         DCONRE(N3,KB,N) = 1.5 * DCONRE(N3,KB,N) / SQUIRE          NST 6260
  222         IF(DCONBE(N2,KB,N)-4096.0E-13) 223,226,223                NST 6270
  223         DCONBE(N2,KB,N) = 1.5 * DCONBE(N2,KB,N) / SQUIRE          NST 6280
              GOTO 226                                                  NST 6290
  224         IF(DCONBE(N2,KB,N)-4096.0E-13) 225,226,225                NST 6300
  225         DCONBE(N2,KB,N) = 2.0 * DCONBE(N2,KB,N) / SQUIRE          NST 6310
  226         IF(I-1) 227,227,228                                       NST 6320
  227         DCONRE(N4,KB,KKK) = 0.                                    NST 6330
              IF(J .NE. JMAX) DCONBE(J,KB,N) = 2.0 * DCONBE(J,KB,N) /   NST 6340
     1         SQUIRE                                                   NST 6350
  228         IF(J-1) 229,229,230                                       NST 6360
  229         DCONRE(N5,KB,KKK) = 0.                                    NST 6370
              IF(I .NE. IMAX) DCONRE(N3,KB,N) = 2.0 * DCONRE(N3,KB,N) / NST 6380
     1         SQUIRE                                                   NST 6390
  230         CONTINUE                                                  NST 6400
  231       CONTINUE                                                    NST 6410
  232     CONTINUE                                                      NST 6420
  233   CONTINUE                                                        NST 6430
        IF(INRB .LE. 1) GOTO 242                                        NST 6440
        DO 241 KB=1,KBMAX                                               NST 6450
          GOTO(240,238,236,234),INRB                                    NST 6460
  234     CONTINUE                                                      NST 6470
          I = IVX / 2                                                   NST 6480
          J2 = IVXP1 * JVXP1                                            NST 6490
          DO 235 LL=1,I                                                 NST 6500
            N12 = LL * JVXP1                                            NST 6510
            N11 = J2 - N12                                              NST 6520
            DCONRE(N11,KB,N) = DCONRE(N12,KB,N)                         NST 6530
  235     CONTINUE                                                      NST 6540
          GOTO 240                                                      NST 6550
  236     CONTINUE                                                      NST 6560
          DCONRE(JP1IXZ,KB,K) = 4096.0E-13                              NST 6570
          LJ = 1                                                        NST 6580
          DO 237 LL=1,IVX                                               NST 6590
            N12 = LL * JVXP1                                            NST 6600
            N14 = JIVX + LJ                                             NST 6610
            DCONBE(N14,KB,N) = DCONRE(N12,KB,N)                         NST 6620
            LJ = LJ + 1                                                 NST 6630
            IF(NGEM .EQ. 14) LJ = LJ + 1                                NST 6640
  237     CONTINUE                                                      NST 6650
          GOTO 240                                                      NST 6660
  238     CONTINUE                                                      NST 6670
          DO 239 I=1,IVX                                                NST 6680
            N12 = I * JVXP1                                             NST 6690
            N15 = N12 - JVX                                             NST 6700
            DCONRE(N15,KB,N) = DCONRE(N12,KB,N)                         NST 6710
  239     CONTINUE                                                      NST 6720
  240     CONTINUE                                                      NST 6730
  241   CONTINUE                                                        NST 6740
  242   CONTINUE                                                        NST 6750
        DO 249 KB=1,KBMAX                                               NST 6760
          DO 248 I=1,IMAX                                               NST 6770
            NN1= (I-1) * JVX                                            NST 6780
            NN2= (I-1) * JVXP1                                          NST 6790
            DO 247 J=1,JMAX                                             NST 6800
              N1 = NN1 + J                                              NST 6810
              N2 = N1 + JVX                                             NST 6820
              N3 = NN2 + J                                              NST 6830
              N4 = N3 + 1                                               NST 6840
              N5 = N3 + JVXP1                                           NST 6850
              NRN = NRGNE(J,I,KB)                                       NST 6860
              M = NCOMP(NRN)                                            NST 6870
              D1 = DCONBE(N1,KB,N)                                      NST 6880
              IF(NEM .NE. 4) GOTO 243                                   NST 6890
              IF(J .EQ. 1) D2 = 0.0                                     NST 6900
              IF(J .EQ. 1) GOTO 244                                     NST 6910
              N2 = N2 - 1                                               NST 6920
  243         CONTINUE                                                  NST 6930
              D2 = DCONBE(N2,KB,N)                                      NST 6940
  244         CONTINUE                                                  NST 6950
              D3 = DCONRE(N3,KB,N)                                      NST 6960
              D4 = DCONRE(N4,KB,N)                                      NST 6970
              D5 = DCONBK(N1,KB,N)                                      NST 6980
              D6 = DCONBK(N1,KB+1,N)                                    NST 6990
              D9 = SIG(K,M,10)                                          NST 7000
              TPTSA = D1 + D2 + D3 + D4 + D5 + D6 + D9 * PVOL(NRN)      NST 7010
              IF(NEM .NE. 3) GOTO 245                                   NST 7020
              D7 = DCONRE(N5,KB,KKK)                                    NST 7030
              D8 = DCONRE(N4,KB,KKK)                                    NST 7040
              TPTSA = TPTSA + D7 + D8                                   NST 7050
  245         CONTINUE                                                  NST 7060
              PTSAE(N1,KB,N) = TPTSA                                    NST 7070
              IF(TPTSA .EQ. 0.0 .OR. SIG(K,M,5) .EQ. 0.0) GOTO 246      NST 7080
              SPARE(57) = AMAX1(SPARE(57),SIG(K,M,5)*PVOL(NRN)/         NST 7090
     1         PTSAE(N1,KB,N))                                          NST 7100
  246         CONTINUE                                                  NST 7110
  247       CONTINUE                                                    NST 7120
  248     CONTINUE                                                      NST 7130
  249   CONTINUE                                                        NST 7140
        IF(IX37 .EQ. 0) GOTO 250                                        NST 7150
        WRITE (IOADJ) AIO                                               NST 7160
  250 CONTINUE                                                          NST 7170
      IF(IX37 .EQ. 0) GOTO 251                                          NST 7180
      END FILE IOADJ                                                    NST 7190
      REWIND IOADJ                                                      NST 7200
  251 CONTINUE                                                          NST 7210
C                                                                       NST 7220
C     DIAGONAL SYMMETRY CHECKOUT                                        NST 7230
C                                                                       NST 7240
      IF(IX(71) .GT. 0) GOTO 271                                        NST 7250
      IF(NUAC(8)) 254,271,252                                           NST 7260
  252 IF(IMAX-JMAX) 253,262,253                                         NST 7270
  253 NUAC(8) = 0                                                       NST 7280
      WRITE (IOUT,1001)                                                 NST 7290
      GOTO 270                                                          NST 7300
  254 II = IMAX / 2                                                     NST 7310
      DO 261 N=1,KMAX                                                   NST 7320
        K = N                                                           NST 7330
        IF(IX37 .EQ. 0) GOTO 255                                        NST 7340
        K = 1                                                           NST 7350
        READ (IO15) AIO                                                 NST 7360
  255   CONTINUE                                                        NST 7370
        DO 260 I=1,II                                                   NST 7380
          L = IMAX - I + 1                                              NST 7390
          NN1 = (I-1) * JVX                                             NST 7400
          NN2 = (L-1) * JVX                                             NST 7410
          DO 259 J=1,JMAX                                               NST 7420
            M = JMAX - J + 1                                            NST 7430
            N6 = NN1 + J                                                NST 7440
            N7 = NN2 + M                                                NST 7450
            DO 258 KB=1,KBMAX                                           NST 7460
              MT = NRGNE(J,I,KB)                                        NST 7470
              ML = NRGNE(M,L,KB)                                        NST 7480
              IF(NCOMP(MT)-NCOMP(ML)) 253,256,253                       NST 7490
  256         CONTINUE                                                  NST 7500
              IF(PTSAE(N6,KB,K) .EQ. 0.) GOTO 257                       NST 7510
              IF(ABS(PTSAE(N7,KB,K)/PTSAE(N6,KB,K)-1.0)-EPICH) 257,253, NST 7520
     1         253                                                      NST 7530
  257         CONTINUE                                                  NST 7540
  258       CONTINUE                                                    NST 7550
  259     CONTINUE                                                      NST 7560
  260   CONTINUE                                                        NST 7570
  261 CONTINUE                                                          NST 7580
      GOTO 270                                                          NST 7590
  262 CONTINUE                                                          NST 7600
      DO 269 N=1,KMAX                                                   NST 7610
        K = N                                                           NST 7620
        IF(IX37 .EQ. 0) GOTO 263                                        NST 7630
        K = 1                                                           NST 7640
        READ (IO15) AIO                                                 NST 7650
  263   CONTINUE                                                        NST 7660
        DO 268 KB=1,KBMAX                                               NST 7670
          DO 267 I=1,IMAX                                               NST 7680
            NN1 = (I-1) * JVX                                           NST 7690
            DO 266 J=1,JMAX                                             NST 7700
              N6 = NN1 + J                                              NST 7710
              N8 = (J-1) * JVX + I                                      NST 7720
              MT = NRGNE(J,I,KB)                                        NST 7730
              ML = NRGNE(I,J,KB)                                        NST 7740
              IF(NCOMP(MT)-NCOMP(ML)) 253,264,253                       NST 7750
  264         CONTINUE                                                  NST 7760
              IF(PTSAE(N8,KB,K) .EQ. 0.) GOTO 265                       NST 7770
              IF(ABS(PTSAE(N6,KB,K)/PTSAE(N8,KB,K)-1.0)-EPICH) 265,253, NST 7780
     1         253                                                      NST 7790
  265         CONTINUE                                                  NST 7800
  266       CONTINUE                                                    NST 7810
  267     CONTINUE                                                      NST 7820
  268   CONTINUE                                                        NST 7830
  269 CONTINUE                                                          NST 7840
      NUAC(20) = 0                                                      NST 7850
  270 CONTINUE                                                          NST 7860
      IF(IX37 .GT. 0) REWIND IO15                                       NST 7870
  271 CONTINUE                                                          NST 7880
      IF(SPARE(51) .NE. 0.0) SPARE(51) = -1.0 / SPARE(51)               NST 7890
      IF(SPARE(57) .NE. 0.0) SPARE(57) = -1.0 / SPARE(57)               NST 7900
      IF(IX(71) .GT. 0) IX(5) = 0                                       NST 7910
      RETURN                                                            NST 7920
C                                                                       NST 7930
 1000 FORMAT (1H0,'ERROR STOP',4I6)                                     NST 7940
 1001 FORMAT (1H0,'DIAGONAL SYMMETRY OPTION DENIED')                    NST 7950
 1002 FORMAT (//' PLEASE ENLARGE GROUP DIMENSION OF FIELD B IN SR KNST')NST 7960
 1003 FORMAT (//' PLEASE ENLARGE ZONE DIMENSION OF FIELD B IN SR KNST') NST 7970
 1004 FORMAT (18I4)                                                     NST 7980
 1005 FORMAT (//'  JZMI=',I5,'  JZMA=',I5,' IRICHT=',I5,' IRIC=',I5/'  SNST 7990
     1OMETHING WRONG WHILE READING BEHRENS CORRECTION')                 NST 8000
 1006 FORMAT (6E12.0)                                                   NST 8010
 1007 FORMAT (////'   THE DIFFUSION COEFFICIENT HAS BEEN MULTIPLIED WITHNST 8020
     1 THE FOLLOWING FACTORS'/)                                         NST 8030
 1008 FORMAT (//'   ZONE',I4,' TO',I4,'  GROUP    X-DIRECTION    Y-DIRECNST 8040
     1TION    Z-DIRECTION'/)                                            NST 8050
 1009 FORMAT (20X,I4,4X,1PE12.5,3X,E12.5,3X,E12.5)                      NST 8060
 1101 FORMAT (///' FOR NUAC(5) =',I3,' B(1,..,..) MUST BE = B(2,..,..)  NST 8070
     1  -->  *** STOP ***')                                             NST 8080
      END                                                               NST 8090
      SUBROUTINE KDUE(SCAC,RESLM,RESSA,P2E,NRGNE,SOURE,DCONRE,DCONBE,   KDU   10
     1 DCONBK,XI,IVX,JVX,KBVX,KVX,LVX,IVXP1,JVXP1,KBVXP1,IVZ,KVZ,JIVX,  KDU   20
     2 JIP1VX,JP1IXZ,IOVX,IOVZ,A,MEMORY,AIO,IX3738,XLAMDA,SIG,PVOL,NCOMPKDU   30
     3 ,MVX)                                                            KDU   40
C                                                                       KDU   50
CKDUE --119 ***CITATION*** RESIDUE CALC. FOR 3-D/ CF-KLUX               KDU   60
C                                                                       KDU   70
      REAL*8 TTT1,TTT2,TTT3,TTT4,TT1,TT2,TT3,TT4,TT5,TT6,XLAMDA,XLD     KDU   80
C                                                                       KDU   90
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KDU  100
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KDU  110
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KDU  120
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KDU  130
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KDU  140
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KDU  150
     6 IXPUT(9999),XPUT(9999)                                           KDU  160
C                                                                       KDU  170
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   KDU  180
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKKDU  190
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    KDU  200
     3 VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  KDU  210
     4 ITMAX,ITIME,BET(211),DEL(211)                                    KDU  220
C                                                                       KDU  230
      DIMENSION SCAC(KVX,MVX,KVX),P2E(JIVX,KBVX,KVX),NRGNE(JVX,IVX,KBVX)KDU  240
     1 ,SOURE(JVX,IVX,KBVX),DCONRE(JP1IXZ,KBVX,IOVZ),                   KDU  250
     2 DCONBE(JIP1VX,KBVX,IOVX),DCONBK(JIVX,KBVXP1,IOVX),XI(KVX),       KDU  260
     3 A(MEMORY),AIO(IX3738),SIG(KVX,MVX,10),PVOL(LVX),NCOMP(LVX)       KDU  270
C                                                                       KDU  280
CCCCC ********* SUBSCRIPT DEFINITIONS (KDUE E-200) ********* CCCCC      KDU  290
C    NEW         OLD            NEW         OLD                         KDU  300
C     N1         J,I             N6       J-1,I+1                       KDU  310
C     N2         J,I-1           N7       J+1,I-1                       KDU  320
C     N3         J,I+1           N8 *       J,I                         KDU  330
C     N4       J-1,I             N9 *       J,I+1                       KDU  340
C     N5       J+1,I             N10 *    J+1,I                         KDU  350
C     N11        1,I             N12      JVX,I                         KDU  360
C     N13      JVX,J             N14        I,IVX                       KDU  370
C     N15      JVX,IVXP1-I                                              KDU  380
C                                                                       KDU  390
C     INRB = 1  ORDINARY                                                KDU  400
C     INRB = 2  PERIODIC(REPEATING)                                     KDU  410
C     INRB = 3  90 DEGREE ROTATIONAL                                    KDU  420
C     INRB = 4  180 DEGREE ROTATIONAL                                   KDU  430
C                                                                       KDU  440
C                                                                       KDU  450
      INRB = IX(72) + 1                                                 KDU  460
      KMAXP1 = KMAX + 1                                                 KDU  470
      IX37 = IX(37)                                                     KDU  480
      IO15 = IX(82)                                                     KDU  490
      IOADJ = IO15                                                      KDU  500
      IF(IX(71) .GT. 0) IOADJ = IO2                                     KDU  510
      IF(IX37 .GT. 0) REWIND IOADJ                                      KDU  520
      TTT1 = 0.0                                                        KDU  530
      TTT2 = 0.0                                                        KDU  540
      TTT3 = 0.0                                                        KDU  550
      TTT4 = 0.0                                                        KDU  560
      IF(IX(24) .EQ. 0 .OR. IX(17) .EQ. 0) GOTO 100                     KDU  570
      XLA = 1.0 / SPARE(50)                                             KDU  580
      XLD = XLAMDA                                                      KDU  590
      IF(IX(17) .GE. 1) XLD = 0.0                                       KDU  600
      GOTO 103                                                          KDU  610
  100 CONTINUE                                                          KDU  620
C                                                                       KDU  630
C********SEARCH OPTIONS                                                 KDU  640
C                                                                       KDU  650
      IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 102                       KDU  660
      XLD = XLAMDA                                                      KDU  670
      IF(IX(5) .EQ. 1) XLD = 0.0                                        KDU  680
      XLA = 1.0 / SPARE(50)                                             KDU  690
      GOTO 103                                                          KDU  700
  102 XLD = 0.0                                                         KDU  710
      XLA = XLAMDA                                                      KDU  720
  103 CONTINUE                                                          KDU  730
      DO 129 KT1=1,KMAX                                                 KDU  740
        IF(IX37 .EQ. 0) GOTO 106                                        KDU  750
        READ (IOADJ) AIO                                                KDU  760
        IF(IX(71) .GT. 0) GOTO 104                                      KDU  770
        K = KT1                                                         KDU  780
        GOTO 105                                                        KDU  790
  104   K = KMAXP1 - KT1                                                KDU  800
  105   N = 1                                                           KDU  810
        GOTO 109                                                        KDU  820
  106   CONTINUE                                                        KDU  830
        IF(IX(24) .GT. 0) GOTO 107                                      KDU  840
        K = KT1                                                         KDU  850
        GOTO 108                                                        KDU  860
  107   K = KMAXP1 - KT1                                                KDU  870
  108   N = K                                                           KDU  880
  109   CONTINUE                                                        KDU  890
        DO 128 KB=1,KBMAX                                               KDU  900
          DO 127 I=1,IMAX                                               KDU  910
            NN1 = (I-1) * JVX                                           KDU  920
            NN2 = (I-1) * JVXP1                                         KDU  930
            DO 126 J=1,JMAX                                             KDU  940
              NOE = J - (J/2) * 2                                       KDU  950
              N1 = NN1 + J                                              KDU  960
              N2 = N1 - JVX                                             KDU  970
              N3 = N1 + JVX                                             KDU  980
              N4 = N1 - 1                                               KDU  990
              N5 = N1 + 1                                               KDU 1000
              N6 = N3 - 1                                               KDU 1010
              N7 = N2 + 1                                               KDU 1020
              N8 = NN2 + J                                              KDU 1030
              N10 = N8 + 1                                              KDU 1040
              T1 = P2E(N1,KB,K)                                         KDU 1050
              IF(T1 .EQ. 0.0) GOTO 126                                  KDU 1060
              L = NRGNE(J,I,KB)                                         KDU 1070
              M = NCOMP(L)                                              KDU 1080
              TT5 = 0.0                                                 KDU 1090
              IF(IX(24) .EQ. 0) GOTO 111                                KDU 1100
              DO 110 KK=1,KMAX                                          KDU 1110
                TT5 = TT5 + SCAC(K,M,KK) * P2E(N1,KB,KK)                KDU 1120
  110         CONTINUE                                                  KDU 1130
              GOTO 113                                                  KDU 1140
  111         CONTINUE                                                  KDU 1150
              DO 112 KK=1,KMAX                                          KDU 1160
                TT5 = TT5 + SCAC(KK,M,K) * P2E(N1,KB,KK)                KDU 1170
  112         CONTINUE                                                  KDU 1180
  113         CONTINUE                                                  KDU 1190
              TT1 = (SIG(K,M,3)+SIG(K,M,9)+XLD*SIG(K,M,5)) * T1 *       KDU 1200
     1         PVOL(L)                                                  KDU 1210
              TT3 = SIG(K,M,2) * T1                                     KDU 1220
              TDR = DCONRE(N10,KB,N)                                    KDU 1230
              IF(TDR .EQ. 4096.0E-13) TDR = 0.0                         KDU 1240
              T2 = DCONBE(N1,KB,N)                                      KDU 1250
              T3 = DCONBE(N3,KB,N)                                      KDU 1260
              IF(NUAC(5) .NE. 14) GOTO 115                              KDU 1270
              T2 = 0.0                                                  KDU 1280
              T3 = 0.0                                                  KDU 1290
              IF(NOE .EQ. 0) GOTO 114                                   KDU 1300
              N2 = N2 + 1                                               KDU 1310
              T2 = DCONBE(N1,KB,N)                                      KDU 1320
              GOTO 115                                                  KDU 1330
  114         CONTINUE                                                  KDU 1340
              N3 = N3 - 1                                               KDU 1350
              T3 = DCONBE(N3,KB,N)                                      KDU 1360
  115         CONTINUE                                                  KDU 1370
              IF(I .NE. 1) P2EUT = P2E(N2,KB,K)                         KDU 1380
              IF(I .NE. IMAX) P2EUB = P2E(N3,KB,K)                      KDU 1390
              IF(J .NE. 1) P2EUL = P2E(N4,KB,K)                         KDU 1400
              IF(J .NE. JMAX) P2EUR = P2E(N5,KB,K)                      KDU 1410
              IF(KB .NE. 1) P2EUF = P2E(N1,KB-1,K)                      KDU 1420
              IF(KB .NE. KBMAX) P2EUK = P2E(N1,KB+1,K)                  KDU 1430
              IF(I .EQ. 1) P2EUT = 0.0                                  KDU 1440
              IF(I .NE. IMAX) GOTO 116                                  KDU 1450
              P2EUB = 0.0                                               KDU 1460
              IF(INRB .NE. 3) GOTO 116                                  KDU 1470
              N13 = J * JVX                                             KDU 1480
              IF(NUAC(5) .EQ. 14) N13 = N13 / 2                         KDU 1490
              P2EUB = P2E(N13,KB,K)                                     KDU 1500
  116         CONTINUE                                                  KDU 1510
              IF(J .NE. 1) GOTO 117                                     KDU 1520
              P2EUL = 0.0                                               KDU 1530
              IF(INRB .NE. 2) GOTO 117                                  KDU 1540
              N12 = NN1 + JMAX                                          KDU 1550
              P2EUL = P2E(N12,KB,K)                                     KDU 1560
  117         IF(J .NE. JMAX) GOTO 121                                  KDU 1570
              P2EUR = 0.0                                               KDU 1580
              GOTO(121,118,119,120),INRB                                KDU 1590
  118         CONTINUE                                                  KDU 1600
              N11 = NN1 + 1                                             KDU 1610
              P2EUR = P2E(N11,KB,K)                                     KDU 1620
              GOTO 121                                                  KDU 1630
  119         CONTINUE                                                  KDU 1640
              N14 = (IVX-1) * JVX + I                                   KDU 1650
              IF(NUAC(5) .EQ. 14) N14 = N14 + I                         KDU 1660
              P2EUR = P2E(N14,KB,K)                                     KDU 1670
              GOTO 121                                                  KDU 1680
  120         CONTINUE                                                  KDU 1690
              N15 = (IVXP1-I) * JVX                                     KDU 1700
              P2EUR = P2E(N15,KB,K)                                     KDU 1710
  121         CONTINUE                                                  KDU 1720
              IF(KB .EQ. 1) P2EUF = 0.0                                 KDU 1730
              IF(KB .EQ. KBMAX) P2EUK = 0.0                             KDU 1740
              IF(NUAC(5) .EQ. 13) GOTO 122                              KDU 1750
              TT4 = (T1-P2EUT) * T2 + (T1-P2EUB) * T3 + (T1-P2EUL) *    KDU 1760
     1         DCONRE(N8,KB,N) + (T1-P2EUR) * TDR + (T1-P2EUF) *        KDU 1770
     2         DCONBK(N1,KB,N) + (T1-P2EUK) * DCONBK(N1,KB+1,N)         KDU 1780
              GOTO 123                                                  KDU 1790
C                                                                       KDU 1800
C     3-D HEXAGONAL RESIDUES                                            KDU 1810
C                                                                       KDU 1820
  122         KKK = N + IOVX                                            KDU 1830
              N9 = N8 + JVXP1                                           KDU 1840
              P2EUH = P2E(N6,KB,K)                                      KDU 1850
              P2EUI = P2E(N7,KB,K)                                      KDU 1860
              IF(I .EQ. 1 .OR. J .EQ. JMAX) P2EUI = 0.0                 KDU 1870
              IF(J .EQ. 1 .OR. I .EQ. IMAX) P2EUH = 0.0                 KDU 1880
              TT4 = (T1-P2EUT) * DCONBE(N1,KB,N) + (T1-P2EUB) *         KDU 1890
     1         DCONBE(N3,KB,N) + (T1-P2EUL) * DCONRE(N8,KB,N) +         KDU 1900
     2         (T1-P2EUR) * TDR + (T1-P2EUF) * DCONBK(N1,KB,N) +        KDU 1910
     3         (T1-P2EUK) * DCONBK(N1,KB+1,N) + (T1-P2EUH) *            KDU 1920
     4         DCONRE(N9,KB,KKK) + (T1-P2EUI) * DCONRE(N10,KB,KKK)      KDU 1930
  123         CONTINUE                                                  KDU 1940
              IF(IX(24) .EQ. 0) GOTO 124                                KDU 1950
              TT6 = SIG(K,M,4) * PVOL(L) * SOURE(J,I,KB) * XKEF1        KDU 1960
              GOTO 125                                                  KDU 1970
  124         TT6 = XI(K) * SOURE(J,I,KB) + XLD * SIG(K,M,8) * PVOL(L)  KDU 1980
  125         CONTINUE                                                  KDU 1990
              TT3 = PVOL(L) * (TT3-TT5)                                 KDU 2000
              TTT1 = TTT1 + TT6 * (TT4+TT1+TT3)                         KDU 2010
              TTT2 = TTT2 + TT6**2                                      KDU 2020
              TTT3 = TTT3 + TT1 * (XLA*TT6-TT4-TT3)                     KDU 2030
              TTT4 = TTT4 + TT1**2                                      KDU 2040
  126       CONTINUE                                                    KDU 2050
  127     CONTINUE                                                      KDU 2060
  128   CONTINUE                                                        KDU 2070
  129 CONTINUE                                                          KDU 2080
      IF(IX37 .GT. 0) REWIND IOADJ                                      KDU 2090
      RESLM = TTT2 / TTT1                                               KDU 2100
      RESSA = TTT3 / TTT4                                               KDU 2110
      IF(NUAC(3) .LE. 0) GOTO 130                                       KDU 2120
      XKEF2 = XKEF1                                                     KDU 2130
      XKEF1 = RESLM                                                     KDU 2140
      XLAMDA = 1.0 / XKEF1                                              KDU 2150
      VRGK1 = ABS(XKEF2/XKEF1-1.0)                                      KDU 2160
  130 CONTINUE                                                          KDU 2170
      RETURN                                                            KDU 2180
      END                                                               KDU 2190
      SUBROUTINE KTRI(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR,NRGNE, KTR   10
     1 E1,LVX,IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,JIVX,JIP1VX,JP1IXZ,   KTR   20
     2 IOVX,IOVZ)                                                       KTR   30
C                                                                       KTR   40
CKTRI 125.1 ***CITATION*** LINE RELAX ON ROWS 3-D TRIANGLE GEOM/CF-KNSD KTR   50
C                                                                       KTR   60
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KTR   70
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KTR   80
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KTR   90
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KTR  100
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KTR  110
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KTR  120
     6 IXPUT(9999),XPUT(9999)                                           KTR  130
C                                                                       KTR  140
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   KTR  150
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKKTR  160
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    KTR  170
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  KTR  180
     4 ITMAX,ITIME,BET(211),DEL(211)                                    KTR  190
C                                                                       KTR  200
      DIMENSION SCATE(JVX,IVX,KBVX),P2E(JIVX,KBVX,KVX),                 KTR  210
     1 DCONRE(JP1IXZ,KBVX,IOVZ),DCONBK(JIVX,KBVXP1,IOVX),               KTR  220
     2 PTSAE(JIVX,KBVX,IOVX),DCONBE(JIP1VX,KBVX,IOVX),E1(LVX,KVX),      KTR  230
     3 NRGNE(JVX,IVX,KBVX),TSOUR(211)                                   KTR  240
C                                                                       KTR  250
CCCC ********** SUBSCRIPT DEFINITIONS (KWRD E-070) ********* CCCCC      KTR  260
C    NEW         OLD            NEW         OLD                         KTR  270
C     N1         J,I             N5 *     J+1,I                         KTR  280
C     N2         J,I-1           N6       JVX,I                         KTR  290
C     N3         J,I+1           N14        M,L                         KTR  300
C     N4         N/A             N15        I,J                         KTR  310
C     M2 = J-1,I+1                                                      KTR  320
C     M3 = J+1,I-1                                                      KTR  330
C                                                                       KTR  340
C                                                                       KTR  350
      INRB = IX(72) + 1                                                 KTR  360
      N = IX(20)                                                        KTR  370
      DO 136 KB=1,KBVX                                                  KTR  380
        ASSIGN 100 TO IBR1                                              KTR  390
        IF(KB .LE. 1) ASSIGN 101 TO IBR1                                KTR  400
        ASSIGN 102 TO IBR2                                              KTR  410
        IF(KB .GE. KBVX) ASSIGN 103 TO IBR2                             KTR  420
        KBM1 = KB - 1                                                   KTR  430
        KBP1 = KB + 1                                                   KTR  440
        DO 135 I=1,IVX                                                  KTR  450
          ASSIGN 104 TO IBR3                                            KTR  460
          IF(I .LE. 1) ASSIGN 107 TO IBR3                               KTR  470
          ASSIGN 106 TO IBR4                                            KTR  480
          IF(I .GE. IVX) ASSIGN 107 TO IBR4                             KTR  490
          IM1 = I - 1                                                   KTR  500
          NN1 = IM1 * JVX                                               KTR  510
          NN3 = NN1 + JVX                                               KTR  520
          MM2 = I * JVX - 1                                             KTR  530
          MM3 = (I-2) * JVX + 1                                         KTR  540
          N1 = NN1                                                      KTR  550
          M2 = MM2                                                      KTR  560
          M3 = MM3                                                      KTR  570
          DO 108 J=1,JVX                                                KTR  580
            NOE = J - (J/2) * 2                                         KTR  590
            N1 = N1 + 1                                                 KTR  600
            M2 = M2 + 1                                                 KTR  610
            M3 = M3 + 1                                                 KTR  620
            CKSS = SCATE(J,I,KB)                                        KTR  630
            GOTO IBR1,(100,101)                                         KTR  640
  100       CKSS = CKSS + P2E(N1,KBM1,K) * DCONBK(N1,KB,N)              KTR  650
  101       GOTO IBR2,(102,103)                                         KTR  660
  102       CKSS = CKSS + P2E(N1,KBP1,K) * DCONBK(N1,KBP1,N)            KTR  670
  103       CONTINUE                                                    KTR  680
            IF(NOE .EQ. 0) GOTO 105                                     KTR  690
            GOTO IBR3,(104,107)                                         KTR  700
  104       CKSS = CKSS + P2E(M3,KB,K) * DCONBE(N1,KB,N)                KTR  710
            GOTO 107                                                    KTR  720
  105       GOTO IBR4,(106,107)                                         KTR  730
  106       CKSS = CKSS + P2E(M2,KB,K) * DCONBE(M2,KB,N)                KTR  740
  107       TSOUR(J) = CKSS                                             KTR  750
  108     CONTINUE                                                      KTR  760
          NN4 = IM1 * JVXP1                                             KTR  770
          N4 = NN4 + 1                                                  KTR  780
          N5 = N4 + 1                                                   KTR  790
          N1 = NN1 + 1                                                  KTR  800
          D4 = DCONRE(N5,KB,N)                                          KTR  810
          IF(P2E(N1,KB,K) .EQ. 0.0) GOTO 109                            KTR  820
          L = NRGNE(1,I,KB)                                             KTR  830
          BET(1) = TSOUR(1) / D4                                        KTR  840
          DEL(1) = D4 / (PTSAE(N1,KB,N)+E1(L,K))                        KTR  850
          IF(INRB .EQ. 2) BET(1) = BET(1) + P2E(NN3,KB,K) *             KTR  860
     1     DCONRE(N4,KB,N) / D4                                         KTR  870
          GOTO 110                                                      KTR  880
  109     DEL(1) = 0.0                                                  KTR  890
  110     CONTINUE                                                      KTR  900
          DO 112 J=2,JVX                                                KTR  910
            N1 = N1 + 1                                                 KTR  920
            N5 = N5 + 1                                                 KTR  930
            IF(P2E(N1,KB,K) .EQ. 0.0) GOTO 111                          KTR  940
            L = NRGNE(J,I,KB)                                           KTR  950
            T = D4 * DEL(J-1)                                           KTR  960
            D4 = DCONRE(N5,KB,N)                                        KTR  970
            BET(J) = (TSOUR(J)+BET(J-1)*T) / D4                         KTR  980
            DEL(J) = D4 / (PTSAE(N1,KB,N)+E1(L,K)-T)                    KTR  990
            IF(INRB .NE. 3) GOTO 112                                    KTR 1000
            IF(I .NE. IMAX) GOTO 112                                    KTR 1010
            IF(((J/2)*2) .NE. J) GOTO 112                               KTR 1020
            IF(J .EQ. JVX) GOTO 112                                     KTR 1030
            NN12 = (J/2) * JVX                                          KTR 1040
            MM22 = IVX * JVX + J - 1                                    KTR 1050
            BET(J) = BET(J) + P2E(NN12,KB,K) * DCONBE(MM22,KB,N) / D4   KTR 1060
            GOTO 112                                                    KTR 1070
  111       DEL(J) = 0.0                                                KTR 1080
  112     CONTINUE                                                      KTR 1090
          GOTO(116,113,114,115),INRB                                    KTR 1100
  113     CONTINUE                                                      KTR 1110
          N222 = NN1 + 1                                                KTR 1120
          BET(JVX) = BET(JVX) + P2E(N222,KB,K)                          KTR 1130
          GOTO 116                                                      KTR 1140
  114     CONTINUE                                                      KTR 1150
          IF(I .EQ. IVX) GOTO 116                                       KTR 1160
          N223 = (IVX-1) * JVX + 2 * I                                  KTR 1170
          BET(JVX) = BET(JVX) + P2E(N223,KB,K)                          KTR 1180
          GOTO 116                                                      KTR 1190
  115     CONTINUE                                                      KTR 1200
          N224 = (IVXP1-I-1) * JVX + JVX                                KTR 1210
          BET(JVX) = BET(JVX) + P2E(N224,KB,K)                          KTR 1220
  116     CONTINUE                                                      KTR 1230
          N6 = NN3                                                      KTR 1240
          TEMP = BET(JVX) * DEL(JVX)                                    KTR 1250
          T = P2E(N6,KB,K)                                              KTR 1260
          TMF = T + BETTA * (TEMP-T)                                    KTR 1270
          IF(IEP) 117,121,118                                           KTR 1280
  117     P2E(N6,KB,K) = TEMP                                           KTR 1290
          GOTO 122                                                      KTR 1300
  118     IF(TMF-TEMP) 120,121,119                                      KTR 1310
  119     TMF = AMIN1(TMF,(TEMP+T))                                     KTR 1320
          GOTO 121                                                      KTR 1330
  120     TMF = AMAX1(TMF,0.5*TEMP)                                     KTR 1340
  121     CONTINUE                                                      KTR 1350
          P2E(N6,KB,K) = TMF                                            KTR 1360
  122     DO 129 JJ=2,JVX                                               KTR 1370
            J = JVXP1 - JJ                                              KTR 1380
            N1 = NN1 + J                                                KTR 1390
            T = P2E(N1,KB,K)                                            KTR 1400
            TEMP = DEL(J) * (TEMP+BET(J))                               KTR 1410
            TMF = T + BETTA * (TEMP-T)                                  KTR 1420
            IF(IEP) 123,127,124                                         KTR 1430
  123       P2E(N1,KB,K) = TEMP                                         KTR 1440
            GOTO 129                                                    KTR 1450
  124       IF(TMF-TEMP) 126,127,125                                    KTR 1460
  125       TMF = AMIN1(TMF,(TEMP+T))                                   KTR 1470
            GOTO 127                                                    KTR 1480
  126       TMF = AMAX1(TMF,0.5*TEMP)                                   KTR 1490
  127       CONTINUE                                                    KTR 1500
            P2E(N1,KB,K) = TMF                                          KTR 1510
  129     CONTINUE                                                      KTR 1520
          IF(NUAC(8)) 130,134,132                                       KTR 1530
  130     L = IVXP1 - I                                                 KTR 1540
          NN7 = (L-1) * JVX                                             KTR 1550
          DO 131 J=1,JVX                                                KTR 1560
            M = JVXP1 - J                                               KTR 1570
            N1 = NN1 + J                                                KTR 1580
            N14 = NN7 + M                                               KTR 1590
            P2E(N14,KB,K) = P2E(N1,KB,K)                                KTR 1600
  131     CONTINUE                                                      KTR 1610
          GOTO 134                                                      KTR 1620
  132     DO 133 J=1,JVX                                                KTR 1630
            N1 = NN1 + J                                                KTR 1640
            N15 = (J-1) *JVX + I                                        KTR 1650
            P2E(N15,KB,K) = P2E(N1,KB,K)                                KTR 1660
  133     CONTINUE                                                      KTR 1670
  134     CONTINUE                                                      KTR 1680
  135   CONTINUE                                                        KTR 1690
  136 CONTINUE                                                          KTR 1700
      RETURN                                                            KTR 1710
      END                                                               KTR 1720
      FUNCTION ICLOCK(N)                                                ICL   10
C                                                                       ICL   20
      CALL WATCH(ENDE)                                                  ICL   30
C                                                                       ICL   40
C                                                                       ICL   50
      ICLOCK = IFIX(ENDE)                                               ICL   60
      RETURN                                                            ICL   70
      END                                                               ICL   80
      SUBROUTINE IDAY(X)                                                IDA   10
C                                                                       IDA   20
      CHARACTER*5 X,Y/'TODAY'/                                          IDA   30
C                                                                       IDA   40
C                                                                       IDA   50
      X = Y                                                             IDA   60
      RETURN                                                            IDA   70
      END                                                               IDA   80