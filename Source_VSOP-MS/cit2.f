      SUBROUTINE MWRD(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR,NRGNE, MWR   10
     1 E1,LVX,IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,JIVX,JIP1VX,JP1IXZ,   MWR   20
     2 IOVX,IOVZ)                                                       MWR   30
C                                                                       MWR   40
CMWRD --125***CITATION*** LINE RELAXATION ON ROWS 3-D HEX/ CF-KNSD      MWR   50
C                                                                       MWR   60
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,MWR   70
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   MWR   80
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), MWR   90
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    MWR  100
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    MWR  110
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   MWR  120
     6 IXPUT(9999),XPUT(9999)                                           MWR  130
C                                                                       MWR  140
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   MWR  150
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKMWR  160
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    MWR  170
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  MWR  180
     4 ITMAX,ITIME,BET(211),DEL(211)                                    MWR  190
C                                                                       MWR  200
      DIMENSION SCATE(JVX,IVX,KBVX),P2E(JIVX,KBVX,KVX),                 MWR  210
     1 DCONBE(JIP1VX,KBVX,IOVX),DCONRE(JP1IXZ,KBVX,IOVZ),               MWR  220
     2 DCONBK(JIVX,KBVXP1,IOVX),PTSAE(JIVX,KBVX,IOVX),E1(LVX,KVX),      MWR  230
     3 NRGNE(JVX,IVX,KBVX),TSOUR(211)                                   MWR  240
C                                                                       MWR  250
CCCCC ********* SUBSCRIPT DEFINITIONS (MWRD E-090) ********* CCCCC      MWR  260
C    NEW         OLD            NEW         OLD                         MWR  270
C     N1         J,I             N14        M,L                         MWR  280
C     N2         J,I-1           N15        I,J                         MWR  290
C     N3         J,I+1           N16      J-1,2                         MWR  300
C     N4         J,1             N17        2,I-1                       MWR  310
C     N5         J,2             N18      J+1,I-1                       MWR  320
C     N6         J,IVX           N19      J-1,I+1                       MWR  330
C     N7         1,1             N20 *      2,I                         MWR  340
C     N8         1,2             N21 *    J+1,1                         MWR  350
C     N9         1,IVX           N22 *      J,2                         MWR  360
C     N10        1,I             N23 *      2,IVX                       MWR  370
C     N11        1,I-1           N24 *    J+1,IVX                       MWR  380
C     N12        1,I+1           N25 *    J+1,I                         MWR  390
C     N13      JVX,I             N26 *      J,I+1                       MWR  400
C                                                                       MWR  410
C                                                                       MWR  420
      N = IX(20)                                                        MWR  430
      N7 = 1                                                            MWR  440
      N8 = JVX + 1                                                      MWR  450
      NN4 = (IVX-1) * JVX                                               MWR  460
      NN6 = (IVX-1) * JVXP1                                             MWR  470
      N9 = NN4 + 1                                                      MWR  480
      N23 = NN6 + 2                                                     MWR  490
      KKK = N + IOVX                                                    MWR  500
      DO 144 KB=1,KBVX                                                  MWR  510
        KBM1 = KB - 1                                                   MWR  520
        KBP1 = KB + 1                                                   MWR  530
        DO 143 I=1,IVX                                                  MWR  540
          IP1 = I - 1                                                   MWR  550
          NN1 = IP1 * JVX                                               MWR  560
          DO 103 J=1,JVX                                                MWR  570
            N1 = NN1 + J                                                MWR  580
            CKSS = SCATE(J,I,KB)                                        MWR  590
            IF(KB .LE. 1) GOTO 101                                      MWR  600
            CKSS = CKSS + P2E(N1,KBM1,K) * DCONBK(N1,KB,N)              MWR  610
  101       IF(KB .GE. KBVX) GOTO 102                                   MWR  620
            CKSS = CKSS + P2E(N1,KBP1,K) * DCONBK(N1,KBP1,N)            MWR  630
  102       TSOUR(J) = CKSS                                             MWR  640
  103     CONTINUE                                                      MWR  650
          NN2 = NN1 - JVX                                               MWR  660
          NN3 = NN1 + JVX                                               MWR  670
          NN5 = IP1 * JVXP1                                             MWR  680
          NN7 = NN5 + JVXP1                                             MWR  690
          N11 = NN2 + 1                                                 MWR  700
          N17 = NN2 + 2                                                 MWR  710
          N13 = NN1 + JVX                                               MWR  720
          N20 = NN5 + 2                                                 MWR  730
          N10 = NN1 + 1                                                 MWR  740
          N12 = NN3 + 1                                                 MWR  750
          DEL(1) = 0.0                                                  MWR  760
          D1 = DCONBE(N10,KB,N)                                         MWR  770
          D2 = DCONBE(N12,KB,N)                                         MWR  780
          D4 = DCONRE(N20,KB,N)                                         MWR  790
          IF(I-1) 105,105,111                                           MWR  800
  105     IF(P2E(N7,KB,K)) 106,107,106                                  MWR  810
  106     BET(1) = (P2E(N8,KB,K)*D2+TSOUR(1)) / D4                      MWR  820
          L = NRGNE(1,1,KB)                                             MWR  830
          DEL(1) = D4 / (PTSAE(N7,KB,N)+E1(L,K))                        MWR  840
  107     CONTINUE                                                      MWR  850
          N4 = 1                                                        MWR  860
          N21 = 2                                                       MWR  870
          N5 = JVX + 1                                                  MWR  880
          N16 = JVX                                                     MWR  890
          N22 = JVXP1 + 1                                               MWR  900
          DO 110 J=2,JVX                                                MWR  910
            N4 = N4 + 1                                                 MWR  920
            N21 = N21 + 1                                               MWR  930
            N5 = N5 + 1                                                 MWR  940
            N16 = N16 + 1                                               MWR  950
            N22 = N22 + 1                                               MWR  960
            IF(P2E(N4,KB,K)) 109,108,109                                MWR  970
  108       DEL(J) = 0.0                                                MWR  980
            GOTO 110                                                    MWR  990
  109       T = D4 * DEL(J-1)                                           MWR 1000
            L = NRGNE(J,1,KB)                                           MWR 1010
            D4 = DCONRE(N21,KB,N)                                       MWR 1020
            BET(J) = (P2E(N5,KB,K)*DCONBE(N5,KB,N)+TSOUR(J)+            MWR 1030
     1       P2E(N16,KB,K)*DCONRE(N22,KB,KKK)+BET(J-1)*T) / D4          MWR 1040
            DEL(J) = D4 / (PTSAE(N4,KB,N)-T+E1(L,K))                    MWR 1050
  110     CONTINUE                                                      MWR 1060
          GOTO 124                                                      MWR 1070
  111     IF(I-IVX) 118,112,112                                         MWR 1080
  112     IF(P2E(N9,KB,K)) 113,114,113                                  MWR 1090
  113     BET(1) = (P2E(N11,KB,K)*D1+TSOUR(1)+P2E(N17,KB,K)*            MWR 1100
     1     DCONRE(N23,KB,KKK)) / D4                                     MWR 1110
          L = NRGNE(1,IVX,KB)                                           MWR 1120
          DEL(1) = D4 / (PTSAE(N9,KB,N)+E1(L,K))                        MWR 1130
  114     CONTINUE                                                      MWR 1140
          N6 = NN4 + 1                                                  MWR 1150
          N24 = NN6 + 2                                                 MWR 1160
          N2 = NN2 + 1                                                  MWR 1170
          N18 = NN2 + 2                                                 MWR 1180
          DO 117 J=2,JVX                                                MWR 1190
            N6 = N6 + 1                                                 MWR 1200
            N24 = N24 + 1                                               MWR 1210
            N2 = N2 + 1                                                 MWR 1220
            N18 = N18 + 1                                               MWR 1230
            IF(P2E(N6,KB,K)) 116,115,116                                MWR 1240
  115       DEL(J) = 0.0                                                MWR 1250
            GOTO 117                                                    MWR 1260
  116       T = D4 * DEL(J-1)                                           MWR 1270
            L = NRGNE(J,IVX,KB)                                         MWR 1280
            D4 = DCONRE(N24,KB,N)                                       MWR 1290
            BET(J) = (P2E(N2,KB,K)*DCONBE(N6,KB,N)+TSOUR(J)+            MWR 1300
     1       P2E(N18,KB,K)*DCONRE(N24,KB,KKK)+BET(J-1)*T) / D4          MWR 1310
            DEL(J) = D4 / (PTSAE(N6,KB,N)-T+E1(L,K))                    MWR 1320
  117     CONTINUE                                                      MWR 1330
          GOTO 124                                                      MWR 1340
  118     IF(P2E(N10,KB,K)) 119,120,119                                 MWR 1350
  119     CONTINUE                                                      MWR 1360
          L = NRGNE(1,I,KB)                                             MWR 1370
          BET(1) = (P2E(N11,KB,K)*D1+TSOUR(1)+P2E(N17,KB,K)*            MWR 1380
     1     DCONRE(N20,KB,KKK)+P2E(N12,KB,K)*D2) / D4                    MWR 1390
          DEL(1) = D4 / (PTSAE(N10,KB,N)+E1(L,K))                       MWR 1400
  120     CONTINUE                                                      MWR 1410
          N1 = NN1 + 1                                                  MWR 1420
          N25 = NN5 + 2                                                 MWR 1430
          N2 = NN2 + 1                                                  MWR 1440
          N3 = NN3 + 1                                                  MWR 1450
          N18 = NN2 + 2                                                 MWR 1460
          N19 = NN3                                                     MWR 1470
          N26 = NN7 + 1                                                 MWR 1480
          DO 123 J=2,JVX                                                MWR 1490
            N1 = N1 + 1                                                 MWR 1500
            N25 = N25 + 1                                               MWR 1510
            N2 = N2 + 1                                                 MWR 1520
            N3 = N3 + 1                                                 MWR 1530
            N18 = N18 + 1                                               MWR 1540
            N19 = N19 + 1                                               MWR 1550
            N26 = N26 + 1                                               MWR 1560
            IF(P2E(N1,KB,K)) 122,121,122                                MWR 1570
  121       DEL(J) = 0.0                                                MWR 1580
            GOTO 123                                                    MWR 1590
  122       T = D4 * DEL(J-1)                                           MWR 1600
            L = NRGNE(J,I,KB)                                           MWR 1610
            D4 = DCONRE(N25,KB,N)                                       MWR 1620
            BET(J) = (P2E(N2,KB,K)*DCONBE(N1,KB,N)+P2E(N3,KB,K)*        MWR 1630
     1       DCONBE(N3,KB,N)+P2E(N18,KB,K)*DCONRE(N25,KB,KKK)+          MWR 1640
     2       P2E(N19,KB,K)*DCONRE(N26,KB,KKK)+TSOUR(J)+BET(J-1)*T) / D4 MWR 1650
            DEL(J) = D4 / (PTSAE(N1,KB,N)-T+E1(L,K))                    MWR 1660
  123     CONTINUE                                                      MWR 1670
  124     CONTINUE                                                      MWR 1680
          TEMP = BET(JVX) * DEL(JVX)                                    MWR 1690
          T = P2E(N13,KB,K)                                             MWR 1700
          TMF = T + BETTA * (TEMP-T)                                    MWR 1710
          IF(IEP) 126,130,127                                           MWR 1720
  126     P2E(N13,KB,K) = TEMP                                          MWR 1730
          GOTO 131                                                      MWR 1740
  127     IF(TMF-TEMP) 129,130,128                                      MWR 1750
  128     TMF = AMIN1(TMF,(TEMP+T))                                     MWR 1760
          GOTO 130                                                      MWR 1770
  129     TMF = AMAX1(TMF,0.5*TEMP)                                     MWR 1780
  130     CONTINUE                                                      MWR 1790
          P2E(N13,KB,K) = TMF                                           MWR 1800
  131     DO 138 JJ=2,JVX                                               MWR 1810
            J = JVXP1 - JJ                                              MWR 1820
            N1 = NN1 + J                                                MWR 1830
            T = P2E(N1,KB,K)                                            MWR 1840
            TEMP = DEL(J) * (TEMP+BET(J))                               MWR 1850
            TMF = T + BETTA * (TEMP-T)                                  MWR 1860
            IF(IEP) 132,136,133                                         MWR 1870
  132       P2E(N1,KB,K) = TEMP                                         MWR 1880
            GOTO 138                                                    MWR 1890
  133       IF(TMF-TEMP) 135,136,134                                    MWR 1900
  134       TMF = AMIN1(TMF,(TEMP+T))                                   MWR 1910
            GOTO 136                                                    MWR 1920
  135       TMF = AMAX1(TMF,0.5*TEMP)                                   MWR 1930
  136       CONTINUE                                                    MWR 1940
            P2E(N1,KB,K) = TMF                                          MWR 1950
  138     CONTINUE                                                      MWR 1960
          IF(NUAC(8)) 139,143,141                                       MWR 1970
  139     L = IVXP1 - I                                                 MWR 1980
          DO 140 J=1,JVX                                                MWR 1990
            M = JVXP1 - J                                               MWR 2000
            N1 = NN1 + J                                                MWR 2010
            N14 = (L-1) * JVX + M                                       MWR 2020
            P2E(N14,KB,K) = P2E(N1,KB,K)                                MWR 2030
  140     CONTINUE                                                      MWR 2040
          GOTO 143                                                      MWR 2050
  141     DO 142 J=1,JVX                                                MWR 2060
            N1 = NN1 + J                                                MWR 2070
            N15 = (J-1) * JVX + I                                       MWR 2080
            P2E(N15,KB,K) = P2E(N1,KB,K)                                MWR 2090
  142     CONTINUE                                                      MWR 2100
  143   CONTINUE                                                        MWR 2110
  144 CONTINUE                                                          MWR 2120
      RETURN                                                            MWR 2130
      END                                                               MWR 2140
      SUBROUTINE NMBL(B1,B4,P2,RVOL,NCOMP,SIG,P2E,XL,IVX,JVX,KBVX,KVX,  NMB   10
     1 LVX,MVX,JIVX,B2,B5)                                              NMB   20
C                                                                       NMB   30
CNMBL --126 ***CITATION*** CALC. NEUTRON BALANCE/ CF-EIGN               NMB   40
C                                                                       NMB   50
      REAL*8 P2,B1                                                      NMB   60
C                                                                       NMB   70
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,NMB   80
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   NMB   90
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), NMB  100
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    NMB  110
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    NMB  120
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   NMB  130
     6 IXPUT(9999),XPUT(9999)                                           NMB  140
C                                                                       NMB  150
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  NMB  160
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), NMB  170
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)NMB  180
     3 ,PDK(211)                                                        NMB  190
C                                                                       NMB  200
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   NMB  210
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKNMB  220
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    NMB  230
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  NMB  240
     4 ITMAX,ITIME,BET(211),DEL(211)                                    NMB  250
C                                                                       NMB  260
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           NMB  270
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    NMB  280
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),NMB  290
     3 NCLASS(9999),NDP(9999)                                           NMB  300
C                                                                       NMB  310
      COMMON /BLOCK1/ IDUM(17),JTPE3                                    NMB  320
C                                                                       NMB  330
      EQUIVALENCE(JTPE3,NT)                                             NMB  340
C                                                                       NMB  350
      DIMENSION XL(6,KVX),B1(MVX,KVX),B4(MVX,KVX),P2(JVX,IVX,KVX),      NMB  360
     1 RVOL(LVX),NCOMP(LVX),SIG(KVX,MVX,10),P2E(JIVX,KBVX,KVX),         NMB  370
     2 B2(MVX,KVX),B5(MVX,KVX)                                          NMB  380
C                                                                       NMB  390
C                                                                       NMB  400
      CORPW = SPARE(100)                                                NMB  410
      RECPRK = XLAMDA                                                   NMB  420
      SPARE(27) = XLAMDA                                                NMB  430
      IF(IX(5) .NE. -5) GOTO 101                                        NMB  440
      SPARE(18) = 1.0                                                   NMB  450
      IF(XLAMDA .GE. 0.0) GOTO 102                                      NMB  460
      XABS = (XABS+XLEK) / XLAMDA                                       NMB  470
      PROD = PROD / XLAMDA                                              NMB  480
      WRITE (IOUT,1000) SPARE(88),PROD,XABS                             NMB  490
C                                                                       NMB  500
      CALL EXIT                                                         NMB  510
C                                                                       NMB  520
  101 CONTINUE                                                          NMB  530
C                                                                       NMB  540
C********SEARCH OPTIONS                                                 NMB  550
C                                                                       NMB  560
      IF(IX(5) .EQ. 1 .OR. IX(5) .LT. 0) RECPRK = 1.0 / XKEF1           NMB  570
      SPARE(18) = RECPRK                                                NMB  580
  102 CONTINUE                                                          NMB  590
      T1 = 0.0                                                          NMB  600
      DO 104 M=1,MMAX                                                   NMB  610
        ZONVOL(M) = 0.0                                                 NMB  620
        DO 103 K=1,KMAX                                                 NMB  630
          T1 = T1 + B1(M,K) * SIG(K,M,7)                                NMB  640
  103   CONTINUE                                                        NMB  650
  104 CONTINUE                                                          NMB  660
      DO 105 L=1,LMAX                                                   NMB  670
        M = NCOMP(L)                                                    NMB  680
        ZONVOL(M) = ZONVOL(M) + RVOL(L)                                 NMB  690
  105 CONTINUE                                                          NMB  700
      IF(IX(5) .NE. -5) GOTO 106                                        NMB  710
      XKEF3 = 1.0                                                       NMB  720
      IF(XLAMDA .NE. 0.0) XKEF3 = 1.0 / XLAMDA                          NMB  730
      GOTO 108                                                          NMB  740
  106 CONTINUE                                                          NMB  750
      XKEF3 = CORPW * 1.0E+6 * XMIS(5) / (XMIS(4)*T1)                   NMB  760
  108 CONTINUE                                                          NMB  770
      NGC11 = NGC(11)                                                   NMB  780
      DO 125 K=1,KMAX                                                   NMB  790
        DO 116 M=1,MMAX                                                 NMB  800
          B1(M,K) = B1(M,K) * XKEF3                                     NMB  810
          B5(M,K) = B2(M,K) * XKEF3                                     NMB  820
          IF(ZONVOL(M)) 109,110,109                                     NMB  830
  109     B4(M,K) = B1(M,K) / ZONVOL(M)                                 NMB  840
          GOTO 111                                                      NMB  850
  110     B4(M,K) = B1(M,K)                                             NMB  860
  111     IF(IX(5) .NE. -2) GOTO 115                                    NMB  870
          IF(NGC11) 112,113,114                                         NMB  880
  112     IF(M .NE. -NGC11) GOTO 115                                    NMB  890
  113     SIG(K,M,6) = SIG(K,M,6) * XLAMDA                              NMB  900
          GOTO 115                                                      NMB  910
  114     IF(NCLASS(M) .EQ. NGC11) GOTO 113                             NMB  920
  115     CONTINUE                                                      NMB  930
  116   CONTINUE                                                        NMB  940
        DO 117 N=1,6                                                    NMB  950
          XL(N,K) = XL(N,K) * XKEF3                                     NMB  960
  117   CONTINUE                                                        NMB  970
        IF(NUAC(5) .LE. 10) GOTO 121                                    NMB  980
        DO 120 KB=1,KBMAX                                               NMB  990
          N1 = 0                                                        NMB 1000
          DO 119 I=1,IMAX                                               NMB 1010
            DO 118 J=1,JMAX                                             NMB 1020
              N1 = N1 + 1                                               NMB 1030
              P2E(N1,KB,K) = P2E(N1,KB,K) * XKEF3                       NMB 1040
  118       CONTINUE                                                    NMB 1050
  119     CONTINUE                                                      NMB 1060
  120   CONTINUE                                                        NMB 1070
        GOTO 124                                                        NMB 1080
  121   DO 123 I=1,IMAX                                                 NMB 1090
          DO 122 J=1,JMAX                                               NMB 1100
            P2(J,I,K) = P2(J,I,K) * XKEF3                               NMB 1110
  122     CONTINUE                                                      NMB 1120
  123   CONTINUE                                                        NMB 1130
  124   CONTINUE                                                        NMB 1140
  125 CONTINUE                                                          NMB 1150
      XABS = XABS * XKEF3                                               NMB 1160
      PROD = PROD * XKEF3                                               NMB 1170
      XLEK = XLEK * XKEF3                                               NMB 1180
      T2 = T1 * XKEF3 / XMIS(5)                                         NMB 1190
      TT11 = XLEK + XABS                                                NMB 1200
      SPARE(56) = TT11                                                  NMB 1210
      WRITE (NT,1001) XLEK,TT11,PROD,T2                                 NMB 1220
      XLAMDA = RECPRK                                                   NMB 1230
      RETURN                                                            NMB 1240
C                                                                       NMB 1250
 1000 FORMAT (1H0,'THERE IS NO SOLUTION TO THIS FIXED SOURCE PROBLEM,  SNMB 1260
     1OURCE',1PE13.5,' PRODUCTIONS',1PE13.5,' LOSSES',1PE13.5)          NMB 1270
 1001 FORMAT (1H0,'    LEAKAGE',1PE13.5,2H  ,'TOTAL LOSSES',E13.5,2H  , NMB 1280
     1 'TOTAL PRODUCTIONS',E13.5,2H  ,'FISSION RATE (1/S)',E13.5)       NMB 1290
      END                                                               NMB 1300
      SUBROUTINE PDWT(P2,UTIL,NRGN,PVOL,IPRIN,IVX,JVX,KVX,LVX,SIG,NCOMP,PDW   10
     1 MVX)                                                             PDW   20
C                                                                       PDW   30
CPDWT --139 ***CITATION*** PRINT REL. POW. DENS. TRAVERSES/ CF-OUTC     PDW   40
C                                                                       PDW   50
      REAL*8 P2                                                         PDW   60
C                                                                       PDW   70
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,PDW   80
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   PDW   90
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), PDW  100
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    PDW  110
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    PDW  120
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   PDW  130
     6 IXPUT(9999),XPUT(9999)                                           PDW  140
C                                                                       PDW  150
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  PDW  160
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), PDW  170
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)PDW  180
     3 ,PDK(211)                                                        PDW  190
C                                                                       PDW  200
CFZJ055                                                       25.09.07  PDW  210
C                                                                       PDW  220
      DIMENSION P2(JVX,IVX,KVX),UTIL(JVX,IVX),NRGN(JVX,IVX),PVOL(LVX),  PDW  230
     1 SIG(KVX,MVX,10),NCOMP(LVX)                                       PDW  240
C                                                                       PDW  250
C                                                                       PDW  260
      T2 = PDI(211)                                                     PDW  270
      IAX = IMAX                                                        PDW  280
      IF(IMAX .GT. 200) IAX = 200                                       PDW  290
      DO 100 N=1,IAX                                                    PDW  300
        XTR1(N) = PDI(N)                                                PDW  310
  100 CONTINUE                                                          PDW  320
      DO 104 I=1,IMAX                                                   PDW  330
        DO 103 J=1,JMAX                                                 PDW  340
          L = NRGN(J,I)                                                 PDW  350
          M = NCOMP(L)                                                  PDW  360
          UTIL(J,I ) = 0.0                                              PDW  370
          DO 102 K=1,KMAX                                               PDW  380
            UTIL(J,I) = UTIL(J,I) + P2(J,I,K) * SIG(K,M,7)              PDW  390
  102     CONTINUE                                                      PDW  400
  103   CONTINUE                                                        PDW  410
  104 CONTINUE                                                          PDW  420
      IF(IPRIN .GT. 0) GOTO 115                                         PDW  430
C                                                                       PDW  440
      CALL RQED(IX(113),IND)                                            PDW  450
C                                                                       PDW  460
      IF(IND .NE. 0) GOTO 115                                           PDW  470
      T1 = 0.0                                                          PDW  480
      DO 108 I=1,IMAX                                                   PDW  490
        DO 107 J=1,JMAX                                                 PDW  500
          IF(UTIL(J,I)-T1) 107,106,106                                  PDW  510
  106     II = I                                                        PDW  520
          JJ = J                                                        PDW  530
          T1 = UTIL(J,I)                                                PDW  540
  107   CONTINUE                                                        PDW  550
  108 CONTINUE                                                          PDW  560
      XT1 = 0.0                                                         PDW  570
      XV1 = 0.0                                                         PDW  580
      YT1 = 0.0                                                         PDW  590
      YV1 = 0.0                                                         PDW  600
      DO 110 J=1,JMAX                                                   PDW  610
        L = NRGN(J,II)                                                  PDW  620
        IF(UTIL(J,II)) 109,110,109                                      PDW  630
  109   XT1 = XT1 + UTIL(J,II) * PVOL(L)                                PDW  640
        XV1 = XV1 + PVOL(L)                                             PDW  650
  110 CONTINUE                                                          PDW  660
      DO 112 I=1,IMAX                                                   PDW  670
        L = NRGN(JJ,I)                                                  PDW  680
        IF(UTIL(JJ,I)) 111,112,111                                      PDW  690
  111   YT1 = YT1 + UTIL(JJ,I) * PVOL(L)                                PDW  700
        YV1 = YV1 + PVOL(L)                                             PDW  710
  112 CONTINUE                                                          PDW  720
      XB1 = XT1 / XV1                                                   PDW  730
      YB1 = YT1 / YV1                                                   PDW  740
      DO 113 J=1,JMAX                                                   PDW  750
        PDJ(J) = UTIL(J,II) / XB1                                       PDW  760
  113 CONTINUE                                                          PDW  770
      DO 114 I=1,IMAX                                                   PDW  780
        PDI(I) = UTIL(JJ,I) / YB1                                       PDW  790
  114 CONTINUE                                                          PDW  800
      WRITE (IOUT,1000) II,JJ,UTIL(JJ,II)                               PDW  810
      WRITE (IOUT,1001) II,XB1,(PDJ(J),J=1,JMAX)                        PDW  820
      WRITE (IOUT,1002) JJ,YB1,(PDI(I),I=1,IMAX)                        PDW  830
  115 CONTINUE                                                          PDW  840
      PDI(211) = T2                                                     PDW  850
      DO 116 N=1,IAX                                                    PDW  860
        PDI(N) = XTR1(N)                                                PDW  870
  116 CONTINUE                                                          PDW  880
      RETURN                                                            PDW  890
C                                                                       PDW  900
 1000 FORMAT (1H0/1H0,'THE MAXIMUM POWER DENSITY',1H(,'WATTS/CC) AT ROW'PDW  910
     1 ,I4,2H  ,'AND COLUMN',I4,2H  ,'IS ',1PE14.6)                     PDW  920
 1001 FORMAT (1H0,'THE AVERAGE POWER DENSITY ALONG ROW',I4,2H  ,'IS',   PDW  930
     1 1PE14.6,2H  ,'AND THE RELATIVE POWER DENSITIES',1H(,'FRACTION OF PDW  940
     2AVERAGE) ARE'/(1H ,9E14.6))                                       PDW  950
 1002 FORMAT (1H0,'THE AVERAGE POWER DENSITY DOWN COLUMN',I4,2H  ,'IS', PDW  960
     1 1PE14.6,2H  ,'AND THE RELATIVE POWER DENSITIES',1H ,'ARE'/(1H ,  PDW  970
     2 9E14.6))                                                         PDW  980
      END                                                               PDW  990
      SUBROUTINE KDWT(P2E,UTILE,NRGNE,PVOL,IPRIN,IVX,JVX,KBVX,KVX,LVX,  KDW   10
     1 JIVX,SIG,NCOMP,MVX)                                              KDW   20
C                                                                       KDW   30
CKDWT --140 ***CITATION*** PRINT REL. POW. DENS. TRAVERSES/ CF-OUTC     KDW   40
C                                                                       KDW   50
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KDW   60
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KDW   70
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KDW   80
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KDW   90
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KDW  100
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KDW  110
     6 IXPUT(9999),XPUT(9999)                                           KDW  120
C                                                                       KDW  130
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  KDW  140
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), KDW  150
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)KDW  160
     3 ,PDK(211)                                                        KDW  170
C                                                                       KDW  180
CFZJ055                                                       25.09.07  KDW  190
C                                                                       KDW  200
      DIMENSION P2E(JIVX,KBVX,KVX),UTILE(JVX,IVX,KBVX),                 KDW  210
     1 NRGNE(JVX,IVX,KBVX),PVOL(LVX),SIG(KVX,MVX,10),NCOMP(LVX)         KDW  220
C                                                                       KDW  230
C                                                                       KDW  240
      T2 = PDI(211)                                                     KDW  250
      IAX = IMAX                                                        KDW  260
      IF(IMAX .GT. 200) IAX = 200                                       KDW  270
      DO 100 N=1,IAX                                                    KDW  280
        XTR1(N) = PDI(N)                                                KDW  290
  100 CONTINUE                                                          KDW  300
      DO 105 KB=1,KBMAX                                                 KDW  310
        DO 104 I=1,IMAX                                                 KDW  320
          NN1= (I-1) * JVX                                              KDW  330
          DO 103 J=1,JMAX                                               KDW  340
            N1= NN1 + J                                                 KDW  350
            L = NRGNE(J,I,KB)                                           KDW  360
            M = NCOMP(L)                                                KDW  370
            UTILE(J,I,KB ) = 0.0                                        KDW  380
            DO 102 K=1,KMAX                                             KDW  390
              UTILE(J,I,KB) = UTILE(J,I,KB) + P2E(N1,KB,K) * SIG(K,M,7) KDW  400
  102       CONTINUE                                                    KDW  410
  103     CONTINUE                                                      KDW  420
  104   CONTINUE                                                        KDW  430
  105 CONTINUE                                                          KDW  440
      IF(IPRIN .GT. 0) GOTO 120                                         KDW  450
C                                                                       KDW  460
      CALL RQED(IX(113),IND)                                            KDW  470
C                                                                       KDW  480
      IF(IND .NE. 0) GOTO 120                                           KDW  490
      T1 = 0.0                                                          KDW  500
      DO 110 KB=1,KBMAX                                                 KDW  510
        DO 109 I=1,IMAX                                                 KDW  520
          DO 108 J=1,JMAX                                               KDW  530
            IF(UTILE(J,I,KB)-T1) 108,107,107                            KDW  540
  107       II = I                                                      KDW  550
            KKB = KB                                                    KDW  560
            JJ = J                                                      KDW  570
            T1 = UTILE(J,I,KB)                                          KDW  580
  108     CONTINUE                                                      KDW  590
  109   CONTINUE                                                        KDW  600
  110 CONTINUE                                                          KDW  610
      XT1 = 0.0                                                         KDW  620
      XV1 = 0.0                                                         KDW  630
      YT1 = 0.0                                                         KDW  640
      YV1 = 0.0                                                         KDW  650
      ZT1 = 0.0                                                         KDW  660
      ZV1 = 0.0                                                         KDW  670
      DO 112 J=1,JMAX                                                   KDW  680
        L = NRGNE(J,II,KKB)                                             KDW  690
        IF(UTILE(J,II,KKB)) 111,112,111                                 KDW  700
  111   XT1 = XT1 + UTILE(J,II,KKB) * PVOL(L)                           KDW  710
        XV1 = XV1 + PVOL(L)                                             KDW  720
  112 CONTINUE                                                          KDW  730
      DO 114 I=1,IMAX                                                   KDW  740
        L = NRGNE(JJ,I,KKB)                                             KDW  750
        IF(UTILE(JJ,I,KKB)) 113,114,113                                 KDW  760
  113   YT1 = YT1 + UTILE(JJ,I,KKB) * PVOL(L)                           KDW  770
        YV1 = YV1 + PVOL(L)                                             KDW  780
  114 CONTINUE                                                          KDW  790
      DO 116 KB=1,KBMAX                                                 KDW  800
        L = NRGNE(JJ,II,KB)                                             KDW  810
        IF(UTILE(JJ,II,KB)) 115,116,115                                 KDW  820
  115   ZT1 = ZT1 + UTILE(JJ,II,KB) * PVOL(L)                           KDW  830
        ZV1 = ZV1 + PVOL(L)                                             KDW  840
  116 CONTINUE                                                          KDW  850
      XB1 = XT1 / XV1                                                   KDW  860
      YB1 = YT1 / YV1                                                   KDW  870
      ZB1 = ZT1 / ZV1                                                   KDW  880
      DO 117 J=1,JMAX                                                   KDW  890
        PDJ(J) = UTILE(J,II,KKB) / XB1                                  KDW  900
  117 CONTINUE                                                          KDW  910
      DO 118 I=1,IMAX                                                   KDW  920
        PDI(I) = UTILE(JJ,I,KKB) / YB1                                  KDW  930
  118 CONTINUE                                                          KDW  940
      DO 119 KB=1,KBMAX                                                 KDW  950
        PDK(KB) = UTILE(JJ,II,KB) / ZB1                                 KDW  960
  119 CONTINUE                                                          KDW  970
      WRITE (IOUT,1000) II,JJ,KKB,UTILE(JJ,II,KKB)                      KDW  980
      WRITE (IOUT,1001) II,XB1,(PDJ(J),J=1,JMAX)                        KDW  990
      WRITE (IOUT,1002) JJ,YB1,(PDI(I),I=1,IMAX)                        KDW 1000
      WRITE (IOUT,1003) KKB,ZB1,(PDK(KB),KB=1,KBMAX)                    KDW 1010
  120 CONTINUE                                                          KDW 1020
      PDI(211) = T2                                                     KDW 1030
      DO 121 N=1,IAX                                                    KDW 1040
        PDI(N) = XTR1(N)                                                KDW 1050
  121 CONTINUE                                                          KDW 1060
      RETURN                                                            KDW 1070
C                                                                       KDW 1080
 1000 FORMAT (1H0/1H0,'THE MAXIMUM POWER DENSITY',1H(,'WATTS/CC) AT ROW'KDW 1090
     1 ,I4,2H  ,'AND COLUMN',I4, 2H  ,'AND LINE',I4,2H  ,'IS ',1PE14.6) KDW 1100
 1001 FORMAT (1H0,'THE AVERAGE POWER DENSITY ALONG ROW',I4,2H  ,'IS',   KDW 1110
     1 1PE14.6,2H  ,'AND THE RELATIVE POWER DENSITIES',1H(,'FRACTION OF KDW 1120
     2AVERAGE) ARE'/(1H ,9E14.6))                                       KDW 1130
 1002 FORMAT (1H0,'THE AVERAGE POWER DENSITY DOWN COLUMN',I4,2H  ,'IS', KDW 1140
     1 1PE14.6,2H  ,'AND THE RELATIVE POWER DENSITIES',1H ,'ARE'/(1H ,  KDW 1150
     2 9E14.6))                                                         KDW 1160
 1003 FORMAT (1H0,'THE AVERAGE POWER DENSITY ALONG LINE',I4,2H  ,'IS',  KDW 1170
     1 1PE14.6,2H  ,'AND THE RELATIVE POWER DENSITIES',1H ,'ARE'/(1H ,  KDW 1180
     2 9E14.6))                                                         KDW 1190
      END                                                               KDW 1200
      SUBROUTINE NUDN(ONEOV,NCOMP,P2,NRGN,UTIL,SOURE,IVX,JVX,KBVX,KVX,  NUD   10
     1 LVX,MVX,NSETVX)                                                  NUD   20
C                                                                       NUD   30
CNUDN --144 ***CITATION*** POINT NEUTRON DENSITY FOR 1,2-D/ CF-OUTC     NUD   40
C                                                                       NUD   50
      REAL*8 P2                                                         NUD   60
C                                                                       NUD   70
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,NUD   80
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   NUD   90
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), NUD  100
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    NUD  110
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    NUD  120
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   NUD  130
     6 IXPUT(9999),XPUT(9999)                                           NUD  140
C                                                                       NUD  150
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           NUD  160
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    NUD  170
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),NUD  180
     3 NCLASS(9999),NDP(9999)                                           NUD  190
C                                                                       NUD  200
CFZJ048 enlarged dimension                                    11.04.07  NUD  210
      COMMON /VARDIT/ B(5000000)                                        NUD  220
C                                                                       NUD  230
CFZJ042                                                       09.09.05  NUD  240
      COMMON /ADDRT/ KX(240),KY(240),LZ(240),NENDPT                     NUD  250
C                                                                       NUD  260
      DIMENSION ONEOV(KVX,NSETVX),NCOMP(LVX),SOURE(JVX,IVX,KBVX),       NUD  270
     1 P2(JVX,IVX,KVX),NRGN(JVX,IVX),UTIL(JVX,IVX)                      NUD  280
C                                                                       NUD  290
CFZJ023                                                       29.01.04  NUD  300
      EQUIVALENCE(LZ(88),IPDTH),(LZ(224),IFFTH)                         NUD  310
C                                                                       NUD  320
C                                                                       NUD  330
      DO 102 I=1,IVX                                                    NUD  340
        DO 101 J=1,JVX                                                  NUD  350
          L = NRGN(J,I)                                                 NUD  360
          M = NCOMP(L)                                                  NUD  370
          NS = NXSET(M)                                                 NUD  380
          NR = NXODR(NS)                                                NUD  390
          UTIL(J,I) = 0.0                                               NUD  400
          DO 100 K=1,KVX                                                NUD  410
            UTIL(J,I) = UTIL(J,I) + P2(J,I,K) * ONEOV(K,NR)             NUD  420
  100     CONTINUE                                                      NUD  430
  101   CONTINUE                                                        NUD  440
  102 CONTINUE                                                          NUD  450
      IND = 2                                                           NUD  460
C                                                                       NUD  470
CFZJ023                                                       29.01.04  NUD  480
      CALL POUT(P2,UTIL,IND,SOURE,IVX,JVX,KBVX,KVX,B(KX(IPDTH)),        NUD  490
     1 B(KX(IFFTH)))                                                    NUD  500
C                                                                       NUD  510
      RETURN                                                            NUD  520
      END                                                               NUD  530
      SUBROUTINE KUDN(ONEOV,NCOMP,P2E,NRGNE,UTILE,SOURE,IVX,JVX,KBVX,KVXKUD   10
     1 ,LVX,MVX,NSETVX,JIVX)                                            KUD   20
C                                                                       KUD   30
CKUDN --145 ***CITATION*** POINT NEUTRON DENSITY FOR 3-D/ CF-OUTC       KUD   40
C                                                                       KUD   50
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KUD   60
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KUD   70
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KUD   80
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KUD   90
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KUD  100
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KUD  110
     6 IXPUT(9999),XPUT(9999)                                           KUD  120
C                                                                       KUD  130
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           KUD  140
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    KUD  150
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),KUD  160
     3 NCLASS(9999),NDP(9999)                                           KUD  170
C                                                                       KUD  180
      DIMENSION ONEOV(KVX,NSETVX),NCOMP(LVX),SOURE(JVX,IVX,KBVX),       KUD  190
     1 P2E(JIVX,KBVX,KVX),NRGNE(JVX,IVX,KBVX),UTILE(JVX,IVX,KBVX)       KUD  200
C                                                                       KUD  210
C                                                                       KUD  220
      DO 103 KB=1,KBVX                                                  KUD  230
        DO 102 I=1,IVX                                                  KUD  240
          NN1 = (I-1) * JVX                                             KUD  250
          DO 101 J=1,JVX                                                KUD  260
            N1 = NN1 + J                                                KUD  270
            L = NRGNE(J,I,KB)                                           KUD  280
            M = NCOMP(L)                                                KUD  290
            NS = NXSET(M)                                               KUD  300
            NR = NXODR(NS)                                              KUD  310
            UTILE(J,I,KB) = 0.0                                         KUD  320
            DO 100 K=1,KVX                                              KUD  330
              UTILE(J,I,KB) = UTILE(J,I,KB) + P2E(N1,KB,K) * ONEOV(K,NR)KUD  340
  100       CONTINUE                                                    KUD  350
  101     CONTINUE                                                      KUD  360
  102   CONTINUE                                                        KUD  370
  103 CONTINUE                                                          KUD  380
      IND = 2                                                           KUD  390
C                                                                       KUD  400
      CALL KOUT(P2E,UTILE,IND,SOURE,IVX,JVX,KBVX,KVX,JIVX)              KUD  410
C                                                                       KUD  420
      RETURN                                                            KUD  430
      END                                                               KUD  440
      SUBROUTINE CRSH(RVOL,PVOL,LVX)                                    CRS   10
C                                                                       CRS   20
CCRSH -82.1 ***CITATION*** CONTROLS DIMENSION SEARCH / CF-EIGN          CRS   30
C                                                                       CRS   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,CRS   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   CRS   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), CRS   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    CRS   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    CRS   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   CRS  100
     6 IXPUT(9999),XPUT(9999)                                           CRS  110
C                                                                       CRS  120
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   CRS  130
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKCRS  140
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    CRS  150
     3 VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  CRS  160
     4 ITMAX,ITIME,BET(211),DEL(211)                                    CRS  170
C                                                                       CRS  180
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           CRS  190
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    CRS  200
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),CRS  210
     3 NCLASS(9999),NDP(9999)                                           CRS  220
C                                                                       CRS  230
      COMMON /ASRCH/ BSRCH(30),XK1,XK2,XK3,XN1,XN2,XN3,DELK1,DELK2,DELK3CRS  240
     1 ,BATTY,DRV,TBF,GWC,EK2,RCCM,DNDK(5),NSC(5),NSCN,NXZ,NXN,NXM,NXS, CRS  250
     2 INIL,INIU,INID                                                   CRS  260
C                                                                       CRS  270
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  CRS  280
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), CRS  290
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)CRS  300
     3 ,PDK(211)                                                        CRS  310
C                                                                       CRS  320
CFZJ055                                                       25.09.07  CRS  330
C                                                                       CRS  340
      DIMENSION RVOL(LVX),PVOL(LVX)                                     CRS  350
C                                                                       CRS  360
C                                                                       CRS  370
      IO18 = IX(85)                                                     CRS  380
      REWIND IO18                                                       CRS  390
      READ (IO18) NXJ,NXI,NXKB,INDX,INGX,IDUM,IDUM,XMX                  CRS  400
      IND = 0                                                           CRS  410
      INEG = 0                                                          CRS  420
      RCCM = 1.0                                                        CRS  430
      NOUT = NSRH(24)                                                   CRS  440
      IF(IX(73) .EQ. 2) GOTO 104                                        CRS  450
      XK1 = XK2                                                         CRS  460
      XK2 = XK3                                                         CRS  470
      XK3 = XKEF1                                                       CRS  480
      XN1 = XN2                                                         CRS  490
      XN2 = XN3                                                         CRS  500
      GOTO(100,101,102),INGX                                            CRS  510
  100 XN3 = XSHJ(INDX)                                                  CRS  520
      GOTO 103                                                          CRS  530
  101 XN3 = XSHI(INDX)                                                  CRS  540
      GOTO 103                                                          CRS  550
  102 XN3 = XSHKB(INDX)                                                 CRS  560
  103 CONTINUE                                                          CRS  570
  104 CONTINUE                                                          CRS  580
      IF(IX(73) .LE. 1) SPARE(65) = 0.0                                 CRS  590
      IF(IX(73) .LE. 2) GOTO 106                                        CRS  600
      S60 = DRV                                                         CRS  610
      DIF = XK3 - XK2                                                   CRS  620
      IF(ABS(S60) .GT. 1.0E-4 .AND. ABS(DIF) .GT. 1.0E-4) GOTO 105      CRS  630
      GOTO 106                                                          CRS  640
  105 SPARE(65) = DIF / S60                                             CRS  650
  106 CONTINUE                                                          CRS  660
      DELK3 = DELK2                                                     CRS  670
      DELK2 = DELK1                                                     CRS  680
      DELK1 = ABS(XKEF1/XSRH1(1)-1.0)                                   CRS  690
      SPARE(58) = 0.0                                                   CRS  700
      IF(IX(73)-2) 107,116,117                                          CRS  710
  107 CONTINUE                                                          CRS  720
C                                                                       CRS  730
C     FIRST TIME IN - K FOR BASE CASE HAS BEEN CALCULATED               CRS  740
C                                                                       CRS  750
      IF(DELK1 .GE. EPI2) GOTO 108                                      CRS  760
      IX(75) = 1                                                        CRS  770
      WRITE (IOUT,1000)                                                 CRS  780
      GOTO 145                                                          CRS  790
  108 CONTINUE                                                          CRS  800
C                                                                       CRS  810
C     LOOK FOR ESTIMATE OF NDK/DN - IF NONE AVAILABLE USE BUILT-IN      CRS  820
C     PROCEDURE                                                         CRS  830
C                                                                       CRS  840
      IF(NSCN .LE. 0) GOTO 110                                          CRS  850
      DO 109 N=1,NSCN                                                   CRS  860
        IF(NSC(N) .NE. NSRH(1)) GOTO 109                                CRS  870
        QQ = DNDK(N)                                                    CRS  880
        GOTO 111                                                        CRS  890
  109 CONTINUE                                                          CRS  900
  110 QQ = XSRH1(3)                                                     CRS  910
  111 IF(QQ .EQ. 0.0) GOTO 112                                          CRS  920
      IX(130) = 1                                                       CRS  930
      GOTO 113                                                          CRS  940
  112 IX(130) = 0                                                       CRS  950
  113 CONTINUE                                                          CRS  960
      BATTY = BETTA                                                     CRS  970
      IX(70) = 0                                                        CRS  980
      IX(74) = 1                                                        CRS  990
      IX(126) = 0                                                       CRS 1000
      SPARE(60) = 0.2                                                   CRS 1010
      SPARE(61) = 1.0                                                   CRS 1020
      SPARE(62) = 0.0                                                   CRS 1030
C                                                                       CRS 1040
C     DETERMINE INITIAL SEARCH DRIVING FACTOR                           CRS 1050
C                                                                       CRS 1060
      T1 = XN3                                                          CRS 1070
      IF(IX(130) .GT. 0) GOTO 114                                       CRS 1080
      IF(XKEF1 .GT. XSRH1(1)) SPARE(60) = (-SPARE(60))                  CRS 1090
      GOTO 115                                                          CRS 1100
  114 CONTINUE                                                          CRS 1110
      SPARE(60) = (XSRH1(1)-XKEF1) / QQ                                 CRS 1120
  115 CONTINUE                                                          CRS 1130
      SPARE(62) = SPARE(60) * T1 / XMX                                  CRS 1140
      GWC = SPARE(62)                                                   CRS 1150
      IF(NOUT .LT. 0) WRITE (IOUT,1001) INGX,INDX,SPARE(60),SPARE(62),  CRS 1160
     1 XKEF1,XSRH1(1),QQ,T1,XMX                                         CRS 1170
      GOTO 131                                                          CRS 1180
  116 CONTINUE                                                          CRS 1190
C                                                                       CRS 1200
C     SECOND TIME IN - K WITH UPDATED DIMENSIONS HAS BEEN CALCULATED    CRS 1210
C     WITH ONE ITERATION IN FLUX OR KLUX                                CRS 1220
C                                                                       CRS 1230
      IF(IX(74) .EQ. 0) GOTO 127                                        CRS 1240
      GOTO 125                                                          CRS 1250
  117 CONTINUE                                                          CRS 1260
C                                                                       CRS 1270
C     THIRD ETC. TIME IN -                                              CRS 1280
C                                                                       CRS 1290
      IF(DELK1 .GE. EPI2) GOTO 122                                      CRS 1300
C                                                                       CRS 1310
C     EXCELSIOR - WE ARE DONE                                           CRS 1320
C                                                                       CRS 1330
      IX(75) = 1                                                        CRS 1340
      IND = 1                                                           CRS 1350
      BETTA = BATTY                                                     CRS 1360
      WRITE (IOUT,1002)                                                 CRS 1370
      IF(NOUT .LT. 0) WRITE (IOUT,1003) SPARE(60),SPARE(61),SPARE(62)   CRS 1380
C                                                                       CRS 1390
C     SAVE ESTIMATE OF NDK/DN FOR POSSIBLE LATER USE                    CRS 1400
C                                                                       CRS 1410
      QF = (XK3-XK2) / SPARE(60)                                        CRS 1420
      IF(SPARE(65) .EQ. 0.0) SPARE(65) = QF                             CRS 1430
      IF(NSCN .GT. 0) GOTO 118                                          CRS 1440
      NSCN = 1                                                          CRS 1450
      NSC(NSCN) = NSRH(1)                                               CRS 1460
      DNDK(NSCN) = SPARE(65)                                            CRS 1470
      GOTO 121                                                          CRS 1480
  118 DO 119 N=1,NSCN                                                   CRS 1490
        IF(NSC(N) .EQ. NSRH(1)) GOTO 120                                CRS 1500
  119 CONTINUE                                                          CRS 1510
      NSCN = NSCN + 1                                                   CRS 1520
      IF(NSCN .GT. 5) NSCN = 5                                          CRS 1530
      NSC(NSCN) = NSRH(1)                                               CRS 1540
      DNDK(NSCN) = SPARE(65)                                            CRS 1550
      GOTO 121                                                          CRS 1560
  120 CONTINUE                                                          CRS 1570
      DNDK(N) = SPARE(65)                                               CRS 1580
  121 CONTINUE                                                          CRS 1590
      WRITE (IOUT,1004) SPARE(65),QF                                    CRS 1600
      GOTO 138                                                          CRS 1610
  122 CONTINUE                                                          CRS 1620
      IX(126) = 0                                                       CRS 1630
      IF(DELK1 .GE. DELK3) IX(126) = 1                                  CRS 1640
      IF(IX(73) .GT. 4 .AND. IX(126) .EQ. 1) IX(74) = 0                 CRS 1650
      T1 = ABS(XKEF1-EK2) / EK2                                         CRS 1660
      IF(T1 .GT. 1.0E-5) GOTO 124                                       CRS 1670
      SPARE(60) = 2.0 * SPARE(60)                                       CRS 1680
      IF(ABS(SPARE(60)) .LE. 1.0E-6 .AND. SPARE(60) .LT. 0.0) GOTO 123  CRS 1690
      SPARE(60) = 1.0E-5                                                CRS 1700
      GOTO 129                                                          CRS 1710
  123 CONTINUE                                                          CRS 1720
      SPARE(60) = -1.0E-5                                               CRS 1730
      GOTO 129                                                          CRS 1740
  124 CONTINUE                                                          CRS 1750
      IF(IX(74) .EQ. 0) GOTO 127                                        CRS 1760
      IF(IX(73) .GT. 3) GOTO 130                                        CRS 1770
  125 CONTINUE                                                          CRS 1780
C                                                                       CRS 1790
C     TWO POINT APPROXIMATION                                           CRS 1800
C                                                                       CRS 1810
      B = DRV / ((1.0+DRV)/XKEF1-1.0/EK2)                               CRS 1820
      IF(ABS(B) .GT. 1.0E+4) GOTO 126                                   CRS 1830
      C = (B/EK2-1.0) / (1.0+DRV)                                       CRS 1840
      SPARE(60) = C *XSRH1(1) / (B-XSRH1(1)) - 1.0                      CRS 1850
      IF(SPARE(60) .GT. 0. .AND. XKEF1 .LT. XSRH1(1) .AND. EK2 .LT.     CRS 1860
     1 XSRH1(1)) GOTO 127                                               CRS 1870
      IF(SPARE(60) .GT. 0. .AND. XKEF1 .GT. XSRH1(1) .AND. EK2 .GT.     CRS 1880
     1 XSRH1(1)) GOTO 127                                               CRS 1890
      IF(SPARE(60) .LE. -1.0) GOTO 126                                  CRS 1900
      IF(NOUT .LT. 0) WRITE (IOUT,1005)                                 CRS 1910
      GOTO 128                                                          CRS 1920
  126 CONTINUE                                                          CRS 1930
  127 CONTINUE                                                          CRS 1940
C                                                                       CRS 1950
C     LINEAR APPROXIMATION                                              CRS 1960
C                                                                       CRS 1970
      IF(NOUT .LT. 0) WRITE (IOUT,1006)                                 CRS 1980
      SPARE(60) = DRV * (XSRH1(1)-XKEF1) / ((1.0+DRV)*(XKEF1-EK2))      CRS 1990
  128 CONTINUE                                                          CRS 2000
      IF(NOUT .LT. 0) WRITE (IOUT,1007) IX(73),IX(74),SPARE(60),DRV,    CRS 2010
     1 XKEF1,EK2,B,C                                                    CRS 2020
  129 CONTINUE                                                          CRS 2030
      GOTO 131                                                          CRS 2040
  130 CONTINUE                                                          CRS 2050
C                                                                       CRS 2060
C     THREE POINT APPROXIMATION                                         CRS 2070
C                                                                       CRS 2080
      IF(NOUT .LT. 0) WRITE (IOUT,1008) XN3,XN2,XN1,XK3,XK2,XK1         CRS 2090
      C1 = (XN1-XN3) / (XN1-XN2)                                        CRS 2100
      C2 = XK1 / XK2                                                    CRS 2110
      C3 = XK1 / XK3                                                    CRS 2120
      C4 = C3 - 1.0 - C1 * (C2-1.0)                                     CRS 2130
      IF(ABS(C4) .LT. 1.0E-4) GOTO 126                                  CRS 2140
      C = (C1*(XN1-C2*XN2)-XN1+C3*XN3) / C4                             CRS 2150
      C1 = (XN2-C) / (XN1-C)                                            CRS 2160
      C5 = 1.0 - C2 * C1                                                CRS 2170
      IF(ABS(C5) .LT. 1.0E-4) GOTO 126                                  CRS 2180
      B = (XK1*(1.0-C1)) / C5                                           CRS 2190
      A = (XN1-C) * (B-XK1) / XK1                                       CRS 2200
      XND = C + A * XSRH1(1) / (B-XSRH1(1))                             CRS 2210
      SPARE(60) = XND / XN3 - 1.0                                       CRS 2220
      IF(NOUT .LT. 0) WRITE (IOUT,1009)                                 CRS 2230
      IF(NOUT .LT. 0) WRITE (IOUT,1010) IX(73),IX(74),SPARE(60),A,B,C,C4CRS 2240
     1 ,C5,XND                                                          CRS 2250
  131 CONTINUE                                                          CRS 2260
      IF(IX(73) .GT. 1) SPARE(62) = TBF * (SPARE(60)*(1.0+1.0/DRV))     CRS 2270
      SPARE(61) = SPARE(61) * (1.0+SPARE(60))                           CRS 2280
      IF(NOUT .LT. 0) WRITE (IOUT,1011) SPARE(60),SPARE(61),SPARE(62)   CRS 2290
C                                                                       CRS 2300
C     UPDATE SEARCH DIMENSIONS                                          CRS 2310
C                                                                       CRS 2320
      IF(NXJ .LE. 0) GOTO 133                                           CRS 2330
      READ (IO18) (XTR1(J),J=1,NXJ)                                     CRS 2340
      DO 132 J=1,NXJ                                                    CRS 2350
        IF(XTR1(J) .EQ. 0.0) GOTO 132                                   CRS 2360
        T1 = XSHJ(J)                                                    CRS 2370
        T7 = T1 + SPARE(62) * XTR1(J)                                   CRS 2380
        XSHJ(J) = T7                                                    CRS 2390
        IF(T7 .LE. 0.0) INEG = 1                                        CRS 2400
  132 CONTINUE                                                          CRS 2410
  133 IF(NXI .LE. 0) GOTO 135                                           CRS 2420
      READ (IO18) (XTR1(I),I=1,NXI)                                     CRS 2430
      DO 134 I=1,NXI                                                    CRS 2440
        IF(XTR1(I) .EQ. 0.0) GOTO 134                                   CRS 2450
        T1 = XSHI(I)                                                    CRS 2460
        T7 = T1 + SPARE(62) * XTR1(I)                                   CRS 2470
        XSHI(I) = T7                                                    CRS 2480
        IF(T7 .LE. 0.0) INEG = 1                                        CRS 2490
  134 CONTINUE                                                          CRS 2500
  135 IF(NXKB .LE. 0) GOTO 137                                          CRS 2510
      READ (IO18) (XTR1(KB),KB=1,NXKB)                                  CRS 2520
      DO 136 KB=1,NXKB                                                  CRS 2530
        IF(XTR1(KB) .EQ. 0.0) GOTO 136                                  CRS 2540
        T1 = XSHKB(KB)                                                  CRS 2550
        T7 = T1 + SPARE(62) * XTR1(KB)                                  CRS 2560
        XSHKB(KB) = T7                                                  CRS 2570
        IF(T7 .LE. 0.0) INEG = 1                                        CRS 2580
  136 CONTINUE                                                          CRS 2590
  137 CONTINUE                                                          CRS 2600
  138 CONTINUE                                                          CRS 2610
C                                                                       CRS 2620
      CALL MASH(RVOL,PVOL,LVX,IND,NOUT,INEG)                            CRS 2630
C                                                                       CRS 2640
      IF(IND .GT. 0) GOTO 142                                           CRS 2650
      IF(INEG .LE. 0) GOTO 139                                          CRS 2660
      WRITE (IOUT,1013)                                                 CRS 2670
      STOP                                                              CRS 2680
  139 CONTINUE                                                          CRS 2690
      IF(IX(73) .EQ. 2) GOTO 141                                        CRS 2700
      EK2 = XKEF1                                                       CRS 2710
      DRV = SPARE(60)                                                   CRS 2720
      TBF = SPARE(62)                                                   CRS 2730
      GOTO 142                                                          CRS 2740
  141 CONTINUE                                                          CRS 2750
      TBF = GWC + GWC * (SPARE(60)*(1.0+1.0/DRV))                       CRS 2760
      DRV = (SPARE(60)+1.0) * (DRV+1.0) - 1.0                           CRS 2770
  142 CONTINUE                                                          CRS 2780
      IF(NOUT .LT. 0) WRITE (IOUT,1012) IX(73),EK2,DRV,TBF              CRS 2790
      J2 = ICLOCK(0)                                                    CRS 2800
      JJ = (J2-IX(127)) / 6000                                          CRS 2810
      IF(IX(75) .EQ. 1) GOTO 145                                        CRS 2820
C                                                                       CRS 2830
C     NOT CONVERGED BUT SET FLAG TO WRITE FLUXES IF ITERATION COUNT     CRS 2840
C     OR TIME LIMIT HAS BEEN EXCEEDED                                   CRS 2850
C                                                                       CRS 2860
      IF(IX(73) .GE. NSRH(3)) IX(70) = 1                                CRS 2870
      IF(JJ .GE. ITMX(21)) IX(70) = (-1)                                CRS 2880
      IF(IX(70)) 143,145,144                                            CRS 2890
  143 WRITE (IOUT,1014)                                                 CRS 2900
      GOTO 145                                                          CRS 2910
  144 WRITE (IOUT,1015)                                                 CRS 2920
  145 CONTINUE                                                          CRS 2930
      IF(NOUT .LT. 0) WRITE (IOUT,1016) IX(70),IX(73),IX(74),IX(75),    CRS 2940
     1 IX(126),IX(130)                                                  CRS 2950
C                                                                       CRS 2960
C     WHEN USING ESTIMATE OF NDK/DN - I.E. IX(130) = 1 ,                CRS 2970
C     COUNTER MUST BE INCREMENTED - DONE IN DASH OR KASH                CRS 2980
C                                                                       CRS 2990
      REWIND IO18                                                       CRS 3000
      RETURN                                                            CRS 3010
C                                                                       CRS 3020
 1000 FORMAT (1H0,'NO SEARCH REQUIRED - MULTIPLICATION FACTOR SATISFIED'CRS 3030
     1 )                                                                CRS 3040
 1001 FORMAT (' DB 1',2I5,7(1PE13.5))                                   CRS 3050
 1002 FORMAT (1H0,'INNER REACTIVITY LOOP SEARCH CONVERGED')             CRS 3060
 1003 FORMAT (' DB 4',3(1PE15.6))                                       CRS 3070
 1004 FORMAT (1H0,'ESTIMATE OF N(DK/DN) SAVED',1PE15.6,'  FINAL',1PE15.6CRS 3080
     1 )                                                                CRS 3090
 1005 FORMAT (1H ,'USING TWO POINT NON-LINEAR APPROXIMATION')           CRS 3100
 1006 FORMAT (1H ,'USING LINEAR APPROXIMATION')                         CRS 3110
 1007 FORMAT (' DB 6',2I5,6(1PE15.6))                                   CRS 3120
 1008 FORMAT (1H ,'C(N)=',1PE15.6,' C(N-1)=',1PE15.6,' C(N-2)=',1PE15.6/CRS 3130
     1 1H ,'K(N)=',1PE15.6,' K(N-1)=',1PE15.6,' K(N-2)=',1PE15.6)       CRS 3140
 1009 FORMAT (1H ,'USING THREE POINT NON-LINEAR APPROXIMATION')         CRS 3150
 1010 FORMAT (' DB 8',2I5,7(1PE15.6))                                   CRS 3160
 1011 FORMAT (' DB 9',' DIMENSIONS UPDATED WITH',3(1PE15.6))            CRS 3170
 1012 FORMAT (' DB10',I5,3(1PE15.6))                                    CRS 3180
 1013 FORMAT (1H0,'SEARCH DIMENSION LESS THAN OR EQUAL TO ZERO - STOP') CRS 3190
 1014 FORMAT (1H0,'INNER REACTIVITY LOOP SEARCH TIME LIMIT EXCEEDED')   CRS 3200
 1015 FORMAT (1H0,'INNER REACTIVITY LOOP SEARCH ITERATION LIMIT EXCEEDEDCRS 3210
     1 ')                                                               CRS 3220
 1016 FORMAT (' DB13 IX(70)=',I5,' IX(73)=',I5,' IX(74)=',I5,' IX(75)=',CRS 3230
     1 I5,' IX(126)=',I5,' IX(130)=',I5)                                CRS 3240
      END                                                               CRS 3250
      SUBROUTINE MASH(RVOL,PVOL,LVX,IND,NOUT,INEG)                      MAS   10
C                                                                       MAS   20
CMASH -82.2 ***CITATION*** REGION VOLUMES AND MESH SPACING /CF - CRSH   MAS   30
C                          FOR DIMENSION SEARCH                         MAS   40
C                                                                       MAS   50
      REAL*8 XDEL,ADR,YDELSQ,XNIPTS,XNJPTS,XNKPTS,TEMP,ZXYR,YDIST,XDIST,MAS   60
     1 ZDIST,YDEL,ZDEL,AXYR,XDIST1,XDIST2,YDIST1,YDIST2,XNTOPS,U1,U2,H, MAS   70
     2 H1,H2                                                            MAS   80
C                                                                       MAS   90
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,MAS  100
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   MAS  110
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), MAS  120
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    MAS  130
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    MAS  140
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   MAS  150
     6 IXPUT(9999),XPUT(9999)                                           MAS  160
C                                                                       MAS  170
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  MAS  180
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), MAS  190
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)MAS  200
     3 ,PDK(211)                                                        MAS  210
C                                                                       MAS  220
CFZJ055                                                       25.09.07  MAS  230
C                                                                       MAS  240
      COMMON /IFLUX/ IFLX,YFLX(211),XFLX(211)                           MAS  250
C                                                                       MAS  260
      DIMENSION RVOL(LVX),PVOL(LVX),NX(6),TX(6)                         MAS  270
C                                                                       MAS  280
C                                                                       MAS  290
C     SET NXY TO SAME NUMBER AS THE DIMENSION OF XX AND YY              MAS  300
C                                                                       MAS  310
      NXY = 211                                                         MAS  320
C                                                                       MAS  330
      NGEM = IX(26)                                                     MAS  340
      IF(IND .GT. 0) GOTO 165                                           MAS  350
      MGEM = 0                                                          MAS  360
      H1 = 6 * MSHI(1) * MSHJ(1)                                        MAS  370
      H2 = XSHI(1) * XSHJ(1)                                            MAS  380
      H = DSQRT(H2/H1)                                                  MAS  390
      IF(NGEM .EQ. 8 .OR. NGEM .EQ. 12) MGEM = 8                        MAS  400
      NRGNO = 0                                                         MAS  410
      MESHGE = 0                                                        MAS  420
      DO 118 KBR=1,NREGKB                                               MAS  430
        NKBPTS = MSHKB(KBR)                                             MAS  440
        ZDIST = XSHKB(KBR)                                              MAS  450
        YDIST2 = 0.D+0                                                  MAS  460
        DO 117 IR=1,NREGI                                               MAS  470
          NIPTS = MSHI(IR)                                              MAS  480
          U1 = NIPTS                                                    MAS  490
          YDIST = XSHI(IR)                                              MAS  500
          YDIST1 = YDIST2                                               MAS  510
          YDIST2 = YDIST2 + XSHI(IR)                                    MAS  520
          XDIST2 = 0.D+0                                                MAS  530
          DO 116 JR=1,NREGJ                                             MAS  540
            XDIST1 = XDIST2                                             MAS  550
            XDIST2 = XDIST2 + XSHJ(JR)                                  MAS  560
            NJPTS = MSHJ(JR)                                            MAS  570
            U2 = NJPTS                                                  MAS  580
            XDIST = XSHJ(JR)                                            MAS  590
            NTOPTS = NKBPTS * NIPTS * NJPTS                             MAS  600
            XNTOPS = FLOAT(NTOPTS)                                      MAS  610
            NRGNO = NRGNO + 1                                           MAS  620
            GOTO(101,102,103,104,105,106,107,108,109,110,111,112,113,114MAS  630
     1       ),NGEM                                                     MAS  640
  101       AXYR = XDIST                                                MAS  650
            GOTO 115                                                    MAS  660
  102       AXYR = 3.141593 * (XDIST2**2-XDIST1**2)                     MAS  670
            GOTO 115                                                    MAS  680
  103       AXYR = 4.0 * 3.141593 / 3.0 * (XDIST2**3-XDIST1**3)         MAS  690
            GOTO 115                                                    MAS  700
  104       AXYR = 1.0                                                  MAS  710
            GOTO 115                                                    MAS  720
  105       GOTO 115                                                    MAS  730
  106       AXYR = YDIST * XDIST                                        MAS  740
            GOTO 115                                                    MAS  750
  107       AXYR = 3.141593 * (XDIST2**2-XDIST1**2) * YDIST             MAS  760
            GOTO 115                                                    MAS  770
  108       AXYR = XDIST * (YDIST2**2-YDIST1**2) / 2.0                  MAS  780
            GOTO 115                                                    MAS  790
  109       AXYR = 0.3608439 * ((XDIST/U2)**2+(YDIST/U1)**2+0.4*XDIST*  MAS  800
     1       YDIST/(U2*U1)) * XNTOPS                                    MAS  810
            GOTO 115                                                    MAS  820
  110       CONTINUE                                                    MAS  830
            AXYR = 0.8660254040 * XDIST * YDIST                         MAS  840
C           SQRT(3)/2 = 0.8660254040                                    MAS  850
            GOTO 115                                                    MAS  860
  111       AXYR = ZDIST * YDIST * XDIST                                MAS  870
            GOTO 115                                                    MAS  880
  112       AXYR = 0.5 * ZDIST * XDIST * (YDIST2**2-YDIST1**2)          MAS  890
            GOTO 115                                                    MAS  900
  113       AXYR = 0.3608439 * ZDIST * ((XDIST/U2)**2+(YDIST/U1)**2+0.4*MAS  910
     1       XDIST*YDIST/(U2*U1)) * XNTOPS                              MAS  920
            GOTO 115                                                    MAS  930
  114       CONTINUE                                                    MAS  940
            AXYR = 0.8660254040 * XDIST * YDIST * ZDIST                 MAS  950
  115       RVOL(NRGNO) = AXYR                                          MAS  960
            PVOL(NRGNO) = AXYR / XNTOPS                                 MAS  970
            IF(NGEM .NE. 9 .OR. NGEM .NE. 13) GOTO 116                  MAS  980
            IF(NRGNO .EQ. 1) GOTO 116                                   MAS  990
            IF(PVOL(1) .NE. PVOL(NRGNO)) MESHGE = 1                     MAS 1000
  116     CONTINUE                                                      MAS 1010
  117   CONTINUE                                                        MAS 1020
  118 CONTINUE                                                          MAS 1030
      KB = 0                                                            MAS 1040
      ZZ(1) = 0.0                                                       MAS 1050
      ZXYR = 0.D+0                                                      MAS 1060
      ADR = 0.0                                                         MAS 1070
      DO 126 KBR=1,NREGKB                                               MAS 1080
        NKBPTS = MSHKB(KBR)                                             MAS 1090
        XNKPTS = FLOAT(NKBPTS)                                          MAS 1100
        ZDEL = XSHKB(KBR) / XNKPTS                                      MAS 1110
        DO 125 KNR=1,NKBPTS                                             MAS 1120
          KB = KB + 1                                                   MAS 1130
          ZXYR = ZXYR + ZDEL                                            MAS 1140
          IF(KB-1) 119,119,120                                          MAS 1150
  119     AXYR = 0.5D+0 * ZDEL                                          MAS 1160
          GOTO 123                                                      MAS 1170
  120     IF(KNR-1) 121,121,122                                         MAS 1180
  121     AXYR = AXYR + 0.5 * (ZDEL+ADR)                                MAS 1190
          GOTO 123                                                      MAS 1200
  122     AXYR = AXYR + ZDEL                                            MAS 1210
  123     ZZ(KB+1) = ZXYR                                               MAS 1220
          Z(KB) = AXYR                                                  MAS 1230
          IF(KNR-NKBPTS) 125,124,124                                    MAS 1240
  124     ADR = ZDEL                                                    MAS 1250
  125   CONTINUE                                                        MAS 1260
  126 CONTINUE                                                          MAS 1270
      NREGIJ = NREGI                                                    MAS 1280
      IF(MGEM-8) 128,127,128                                            MAS 1290
  127 NREGIJ = NREGJ                                                    MAS 1300
  128 I = 0                                                             MAS 1310
      YY(1) = 0.0                                                       MAS 1320
      ZXYR = 0.D+0                                                      MAS 1330
      ADR = 0.0                                                         MAS 1340
      DO 140 IR=1,NREGIJ                                                MAS 1350
        IF(MGEM-8) 129,130,129                                          MAS 1360
  129   NIPTS = MSHI(IR)                                                MAS 1370
        XNIPTS = FLOAT(NIPTS)                                           MAS 1380
        XDEL = XSHI(IR) / XNIPTS                                        MAS 1390
        GOTO 131                                                        MAS 1400
  130   NIPTS = MSHJ(IR)                                                MAS 1410
        XNIPTS = FLOAT(NIPTS)                                           MAS 1420
        XDEL = XSHJ(IR) / XNIPTS                                        MAS 1430
  131   DO 139 INR=1,NIPTS                                              MAS 1440
          I = I + 1                                                     MAS 1450
          IF(NGEM .NE. 10 .AND. NGEM .NE. 14) GOTO 132                  MAS 1460
          Y(I) = YY(I) + H                                              MAS 1470
          YY(I+1) = YY(I) + 3.0 * H                                     MAS 1480
          PDI(I) = YY(I+1) - H                                          MAS 1490
          GOTO 139                                                      MAS 1500
  132     CONTINUE                                                      MAS 1510
          ZXYR = ZXYR + XDEL                                            MAS 1520
          IF(I-1) 133,133,134                                           MAS 1530
  133     AXYR = 0.5D+0 * XDEL                                          MAS 1540
          GOTO 137                                                      MAS 1550
  134     IF(INR-1) 135,135,136                                         MAS 1560
  135     AXYR = AXYR + 0.5 * (XDEL+ADR)                                MAS 1570
          GOTO 137                                                      MAS 1580
  136     AXYR = AXYR + XDEL                                            MAS 1590
  137     Y(I) = AXYR                                                   MAS 1600
          YY(I+1) = ZXYR                                                MAS 1610
          IF(INR-NIPTS) 139,138,138                                     MAS 1620
  138     ADR = XDEL                                                    MAS 1630
  139   CONTINUE                                                        MAS 1640
  140 CONTINUE                                                          MAS 1650
      ZTEMP = 1.0 / 3.0                                                 MAS 1660
      J = 0                                                             MAS 1670
      XX(1) = 0.0                                                       MAS 1680
      ZXYR = 0.D+0                                                      MAS 1690
      ZDEL1 = ZZ(2) - ZZ(1)                                             MAS 1700
      YDEL1 = YY(2) - YY(1)                                             MAS 1710
      IF(NGEM-5) 141,141,142                                            MAS 1720
  141 T1 = 1.0                                                          MAS 1730
      GOTO 145                                                          MAS 1740
  142 IF(NGEM-11) 143,144,144                                           MAS 1750
  143 T1 = YDEL1                                                        MAS 1760
      GOTO 145                                                          MAS 1770
  144 T1 = YDEL1 * ZDEL1                                                MAS 1780
  145 CONTINUE                                                          MAS 1790
      YDELSQ = YY(2)**2 - YY(1)**2                                      MAS 1800
      NREGIJ = NREGJ                                                    MAS 1810
      IF(MGEM-8) 147,146,147                                            MAS 1820
  146 NREGIJ = NREGI                                                    MAS 1830
  147 DO 157 JR=1,NREGIJ                                                MAS 1840
        IF(MGEM .EQ. 8) GOTO 148                                        MAS 1850
        NJPTS = MSHJ(JR)                                                MAS 1860
        TEMP = PVOL(JR) / T1                                            MAS 1870
        XNJPTS = FLOAT(NJPTS)                                           MAS 1880
        XDEL = XSHJ(JR) / XNJPTS                                        MAS 1890
        GOTO 149                                                        MAS 1900
  148   NJPTS = MSHI(JR)                                                MAS 1910
        JS = (JR-1) * NREGJ + 1                                         MAS 1920
        TEMP = PVOL(JS) / T1                                            MAS 1930
        XNJPTS = FLOAT(NJPTS)                                           MAS 1940
        XDEL = XSHI(JR) / XNJPTS                                        MAS 1950
  149   DO 156 JNR=1,NJPTS                                              MAS 1960
          J = J + 1                                                     MAS 1970
          GOTO(150,151,154,156,156,150,151,152,150,153,150,152,150,153),MAS 1980
     1     NGEM                                                         MAS 1990
  150     ZXYR = ZXYR + XDEL                                            MAS 2000
          GOTO 155                                                      MAS 2010
  151     ZXYR = DSQRT(TEMP/3.141593+ZXYR**2)                           MAS 2020
          GOTO 155                                                      MAS 2030
  152     ZXYR = DSQRT(2.0*TEMP+ZXYR **2)                               MAS 2040
          GOTO 155                                                      MAS 2050
  153     CONTINUE                                                      MAS 2060
          X(J) = XX(J) + H                                              MAS 2070
          XX(J+1) = X(J) + H                                            MAS 2080
          GOTO 156                                                      MAS 2090
  154     ZXYR = (3.0*TEMP/(4.0*3.141593)+ZXYR**3)**ZTEMP               MAS 2100
  155     CONTINUE                                                      MAS 2110
          XX(J+1) = ZXYR                                                MAS 2120
  156   CONTINUE                                                        MAS 2130
  157 CONTINUE                                                          MAS 2140
      IF(NGEM .EQ. 10 .OR. NGEM .EQ. 14) GOTO 164                       MAS 2150
      JXLM = JMAX                                                       MAS 2160
      IF(MGEM .EQ. 8) JXLM = IMAX                                       MAS 2170
      DO 161 J=1,JXLM                                                   MAS 2180
        GOTO(158,159,160,161,161,158,159,159,158,161,158,159,158,161),  MAS 2190
     1   NGEM                                                           MAS 2200
  158   X(J) = (XX(J+1)+XX(J)) * 0.5                                    MAS 2210
        GOTO 161                                                        MAS 2220
  159   X(J) = SQRT((XX(J+1)**2+XX(J)**2)*0.5)                          MAS 2230
        GOTO 161                                                        MAS 2240
  160   X(J) = ((XX(J+1)**3+XX(J)**3)*0.5)**ZTEMP                       MAS 2250
  161 CONTINUE                                                          MAS 2260
      IF(MGEM-8) 164,162,164                                            MAS 2270
  162 DO 163 N=1,NXY                                                    MAS 2280
        TEMP1 = XX(N)                                                   MAS 2290
        TEMP2 = YY(N)                                                   MAS 2300
        XX(N) = TEMP2                                                   MAS 2310
        YY(N) = TEMP1                                                   MAS 2320
        TEMP1 = X(N)                                                    MAS 2330
        TEMP2 = Y(N)                                                    MAS 2340
        X(N) = TEMP2                                                    MAS 2350
        Y(N) = TEMP1                                                    MAS 2360
  163 CONTINUE                                                          MAS 2370
  164 CONTINUE                                                          MAS 2380
      IF(NOUT .NE. 0) GOTO 165                                          MAS 2390
      IF(INEG .GT. 0) GOTO 165                                          MAS 2400
      GOTO 184                                                          MAS 2410
  165 CONTINUE                                                          MAS 2420
      GOTO(166,168,169,169,169,170,173,174,175,176,177,181,182,183),NGEMMAS 2430
  166 WRITE (IOUT,1005) XX(JMAX+1)                                      MAS 2440
  167 WRITE (IOUT,1006)                                                 MAS 2450
      WRITE (IOUT,1007) (MSHJ(J),XSHJ(J),J=1,NREGJ)                     MAS 2460
      WRITE (IOUT,1008) JMAX                                            MAS 2470
      WRITE (IOUT,1009)                                                 MAS 2480
      WRITE (IOUT,1010) (J,XX(J),J=2,JMXP1)                             MAS 2490
      WRITE (IOUT,1011)                                                 MAS 2500
      WRITE (IOUT,1010) (J,X(J),J=1,JMAX)                               MAS 2510
      GOTO 184                                                          MAS 2520
  168 WRITE (IOUT,1012) XX(JMAX+1)                                      MAS 2530
      GOTO 167                                                          MAS 2540
  169 WRITE (IOUT,1013) XX(JMAX+1)                                      MAS 2550
      GOTO 167                                                          MAS 2560
  170 WRITE (IOUT,1014) XX(JMAX+1),YY(IMAX+1)                           MAS 2570
  171 WRITE (IOUT,1006)                                                 MAS 2580
      WRITE (IOUT,1007) (MSHJ(J),XSHJ(J),J=1,NREGJ)                     MAS 2590
      WRITE (IOUT,1015)                                                 MAS 2600
      WRITE (IOUT,1007) (MSHI(I),XSHI(I),I=1,NREGI)                     MAS 2610
      WRITE (IOUT,1016) JMAX,IMAX                                       MAS 2620
      WRITE (IOUT,1009)                                                 MAS 2630
      WRITE (IOUT,1010) (J,XX(J),J=2,JMXP1)                             MAS 2640
      WRITE (IOUT,1017)                                                 MAS 2650
      WRITE (IOUT,1010) (I,YY(I),I=2,IMXP1)                             MAS 2660
      WRITE (IOUT,1011)                                                 MAS 2670
      WRITE (IOUT,1010) (J,X(J),J=1,JMAX)                               MAS 2680
      IF(NGEM .NE. 10) GOTO 172                                         MAS 2690
      WRITE (IOUT,1000)                                                 MAS 2700
      WRITE (IOUT,1001) (I,Y(I),PDI(I),I=1,IMAX)                        MAS 2710
      GOTO 184                                                          MAS 2720
  172 CONTINUE                                                          MAS 2730
      WRITE (IOUT,1017)                                                 MAS 2740
      WRITE (IOUT,1010) (I,Y(I),I=1,IMAX)                               MAS 2750
      DO 2001 I=1,IMAX                                                  MAS 2760
        YFLX(I) = Y(I)                                                  MAS 2770
 2001 CONTINUE                                                          MAS 2780
      DO 2002 J=1,JMAX                                                  MAS 2790
        XFLX(J) = X(J)                                                  MAS 2800
 2002 CONTINUE                                                          MAS 2810
      GOTO 184                                                          MAS 2820
  173 WRITE (IOUT,1018) XX(JMAX+1),YY(IMAX+1)                           MAS 2830
      GOTO 171                                                          MAS 2840
  174 WRITE (IOUT,1019) XX(JMAX+1),YY(IMAX+1)                           MAS 2850
      GOTO 171                                                          MAS 2860
  175 WRITE (IOUT,1020) XX(JMAX+1),YY(IMAX+1)                           MAS 2870
      GOTO 171                                                          MAS 2880
  176 CONTINUE                                                          MAS 2890
      WRITE (IOUT,1002) XX(JMAX+1),YY(IMAX+1)                           MAS 2900
      GOTO 171                                                          MAS 2910
  177 WRITE (IOUT,1021) XX(JMAX+1),YY(IMAX+1),ZZ(KBMAX+1)               MAS 2920
  178 WRITE (IOUT,1006)                                                 MAS 2930
      WRITE (IOUT,1007) (MSHJ(J),XSHJ(J),J=1,NREGJ)                     MAS 2940
      WRITE (IOUT,1015)                                                 MAS 2950
      WRITE (IOUT,1007) (MSHI(I),XSHI(I),I=1,NREGI)                     MAS 2960
      WRITE (IOUT,1022)                                                 MAS 2970
      WRITE (IOUT,1007) (MSHKB(I),XSHKB(I),I=1,NREGKB)                  MAS 2980
      WRITE (IOUT,1023) JMAX,IMAX,KBMAX                                 MAS 2990
      WRITE (IOUT,1009)                                                 MAS 3000
      WRITE (IOUT,1010) (J,XX(J),J=2,JMXP1)                             MAS 3010
      WRITE (IOUT,1017)                                                 MAS 3020
      WRITE (IOUT,1010) (I,YY(I),I=2,IMXP1)                             MAS 3030
      WRITE (IOUT,1024)                                                 MAS 3040
      WRITE (IOUT,1010) (KB,ZZ(KB),KB=2,KBMXP1)                         MAS 3050
      WRITE (IOUT,1011)                                                 MAS 3060
      WRITE (IOUT,1010) (J,X(J),J=1,JMAX)                               MAS 3070
      IF(NGEM .NE. 14) GOTO 179                                         MAS 3080
      WRITE (IOUT,1000)                                                 MAS 3090
      WRITE (IOUT,1001) (I,Y(I),PDI(I),I=1,IMAX)                        MAS 3100
      GOTO 180                                                          MAS 3110
  179 CONTINUE                                                          MAS 3120
      WRITE (IOUT,1017)                                                 MAS 3130
      WRITE (IOUT,1010) (I,Y(I),I=1,IMAX)                               MAS 3140
  180 CONTINUE                                                          MAS 3150
      WRITE (IOUT,1024)                                                 MAS 3160
      WRITE (IOUT,1010) (KB,Z(KB),KB=1,KBMAX)                           MAS 3170
      GOTO 184                                                          MAS 3180
  181 WRITE (IOUT,1025) XX(JMAX+1),YY(IMAX+1),ZZ(KBMAX+1)               MAS 3190
      GOTO 178                                                          MAS 3200
  182 WRITE (IOUT,1026) XX(JMAX+1),YY(IMAX+1),ZZ(KBMAX+1)               MAS 3210
      GOTO 178                                                          MAS 3220
  183 CONTINUE                                                          MAS 3230
      WRITE (IOUT,1003) XX(JMAX+1),YY(IMAX+1),ZZ(KBMAX+1)               MAS 3240
      GOTO 178                                                          MAS 3250
  184 CONTINUE                                                          MAS 3260
      IF(IND .GT. 0) GOTO 194                                           MAS 3270
      DO 185 J=1,JMAX                                                   MAS 3280
        IF(XX(J)-XX(J+1)) 185,188,188                                   MAS 3290
  185 CONTINUE                                                          MAS 3300
      DO 186 I=1,IMAX                                                   MAS 3310
        IF(YY(I)-YY(I+1)) 186,188,188                                   MAS 3320
  186 CONTINUE                                                          MAS 3330
      DO 187 KB=1,KBMAX                                                 MAS 3340
        IF(ZZ(KB)-ZZ(KB+1)) 187,187,188                                 MAS 3350
  187 CONTINUE                                                          MAS 3360
      GOTO 189                                                          MAS 3370
  188 STOP 1357                                                         MAS 3380
  189 IF(MESHGE .EQ. 0) GOTO 190                                        MAS 3390
      WRITE (IOUT,1027)                                                 MAS 3400
  190 CONTINUE                                                          MAS 3410
      IF(NGEM .NE. 10 .AND. NGEM .NE. 14) GOTO 194                      MAS 3420
      DO 193 I=1,NREGI                                                  MAS 3430
        T1 = 2 * MSHI(I)                                                MAS 3440
        T2 = XSHI(I)                                                    MAS 3450
        DO 192 J=1,NREGJ                                                MAS 3460
          T3 = MSHJ(J)                                                  MAS 3470
          T4 = XSHJ(J)                                                  MAS 3480
          T5 = (T1*T4) / (T2*T3)                                        MAS 3490
          T6 = ABS(T5-1.0)                                              MAS 3500
          IF(T6 .LE. 0.00001) GOTO 191                                  MAS 3510
          WRITE (IOUT,1004) I,J,T1,T4,T3,T2,T5                          MAS 3520
  191     CONTINUE                                                      MAS 3530
  192   CONTINUE                                                        MAS 3540
  193 CONTINUE                                                          MAS 3550
  194 CONTINUE                                                          MAS 3560
      RETURN                                                            MAS 3570
C                                                                       MAS 3580
 1000 FORMAT (1H0,'    I   ODD J  EVEN J')                              MAS 3590
 1001 FORMAT (1H ,I5,2F8.3,I5,2F8.3,I5,2F8.3,I5,2F8.3,I5,2F8.3,I5,2F8.3)MAS 3600
 1002 FORMAT (2H0 ,'TWO DIMENSIONAL TRIANGULAR GEOMETRY (X,Y)',2H  ,'WIDMAS 3610
     1TH',1PE14.6,2H  ,'HEIGHT',E14.6//)                                MAS 3620
 1003 FORMAT (2H0 ,'THREE DIMENSIONAL TRIANGULAR GEOMETRY (X,Y,Z)',2H  ,MAS 3630
     1 'WIDTH',1PE14.6,2H  ,'HEIGHT',E14.6,2H  ,'DEPTH',E14.6//)        MAS 3640
 1004 FORMAT (1H0,'******WARNING--TRIANGLES NOT EQUILATERAL FOR HORIZONTMAS 3650
     1AL-VERTICAL REGION',2I3/1H ,'2*I, WIDTH, J, HEIGHT, AND (2*I*WIDTHMAS 3660
     2)/(J*HEIGHT) ARE',5(1PE13.5))                                     MAS 3670
 1005 FORMAT (2H0 ,'ONE DIMENSIONAL SLAB GEOMETRY (X)',2H  ,'WIDTH',    MAS 3680
     1 1PE14.6//)                                                       MAS 3690
 1006 FORMAT (2H0 ,'REGION SPECIFICATIONS'/4H    ,'PTS  REGION WIDTH')  MAS 3700
 1007 FORMAT (1H ,I6,1PE14.6,I6,E14.6,I6,E14.6,I6,E14.6,I6,E14.6,I6,    MAS 3710
     1 E14.6)                                                           MAS 3720
 1008 FORMAT (2H0 ,'X-DIR. POINTS',I4)                                  MAS 3730
 1009 FORMAT (2H0 ,'DISTANCES TO MESH INTERVAL',1H ,'INTERFACES'/1H0,'  MAS 3740
     1  J   DIST.')                                                     MAS 3750
 1010 FORMAT (1H ,I5,F8.3,I5,F8.3,I5,F8.3,I5,F8.3,I5,F8.3,I5,F8.3,I5,   MAS 3760
     1 F8.3,I5,F8.3,I5,F8.3)                                            MAS 3770
 1011 FORMAT (2H0 ,'DISTANCES TO FLUX POINTS'/1H0,'    J   DIST.')      MAS 3780
 1012 FORMAT (2H0 ,'ONE DIMENSIONAL CYLINDRICAL GEOMETRY (R)',2H  ,'WIDTMAS 3790
     1H',1PE14.6//)                                                     MAS 3800
 1013 FORMAT (2H0 ,'ONE DIMENSIONAL SPHERICAL GEOMETRY (R)',2H  ,'WIDTH'MAS 3810
     1 ,1PE14.6//)                                                      MAS 3820
 1014 FORMAT (2H0 ,'TWO DIMENSIONAL SLAB GEOMETRY (X,Y)',2H  ,'WIDTH',  MAS 3830
     1 1PE14.6,2H  ,'HEIGHT',E14.6//)                                   MAS 3840
 1015 FORMAT (4H0   ,'PTS  REGION HEIGHT')                              MAS 3850
 1016 FORMAT (2H0 ,'X-DIR. POINTS',I4,9H         ,'Y-DIR. POINTS',I4)   MAS 3860
 1017 FORMAT (1H0,'    I   DIST.')                                      MAS 3870
 1018 FORMAT (2H0 ,'TWO DIMENSIONAL CYLINDRICAL GEOMETRY (R,Z)',2H  ,'WIMAS 3880
     1DTH',1PE14.6,2H  ,'HEIGHT',E14.6//)                               MAS 3890
 1019 FORMAT (2H0 ,'TWO DIMENSIONAL CIRCULAR GEOMETRY (R,THETA)',2H  ,'WMAS 3900
     1IDTH',1PE14.6,2H  ,'HEIGHT',E14.6//)                              MAS 3910
 1020 FORMAT (2H0 ,'TWO DIMENSIONAL HEXAGIONAL GEOMETRY (X,Y)',2H  ,'WIDMAS 3920
     1TH',1PE14.6,2H  ,'HEIGHT',E14.6//)                                MAS 3930
 1021 FORMAT (2H0 ,'THREE DIMENSIONAL SLAB GEOMETRY (X,Y,Z)',2H  ,'WIDTHMAS 3940
     1',1PE14.6,2H  ,'HEIGHT',E14.6,2H  ,'DEPTH',E14.6//)               MAS 3950
 1022 FORMAT (4H0   ,'PTS  REGION DEPTH')                               MAS 3960
 1023 FORMAT (2H0 ,'X-DIR. POINTS',I4,9H         ,'Y-DIR. POINTS',I4,9H MAS 3970
     1        ,'Z-DIR. POINTS',I4)                                      MAS 3980
 1024 FORMAT (1H0,'    KB   DIST')                                      MAS 3990
 1025 FORMAT (2H0 ,'THREE DIMENSIONAL CYLINDRICAL GEOMETRY (R,THETA,Z)',MAS 4000
     1 2H  ,'WIDTH',1PE14.6,2H  ,'HEIGHT',E14.6,2H  ,'DEPTH',E14.6//)   MAS 4010
 1026 FORMAT (2H0 ,'THREE DIMENSIONAL HEXAGIONAL GEOMETRY (X,Y,Z)',2H  ,MAS 4020
     1 'WIDTH',1PE14.6,2H  ,'HEIGHT',E14.6,2H  ,'DEPTH',E14.6//)        MAS 4030
 1027 FORMAT (1H0,'*****WARNING--REGION VOLUMES NOT EQUAL IN HEX GEOM.')MAS 4040
      END                                                               MAS 4050
      SUBROUTINE HOWE(NCY,IND1,IND2,IHW,ITX,ITM)                        HOW   10
C                                                                       HOW   20
CHOWE --092 ***CITATION*** GET TYPE OF EIGNVALUE CALC./ CF-CALR         HOW   30
C                                                                       HOW   40
C     INPUT - IND1.EQ.0 = INITIAL COND.   IND1.GT.0 = DURING CYCLE      HOW   50
C     OUTPUT - IND2.EQ.0 = NO INITIAL COND.   IND2.GT.0 = YES           HOW   60
C     OUTPUT - IHW = TYPE EIGN.   ITX = NO. ITER.   ITM = TIME          HOW   70
C                                                                       HOW   80
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,HOW   90
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   HOW  100
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), HOW  110
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    HOW  120
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    HOW  130
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   HOW  140
     6 IXPUT(9999),XPUT(9999)                                           HOW  150
C                                                                       HOW  160
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   HOW  170
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKHOW  180
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    HOW  190
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  HOW  200
     4 ITMAX,ITIME,BET(211),DEL(211)                                    HOW  210
C                                                                       HOW  220
C                                                                       HOW  230
      DO 100 I=44,49                                                    HOW  240
        IX(I) = 0                                                       HOW  250
  100 CONTINUE                                                          HOW  260
      IX(23) = 0                                                        HOW  270
      IX(128) = 0                                                       HOW  280
      IF(NGC(11) .LT. 0) GOTO 101                                       HOW  290
      IX(49) = NGC(11)                                                  HOW  300
      GOTO 102                                                          HOW  310
  101 IX(44) = IABS(NGC(11))                                            HOW  320
  102 IF(IND1 .GT. 0) GOTO 107                                          HOW  330
      DO 103 I=1,24                                                     HOW  340
        IX(I+100) = IEDG(I)                                             HOW  350
  103 CONTINUE                                                          HOW  360
      IF(IX(3) .EQ. NGC(12)) IX(23) = 1                                 HOW  370
      EPI1 = EPI(1)                                                     HOW  380
      IF(NGC(1) .EQ. 0) GOTO 110                                        HOW  390
      IF(NDPL(1) .EQ. 0) GOTO 110                                       HOW  400
      IF(NCY .GT. 1) GOTO 105                                           HOW  410
      IF(NDPL(7) .NE. 0) GOTO 104                                       HOW  420
      IND2 = 0                                                          HOW  430
      GOTO 136                                                          HOW  440
  104 IND2 = 1                                                          HOW  450
      IHW = NDPL(7)                                                     HOW  460
      ITX = ITMX(1)                                                     HOW  470
      ITM = ITMX(19)                                                    HOW  480
      IF(NDPL(7) .GT. 0) GOTO 111                                       HOW  490
      GOTO 136                                                          HOW  500
  105 IF(NDPL(8) .NE. 0) GOTO 106                                       HOW  510
      IND2 = 0                                                          HOW  520
      GOTO 136                                                          HOW  530
  106 IND2 = 1                                                          HOW  540
      IHW = NDPL(8)                                                     HOW  550
      ITX = ITMX(1)                                                     HOW  560
      ITM = ITMX(19)                                                    HOW  570
      IF(NDPL(8) .GT. 0) GOTO 111                                       HOW  580
      GOTO 136                                                          HOW  590
  107 DO 108 I=1,24                                                     HOW  600
        IX(I+100) = IXPUT(I)                                            HOW  610
  108 CONTINUE                                                          HOW  620
      EPI1 = EPI(4)                                                     HOW  630
      IF(NCY .GT. 1) GOTO 109                                           HOW  640
      IHW = NDPL(9)                                                     HOW  650
      ITX = ITMX(2)                                                     HOW  660
      ITM = ITMX(20)                                                    HOW  670
      IF(NDPL(9) .GT. 0) GOTO 111                                       HOW  680
      GOTO 136                                                          HOW  690
  109 IHW = NDPL(10)                                                    HOW  700
      ITX = ITMX(2)                                                     HOW  710
      ITM = ITMX(20)                                                    HOW  720
      IF(NDPL(10) .GT. 0) GOTO 111                                      HOW  730
      GOTO 136                                                          HOW  740
  110 IND2 = 1                                                          HOW  750
      IHW = NGC(10)                                                     HOW  760
      ITX = ITMX(1)                                                     HOW  770
      ITM = ITMX(19)                                                    HOW  780
      IF(NGC(10) .LE. 0) GOTO 136                                       HOW  790
  111 GOTO 118                                                          HOW  800
  112 NOW = 1                                                           HOW  810
      GOTO 117                                                          HOW  820
  113 NOW = 2                                                           HOW  830
      GOTO 117                                                          HOW  840
  114 NOW = 3                                                           HOW  850
      GOTO 117                                                          HOW  860
  115 NOW = 4                                                           HOW  870
      GOTO 117                                                          HOW  880
  116 NOW = 5                                                           HOW  890
  117 WRITE (IOUT,1000) NOW                                             HOW  900
C                                                                       HOW  910
      CALL EXIT                                                         HOW  920
C                                                                       HOW  930
  118 IO10 = IX(77)                                                     HOW  940
  119 READ (IO10,END=112) N1,N2,N3                                      HOW  950
      IF(N1 .EQ. 28) GOTO 121                                           HOW  960
      IF(N2 .EQ. 0) GOTO 119                                            HOW  970
      DO 120 I=1,N2                                                     HOW  980
        READ (IO10)                                                     HOW  990
  120 CONTINUE                                                          HOW 1000
      GOTO 119                                                          HOW 1010
  121 IF(N3 .EQ. 0) GOTO 113                                            HOW 1020
      DO 124 N=1,N3                                                     HOW 1030
        READ (IO10) (NSRH(I),I=1,24),X,(XSRH1(I),I=2,6)                 HOW 1040
        IF(NSRH(1) .EQ. IHW) GOTO 125                                   HOW 1050
        IF(NSRH(2) .EQ. 3) GOTO 123                                     HOW 1060
        IF(NSRH(10) .NE. -1) GOTO 124                                   HOW 1070
        READ (IO10) NXZ                                                 HOW 1080
        READ (IO10)                                                     HOW 1090
        DO 122 M=1,NXZ                                                  HOW 1100
          READ (IO10)                                                   HOW 1110
  122   CONTINUE                                                        HOW 1120
        GOTO 124                                                        HOW 1130
  123   CONTINUE                                                        HOW 1140
        READ (IO10) NXJ,NXI,NXKB                                        HOW 1150
        IF(NXJ .GT. 0) READ (IO10)                                      HOW 1160
        IF(NXI .GT. 0) READ (IO10)                                      HOW 1170
        IF(NXKB .GT. 0) READ (IO10)                                     HOW 1180
  124 CONTINUE                                                          HOW 1190
      GOTO 114                                                          HOW 1200
  125 IHW = NSRH(2)                                                     HOW 1210
      IF(X) 126,115,127                                                 HOW 1220
  126 XSRH1(1) = ABS(X) * XKEF1                                         HOW 1230
      GOTO 128                                                          HOW 1240
  127 XSRH1(1) = X                                                      HOW 1250
  128 IF(XSRH1(1) .LT. 0.1 .OR. XSRH1(1) .GT. 3.0) GOTO 116             HOW 1260
      SPARE(50) = XSRH1(1)                                              HOW 1270
      IF(NSRH(2) .EQ. 3) GOTO 130                                       HOW 1280
      IF(NSRH(2) .EQ. 1 .AND. NSRH(10) .EQ. -1) IX(128) = 1             HOW 1290
      IF(NSRH(10) .NE. -1) GOTO 134                                     HOW 1300
      IO18 = IX(85)                                                     HOW 1310
      REWIND IO18                                                       HOW 1320
      READ (IO10) (NXTR1(I),I=1,5),(XTR1(I),I=1,5)                      HOW 1330
      WRITE (IO18) (NXTR1(I),I=1,5),(XTR1(I),I=1,5)                     HOW 1340
      NXZ = NXTR1(1)                                                    HOW 1350
      READ (IO10) (NXTR1(I),I=1,NXZ)                                    HOW 1360
      WRITE (IO18) (NXTR1(I),I=1,NXZ)                                   HOW 1370
      DO 129 M=1,NXZ                                                    HOW 1380
        READ (IO10) (XTR1(NN),NN=1,NMAX)                                HOW 1390
        WRITE (IO18) (XTR1(NN),NN=1,NMAX)                               HOW 1400
  129 CONTINUE                                                          HOW 1410
      REWIND IO18                                                       HOW 1420
      GOTO 134                                                          HOW 1430
  130 CONTINUE                                                          HOW 1440
      IO18 = IX(85)                                                     HOW 1450
      REWIND IO18                                                       HOW 1460
      READ (IO10) (NXTR1(I),I=1,7),(XTR1(I),I=1,5)                      HOW 1470
      WRITE (IO18) (NXTR1(I),I=1,7),(XTR1(I),I=1,5)                     HOW 1480
      NXJ = NXTR1(1)                                                    HOW 1490
      NXI = NXTR1(2)                                                    HOW 1500
      NXKB = NXTR1(3)                                                   HOW 1510
      IF(NXJ .LE. 0) GOTO 131                                           HOW 1520
      READ (IO10) (XTR1(I),I=1,NXJ)                                     HOW 1530
      WRITE (IO18) (XTR1(I),I=1,NXJ)                                    HOW 1540
  131 IF(NXI .LE. 0) GOTO 132                                           HOW 1550
      READ (IO10) (XTR1(I),I=1,NXI)                                     HOW 1560
      WRITE (IO18) (XTR1(I),I=1,NXI)                                    HOW 1570
  132 IF(NXKB .LE. 0) GOTO 133                                          HOW 1580
      READ (IO10) (XTR1(I),I=1,NXKB)                                    HOW 1590
      WRITE (IO18) (XTR1(I),I=1,NXKB)                                   HOW 1600
  133 CONTINUE                                                          HOW 1610
      REWIND IO18                                                       HOW 1620
  134 CONTINUE                                                          HOW 1630
      REWIND IO10                                                       HOW 1640
      DO 135 I=1,6                                                      HOW 1650
        IX(I+43) = NSRH(I+3)                                            HOW 1660
  135 CONTINUE                                                          HOW 1670
  136 CONTINUE                                                          HOW 1680
      T1 = IMAX * JMAX                                                  HOW 1690
      T2 = T1**0.25 * 1.5                                               HOW 1700
      IA = T2                                                           HOW 1710
      I = IA + IX(29) / 2                                               HOW 1720
      IX(35) = NUAC(22)                                                 HOW 1730
      IF(NUAC(22) .EQ. 0) IX(35) = I                                    HOW 1740
      IF(IHW .LT. 0 .OR. IHW .EQ. 1) NUAC(3) = 0                        HOW 1750
      RETURN                                                            HOW 1760
C                                                                       HOW 1770
 1000 FORMAT (1H0,'ERROR STOP 21-',I3)                                  HOW 1780
      END                                                               HOW 1790
      SUBROUTINE BIGS(F1,CONC,SS1,SS2,SS3,SS4,SS5,SSC,SIG,HOL,ONEOV,INDOBIG   10
     1 ,NJJR,KVX,MVX,NVX,NSETVX,BBND,XI,ZONEN,NNXTRA,NVO)               BIG   20
C                                                                       BIG   30
CBIGS --077 ***CITATION*** CALC. ZONE MACRO. SIGS/ CF-EIGN              BIG   40
C     INDO = 0 - NORMAL                                                 BIG   50
C     INDO = 1 - UPDATING IN DIRECT SEARCH                              BIG   60
C                                                                       BIG   70
C     ARRAY F1 IS USING THE CORE RESERVED FOR ARRAY SCAC                BIG   80
C                                                                       BIG   90
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,BIG  100
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   BIG  110
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), BIG  120
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    BIG  130
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    BIG  140
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   BIG  150
     6 IXPUT(9999),XPUT(9999)                                           BIG  160
C                                                                       BIG  170
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           BIG  180
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    BIG  190
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),BIG  200
     3 NCLASS(9999),NDP(9999)                                           BIG  210
C                                                                       BIG  220
CFZJ055                                                       25.09.07  BIG  230
C                                                                       BIG  240
      COMMON /MU/ MU4                                                   BIG  250
C                                                                       BIG  260
      DIMENSION F1(KVX,KVX,MVX),CONC(NVX,MVX),SS1(KVX,NVX,NSETVX),      BIG  270
     1 SS2(KVX,NVX,NSETVX),SS3(KVX,NVX,NSETVX),SS4(KVX,NVX,NSETVX),     BIG  280
     2 SS5(KVX,NVX,NSETVX),SSC(KVX,KVX ),SIG(KVX,MVX,10),               BIG  290
     3 HOL(NVX,NSETVX,10),ONEOV(KVX,NSETVX),NJJR(NVX,NSETVX),BBND(KVX), BIG  300
     4 XI(KVX),ZONEN(NVO),NNXTRA(NVX,NSETVX)                            BIG  310
C                                                                       BIG  320
C                                                                       BIG  330
      IO14 = IX(81)                                                     BIG  340
      I = 0                                                             BIG  350
      IF(INDO .EQ. 1) GOTO 105                                          BIG  360
      IF(NGC(19)) 100,100,148                                           BIG  370
  100 IF(MM1VX-MVX) 103,101,103                                         BIG  380
  101 IF(KM1VX-KVX) 103,102,103                                         BIG  390
  102 I = 0                                                             BIG  400
      GOTO 105                                                          BIG  410
  103 IF(INNO(24)) 102,104,102                                          BIG  420
  104 I = 1                                                             BIG  430
  105 DO 110 M=1,MMAX                                                   BIG  440
        DO 109 K=1,KMAX                                                 BIG  450
          SIG(K,M,1) = 0.0                                              BIG  460
          SIG(K,M,2) = 0.0                                              BIG  470
          SIG(K,M,3) = 0.0                                              BIG  480
          SIG(K,M,4) = 0.0                                              BIG  490
          SIG(K,M,5) = 0.0                                              BIG  500
          IF(I) 107,107,106                                             BIG  510
  106     SIG(K,M,6) = 0.0                                              BIG  520
  107     SIG(K,M,7) = 0.0                                              BIG  530
          SIG(K,M,8) = 0.0                                              BIG  540
          DO 108 KK=1,KMAX                                              BIG  550
            F1(KK,K,M) = 0.0                                            BIG  560
  108     CONTINUE                                                      BIG  570
  109   CONTINUE                                                        BIG  580
  110 CONTINUE                                                          BIG  590
      IF(IX(166) .LE. 1) GOTO 111                                       BIG  600
      IO12 = IX(79)                                                     BIG  610
      REWIND IO12                                                       BIG  620
      READ (IO12)                                                       BIG  630
  111 CONTINUE                                                          BIG  640
      DO 134 M=1,MMAX                                                   BIG  650
        NS = NXSET(M)                                                   BIG  660
        NR = NXODR(NS)                                                  BIG  670
        NSX = NSIG2(NR)                                                 BIG  680
        IF(IX(166) .LE. 1) GOTO 112                                     BIG  690
        NSUBZ = NZON(M)                                                 BIG  700
        NL = NSX * NSUBZ                                                BIG  710
        READ (IO12)                                                     BIG  720
        READ (IO12) (ZONEN(L),L=1,NL)                                   BIG  730
  112   CONTINUE                                                        BIG  740
        DO 133 K=1,KMAX                                                 BIG  750
          DO 132 N=1,NSX                                                BIG  760
            IF(IX(166) .LE. 0) GOTO 119                                 BIG  770
            IF(NNXTRA(N,NR) .NE. 10) GOTO 119                           BIG  780
            IF(IX(166) .NE. 1) GOTO 115                                 BIG  790
            CN = CONC(N,M)                                              BIG  800
            IF(CN .EQ. 0.0) GOTO 113                                    BIG  810
            CNS = SS5(K,N,NR)                                           BIG  820
            IF(CNS .EQ. 0.0) GOTO 113                                   BIG  830
            C = CNS / CN                                                BIG  840
            IF(C .GT. 200) GOTO 113                                     BIG  850
            Y = 2.0 * (SQRT(C*(C+1))-C)                                 BIG  860
            GOTO 114                                                    BIG  870
  113       Y = 1.0                                                     BIG  880
  114       S1 = Y * SS1(K,N,NR)                                        BIG  890
            S2 = Y * SS2(K,N,NR)                                        BIG  900
            CSS1 = CN * S1                                              BIG  910
            CSS2 = CN * S2                                              BIG  920
            GOTO 122                                                    BIG  930
  115       CONTINUE                                                    BIG  940
            CSS1 = 0.0                                                  BIG  950
            CSS2 = 0.0                                                  BIG  960
            DO 118 LZ=1,NSUBZ                                           BIG  970
              NSZ = (LZ-1) * NSX + N                                    BIG  980
              CN = ZONEN(NSZ)                                           BIG  990
              IF(CN .EQ. 0.0) GOTO 116                                  BIG 1000
              CNS = SS5(K,N,NR)                                         BIG 1010
              IF(CNS .EQ. 0.0) GOTO 116                                 BIG 1020
              C = CNS / CN                                              BIG 1030
              IF(C .GT. 200) GOTO 116                                   BIG 1040
              Y = 2.0 * (SQRT(C*(C+1))-C)                               BIG 1050
              GOTO 117                                                  BIG 1060
  116         Y = 1.0                                                   BIG 1070
  117         S1 = Y * SS1(K,N,NR)                                      BIG 1080
              S2 = Y * SS2(K,N,NR)                                      BIG 1090
              CSS1 = CSS1 + CN * S1                                     BIG 1100
              CSS2 = CSS2 + CN * S2                                     BIG 1110
  118       CONTINUE                                                    BIG 1120
            ZN = FLOAT(NSUBZ)                                           BIG 1130
            CSS1 = CSS1 / ZN                                            BIG 1140
            CSS2 = CSS2 / ZN                                            BIG 1150
            GOTO 122                                                    BIG 1160
  119       CONTINUE                                                    BIG 1170
            IF(NNXTRA(N,NR) .NE. 6) GOTO 120                            BIG 1180
            CSS1 = CONC(N,M) * (SS1(K,N,NR)-2.0*SS5(K,N,NR))            BIG 1190
            GOTO 121                                                    BIG 1200
  120       CSS1 = CONC(N,M) * SS1(K,N,NR)                              BIG 1210
  121       CSS2 = CONC(N,M) * SS2(K,N,NR)                              BIG 1220
  122       CONTINUE                                                    BIG 1230
            DTMP = 3.0 * CONC(N,M) * SS3(K,N,NR)                        BIG 1240
            IF(SIG(K,M,1)) 123,124,123                                  BIG 1250
  123       DTMP = DTMP + 1.0 / SIG(K,M,1)                              BIG 1260
  124       IF(DTMP) 125,126,125                                        BIG 1270
  125       SIG(K,M,1) = 1.0 / DTMP                                     BIG 1280
  126       SIG(K,M,3) = SIG(K,M,3) + CSS1                              BIG 1290
            SIG(K,M,4) = SIG(K,M,4) + CSS2 * SS4(K,N,NR)                BIG 1300
            SIG(K,M,7) = SIG(K,M,7) + CSS2 * HOL(N,NR,9)                BIG 1310
            IF(IX(5) .NE. 1) GOTO 131                                   BIG 1320
            IF(IX(128) .GT. 0) GOTO 131                                 BIG 1330
            IF(IX(44) .EQ. 0 .AND. IX(49) .EQ. 0) GOTO 129              BIG 1340
            IF(IX(49) .GT. 0) GOTO 128                                  BIG 1350
            IF(M .EQ. IX(44) .OR. M .EQ. IX(45) .OR. M .EQ. IX(46) .OR. BIG 1360
     1       M .EQ. IX(47) .OR. M .EQ. IX(48)) GOTO 129                 BIG 1370
            GOTO 131                                                    BIG 1380
  128       IF(IX(49) .NE. NCLASS(M)) GOTO 131                          BIG 1390
  129       DO 130 INI=12,18                                            BIG 1400
              IF(NJJR(N,NR) .NE. NSRH(INI)) GOTO 130                    BIG 1410
              SIG(K,M,5) = SIG(K,M,5) + CSS1                            BIG 1420
              SIG(K,M,8) = SIG(K,M,8) + CSS2 * SS4(K,N,NR)              BIG 1430
  130       CONTINUE                                                    BIG 1440
  131       CONTINUE                                                    BIG 1450
            IF(IX(5) .EQ. -1) SIG(K,M,5) = ONEOV(K,NR)                  BIG 1460
  132     CONTINUE                                                      BIG 1470
  133   CONTINUE                                                        BIG 1480
  134 CONTINUE                                                          BIG 1490
      IF(IX(166) .GT. 1) REWIND IO12                                    BIG 1500
      DO 139 NR=1,NSETMX                                                BIG 1510
        NSX = NSIG2(NR)                                                 BIG 1520
        NKC = NSIG3(NR)                                                 BIG 1530
        DO 138 N=1,NSX                                                  BIG 1540
          READ (IO4) ((SSC(KK,K),KK=1,NKC),K=1,NKC)                     BIG 1550
          DO 137 M=1,MMAX                                               BIG 1560
            NS = NXSET(M)                                               BIG 1570
            NRR = NXODR(NS)                                             BIG 1580
            IF(NRR .NE. NR) GOTO 137                                    BIG 1590
            DO 136 K=1,KMAX                                             BIG 1600
              DO 135 KK=1,KMAX                                          BIG 1610
                IF(K .EQ. KK) GOTO 135                                  BIG 1620
                F1(KK,K,M) = F1(KK,K,M) + SSC(KK,K) * CONC(N,M)         BIG 1630
  135         CONTINUE                                                  BIG 1640
  136       CONTINUE                                                    BIG 1650
  137     CONTINUE                                                      BIG 1660
  138   CONTINUE                                                        BIG 1670
  139 CONTINUE                                                          BIG 1680
      REWIND IO4                                                        BIG 1690
      IF(IX(128) .LE. 0) GOTO 143                                       BIG 1700
      IO18 = IX(85)                                                     BIG 1710
      REWIND IO18                                                       BIG 1720
      READ (IO18) NXZ                                                   BIG 1730
      READ (IO18) (NXTR1(I),I=1,NXZ)                                    BIG 1740
      DO 142 I=1,NXZ                                                    BIG 1750
        M = NXTR1(I)                                                    BIG 1760
        NS = NXSET(M)                                                   BIG 1770
        NR = NXODR(NS)                                                  BIG 1780
        NSX = NSIG2(NR)                                                 BIG 1790
        READ (IO18) (XTR1(N),N=1,NSX)                                   BIG 1800
        DO 141 N=1,NSX                                                  BIG 1810
          DO 140 K=1,KMAX                                               BIG 1820
            SIG(K,M,5) = SIG(K,M,5) + XTR1(N) * SS1(K,N,NR)             BIG 1830
            SIG(K,M,8) = SIG(K,M,8) + XTR1(N) * SS2(K,N,NR) *           BIG 1840
     1       SS4(K,N,NR)                                                BIG 1850
  140     CONTINUE                                                      BIG 1860
  141   CONTINUE                                                        BIG 1870
  142 CONTINUE                                                          BIG 1880
      REWIND IO18                                                       BIG 1890
  143 CONTINUE                                                          BIG 1900
      DO 147 M=1,MMAX                                                   BIG 1910
        DO 146 K=1,KMAX                                                 BIG 1920
          DO 145 KK=1,KMAX                                              BIG 1930
            SIG(K,M,2) = SIG(K,M,2) + F1(KK,K,M)                        BIG 1940
  145     CONTINUE                                                      BIG 1950
  146   CONTINUE                                                        BIG 1960
  147 CONTINUE                                                          BIG 1970
      IF(INDO .EQ. 1) GOTO 176                                          BIG 1980
  148 NGSCU = 0                                                         BIG 1990
      NGSCD = 0                                                         BIG 2000
      NSIGNL = 0                                                        BIG 2010
      IF(NGC(19) .LE. 0) GOTO 153                                       BIG 2020
      IF(IX(5) .EQ. -1) GOTO 151                                        BIG 2030
      DO 150 M=1,MMAX                                                   BIG 2040
        DO 149 K=1,KMAX                                                 BIG 2050
          SIG(K,M,5) = 0.0                                              BIG 2060
  149   CONTINUE                                                        BIG 2070
  150 CONTINUE                                                          BIG 2080
  151 CONTINUE                                                          BIG 2090
      REWIND IO14                                                       BIG 2100
      DO 152 K=1,KMAX                                                   BIG 2110
        READ (IO14) ((F1(KK,K,M),KK=1,KMAX),M=1,MMAX)                   BIG 2120
  152 CONTINUE                                                          BIG 2130
  153 CONTINUE                                                          BIG 2140
      N17 = NUAC(17)                                                    BIG 2150
      IF(N17 .EQ. 0) GOTO 159                                           BIG 2160
      NOTZ = 0                                                          BIG 2170
      DO 158 M=1,MMAX                                                   BIG 2180
        IF(N17 .NE. M) GOTO 158                                         BIG 2190
        NOTZ = 1                                                        BIG 2200
        DO 157 K=1,KMAX                                                 BIG 2210
          IF(XMIS(2) .GE. 0) GOTO 154                                   BIG 2220
          IF(BBND(K) .EQ. 0) GOTO 157                                   BIG 2230
  154     CONTINUE                                                      BIG 2240
          DO 155 KK=1,KMAX                                              BIG 2250
            F1(KK,K,M) = 0.0                                            BIG 2260
  155     CONTINUE                                                      BIG 2270
          DO 156 I=1,8                                                  BIG 2280
            SIG(K,M,I) = 0.0                                            BIG 2290
  156     CONTINUE                                                      BIG 2300
  157   CONTINUE                                                        BIG 2310
  158 CONTINUE                                                          BIG 2320
      IF(NOTZ .GT. 0) GOTO 159                                          BIG 2330
      WRITE (IOUT,1000)                                                 BIG 2340
C                                                                       BIG 2350
      CALL EXIT                                                         BIG 2360
C                                                                       BIG 2370
  159 CONTINUE                                                          BIG 2380
      DO 167 M=1,MMAX                                                   BIG 2390
        DO 166 K=1,KMAX                                                 BIG 2400
          DO 165 KK=1,KMAX                                              BIG 2410
            IF(F1(KK,K,M) .EQ. 0.0) GOTO 165                            BIG 2420
            T1 = ABS(SIG(K,M,2)/F1(KK,K,M))                             BIG 2430
            IF(T1 .LE. 1.0E+10) GOTO 160                                BIG 2440
            F1(KK,K,M) = 0.0                                            BIG 2450
            GOTO 165                                                    BIG 2460
  160       CONTINUE                                                    BIG 2470
            KKMK = KK - K                                               BIG 2480
            KXMKK = KMAX - KK                                           BIG 2490
            IF(KKMK) 164,162,163                                        BIG 2500
  162       NSIGNL = 1                                                  BIG 2510
            F1(KK,K,M) = 0.0                                            BIG 2520
            GOTO 165                                                    BIG 2530
  163       NGSCD = MAX0(NGSCD,KKMK)                                    BIG 2540
            GOTO 165                                                    BIG 2550
  164       NGSCU = MAX0(NGSCU,KXMKK)                                   BIG 2560
  165     CONTINUE                                                      BIG 2570
  166   CONTINUE                                                        BIG 2580
  167 CONTINUE                                                          BIG 2590
      IDTI = 0                                                          BIG 2600
      IF(NSIGNL) 169,169,168                                            BIG 2610
  168 WRITE (IOUT,1001)                                                 BIG 2620
      IDTI = 1                                                          BIG 2630
  169 IF(NGSCU-IX(29)) 170,171,170                                      BIG 2640
  170 WRITE (IOUT,1002) IX(29),NGSCU                                    BIG 2650
      IX(29) = NGSCU                                                    BIG 2660
      IDTI = 1                                                          BIG 2670
  171 IF(NGSCD-IX(28)) 172,173,172                                      BIG 2680
  172 IF(MU4 .EQ. 1) WRITE (IOUT,1003) IX(28),NGSCD                     BIG 2690
      IX(28) = NGSCD                                                    BIG 2700
      IDTI = 1                                                          BIG 2710
C                                                                       BIG 2720
C     IDTI=0 SUPRESSES SCATTERING ARRAY OUTPUT UNLESS WANTED            BIG 2730
C                                                                       BIG 2740
      IDTI = 0                                                          BIG 2750
  173 IF(IDTI) 174,174,175                                              BIG 2760
  174 IF(IX(103) .EQ. 0 .AND. IX(104) .EQ. 0) GOTO 176                  BIG 2770
C                                                                       BIG 2780
  175 CALL XSET(SIG,F1,KVX,MVX,IDTI)                                    BIG 2790
C                                                                       BIG 2800
  176 CONTINUE                                                          BIG 2810
      IF(IX(40) .GT. 0) GOTO 178                                        BIG 2820
      REWIND IO14                                                       BIG 2830
      DO 177 K=1,KMAX                                                   BIG 2840
        WRITE (IO14) ((F1(KK,K,M),KK=1,KMAX),M=1,MMAX)                  BIG 2850
  177 CONTINUE                                                          BIG 2860
      END FILE IO14                                                     BIG 2870
      REWIND IO14                                                       BIG 2880
  178 CONTINUE                                                          BIG 2890
      IF(NUAC(20) .LT. 0) GOTO 180                                      BIG 2900
      IF(NUAC(5) .GT. 10) GOTO 179                                      BIG 2910
      IF(NUAC(20) .EQ. 0 .AND. NGSCU .GT. 0) NUAC(20) = 1               BIG 2920
      GOTO 180                                                          BIG 2930
  179 IF(NUAC(20) .EQ. 0 .AND. NGSCU .GT. 0) NUAC(20) = 0               BIG 2940
  180 CONTINUE                                                          BIG 2950
      IF(NUAC(23) .GT. 0) GOTO 182                                      BIG 2960
      IF(NUAC(5) .GT. 10) GOTO 181                                      BIG 2970
      NUAC(23) = 1                                                      BIG 2980
      GOTO 182                                                          BIG 2990
  181 NUAC(23) = 3                                                      BIG 3000
      IF(NUAC(20) .LT. 0) NUAC(23) = 1                                  BIG 3010
      IF(NGSCU .GT. 0) NUAC(23) = 1                                     BIG 3020
  182 CONTINUE                                                          BIG 3030
      RETURN                                                            BIG 3040
C                                                                       BIG 3050
 1000 FORMAT (1H0,'ERROR STOP 30')                                      BIG 3060
 1001 FORMAT (1H0/1H0,'**********WARNING**********',2H  ,'THERE IS SCATTBIG 3070
     1ERING FROM SOME GROUP TO ITSELF - HAS BEEN SET TO ZERO')          BIG 3080
 1002 FORMAT (1H0/1H0,'**********WARNING**********',2H  ,'INPUT SPECIFIEBIG 3090
     1D UPSCATTER =',I3,2H  ,'HAS BEEN CHANGED TO ACTUAL UPSCATTER =',I3BIG 3100
     2 )                                                                BIG 3110
 1003 FORMAT (1H0/1H0,'**********WARNING**********',2H  ,'INPUT SPECIFIEBIG 3120
     1D DOWNSCATTER =',I3,2H  ,'HAS BEEN CHANGED TO ACTUAL DOWNSCATTER =BIG 3130
     2',I3)                                                             BIG 3140
      END                                                               BIG 3150
      SUBROUTINE DNSD(P1,P2,SOUR,NRGN,SCAC,SCAT,XII,DCONB,DCONR,PTSA,BETDNS   10
     1 ,DEL,E1,IVX,JVX,KVX,LVX,IVXP1,JVXP1,IVZ,KVZ,IOVX,IOVZ,SPAR,BIEMS,DNS   20
     2 NCRP,NSPA,SIG,PVOL,NCOMP,MVX,AIO,IX3738,XLAMDA,XI,XL,B2,IOADJ,   DNS   30
     3 IOFS,KGP1)                                                       DNS   40
C                                                                       DNS   50
CDNSD --096***CITATION*** FLUX CALCULATION CONTROL FOR 2-D/ CF-FLUX     DNS   60
C                                                                       DNS   70
      REAL*8 SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2S,XLL1,D8DNS   80
     1 ,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK,B5LK,D1, DNS   90
     2 D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,YRDB,SPR50DNS  100
     3 ,XLAST,BET(211),DEL(211),P2,SCAT,SOUR,XII,CKSS,XLAMDA,T2         DNS  110
C                                                                       DNS  120
      COMMON /ADUBP/ SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2SDNS  130
     1 ,XLL1,D8,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK, DNS  140
     2 B5LK,D1,D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,  DNS  150
     3 YRDB,SPR50,XLAST                                                 DNS  160
C                                                                       DNS  170
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,DNS  180
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   DNS  190
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), DNS  200
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    DNS  210
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    DNS  220
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   DNS  230
     6 IXPUT(9999),XPUT(9999)                                           DNS  240
C                                                                       DNS  250
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   DNS  260
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XELKDNS  270
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    DNS  280
     3 VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2,BETTA,SAMXI,IX25,IX28,I,J,KB,K,  DNS  290
     4 ITMAX,ITIME,BAT(211),DAL(211)                                    DNS  300
C                                                                       DNS  310
      DIMENSION P1(JVX,IVX),P2(JVX,IVX,KVX),NRGN(JVX,IVX),              DNS  320
     1 SCAC(KVX,MVX,KVX),SCAT(JVX,IVX),XII(KVX),DCONB(JVX,IVXP1,IOVX),  DNS  330
     2 DCONR(JVXP1,IVZ,IOVZ),PTSA(JVX,IVX,IOVX),SOUR(JVX,IVX),          DNS  340
     3 E1(LVX,KVX),SPAR(NCRP,NSPA),BIEMS(KVX),SIG(KVX,MVX,10),PVOL(LVX),DNS  350
     4 NCOMP(LVX),AIO(IX3738),XI(KVX),XL(6,KVX),B2(MVX,KVX)             DNS  360
C                                                                       DNS  370
C                                                                       DNS  380
C     INRB = 1  ORDINARY                                                DNS  390
C     INRB = 2  PERIODIC(REPEATING)                                     DNS  400
C     INRB = 3  90 DEGREE ROTATIONAL                                    DNS  410
C     INRB = 4  180 DEGREE ROTATIONAL                                   DNS  420
C                                                                       DNS  430
C                                                                       DNS  440
      INRB = IX(72) + 1                                                 DNS  450
      IO19 = IX(86)                                                     DNS  460
      IF(IX(135) .EQ. 1) REWIND IO19                                    DNS  470
      RMX = 1.0                                                         DNS  480
      RMN = 1.0                                                         DNS  490
      IX37 = IX(37)                                                     DNS  500
      DO 151 KT1=1,KMAX                                                 DNS  510
        IF(IX37 .EQ. 0) GOTO 103                                        DNS  520
        READ (IOADJ) AIO                                                DNS  530
        IF(IX(71) .GT. 0) GOTO 101                                      DNS  540
        K = KT1                                                         DNS  550
        GOTO 102                                                        DNS  560
  101   K = KGP1 - KT1                                                  DNS  570
  102   CONTINUE                                                        DNS  580
        IX(20) = 1                                                      DNS  590
        GOTO 106                                                        DNS  600
  103   CONTINUE                                                        DNS  610
        IF(IX(24) .GT. 0) GOTO 104                                      DNS  620
        K = KT1                                                         DNS  630
        GOTO 105                                                        DNS  640
  104   K = KGP1 - KT1                                                  DNS  650
  105   CONTINUE                                                        DNS  660
        IX(20) = K                                                      DNS  670
  106   CONTINUE                                                        DNS  680
        IF(IX(5) .EQ. -5) GOTO 113                                      DNS  690
        IF(IX(24) .EQ. 0) GOTO 107                                      DNS  700
        IF(IX(17) .GE. 1) GOTO 109                                      DNS  710
  107   CONTINUE                                                        DNS  720
        DO 108 L=1,LMAX                                                 DNS  730
          M = NCOMP(L)                                                  DNS  740
          E1(L,K) = XLAMDA * SIG(K,M,5) * PVOL(L)                       DNS  750
          IF(IX(24) .EQ. 0) GOTO 108                                    DNS  760
          IF(IX(17) .EQ. -2 .AND. IX(71) .GT. 0) E1(L,K) = SIG(K,M,5) * DNS  770
     1     PVOL(L)                                                      DNS  780
  108   CONTINUE                                                        DNS  790
  109   CONTINUE                                                        DNS  800
        IF(IX(24) .GT. 0) GOTO 112                                      DNS  810
C                                                                       DNS  820
C********SEARCH OPTIONS                                                 DNS  830
C                                                                       DNS  840
        IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 111                     DNS  850
        XII(K) = XI(K) / SPARE(50)                                      DNS  860
        GOTO 112                                                        DNS  870
  111   XII(K) = XI(K) * XLAMDA                                         DNS  880
  112   CONTINUE                                                        DNS  890
        GOTO 114                                                        DNS  900
  113   CONTINUE                                                        DNS  910
        IF(IX(132) .GT. 0) READ (IOFS) SPAR                             DNS  920
        BIEMS(K) = XLAMDA * XI(K)                                       DNS  930
  114 CONTINUE                                                          DNS  940
        IF(IX(24) .GT. 0) GOTO 121                                      DNS  950
        KSCT1 = K - IX28                                                DNS  960
        IF(KSCT1 .LE. 0) KSCT1 = 1                                      DNS  970
        KSCT2 = MAX0((K-1),1)                                           DNS  980
        IF(K .GE. KXMN8) KSCT2 = KVX                                    DNS  990
C                                                                       DNS 1000
C     IRECV IS THE GROUP NO. WHICH CAN UPSCATTER TO GROUP 1. IT IS NOT  DNS 1010
C     BEING USED AND IS SET TO 0 IN BEGN.                               DNS 1020
C                                                                       DNS 1030
        IF(K .LT. IRECV) KSCT2 = IRECV                                  DNS 1040
        DO 118 I=1,IVX                                                  DNS 1050
          DO 117 J=1,JVX                                                DNS 1060
            P1(J,I) = P2(J,I,K)                                         DNS 1070
            L = NRGN(J,I)                                               DNS 1080
            M = NCOMP(L)                                                DNS 1090
            CKSS = 0.0                                                  DNS 1100
            DO 116 KK=KSCT1,KSCT2                                       DNS 1110
              CKSS = CKSS + SCAC(KK,M,K) * P2(J,I,KK)                   DNS 1120
  116       CONTINUE                                                    DNS 1130
            SCAT(J,I) = CKSS * PVOL(L) + SOUR(J,I) * XII(K)             DNS 1140
  117     CONTINUE                                                      DNS 1150
  118   CONTINUE                                                        DNS 1160
        IF(IX(5) .NE. -5) GOTO 126                                      DNS 1170
        BM = BIEMS(K)                                                   DNS 1180
        NP = 0                                                          DNS 1190
        DO 120 I=1,IVX                                                  DNS 1200
          DO 119 J=1,JVX                                                DNS 1210
            NP = NP + 1                                                 DNS 1220
            SCAT(J,I) = SCAT(J,I) + BM * SPAR(NP,1)                     DNS 1230
  119     CONTINUE                                                      DNS 1240
  120   CONTINUE                                                        DNS 1250
        GOTO 126                                                        DNS 1260
  121   CONTINUE                                                        DNS 1270
        KSCT1 = K                                                       DNS 1280
        IF(K .GE. KXMN8) KSCT1 = KXMN8                                  DNS 1290
        KSCT2 = K + IX28                                                DNS 1300
        IF(KSCT2 .GT. KVX) KSCT2 = KVX                                  DNS 1310
        IF(K .LT. IRECV) KSCT1 = 1                                      DNS 1320
        DO 124 I=1,IVX                                                  DNS 1330
          DO 123 J=1,JVX                                                DNS 1340
            L = NRGN(J,I)                                               DNS 1350
            M = NCOMP(L)                                                DNS 1360
            P1(J,I) = P2(J,I,K)                                         DNS 1370
            CKSS = SOUR(J,I) * SIG(K,M,4)                               DNS 1380
            DO 122 KK=KSCT1,KSCT2                                       DNS 1390
              CKSS = CKSS + SCAC(K,M,KK) * P2(J,I,KK)                   DNS 1400
  122       CONTINUE                                                    DNS 1410
            SCAT(J,I) = CKSS * PVOL(L)                                  DNS 1420
  123     CONTINUE                                                      DNS 1430
  124   CONTINUE                                                        DNS 1440
  126   CONTINUE                                                        DNS 1450
        IF(IX(135) .EQ. 1) WRITE (IO19) P1                              DNS 1460
        NOINNR = NUAC(23)                                               DNS 1470
        NUAC20 = NUAC(20)                                               DNS 1480
        DO 133 INNR=1,NOINNR                                            DNS 1490
          IF(NUAC(5) .EQ. 10) GOTO 132                                  DNS 1500
          IF(NUAC20 .EQ. 2) GOTO 129                                    DNS 1510
          IF(IX(72) .EQ. 1) GOTO 131                                    DNS 1520
          IF(NUAC(5) .EQ. 9) GOTO 130                                   DNS 1530
C                                                                       DNS 1540
  129     CALL FWRD(SCAT,P2,DCONB,DCONR,PTSA,IVX,JVX,KVX,IVXP1,JVXP1,IVZDNS 1550
     1     ,KVZ,BET,DEL,NRGN,E1,LVX,IOVX,IOVZ)                          DNS 1560
C                                                                       DNS 1570
          IF(NUAC20 .GT. 0) GOTO 133                                    DNS 1580
C                                                                       DNS 1590
          CALL FXRD(SCAT,P2,DCONB,DCONR,PTSA,IVX,JVX,KVX,IVXP1,JVXP1,IVZDNS 1600
     1     ,KVZ,BET,DEL,NRGN,E1,LVX,IOVX,IOVZ)                          DNS 1610
C                                                                       DNS 1620
          GOTO 133                                                      DNS 1630
C                                                                       DNS 1640
  130     CALL HWRD(SCAT,P2,DCONB,DCONR,PTSA,IVX,JVX,KVX,IVXP1,JVXP1,IVZDNS 1650
     1     ,KVZ,BET,DEL,NRGN,E1,LVX,IOVX,IOVZ)                          DNS 1660
C                                                                       DNS 1670
          IF(NUAC20 .GT. 0) GOTO 133                                    DNS 1680
C                                                                       DNS 1690
          CALL HXRD(SCAT,P2,DCONB,DCONR,PTSA,IVX,JVX,KVX,IVXP1,JVXP1,IVZDNS 1700
     1     ,KVZ,BET,DEL,NRGN,E1,LVX,IOVX,IOVZ)                          DNS 1710
C                                                                       DNS 1720
          GOTO 133                                                      DNS 1730
C                                                                       DNS 1740
  131     CALL DPER(SCAT,P2,DCONB,DCONR,PTSA,NRGN,E1,BET,DEL,IVX,JVX,KVXDNS 1750
     1     ,IVXP1,JVXP1,IVZ,KVZ,LVX,IOVX,IOVZ)                          DNS 1760
C                                                                       DNS 1770
          GOTO 133                                                      DNS 1780
  132     CONTINUE                                                      DNS 1790
C                                                                       DNS 1800
          CALL FTRI(SCAT,P2,DCONB,DCONR,PTSA,IVX,JVX,KVX,IVXP1,JVXP1,IVZDNS 1810
     1     ,KVZ,BET,DEL,NRGN,E1,LVX,IOVX,IOVZ)                          DNS 1820
C                                                                       DNS 1830
  133   CONTINUE                                                        DNS 1840
        IF(NUAC(3) .GT. 0 .OR. IX(24) .GT. 0) GOTO 147                  DNS 1850
        XLL1 = 0.0                                                      DNS 1860
        XLL2 = 0.0                                                      DNS 1870
        XLL3 = 0.0                                                      DNS 1880
        XLL4 = 0.0                                                      DNS 1890
        N = IX(20)                                                      DNS 1900
        DO 135 M=1,MMAX                                                 DNS 1910
          B2(M,K) = 0.0                                                 DNS 1920
  135   CONTINUE                                                        DNS 1930
        DO 138 J=1,JMAX                                                 DNS 1940
          XLL2 = XLL2 + P2(J,1,K) * DCONB(J,1,N)                        DNS 1950
          T1 = DCONB(J,IMXP1,N)                                         DNS 1960
          IF(NUAC(5) .NE. 10) GOTO 136                                  DNS 1970
          IF(J .EQ. 1) GOTO 138                                         DNS 1980
          T1 = DCONB(J-1,IMXP1,N)                                       DNS 1990
  136     CONTINUE                                                      DNS 2000
          IF(T1-4096.0E-13) 137,138,137                                 DNS 2010
  137     XLL4 = XLL4 + P2(J,IMAX,K) * T1                               DNS 2020
  138   CONTINUE                                                        DNS 2030
        DO 140 I=1,IMAX                                                 DNS 2040
          XLL1 = XLL1 + P2(1,I,K) * DCONR(1,I,N)                        DNS 2050
          T1 = DCONR(JMXP1,I,N)                                         DNS 2060
          IF(T1-4096.0E-13) 139,140,139                                 DNS 2070
  139     XLL3 = XLL3 + P2(JMAX,I,K) * T1                               DNS 2080
  140   CONTINUE                                                        DNS 2090
        GOTO(141,142,143,144),INRB                                      DNS 2100
  141   XLEK = XLEK + XLL1 + XLL2 + XLL3 + XLL4                         DNS 2110
        GOTO 145                                                        DNS 2120
  142   XLEK = XLEK + XLL2 + XLL4                                       DNS 2130
        XLL1 = XLL1 - XLL3                                              DNS 2140
        XLL3 = -XLL1                                                    DNS 2150
        GOTO 145                                                        DNS 2160
  143   XLEK = XLEK + XLL1 + XLL2                                       DNS 2170
        XLL4 = -XLL3                                                    DNS 2180
        GOTO 145                                                        DNS 2190
  144   XLEK = XLEK + XLL1 + XLL2 + XLL4                                DNS 2200
        XLL3 = 0.0                                                      DNS 2210
  145   CONTINUE                                                        DNS 2220
        XL(1,K) = XLL1                                                  DNS 2230
        XL(2,K) = XLL2                                                  DNS 2240
        XL(3,K) = XLL3                                                  DNS 2250
        XL(4,K) = XLL4                                                  DNS 2260
        IF(NUAC(17) .LE. 0) GOTO 146                                    DNS 2270
C                                                                       DNS 2280
        CALL FINS(P2,B2,NRGN,DCONB,DCONR,SCAC,XL,IVX,JVX,KVX,LVX,IVXP1, DNS 2290
     1   JVXP1,IVZ,IOVX,IOVZ,PVOL,NCOMP,MVX)                            DNS 2300
C                                                                       DNS 2310
  146   CONTINUE                                                        DNS 2320
  147   CONTINUE                                                        DNS 2330
        DO 150 I=1,IMAX                                                 DNS 2340
          DO 149 J=1,JMAX                                               DNS 2350
            TT1 = P1(J,I)                                               DNS 2360
            T2 = P2(J,I,K)                                              DNS 2370
            IF(TT1 .EQ. 0.0) GOTO 148                                   DNS 2380
            RATO = T2 / TT1                                             DNS 2390
            RMX = AMAX1(RMX,RATO)                                       DNS 2400
            RMN = AMIN1(RMN,RATO)                                       DNS 2410
  148       CONTINUE                                                    DNS 2420
  149     CONTINUE                                                      DNS 2430
  150   CONTINUE                                                        DNS 2440
  151 CONTINUE                                                          DNS 2450
      IF(IX(135) .EQ. 1) REWIND IO19                                    DNS 2460
      RETURN                                                            DNS 2470
      END                                                               DNS 2480
      SUBROUTINE WFLX(P1,P2,SOUR,SCAT,SCAC,DCONR,PTSA,NRGN,E1,BET,DEL,  WFL   10
     1 XII,IVX,JVX,KVX,LVX,IVXP1,JVXP1,IVZ,KVZ,IOVX,IOVZ,SPAR,BIEMS,NCRPWFL   20
     2 ,NSPA,SIG,PVOL,NCOMP,MVX,AIO,IX3738,XLAMDA,XI,XL,B2,IOADJ,IOFS,  WFL   30
     3 KGP1)                                                            WFL   40
C                                                                       WFL   50
CWFLX --110 ***CITATION*** LINE RELAXATION AND SOURCE FOR 1-D  CF-FLUX  WFL   60
C                                                                       WFL   70
      REAL*8 SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2S,XLL1,D8WFL   80
     1 ,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK,B5LK,D1, WFL   90
     2 D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,YRDB,SPR50WFL  100
     3 ,XLAST,P2,CKSS,SOUR,XII,BET(211),DEL(211),T,TEMP,TMF,SCAT,XLAMDA,WFL  110
     4 T2                                                               WFL  120
C                                                                       WFL  130
      COMMON /ADUBP/ SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2SWFL  140
     1 ,XLL1,D8,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK, WFL  150
     2 B5LK,D1,D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,  WFL  160
     3 YRDB,SPR50,XLAST                                                 WFL  170
C                                                                       WFL  180
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,WFL  190
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   WFL  200
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), WFL  210
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    WFL  220
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    WFL  230
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   WFL  240
     6 IXPUT(9999),XPUT(9999)                                           WFL  250
C                                                                       WFL  260
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   WFL  270
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XELKWFL  280
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    WFL  290
     3 VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2,BETTA,SAMXI,IX25,IX28,I,J,KB,K,  WFL  300
     4 ITMAX,ITIME,BAT(211),DAL(211)                                    WFL  310
C                                                                       WFL  320
      DIMENSION P1(JVX,IVX),P2(JVX,IVX,KVX),SOUR(JVX,IVX),SCAT(JVX,IVX),WFL  330
     1 SCAC(KVX,MVX,KVX),DCONR(JVXP1,IVZ,IOVZ),PTSA(JVX,IVX,IOVX),      WFL  340
     2 NRGN(JVX,IVX),E1(LVX,KVX),XII(KVX),SPAR(NCRP,NSPA),BIEMS(KVX),   WFL  350
     3 SIG(KVX,MVX,10),PVOL(LVX),NCOMP(LVX),AIO(IX3738),XI(KVX),        WFL  360
     4 XL(6,KVX),B2(MVX,KVX)                                            WFL  370
C                                                                       WFL  380
C                                                                       WFL  390
      IO19 = IX(86)                                                     WFL  400
      IF(IX(135) .EQ. 1) REWIND IO19                                    WFL  410
      RMX = 1.0                                                         WFL  420
      RMN = 1.0                                                         WFL  430
      IX37 = IX(37)                                                     WFL  440
      DO 142 KT1=1,KMAX                                                 WFL  450
        IF(IX37 .EQ. 0) GOTO 102                                        WFL  460
        READ (IOADJ) AIO                                                WFL  470
        IF(IX(71) .GT. 0) GOTO 100                                      WFL  480
        K = KT1                                                         WFL  490
        GOTO 101                                                        WFL  500
  100   K = KGP1 - KT1                                                  WFL  510
  101   CONTINUE                                                        WFL  520
        IX(20) = 1                                                      WFL  530
        GOTO 105                                                        WFL  540
  102   CONTINUE                                                        WFL  550
        IF(IX(24) .GT. 0) GOTO 103                                      WFL  560
        K = KT1                                                         WFL  570
        GOTO 104                                                        WFL  580
  103   K = KGP1 - KT1                                                  WFL  590
  104   CONTINUE                                                        WFL  600
        IX(20) = K                                                      WFL  610
  105   CONTINUE                                                        WFL  620
        IF(IX(5) .EQ. -5) GOTO 112                                      WFL  630
        IF(IX(24) .EQ. 0) GOTO 106                                      WFL  640
        IF(IX(17) .GE. 1) GOTO 108                                      WFL  650
  106   CONTINUE                                                        WFL  660
        DO 107 L=1,LMAX                                                 WFL  670
          M = NCOMP(L)                                                  WFL  680
          E1(L,K) = XLAMDA * SIG(K,M,5) * PVOL(L)                       WFL  690
          IF(IX(24) .EQ. 0) GOTO 107                                    WFL  700
          IF(IX(17) .EQ. -2 .AND. IX(71) .GT. 0) E1(L,K) = SIG(K,M,5) * WFL  710
     1     PVOL(L)                                                      WFL  720
  107   CONTINUE                                                        WFL  730
  108   CONTINUE                                                        WFL  740
        IF(IX(24) .GT. 0) GOTO 111                                      WFL  750
C                                                                       WFL  760
C********SEARCH OPTIONS                                                 WFL  770
C                                                                       WFL  780
        IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 110                     WFL  790
        XII(K) = XI(K) / SPARE(50)                                      WFL  800
        GOTO 111                                                        WFL  810
  110   XII(K) = XI(K) * XLAMDA                                         WFL  820
  111   CONTINUE                                                        WFL  830
        GOTO 113                                                        WFL  840
  112   CONTINUE                                                        WFL  850
        IF(IX(132) .GT. 0) READ (IOFS) SPAR                             WFL  860
        BIEMS(K) = XLAMDA * XI(K)                                       WFL  870
  113   CONTINUE                                                        WFL  880
        IF(IX(24) .GT. 0) GOTO 118                                      WFL  890
        KSCT1 = K - IX28                                                WFL  900
        IF(KSCT1 .LE. 0) KSCT1 = 1                                      WFL  910
        KSCT2 = MAX0((K-1),1)                                           WFL  920
        IF(K .GE. KXMN8) KSCT2 = KVX                                    WFL  930
C                                                                       WFL  940
C     IRECV IS THE GROUP NO. WHICH CAN UPSCATTER TO GROUP 1. IT IS NOT  WFL  950
C     BEING USED AND IS SET TO 0 IN BEGN.                               WFL  960
C                                                                       WFL  970
        IF(K .LT. IRECV) KSCT2 = IRECV                                  WFL  980
        DO 116 J=1,JVX                                                  WFL  990
          P1(J,1) = P2(J,1,K)                                           WFL 1000
          CKSS = SOUR(J,1) * XII(K)                                     WFL 1010
          L = NRGN(J,1)                                                 WFL 1020
          M = NCOMP(L)                                                  WFL 1030
          DO 115 KK=KSCT1,KSCT2                                         WFL 1040
            CKSS = CKSS + SCAC(KK,M,K) * P2(J,1,KK) * PVOL(L)           WFL 1050
  115     CONTINUE                                                      WFL 1060
          SCAT(J,1) = CKSS                                              WFL 1070
  116   CONTINUE                                                        WFL 1080
        IF(IX(5) .NE. -5) GOTO 122                                      WFL 1090
        BM = BIEMS(K)                                                   WFL 1100
        DO 117 J=1,JVX                                                  WFL 1110
          SCAT(J,1) = SCAT(J,1) + BM * SPAR(J,1)                        WFL 1120
  117   CONTINUE                                                        WFL 1130
        GOTO 122                                                        WFL 1140
  118   CONTINUE                                                        WFL 1150
        KSCT1 = K                                                       WFL 1160
        IF(K .GE. KXMN8) KSCT1 = KXMN8                                  WFL 1170
        KSCT2 = K + IX28                                                WFL 1180
        IF(KSCT2 .GT. KVX) KSCT2 = KVX                                  WFL 1190
        IF(K .LT. IRECV) KSCT1 = 1                                      WFL 1200
        DO 120 J=1,JVX                                                  WFL 1210
          L = NRGN(J,1)                                                 WFL 1220
          M = NCOMP(L)                                                  WFL 1230
          P1(J,1) = P2(J,1,K)                                           WFL 1240
          CKSS = SOUR(J,1) * SIG(K,M,4)                                 WFL 1250
          DO 119 KK=KSCT1,KSCT2                                         WFL 1260
            CKSS = CKSS + SCAC(K,M,KK) * P2(J,1,KK)                     WFL 1270
  119     CONTINUE                                                      WFL 1280
          SCAT(J,1) = CKSS * PVOL(L)                                    WFL 1290
  120   CONTINUE                                                        WFL 1300
  122   CONTINUE                                                        WFL 1310
        IF(IX(135) .EQ. 1) WRITE (IO19) P1                              WFL 1320
        N = IX(20)                                                      WFL 1330
        DEL(1) = 0.0                                                    WFL 1340
        D4 = DCONR(2,1,N)                                               WFL 1350
        IF(P2(1,1,K)) 124,128,124                                       WFL 1360
  124   BET(1) = SCAT(1,1) / D4                                         WFL 1370
        M = NCOMP(1)                                                    WFL 1380
        IF(NUAC(19) .GE. 0) GOTO 126                                    WFL 1390
        IF(JVX .NE. 1) GOTO 125                                         WFL 1400
        P2(1,1,K) = SCAT(1,1) / (PTSA(1,1,N)+E1(1,K))                   WFL 1410
        GOTO 134                                                        WFL 1420
  125   CONTINUE                                                        WFL 1430
        DEL(1) = D4 / (PTSA(1,1,N)+E1(1,K))                             WFL 1440
        GOTO 128                                                        WFL 1450
  126   CKSS = E1(1,K)                                                  WFL 1460
        CKSS = CKSS + DCONR(1,1,N) + DCONR(2,1,N) + (SIG(K,M,2)+        WFL 1470
     1   SIG(K,M,3)+SIG(K,M,9)) * PVOL(1)                               WFL 1480
        IF(JVX .NE. 1) GOTO 127                                         WFL 1490
        P2(1,1,K) = SCAT(1,1) / CKSS                                    WFL 1500
        GOTO 134                                                        WFL 1510
  127   CONTINUE                                                        WFL 1520
        DEL(1) = D4 / CKSS                                              WFL 1530
  128   DO 132 J=2,JVX                                                  WFL 1540
          IF(P2(J,1,K)) 130,129,130                                     WFL 1550
  129     DEL(J) = 0.0                                                  WFL 1560
          GOTO 132                                                      WFL 1570
  130     T = D4 * DEL(J-1)                                             WFL 1580
          L = NRGN(J,1)                                                 WFL 1590
          M = NCOMP(L)                                                  WFL 1600
          D4 = DCONR(J+1,1,N)                                           WFL 1610
          BET(J) = (SCAT(J,1)+BET(J-1)*T) / D4                          WFL 1620
          IF(NUAC(19) .GE. 0) GOTO 131                                  WFL 1630
          DEL(J) = D4 / (PTSA(J,1,N)-T+E1(L,K))                         WFL 1640
          GOTO 132                                                      WFL 1650
  131     CKSS = E1(L,K)                                                WFL 1660
          CKSS = CKSS + DCONR(J,1,N) + DCONR(J+1,1,N) + (SIG(K,M,2)+    WFL 1670
     1     SIG(K,M,3)+SIG(K,M,9)) * PVOL(L) - T                         WFL 1680
          DEL(J) = D4 / CKSS                                            WFL 1690
  132   CONTINUE                                                        WFL 1700
        TEMP = BET(JVX) * DEL(JVX)                                      WFL 1710
        P2(JVX,1,K) = TEMP                                              WFL 1720
        DO 133 JJ=2,JVX                                                 WFL 1730
          J = JVXP1 - JJ                                                WFL 1740
          TEMP = DEL(J) * (TEMP+BET(J))                                 WFL 1750
          P2(J,1,K) = TEMP                                              WFL 1760
  133   CONTINUE                                                        WFL 1770
  134   CONTINUE                                                        WFL 1780
        IF(NUAC(3) .GT. 0 .OR. IX(24) .GT. 0) GOTO 139                  WFL 1790
        XLL1 = 0.0                                                      WFL 1800
        XLL3 = 0.0                                                      WFL 1810
        DO 135 M=1,MMAX                                                 WFL 1820
          B2(M,K) = 0.0                                                 WFL 1830
  135   CONTINUE                                                        WFL 1840
        XLL1 = XLL1 + P2(1,1,K) * DCONR(1,1,N)                          WFL 1850
        T1 = DCONR(JMXP1,1,N)                                           WFL 1860
        IF(T1-4096.0E-13) 136,137,136                                   WFL 1870
  136   XLL3 = XLL3 + P2(JMAX,1,K) * T1                                 WFL 1880
  137   CONTINUE                                                        WFL 1890
        XLEK = XLEK + XLL1 + XLL3                                       WFL 1900
        XL(1,K) = XLL1                                                  WFL 1910
        XL(3,K) = XLL3                                                  WFL 1920
        IF(NUAC(17) .LE. 0) GOTO 138                                    WFL 1930
C                                                                       WFL 1940
        CALL FINS(P2,B2,NRGN,DCONB,DCONR,SCAC,XL,IVX,JVX,KVX,LVX,IVXP1, WFL 1950
     1   JVXP1,IVZ,IOVX,IOVZ,PVOL,NCOMP,MVX)                            WFL 1960
C                                                                       WFL 1970
  138   CONTINUE                                                        WFL 1980
  139   CONTINUE                                                        WFL 1990
        DO 141 J=1,JMAX                                                 WFL 2000
          TT1 = P1(J,1)                                                 WFL 2010
          T2 = P2(J,1,K)                                                WFL 2020
          IF(TT1 .EQ. 0.0) GOTO 140                                     WFL 2030
          RATO = T2 / TT1                                               WFL 2040
          RMX = AMAX1(RMX,RATO)                                         WFL 2050
          RMN = AMIN1(RMN,RATO)                                         WFL 2060
  140     CONTINUE                                                      WFL 2070
  141   CONTINUE                                                        WFL 2080
  142 CONTINUE                                                          WFL 2090
      IF(IX(135) .EQ. 1) REWIND IO19                                    WFL 2100
      RETURN                                                            WFL 2110
      END                                                               WFL 2120
      SUBROUTINE CMXS(SIG,SCAC,XI,BAL,SSC,B4,ZONVOL,KVX,MVX)            CMX   10
C                                                                       CMX   20
CCMXS 152.1 ***CITATION*** COLLAPSE AND PUNCH MACRO X-SECT  / CF-OUTC   CMX   30
C                                                                       CMX   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,CMX   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   CMX   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), CMX   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    CMX   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    CMX   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   CMX  100
     6 IXPUT(9999),XPUT(9999)                                           CMX  110
C                                                                       CMX  120
      COMMON /SECT38/ KN(100),MN(100),NKB,NMB,NM1,NM2,NM3               CMX  130
C                                                                       CMX  140
      DIMENSION SIG(KVX,MVX,10),SCAC(KVX,MVX,KVX),XI(KVX),BAL(KVX,MVX,8)CMX  150
     1 ,SSC(KVX,KVX),B4(MVX,KVX),ZONVOL(MVX),SUM(7),SUMT(7)             CMX  160
C                                                                       CMX  170
C                                                                       CMX  180
      IX5 = IX(5)                                                       CMX  190
      IF(IX(24) .GT. 0) IX5 = IX(17)                                    CMX  200
      IF(NGC(5)) 101,100,102                                            CMX  210
  100 CONTINUE                                                          CMX  220
      WRITE (IOUT,1000)                                                 CMX  230
C                                                                       CMX  240
      CALL EXIT                                                         CMX  250
C                                                                       CMX  260
  101 IOXS = 7                                                          CMX  270
      GOTO 103                                                          CMX  280
  102 IO31 = IX(137)                                                    CMX  290
      IOXS = IO31                                                       CMX  300
      REWIND IOXS                                                       CMX  310
  103 CONTINUE                                                          CMX  320
      IF(IX(151) .LE. 0) GOTO 132                                       CMX  330
      NZB = 0                                                           CMX  340
      IF(NM1 .GT. 1) GOTO 106                                           CMX  350
      DO 105 M=1,MMAX                                                   CMX  360
        DO 104 K=1,KMAX                                                 CMX  370
          IF(SIG(K,M,6) .EQ. 0.0) GOTO 106                              CMX  380
  104   CONTINUE                                                        CMX  390
  105 CONTINUE                                                          CMX  400
      GOTO 107                                                          CMX  410
  106 NZB = 1                                                           CMX  420
  107 CONTINUE                                                          CMX  430
      KNEW = NKB                                                        CMX  440
      MNEW = NMB                                                        CMX  450
      IF(NMB .LE. 0) MNEW = MMAX                                        CMX  460
      WRITE (IOXS,1001)                                                 CMX  470
      NERO = 0                                                          CMX  480
      WRITE (IOXS,1002) KNEW,NERO,NERO                                  CMX  490
      DO 127 MM=1,MNEW                                                  CMX  500
        IF(MNEW .NE. MMAX) GOTO 108                                     CMX  510
        M1 = MM                                                         CMX  520
        M2 = MM                                                         CMX  530
        GOTO 111                                                        CMX  540
  108   CONTINUE                                                        CMX  550
        IF(MM .GT. 1) GOTO 109                                          CMX  560
        M1 = 1                                                          CMX  570
        GOTO 110                                                        CMX  580
  109   M1 = MN(MM-1) + 1                                               CMX  590
  110   M2 = MN(MM)                                                     CMX  600
  111   CONTINUE                                                        CMX  610
        DO 125 KK=1,KNEW                                                CMX  620
          IF(KK .GT. 1) GOTO 112                                        CMX  630
          K1 = 1                                                        CMX  640
          GOTO 113                                                      CMX  650
  112     K1 = KN(KK-1) + 1                                             CMX  660
  113     K2 = KN(KK)                                                   CMX  670
          DO 114 I=1,7                                                  CMX  680
            SUMT(I) = 0.0                                               CMX  690
  114     CONTINUE                                                      CMX  700
          DO 118 M=M1,M2                                                CMX  710
            ZV = ZONVOL(M)                                              CMX  720
            DO 115 I=1,7                                                CMX  730
              SUM(I) = 0.0                                              CMX  740
  115       CONTINUE                                                    CMX  750
            DO 116 K=K1,K2                                              CMX  760
              PHIBAR = B4(M,K)                                          CMX  770
              D = SIG(K,M,1)                                            CMX  780
              B = 1.0                                                   CMX  790
              IF(NZB .EQ. 0) B = SIG(K,M,6)                             CMX  800
              SUM(1) = SUM(1) + D * B * PHIBAR                          CMX  810
              SUM(2) = SUM(2) + B * PHIBAR                              CMX  820
              SUM(3) = SUM(3) + SIG(K,M,3) * PHIBAR                     CMX  830
              SUM(4) = SUM(4) + SIG(K,M,4) * PHIBAR                     CMX  840
              IF(IX5 .EQ. -1) SUM(5) = SUM(5) + SIG(K,M,5) * PHIBAR     CMX  850
              SUM(6) = SUM(6) + SIG(K,M,7) * PHIBAR                     CMX  860
              SUM(7) = SUM(7) + PHIBAR                                  CMX  870
  116       CONTINUE                                                    CMX  880
            DO 117 I=1,7                                                CMX  890
              SUMT(I) = SUMT(I) + SUM(I) * ZV                           CMX  900
  117       CONTINUE                                                    CMX  910
  118     CONTINUE                                                      CMX  920
          S7 = SUMT(7)                                                  CMX  930
          IF(S7 .EQ. 0.0) S7 = 1.0                                      CMX  940
          IF(SUMT(2) .EQ. 0.0) SUMT(2) = 1.0                            CMX  950
          BAL(KK,MM,2) = SUMT(1) / SUMT(2)                              CMX  960
          BAL(KK,MM,3) = SUMT(3) / S7                                   CMX  970
          BAL(KK,MM,4) = SUMT(4) / S7                                   CMX  980
          BAL(KK,MM,5) = SUMT(5) / S7                                   CMX  990
          BAL(KK,MM,6) = SUMT(6) / S7                                   CMX 1000
          DO 124 KKP=1,KNEW                                             CMX 1010
            IF(KKP .GT. 1) GOTO 119                                     CMX 1020
            KP1 = 1                                                     CMX 1030
            GOTO 120                                                    CMX 1040
  119       KP1 = KN(KKP-1) + 1                                         CMX 1050
  120       KP2 = KN(KKP)                                               CMX 1060
            S3 = 0.0                                                    CMX 1070
            DO 123 M=M1,M2                                              CMX 1080
              ZV = ZONVOL(M)                                            CMX 1090
              S2 = 0.0                                                  CMX 1100
              DO 122 K=K1,K2                                            CMX 1110
                S1 = 0.0                                                CMX 1120
                DO 121 KP=KP1,KP2                                       CMX 1130
                  S1 = S1 + SCAC(K,M,KP)                                CMX 1140
  121           CONTINUE                                                CMX 1150
                S2 = S2 + S1 * B4(M,K)                                  CMX 1160
  122         CONTINUE                                                  CMX 1170
              S3 = S3 + S2 * ZV                                         CMX 1180
  123       CONTINUE                                                    CMX 1190
            SSC(KK,KKP) = S3 / S7                                       CMX 1200
            IF(KK .EQ. KKP) SSC(KK,KKP) = 0.0                           CMX 1210
  124     CONTINUE                                                      CMX 1220
  125   CONTINUE                                                        CMX 1230
        DO 126 KK=1,KNEW                                                CMX 1240
          WRITE (IOXS,1003) MM,KK,(BAL(KK,MM,I),I=2,6)                  CMX 1250
          WRITE (IOXS,1004) (SSC(KK,KKP),KKP=1,KNEW)                    CMX 1260
  126   CONTINUE                                                        CMX 1270
  127 CONTINUE                                                          CMX 1280
      MM = 0                                                            CMX 1290
      KK = 0                                                            CMX 1300
      WRITE (IOXS,1003) MM,KK                                           CMX 1310
      DO 131 KK=1,KNEW                                                  CMX 1320
        IF(KK .GT. 1) GOTO 128                                          CMX 1330
        K1 = 1                                                          CMX 1340
        GOTO 129                                                        CMX 1350
  128   K1 = KN(KK-1) + 1                                               CMX 1360
  129   K2 = KN(KK)                                                     CMX 1370
        S1 = 0.0                                                        CMX 1380
        DO 130 K=K1,K2                                                  CMX 1390
          S1 = S1 + XI(K)                                               CMX 1400
  130   CONTINUE                                                        CMX 1410
        BAL(KK,1,1) = S1                                                CMX 1420
  131 CONTINUE                                                          CMX 1430
      WRITE (IOXS,1004) (BAL(KK,1,1),KK=1,KNEW)                         CMX 1440
      WRITE (IOUT,1005) IOXS                                            CMX 1450
      GOTO 135                                                          CMX 1460
  132 CONTINUE                                                          CMX 1470
      WRITE (IOXS,1001)                                                 CMX 1480
      WRITE (IOXS,1002) KMAX,IX(28),IX(29)                              CMX 1490
      DO 134 M=1,MMAX                                                   CMX 1500
        DO 133 K=1,KMAX                                                 CMX 1510
          S5 = SIG(K,M,5)                                               CMX 1520
          IF(IX5 .NE. -1) S5 = 0.0                                      CMX 1530
          WRITE (IOXS,1003) M,K,SIG(K,M,1),SIG(K,M,3),SIG(K,M,4),S5,    CMX 1540
     1     SIG(K,M,7)                                                   CMX 1550
          WRITE (IOXS,1004) (SCAC(K,M,KK),KK=1,KMAX)                    CMX 1560
  133   CONTINUE                                                        CMX 1570
  134 CONTINUE                                                          CMX 1580
      M = 0                                                             CMX 1590
      WRITE (IOXS,1003) M                                               CMX 1600
      WRITE (IOXS,1004) (XI(K),K=1,KMAX)                                CMX 1610
      WRITE (IOUT,1006) IOXS                                            CMX 1620
  135 CONTINUE                                                          CMX 1630
      IF(NGC(5) .GT. 0) REWIND IOXS                                     CMX 1640
      RETURN                                                            CMX 1650
C                                                                       CMX 1660
 1000 FORMAT (1H0,'ERROR STOP 7777(CMXS)')                              CMX 1670
 1001 FORMAT (3H008)                                                    CMX 1680
 1002 FORMAT (3I3)                                                      CMX 1690
 1003 FORMAT (2I6,5(1PE12.5))                                           CMX 1700
 1004 FORMAT (6(1PE12.5))                                               CMX 1710
 1005 FORMAT (1H0,'MACROSCOPIC CROSS SECTIONS HAVE BEEN COLLAPSED AND WRCMX 1720
     1ITTEN ON LOGICAL',I4)                                             CMX 1730
 1006 FORMAT (1H0,'MACROSCOPIC CROSS SECTIONS HAVE BEEN WRITTEN ON LOGICCMX 1740
     1AL',I4)                                                           CMX 1750
      END                                                               CMX 1760