      SUBROUTINE IPTM(A,MEMORY,NTITE,IMEM,NM1VX,NSM1VX,MM1AC5)          IPT   10
C                                                                       IPT   20
CIPTM --006 ***CITATION*** INPUT MANAGER ROUTINE                        IPT   30
C                                                                       IPT   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,IPT   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   IPT   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), IPT   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    IPT   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    IPT   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   IPT  100
     6 IXPUT(9999),XPUT(9999)                                           IPT  110
C                                                                       IPT  120
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  IPT  130
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), IPT  140
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)IPT  150
     3 ,PDK(211)                                                        IPT  160
C                                                                       IPT  170
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   IPT  180
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKIPT  190
     2 RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,     IPT  200
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  IPT  210
     4 ITMAX,ITIME,BET(211),DEL(211)                                    IPT  220
C                                                                       IPT  230
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           IPT  240
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    IPT  250
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),IPT  260
     3 NCLASS(9999),NDP(9999)                                           IPT  270
C                                                                       IPT  280
      COMMON /AVDLM/ IVDLM(1),IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1, IPT  290
     1 KBVXP1,NSETVX,NVO,IVO,IVZ,KVZ,NCRP,NSPA,N3DDIM,NBLOCK,JIVX,JIP1VXIPT  300
     2 ,JP1IXZ,IOVX,IOVZ                                                IPT  310
C                                                                       IPT  320
      COMMON /AKADD/ KAY(1),K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13, IPT  330
     1 K131,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,    IPT  340
     2 K28,K29,K30,K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,K41,K42,K43, IPT  350
     3 K44,K45,K46,K47,K48,K49,K50,K51,K52,K53,K54,K55,K56,K57,K58,     IPT  360
     4 K59,K60,K61,K62,K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,     IPT  370
     5 K74,K75,K76,K77,K78,K79,K80,K81,K82,K83,K84,K85,K86,K87,K88,     IPT  380
     6 K89,K90,K91,K92,K93,K94,K95,K96,K97,K98,K99,K100,NDATA,KNRGN,    IPT  390
     7 KNCOMP,KPVOL,KRVOL,MEMVRY,MEMX                                   IPT  400
C                                                                       IPT  410
      COMMON /ASRCH/ BSRCH(30),XK1,XK2,XK3,XN1,XN2,XN3,DELK1,DELK2,DELK3IPT  420
     1 ,BATTY,DRV,TBF,GWC,EK2,RCCM,DNDK(5),NSC(5),NSCN,NXZ,NXN,NXM,NXS, IPT  430
     2 INIL,INIU,INID                                                   IPT  440
C                                                                       IPT  450
      COMMON /CMARY/ MEMARY,IMN,MNI,IJLMN,NMLJI,IY(50),AX(50),TITL(36)  IPT  460
C                                                                       IPT  470
CFZJ056                                                       06.11.07  IPT  480
      COMMON /COOPD/ FLOTR(9999),INTGR(200)                             IPT  490
C                                                                       IPT  500
      COMMON /MU/ MU4                                                   IPT  510
C                                                                       IPT  520
CFZJ055                                                       25.09.07  IPT  530
C                                                                       IPT  540
      REAL*8 FLOTR                                                      IPT  550
C                                                                       IPT  560
      DIMENSION A(MEMORY),NTXR1(200)                                    IPT  570
C                                                                       IPT  580
C                                                                       IPT  590
      NUO = IX(190)                                                     IPT  600
      NHEX = IX(191)                                                    IPT  610
      NRVX = IX(192)                                                    IPT  620
      IJKBX = IX(193)                                                   IPT  630
      DO 100 I=1,200                                                    IPT  640
        NER(I) = 0                                                      IPT  650
        INNO(I) = 0                                                     IPT  660
        NXTR1(I) = 0                                                    IPT  670
  100 CONTINUE                                                          IPT  680
      IF(IX(27) .EQ. 0) GOTO 101                                        IPT  690
C                                                                       IPT  700
      CALL SHOX(A(K30),A(K31),A(K32),A(K33),NVX,NSETVX)                 IPT  710
C                                                                       IPT  720
      IF(NCRP .EQ. 1) NSPA = 0                                          IPT  730
  101 CONTINUE                                                          IPT  740
      NTYP = 0                                                          IPT  750
      INCT = 0                                                          IPT  760
  102 CONTINUE                                                          IPT  770
      NTYP0 = NTYP                                                      IPT  780
      I9193 = 0                                                         IPT  790
      READ (IOIN,1003) NTYP                                             IPT  800
  103 CONTINUE                                                          IPT  810
      INTGR(16) = 0                                                     IPT  820
      INCT = INCT + 1                                                   IPT  830
      IF(NTYP-999) 104,192,105                                          IPT  840
  104 IF(NTYP) 105,107,107                                              IPT  850
  105 IERR = 1                                                          IPT  860
      NER(IERR) = IERR                                                  IPT  870
      INCT = INCT - 1                                                   IPT  880
      WRITE (IOUT,1004) (NTXR1(I),I=1,INCT),NTYP                        IPT  890
      GOTO 206                                                          IPT  900
  107 CONTINUE                                                          IPT  910
      IF(NTYP .LT. NTYP0) GOTO 105                                      IPT  920
      IF(NTYP .NE. 0) GOTO 108                                          IPT  930
      REWIND IO1                                                        IPT  940
      WRITE (IO1) (A(I),I=1,MEMORY)                                     IPT  950
      END FILE IO1                                                      IPT  960
      REWIND IO1                                                        IPT  970
      IO30 = IX(97)                                                     IPT  980
      READ (IO1) (A(I),I=1,MEMORY)                                      IPT  990
      REWIND IO1                                                        IPT 1000
      READ (IOIN,1003) NTYP                                             IPT 1010
      IX(167) = 0                                                       IPT 1020
      IF(NTYP .EQ. 999) IX(167) = 1                                     IPT 1030
      IF(NTYP .EQ. 999) GOTO 209                                        IPT 1040
      NTYP0 = NTYP                                                      IPT 1050
      GOTO 103                                                          IPT 1060
  108 CONTINUE                                                          IPT 1070
      INNO(NTYP) = NTYP                                                 IPT 1080
      NTXR1(INCT) = NTYP                                                IPT 1090
      IF(NTYP .EQ. 91 .OR. NTYP .EQ. 93) GOTO 187                       IPT 1100
      GOTO(111,116,117,118,122,127,132,133,137,138,139,140,147,148,149, IPT 1110
     1 150,151,152,153,154,155,156,157,158,159,160,162,163,164,165,167, IPT 1120
     2 168,169,170,171,102,173,174,175,176,177,178,179,180,181,182,183, IPT 1130
     3 184,185,186),NTYP                                                IPT 1140
      WRITE (IOUT,1005) NTYP                                            IPT 1150
      GOTO 208                                                          IPT 1160
C                                                                       IPT 1170
  111 CALL CNTR                                                         IPT 1180
C                                                                       IPT 1190
      IF(NGC(2) .EQ. 0) GOTO 114                                        IPT 1200
C                                                                       IPT 1210
      CALL RSTR(NTITE,A,MEMORY)                                         IPT 1220
C                                                                       IPT 1230
      IX(27) = 1                                                        IPT 1240
      IF(NCRP .EQ. 1) NSPA = 0                                          IPT 1250
      REWIND IO2                                                        IPT 1260
      N1 = KNRGN + IVX * JVX * KBVX - 1                                 IPT 1270
      WRITE (IO2) (A(N),N=KNRGN,N1)                                     IPT 1280
      END FILE IO2                                                      IPT 1290
      REWIND IO2                                                        IPT 1300
      IRV = 1                                                           IPT 1310
C                                                                       IPT 1320
      CALL BNSB(A(K50),A(K51),KVX,IRV)                                  IPT 1330
C                                                                       IPT 1340
      IMEM = 1                                                          IPT 1350
      IF(NGC(19) .GT. 0) GOTO 102                                       IPT 1360
      IF(NMAX .EQ. NVX .AND. KMAX .EQ. KVX) GOTO 112                    IPT 1370
      WRITE (IOUT,1000) NMAX,NVX,KMAX,KVX                               IPT 1380
      GOTO 208                                                          IPT 1390
  112 CONTINUE                                                          IPT 1400
      ISRTT = 1                                                         IPT 1410
      NUO = 0                                                           IPT 1420
      DO 113 M=1,MMAX                                                   IPT 1430
        N1 = NZON(M)                                                    IPT 1440
        N2 = NXSET(M)                                                   IPT 1450
        N3 = NXODR(N2)                                                  IPT 1460
        N4 = NSIG2(N3)                                                  IPT 1470
        NUO = NUO + N1 * N4                                             IPT 1480
  113 CONTINUE                                                          IPT 1490
      IF(NTITE .LE. 0) GOTO 102                                         IPT 1500
      INTGR(16) = 1                                                     IPT 1510
      IO13 = IX(80)                                                     IPT 1520
      INTGR(13) = IO13                                                  IPT 1530
      INTGR(14) = NGC(2)                                                IPT 1540
      INTGR(15) = NGC(3)                                                IPT 1550
      IO12 = IX(79)                                                     IPT 1560
      INTGR(17) = IO12                                                  IPT 1570
      IO24 = IX(91)                                                     IPT 1580
      INTGR(18) = IO24                                                  IPT 1590
      I9193 = 1                                                         IPT 1600
      GOTO 187                                                          IPT 1610
  114 CONTINUE                                                          IPT 1620
      IF(IX(27) .GT. 0) GOTO 102                                        IPT 1630
      DO 115 N=1,MEMORY                                                 IPT 1640
        A(N) = 0.0                                                      IPT 1650
  115 CONTINUE                                                          IPT 1660
      GOTO 102                                                          IPT 1670
  116 CONTINUE                                                          IPT 1680
      IX(76) = 0                                                        IPT 1690
      IF(NGC(2) .GT. 0) IX(76) = 1                                      IPT 1700
      GOTO 102                                                          IPT 1710
C                                                                       IPT 1720
  117 CALL GEOM(A(1))                                                   IPT 1730
C                                                                       IPT 1740
      NHEX = 0                                                          IPT 1750
      IF(NUAC(5) .EQ. 9 .OR. NUAC(5) .EQ. 13) NHEX = 1                  IPT 1760
      IF(IX(27) .EQ. 0) GOTO 102                                        IPT 1770
      IVZ = IVX + NHEX                                                  IPT 1780
      KVZ = KVX * (NHEX+1)                                              IPT 1790
      GOTO 102                                                          IPT 1800
C                                                                       IPT 1810
  118 CALL LVMX                                                         IPT 1820
C                                                                       IPT 1830
      IF(LVX .NE. LMAX) IX(7) = 1                                       IPT 1840
      LVX = LMAX                                                        IPT 1850
      KRVOL = MEMORY - LVX + 1                                          IPT 1860
      KPVOL = KRVOL - LVX                                               IPT 1870
      KNCOMP = KPVOL - LVX                                              IPT 1880
      KNO = IABS(KNCOMP)                                                IPT 1890
      IF(KNCOMP .LE. 0) GOTO 207                                        IPT 1900
C                                                                       IPT 1910
      CALL MESH(A(KRVOL),A(KPVOL),LVX)                                  IPT 1920
C                                                                       IPT 1930
      IVX = IMAX                                                        IPT 1940
      IVXP1 = IMXP1                                                     IPT 1950
      JVX = JMAX                                                        IPT 1960
      JVXP1 = JMXP1                                                     IPT 1970
      KBVX = KBMAX                                                      IPT 1980
      KBVXP1 = KBMXP1                                                   IPT 1990
      KNRGN = 1                                                         IPT 2000
      IF(IX(27) .GT. 0) KNRGN = K19                                     IPT 2010
      IJKBX = IVX * JVX * KBVX                                          IPT 2020
      ISK = 0                                                           IPT 2030
      IF(NUAC(5)-10) 119,119,120                                        IPT 2040
C                                                                       IPT 2050
  119 CALL COMP(ISK,A(KNRGN),A(KNCOMP),IVX,JVX,LVX)                     IPT 2060
C                                                                       IPT 2070
      GOTO 121                                                          IPT 2080
C                                                                       IPT 2090
  120 CALL KOMP(ISK,A(KNRGN),A(KNCOMP),IVX,JVX,KBVX,LVX)                IPT 2100
C                                                                       IPT 2110
  121 REWIND IO2                                                        IPT 2120
      N1 = KNRGN + IJKBX - 1                                            IPT 2130
      WRITE (IO2) (A(N),N=KNRGN,N1)                                     IPT 2140
      END FILE IO2                                                      IPT 2150
      REWIND IO2                                                        IPT 2160
      GOTO 102                                                          IPT 2170
  122 ISK = 1                                                           IPT 2180
      IF(NUAC(5)-10) 123,123,124                                        IPT 2190
C                                                                       IPT 2200
  123 CALL COMP(ISK,A(KNRGN),A(KNCOMP),IVX,JVX,LVX)                     IPT 2210
C                                                                       IPT 2220
      GOTO 125                                                          IPT 2230
C                                                                       IPT 2240
  124 CALL KOMP(ISK,A(KNRGN),A(KNCOMP),IVX,JVX,KBVX,LVX)                IPT 2250
C                                                                       IPT 2260
  125 CONTINUE                                                          IPT 2270
      IF(NGC(2) .EQ. 0) GOTO 126                                        IPT 2280
      IF(MMAX .EQ. MVX) GOTO 126                                        IPT 2290
      WRITE (IOUT,1000) MMAX,MVX                                        IPT 2300
      GOTO 208                                                          IPT 2310
  126 MVX = MMAX                                                        IPT 2320
      GOTO 102                                                          IPT 2330
  127 CONTINUE                                                          IPT 2340
      IF(INNO(5) .NE. 5) GOTO 105                                       IPT 2350
      N1 = NDATA + 1 + IJKBX                                            IPT 2360
      NT1 = MEMORY - (N1+LVX) + 1                                       IPT 2370
      NT2 = (NT1-2*LVX) / 3                                             IPT 2380
      N2 = N1 + LVX + NT2                                               IPT 2390
      N3 = N2 + LVX + NT2                                               IPT 2400
      NCHECK = N3 + LVX                                                 IPT 2410
      IF(NCHECK .LE. MEMORY) GOTO 128                                   IPT 2420
      WRITE (IOUT,1006) NDATA,MEMORY,LVX,N1,NT1,NT2,N2,N3               IPT 2430
C                                                                       IPT 2440
      CALL EXIT                                                         IPT 2450
C                                                                       IPT 2460
  128 CONTINUE                                                          IPT 2470
      DO 129 N=1,LVX                                                    IPT 2480
        NN = N - 1                                                      IPT 2490
        NN1 = KNCOMP + NN                                               IPT 2500
        NN2 = N1 + NN                                                   IPT 2510
        NN3 = KPVOL + NN                                                IPT 2520
        NN4 = N2 + NN                                                   IPT 2530
        NN5 = KRVOL + NN                                                IPT 2540
        NN6 = N3 + NN                                                   IPT 2550
        A(NN2) = A(NN1)                                                 IPT 2560
        A(NN4) = A(NN3)                                                 IPT 2570
        A(NN6) = A(NN5)                                                 IPT 2580
  129 CONTINUE                                                          IPT 2590
      NNN1 = N1 + LVX - 1                                               IPT 2600
      LSZ = NT2 + LVX                                                   IPT 2610
C                                                                       IPT 2620
      CALL OVER(A(KNRGN),A(N1),A(N3),A(N2),IVX,JVX,KBVX,LSZ,NNN1,N2)    IPT 2630
C                                                                       IPT 2640
      REWIND IO2                                                        IPT 2650
      NLIM = KNRGN + IVX * JVX * KBVX - 1                               IPT 2660
      WRITE (IO2) (A(N),N=KNRGN,NLIM)                                   IPT 2670
      END FILE IO2                                                      IPT 2680
      REWIND IO2                                                        IPT 2690
      LVX = LMAX                                                        IPT 2700
      MVX = MMAX                                                        IPT 2710
      KRVOL = MEMORY - LVX + 1                                          IPT 2720
      KPVOL = KRVOL - LVX                                               IPT 2730
      KNCOMP = KPVOL - LVX                                              IPT 2740
      DO 130 N=1,LVX                                                    IPT 2750
        NN = N - 1                                                      IPT 2760
        NN1 = N1 + NN                                                   IPT 2770
        NN2 = KNCOMP + NN                                               IPT 2780
        NN3 = N2 + NN                                                   IPT 2790
        NN4 = KPVOL + NN                                                IPT 2800
        NN5 = N3 + NN                                                   IPT 2810
        NN6 = KRVOL + NN                                                IPT 2820
        A(NN2) = A(NN1)                                                 IPT 2830
        A(NN4) = A(NN3)                                                 IPT 2840
        A(NN6) = A(NN5)                                                 IPT 2850
        A(NN1) = 0.0                                                    IPT 2860
        A(NN3) = 0.0                                                    IPT 2870
        A(NN5) = 0.0                                                    IPT 2880
  130 CONTINUE                                                          IPT 2890
      IF(NUAC(5) .GT. 10) GOTO 131                                      IPT 2900
C                                                                       IPT 2910
      CALL CMOT(A(KNRGN),A(KNCOMP),IVX,JVX,LVX)                         IPT 2920
C                                                                       IPT 2930
      GOTO 102                                                          IPT 2940
C                                                                       IPT 2950
  131 CALL KMOT(A(KNRGN),A(KNCOMP),IVX,JVX,KBVX,LVX)                    IPT 2960
C                                                                       IPT 2970
      GOTO 102                                                          IPT 2980
  132 CONTINUE                                                          IPT 2990
      GOTO 102                                                          IPT 3000
  133 READ (IOIN,1007) NSIG3(1),NSIG4(1),NSIG5(1)                       IPT 3010
      IF(NSIG3(1) .GT. 0) GOTO 134                                      IPT 3020
      IX(136) = 1                                                       IPT 3030
      IOMC = IX(137)                                                    IPT 3040
      REWIND IOMC                                                       IPT 3050
      READ (IOMC,1007) NATE                                             IPT 3060
      IF(NATE .NE. 8) NER(35) = 35                                      IPT 3070
      READ (IOMC,1007) NSIG3(1),NSIG4(1),NSIG5(1)                       IPT 3080
      GOTO 135                                                          IPT 3090
  134 CONTINUE                                                          IPT 3100
      IX(136) = 0                                                       IPT 3110
      IOMC = IOIN                                                       IPT 3120
  135 CONTINUE                                                          IPT 3130
      NLIM = KNRGN + IVX * JVX * KBVX - 1                               IPT 3140
      DO 136 N=KNRGN,NLIM                                               IPT 3150
        A(N) = 0.0                                                      IPT 3160
  136 CONTINUE                                                          IPT 3170
      NSIG1(1) = 0                                                      IPT 3180
      NSIG2(1) = 0                                                      IPT 3190
      KMAX = NSIG3(1)                                                   IPT 3200
      NMAX = 1                                                          IPT 3210
      IX(28) = NSIG4(1)                                                 IPT 3220
      IX(29) = NSIG5(1)                                                 IPT 3230
      NVX = NMAX                                                        IPT 3240
      KVX = KMAX                                                        IPT 3250
      NSETMX = 1                                                        IPT 3260
      NSETVX = 1                                                        IPT 3270
      NRVX = 1                                                          IPT 3280
C                                                                       IPT 3290
      CALL KSIG(KVX,MVX,NVX,NSETVX)                                     IPT 3300
C                                                                       IPT 3310
      KNO = K18 - KNCOMP                                                IPT 3320
      IF(KNO .GE. 0) GOTO 207                                           IPT 3330
C                                                                       IPT 3340
      CALL MACR(A(K14),A(K15),A(K16),MVX,KVX,IOMC)                      IPT 3350
C                                                                       IPT 3360
      IF(IX(136) .GT. 0) REWIND IOMC                                    IPT 3370
      NUO = 1                                                           IPT 3380
      NVO = 1                                                           IPT 3390
      IVO = 1                                                           IPT 3400
      IVZ = IVX + NHEX                                                  IPT 3410
      KVZ = KVX * (NHEX+1)                                              IPT 3420
C                                                                       IPT 3430
      CALL KRST(IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1,KBVXP1,NSETVX, IPT 3440
     1 NVO,IVO,IVZ,KVZ,N3DDIM)                                          IPT 3450
C                                                                       IPT 3460
      IMEM = 1                                                          IPT 3470
      KNO = K62 - KNCOMP                                                IPT 3480
      IF(KNO .GE. 0) GOTO 207                                           IPT 3490
      IF(NGC(19) .EQ. 0) NGC(19) = 1                                    IPT 3500
      GOTO 102                                                          IPT 3510
  137 CONTINUE                                                          IPT 3520
      GOTO 102                                                          IPT 3530
  138 CONTINUE                                                          IPT 3540
      GOTO 102                                                          IPT 3550
  139 CONTINUE                                                          IPT 3560
      GOTO 102                                                          IPT 3570
  140 CONTINUE                                                          IPT 3580
      IF(NGC(19) .GT. 0) GOTO 142                                       IPT 3590
      NLIM = KNRGN + IVX * JVX * KBVX - 1                               IPT 3600
      DO 141 N=KNRGN,NLIM                                               IPT 3610
        A(N) = 0.0                                                      IPT 3620
  141 CONTINUE                                                          IPT 3630
  142 CONTINUE                                                          IPT 3640
      NSETVX = NSETMX                                                   IPT 3650
      IF(NGC(19) .GT. 0) NSETMX = 1                                     IPT 3660
      IF(NGC(19) .GT. 0) NSETVX = 1                                     IPT 3670
      IF(NGC(19)) 143,143,102                                           IPT 3680
  143 CONTINUE                                                          IPT 3690
      NVX = NMAX                                                        IPT 3700
      KVX = KMAX                                                        IPT 3710
C                                                                       IPT 3720
      CALL KSIG(KVX,MVX,NVX,NSETVX)                                     IPT 3730
C                                                                       IPT 3740
      KNO = K18 - KNCOMP                                                IPT 3750
      IF(KNO .GE. 0) GOTO 207                                           IPT 3760
      NN = K14 - 1                                                      IPT 3770
      DO 144 N=1,NN                                                     IPT 3780
        A(N) = 0.0                                                      IPT 3790
  144 CONTINUE                                                          IPT 3800
      NN = K34 - 1                                                      IPT 3810
      DO 145 N=K30,NN                                                   IPT 3820
        A(N) = 0.0                                                      IPT 3830
  145 CONTINUE                                                          IPT 3840
      ISRTT = 0                                                         IPT 3850
      NUO = 0                                                           IPT 3860
      IVO = 0                                                           IPT 3870
      DO 146 M=1,MMAX                                                   IPT 3880
        N1 = NZON(M)                                                    IPT 3890
        N2 = NXSET(M)                                                   IPT 3900
        N3 = NXODR(N2)                                                  IPT 3910
        N4 = NSIG2(N3)                                                  IPT 3920
        N1N4 = N1 * N4                                                  IPT 3930
        IVO = MAX0(IVO,N1N4)                                            IPT 3940
        NUO = NUO + N1N4                                                IPT 3950
  146 CONTINUE                                                          IPT 3960
      NVO = IVO                                                         IPT 3970
      IVZ = IVX + NHEX                                                  IPT 3980
      KVZ = KVX * (NHEX+1)                                              IPT 3990
C                                                                       IPT 4000
      CALL KRST(IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1,KBVXP1,NSETVX, IPT 4010
     1 NVO,IVO,IVZ,KVZ,N3DDIM)                                          IPT 4020
C                                                                       IPT 4030
      IMEM = 1                                                          IPT 4040
      KNO = K62 - KNCOMP                                                IPT 4050
      IF(KNO .GE. 0) GOTO 207                                           IPT 4060
      N1 = MEMORY - K18                                                 IPT 4070
      KNO = NUO - N1                                                    IPT 4080
      INSEC = -36                                                       IPT 4090
      REWIND IO3                                                        IPT 4100
      IF(KNO .GT. 0) GOTO 207                                           IPT 4110
      GOTO 102                                                          IPT 4120
  147 CONTINUE                                                          IPT 4130
      GOTO 102                                                          IPT 4140
  148 CONTINUE                                                          IPT 4150
      GOTO 102                                                          IPT 4160
  149 CONTINUE                                                          IPT 4170
      GOTO 102                                                          IPT 4180
  150 CONTINUE                                                          IPT 4190
      GOTO 102                                                          IPT 4200
  151 CONTINUE                                                          IPT 4210
      GOTO 102                                                          IPT 4220
  152 CONTINUE                                                          IPT 4230
      GOTO 102                                                          IPT 4240
  153 CONTINUE                                                          IPT 4250
      GOTO 102                                                          IPT 4260
  154 CONTINUE                                                          IPT 4270
      GOTO 102                                                          IPT 4280
  155 CONTINUE                                                          IPT 4290
      GOTO 102                                                          IPT 4300
  156 CONTINUE                                                          IPT 4310
      GOTO 102                                                          IPT 4320
  157 CONTINUE                                                          IPT 4330
      GOTO 102                                                          IPT 4340
  158 CONTINUE                                                          IPT 4350
      GOTO 102                                                          IPT 4360
  159 CONTINUE                                                          IPT 4370
      GOTO 102                                                          IPT 4380
  160 CONTINUE                                                          IPT 4390
      N1 = JVX * IVX * KBVX                                             IPT 4400
      N2 = K62 + N1                                                     IPT 4410
      N3 = N2 + N1                                                      IPT 4420
      KNO = N3 - KNCOMP                                                 IPT 4430
      IF(KNO .GE. 0) GOTO 207                                           IPT 4440
      N4 = N3 - 1                                                       IPT 4450
      REWIND IO2                                                        IPT 4460
      READ (IO2) (A(N),N=N2,N4)                                         IPT 4470
      REWIND IO2                                                        IPT 4480
C                                                                       IPT 4490
      CALL FXSO(A(K62),A(K45),A(KPVOL),A(N2),A(KNCOMP),JVX,IVX,KBVX,KVX,IPT 4500
     1 LVX)                                                             IPT 4510
C                                                                       IPT 4520
      DO 161 N=N2,N4                                                    IPT 4530
        A(N) = 0.0                                                      IPT 4540
  161 CONTINUE                                                          IPT 4550
      NSPA = 1                                                          IPT 4560
      IX(131) = 1                                                       IPT 4570
      GOTO 102                                                          IPT 4580
  162 CONTINUE                                                          IPT 4590
      GOTO 102                                                          IPT 4600
  163 INSEC = 28                                                        IPT 4610
      REWIND IO3                                                        IPT 4620
      GOTO 102                                                          IPT 4630
  164 CONTINUE                                                          IPT 4640
      GOTO 102                                                          IPT 4650
  165 CONTINUE                                                          IPT 4660
      INSEC = 30                                                        IPT 4670
      REWIND IO3                                                        IPT 4680
      N1 = JVX * IVX * KBVX                                             IPT 4690
      N2 = K62 + N1 * NSPA                                              IPT 4700
      N3 = N2 + N1                                                      IPT 4710
      KNO = N3 - KNCOMP                                                 IPT 4720
      IF(KNO .GE. 0) GOTO 207                                           IPT 4730
      N4 = N3 - 1                                                       IPT 4740
      REWIND IO2                                                        IPT 4750
      READ (IO2) (A(N),N=N2,N4)                                         IPT 4760
      REWIND IO2                                                        IPT 4770
C                                                                       IPT 4780
      CALL RODI(A(N2),A(KNCOMP),JVX,IVX,KBVX,LVX,A(K10),NSETVX)         IPT 4790
C                                                                       IPT 4800
      DO 166 N=N2,N4                                                    IPT 4810
        A(N) = 0.0                                                      IPT 4820
  166 CONTINUE                                                          IPT 4830
      IX(142) = 1                                                       IPT 4840
      GOTO 102                                                          IPT 4850
  167 CONTINUE                                                          IPT 4860
      GOTO 102                                                          IPT 4870
  168 CONTINUE                                                          IPT 4880
      GOTO 102                                                          IPT 4890
  169 CONTINUE                                                          IPT 4900
      GOTO 102                                                          IPT 4910
  170 CONTINUE                                                          IPT 4920
      GOTO 102                                                          IPT 4930
  171 CONTINUE                                                          IPT 4940
      GOTO 102                                                          IPT 4950
  173 CONTINUE                                                          IPT 4960
      GOTO 102                                                          IPT 4970
  174 CONTINUE                                                          IPT 4980
      GOTO 102                                                          IPT 4990
  175 CONTINUE                                                          IPT 5000
      GOTO 102                                                          IPT 5010
  176 INSEC = 40                                                        IPT 5020
      REWIND IO3                                                        IPT 5030
      GOTO 102                                                          IPT 5040
  177 CONTINUE                                                          IPT 5050
      GOTO 102                                                          IPT 5060
  178 CONTINUE                                                          IPT 5070
      GOTO 102                                                          IPT 5080
  179 CONTINUE                                                          IPT 5090
      GOTO 102                                                          IPT 5100
  180 CONTINUE                                                          IPT 5110
      GOTO 102                                                          IPT 5120
  181 CONTINUE                                                          IPT 5130
      GOTO 102                                                          IPT 5140
  182 CONTINUE                                                          IPT 5150
      GOTO 102                                                          IPT 5160
  183 CONTINUE                                                          IPT 5170
      GOTO 102                                                          IPT 5180
  184 CONTINUE                                                          IPT 5190
      GOTO 102                                                          IPT 5200
  185 CONTINUE                                                          IPT 5210
      GOTO 102                                                          IPT 5220
  186 CONTINUE                                                          IPT 5230
      GOTO 102                                                          IPT 5240
  187 CONTINUE                                                          IPT 5250
      REWIND IO1                                                        IPT 5260
      WRITE (IO1) (A(I),I=1,MEMORY)                                     IPT 5270
      END FILE IO1                                                      IPT 5280
      REWIND IO1                                                        IPT 5290
      INTGR(11) = 1                                                     IPT 5300
      INTGR(200) = NTYP                                                 IPT 5310
      IF(INTGR(16) .EQ. 0) GOTO 189                                     IPT 5320
      INTGR(16) = 0                                                     IPT 5330
      NGC(2) = INTGR(14)                                                IPT 5340
      IX(19) = NGC(2)                                                   IPT 5350
      IOR = IX(80)                                                      IPT 5360
      IO12 = IX(79)                                                     IPT 5370
      REWIND IO12                                                       IPT 5380
      IF(NGC(2) .GT. 0) GOTO 189                                        IPT 5390
      READ (IOR) II                                                     IPT 5400
      IX19 = II                                                         IPT 5410
      IX22 = IX(22)                                                     IPT 5420
      IX2 = IX(2)                                                       IPT 5430
      IX39 = IX(39)                                                     IPT 5440
      IX198 = IX(198)                                                   IPT 5450
      READ (IOR) N12,(IX(I),I=1,200),(SPARE(I),I=1,200),(A(I),I=K17,N12)IPT 5460
      IX(22) = IX22                                                     IPT 5470
      IX(19) = IX19                                                     IPT 5480
      IX(2) = IX2                                                       IPT 5490
      IX(39) = IX39                                                     IPT 5500
      IX(198) = IX198                                                   IPT 5510
      BACKSPACE IOR                                                     IPT 5520
      BACKSPACE IOR                                                     IPT 5530
      NZN2 = 0                                                          IPT 5540
      WRITE (IO12) MMAX,MMAX,MMAX,MMAX,MMAX                             IPT 5550
      INCR = K18 - 1                                                    IPT 5560
      DO 188 M=1,MMAX                                                   IPT 5570
        NACT = NXSET(M)                                                 IPT 5580
        MS1 = NXODR(NACT)                                               IPT 5590
        NZN1 = NZN2 + 1                                                 IPT 5600
        NZN2 = NZN2 + NJM(MS1) * NZON(M)                                IPT 5610
        J1 = NZN1 + INCR                                                IPT 5620
        J2 = NZN2 + INCR                                                IPT 5630
        WRITE (IO12) NZN1,NZN2                                          IPT 5640
        WRITE (IO12) (A(I),I=J1,J2)                                     IPT 5650
  188 CONTINUE                                                          IPT 5660
      END FILE IO12                                                     IPT 5670
      REWIND IO12                                                       IPT 5680
      IX(27) = 1                                                        IPT 5690
  189 CONTINUE                                                          IPT 5700
      READ (IO1) (A(I),I=1,MEMORY)                                      IPT 5710
      REWIND IO1                                                        IPT 5720
      IF(I9193 .EQ. 0) GOTO 102                                         IPT 5730
      IF(NGC(2) .GT. 0) GOTO 190                                        IPT 5740
      GOTO 102                                                          IPT 5750
  190 CONTINUE                                                          IPT 5760
      READ (IO12)                                                       IPT 5770
      DO 191 M=1,MVX                                                    IPT 5780
        READ (IO12)                                                     IPT 5790
        READ (IO12)                                                     IPT 5800
  191 CONTINUE                                                          IPT 5810
      IEND = K17 + NMAX * MMAX - 1                                      IPT 5820
      READ (IO12) (A(I),I=K17,IEND)                                     IPT 5830
      REWIND IO12                                                       IPT 5840
      IX19 = IX(19)                                                     IPT 5850
      IX22 = IX(22)                                                     IPT 5860
      IX2 = IX(2)                                                       IPT 5870
      IX39 = IX(39)                                                     IPT 5880
      IX198 = IX(198)                                                   IPT 5890
      READ (IO13) (IX(I),I=1,200),(SPARE(I),I=1,200)                    IPT 5900
      IX(19) = IX19                                                     IPT 5910
      IX(22) = IX22                                                     IPT 5920
      IX(2) = IX2                                                       IPT 5930
      IX(39) = IX39                                                     IPT 5940
      IX(198) = IX198                                                   IPT 5950
      IX(27) = 1                                                        IPT 5960
      GOTO 102                                                          IPT 5970
  192 CONTINUE                                                          IPT 5980
      IF(NGC(19) .GT. 0) GOTO 194                                       IPT 5990
      IF(IX(27) .EQ. 0) GOTO 193                                        IPT 6000
      IF(INNO(34) .EQ. 34 .OR. INNO(36) .EQ. 36) GOTO 193               IPT 6010
      GOTO 194                                                          IPT 6020
  193 CONTINUE                                                          IPT 6030
C                                                                       IPT 6040
      CALL WIO3(A(K30),A(K31),A(K32),A(K33),NVX,NSETVX)                 IPT 6050
C                                                                       IPT 6060
      INSEC = 36                                                        IPT 6070
  194 CONTINUE                                                          IPT 6080
      IF(NGC(2) .EQ. 0) GOTO 195                                        IPT 6090
      IO26 = IX(93)                                                     IPT 6100
      REWIND IO26                                                       IPT 6110
      READ (IO26) N11,(A(I),I=K24,N11)                                  IPT 6120
      REWIND IO26                                                       IPT 6130
  195 CONTINUE                                                          IPT 6140
      IF(IX(27) .EQ. 0) GOTO 196                                        IPT 6150
      IF(IMEM .GT. 0) GOTO 196                                          IPT 6160
      IVZ = IVX + NHEX                                                  IPT 6170
      KVZ = KVX * (NHEX+1)                                              IPT 6180
C                                                                       IPT 6190
      CALL KSIG(KVX,MVX,NVX,NSETVX)                                     IPT 6200
C                                                                       IPT 6210
      KNO = K18 - KNCOMP                                                IPT 6220
      IF(KNO .GE. 0) GOTO 207                                           IPT 6230
C                                                                       IPT 6240
      CALL KRST(IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1,KBVXP1,NSETVX, IPT 6250
     1 NVO,IVO,IVZ,KVZ,N3DDIM)                                          IPT 6260
C                                                                       IPT 6270
      KNO = K62 - KNCOMP                                                IPT 6280
      IF(KNO .GE. 0) GOTO 207                                           IPT 6290
  196 CONTINUE                                                          IPT 6300
      IRV = 0                                                           IPT 6310
      IF(KVX .GT. 1000) STOP 445                                        IPT 6320
C                                                                       IPT 6330
      CALL BNSB(A(K50),A(K51),KVX,IRV)                                  IPT 6340
C                                                                       IPT 6350
C     NSPA = NUMBER OF SPACE POINT ARRAYS                               IPT 6360
C            0 FOR STATICS OR DEPLETION PROBLEMS.                       IPT 6370
C            1 FOR FIXED-SOURCE PROBLEMS.                               IPT 6380
C            5 FOR XENON PROBLEMS.                                      IPT 6390
C            2 TIMES THE NUMBER OF DELAYED-NEUTRON GROUPS               IPT 6400
C            PLUS 5 FOR DYNAMICS PROBLEMS.                              IPT 6410
C                                                                       IPT 6420
      IF(NGC(10) .NE. -5) GOTO 197                                      IPT 6430
      IF(IX(131) .EQ. 1) GOTO 197                                       IPT 6440
      IERR = 50                                                         IPT 6450
      GOTO 206                                                          IPT 6460
  197 CONTINUE                                                          IPT 6470
      NCRP = IVX * JVX * KBVX                                           IPT 6480
      IF(NSPA .EQ. 0) NCRP = 1                                          IPT 6490
      IF(NSPA .EQ. 0) NSPA = 1                                          IPT 6500
      K64 = K62 + NCRP * NSPA                                           IPT 6510
      NBLOCK = 2200                                                     IPT 6520
C                                                                       IPT 6530
      CALL CNIO(IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,IVZ,KVZ,N3DDIM,LVX, IPT 6540
     1 NBLOCK,IOVX,IOVZ)                                                IPT 6550
C                                                                       IPT 6560
      IF(IX(37) .EQ. 0) GOTO 198                                        IPT 6570
      IF(MU4 .EQ. 1) WRITE (IOUT,1001)                                  IPT 6580
      GOTO 199                                                          IPT 6590
  198 IF(MU4 .EQ. 1) WRITE (IOUT,1002)                                  IPT 6600
  199 CONTINUE                                                          IPT 6610
      NDATA = K68                                                       IPT 6620
      KNRGN = NDATA + 1                                                 IPT 6630
      N1 = KNRGN + IVX * JVX * KBVX                                     IPT 6640
      KNO = N1 - KNCOMP                                                 IPT 6650
      IF(KNO .GE. 0) GOTO 207                                           IPT 6660
      N2 = N1 - 1                                                       IPT 6670
      READ (IO2) (A(N),N=KNRGN,N2)                                      IPT 6680
      REWIND IO2                                                        IPT 6690
      IF(MU4 .EQ. 1) WRITE (IOUT,1008) JMAX,IMAX,KBMAX,KMAX,IX(29),     IPT 6700
     1 IX(28),LMAX,MMAX                                                 IPT 6710
      NUSE = NDATA + IVX * JVX * KBVX + 3 * LVX                         IPT 6720
      MEMX = MEMORY - NUSE                                              IPT 6730
      IF(MU4 .EQ. 1) WRITE (IOUT,1009) MEMORY,NUSE,MEMX                 IPT 6740
C                                                                       IPT 6750
      CALL SIZE(IVX,JVX,KBVX,KVX,MVX,NVX,NSETVX,NRVX)                   IPT 6760
C                                                                       IPT 6770
      IF(IX(27) .EQ. 0) GOTO 201                                        IPT 6780
      IF(NGC(2) .NE. 0) GOTO 201                                        IPT 6790
      IF(NUAC(5) .NE. MM1AC5 .AND. INNO(4) .EQ. 0) NER(28) = 28         IPT 6800
      IF(MM1VX .EQ. MMAX .AND. KM1VX .EQ. KMAX .AND. NM1VX .EQ. NMAX    IPT 6810
     1 .AND. NSM1VX .EQ. NSETVX) GOTO 201                               IPT 6820
      IF(INNO(8) .EQ. 0 .AND. NGC(19) .GT. 0) NER(25) = 25              IPT 6830
      IF(NGC(19) .EQ. 0 .AND. INNO(20) .EQ. 0) NER(25) = 25             IPT 6840
      IF(INNO(24) .EQ. 0) NER(25) = 25                                  IPT 6850
  201 N1 = 0                                                            IPT 6860
      IF(IX(7) .EQ. 1 .AND. INNO(5) .EQ. 0) NER(19) = 19                IPT 6870
      DO 203 I=1,200                                                    IPT 6880
        IF(NER(I)) 203,203,202                                          IPT 6890
  202   N1 = NER(I)                                                     IPT 6900
        WRITE (IOUT,1010) N1                                            IPT 6910
  203 CONTINUE                                                          IPT 6920
      IF(N1) 204,204,208                                                IPT 6930
  204 CONTINUE                                                          IPT 6940
      IF(IX(27)) 205,205,209                                            IPT 6950
  205 N1 = INNO(1) + INNO(3) + INNO(4) + INNO(5)                        IPT 6960
      N2 = N1 * INNO(8)                                                 IPT 6970
      N3 = N1 * INNO(12) * INNO(20)                                     IPT 6980
      N = N2 + N3                                                       IPT 6990
      IF(N) 105,105,209                                                 IPT 7000
  206 WRITE (IOUT,1010) IERR                                            IPT 7010
      GOTO 208                                                          IPT 7020
  207 WRITE (IOUT,1013) KNO                                             IPT 7030
      IF(MU4 .EQ. 1)                                                    IPT 7040
     1WRITE (IOUT,1008) JMAX,IMAX,KBMAX,KMAX,IX(29),IX(28),LMAX,MMAX    IPT 7050
C                                                                       IPT 7060
  208 CALL EXIT                                                         IPT 7070
C                                                                       IPT 7080
  209 CONTINUE                                                          IPT 7090
C                                                                       IPT 7100
      IF(NGC(2) .EQ. 0 .AND. NDPL(11) .GT. 0) CALL RODO(A(KNCOMP),LVX)  IPT 7110
C                                                                       IPT 7120
      IX(190) = NUO                                                     IPT 7130
      IX(191) = NHEX                                                    IPT 7140
      IX(192) = NRVX                                                    IPT 7150
      IX(193) = IJKBX                                                   IPT 7160
      IX(150) = 0                                                       IPT 7170
      IF(NGC(5) .NE. 0 .AND. NGC(1) .EQ. 0) IX(150) = 1                 IPT 7180
      RETURN                                                            IPT 7190
C                                                                       IPT 7200
 1000 FORMAT (1H0,'ERROR STOP 27',4I5)                                  IPT 7210
 1001 FORMAT (1H0/1H0,'EQUATION CONSTANTS WILL BE STORED ON I/O LOGICAL IPT 7220
     115')                                                              IPT 7230
 1002 FORMAT (1H0/1H0,'EQUATION CONSTANTS WILL BE STORED IN CORE')      IPT 7240
 1003 FORMAT (24I3)                                                     IPT 7250
 1004 FORMAT (1H0,'THE FOLLOWING INPUT SECTIONS HAVE BEEN READ'/1H ,32I4IPT 7260
     > / 1H ,32I4)                                                      IPT 7270
 1005 FORMAT (1H0,'INPUT SECTION NUMBER',I6,'    IS IN ERROR')          IPT 7280
 1006 FORMAT (1H0,'NCHECK',8I8)                                         IPT 7290
 1007 FORMAT (24I3)                                                     IPT 7300
 1008 FORMAT (1H0,'NUMBER OF---COLUMNS, ROWS, PLANES, GROUPS,',1H ,'UPSCIPT 7310
     1AT, DOWNSCAT, MAT.COMP., AND ZONES    ',6I4,I6,I5)                IPT 7320
 1009 FORMAT (1H0,'MEMORY LOCATIONS RESERVED FOR DATA STORAGE---',I7/1H IPT 7330
     1 ,'MEMORY LOCATIONS USED FOR THIS PROBLEM-------',I7/1H ,'MEMORY LIPT 7340
     2OCATIONS NOT USED--------------------',I7)                        IPT 7350
 1010 FORMAT (1H0,'ERROR STOP NUMBER',I3)                               IPT 7360
 1013 FORMAT (1H0,'ERROR STOP NUMBER 2'/1H ,'MEMORY RESERVED FOR DATA EXIPT 7370
     1CEEDED BY MORE THAN',I7,' WORDS')                                 IPT 7380
      END                                                               IPT 7390
      SUBROUTINE MACR(SIG,F1,XI,MVX,KVX,IOMC)                           MAC   10
C                                                                       MAC   20
CMACR --035 ***CITATION*** READS INPUT SECTION 008/ CF-IPTM             MAC   30
C                                                                       MAC   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,MAC   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   MAC   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), MAC   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    MAC   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    MAC   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   MAC  100
     6 IXPUT(9999),XPUT(9999)                                           MAC  110
C                                                                       MAC  120
      COMMON /MU/ MU4                                                   MAC  130
C                                                                       MAC  140
      DIMENSION SIG(KVX,MVX,8),F1(KVX,KVX,MVX),XI(KVX)                  MAC  150
C                                                                       MAC  160
C     FOR INPUT PURPOSES WE SHALL TEMPORARILY EXEED THE SPACE           MAC  170
C     ALLOCATED FOR ARRAY F1, THEREFORE SAVE VALUES                     MAC  180
C                                                                       MAC  190
C                                                                       MAC  200
      REWIND IO1                                                        MAC  210
      WRITE (IO1) (((F1(KK,K,M),KK=1,KMAX),K=1,KMAX),M=1,MMAX)          MAC  220
      END FILE IO1                                                      MAC  230
      REWIND IO1                                                        MAC  240
      IO14 = IX(81)                                                     MAC  250
      IF(IX(27) .LE. 0) GOTO 101                                        MAC  260
      IF(MM1VX .NE. MVX .OR. KM1VX .NE. KVX) GOTO 101                   MAC  270
      REWIND IO14                                                       MAC  280
      DO 100 K=1,KMAX                                                   MAC  290
        READ (IO14) ((F1(KK,K,M),KK=1,KMAX),M=1,MMAX)                   MAC  300
  100 CONTINUE                                                          MAC  310
      REWIND IO14                                                       MAC  320
  101 CONTINUE                                                          MAC  330
      AX = 0                                                            MAC  340
      DO 103 M=1,MMAX                                                   MAC  350
        DO 102 K=1,KMAX                                                 MAC  360
          SIG(K,M,7) = 0.0                                              MAC  370
          SIG(K,M,8) = 0.0                                              MAC  380
          IF(MM1VX .EQ. MVX .AND. KM1VX .EQ. KVX) GOTO 102              MAC  390
          SIG(K,M,1) = 0.0                                              MAC  400
          SIG(K,M,3) = 0.0                                              MAC  410
          SIG(K,M,4) = 0.0                                              MAC  420
          SIG(K,M,5) = 0.0                                              MAC  430
          SIG(K,M,6) = 0.0                                              MAC  440
  102   CONTINUE                                                        MAC  450
  103 CONTINUE                                                          MAC  460
  104 READ (IOMC,1000) M,K,T1,T2,T3,T4,T5                               MAC  470
      IF(M) 107,107,105                                                 MAC  480
  105 IF(K .GT. KMAX) GOTO 106                                          MAC  490
      IF(M .GT. MMAX) GOTO 106                                          MAC  500
      IF(K .EQ. 0) GOTO 106                                             MAC  510
      READ (IOMC,1001) (F1(KK,K,M),KK=1,KMAX)                           MAC  520
      SIG(K,M,1) = T1                                                   MAC  530
      SIG(K,M,3) = T2                                                   MAC  540
      SIG(K,M,4) = T3                                                   MAC  550
      SIG(K,M,5) = T4                                                   MAC  560
      AX = AX + T5                                                      MAC  570
      SIG(K,M,7) = T5                                                   MAC  580
      GOTO 104                                                          MAC  590
  106 JB = 34                                                           MAC  600
      WRITE (IOUT,1003) JB,K,KMAX,M,MMAX                                MAC  610
C                                                                       MAC  620
      CALL EXIT                                                         MAC  630
C                                                                       MAC  640
  107 IF(AX .NE. 0.) GOTO 110                                           MAC  650
      DO 109 M=1,MMAX                                                   MAC  660
        DO 108 K=1,KMAX                                                 MAC  670
          SIG(K,M,7) = SIG(K,M,4) * 1.0E+6                              MAC  680
  108   CONTINUE                                                        MAC  690
  109 CONTINUE                                                          MAC  700
  110 CONTINUE                                                          MAC  710
C                                                                       MAC  720
C     STORE F1 ARRAY IN FORM TO BE USED LATER                           MAC  730
C                                                                       MAC  740
      REWIND IO14                                                       MAC  750
      DO 111 K=1,KMAX                                                   MAC  760
        WRITE (IO14) ((F1(KK,K,M),KK=1,KMAX),M=1,MMAX)                  MAC  770
  111 CONTINUE                                                          MAC  780
      END FILE IO14                                                     MAC  790
      REWIND IO14                                                       MAC  800
      DO 115 M=1,MMAX                                                   MAC  810
        DO 114 K=1,KMAX                                                 MAC  820
          SIG(K,M,2) = 0.0                                              MAC  830
          DO 113 KK=1,KMAX                                              MAC  840
            SIG(K,M,2) = SIG(K,M,2) + F1(KK,K,M)                        MAC  850
  113     CONTINUE                                                      MAC  860
  114   CONTINUE                                                        MAC  870
  115 CONTINUE                                                          MAC  880
C                                                                       MAC  890
C     RESET F1 TO PREVIOUS VALUES                                       MAC  900
C                                                                       MAC  910
      READ (IO1) (((F1(KK,K,M),KK=1,KMAX),K=1,KMAX),M=1,MMAX)           MAC  920
      REWIND IO1                                                        MAC  930
      READ (IOMC,1001) (XI(K),K=1,KMAX)                                 MAC  940
      SUMXI = 0.0                                                       MAC  950
      DO 116 K=1,KMAX                                                   MAC  960
        SUMXI = SUMXI + XI(K)                                           MAC  970
  116 CONTINUE                                                          MAC  980
      IF(MU4 .EQ. 1) WRITE (IOUT,1002) (XI(K),K=1,KMAX),SUMXI           MAC  990
      RETURN                                                            MAC 1000
C                                                                       MAC 1010
 1000 FORMAT (2I6,5E12.0)                                               MAC 1020
 1001 FORMAT (6E12.0)                                                   MAC 1030
 1002 FORMAT (1H0/1H0,'FISSION SOURCE DISTRIBUTION AND SUM',10F9.5/(1H ,MAC 1040
     1 14F9.5))                                                         MAC 1050
 1003 FORMAT (1H0,'***DATA ERROR STOP',6I6)                             MAC 1060
      END                                                               MAC 1070
      SUBROUTINE TRAN(CONC,MVX,NVX,KO3,KO10,HOX,NFO,NNFO,NIC,NSETVX)    TRA   10
C                                                                       TRA   20
CTRAN --060***CITATION***TRANSFERS INPUT DATA FROM KO3 TO KO10/CF-DISK, TRA   30
C                                                                 RSTR  TRA   40
C                                                                       TRA   50
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,TRA   60
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   TRA   70
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), TRA   80
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    TRA   90
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    TRA  100
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   TRA  110
     6 IXPUT(9999),XPUT(9999)                                           TRA  120
C                                                                       TRA  130
      DIMENSION CONC(NVX,MVX),HOX(NVX,NSETVX,20),NFO(20,NSETVX),        TRA  140
     1 NNFO(20,NSETVX),NIC(1000)                                        TRA  150
C                                                                       TRA  160
C                                                                       TRA  170
  100 READ (KO3,END=118) INSEC,NREC,NSET,I1,I2                          TRA  180
      IF(INSEC .EQ. -9999 .AND. NREC .EQ. -9999) GOTO 118               TRA  190
      WRITE (KO10) INSEC,NREC,NSET,I1,I2                                TRA  200
      IF(INSEC .EQ. 28) GOTO 101                                        TRA  210
      IF(INSEC .EQ. 40) GOTO 108                                        TRA  220
      IF(INSEC .EQ. 36) GOTO 115                                        TRA  230
      IF(INSEC .EQ. 30) GOTO 116                                        TRA  240
C                                                                       TRA  250
C     ADD AN IF FOR EACH NEW SECT. ON IO10                              TRA  260
C                                                                       TRA  270
      GOTO 100                                                          TRA  280
  101 IF(NSET .EQ. 0) GOTO 100                                          TRA  290
      DO 107 N=1,NSET                                                   TRA  300
        READ (KO3) (NSRH(I),I=1,24),(XSRH1(I),I=1,6)                    TRA  310
        WRITE (KO10) (NSRH(I),I=1,24),(XSRH1(I),I=1,6)                  TRA  320
        IF(NSRH(2) .EQ. 3) GOTO 103                                     TRA  330
        IF(NSRH(10) .NE. -1) GOTO 107                                   TRA  340
        READ (KO3) (NXTR1(I),I=1,5),(XTR1(I),I=1,5)                     TRA  350
        WRITE (KO10) (NXTR1(I),I=1,5),(XTR1(I),I=1,5)                   TRA  360
        NXZ = NXTR1(1)                                                  TRA  370
        READ (KO3) (NXTR1(I),I=1,NXZ)                                   TRA  380
        WRITE (KO10) (NXTR1(I),I=1,NXZ)                                 TRA  390
        DO 102 M=1,NXZ                                                  TRA  400
          READ (KO3) (XTR1(I),I=1,I1)                                   TRA  410
          WRITE (KO10) (XTR1(I),I=1,I1)                                 TRA  420
  102   CONTINUE                                                        TRA  430
        GOTO 107                                                        TRA  440
  103   CONTINUE                                                        TRA  450
        READ (KO3) (NXTR1(I),I=1,7),(XTR1(I),I=1,5)                     TRA  460
        WRITE (KO10) (NXTR1(I),I=1,7),(XTR1(I),I=1,5)                   TRA  470
        NXJ = NXTR1(1)                                                  TRA  480
        NXI = NXTR1(2)                                                  TRA  490
        NXKB = NXTR1(3)                                                 TRA  500
        IF(NXJ .LE. 0) GOTO 104                                         TRA  510
        READ (KO3) (XTR1(I),I=1,NXJ)                                    TRA  520
        WRITE (KO10) (XTR1(I),I=1,NXJ)                                  TRA  530
  104   IF(NXI .LE. 0) GOTO 105                                         TRA  540
        READ (KO3) (XTR1(I),I=1,NXI)                                    TRA  550
        WRITE (KO10) (XTR1(I),I=1,NXI)                                  TRA  560
  105   IF(NXKB .LE. 0) GOTO 106                                        TRA  570
        READ (KO3) (XTR1(I),I=1,NXKB)                                   TRA  580
        WRITE (KO10) (XTR1(I),I=1,NXKB)                                 TRA  590
  106   CONTINUE                                                        TRA  600
  107 CONTINUE                                                          TRA  610
      GOTO 100                                                          TRA  620
  108 IF(NREC .EQ. 0) GOTO 110                                          TRA  630
      DO 109 N=1,NREC                                                   TRA  640
        READ (KO3) I1,I2,J1,T1,J2,T2,J3,T3,J4,T4,J5,T5                  TRA  650
        WRITE (KO10) I1,I2,J1,T1,J2,T2,J3,T3,J4,T4,J5,T5                TRA  660
  109 CONTINUE                                                          TRA  670
  110 READ (KO3) INSEC,NREC,KAAX,I1,I2                                  TRA  680
      WRITE (KO10) INSEC,NREC,KAAX,I1,I2                                TRA  690
      IF(NREC .EQ. 0) GOTO 112                                          TRA  700
      DO 111 N=1,NREC                                                   TRA  710
        READ (KO3) (XTR1(K),K=1,KAAX)                                   TRA  720
        WRITE (KO10) (XTR1(K),K=1,KAAX)                                 TRA  730
  111 CONTINUE                                                          TRA  740
  112 READ (KO3) INSEC,NREC,NN,NAAX,MAAX                                TRA  750
      WRITE (KO10) INSEC,NREC,NN,NAAX,MAAX                              TRA  760
      IF(NREC .EQ. 0) GOTO 100                                          TRA  770
      IF(NN .EQ. 0) GOTO 113                                            TRA  780
      READ (KO3) (NXTR1(N),N=1,NN)                                      TRA  790
  113 CONTINUE                                                          TRA  800
      READ (KO3) ((CONC(N,M),N=1,NAAX),M=1,MAAX)                        TRA  810
      IF(NN .EQ. 0) GOTO 114                                            TRA  820
      WRITE (KO10) (NXTR1(N),N=1,NN)                                    TRA  830
  114 CONTINUE                                                          TRA  840
      WRITE (KO10) ((CONC(N,M),N=1,NAAX),M=1,MAAX)                      TRA  850
      GOTO 112                                                          TRA  860
  115 CONTINUE                                                          TRA  870
      READ (KO3) (((HOX(NV,NS,I20),NV=1,NSET),NS=1,I1),I20=1,20)        TRA  880
      READ (KO3) ((NFO(I20,NS),I20=1,20),NS=1,I1)                       TRA  890
      READ (KO3) ((NNFO(I20,NS),I20=1,20),NS=1,I1)                      TRA  900
      READ (KO3) NIC                                                    TRA  910
      WRITE (KO10) (((HOX(NV,NS,I20),NV=1,NSET),NS=1,I1),I20=1,20)      TRA  920
      WRITE (KO10) ((NFO(I20,NS),I20=1,20),NS=1,I1)                     TRA  930
      WRITE (KO10) ((NNFO(I20,NS),I20=1,20),NS=1,I1)                    TRA  940
      WRITE (KO10) NIC                                                  TRA  950
      GOTO 100                                                          TRA  960
  116 READ (KO3) (NXTR1(I),I=1,24)                                      TRA  970
      WRITE (KO10) (NXTR1(I),I=1,24)                                    TRA  980
      READ (KO3) (NXTR1(I),I=1,24)                                      TRA  990
      WRITE (KO10) (NXTR1(I),I=1,24)                                    TRA 1000
      DO 117 II=1,NSET                                                  TRA 1010
        READ (KO3) J1,J2,J3,(NXTR1(I),I=1,J2)                           TRA 1020
        WRITE (KO10) J1,J2,J3,(NXTR1(I),I=1,J2)                         TRA 1030
  117 CONTINUE                                                          TRA 1040
      GOTO 100                                                          TRA 1050
  118 CONTINUE                                                          TRA 1060
      RETURN                                                            TRA 1070
      END                                                               TRA 1080
      SUBROUTINE WIO3(HOX,NFO,NNFO,NIC,NVX,NSETVX)                      WIO   10
C                                                                       WIO   20
CWIO3 --60.2 ***CITATION*** WRITE 036 DATA ON IO3/ CF-IPTM              WIO   30
C                                                                       WIO   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,WIO   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   WIO   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), WIO   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    WIO   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    WIO   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   WIO  100
     6 IXPUT(9999),XPUT(9999)                                           WIO  110
C                                                                       WIO  120
      DIMENSION HOX(NVX,NSETVX,20),NFO(NSETVX,20),NNFO(NSETVX,20),      WIO  130
     1 NIC(1000)                                                        WIO  140
C                                                                       WIO  150
C                                                                       WIO  160
      REWIND IO3                                                        WIO  170
      NSEC = 36                                                         WIO  180
      NREC = 4                                                          WIO  190
      IDUM = 0                                                          WIO  200
      WRITE (IO3) NSEC,NREC,NVX,NSETVX,IDUM                             WIO  210
      WRITE (IO3) HOX                                                   WIO  220
      WRITE (IO3) NFO                                                   WIO  230
      WRITE (IO3) NNFO                                                  WIO  240
      WRITE (IO3) NIC                                                   WIO  250
      RETURN                                                            WIO  260
      END                                                               WIO  270
      SUBROUTINE EIGN(A,P2,P2E,NTITE,IDX,JDX,KDX,JIDX,KBDX,IX3738)      EIG   10
C                                                                       EIG   20
CEIGN --076 ***CITATION*** EIGENVALUE CALC. CONTROL/ CF-CALR            EIG   30
C                                                                       EIG   40
      REAL*8 P2                                                         EIG   50
C                                                                       EIG   60
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,EIG   70
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   EIG   80
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), EIG   90
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    EIG  100
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    EIG  110
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   EIG  120
     6 IXPUT(9999),XPUT(9999)                                           EIG  130
C                                                                       EIG  140
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   EIG  150
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKEIG  160
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    EIG  170
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  EIG  180
     4 ITMAX,ITIME,BET(211),DEL(211)                                    EIG  190
C                                                                       EIG  200
      COMMON /AKADD/ KAY(1),K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13, EIG  210
     1 K131,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,    EIG  220
     2 K28,K29,K30,K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,K41,K42,K43, EIG  230
     3 K44,K45,K46,K47,K48,K49,K50,K51,K52,K53,K54,K55,K56,K57,K58,     EIG  240
     4 K59,K60,K61,K62,K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,     EIG  250
     5 K74,K75,K76,K77,K78,K79,K80,K81,K82,K83,K84,K85,K86,K87,K88,     EIG  260
     6 K89,K90,K91,K92,K93,K94,K95,K96,K97,K98,K99,K100,NDATA,KNRGN,    EIG  270
     7 KNCOMP,KPVOL,KRVOL,MEMVRY,MEMX                                   EIG  280
C                                                                       EIG  290
      COMMON /AVDLM/ IVDLM(1),IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1, EIG  300
     1 KBVXP1,NSETVX,NVO,IVO,IVZ,KVZ,NCRP,NSPA,N3DDIM,NBLOCK,JIVX,JIP1VXEIG  310
     2 ,JP1IXZ,IOVX,IOVZ                                                EIG  320
C                                                                       EIG  330
      DIMENSION A(1),P2(JDX,IDX,KDX),P2E(JIDX,KBDX,KDX)                 EIG  340
C                                                                       EIG  350
C                                                                       EIG  360
      IX(40) = 0                                                        EIG  370
      JIPROD = JMAX * IMAX                                              EIG  380
      IF(IX(24) .EQ. 0) GOTO 102                                        EIG  390
      DO 100 K=1,KMAX                                                   EIG  400
        READ (IOFLX)                                                    EIG  410
  100 CONTINUE                                                          EIG  420
      READ (IOFLX)                                                      EIG  430
      IX(5) = 0                                                         EIG  440
      XLAMDA = SPARE(27)                                                EIG  450
      IF(NUAC(5) .GT. 10) GOTO 101                                      EIG  460
      IF(IX(71) .GT. 0) GOTO 106                                        EIG  470
      GOTO 107                                                          EIG  480
  101 IF(IX(71) .GT. 0) GOTO 111                                        EIG  490
      GOTO 112                                                          EIG  500
  102 CONTINUE                                                          EIG  510
      IX(17) = IX(5)                                                    EIG  520
      INDO = 0                                                          EIG  530
C                                                                       EIG  540
C     IN BIGS, ARRAY F1 WILL USE THE CORE RESERVED FOR ARRAY SCAC       EIG  550
C                                                                       EIG  560
      CALL BIGS(A(K23),A(K17),A(K1),A(K2),A(K3),A(K4),A(K5),A(K6),A(K14)EIG  570
     1 ,A(K8),A(K7),INDO,A(K9),KVX,MVX,NVX,NSETVX,A(K50),A(K16),A(K18), EIG  580
     2 A(K13),NVO)                                                      EIG  590
C                                                                       EIG  600
      IF(NUAC(5)-10) 105,105,110                                        EIG  610
  105 CONTINUE                                                          EIG  620
  106 CONTINUE                                                          EIG  630
C                                                                       EIG  640
      CALL CNST(A(KNRGN),A(K23),A(K65),A(K64),A(K15),A(K14),A(K63),     EIG  650
     1 A(KNCOMP),A(KPVOL),A(K50),A(K51),IVX,JVX,KVX,LVX,MVX,IVXP1,JVXP1,EIG  660
     2 IVZ,KVZ,IOVX,IOVZ,A,MEMVRY,A(K64),IX3738)                        EIG  670
C                                                                       EIG  680
      IF(IX(172) .GT. 0) GOTO 108                                       EIG  690
      IF(IX(71) .GT. 0) GOTO 107                                        EIG  700
      IF(IX(39) .NE. 0) GOTO 107                                        EIG  710
      IF(IX(22) .NE. 0) GOTO 107                                        EIG  720
      IF(NTITE .NE. 0) GOTO 107                                         EIG  730
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .EQ. 0) GOTO 109                   EIG  740
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .NE. 0) GOTO 107                   EIG  750
      IF(IX(2) .GT. 1) GOTO 109                                         EIG  760
  107 CONTINUE                                                          EIG  770
      IF(IX(5) .GE. 2 .AND. IX(73) .GT. 1) GOTO 108                     EIG  780
C                                                                       EIG  790
      CALL INFX(P2,A(KNRGN),A(KNCOMP),A(K50),IVX,JVX,KVX,LVX)           EIG  800
C                                                                       EIG  810
  108 CONTINUE                                                          EIG  820
      IX(39) = 0                                                        EIG  830
      IX(22) = 0                                                        EIG  840
  109 CONTINUE                                                          EIG  850
C                                                                       EIG  860
      CALL FLUX(A(KNRGN),A(K29),A(K21),A(K36),A(K37),A(K38),A(K39),     EIG  870
     1 A(K23),A(K17),A(K41),A(K24),A(K65),A(K64),A(K19),A(K20),A(K63),  EIG  880
     2 A(KNCOMP),A(K15),A(K1),A(K2),A(K3),A(K4),A(K5),A(K6),A(K14),A(K8)EIG  890
     3 ,A(K7),A(KPVOL),A(K9),A(K50),A(K51),A(K16),A(K52),A(K49),IVX,JVX,EIG  900
     4 KVX,LVX,MVX,NVX,IVXP1,JVXP1,IVZ,KVZ,NSETVX,IOVX,IOVZ,A,MEMVRY,   EIG  910
     5 A(K64),IX3738,A(K62),A(K45),NCRP,NSPA,A(K18),A(K13),NVO)         EIG  920
C                                                                       EIG  930
      GOTO 115                                                          EIG  940
  110 CONTINUE                                                          EIG  950
  111 CONTINUE                                                          EIG  960
C                                                                       EIG  970
      CALL KNST(A(KNRGN),A(K23),A(K65),A(K64),A(K66),A(K15),A(K14),     EIG  980
     1 A(K63),A(KNCOMP),A(KPVOL),A(K50),A(K51),IVX,JVX,KBVX,KVX,LVX,MVX,EIG  990
     2 IVXP1,JVXP1,KBVXP1,IVZ,KVZ,JIVX,JIP1VX,JP1IXZ,IOVX,IOVZ,A,MEMVRY,EIG 1000
     3 A(K64),IX3738)                                                   EIG 1010
C                                                                       EIG 1020
      IF(IX(172) .GT. 0) GOTO 113                                       EIG 1030
      IF(IX(71) .GT. 0) GOTO 112                                        EIG 1040
      IF(IX(39) .NE. 0) GOTO 112                                        EIG 1050
      IF(IX(22) .NE. 0) GOTO 112                                        EIG 1060
      IF(NTITE .NE. 0) GOTO 112                                         EIG 1070
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .EQ. 0) GOTO 114                   EIG 1080
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .NE. 0) GOTO 112                   EIG 1090
      IF(IX(2) .GT. 1) GOTO 114                                         EIG 1100
  112 CONTINUE                                                          EIG 1110
      IF(IX(5) .GE. 2 .AND. IX(73) .GT. 1) GOTO 113                     EIG 1120
C                                                                       EIG 1130
      CALL KNFX(P2E,A(KNRGN),A(KNCOMP),A(K50),IVX,JVX,KBVX,KVX,LVX,JIVX)EIG 1140
C                                                                       EIG 1150
  113 CONTINUE                                                          EIG 1160
      IX(39) = 0                                                        EIG 1170
      IX(22) = 0                                                        EIG 1180
  114 CONTINUE                                                          EIG 1190
C                                                                       EIG 1200
      CALL KLUX(A(KNRGN),A(K29),A(K21),A(K36),A(K37),A(K38),A(K39),     EIG 1210
     1 A(K23),A(K17),A(K41),A(K24),A(K65),A(K64),A(K66),A(K19),A(K20),  EIG 1220
     2 A(K63),A(KNCOMP),A(K15),A(K1),A(K2),A(K3),A(K4),A(K5),A(K6),     EIG 1230
     3 A(K14),A(K8),A(K7),A(KPVOL),A(K9),A(K50),A(K51),A(K16),A(K52),   EIG 1240
     4 A(K49),IVX,JVX,KBVX,KVX,LVX,MVX,NVX,IVXP1,JVXP1,KBVXP1,IVZ,KVZ,  EIG 1250
     5 NSETVX,JIVX,JIP1VX,JP1IXZ,IOVX,IOVZ,A,MEMVRY,A(K64),IX3738,A(K62)EIG 1260
     6 ,A(K45),NCRP,NSPA,A(K18),A(K13),NVO)                             EIG 1270
C                                                                       EIG 1280
  115 CONTINUE                                                          EIG 1290
      IF(IX(5) .LT. 2) GOTO 122                                         EIG 1300
      IF(IX(5) .GT. 2) GOTO 117                                         EIG 1310
CFZJ055                                                       25.09.07  EIG 1320
      GOTO 118                                                          EIG 1330
  117 CONTINUE                                                          EIG 1340
C                                                                       EIG 1350
      CALL CRSH(A(KRVOL),A(KPVOL),LVX)                                  EIG 1360
C                                                                       EIG 1370
  118 CONTINUE                                                          EIG 1380
      IF(IX(73) .EQ. 2) GOTO 122                                        EIG 1390
      IF(IX(75) .EQ. 1 .OR. IX(70) .NE. 0) GOTO 119                     EIG 1400
      GOTO 120                                                          EIG 1410
  119 IF(IX(5) .EQ. 2) GOTO 123                                         EIG 1420
      GOTO 124                                                          EIG 1430
  120 IF(NUAC(5) .GT. 10) GOTO 121                                      EIG 1440
C                                                                       EIG 1450
      CALL DASH(P2,A(K19),JVX,IVX,KVX)                                  EIG 1460
C                                                                       EIG 1470
      GOTO 122                                                          EIG 1480
C                                                                       EIG 1490
  121 CALL KASH(P2E,A(K19),JVX,IVX,KBVX,KVX,JIVX)                       EIG 1500
C                                                                       EIG 1510
  122 CONTINUE                                                          EIG 1520
      IF(IX(5) .EQ. 1) GOTO 123                                         EIG 1530
      IF(IX(5) .EQ. -1) GOTO 123                                        EIG 1540
      IF(IX(5) .EQ. -2) GOTO 123                                        EIG 1550
      GOTO 124                                                          EIG 1560
C                                                                       EIG 1570
  123 CALL FASP(A(K1),A(K14),A(K17),A(K9),A(KPVOL),A(KNCOMP),A(K21),    EIG 1580
     1 XLAMDA,KVX,NVX,MVX,LVX,NSETVX)                                   EIG 1590
C                                                                       EIG 1600
  124 CONTINUE                                                          EIG 1610
      IF(IX(24) .GT. 0) GOTO 129                                        EIG 1620
      IF(IX(5) .GE. 2 .AND. IX(70) .NE. 0) GOTO 125                     EIG 1630
      IF(IX(5) .GE. 2 .AND. IX(75) .LE. 0) GOTO 134                     EIG 1640
  125 CONTINUE                                                          EIG 1650
      IF(NUAC(3) .GT. 0) GOTO 126                                       EIG 1660
C                                                                       EIG 1670
      CALL NMBL(A(K21),A(K38),P2,A(KRVOL),A(KNCOMP),A(K14),P2E,A(K52),  EIG 1680
     1 IVX,JVX,KBVX,KVX,LVX,MVX,JIVX,A(K36),A(K39))                     EIG 1690
C                                                                       EIG 1700
  126 CONTINUE                                                          EIG 1710
      DO 128 K=1,KMAX                                                   EIG 1720
        IF(NUAC(5) .LE. 10) GOTO 127                                    EIG 1730
        WRITE (IOFLX) ((P2E(N1,KB,K),N1=1,JIPROD),KB=1,KBMAX)           EIG 1740
        GOTO 128                                                        EIG 1750
  127   WRITE (IOFLX) ((P2(J,I,K),J=1,JMAX),I=1,IMAX)                   EIG 1760
  128 CONTINUE                                                          EIG 1770
      WRITE (IOFLX) XLAMDA,BETTA,VRGK2                                  EIG 1780
      END FILE IOFLX                                                    EIG 1790
      REWIND IOFLX                                                      EIG 1800
      GOTO 134                                                          EIG 1810
  129 CONTINUE                                                          EIG 1820
      DO 131 K=1,KMAX                                                   EIG 1830
        IF(NUAC(5) .LE. 10) GOTO 130                                    EIG 1840
        WRITE (IOFLX) ((P2E(N1,KB,K),N1=1,JIPROD),KB=1,KBMAX)           EIG 1850
        GOTO 131                                                        EIG 1860
  130   WRITE (IOFLX) ((P2(J,I,K),J=1,JMAX),I=1,IMAX)                   EIG 1870
  131 CONTINUE                                                          EIG 1880
      END FILE IOFLX                                                    EIG 1890
      REWIND IOFLX                                                      EIG 1900
      DO 133 K=1,KMAX                                                   EIG 1910
        IF(NUAC(5) .LE. 10) GOTO 132                                    EIG 1920
        READ (IOFLX) ((P2E(N1,KB,K),N1=1,JIPROD),KB=1,KBMAX)            EIG 1930
        GOTO 133                                                        EIG 1940
  132   READ (IOFLX) ((P2(J,I,K),J=1,JMAX),I=1,IMAX)                    EIG 1950
  133 CONTINUE                                                          EIG 1960
      READ (IOFLX) XLAMDA,BETTA,VRGK2                                   EIG 1970
      REWIND IOFLX                                                      EIG 1980
  134 CONTINUE                                                          EIG 1990
      RETURN                                                            EIG 2000
      END                                                               EIG 2010
      SUBROUTINE EXTR                                                   EXT   10
C                                                                       EXT   20
CEXTR --079***CITATION*** EXTRAPOLATION SUBROUTINE/CF FLUX,KLUX         EXT   30
C                                                                       EXT   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,EXT   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   EXT   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), EXT   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    EXT   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    EXT   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   EXT  100
     6 IXPUT(9999),XPUT(9999)                                           EXT  110
C                                                                       EXT  120
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   EXT  130
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKEXT  140
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    EXT  150
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  EXT  160
     4 ITMAX,ITIME,BET(211),DEL(211)                                    EXT  170
C                                                                       EXT  180
CFZJ055                                                       25.09.07  EXT  190
C                                                                       EXT  200
      DIMENSION RP(10)                                                  EXT  210
C                                                                       EXT  220
C                                                                       EXT  230
      IF(IX(196) .EQ. 0) GOTO 101                                       EXT  240
      IX(196) = 0                                                       EXT  250
      NTITE = IX(198)                                                   EXT  260
      IF(NTITE .NE. 0) GOTO 101                                         EXT  270
      IF(NGC(2) .EQ. 0 .OR. NUAC(2) .NE. 0) GOTO 101                    EXT  280
      ISK = IX(160)                                                     EXT  290
      IEXR = IX(161)                                                    EXT  300
      NI3 = IX(162)                                                     EXT  310
      IC10 = IX(163)                                                    EXT  320
      IC20 = IX(164)                                                    EXT  330
      IBCUT = IX(197)                                                   EXT  340
      XXMU = SPARE(31)                                                  EXT  350
      XOMU = SPARE(32)                                                  EXT  360
      VVVV = SPARE(33)                                                  EXT  370
      XUSQ = SPARE(40)                                                  EXT  380
      DO 100 I=1,10                                                     EXT  390
        I1 = 160 + I                                                    EXT  400
        RP(I) = SPARE(I1)                                               EXT  410
  100 CONTINUE                                                          EXT  420
  101 CONTINUE                                                          EXT  430
      IX135 = IX(135)                                                   EXT  440
      IX(135) = 0                                                       EXT  450
      NA = IX(35)                                                       EXT  460
      NIT = NIT + 1                                                     EXT  470
      IF(NIT .EQ. 1) ISK = 0                                            EXT  480
      IF(IX(172) .GT. 0 .AND. NIT .EQ. 1) GOTO 105                      EXT  490
      IF(IX(172) .GT. 0) GOTO 109                                       EXT  500
      IF(NIT-30) 103,103,109                                            EXT  510
  103 IF(NIT-1) 104,104,106                                             EXT  520
  104 CONTINUE                                                          EXT  530
      ISK = 0                                                           EXT  540
      IBCUT = NUAC(19)                                                  EXT  550
      IF(IBCUT .GT. 0) GOTO 105                                         EXT  560
      XUSQ = 1.0 - (2.0/BETTA-1.0)**2                                   EXT  570
      SPARE(40) = XUSQ                                                  EXT  580
      BETTA = 2.0 / (2.0-XUSQ)                                          EXT  590
  105 IEXR = 0                                                          EXT  600
      NI3 = 0                                                           EXT  610
      VOG1 = 1.0                                                        EXT  620
      IC10 = 12                                                         EXT  630
      IC20 = 20                                                         EXT  640
      GOTO 108                                                          EXT  650
  106 IF(IBCUT) 107,107,109                                             EXT  660
  107 X = 1.0 - 0.25 * XUSQ * BETTA                                     EXT  670
      IF(X .LE. 0.) GOTO 109                                            EXT  680
      BETTA = AMIN1(1.0/X,1.9999)                                       EXT  690
  108 VRGK2 = BETTA                                                     EXT  700
      SPARE(39) = BETTA                                                 EXT  710
  109 CONTINUE                                                          EXT  720
      NIIT = NIIT + 1                                                   EXT  730
      IF(NIIT-3) 111,110,111                                            EXT  740
  110 IEXR = 1                                                          EXT  750
  111 CONTINUE                                                          EXT  760
      IX(32) = 0                                                        EXT  770
      IX(33) = 0                                                        EXT  780
      VRG2 = VRG1                                                       EXT  790
      VRGP2 = VRGP1                                                     EXT  800
      EXFC3 = EXFC2                                                     EXT  810
      EXFC2 = EXFC1                                                     EXT  820
      VOG2 = VOG1                                                       EXT  830
      NI3 = NI3 + 1                                                     EXT  840
      XO1 = ABS(RMX-1.0)                                                EXT  850
      XO2 = ABS(RMN-1.0)                                                EXT  860
      VRGP1 = AMAX1(XO1,XO2)                                            EXT  870
      IEP = 0                                                           EXT  880
      IF(NUAC(18) .GT. 0 .OR. VRGP1 .LT. 0.01) GOTO 114                 EXT  890
      IEP = 1                                                           EXT  900
  114 CONTINUE                                                          EXT  910
      IF(XO1-XO2) 116,116,117                                           EXT  920
  116 VRG1 = RMN - 1.0                                                  EXT  930
      VOG1 = RMX - 1.0                                                  EXT  940
      VVVV = RMN                                                        EXT  950
      GOTO 118                                                          EXT  960
  117 VRG1 = RMX - 1.0                                                  EXT  970
      VOG1 = RMN - 1.0                                                  EXT  980
      VVVV = RMX                                                        EXT  990
  118 IF(VRG2 .EQ. 0.0) GOTO 165                                        EXT 1000
      YYMU = ABS(XXMU)                                                  EXT 1010
      XXMU = (VRG1/VRG2) * (1.0+VRG2)                                   EXT 1020
      XOMU = XXMU                                                       EXT 1030
      IF(VOG2) 119,120,119                                              EXT 1040
  119 XOMU = (VOG1/VOG2) * (1.0+VOG2)                                   EXT 1050
  120 CONTINUE                                                          EXT 1060
      VRGABS = 1.0                                                      EXT 1070
      EXFC1 = 1.0                                                       EXT 1080
      EOFC1 = 1.0                                                       EXT 1090
      T35 = 1.0 - XXMU                                                  EXT 1100
      T36 = 1.0 - XOMU                                                  EXT 1110
      IF(T35) 121,122,121                                               EXT 1120
  121 VRGABS = VRG1 / T35                                               EXT 1130
      EXFC1 = XXMU / T35                                                EXT 1140
  122 IF(T36) 123,124,123                                               EXT 1150
  123 EOFC1 = XOMU / T36                                                EXT 1160
  124 CONTINUE                                                          EXT 1170
      SPARE(31) = XXMU                                                  EXT 1180
      SPARE(32) = XOMU                                                  EXT 1190
      SPARE(33) = VVVV                                                  EXT 1200
      XMUAV = (XXMU+XOMU) * 0.5                                         EXT 1210
      IF(XMUAV .NE. 1.) GOTO 126                                        EXT 1220
      EXFC1 = 1.0                                                       EXT 1230
      GOTO 127                                                          EXT 1240
  126 EXFC1 = XMUAV / (1.0-XMUAV)                                       EXT 1250
  127 SPARE(34) = EXFC1                                                 EXT 1260
      IF(VVVV .EQ. 0.0) GOTO 128                                        EXT 1270
      VRGABS = EXFC1 * (1.0-1.0/VVVV)                                   EXT 1280
  128 CONTINUE                                                          EXT 1290
      IF(VRGP2 .EQ. 0.0) PROUT = 1.0                                    EXT 1300
      IF(VRGP2 .EQ. 0.0) GOTO 129                                       EXT 1310
      PROUT = VRGP1 / VRGP2                                             EXT 1320
  129 CONTINUE                                                          EXT 1330
      IF(ISK .EQ. 0) GOTO 131                                           EXT 1340
      DO 130 N=1,7                                                      EXT 1350
        RP(N) = RP(N+1)                                                 EXT 1360
  130 CONTINUE                                                          EXT 1370
      RP(8) = PROUT                                                     EXT 1380
  131 CONTINUE                                                          EXT 1390
      IF(NIIT-IC10) 132,132,133                                         EXT 1400
  132 IF(ISK .GT. 0) GOTO 141                                           EXT 1410
      IF(IEXR) 165,165,150                                              EXT 1420
  133 IF(ISK .GT. 0) GOTO 134                                           EXT 1430
      NIIIT = NIIIT + 1                                                 EXT 1440
      RP(NIIIT) = PROUT                                                 EXT 1450
  134 CONTINUE                                                          EXT 1460
      IF(NIIT-IC20) 132,135,135                                         EXT 1470
  135 NIIT = 0                                                          EXT 1480
      ISK = 1                                                           EXT 1490
      NIIIT = 0                                                         EXT 1500
      IC10 = 2                                                          EXT 1510
      IC20 = 10                                                         EXT 1520
      IF(ABS(VRG1)-10.0) 139,138,138                                    EXT 1530
  138 BETTA = 0.2 + 0.8 * BETTA                                         EXT 1540
      VRGK2 = BETTA                                                     EXT 1550
      SPARE(39) = BETTA                                                 EXT 1560
      IBCUT = 1                                                         EXT 1570
      ISK = 0                                                           EXT 1580
      GOTO 163                                                          EXT 1590
  139 IF(ABS(VRG1)-2.0) 141,140,140                                     EXT 1600
  140 IF(ABS(XMUAV)-1.0) 141,138,138                                    EXT 1610
  141 CONTINUE                                                          EXT 1620
      IF(NIT .LE. 30) GOTO 142                                          EXT 1630
      IF(ABS(XXMU) .GT. 1.3 .AND. YYMU .GT. 1.3) GOTO 161               EXT 1640
  142 CONTINUE                                                          EXT 1650
      III = 0                                                           EXT 1660
      DO 145 IIII=1,8                                                   EXT 1670
        IF(RP(IIII)-1.0) 145,144,144                                    EXT 1680
  144   III = III + 1                                                   EXT 1690
  145 CONTINUE                                                          EXT 1700
      IF(III-2) 147,148,148                                             EXT 1710
  147 NPOS = 0                                                          EXT 1720
      GOTO 149                                                          EXT 1730
  148 NPOS = 1                                                          EXT 1740
  149 IF(NPOS) 150,150,161                                              EXT 1750
  150 CONTINUE                                                          EXT 1760
      IF(IX(129) .GT. 0) NI3 = 0                                        EXT 1770
      IF(NI3-NA) 165,151,151                                            EXT 1780
  151 IF(ABS(XMUAV)-1.0) 152,165,165                                    EXT 1790
  152 IF(ABS(XXMU)-1.0) 153,165,165                                     EXT 1800
  153 CONTINUE                                                          EXT 1810
      IF(EXFC2 .EQ. 0) GOTO 155                                         EXT 1820
      IF(ABS(EXFC1/EXFC2-1.0)-0.05) 154,154,155                         EXT 1830
  154 CONTINUE                                                          EXT 1840
      IF(NIT .LT. NUAC(22)-1) GOTO 155                                  EXT 1850
      IF(IX135 .EQ. 1) GOTO 156                                         EXT 1860
      IX(135) = 1                                                       EXT 1870
  155 IEXR = 1                                                          EXT 1880
      GOTO 165                                                          EXT 1890
  156 CONTINUE                                                          EXT 1900
      IF((EXFC1*SPARE(35)) .LT. 0.0) GOTO 155                           EXT 1910
      IEXR = 0                                                          EXT 1920
      NIIT = 0                                                          EXT 1930
      ISK = 0                                                           EXT 1940
      NIIIT = 0                                                         EXT 1950
      NI3 = 0                                                           EXT 1960
      VOGP1 = ABS(RMN-1.0)                                              EXT 1970
      P98T = EXFC1                                                      EXT 1980
      IF(NUAC(18) .GT. 0) GOTO 157                                      EXT 1990
      IF(VOGP1 .EQ. 0.0) GOTO 157                                       EXT 2000
C                                                                       EXT 2010
C     SEQUENCE                                                          EXT 2020
C                                                                       EXT 2030
      O98 = 0.9                                                         EXT 2040
      P98T = O98 * (1.0-VOGP1) / VOGP1                                  EXT 2050
  157 CONTINUE                                                          EXT 2060
      T1 = NUAC(21)                                                     EXT 2070
      IF(T1 .EQ. 0.0) T1 = 75.0                                         EXT 2080
      EXFC1 = AMIN1(EXFC1,P98T,T1)                                      EXT 2090
      IX(32) = 1                                                        EXT 2100
      IC10 = 12                                                         EXT 2110
      IC20 = 20                                                         EXT 2120
      NI3 = 0                                                           EXT 2130
      GOTO 165                                                          EXT 2140
  161 CONTINUE                                                          EXT 2150
      BETTA = (6.0*BETTA+1.0) / 7.0                                     EXT 2160
      VRGK2 = BETTA                                                     EXT 2170
      SPARE(39) = BETTA                                                 EXT 2180
      IBCUT = 1                                                         EXT 2190
      ISK = 0                                                           EXT 2200
  163 CONTINUE                                                          EXT 2210
      IX(33) = 1                                                        EXT 2220
      NI3 = 0                                                           EXT 2230
  165 CONTINUE                                                          EXT 2240
      IX(197) = IBCUT                                                   EXT 2250
      IX(160) = ISK                                                     EXT 2260
      IX(161) = IEXR                                                    EXT 2270
      IX(162) = NI3                                                     EXT 2280
      IX(163) = IC10                                                    EXT 2290
      IX(164) = IC20                                                    EXT 2300
      SPARE(160) = VOG1                                                 EXT 2310
      DO 166 I=1,10                                                     EXT 2320
        I1 = 160 + I                                                    EXT 2330
        SPARE(I1) = RP(I)                                               EXT 2340
  166 CONTINUE                                                          EXT 2350
      RETURN                                                            EXT 2360
      END                                                               EXT 2370
      SUBROUTINE FASP(SS1,SIG,CONC,NJJR,PVOL,NCOMP,B1,XLAMDA,KVX,NVX,MVXFAS   10
     1 ,LVX,NSETVX)                                                     FAS   20
C                                                                       FAS   30
CFASP --084 ***CITATION*** CALC. ABS. IN SEARCH NUCLIDES /CF-FLUX,KLUX  FAS   40
C                                                                       FAS   50
      REAL*8 B1                                                         FAS   60
C                                                                       FAS   70
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,FAS   80
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   FAS   90
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), FAS  100
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    FAS  110
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    FAS  120
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   FAS  130
     6 IXPUT(9999),XPUT(9999)                                           FAS  140
C                                                                       FAS  150
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           FAS  160
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    FAS  170
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),FAS  180
     3 NCLASS(9999),NDP(9999)                                           FAS  190
C                                                                       FAS  200
      DIMENSION SS1(KVX,NVX,NSETVX),SIG(KVX,MVX,10),CONC(NVX,MVX),      FAS  210
     1 NJJR(NVX,NSETVX),PVOL(LVX),NCOMP(LVX),B1(MVX,KVX)                FAS  220
C                                                                       FAS  230
C                                                                       FAS  240
      XWACH = SPARE(66)                                                 FAS  250
      T11 = 1.0                                                         FAS  260
      IF(IX(5) .EQ. 2) GOTO 101                                         FAS  270
      IF(IX(5) .EQ. 1) GOTO 100                                         FAS  280
      IF(IX(5) .LT. 0) GOTO 119                                         FAS  290
      STOP 2468                                                         FAS  300
  100 IF(IX(128) .LE. 0) GOTO 116                                       FAS  310
  101 CONTINUE                                                          FAS  320
      DO 103 M=1,MMAX                                                   FAS  330
        DO 102 K=1,KMAX                                                 FAS  340
          SIG(K,M,5) = 0.0                                              FAS  350
  102   CONTINUE                                                        FAS  360
  103 CONTINUE                                                          FAS  370
      IF(NSRH(10) .EQ. -1) GOTO 111                                     FAS  380
      DO 110 M=1,MMAX                                                   FAS  390
        NS = NXSET(M)                                                   FAS  400
        NR = NXODR(NS)                                                  FAS  410
        NSX = NSIG2(NR)                                                 FAS  420
        IF(IX(44) .EQ. 0 .AND. IX(49) .EQ. 0) GOTO 105                  FAS  430
        IF(IX(49) .GT. 0) GOTO 104                                      FAS  440
        IF(M .EQ. IX(44) .OR. M .EQ. IX(45) .OR. M .EQ. IX(46) .OR. M   FAS  450
     1   .EQ. IX(47) .OR. M .EQ. IX(48)) GOTO 105                       FAS  460
        GOTO 109                                                        FAS  470
  104   IF(IX(49) .NE. NCLASS(M)) GOTO 109                              FAS  480
  105   CONTINUE                                                        FAS  490
        DO 108 N=1,NSX                                                  FAS  500
          DO 107 INI=12,18                                              FAS  510
            IF(NJJR(N,NR) .NE. NSRH(INI)) GOTO 107                      FAS  520
            DO 106 K=1,KMAX                                             FAS  530
              SIG(K,M,5) = SIG(K,M,5) + CONC(N,M) * SS1(K,N,NR)         FAS  540
  106       CONTINUE                                                    FAS  550
  107     CONTINUE                                                      FAS  560
  108   CONTINUE                                                        FAS  570
  109   CONTINUE                                                        FAS  580
  110 CONTINUE                                                          FAS  590
      GOTO 115                                                          FAS  600
  111 CONTINUE                                                          FAS  610
      IO18 = IX(85)                                                     FAS  620
      REWIND IO18                                                       FAS  630
      READ (IO18) NXZ                                                   FAS  640
      READ (IO18) (NXTR1(I),I=1,NXZ)                                    FAS  650
      DO 114 I=1,NXZ                                                    FAS  660
        M = NXTR1(I)                                                    FAS  670
        NS = NXSET(M)                                                   FAS  680
        NR = NXODR(NS)                                                  FAS  690
        NSX = NSIG2(NR)                                                 FAS  700
        READ (IO18) (XTR1(N),N=1,NSX)                                   FAS  710
        DO 113 N=1,NSX                                                  FAS  720
          IF(XTR1(N) .EQ. 0.0) GOTO 113                                 FAS  730
          IF(NSRH(23) .GT. 0 .AND. XTR1(N) .LT. 0.0) GOTO 113           FAS  740
          DO 112 K=1,KMAX                                               FAS  750
            SIG(K,M,5) = SIG(K,M,5) + CONC(N,M) * SS1(K,N,NR)           FAS  760
  112     CONTINUE                                                      FAS  770
  113   CONTINUE                                                        FAS  780
  114 CONTINUE                                                          FAS  790
      REWIND IO18                                                       FAS  800
  115 CONTINUE                                                          FAS  810
  116 CONTINUE                                                          FAS  820
      SPARE(48) = 0.0                                                   FAS  830
      DO 118 K=1,KMAX                                                   FAS  840
        DO 117 M=1,MMAX                                                 FAS  850
          SPARE(48) = SPARE(48) + B1(M,K) * SIG(K,M,5)                  FAS  860
  117   CONTINUE                                                        FAS  870
  118 CONTINUE                                                          FAS  880
      GOTO 120                                                          FAS  890
  119 T11 = XLAMDA                                                      FAS  900
  120 T1 = T11 * SPARE(48) / SPARE(56)                                  FAS  910
      SPARE(52) = T1                                                    FAS  920
      IF(IX(5) .EQ. 2) GOTO 122                                         FAS  930
      WRITE (IOUT,1000) XWACH                                           FAS  940
      IF(IX(5) .NE. 1) GOTO 121                                         FAS  950
      IF(IX(128) .LE. 0) WRITE (IOUT,1001) SPARE(29),T1                 FAS  960
      IF(IX(128) .GT. 0) WRITE (IOUT,1004) SPARE(29),T1                 FAS  970
      GOTO 123                                                          FAS  980
  121 CONTINUE                                                          FAS  990
      IF(IX(5) .EQ. -1) WRITE (IOUT,1002) XLAMDA,T1                     FAS 1000
      IF(IX(5) .EQ. -2) WRITE (IOUT,1003) XLAMDA,T1                     FAS 1010
      GOTO 123                                                          FAS 1020
  122 CONTINUE                                                          FAS 1030
      WRITE (IOUT,1005) T1                                              FAS 1040
  123 CONTINUE                                                          FAS 1050
      RETURN                                                            FAS 1060
C                                                                       FAS 1070
 1000 FORMAT (1H0,'END OF CRITICALITY SEARCH - ITERATION TIME',0PF7.3,  FAS 1080
     1 ' MINUTES')                                                      FAS 1090
 1001 FORMAT (1H0,'SEARCH NUCLIDES WERE MULTIPLIED BY',1PE13.5,', FRACTIFAS 1100
     1ON ABSORPTIONS IN SEARCH NUCLIDES ',0PF10.6)                      FAS 1110
 1002 FORMAT (1H0,'DENSITY OF 1/V CROSS SECTION IS',1PE13.5,', FRACTION FAS 1120
     1ABSORPTIONS IN SEARCH PARAMETER',0PF10.6)                         FAS 1130
 1003 FORMAT (1H0,'BUCKLINGS WERE MULTIPLIED BY',1PE13.5,', FRACTION ABSFAS 1140
     1ORPTIONS IN SEARCH PARAMETER',0PF10.6)                            FAS 1150
 1004 FORMAT (1H0,'INPUT RELATIVE CONCENTRATION CHANGE TIMES ',1PE13.5, FAS 1160
     1 ' HAS BEEN ADDED TO THE INITIAL SEARCH CONCENTRATIONS'/1H0,'FRACTFAS 1170
     2ION ABSORPTION IN SEARCH NUCLIDES ',0PF10.6)                      FAS 1180
 1005 FORMAT (1H0,'FRACTION ABSORPTION IN SEARCH NUCLIDES ',0PF10.6)    FAS 1190
      END                                                               FAS 1200
      SUBROUTINE BEGN(XL,P2,E1,XLAMDA,IVX,JVX,KVX,LVX)                  BEG   10
C                                                                       BEG   20
CBEGN --102 ***CITATION*** INITILIZE 1,2-D FLUX CALC/ CF-FLUX           BEG   30
C                                                                       BEG   40
      REAL*8 P2,XLAMDA                                                  BEG   50
C                                                                       BEG   60
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,BEG   70
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   BEG   80
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), BEG   90
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    BEG  100
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    BEG  110
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   BEG  120
     6 IXPUT(9999),XPUT(9999)                                           BEG  130
C                                                                       BEG  140
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   BEG  150
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKBEG  160
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    BEG  170
     3 VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  BEG  180
     4 ITMAX,ITIME,BET(211),DEL(211)                                    BEG  190
C                                                                       BEG  200
      COMMON /BLOCK1/ IDUM(17),JTPE3                                    BEG  210
C                                                                       BEG  220
      COMMON /MU/ MU4                                                   BEG  230
C                                                                       BEG  240
      EQUIVALENCE(JTPE3,NT)                                             BEG  250
C                                                                       BEG  260
      DIMENSION XL(6,KVX),P2(JVX,IVX,KVX),E1(LVX,KVX)                   BEG  270
C                                                                       BEG  280
C                                                                       BEG  290
      NTITE = IX(198)                                                   BEG  300
      IF(IX(172) .GT. 0) GOTO 101                                       BEG  310
      IF(NTITE .NE. 0) GOTO 100                                         BEG  320
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .EQ. 0) GOTO 101                   BEG  330
  100 CONTINUE                                                          BEG  340
      VRGK2 = BETTA                                                     BEG  350
      SPARE(39) = BETTA                                                 BEG  360
  101 CONTINUE                                                          BEG  370
      NOIR = NUAC(23)                                                   BEG  380
      DO 103 K=1,KMAX                                                   BEG  390
        DO 102 L=1,LMAX                                                 BEG  400
          E1(L,K) = 0.0                                                 BEG  410
  102   CONTINUE                                                        BEG  420
  103 CONTINUE                                                          BEG  430
      DO 106 K=1,KMAX                                                   BEG  440
        DO 105 I=1,IMAX                                                 BEG  450
          DO 104 J=1,JMAX                                               BEG  460
            IF(P2(J,I,K) .EQ. 0.0) GOTO 104                             BEG  470
            IX(10) = I                                                  BEG  480
            IX(11) = J                                                  BEG  490
            IX(13) = K                                                  BEG  500
            GOTO 107                                                    BEG  510
  104     CONTINUE                                                      BEG  520
  105   CONTINUE                                                        BEG  530
  106 CONTINUE                                                          BEG  540
  107 CONTINUE                                                          BEG  550
      VRGK1 = 1.0                                                       BEG  560
      IF(IX(24) .EQ. 0) GOTO 108                                        BEG  570
      WRITE (IOUT,1009)                                                 BEG  580
      IF(NUAC(3) .EQ. 0) VRGK1 = 0.0                                    BEG  590
      GOTO 134                                                          BEG  600
  108 CONTINUE                                                          BEG  610
      XLAMDA = XLAMDB                                                   BEG  620
      IF(IX(172) .GT. 0) GOTO 110                                       BEG  630
      IF(NTITE .NE. 0) GOTO 109                                         BEG  640
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .EQ. 0) GOTO 110                   BEG  650
  109 CONTINUE                                                          BEG  660
      XLAMDA = 1.0                                                      BEG  670
  110 CONTINUE                                                          BEG  680
      IF(IX(5) .GE. 2) XLAMDA = 1.0 / XSRH1(1)                          BEG  690
C                                                                       BEG  700
C********SEARCH OPTIONS                                                 BEG  710
C                                                                       BEG  720
      IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 123                       BEG  730
      IF(IX(5) .EQ. -5) GOTO 123                                        BEG  740
      IF(IX(172) .GT. 0) GOTO 112                                       BEG  750
      IF(NTITE .NE. 0) GOTO 111                                         BEG  760
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .EQ. 0) GOTO 112                   BEG  770
  111 CONTINUE                                                          BEG  780
      XLAMDA = 1.0                                                      BEG  790
      XKEF1 = 1.0                                                       BEG  800
      SPARE(45) = 1.0                                                   BEG  810
      SPARE(46) = 0.1                                                   BEG  820
      IF(IX(5) .EQ. -1 .OR. IX(5) .EQ. 1) XLAMDA = 0.0                  BEG  830
  112 CONTINUE                                                          BEG  840
      IF(NGC(1) .GT. 0 .AND. IX(2) .GT. 1) GOTO 119                     BEG  850
      IF(MU4 .EQ. 1) WRITE (IOUT,1007) (TITL1(I),I=1,18)                BEG  860
      IF(MU4 .EQ. 1) WRITE (IOUT,1008) (TITL2(I),I=1,18)                BEG  870
      IF(NGC(1) .EQ. 0) GOTO 116                                        BEG  880
      IF(IX(2) .GT. 0) GOTO 113                                         BEG  890
      WRITE (IOUT,1000) IX(3),SPARE(12)                                 BEG  900
      GOTO 116                                                          BEG  910
  113 CONTINUE                                                          BEG  920
      IF(IX(99) .EQ. 0) GOTO 114                                        BEG  930
      WRITE (IOUT,1001) IX(3),SPARE(12)                                 BEG  940
      GOTO 115                                                          BEG  950
  114 CONTINUE                                                          BEG  960
      WRITE (IOUT,1006) IX(3),SPARE(2),SPARE(12)                        BEG  970
  115 CONTINUE                                                          BEG  980
  116 CONTINUE                                                          BEG  990
      IF(NUAC(20) .LE. 0) GOTO 117                                      BEG 1000
      WRITE (IOUT,1010) NOIR                                            BEG 1010
      GOTO 118                                                          BEG 1020
  117 CONTINUE                                                          BEG 1030
      WRITE (NT,1011) NOIR                                              BEG 1040
  118 CONTINUE                                                          BEG 1050
      WRITE (IOUT,1005) SPARE(51),SPARE(57)                             BEG 1060
      GOTO 122                                                          BEG 1070
  119 CONTINUE                                                          BEG 1080
      IF(IX(99) .EQ. 0) GOTO 120                                        BEG 1090
      WRITE (IOUT,1001) IX(3),SPARE(12)                                 BEG 1100
      GOTO 121                                                          BEG 1110
  120 CONTINUE                                                          BEG 1120
      WRITE (IOUT,1006) IX(3),SPARE(2),SPARE(12)                        BEG 1130
  121 CONTINUE                                                          BEG 1140
  122 WRITE (IOUT,1012)                                                 BEG 1150
      GOTO 136                                                          BEG 1160
  123 CONTINUE                                                          BEG 1170
      IF(IX(172) .GT. 0) GOTO 125                                       BEG 1180
      IF(NTITE .NE. 0) GOTO 124                                         BEG 1190
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .EQ. 0) GOTO 125                   BEG 1200
  124 CONTINUE                                                          BEG 1210
      XKEF1 = 1.0                                                       BEG 1220
  125 CONTINUE                                                          BEG 1230
      IF(XLAMDA .NE. 0.0) XKEF1 = 1.0 / XLAMDA                          BEG 1240
      IF(NGC(1) .GT. 0 .AND. IX(2) .GT. 1) GOTO 131                     BEG 1250
      IF(MU4 .EQ. 1) WRITE (IOUT,1007) (TITL1(I),I=1,18)                BEG 1260
      IF(MU4 .EQ. 1) WRITE (IOUT,1008) (TITL2(I),I=1,18)                BEG 1270
      IF(NGC(1) .EQ. 0) GOTO 129                                        BEG 1280
      IF(IX(2) .GT. 0) GOTO 126                                         BEG 1290
      WRITE (IOUT,1000) IX(3),SPARE(12)                                 BEG 1300
      GOTO 129                                                          BEG 1310
  126 CONTINUE                                                          BEG 1320
      IF(IX(99) .EQ. 0) GOTO 127                                        BEG 1330
      WRITE (IOUT,1001) IX(3),SPARE(12)                                 BEG 1340
      GOTO 128                                                          BEG 1350
  127 CONTINUE                                                          BEG 1360
      WRITE (IOUT,1006) IX(3),SPARE(2),SPARE(12)                        BEG 1370
  128 CONTINUE                                                          BEG 1380
  129 CONTINUE                                                          BEG 1390
      IF(IX(5) .EQ. -5) WRITE (IOUT,1002)                               BEG 1400
      IF(NUAC(20) .LE. 0) GOTO 130                                      BEG 1410
      WRITE (IOUT,1010) NOIR                                            BEG 1420
      GOTO 134                                                          BEG 1430
  130 CONTINUE                                                          BEG 1440
      WRITE (NT,1011) NOIR                                              BEG 1450
      GOTO 134                                                          BEG 1460
  131 CONTINUE                                                          BEG 1470
      IF(IX(99) .EQ. 0) GOTO 132                                        BEG 1480
      WRITE (IOUT,1001) IX(3),SPARE(12)                                 BEG 1490
      GOTO 133                                                          BEG 1500
  132 CONTINUE                                                          BEG 1510
      WRITE (IOUT,1006) IX(3),SPARE(2),SPARE(12)                        BEG 1520
  133 CONTINUE                                                          BEG 1530
  134 CONTINUE                                                          BEG 1540
      IF(IX(5) .EQ. -5) GOTO 135                                        BEG 1550
      WRITE (NT,1013)                                                   BEG 1560
      GOTO 136                                                          BEG 1570
  135 WRITE (IOUT,1003)                                                 BEG 1580
      XLAMDA = 1.0                                                      BEG 1590
      XKEF1 = 1.0                                                       BEG 1600
      SPARE(46) = 0.1                                                   BEG 1610
  136 CONTINUE                                                          BEG 1620
      IF(NTITE .NE. 0) GOTO 137                                         BEG 1630
      IX(196) = 0                                                       BEG 1640
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .EQ. 0) IX(196) = 1                BEG 1650
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .EQ. 0) GOTO 138                   BEG 1660
  137 CONTINUE                                                          BEG 1670
      KXMN8 = KMAX - IX(29)                                             BEG 1680
      KXMN8 = MAX0(KXMN8,1)                                             BEG 1690
      IEP = 1                                                           BEG 1700
      JXP1 = JMAX + 1                                                   BEG 1710
      IX25 = IX(25)                                                     BEG 1720
      IX28 = IX(28)                                                     BEG 1730
      NIT = 0                                                           BEG 1740
      IRECV = 0                                                         BEG 1750
      NIIT = 0                                                          BEG 1760
      NIIIT = 0                                                         BEG 1770
      LO3 = 0                                                           BEG 1780
      LO4 = 0                                                           BEG 1790
      VRG1 = 1.0                                                        BEG 1800
      VRGP1 = 1.0                                                       BEG 1810
      XKEF2 = 2.0                                                       BEG 1820
  138 CONTINUE                                                          BEG 1830
      IF(IX(5) .EQ. -5) XKEF2 = 1.0                                     BEG 1840
      IF(IX(24) .NE. 0) GOTO 141                                        BEG 1850
      DO 140 K=1,KMAX                                                   BEG 1860
        DO 139 N=1,6                                                    BEG 1870
          XL(N,K) = 0.0                                                 BEG 1880
  139   CONTINUE                                                        BEG 1890
  140 CONTINUE                                                          BEG 1900
  141 CONTINUE                                                          BEG 1910
      RETURN                                                            BEG 1920
C                                                                       BEG 1930
 1000 FORMAT (1H0,'AN INITIALIZATION FLUX - EIGENVALUE PROBLEM FOLLOWS FBEG 1940
     1OR CYCLE',I3,'  TOTAL DEPLETION TIME',F8.2,' DAYS'//)             BEG 1950
 1001 FORMAT (1H0,'A FINAL FLUX - EIGENVALUE PROBLEM FOLLOWS FOR CYCLE',BEG 1960
     1 I3,'  TOTAL DEPLETION TIME',F11.3,' DAYS')                       BEG 1970
 1002 FORMAT (1H0,'A FIXED SOURCE PROBLEM FOLLOWS')                     BEG 1980
 1003 FORMAT (1H ,'ITERATION  FLUX CHANGE',4X,'BETA',6X,'MU-1',6X,'MU-2'BEG 1990
     1 , 6X,'MU-3',4X,'DRIVE FACTOR',7X,'BALANCE')                      BEG 2000
 1005 FORMAT (1H ,'LIMITING VALUES OF THE SEARCH FACTOR FOR ABSORPTION ABEG 2010
     1ND TOTAL LOSS ARE',2(1PE13.5))                                    BEG 2020
 1006 FORMAT (1H0,'A FLUX - EIGENVALUE PROBLEM FOLLOWS FOR CYCLE',I3,' CBEG 2030
     1YCLE TIME',0PF11.4,' DAYS   TOTAL TIME',F11.4,' DAYS'//)          BEG 2040
 1007 FORMAT (1H1,18A4)                                                 BEG 2050
 1008 FORMAT (1H ,18A4)                                                 BEG 2060
 1009 FORMAT (1H0,'ADJOINT PROBLEM FOLLOWS')                            BEG 2070
 1010 FORMAT (1H ,'LINE RELAXATION WILL BE DONE ON ROWS - ',I3,' INNER IBEG 2080
     1TERATION(S)')                                                     BEG 2090
 1011 FORMAT (1H ,'LINE RELAXATION WILL BE DONE ON ROWS AND COLUMNS - ',BEG 2100
     1 I3,' INNER ITERATION(S)')                                        BEG 2110
 1012 FORMAT (1H ,'ITERATION  FLUX CHANGE    BETA      MU-1      MU-2   BEG 2120
     1   MU-3            K     SEARCH FACTOR')                          BEG 2130
 1013 FORMAT (1H ,'ITERATION  FLUX CHANGE    BETA      MU-1      MU-2   BEG 2140
     1   MU-3            K')                                            BEG 2150
      END                                                               BEG 2160
      SUBROUTINE DASH(P2,SOUR,JVX,IVX,KVX)                              DAS   10
C                                                                       DAS   20
CDASH --104 ***CITATION*** FLUX DRIVING FOR INDIRECT       / CF-FLUX    DAS   30
C                          SEARCH 1,2D                                  DAS   40
C                                                                       DAS   50
      REAL*8 SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2S,XLL1,D8DAS   60
     1 ,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK,B5LK,D1, DAS   70
     2 D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,YRDB,SPR50DAS   80
     3 ,XLAST,ALPHA,X,PN,PNM1,PNP1,PRD,P2,SOUR                          DAS   90
C                                                                       DAS  100
      COMMON /ADUBP/ SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2SDAS  110
     1 ,XLL1,D8,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK, DAS  120
     2 B5LK,D1,D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,  DAS  130
     3 YRDB,SPR50,XLAST                                                 DAS  140
C                                                                       DAS  150
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,DAS  160
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   DAS  170
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), DAS  180
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    DAS  190
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    DAS  200
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   DAS  210
     6 IXPUT(9999),XPUT(9999)                                           DAS  220
C                                                                       DAS  230
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   DAS  240
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XELKDAS  250
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    DAS  260
     3 VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2,BETTA,SAMXI,IX25,IX28,I,J,KB,K,  DAS  270
     4 ITMAX,ITIME,BET(211),DEL(211)                                    DAS  280
C                                                                       DAS  290
      DIMENSION P2(JVX,IVX,KVX),SOUR(JVX,IVX)                           DAS  300
C                                                                       DAS  310
C                                                                       DAS  320
      IF(IX(73) .GT. 1) GOTO 100                                        DAS  330
      NFLXA = IOFLX                                                     DAS  340
      NFLXB = IX(84)                                                    DAS  350
      GOTO 101                                                          DAS  360
  100 CONTINUE                                                          DAS  370
      NFLXA = IX(152)                                                   DAS  380
      NFLXB = IX(153)                                                   DAS  390
      ITEMP = NFLXB                                                     DAS  400
      NFLXB = NFLXA                                                     DAS  410
      NFLXA = ITEMP                                                     DAS  420
  101 CONTINUE                                                          DAS  430
      REWIND NFLXA                                                      DAS  440
      WRITE (NFLXA) XPDB,XKEF1                                          DAS  450
      DO 102 K=1,KMAX                                                   DAS  460
        WRITE (NFLXA) ((P2(J,I,K),J=1,JMAX),I=1,IMAX)                   DAS  470
  102 CONTINUE                                                          DAS  480
      REWIND NFLXA                                                      DAS  490
      IF(IX(73) .LE. 1) GOTO 108                                        DAS  500
      REWIND NFLXB                                                      DAS  510
      READ (NFLXB) PRD,XK                                               DAS  520
      X = XPDB * XK / (PRD*XKEF1)                                       DAS  530
      ALPHA = -X * (XSRH1(1)-XKEF1) / (XSRH1(1)-XK)                     DAS  540
      IF(ALPHA .GE. -0.5) GOTO 104                                      DAS  550
      ALPHA = -0.5                                                      DAS  560
  104 CONTINUE                                                          DAS  570
      DO 107 K=1,KMAX                                                   DAS  580
        READ (NFLXB) ((SOUR(J,I),J=1,JMAX),I=1,IMAX)                    DAS  590
        DO 106 I=1,IMAX                                                 DAS  600
          DO 105 J=1,JMAX                                               DAS  610
            PN = P2(J,I,K)                                              DAS  620
            IF(PN .LE. 0.0) GOTO 105                                    DAS  630
            PNP1 = PN + ALPHA * SOUR(J,I)                               DAS  640
            IF(PNP1 .LE. 0.0) PNP1 = 1.0D-38                            DAS  650
            P2(J,I,K) = PNP1                                            DAS  660
  105     CONTINUE                                                      DAS  670
  106   CONTINUE                                                        DAS  680
  107 CONTINUE                                                          DAS  690
      REWIND NFLXB                                                      DAS  700
      IF(NSRH(24) .LT. 0) WRITE (IOUT,1000) ALPHA,NFLXA,NFLXB           DAS  710
  108 CONTINUE                                                          DAS  720
      IX(152) = NFLXA                                                   DAS  730
      IX(153) = NFLXB                                                   DAS  740
      IF(IX(130) .GT. 0 .AND. IX(73) .EQ. 1) IX(73) = IX(73) + 1        DAS  750
      RETURN                                                            DAS  760
C                                                                       DAS  770
 1000 FORMAT (' DB20',' ALPHA=',1PE15.6,2I10)                           DAS  780
      END                                                               DAS  790
      SUBROUTINE KEGN(XL,P2E,E1,XLAMDA,IVX,JVX,KBVX,KVX,LVX,JIVX)       KEG   10
C                                                                       KEG   20
CKEGN --118 ***CITATION*** INITILIZE 3-D FLUX CALC/ CF-KLUX             KEG   30
C                                                                       KEG   40
      REAL*8 XLAMDA                                                     KEG   50
C                                                                       KEG   60
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KEG   70
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KEG   80
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KEG   90
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KEG  100
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KEG  110
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KEG  120
     6 IXPUT(9999),XPUT(9999)                                           KEG  130
C                                                                       KEG  140
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   KEG  150
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKKEG  160
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    KEG  170
     3 VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  KEG  180
     4 ITMAX,ITIME,BET(211),DEL(211)                                    KEG  190
C                                                                       KEG  200
      COMMON /BLOCK1/ IDUM(17),JTPE3                                    KEG  210
C                                                                       KEG  220
      COMMON /MU/ MU4                                                   KEG  230
C                                                                       KEG  240
      EQUIVALENCE(JTPE3,NT)                                             KEG  250
C                                                                       KEG  260
      DIMENSION P2E(JIVX,KBVX,KVX),E1(LVX,KVX),XL(6,KVX)                KEG  270
C                                                                       KEG  280
C                                                                       KEG  290
      NOIR = NUAC(23)                                                   KEG  300
      NTITE = IX(198)                                                   KEG  310
      IF(IX(172) .GT. 0) GOTO 101                                       KEG  320
      IF(NTITE .NE. 0) GOTO 100                                         KEG  330
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .EQ. 0) GOTO 101                   KEG  340
  100 CONTINUE                                                          KEG  350
      VRGK2 = BETTA                                                     KEG  360
      SPARE(39) = BETTA                                                 KEG  370
  101 CONTINUE                                                          KEG  380
      DO 103 K=1,KMAX                                                   KEG  390
        DO 102 L=1,LMAX                                                 KEG  400
          E1(L,K) = 0.0                                                 KEG  410
  102   CONTINUE                                                        KEG  420
  103 CONTINUE                                                          KEG  430
      DO 107 K=1,KMAX                                                   KEG  440
        DO 106 KB=1,KBMAX                                               KEG  450
          N1 = 0                                                        KEG  460
          DO 105 I=1,IMAX                                               KEG  470
            DO 104 J=1,JMAX                                             KEG  480
              N1 = N1 + 1                                               KEG  490
              IF(P2E(N1,KB,K) .EQ. 0.0) GOTO 104                        KEG  500
              IX(10) = I                                                KEG  510
              IX(11) = J                                                KEG  520
              IX(12) = KB                                               KEG  530
              IX(13) = K                                                KEG  540
              GOTO 108                                                  KEG  550
  104       CONTINUE                                                    KEG  560
  105     CONTINUE                                                      KEG  570
  106   CONTINUE                                                        KEG  580
  107 CONTINUE                                                          KEG  590
  108 CONTINUE                                                          KEG  600
      VRGK1 = 1.0                                                       KEG  610
      IF(IX(24) .EQ. 0) GOTO 109                                        KEG  620
      WRITE (IOUT,1007)                                                 KEG  630
      IF(NUAC(3) .EQ. 0) VRGK1 = 0.0                                    KEG  640
      GOTO 139                                                          KEG  650
  109 CONTINUE                                                          KEG  660
      XLAMDA = XLAMDB                                                   KEG  670
      IF(IX(172) .GT. 0) GOTO 111                                       KEG  680
      IF(NTITE .NE. 0) GOTO 110                                         KEG  690
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .EQ. 0) GOTO 111                   KEG  700
  110 CONTINUE                                                          KEG  710
      XLAMDA = 1.0                                                      KEG  720
  111 CONTINUE                                                          KEG  730
      IF(IX(5) .GE. 2) XLAMDA = 1.0 / XSRH1(1)                          KEG  740
C                                                                       KEG  750
C********SEARCH OPTIONS                                                 KEG  760
C                                                                       KEG  770
      IF(IX(5) .EQ. -5) GOTO 126                                        KEG  780
      IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 126                       KEG  790
      IF(IX(172) .GT. 0) GOTO 113                                       KEG  800
      IF(NTITE .NE. 0) GOTO 112                                         KEG  810
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .EQ. 0) GOTO 113                   KEG  820
  112 CONTINUE                                                          KEG  830
      XLAMDA = 1.0                                                      KEG  840
      XKEF1 = 1.0                                                       KEG  850
      SPARE(45) = 1.0                                                   KEG  860
      SPARE(46) = 0.1                                                   KEG  870
      IF(IX(5) .EQ. -1 .OR. IX(5) .EQ. 1) XLAMDA = 0.0                  KEG  880
  113 CONTINUE                                                          KEG  890
      IF(NGC(1) .GT. 0 .AND. IX(2) .GT. 1) GOTO 122                     KEG  900
      IF(MU4 .EQ. 1) WRITE (IOUT,1005) (TITL1(I),I=1,18)                KEG  910
      IF(MU4 .EQ. 1) WRITE (IOUT,1006) (TITL2(I),I=1,18)                KEG  920
      IF(NGC(1) .EQ. 0) GOTO 117                                        KEG  930
      IF(IX(2) .GT. 0) GOTO 114                                         KEG  940
      WRITE (IOUT,1000) IX(3),SPARE(12)                                 KEG  950
      GOTO 117                                                          KEG  960
  114 CONTINUE                                                          KEG  970
      IF(IX(99) .EQ. 0) GOTO 115                                        KEG  980
      WRITE (IOUT,1001) IX(3),SPARE(12)                                 KEG  990
      GOTO 116                                                          KEG 1000
  115 CONTINUE                                                          KEG 1010
      WRITE (IOUT,1013) IX(3),SPARE(2),SPARE(12)                        KEG 1020
  116 CONTINUE                                                          KEG 1030
  117 CONTINUE                                                          KEG 1040
      IF(NUAC(5) .EQ. 13) GOTO 118                                      KEG 1050
      IF(NUAC(20)+1) 120,119,118                                        KEG 1060
  118 WRITE (IOUT,1008) NOIR                                            KEG 1070
      GOTO 121                                                          KEG 1080
  119 WRITE (IOUT,1010) NOIR                                            KEG 1090
      GOTO 121                                                          KEG 1100
  120 WRITE (IOUT,1009) NOIR                                            KEG 1110
  121 CONTINUE                                                          KEG 1120
      WRITE (IOUT,1004) SPARE(51),SPARE(57)                             KEG 1130
      GOTO 125                                                          KEG 1140
  122 CONTINUE                                                          KEG 1150
      IF(IX(99) .EQ. 0) GOTO 123                                        KEG 1160
      WRITE (IOUT,1001) IX(3),SPARE(12)                                 KEG 1170
      GOTO 124                                                          KEG 1180
  123 CONTINUE                                                          KEG 1190
      WRITE (IOUT,1013) IX(3),SPARE(2),SPARE(12)                        KEG 1200
  124 CONTINUE                                                          KEG 1210
  125 WRITE (IOUT,1011)                                                 KEG 1220
      GOTO 141                                                          KEG 1230
  126 CONTINUE                                                          KEG 1240
      IF(IX(172) .GT. 0) GOTO 128                                       KEG 1250
      IF(NTITE .NE. 0) GOTO 127                                         KEG 1260
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .EQ. 0) GOTO 128                   KEG 1270
  127 CONTINUE                                                          KEG 1280
      XKEF1 = 1.0                                                       KEG 1290
      IF(XLAMDA .NE. 0.0) XKEF1 = 1.0 / XLAMDA                          KEG 1300
  128 CONTINUE                                                          KEG 1310
      IF(NGC(1) .GT. 0 .AND. IX(2) .GT. 1) GOTO 136                     KEG 1320
      IF(MU4 .EQ. 1) WRITE (IOUT,1005) (TITL1(I),I=1,18)                KEG 1330
      IF(MU4 .EQ. 1) WRITE (IOUT,1006) (TITL2(I),I=1,18)                KEG 1340
      IF(NGC(1) .EQ. 0) GOTO 132                                        KEG 1350
      IF(IX(2) .GT. 0) GOTO 129                                         KEG 1360
      WRITE (IOUT,1000) IX(3),SPARE(12)                                 KEG 1370
      GOTO 132                                                          KEG 1380
  129 CONTINUE                                                          KEG 1390
      IF(IX(99) .EQ. 0) GOTO 130                                        KEG 1400
      WRITE (IOUT,1001) IX(3),SPARE(12)                                 KEG 1410
      GOTO 131                                                          KEG 1420
  130 CONTINUE                                                          KEG 1430
      WRITE (IOUT,1013) IX(3),SPARE(2),SPARE(12)                        KEG 1440
  131 CONTINUE                                                          KEG 1450
  132 CONTINUE                                                          KEG 1460
      IF(IX(5) .EQ. -5) WRITE (IOUT,1002)                               KEG 1470
      IF(NUAC(5) .EQ. 13) GOTO 133                                      KEG 1480
      IF(NUAC(20)+1) 135,134,133                                        KEG 1490
  133 WRITE (IOUT,1008) NOIR                                            KEG 1500
      GOTO 139                                                          KEG 1510
  134 WRITE (NT,1010) NOIR                                              KEG 1520
      GOTO 139                                                          KEG 1530
  135 WRITE (IOUT,1009) NOIR                                            KEG 1540
      GOTO 139                                                          KEG 1550
  136 CONTINUE                                                          KEG 1560
      IF(IX(99) .EQ. 0) GOTO 137                                        KEG 1570
      WRITE (IOUT,1001) IX(3),SPARE(12)                                 KEG 1580
      GOTO 138                                                          KEG 1590
  137 CONTINUE                                                          KEG 1600
      WRITE (IOUT,1013) IX(3),SPARE(2),SPARE(12)                        KEG 1610
  138 CONTINUE                                                          KEG 1620
  139 CONTINUE                                                          KEG 1630
      IF(IX(5) .EQ. -5) GOTO 140                                        KEG 1640
      WRITE (NT,1012)                                                   KEG 1650
      GOTO 141                                                          KEG 1660
  140 WRITE (IOUT,1003)                                                 KEG 1670
      XLAMDA = 1.0                                                      KEG 1680
      XKEF1 = 1.0                                                       KEG 1690
      SPARE(46) = 0.1                                                   KEG 1700
  141 CONTINUE                                                          KEG 1710
      IF(NTITE .NE. 0) GOTO 142                                         KEG 1720
      IX(196) = 0                                                       KEG 1730
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .EQ. 0) IX(196) = 1                KEG 1740
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .EQ. 0) GOTO 143                   KEG 1750
  142 CONTINUE                                                          KEG 1760
      KXMN8 = KMAX - IX(29)                                             KEG 1770
      KXMN8 = MAX0(KXMN8,1)                                             KEG 1780
      IEP = 1                                                           KEG 1790
      JXP1 = JMAX+1                                                     KEG 1800
      IX25 = IX(25)                                                     KEG 1810
      IX28 = IX(28)                                                     KEG 1820
      NIT = 0                                                           KEG 1830
      IRECV = 0                                                         KEG 1840
      NIIT = 0                                                          KEG 1850
      NIIIT = 0                                                         KEG 1860
      LO3 = 0                                                           KEG 1870
      LO4 = 0                                                           KEG 1880
      VRG1 = 1.0                                                        KEG 1890
      VRGP1 = 1.0                                                       KEG 1900
      XKEF2 = 2.0                                                       KEG 1910
  143 CONTINUE                                                          KEG 1920
      IF(IX(5) .EQ. -5) XKEF2 = 1.0                                     KEG 1930
      IF(IX(24) .NE. 0) GOTO 146                                        KEG 1940
      DO 145 K=1,KMAX                                                   KEG 1950
        DO 144 N=1,6                                                    KEG 1960
          XL(N,K) = 0.0                                                 KEG 1970
  144   CONTINUE                                                        KEG 1980
  145 CONTINUE                                                          KEG 1990
  146 CONTINUE                                                          KEG 2000
      RETURN                                                            KEG 2010
C                                                                       KEG 2020
 1000 FORMAT (1H0,'AN INITIALIZATION FLUX - EIGENVALUE PROBLEM FOLLOWS FKEG 2030
     1OR CYCLE',I3,'  TOTAL DEPLETION TIME',F8.2,' DAYS'//)             KEG 2040
 1001 FORMAT (1H0,'A FINAL FLUX - EIGENVALUE PROBLEM FOLLOWS FOR CYCLE',KEG 2050
     1 I3,'  TOTAL DEPLETION TIME',F11.3,' DAYS')                       KEG 2060
 1002 FORMAT (1H0,'A FIXED SOURCE PROBLEM FOLLOWS')                     KEG 2070
 1003 FORMAT (1H ,'ITERATION  FLUX CHANGE',4X,'BETA',6X,'MU-1',6X,'MU-2'KEG 2080
     1 , 6X,'MU-3',4X,'DRIVE FACTOR',7X,'BALANCE')                      KEG 2090
 1004 FORMAT (1H ,'LIMITING VALUES OF THE SEARCH FACTOR FOR ABSORPTION AKEG 2100
     1ND TOTAL LOSS ARE',2(1PE13.5))                                    KEG 2110
 1005 FORMAT (1H1,18A4)                                                 KEG 2120
 1006 FORMAT (1H ,18A4)                                                 KEG 2130
 1007 FORMAT (1H0,'ADJOINT PROBLEM FOLLOWS')                            KEG 2140
 1008 FORMAT (1H ,'LINE RELAXATION WILL BE DONE ON ROWS - ',I3,' INNER IKEG 2150
     1TERATION(S)')                                                     KEG 2160
 1009 FORMAT (1H ,'LINE RELAXATION WILL BE DONE ON ROWS AND COLUMNS - ',KEG 2170
     1 I3,' INNER ITERATION(S)')                                        KEG 2180
 1010 FORMAT (1H ,'LINE RELAXATION WILL BE DONE ON ROWS,COLUMNS,AND PLANKEG 2190
     1ES - ',I3,' INNER ITERATION(S)')                                  KEG 2200
 1011 FORMAT (1H ,'ITERATION  FLUX CHANGE    BETA      MU-1      MU-2   KEG 2210
     1   MU-3            K     SEARCH FACTOR')                          KEG 2220
 1012 FORMAT (1H ,'ITERATION  FLUX CHANGE    BETA      MU-1      MU-2   KEG 2230
     1   MU-3            K')                                            KEG 2240
 1013 FORMAT (1H0,'A FLUX - EIGENVALUE PROBLEM FOLLOWS FOR CYCLE',I3,' CKEG 2250
     1YCLE TIME',0PF11.4,' DAYS   TOTAL TIME',F11.4,' DAYS'//)          KEG 2260
      END                                                               KEG 2270
      SUBROUTINE KASH(P2E,SOURE,JVX,IVX,KBVX,KVX,JIVX)                  KAS   10
C                                                                       KAS   20
CKASH --120 ***CITATION*** FLUX DRIVING FOR INDIRECT       / CF-KLUX    KAS   30
C                          SEARCH 3 D                                   KAS   40
C                                                                       KAS   50
      REAL*8 SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2S,XLL1,D8KAS   60
     1 ,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK,B5LK,D1, KAS   70
     2 D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,YRDB,SPR50KAS   80
     3 ,XLAST,ALPHA,X,PN,PNM1,PNP1,PRD                                  KAS   90
C                                                                       KAS  100
      COMMON /ADUBP/ SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2SKAS  110
     1 ,XLL1,D8,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK, KAS  120
     2 B5LK,D1,D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,  KAS  130
     3 YRDB,SPR50,XLAST                                                 KAS  140
C                                                                       KAS  150
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KAS  160
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KAS  170
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KAS  180
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KAS  190
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KAS  200
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KAS  210
     6 IXPUT(9999),XPUT(9999)                                           KAS  220
C                                                                       KAS  230
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   KAS  240
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XELKKAS  250
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    KAS  260
     3 VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2,BETTA,SAMXI,IX25,IX28,I,J,KB,K,  KAS  270
     4 ITMAX,ITIME,BET(211),DEL(211)                                    KAS  280
C                                                                       KAS  290
      DIMENSION P2E(JIVX,KBVX,KVX),SOURE(JVX,IVX,KBVX)                  KAS  300
C                                                                       KAS  310
C                                                                       KAS  320
      JIPROD = JMAX * IMAX                                              KAS  330
      IF(IX(73) .GT. 1) GOTO 100                                        KAS  340
      NFLXA = IOFLX                                                     KAS  350
      NFLXB = IX(84)                                                    KAS  360
      GOTO 101                                                          KAS  370
  100 CONTINUE                                                          KAS  380
      NFLXA = IX(152)                                                   KAS  390
      NFLXB = IX(153)                                                   KAS  400
      ITEMP = NFLXB                                                     KAS  410
      NFLXB = NFLXA                                                     KAS  420
      NFLXA = ITEMP                                                     KAS  430
  101 CONTINUE                                                          KAS  440
      REWIND NFLXA                                                      KAS  450
      WRITE (NFLXA) XPDB,XKEF1                                          KAS  460
      DO 102 K=1,KMAX                                                   KAS  470
        WRITE (NFLXA) ((P2E(N1,KB,K),N1=1,JIPROD),KB=1,KBMAX)           KAS  480
  102 CONTINUE                                                          KAS  490
      REWIND NFLXA                                                      KAS  500
      IF(IX(73) .LE. 1) GOTO 109                                        KAS  510
      REWIND NFLXB                                                      KAS  520
      READ (NFLXB) PRD,XK                                               KAS  530
      X = XPDB * XK / (PRD*XKEF1)                                       KAS  540
      ALPHA = -X * (XSRH1(1)-XKEF1) / (XSRH1(1)-XK)                     KAS  550
      IF(ALPHA .GE. -0.5) GOTO 104                                      KAS  560
      ALPHA = -0.5                                                      KAS  570
  104 CONTINUE                                                          KAS  580
      DO 108 K=1,KMAX                                                   KAS  590
        READ (NFLXB) (((SOURE(J,I,KB),J=1,JMAX),I=1,IMAX),KB=1,KBMAX)   KAS  600
        DO 107 KB=1,KBMAX                                               KAS  610
          N1 = 0                                                        KAS  620
          DO 106 I=1,IMAX                                               KAS  630
            DO 105 J=1,JMAX                                             KAS  640
              N1 = N1 + 1                                               KAS  650
              PN = P2E(N1,KB,K)                                         KAS  660
              IF(PN .LE. 0.0) GOTO 105                                  KAS  670
              PNP1 = PN + ALPHA * SOURE(J,I,KB)                         KAS  680
              IF(PNP1 .LE. 0.0) PNP1 = 1.0D-38                          KAS  690
              P2E(N1,KB,K) = PNP1                                       KAS  700
  105       CONTINUE                                                    KAS  710
  106     CONTINUE                                                      KAS  720
  107   CONTINUE                                                        KAS  730
  108 CONTINUE                                                          KAS  740
      REWIND NFLXB                                                      KAS  750
      IF(NSRH(24) .LT. 0) WRITE (IOUT,1000) ALPHA,NFLXA,NFLXB           KAS  760
  109 CONTINUE                                                          KAS  770
      IX(152) = NFLXA                                                   KAS  780
      IX(153) = NFLXB                                                   KAS  790
      IF(IX(130) .GT. 0 .AND. IX(73) .EQ. 1) IX(73) = IX(73) + 1        KAS  800
      RETURN                                                            KAS  810
C                                                                       KAS  820
 1000 FORMAT (' DB20',' ALPHA=',1PE15.6,2I10)                           KAS  830
      END                                                               KAS  840
      SUBROUTINE KRAN(CONC,MVX,NVX,KO3,KO10,HOX,NFO,NNFO,NIC,NSETVX)    KRA   10
C                                                                       KRA   20
CKRAN --131***CITATION***TRANSFERS INPUT DATA FROM KO3 TO KO10/CF-WSTR  KRA   30
C                                                                       KRA   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KRA   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KRA   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KRA   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KRA   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KRA   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KRA  100
     6 IXPUT(9999),XPUT(9999)                                           KRA  110
C                                                                       KRA  120
      DIMENSION HOX(NVX,NSETVX,20),NFO(20,NSETVX),NNFO(20,NSETVX),      KRA  130
     1 NIC(1000),CONC(NVX,MVX)                                          KRA  140
C                                                                       KRA  150
C                                                                       KRA  160
  100 READ (KO3,END=118) INSEC,NREC,NSET,I1,I2                          KRA  170
      WRITE (KO10) INSEC,NREC,NSET,I1,I2                                KRA  180
      IF(INSEC .EQ. 28) GOTO 101                                        KRA  190
      IF(INSEC .EQ. 40) GOTO 108                                        KRA  200
      IF(INSEC .EQ. 36) GOTO 115                                        KRA  210
      IF(INSEC .EQ. 30) GOTO 116                                        KRA  220
C                                                                       KRA  230
C     ADD AN IF FOR EACH NEW SECT. ON IO10                              KRA  240
C                                                                       KRA  250
      GOTO 100                                                          KRA  260
  101 IF(NSET .EQ. 0) GOTO 100                                          KRA  270
      DO 107 N=1,NSET                                                   KRA  280
        READ (KO3) (NSRH(I),I=1,24),(XSRH1(I),I=1,6)                    KRA  290
        WRITE (KO10) (NSRH(I),I=1,24),(XSRH1(I),I=1,6)                  KRA  300
        IF(NSRH(2) .EQ. 3) GOTO 103                                     KRA  310
        IF(NSRH(10) .NE. -1) GOTO 107                                   KRA  320
        READ (KO3) (NXTR1(I),I=1,5),(XTR1(I),I=1,5)                     KRA  330
        WRITE (KO10) (NXTR1(I),I=1,5),(XTR1(I),I=1,5)                   KRA  340
        NXZ = NXTR1(1)                                                  KRA  350
        READ (KO3) (NXTR1(I),I=1,NXZ)                                   KRA  360
        WRITE (KO10) (NXTR1(I),I=1,NXZ)                                 KRA  370
        DO 102 M=1,NXZ                                                  KRA  380
          READ (KO3) (XTR1(I),I=1,I1)                                   KRA  390
          WRITE (KO10) (XTR1(I),I=1,I1)                                 KRA  400
  102   CONTINUE                                                        KRA  410
        GOTO 107                                                        KRA  420
  103   CONTINUE                                                        KRA  430
        READ (KO3) (NXTR1(I),I=1,7),(XTR1(I),I=1,5)                     KRA  440
        WRITE (KO10) (NXTR1(I),I=1,7),(XTR1(I),I=1,5)                   KRA  450
        NXJ = NXTR1(1)                                                  KRA  460
        NXI = NXTR1(2)                                                  KRA  470
        NXKB = NXTR1(3)                                                 KRA  480
        IF(NXJ .LE. 0) GOTO 104                                         KRA  490
        READ (KO3) (XTR1(I),I=1,NXJ)                                    KRA  500
        WRITE (KO10) (XTR1(I),I=1,NXJ)                                  KRA  510
  104   IF(NXI .LE. 0) GOTO 105                                         KRA  520
        READ (KO3) (XTR1(I),I=1,NXI)                                    KRA  530
        WRITE (KO10) (XTR1(I),I=1,NXI)                                  KRA  540
  105   IF(NXKB .LE. 0) GOTO 106                                        KRA  550
        READ (KO3) (XTR1(I),I=1,NXKB)                                   KRA  560
        WRITE (KO10) (XTR1(I),I=1,NXKB)                                 KRA  570
  106   CONTINUE                                                        KRA  580
  107 CONTINUE                                                          KRA  590
      GOTO 100                                                          KRA  600
  108 IF(NREC .EQ. 0) GOTO 110                                          KRA  610
      DO 109 N=1,NREC                                                   KRA  620
        READ (KO3) I1,I2,J1,T1,J2,T2,J3,T3,J4,T4,J5,T5                  KRA  630
        WRITE (KO10) I1,I2,J1,T1,J2,T2,J3,T3,J4,T4,J5,T5                KRA  640
  109 CONTINUE                                                          KRA  650
  110 READ (KO3) INSEC,NREC,KAAX,I1,I2                                  KRA  660
      WRITE (KO10) INSEC,NREC,KAAX,I1,I2                                KRA  670
      IF(NREC .EQ. 0) GOTO 112                                          KRA  680
      DO 111 N=1,NREC                                                   KRA  690
        READ (KO3) (XTR1(K),K=1,KAAX)                                   KRA  700
        WRITE (KO10) (XTR1(K),K=1,KAAX)                                 KRA  710
  111 CONTINUE                                                          KRA  720
  112 READ (KO3) INSEC,NREC,NN,NAAX,MAAX                                KRA  730
      WRITE (KO10) INSEC,NREC,NN,NAAX,MAAX                              KRA  740
      IF(NREC .EQ. 0) GOTO 100                                          KRA  750
      IF(NN .EQ. 0) GOTO 113                                            KRA  760
      READ (KO3) (NXTR1(N),N=1,NN)                                      KRA  770
  113 CONTINUE                                                          KRA  780
      READ (KO3) ((CONC(N,M),N=1,NAAX),M=1,MAAX)                        KRA  790
      IF(NN .EQ. 0) GOTO 114                                            KRA  800
      WRITE (KO10) (NXTR1(N),N=1,NN)                                    KRA  810
  114 CONTINUE                                                          KRA  820
      WRITE (KO10) ((CONC(N,M),N=1,NAAX),M=1,MAAX)                      KRA  830
      GOTO 112                                                          KRA  840
  115 CONTINUE                                                          KRA  850
      READ (KO3) (((HOX(NV,NS,I20),NV=1,NSET),NS=1,I1),I20=1,20)        KRA  860
      READ (KO3) ((NFO(I20,NS),I20=1,20),NS=1,I1)                       KRA  870
      READ (KO3) ((NNFO(I20,NS),I20=1,20),NS=1,I1)                      KRA  880
      READ (KO3) NIC                                                    KRA  890
      WRITE (KO10) (((HOX(NV,NS,I20),NV=1,NSET),NS=1,I1),I20=1,20)      KRA  900
      WRITE (KO10) ((NFO(I20,NS),I20=1,20),NS=1,I1)                     KRA  910
      WRITE (KO10) ((NNFO(I20,NS),I20=1,20),NS=1,I1)                    KRA  920
      WRITE (KO10) NIC                                                  KRA  930
      GOTO 100                                                          KRA  940
  116 READ (KO3) (NXTR1(I),I=1,24)                                      KRA  950
      WRITE (KO10) (NXTR1(I),I=1,24)                                    KRA  960
      READ (KO3) (NXTR1(I),I=1,24)                                      KRA  970
      WRITE (KO10) (NXTR1(I),I=1,24)                                    KRA  980
      DO 117 II=1,NSET                                                  KRA  990
        READ (KO3) J1,J2,J3,(NXTR1(I),I=1,J2)                           KRA 1000
        WRITE (KO10) J1,J2,J3,(NXTR1(I),I=1,J2)                         KRA 1010
  117 CONTINUE                                                          KRA 1020
      GOTO 100                                                          KRA 1030
  118 CONTINUE                                                          KRA 1040
      INSEC = -9999                                                     KRA 1050
      NREC = -9999                                                      KRA 1060
      WRITE (KO10) INSEC,NREC,NREC,NREC,NREC                            KRA 1070
      RETURN                                                            KRA 1080
      END                                                               KRA 1090
      SUBROUTINE GEOM(DND)                                              GEO   10
C                                                                       GEO   20
CGEOM --027 ***CITATION*** READS INPUT SECTION 003/ CF-INPT             GEO   30
C                                                                       GEO   40
      REAL*8 HOLBND                                                     GEO   50
C                                                                       GEO   60
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,GEO   70
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   GEO   80
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), GEO   90
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    GEO  100
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    GEO  110
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   GEO  120
     6 IXPUT(9999),XPUT(9999)                                           GEO  130
C                                                                       GEO  140
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   GEO  150
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKGEO  160
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    GEO  170
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  GEO  180
     4 ITMAX,ITIME,BET(211),DEL(211)                                    GEO  190
C                                                                       GEO  200
CFZJ055                                                       25.09.07  GEO  210
C                                                                       GEO  220
      DIMENSION DND(1),B(6),HOLBND(6)                                   GEO  230
C                                                                       GEO  240
      DATA HOLBND/8HLEFT    ,8HTOP     ,8HRIGHT   ,8HBOTTOM  ,8HFRONT   GEO  250
     1 ,8HBACK    /                                                     GEO  260
C                                                                       GEO  270
C                                                                       GEO  280
      READ (IOIN,1000) (NUAC(I),I=1,24)                                 GEO  290
C                                                                       GEO  300
C     FORCE KEFF TO BE CALCULATED WITH NEUTRON BALANCE INSTEAD          GEO  310
C     OF WITH RESIDUES.                                                 GEO  320
C                                                                       GEO  330
      NUAC(3) = 0                                                       GEO  340
      IF(NUAC(5)-5) 101,101,102                                         GEO  350
  101 NUAC(20) = 1                                                      GEO  360
      NUAC(12) = 1                                                      GEO  370
      NUAC(14) = 1                                                      GEO  380
  102 CONTINUE                                                          GEO  390
      IX(72) = 0                                                        GEO  400
      IF(NUAC(11) .GE. 0) GOTO 103                                      GEO  410
      NUAC(13) = NUAC(11)                                               GEO  420
      IX(72) = 1                                                        GEO  430
      NUAC(8) = 0                                                       GEO  440
  103 CONTINUE                                                          GEO  450
      IF(NUAC(13) .LE. 1) GOTO 104                                      GEO  460
      IX(72) = NUAC(13)                                                 GEO  470
      NUAC(8) = 0                                                       GEO  480
  104 CONTINUE                                                          GEO  490
      IF(NUAC(12) .LT. 0 .OR. NUAC(14) .LT. 0 .OR. NUAC(15) .LT. 0 .OR. GEO  500
     1 NUAC(16) .LT. 0) NER(30) = 30                                    GEO  510
      IF(IX(72) .NE. 1) GOTO 105                                        GEO  520
      N = NUAC(5)                                                       GEO  530
      IF(N .EQ. 2 .OR. N .EQ. 3 .OR. N .EQ. 7 .OR. N .EQ .9 .OR. N .EQ. GEO  540
     1 13) NER(30) = 30                                                 GEO  550
  105 CONTINUE                                                          GEO  560
      INRB = IX(72) + 1                                                 GEO  570
C                                                                       GEO  580
C     CONVERGENCE CRITERIA                                              GEO  590
C                                                                       GEO  600
      READ (IOIN,1001) (EPI(I),I=1,6)                                   GEO  610
      IF(EPI(1) .LE. 0) EPI(1) = 0.0001                                 GEO  620
      IF(EPI(2) .LE. 0) EPI(2) = 0.00001                                GEO  630
      IF(EPI(3) .LE. 0) EPI(3) = 0.0001                                 GEO  640
      IF(EPI(4) .LE. 0) EPI(4) = 0.0001                                 GEO  650
      IF(EPI(5) .LE. 0) EPI(5) = 0.0001                                 GEO  660
      EPI1 = EPI(1)                                                     GEO  670
      EPI2 = EPI(2)                                                     GEO  680
      READ (IOIN,1001) (XMIS(I),I=1,6)                                  GEO  690
      IF(XMIS(3)) 107,106,107                                           GEO  700
  106 XMIS(3) = 1.0                                                     GEO  710
  107 IF(XMIS(4)) 109,108,109                                           GEO  720
  108 XMIS(4) = 1.0                                                     GEO  730
  109 IF(XMIS(5)) 111,110,111                                           GEO  740
  110 XMIS(5) = 1.0                                                     GEO  750
  111 CONTINUE                                                          GEO  760
      SPARE(100) = XMIS(3)                                              GEO  770
      IX(31) = 0                                                        GEO  780
      IF(XMIS(6)) 113,113,112                                           GEO  790
  112 IX(31) = 1                                                        GEO  800
  113 CONTINUE                                                          GEO  810
      WRITE (IOUT,1002)                                                 GEO  820
      WRITE (IOUT,1003) (NUAC(I),I=1,24)                                GEO  830
      WRITE (IOUT,1008) (EPI(I),I=1,6)                                  GEO  840
      WRITE (IOUT,1008) (XMIS(I),I=1,6)                                 GEO  850
      IX(26) = NUAC(5)                                                  GEO  860
      IF(NUAC(5) .GT. 5) GOTO 114                                       GEO  870
      IX(25) = 1                                                        GEO  880
      GOTO 116                                                          GEO  890
  114 IF(NUAC(5) .GT. 10) GOTO 115                                      GEO  900
      IX(25) = 2                                                        GEO  910
      GOTO 116                                                          GEO  920
  115 IX(25) = 3                                                        GEO  930
  116 CONTINUE                                                          GEO  940
      I1 = 0                                                            GEO  950
      I2 = 0                                                            GEO  960
      IF(XMIS(1) .LT. 0.0) I1 = IFIX(-XMIS(1)+0.01)                     GEO  970
      IF(XMIS(2) .LT. 0.0) I2 = IFIX(-XMIS(2)+0.01)                     GEO  980
      I3 = MAX0(I1,I2)                                                  GEO  990
      IF(I3 .LE. 0) GOTO 117                                            GEO 1000
      IO19 = IX(86)                                                     GEO 1010
      REWIND IO19                                                       GEO 1020
      REWIND IO3                                                        GEO 1030
      WRITE (IO3) (DND(K),K=1,I3)                                       GEO 1040
      REWIND IO3                                                        GEO 1050
  117 CONTINUE                                                          GEO 1060
      IF(XMIS(1)) 118,132,133                                           GEO 1070
  118 KMX = I1                                                          GEO 1080
      IGOTO = IX(25) - 2                                                GEO 1090
      IF(IGOTO) 119,120,126                                             GEO 1100
  119 IF(IX(72) .GT. 0) GOTO 144                                        GEO 1110
      I = 1                                                             GEO 1120
      READ (IOIN,1001) (DND(K),K=1,KMX)                                 GEO 1130
      WRITE (IO19) I,(DND(K),K=1,KMX)                                   GEO 1140
      WRITE (IOUT,1004) HOLBND(I),(DND(K),K=1,KMX)                      GEO 1150
      I = 3                                                             GEO 1160
      READ (IOIN,1001) (DND(K),K=1,KMX)                                 GEO 1170
      WRITE (IO19) I,(DND(K),K=1,KMX)                                   GEO 1180
      WRITE (IOUT,1004) HOLBND(I),(DND(K),K=1,KMX)                      GEO 1190
      GOTO 144                                                          GEO 1200
  120 DO 125 I=1,4                                                      GEO 1210
        GOTO(124,121,122,123),INRB                                      GEO 1220
  121   IF(I .EQ. 1 .OR. I .EQ. 3) GOTO 125                             GEO 1230
        GOTO 124                                                        GEO 1240
  122   IF(I .EQ. 3 .OR. I .EQ. 4) GOTO 125                             GEO 1250
        GOTO 124                                                        GEO 1260
  123   IF(I .EQ. 3) GOTO 125                                           GEO 1270
  124   CONTINUE                                                        GEO 1280
        READ (IOIN,1001) (DND(K),K=1,KMX)                               GEO 1290
        WRITE (IO19) I,(DND(K),K=1,KMX)                                 GEO 1300
        WRITE (IOUT,1004) HOLBND(I),(DND(K),K=1,KMX)                    GEO 1310
  125 CONTINUE                                                          GEO 1320
      GOTO 144                                                          GEO 1330
  126 DO 131 I=1,6                                                      GEO 1340
        GOTO(130,127,128,129),INRB                                      GEO 1350
  127   IF(I .EQ. 1 .OR. I .EQ. 3) GOTO 131                             GEO 1360
        GOTO 130                                                        GEO 1370
  128   IF(I .EQ. 3 .OR. I .EQ. 4) GOTO 131                             GEO 1380
        GOTO 130                                                        GEO 1390
  129   IF(I .EQ. 3) GOTO 131                                           GEO 1400
  130   CONTINUE                                                        GEO 1410
        READ (IOIN,1001) (DND(K),K=1,KMX)                               GEO 1420
        WRITE (IO19) I,(DND(K),K=1,KMX)                                 GEO 1430
        WRITE (IOUT,1004) HOLBND(I),(DND(K),K=1,KMX)                    GEO 1440
  131 CONTINUE                                                          GEO 1450
      GOTO 144                                                          GEO 1460
  132 T1 = 0.4692                                                       GEO 1470
      GOTO 134                                                          GEO 1480
  133 T1 = XMIS(1)                                                      GEO 1490
  134 DO 137 I=11,16                                                    GEO 1500
        II = I - 10                                                     GEO 1510
        IF(NUAC(I)) 136,135,136                                         GEO 1520
  135   B(II) = T1                                                      GEO 1530
        GOTO 137                                                        GEO 1540
  136   B(II) = 0.0                                                     GEO 1550
  137 CONTINUE                                                          GEO 1560
      WRITE (IOUT,1009)                                                 GEO 1570
      GOTO(138,139,140,142),INRB                                        GEO 1580
  138 WRITE (IOUT,1005) B(1),B(2),B(3),B(4),B(5),B(6)                   GEO 1590
      GOTO 143                                                          GEO 1600
  139 WRITE (IOUT,1010) B(2),B(4),B(5),B(6)                             GEO 1610
      GOTO 143                                                          GEO 1620
  140 CONTINUE                                                          GEO 1630
      IF(NUAC(5) .EQ. 10 .OR. NUAC(5) .EQ. 14) GOTO 141                 GEO 1640
      WRITE (IOUT,1011) B(1),B(2),B(5),B(6)                             GEO 1650
      GOTO 143                                                          GEO 1660
  141 CONTINUE                                                          GEO 1670
      WRITE (IOUT,1012) B(1),B(2),B(5),B(6)                             GEO 1680
      GOTO 143                                                          GEO 1690
  142 WRITE (IOUT,1013) B(1),B(2),B(4),B(5),B(6)                        GEO 1700
  143 CONTINUE                                                          GEO 1710
  144 IF(NUAC(17)) 150,150,145                                          GEO 1720
  145 IF(XMIS(2)) 146,147,148                                           GEO 1730
  146 KMX = I2                                                          GEO 1740
      I = 7                                                             GEO 1750
      READ (IOIN,1001) (DND(K),K=1,KMX)                                 GEO 1760
      WRITE (IO19) I,(DND(K),K=1,KMX)                                   GEO 1770
      WRITE (IOUT,1006) NUAC(17),(DND(K),K=1,KMX)                       GEO 1780
      GOTO 150                                                          GEO 1790
  147 T1 = 0.4692                                                       GEO 1800
      GOTO 149                                                          GEO 1810
  148 T1 = XMIS(2)                                                      GEO 1820
  149 WRITE (IOUT,1007) NUAC(17),T1                                     GEO 1830
  150 CONTINUE                                                          GEO 1840
      IF(I3 .LE. 0) GOTO 151                                            GEO 1850
      END FILE IO19                                                     GEO 1860
      REWIND IO19                                                       GEO 1870
      READ (IO3) (DND(K),K=1,I3)                                        GEO 1880
      REWIND IO3                                                        GEO 1890
  151 CONTINUE                                                          GEO 1900
      RETURN                                                            GEO 1910
C                                                                       GEO 1920
 1000 FORMAT (15I3,I2,I4,7I3)                                           GEO 1930
 1001 FORMAT (6E12.0)                                                   GEO 1940
 1002 FORMAT (1H0/1H0,'NEUTRON FLUX PROBLEM DESCRIPTION - SECTION 003') GEO 1950
 1003 FORMAT (1H0,24I4)                                                 GEO 1960
 1004 FORMAT (1H ,A8,6(1PE14.6)/(1H ,8X,6(1PE14.6)))                    GEO 1970
 1005 FORMAT (1H ,6(1PE14.6))                                           GEO 1980
 1006 FORMAT (1H0/1H0,'ROD BND. CONSTANTS FOR ZONE',I3/(1H ,6(1PE14.6)))GEO 1990
 1007 FORMAT (1H0/1H0,'ROD BND. CONSTANT FOR ALL GROUPS IN ZONE',I5,2H  GEO 2000
     1 ,'IS',1PE14.6)                                                   GEO 2010
 1008 FORMAT (1H ,6(1PE14.6))                                           GEO 2020
 1009 FORMAT (1H0/1H0,'LEFT,TOP,RIGHT,BOTTOM,FRONT,BACK BOUNDARY CONDITIGEO 2030
     1ONS ARE')                                                         GEO 2040
 1010 FORMAT (1H ,6X,'PERIODIC',1PE14.6,6X,'PERIODIC',3(1PE14.6))       GEO 2050
 1011 FORMAT (1H ,2(1PE14.6),2X,'ROTATION(90)',2X,'ROTATION(90)',       GEO 2060
     1 2(1PE14.6))                                                      GEO 2070
 1012 FORMAT (1H ,2(1PE14.6),2X,'ROTATION(60)',2X,'ROTATION(60)',       GEO 2080
     1 2(1PE14.6))                                                      GEO 2090
 1013 FORMAT (1H ,2(1PE14.6),1X,'ROTATION(180)',3(1PE14.6))             GEO 2100
      END                                                               GEO 2110
      SUBROUTINE FINS(P2,B2,NRGN,DCONB,DCONR,SCAC,XL,IVX,JVX,KVX,LVX,   FIN   10
     1 IVXP1,JVXP1,IVZ,IOVX,IOVZ,PVOL,NCOMP,MVX)                        FIN   20
C                                                                       FIN   30
CFINS --099 ***CITATION*** CALC. ROD LOSSES FOR 1,2-D /CF-DNSD,WFLX     FIN   40
C                                                                       FIN   50
      REAL*8 SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2S,XLL1,D8FIN   60
     1 ,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK,B5LK,D1, FIN   70
     2 D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,YRDB,SPR50FIN   80
     3 ,XLAST,P2                                                        FIN   90
C                                                                       FIN  100
      COMMON /ADUBP/ SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2SFIN  110
     1 ,XLL1,D8,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK, FIN  120
     2 B5LK,D1,D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,  FIN  130
     3 YRDB,SPR50,XLAST                                                 FIN  140
C                                                                       FIN  150
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,FIN  160
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   FIN  170
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), FIN  180
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    FIN  190
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    FIN  200
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   FIN  210
     6 IXPUT(9999),XPUT(9999)                                           FIN  220
C                                                                       FIN  230
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   FIN  240
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XELKFIN  250
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    FIN  260
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SAMXI,IX25,IX28,I,J,KB,K,  FIN  270
     4 ITMAX,ITIME,BET(211),DEL(211)                                    FIN  280
C                                                                       FIN  290
      DIMENSION P2(JVX,IVX,KVX),NRGN(JVX,IVX),DCONB(JVX,IVXP1,IOVX),    FIN  300
     1 DCONR(JVXP1,IVZ,IOVZ),B2(MVX,KVX),SCAC(KVX,MVX,KVX),XL(6,KVX),   FIN  310
     2 PVOL(LVX),NCOMP(LVX)                                             FIN  320
C                                                                       FIN  330
C                                                                       FIN  340
C     INRB = 1  ORDINARY                                                FIN  350
C     INRB = 2  PERIODIC(REPEATING)                                     FIN  360
C     INRB = 3  90 DEGREE ROTATIONAL                                    FIN  370
C     INRB = 4  180 DEGREE ROTATIONAL                                   FIN  380
C                                                                       FIN  390
      INRB = IX(72) + 1                                                 FIN  400
      N = IX(20)                                                        FIN  410
      DO 138 I=1,IMAX                                                   FIN  420
        DO 137 J=1,JMAX                                                 FIN  430
          L = NRGN(J,I)                                                 FIN  440
          M = NCOMP(L)                                                  FIN  450
          IF(P2(J,I,K)) 103,101,103                                     FIN  460
  101     IF(XMIS(2) .GE. 0) GOTO 136                                   FIN  470
          TT5 = 0.0                                                     FIN  480
          DO 102 KK=1,KMAX                                              FIN  490
            TT5 = TT5 + SCAC(KK,M,K) * P2(J,I,KK) * PVOL(L)             FIN  500
  102     CONTINUE                                                      FIN  510
          B2(M,K) = B2(M,K) + TT5                                       FIN  520
          GOTO 136                                                      FIN  530
  103     IF(I-1) 107,107,104                                           FIN  540
  104     CONTINUE                                                      FIN  550
          IF(NUAC(5) .NE. 10) GOTO 105                                  FIN  560
          IF(P2(J+1,I-1,K) .NE. 0.0) GOTO 107                           FIN  570
          LT = NRGN(J+1,I-1)                                            FIN  580
          GOTO 106                                                      FIN  590
  105     CONTINUE                                                      FIN  600
          IF(P2(J,I-1,K) .NE. 0.0) GOTO 107                             FIN  610
          LT = NRGN(J,I-1)                                              FIN  620
  106     CONTINUE                                                      FIN  630
          MT = NCOMP(LT)                                                FIN  640
          B2(MT,K) = B2(MT,K) + P2(J,I,K) * DCONB(J,I,N)                FIN  650
  107     IF(I-IVX) 110,108,108                                         FIN  660
  108     IF(INRB .NE. 3) GOTO 115                                      FIN  670
          G = P2(JVX,J,K)                                               FIN  680
          LB = NRGN(JVX,J)                                              FIN  690
          IF(NUAC(5) .NE. 10) GOTO 109                                  FIN  700
          IF((J/2)*2 .NE. J) GOTO 115                                   FIN  710
          G = P2(JVX,J/2,K)                                             FIN  720
          LB = NRGN(JVX,J/2)                                            FIN  730
  109     CONTINUE                                                      FIN  740
          IF(G .NE. 0.0) GOTO 115                                       FIN  750
          MB = NCOMP(LB)                                                FIN  760
          T1 = DCONB(J,I+1,N)                                           FIN  770
          GOTO 113                                                      FIN  780
  110     CONTINUE                                                      FIN  790
          IF(NUAC(5) .NE. 10) GOTO 111                                  FIN  800
          IF(J .EQ. 1) GOTO 115                                         FIN  810
          IF(P2(J-1,I+1,K) .NE. 0.0) GOTO 115                           FIN  820
          LB = NRGN(J-1,I+1)                                            FIN  830
          T1 = DCONB(J-1,I+1,N)                                         FIN  840
          GOTO 112                                                      FIN  850
  111     CONTINUE                                                      FIN  860
          IF(P2(J,I+1,K) .NE. 0.0) GOTO 115                             FIN  870
          LB = NRGN(J,I+1)                                              FIN  880
          T1 = DCONB(J,I+1,N)                                           FIN  890
  112     CONTINUE                                                      FIN  900
          MB = NCOMP(LB)                                                FIN  910
  113     IF(T1-4096.0E-13) 114,115,114                                 FIN  920
  114     B2(MB,K) = B2(MB,K) + P2(J,I,K) * T1                          FIN  930
  115     IF(J .LE. 1) GOTO 116                                         FIN  940
          IF(P2(J-1,I,K) .NE. 0.0) GOTO 118                             FIN  950
          LL = NRGN(J-1,I)                                              FIN  960
          ML = NCOMP(LL)                                                FIN  970
          GOTO 117                                                      FIN  980
  116     IF(INRB .NE. 2) GOTO 118                                      FIN  990
          IF(P2(JVX,I,K) .NE. 0.0) GOTO 118                             FIN 1000
          LL = NRGN(JVX,I)                                              FIN 1010
          ML = NCOMP(LL)                                                FIN 1020
  117     B2(ML,K) = B2(ML,K) + P2(J,I,K) * DCONR(J,I,N)                FIN 1030
  118     IF(J .GE. JVX) GOTO 119                                       FIN 1040
          IF(P2(J+1,I,K) .NE. 0.0) GOTO 126                             FIN 1050
          LR = NRGN(J+1,I)                                              FIN 1060
          MR = NCOMP(LR)                                                FIN 1070
          T1 = DCONR(J+1,I,N)                                           FIN 1080
          GOTO 124                                                      FIN 1090
  119     GOTO(126,120,121,123),INRB                                    FIN 1100
  120     CONTINUE                                                      FIN 1110
          IF(P2(1,I,K) .NE. 0.0) GOTO 126                               FIN 1120
          LR = NRGN(1,I)                                                FIN 1130
          MR = NCOMP(LR)                                                FIN 1140
          T1 = DCONR(J+1,I,N)                                           FIN 1150
          GOTO 124                                                      FIN 1160
  121     CONTINUE                                                      FIN 1170
          G = P2(I,JVX,K)                                               FIN 1180
          LR = NRGN(I,IVX)                                              FIN 1190
          IF(NUAC(5) .NE. 10) GOTO 122                                  FIN 1200
          G = P2(2*I,IVX,K)                                             FIN 1210
          LR = NRGN(2*I,IVX)                                            FIN 1220
  122     CONTINUE                                                      FIN 1230
          IF(G .NE. 0.0) GOTO 126                                       FIN 1240
          MR = NCOMP(LR)                                                FIN 1250
          T1 = DCONR(J+1,I,N)                                           FIN 1260
          GOTO 124                                                      FIN 1270
  123     CONTINUE                                                      FIN 1280
          IF(P2(JVX,IVXP1-I,K) .NE. 0.0) GOTO 126                       FIN 1290
          LR = NRGN(JVX,IVXP1-I)                                        FIN 1300
          MR = NCOMP(LR)                                                FIN 1310
          T1 = DCONR(J+1,I,N)                                           FIN 1320
  124     IF(T1 .EQ. 4096.0E-13) GO TO 126                              FIN 1330
          B2(MR,K) = B2(MR,K) + P2(J,I,K) * T1                          FIN 1340
  126     IF(NUAC(5)-9) 136,127,136                                     FIN 1350
  127     KKK = N + IOVX                                                FIN 1360
          IF(I-1) 131,131,128                                           FIN 1370
  128     IF(J-JMAX) 129,131,131                                        FIN 1380
  129     IF(P2(J+1,I-1,K)) 131,130,131                                 FIN 1390
  130     LB = NRGN(J+1,I-1)                                            FIN 1400
          MB = NCOMP(LB)                                                FIN 1410
          B2(MB,K) = B2(MB,K) + P2(J,I,K) * DCONR(J+1,I,KKK)            FIN 1420
  131     IF(J-1) 135,135,132                                           FIN 1430
  132     IF(I-IMAX) 133,135,135                                        FIN 1440
  133     IF(P2(J-1,I+1,K)) 135,134,135                                 FIN 1450
  134     LB = NRGN(J-1,I+1)                                            FIN 1460
          MB = NCOMP(LB)                                                FIN 1470
          B2(MB,K) = B2(MB,K) + P2(J,I,K) * DCONR(J,I+1,KKK)            FIN 1480
  135     CONTINUE                                                      FIN 1490
  136     CONTINUE                                                      FIN 1500
  137   CONTINUE                                                        FIN 1510
  138 CONTINUE                                                          FIN 1520
      DO 140 M=1,MMAX                                                   FIN 1530
        XRDB = XRDB + B2(M,K)                                           FIN 1540
  140 CONTINUE                                                          FIN 1550
      RETURN                                                            FIN 1560
      END                                                               FIN 1570