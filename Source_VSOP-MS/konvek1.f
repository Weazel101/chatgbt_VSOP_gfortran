      BLOCK DATA                                                        OCK   10
C                                                                       OCK   20
CFZJ042                                                       09.09.05  OCK   30
C                                                                       OCK   40
      COMMON /ITPARM/ IFGO1,IFGO3,IFGO4,OVLOOP                          OCK   50
C                                                                       OCK   60
      COMMON /PRESYS/ SYSINV,SYSUM1,SYSUM2,DELINV,PRMINV,PRSUM1,PRSUM2  OCK   70
C                                                                       OCK   80
      DATA SYSINV/0./,SYSUM1/0./,SYSUM2/0./,DELINV/0./,PRMINV/0./,PRSUM1OCK   90
     1 /0./,PRSUM2/0./,OVLOOP/1./                                       OCK  100
      END                                                               OCK  110
      SUBROUTINE KONVEK(IMT,NMT,RT,ZT,TT,QT,FELDH,TFLU,BU1,IFKON,IFKO1, KON   10
     1 IFZW,ITM3,ZEITH,ZEITS,GEOFAK,IFPPP,IFSTOP,IFINST,N200,NDR,NXS,   KON   20
     2 NENDK0,ROGG,EPSIL,DHYD,STZUK,TFLVOR,T,TFL,RHO,IFB,FZQ,DZ,MZ,MR,P,KON   30
     3 KOM,ZP,RP,STROM,IFZST,IFZTF,TGRA,TFUL,HG,HF,IPAR,JPAR,NPAR,XKP,  KON   40
     4 ROGP,MIX,FELD,VRP,VREG)                                          KON   50
C                                                                       KON   60
CFZJ042                                                       09.09.05  KON   70
C                                                                       KON   80
C     STEUERPROGRAMM FUER STROEMUNGS-UND GASTEMPERATUR-BERECHNUNG       KON   90
C     (QUASISTATIONAER)                                                 KON  100
C                                                                       KON  110
      COMMON /ITPARM/ IFGO1,IFGO3,IFGO4,OVLOOP                          KON  120
C                                                                       KON  130
      COMMON /CPBIL/ CPTER,CPKIN,CPSTR,CPGAS                            KON  140
C                                                                       KON  150
      COMMON /BILANZ/ TGASM,IZAEL                                       KON  160
C                                                                       KON  170
      COMMON /BIL/ IFPR                                                 KON  180
C                                                                       KON  190
      COMMON /EXTX/ WMAX(5),WMIN(5),WMIT(5),RWMAX(5),RWMIN(5),ZWMAX(5), KON  200
     1 ZWMIN(5)                                                         KON  210
CFZJ042                                                       09.09.05  KON  220
C                                                                       KON  230
      COMMON /COUPL/ IPRINT                                             KON  240
C                                                                       KON  250
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      KON  260
C                                                                       KON  270
      COMMON /ITER/ IB,IT,IT1,IT2,ITM1,ITM2,EPSI1,EPSI2,IFSQ,OVM1,OVM2  KON  280
C                                                                       KON  290
      COMMON /ZEITVR/ IFZDR,ZTF(3,100),ZST(3,100),ZDR(100),ZVOR(100),IFZKON  300
     1 ,IZKOM(3),IZKO,IFAGL,IZAEHL                                      KON  310
C                                                                       KON  320
      COMMON /SCHR/ IF0                                                 KON  330
C                                                                       KON  340
      COMMON /ITE1/ OVREL,PREL,PMR,IME,NME,ZRG,NKORR,GASREL,DGASM,IFGG  KON  350
C                                                                       KON  360
      COMMON /ITE2/ SMAX,TDIFF,SROWM,IRA,ZRS,NVIT,WTRANS,WSUM,WREST,IFGSKON  370
C                                                                       KON  380
      COMMON /SPECTI/ ITIK(10)                                          KON  390
C                                                                       KON  400
      COMMON /OPT/ KENN,IOPUT,NIFKO                                     KON  410
C                                                                       KON  420
CFZJ006 enlarged dimensions common QVAR                       28.11.03  KON  430
      COMMON /QVAR/ DUM(1211),TA(300),N61,URZ,ZLEKA,ABXEN,TI(300),DUMM, KON  440
     1 TAUFH,JRESTW,JRESTR,JREST,TAU,D(11),ST(300)                      KON  450
C                                                                       KON  460
      COMMON /MPUNKT/ IMPU,KMPU,EMPU,TTTEIN,TTTAUS                      KON  470
C                                                                       KON  480
      COMMON /MPUTA/ TEIMIN,TEIMAX,EMP0,TAU0,MPUTAU,QWU,DTAU,TEI0       KON  490
C                                                                       KON  500
CFZJ048 enlarged dimension                                    11.04.07  KON  510
      COMMON /VARDIT/ B(5000000)                                        KON  520
C                                                                       KON  530
CFZJ042                                                       09.09.05  KON  540
      COMMON /ADDRT/ KX(240),KY(240),LZ(240),NENDPT                     KON  550
CFZJ042                                                       09.09.05  KON  560
CFZJ055                                                       25.09.07  KON  570
CFZJ042                                                       09.09.05  KON  580
C                                                                       KON  590
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             KON  600
C                                                                       KON  610
      COMMON /UPERR/ OERR                                               KON  620
C                                                                       KON  630
CFZJ058                                                       05.11.08  KON  640
      COMMON /KEFFT/ TKEFF(5000),RKEFF(5000),IPKEFF,NKEFF,TKUM,POWLMAX, KON  650
     1 THETMX,POWLM(5000),POWB(5000),BUSCRAP(5000),THETM(5000),         KON  660
     2 POWERF(5000),TEIN(5000),TAUS(5000),TEINTR,TAUSTR                 KON  670
C                                                                       KON  680
      COMMON /COUNT/ NCOUNT                                             KON  690
C                                                                       KON  700
      COMMON /TETA/ TTEIN,TTAUS                                         KON  710
C                                                                       KON  720
CFZJ042                                                       09.09.05  KON  730
      CHARACTER*4 U(4)/'GRID',' TEM','PERA','TURE'/,U1(4)/'SOUR','CE (',KON  740
     1 'W/CM','**3)'/                                                   KON  750
C                                                                       KON  760
CFZJ042                                                       09.09.05  KON  770
      DIMENSION TT(IMAZ,NMAZ),QT(IMAZ,NMAZ),FELDH(IMAZ,NMAZ),           KON  780
     1 TFLU(IMAZ,NMAZ),BU1(IMAZ,NMAZ),RT(IMAZ+1),ZT(NMAZ+1),            KON  790
     2 ROGG(KONN,KONI),EPSIL(KOMAX),DHYD(KOMAX),STZUK(KOMAX),           KON  800
     3 TFLVOR(KOMAX),T(KONN,KONI),TFL(KONN,KONI),RHO(KONN,KONI),        KON  810
     4 IFB(KONN,KONI),FZQ(KONI),DZ(KONN),P(KONN,KONI),KOM(KONN,KONI),   KON  820
     5 ZP(KONN),RP(KONI),STROM(KONN,KONI),IFZST(KOMAX),IFZTF(KOMAX),    KON  830
     6 TGRA(IMAZ,NMAZ),TFUL(IMAZ,NMAZ),HG(KONN,KONI),HF(KONN,KONI),     KON  840
     7 FELD(KONN,KONI),FELDP(100),IPAR(KONI*2,10),JPAR(KONI*2,10),      KON  850
     8 NPAR(KONI*2,10),XKP(KONI*2),ROGP(KONI*2),MIX(NDR),VRP(NDR),      KON  860
     9 VREG(NDR)                                                        KON  870
C                                                                       KON  880
      REAL MZ(KONN,KONI),MR(KONN,KONI)                                  KON  890
C                                                                       KON  900
CFZJ042                                                       09.09.05  KON  910
      EQUIVALENCE(LZ(8),ITEML),(LZ(174),ISTRB),(LZ(175),IFQRO),         KON  920
     1 (LZ(164),IVOL),(LZ(204),IXKR),(LZ(205),IXKZ),(LZ(206),ISUMR),    KON  930
     2 (LZ(165),IIOVE),(LZ(166),IIUVE),(LZ(207),ISUMX),(LZ(138),IXKK),  KON  940
     3 (LZ(150),IFRQ),(LZ(151),IDRK),(LZ(158),IXKON),(LZ(133),IQTV),    KON  950
     4 (LZ(134),ILTV),(LZ(135),IXGEO),(LZ(171),IXZSU),(LZ(172),IXNSU),  KON  960
     5 (LZ(168),IIOKU),(LZ(169),IIUKU),(LZ(143),IIFBR),(LZ(81),IR),     KON  970
     6 (LZ(82),IZ),(LZ(153),IFZQ1),(LZ(154),IFRQ1),(LZ(208),ILAMT),     KON  980
     7 (LZ(132),IDROH),(LZ(144),IALPH),(LZ(146),IALFI),(LZ(167),IXVER), KON  990
     8 (LZ(209),IALGA),(LZ(145),IIFBQ),(LZ(103),IDU),(LZ(33),IKOMP),    KON 1000
     9 (LZ(25),IXKSU),(LZ(26),IXKIN),(LZ(27),IXKSA),(LZ(83),ITFMI),     KON 1010
     X (LZ(85),IXH)                                                     KON 1020
C                                                                       KON 1030
   50 FORMAT (/5X,'RATE OF MASS FLOW          ',10X,1PE10.2,' KG/S'/5X, KON 1040
     1 'PRESSURE-I-O ',10X,0PF8.4,'   ---> ',F8.4,' BAR'/5X,'TEMPERATUREKON 1050
     2-I-O ',7X,F8.1,'   ---> ',F8.1,' DEG C'/5X,'===>   CARRIED AWAY BYKON 1060
     3 THE COOLANT    ',1PE9.2,' W')                                    KON 1070
   51 FORMAT (5X,'BLOWER POWER AT T-GAS =',F5.0,' GRD-C:',F10.3,' KW')  KON 1080
   60 FORMAT (/5X,'RE-MIN   RE-AVG.   RE-MAX    V-MIN  V-AVG.   V-MAX   KON 1090
     1 P-MIN    P-MAX STRM-MIN STRM-MAX    Q-MIN    Q-MAX  TGAS-AVG'/3X,KON 1100
     2 F8.1,2X,F8.1,1X,F10.1,F7.1,2F8.1,2(1PE9.1),5(0PF9.2))            KON 1110
   70 FORMAT (1X,'R:',F8.1,12X,2F8.1,8X,F8.1,6F9.1)                     KON 1120
   80 FORMAT (1X,'Z:',F8.1,12X,2F8.1,8X,F8.1,6F9.1)                     KON 1130
   75 FORMAT (5X,'SYSTEM PRESSURE  =',F10.4,'  BAR')                    KON 1140
   79 FORMAT (5X,'SOURCE (COMP.',I2,') = ',1PE9.2,'  KG/SEC'/5X,'TFL    KON 1150
     1(COMP.',I2,') = ',0PF9.2,'  DEG C')                               KON 1160
   82 FORMAT (5X,'SOURCE (COMP.',I2,') = ',1PE9.2,'  KG/SEC')           KON 1170
   83 FORMAT (5X,'TFL    (COMP.',I2,') = ',0PF9.2,'  DEG C')            KON 1180
   86 FORMAT (/5X,'AT REFERENCE TIME ',1PE8.2,' MIN. RESULTS TO :')     KON 1190
  250 FORMAT (5X,'CONVECTIONAL SUPPLY TO THE GRID ',1PE15.2)            KON 1200
  260 FORMAT (///T40,'*** CALCULATION OF CONVECTIONAL HEAT TRANSPORT ***KON 1210
     1'/)                                                               KON 1220
  421 FORMAT (/T30,' **WARNING** CALCULATION OF GAS TEMPERATURE NOT CONVKON 1230
     1ERGED'/)                                                          KON 1240
  423 FORMAT (T30,' **WARNING** CALCULATION OF FLOW FIELD NOT CONVERGED KON 1250
     1')                                                                KON 1260
  425 FORMAT (/T30,' **WARNING** FLOW/GASTEMP.-ITERATION NOT CONVERGED'/KON 1270
     1 )                                                                KON 1280
  500 FORMAT (/16X,'     FLOW FIELD CALCULATION',T84,'   GAS TEMPERATUREKON 1290
     1-CALCULATION'/1X,'IB',2X,'IT2',4X,'SMAX',3X,'SDIFF/SMAX',2X,'SROWMKON 1300
     2',3X,'LINE',2X,'CPU-TIME',1X,'NVIT',T60,'IT1',2X,'OVREL',2X,'PREL'KON 1310
     3 ,4X,'TFL-CRIT',2X,'(I,N)-KR',1X,'CPU-TIME',1X,'NCOR',1X,'DTF-MAX'KON 1320
     4 ,1X,'DTF-AVG OVLP')                                              KON 1330
  501 FORMAT (1X,I2,I5,1X,3(1PE9.2,1X),1X,I3,1X,1PE9.2,1X,I3,T60,I3,2X, KON 1340
     1 0PF4.2,2(1PE9.2,1X),1X,'(',I3,',',I2,')',1PE9.2,I4,1X,2(0PF6.2,  KON 1350
     2 ' %'),1X,F4.2)                                                   KON 1360
C                                                                       KON 1370
C                                                                       KON 1380
      OERR = 1.0E+20                                                    KON 1390
CFZJ042                                                       09.09.05  KON 1400
C                                                                       KON 1410
C     ANZAHL THERMIX-RADIEN UND -HOEHEN (WEITERGABE AN SUBR. TOPF)      KON 1420
C                                                                       KON 1430
      IMM = IMT + 1                                                     KON 1440
      NMM = NMT + 1                                                     KON 1450
      IF(IZAEL .NE. 0) GOTO 917                                         KON 1460
      DFAK = 1.                                                         KON 1470
  917 CONTINUE                                                          KON 1480
      IF(IFKON .EQ. 2) IFKO1 = 1                                        KON 1490
      IM = IM1 + 1                                                      KON 1500
      NM = NM1 + 1                                                      KON 1510
C                                                                       KON 1520
      CALL SETZT(IM,NM,ZP,RP,KONN,KONI,HG,IMT,NMT,RT,ZT,IMAZ,NMAZ,TGRA, KON 1530
     1 FELDH)                                                           KON 1540
C                                                                       KON 1550
      CALL SETZT(IM,NM,ZP,RP,KONN,KONI,HF,IMT,NMT,RT,ZT,IMAZ,NMAZ,TFUL, KON 1560
     1 FELDH)                                                           KON 1570
C                                                                       KON 1580
CFZJ042                                                       09.09.05  KON 1590
      IF(NIFKO .EQ. 0) GOTO 1                                           KON 1600
      DO 3 I=1,IMT                                                      KON 1610
        DO 3 N=1,NMT                                                    KON 1620
          BU1(I,N) = TT(I,N)                                            KON 1630
    3 CONTINUE                                                          KON 1640
C                                                                       KON 1650
      CALL SETZT(IM,NM,ZP,RP,KONN,KONI,T,IMT,NMT,RT,ZT,IMAZ,NMAZ,BU1,   KON 1660
     1 FELDH)                                                           KON 1670
C                                                                       KON 1680
      GOTO 2                                                            KON 1690
    1 CONTINUE                                                          KON 1700
      DO 10 I=1,IMT                                                     KON 1710
        DO 10 N=1,NMT                                                   KON 1720
          QT(I,N) = TT(I,N)                                             KON 1730
   10 CONTINUE                                                          KON 1740
C                                                                       KON 1750
      CALL SETZT(IM,NM,ZP,RP,KONN,KONI,T,IMT,NMT,RT,ZT,IMAZ,NMAZ,QT,    KON 1760
     1 FELDH)                                                           KON 1770
C                                                                       KON 1780
      IFPR = 0                                                          KON 1790
      IFGO = 1                                                          KON 1800
      IB = 0                                                            KON 1810
      IF0 = 0                                                           KON 1820
      IFZUS = 0                                                         KON 1830
      IF (IPRINT .GE. 1) WRITE (6,260)                                  KON 1840
C                                                                       KON 1850
C     SETZEN VON ZEITABHAENG. DRUCK UND MASSENSTROMQUELLEN              KON 1860
C     SETZEN DES AUSGLEICHSWERTES DER MASSENSTROMQUELLE (IFZST = 2)     KON 1870
C     AUSSCHREIBEN DER ENTSPRECHENDEN WERTE FUER DIE KOMPOSITIONEN      KON 1880
C                                                                       KON 1890
      IF(IFZ .EQ. 0) GOTO 19                                            KON 1900
      ZEITM = ZEITH * 60.                                               KON 1910
      WRITE (6,86) ZEITM                                                KON 1920
      IPSTEU = 0                                                        KON 1930
      IF(ZEITM .GT. ZVOR(1)) GOTO 14                                    KON 1940
      IPSTEU = 1                                                        KON 1950
      GOTO 91                                                           KON 1960
   14 CONTINUE                                                          KON 1970
      IF(ZEITM .LT. ZVOR(IZAEHL)) GOTO 15                               KON 1980
      IPSTEU = IZAEHL                                                   KON 1990
      GOTO 91                                                           KON 2000
   15 CONTINUE                                                          KON 2010
      IZAE1 = IZAEHL - 1                                                KON 2020
      DO 90 IZ1=1,IZAE1                                                 KON 2030
        IF(ZEITM .GE. ZVOR(IZ1) .AND. ZEITM .LT. ZVOR(IZ1+1)) GOTO 89   KON 2040
        GOTO 90                                                         KON 2050
   89   J1 = IZ1                                                        KON 2060
        GOTO 91                                                         KON 2070
   90 CONTINUE                                                          KON 2080
   91 CONTINUE                                                          KON 2090
      IF(IFZDR .NE. 1) GOTO 16                                          KON 2100
C                                                                       KON 2110
C     DRUCKINTERPOLATION                                                KON 2120
C                                                                       KON 2130
      DRUCK = ZTPOL(ZVOR,ZDR,IPSTEU,J1,ZEITM)                           KON 2140
C                                                                       KON 2150
      WRITE (6,75) DRUCK                                                KON 2160
   16 CONTINUE                                                          KON 2170
      IF(IZKO .EQ. 0) GOTO 19                                           KON 2180
      DO 21 K=1,IZKO                                                    KON 2190
        KK = IZKOM(K)                                                   KON 2200
        IF(IFZST(KK) .NE. 1) GOTO 23                                    KON 2210
        DO 22 JJ=1,IZAEHL                                               KON 2220
          FELDP(JJ) = ZST(K,JJ)                                         KON 2230
   22   CONTINUE                                                        KON 2240
C                                                                       KON 2250
        STZUK(KK) = ZTPOL(ZVOR,FELDP,IPSTEU,J1,ZEITM)                   KON 2260
C                                                                       KON 2270
   23   CONTINUE                                                        KON 2280
        IF(IFZTF(KK) .NE. 1) GOTO 21                                    KON 2290
        DO 24 JJ=1,IZAEHL                                               KON 2300
          FELDP(JJ) = ZTF(K,JJ)                                         KON 2310
   24   CONTINUE                                                        KON 2320
C                                                                       KON 2330
        TFLVOR(KK) = ZTPOL(ZVOR,FELDP,IPSTEU,J1,ZEITM)                  KON 2340
C                                                                       KON 2350
   21 CONTINUE                                                          KON 2360
      IF(IZKO .EQ. 0) GOTO 19                                           KON 2370
      DO 85 K=1,IZKO                                                    KON 2380
        KK = IZKOM(K)                                                   KON 2390
        IF(IFZST(KK) .NE. 1 .OR. IFZTF(KK) .NE. 1) GOTO 78              KON 2400
        WRITE (6,79) KK,STZUK(KK),KK,TFLVOR(KK)                         KON 2410
        GOTO 85                                                         KON 2420
   78   CONTINUE                                                        KON 2430
        IF(IFZST(KK) .NE. 1) GOTO 81                                    KON 2440
        WRITE (6,82) KK,STZUK(KK)                                       KON 2450
        GOTO 85                                                         KON 2460
   81   CONTINUE                                                        KON 2470
        IF(IFZTF(KK) .NE. 1) GOTO 85                                    KON 2480
        WRITE (6,83) KK,TFLVOR(KK)                                      KON 2490
   85 CONTINUE                                                          KON 2500
   19 CONTINUE                                                          KON 2510
      IF(IFZDR .NE. 2) GOTO 601                                         KON 2520
      DRUCK = DRUCK * DFAK                                              KON 2530
      WRITE (6,75) DRUCK                                                KON 2540
  601 CONTINUE                                                          KON 2550
C                                                                       KON 2560
      CALL BEZUG(FZQ,DZ,B(KX(ISTRB)),B(KX(IFQRO)),KOM,B(KX(IVOL)),STZUK)KON 2570
C                                                                       KON 2580
      DO 20 K=1,KM                                                      KON 2590
        IF(STZUK(K) .NE. 0.) IFZUS = 1                                  KON 2600
   20 CONTINUE                                                          KON 2610
C                                                                       KON 2620
      IF(IPRINT .GE. -1 .AND. IFPPP .EQ. 1) CALL SCHGAS(T,U,ZP,RP,IFB)  KON 2630
C                                                                       KON 2640
C     GITTERTEMPERATUR (T)                                              KON 2650
C                                                                       KON 2660
    2 CONTINUE                                                          KON 2670
      IF(IFKON .EQ. 0) GOTO 999                                         KON 2680
CFZJ042                                                       09.09.05  KON 2690
      IF0 = 1                                                           KON 2700
      IF(IT .GE. 100) GOTO 300                                          KON 2710
      DO 200 I=1,IM1                                                    KON 2720
        DO 200 N=1,NM1                                                  KON 2730
          TFL(I,N) = T(I,N)                                             KON 2740
C                                                                       KON 2750
          IF(IFBG(I,N,IFB) .EQ. 0) TFL(I,N) = 0.                        KON 2760
C                                                                       KON 2770
  200 CONTINUE                                                          KON 2780
      DO 201 I=1,IM1                                                    KON 2790
        TFL(I,NM) = T(I,NM1)                                            KON 2800
  201 CONTINUE                                                          KON 2810
      DO 202 N=1,NM1                                                    KON 2820
        TFL(IM,N) = T(IM1,N)                                            KON 2830
  202 CONTINUE                                                          KON 2840
  300 CONTINUE                                                          KON 2850
      IF (IPRINT .GE. 1) WRITE (6,500)                                  KON 2860
  100 CONTINUE                                                          KON 2870
      IB = IB + 1                                                       KON 2880
      IF(IB .EQ. ITM3 ) IFPR = 1                                        KON 2890
      IF(IFGO .EQ. 0) IFPR = 1                                          KON 2900
C                                                                       KON 2910
      CALL VORP(ROGG,TFL,RHO,IFB,DZ,MZ,MR,B(KX(IXKR)),B(KX(IXKZ)),KOM,  KON 2920
     1 STROM,B(KX(ISUMR)))                                              KON 2930
C                                                                       KON 2940
      CALL STROEM(0,0,B(KX(IIOVE)),B(KX(IIUVE)),B(KX(ISUMX)),           KON 2950
     1 B(KX(ISUMR)),B(KX(ISTRB)),B(KX(IFQRO)),ROGG,IFB,FZQ,DZ,MZ,MR,P,  KON 2960
     2 B(KX(IXKR)),B(KX(IXKZ)),KOM,STROM,STZUK,TFL,RHO,B(KX(IXKK)),     KON 2970
     3 B(KX(IFRQ)),B(KX(IDRK)),EPSIL,B(KX(IXKON)),DHYD,B(KX(IQTV)),     KON 2980
     4 B(KX(ILTV)),B(KX(IXGEO)),T,B(KX(IXZSU)),B(KX(IXNSU)),B(KX(IIOKU))KON 2990
     5 ,B(KX(IIUKU)),B(KX(IIFBR)),TFLVOR,B(KX(IVOL)),IPAR,JPAR,NPAR,XKP,KON 3000
     6 ROGP)                                                            KON 3010
C                                                                       KON 3020
      CPSTR = CPSTR + ZRS                                               KON 3030
CFZJ042                                                       09.09.05  KON 3040
C                                                                       KON 3050
      IF(IFPR.EQ.1) CALL PRIN(FELD,IPRINT,IFPPP,EPSIL,ROGG,MZ,MR,P,KOM, KON 3060
     1 STROM,T,TFL,RHO,IFB,DHYD,FZQ,B(KX(IFRQ)),B(KX(IR)),B(KX(IZ)),DZ, KON 3070
     2 ZP,RP)                                                           KON 3080
C                                                                       KON 3090
      CALL GASTEM(IFGO,IFZW,FELD,TFL,IFB,FZQ,DZ,KOM,STROM,STZUK,TFLVOR, KON 3100
     1 RHO,B(KX(IDRK)),MZ,MR,B(KX(IXKR)),B(KX(IXKZ)),B(KX(IFZQ1)),      KON 3110
     2 B(KX(IFRQ1)),B(KX(ILAMT)),B(KX(IDROH)),B(KX(IQTV)),B(KX(ILTV)),  KON 3120
     3 B(KX(IXGEO)),T,B(KX(IALPH)),B(KX(IALFI)),B(KX(IXVER)),EPSIL,DHYD,KON 3130
     4 B(KX(IFRQ)),B(KX(IXKON)),B(KX(IALGA)),B(KX(IXZSU)),B(KX(IXNSU)), KON 3140
     5 P,B(KX(IIFBR)),B(KX(IVOL)),B(KX(ISTRB)),B(KX(IFQRO)))            KON 3150
C                                                                       KON 3160
      CPGAS = CPGAS + ZRG                                               KON 3170
      DO 17 I=1,IM1                                                     KON 3180
        DO 17 N=1,NM1                                                   KON 3190
          STROM(I,N) = FELD(I,N)                                        KON 3200
   17 CONTINUE                                                          KON 3210
      IF(IFPR .EQ. 1 .AND. IPRINT .GE. 1 .AND. IFPPP .EQ. 1) WRITE (6,  KON 3220
     1 500)                                                             KON 3230
      IF(IPRINT .GE. 1)                                                 KON 3240
     1 WRITE (6,501) IB,IT2,SMAX,TDIFF,SROWM,IRA,ZRS,NVIT,IT1,OVREL,    KON 3250
     2 PREL,PMR,IME,NME,ZRG,NKORR,GASREL,DGASM,OVLOOP                   KON 3260
      IF(IFKON .EQ. 2 .AND. IB .EQ. 1 .AND. GASREL .LT. 1.) IFKO1 = -1  KON 3270
      IF(IFPR .NE. 1) GOTO 424                                          KON 3280
      IF(IFGG .EQ. 0) GOTO 420                                          KON 3290
      WRITE (6,421)                                                     KON 3300
C                                                                       KON 3310
      CALL ABEND(2)                                                     KON 3320
C                                                                       KON 3330
  420 IF(IFGS .EQ. 0) GOTO 422                                          KON 3340
      WRITE (6,423)                                                     KON 3350
C                                                                       KON 3360
      CALL ABEND(2)                                                     KON 3370
C                                                                       KON 3380
  422 IF(IFGO .EQ. 0) GOTO 424                                          KON 3390
      WRITE (6,425)                                                     KON 3400
C                                                                       KON 3410
      CALL ABEND(2)                                                     KON 3420
C                                                                       KON 3430
  424 CONTINUE                                                          KON 3440
      IF(IFPR .NE. 1) GOTO 100                                          KON 3450
C                                                                       KON 3460
      CALL QUELLE(FELD,ZEITS,GEOFAK,DFAK,IFSTOP,IFINST,EPSIL,T,TFL,RHO, KON 3470
     1 IFB,FZQ,DZ,P,B(KX(IXKR)),B(KX(IXKZ)),KOM,B(KX(IIFBR)),           KON 3480
     2 B(KX(IIFBQ)),STROM,B(KX(IXZSU)),B(KX(IXNSU)),STZUK,TFLVOR,       KON 3490
     3 B(KX(IVOL)),B(KX(ISTRB)),B(KX(IFQRO)),B(KX(IXKSU)),B(KX(IXKIN)), KON 3500
     4 B(KX(IXKSA)),B(KX(ITFMI)),B(KX(IXH)))                            KON 3510
C                                                                       KON 3520
      IF(IFZUS .NE. 1) GOTO 30                                          KON 3530
      PEIN = 0.                                                         KON 3540
      PAUS = 0.                                                         KON 3550
      TTEIN = 0.                                                        KON 3560
      TTAUS = 0.                                                        KON 3570
      IZE = 0                                                           KON 3580
      IZA = 0                                                           KON 3590
      VOLSTA = 0.                                                       KON 3600
      VOLSTE = 0.                                                       KON 3610
      ABGEF = 0.                                                        KON 3620
      DURCHG = 0.                                                       KON 3630
      DO 40 I=2,IM1                                                     KON 3640
        DO 40 N=2,NM1                                                   KON 3650
          K = KOM(I,N)                                                  KON 3660
          STZ = STZUK(K)                                                KON 3670
          VOLMA = FZQ(N) * DZ(I) * STZ                                  KON 3680
          IF(STZ) 35,40,31                                              KON 3690
   31     IZE = IZE + 1                                                 KON 3700
          VOLSTE = VOLSTE + VOLMA                                       KON 3710
          PEIN = PEIN + P(I,N) * VOLMA                                  KON 3720
          TTT = TFLVOR(K) * VOLMA                                       KON 3730
          TTEIN = TTEIN + TTT                                           KON 3740
          GOTO 39                                                       KON 3750
   35     IZA = IZA + 1                                                 KON 3760
          VOLSTA = VOLSTA + VOLMA                                       KON 3770
          PAUS = PAUS + P(I,N) * VOLMA                                  KON 3780
          TEM = (TFL(I,N)+TFL(I-1,N)+TFL(I,N-1)+TFL(I-1,N-1)) / 4.      KON 3790
          TTT = TEM * VOLMA                                             KON 3800
          TTAUS = TTAUS + TTT                                           KON 3810
   39     ABGEF = ABGEF - TTT                                           KON 3820
          DURCHG = DURCHG + ABS(VOLMA)                                  KON 3830
   40 CONTINUE                                                          KON 3840
      PEIN = PEIN / 1.E5 / VOLSTE                                       KON 3850
      PAUS = PAUS / 1.E5 / VOLSTA                                       KON 3860
      DPEA = ABS(PEIN-PAUS)                                             KON 3870
      TTEIN = TTEIN / VOLSTE                                            KON 3880
      TTAUS = TTAUS / VOLSTA                                            KON 3890
      TTTAUS = TTAUS                                                    KON 3900
      DURCHG = DURCHG / 2.                                              KON 3910
      ABGEF = ABGEF * CP                                                KON 3920
      IF(IFSTOP .EQ. 1) WRITE (6,50) DURCHG,PEIN,PAUS,TTEIN,TTAUS,ABGEF KON 3930
      TEINTR = TTEIN                                                    KON 3940
      TAUSTR = TTAUS                                                    KON 3950
      TCOMP = TTEIN                                                     KON 3960
      QGEBL = 2.0786 * (TCOMP+273.15) * DURCHG * (PEIN-PAUS) / DRUCK    KON 3970
      IF(IFSTOP .EQ. 1) WRITE (6,51) TCOMP,QGEBL                        KON 3980
      IT3 = ITIK(1) + 1                                                 KON 3990
      TA(IT3) = TTAUS                                                   KON 4000
      TI(IT3) = TTEIN                                                   KON 4010
      ST(IT3) = DURCHG                                                  KON 4020
      TAU = TFLVOR(KMPU)                                                KON 4030
      DTAU = TTAUS - TTEIN                                              KON 4040
   30 CONTINUE                                                          KON 4050
      IF(IFSTOP .EQ. 1) WRITE (6,250) WREST                             KON 4060
      EXPN = .33                                                        KON 4070
      DO 120 I=1,IM                                                     KON 4080
        DO 120 N=1,NM                                                   KON 4090
          IFB1 = IFB(I,N) + 1                                           KON 4100
          GOTO(130,140),IFB1                                            KON 4110
          GOTO 140                                                      KON 4120
  130     ROGG(I,N) = 0.                                                KON 4130
          GOTO 120                                                      KON 4140
  140     CONTINUE                                                      KON 4150
          TEM = (TFL(I-1,N)+TFL(I,N-1)+TFL(I-1,N-1)+TFL(I,N)) / 4.      KON 4160
          PBEZ = P(I,N) / 1.E5 + DRUCK                                  KON 4170
C                                                                       KON 4180
          XLH = XLHE(TEM) / 100.                                        KON 4190
C                                                                       KON 4200
          RHOH = DICHTE(PBEZ,TEM) / 1.E6                                KON 4210
C                                                                       KON 4220
          K = KOM(I,N)                                                  KON 4230
          EPSI = EPSIL(K)                                               KON 4240
          GOTO(120,150),IFB1                                            KON 4250
          GOTO 160                                                      KON 4260
  150     CONTINUE                                                      KON 4270
C                                                                       KON 4280
C     ROGG = ALPHA (W/CM**2/K)                                          KON 4290
C                                                                       KON 4300
          ROGG(I,N) = RHO(I,N) / FZQ(N) / DZ(I) / (1.-EPSI) / 600. *    KON 4310
     1     DKUG / 1.E4                                                  KON 4320
          GOTO 170                                                      KON 4330
  160     ROGG(I,N) = RHO(I,N) / 4. / EPSI * DHYD(K) / 100. / FZQ(N) /  KON 4340
     1     DZ(I) / 1.E4                                                 KON 4350
  170     E = RHOH * CP / XLH                                           KON 4360
          E = E**EXPN                                                   KON 4370
C                                                                       KON 4380
C     ROGG = BETA-STRICH                                                KON 4390
C                                                                       KON 4400
          ROGG(I,N) = ROGG(I,N) / XLH / E                               KON 4410
  120 CONTINUE                                                          KON 4420
CFZJ042                                                       09.09.05  KON 4430
C                                                                       KON 4440
      IF(IFPR .EQ. 1 .AND. IPRINT .GE. -2 .AND. IFPPP .EQ. 1) CALL      KON 4450
     1 DRUCKE(IPRINT,T,STROM,RHO,ZP,RP,IFB)                             KON 4460
C                                                                       KON 4470
C     FLUIDTEMPERATUR (TFL)                                             KON 4480
C                                                                       KON 4490
      IF(IFPR .EQ. 0) GOTO 100                                          KON 4500
C                                                                       KON 4510
      IF(IPRINT .GE. 1 .AND. IFPPP .EQ. 1) CALL SCHGAS(FELD,U1,ZP,RP,IFBKON 4520
     1 )                                                                KON 4530
C                                                                       KON 4540
      CALL EXTREM(FELD,5,0,IFB,B(KX(IR)),B(KX(IZ)),FZQ,DZ)              KON 4550
C                                                                       KON 4560
      CALL SETZT(IMT,NMT,RT,ZT,IMAZ,NMAZ,QT,IM1,NM1,ZP,RP,KONN,KONI,FELDKON 4570
     1 ,FELDH)                                                          KON 4580
C                                                                       KON 4590
      CALL SETZT(IMT,NMT,RT,ZT,IMAZ,NMAZ,TFLU,IM1,NM1,ZP,RP,KONN,KONI,  KON 4600
     1 STROM,FELDH)                                                     KON 4610
C                                                                       KON 4620
      CALL SETZT(IMT,NMT,RT,ZT,IMAZ,NMAZ,BU1,IM1,NM1,ZP,RP,KONN,KONI,RHOKON 4630
     1 ,FELDH)                                                          KON 4640
C                                                                       KON 4650
C     INTERPOLATION DER ALPHA*F/VOL(=RHO), IST NOTWENDIG BEI TFL/ALPHA- KON 4660
C     KOPPLUNG AN THERMIX, WENN T-TFL<1, DA DANN ALPHA*F UNDEFINIERT    KON 4670
C                                                                       KON 4680
      IF(IZAEL .LE. 3) IZAEL = IZAEL + 1                                KON 4690
      IF (IPRINT .LT. 1)  GOTO 998                                      KON 4700
      WRITE (6,60) WMIN(1),WMIT(1),WMAX(1),WMIN(2),WMIT(2),WMAX(2),     KON 4710
     1 WMIN(3),WMAX(3),WMIN(4),WMAX(4),WMIN(5),WMAX(5),TGASM            KON 4720
      WRITE (6,70) RWMIN(1),RWMAX(1),RWMIN(2),RWMAX(2),RWMIN(3),        KON 4730
     1 RWMAX(3),RWMIN(4),RWMAX(4),RWMIN(5),RWMAX(5)                     KON 4740
      WRITE (6,80) ZWMIN(1),ZWMAX(1),ZWMIN(2),ZWMAX(2),ZWMIN(3),        KON 4750
     1 ZWMAX(3),ZWMIN(4),ZWMAX(4),ZWMIN(5),ZWMAX(5)                     KON 4760
  998 CONTINUE                                                          KON 4770
      PSYS = DRUCK * DFAK                                               KON 4780
C                                                                       KON 4790
      CALL VOLSTZ(B(KX(IVOL)),STZUK)                                    KON 4800
C                                                                       KON 4810
  999 CONTINUE                                                          KON 4820
C                                                                       KON 4830
CFZJ042                                                       09.09.05  KON 4840
      IF(ITIK(7) .EQ. 1) CALL TOPF(N200,NDR,NXS,B(KX(ITEML)),NENDK0,IMM,KON 4850
     1 NMM,RT,ZT,TT,B(KX(IDU)),TGRA,TFUL,B(KX(IKOMP)),MIX,VRP,VREG)     KON 4860
C                                                                       KON 4870
      RETURN                                                            KON 4880
      END                                                               KON 4890
      SUBROUTINE TOPF(N200,NDR,NXS,TEML,NENDK0,IMT,NMT,RADT,PHIT,TT,DU, TOP   10
     1 TGRA,TFUL,KOM,MIX,VRP,VREG)                                      TOP   20
C                                                                       TOP   30
CFZJ042                                                       09.09.05  TOP   40
C                                                                       TOP   50
C     UMBUCHEN DER MODERATOR-(GRAPHIT)-TEMPERATUREN AUF DAS CITATION-   TOP   60
C     NETZ                                                              TOP   70
C                                                                       TOP   80
      COMMON /BLOTIK/ N197,HCORE,NRAD,POWER,IX,JZ,ISP,NLIB,LAYER,DELZ,  TOP   90
     1 TIN,TOUT,LAY(20),RAD(20),JR(20),RZ(2,50),IZ(2,50),Q(50,50,2),    TOP  100
     2 C(3,25,25),ILA                                                   TOP  110
C                                                                       TOP  120
      COMMON /FORALL/ IRAGRO,JINPO1,UU,VV,ZGROB(500),POGROB(500),IJ     TOP  130
C                                                                       TOP  140
      COMMON /SPECTI/ ITIK(10)                                          TOP  150
C                                                                       TOP  160
CFZJ048 enlarged dimension                                    11.04.07  TOP  170
      COMMON /KONTHX/ F,NEU,JT0,JT1,JT2,NT0,NT1,NT2,K0,IADVT(4000,2),   TOP  180
     1 VOLVT(4000),PI                                                   TOP  190
C                                                                       TOP  200
      COMMON /BLINDL/ DUM(2),NGEOM,CIZET0                               TOP  210
C                                                                       TOP  220
      COMMON /VARDIM/ A(8000000)                                        TOP  230
C                                                                       TOP  240
CFZJ048 enlarged dimension                                    11.04.07  TOP  250
      COMMON /VARDIT/ B(5000000)                                        TOP  260
C                                                                       TOP  270
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       TOP  280
C                                                                       TOP  290
CFZJ042                                                       09.09.05  TOP  300
      COMMON /ADDRT/ KX(240),KY(240),LZ(240),NENDPT                     TOP  310
C                                                                       TOP  320
      COMMON /TRANS/ IFINST,INTVAL                                      TOP  330
C                                                                       TOP  340
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             TOP  350
C                                                                       TOP  360
      COMMON /COUPL/ IPRINT                                             TOP  370
C                                                                       TOP  380
CFZJ042                                                       09.09.05  TOP  390
      DIMENSION N1(90),N2(120),V1(120),FR(90),RM(90),FZ(120),ZM(120),   TOP  400
     1 KOMC(120,90),VRZC(120,90),TC(120,90),VLY(9999),TEMC(9999),RADT(1)TOP  410
     2 ,PHIT(1),NB(120,90),FLD(120,IMAZ),TEML(3,N200),TT(IMAZ,NMAZ),    TOP  420
     3 DU(IMAZ,NMAZ),TGRA(IMAZ,NMAZ),TFUL(IMAZ,NMAZ),KOM(IMAZ,NMAZ),    TOP  430
     4 MIX(NDR),VRP(NDR),VREG(NDR)                                      TOP  440
C                                                                       TOP  450
      EQUIVALENCE(LZ(11),INOPO),(LZ(13),INCOL),(LI(98),LTMO)            TOP  460
C                                                                       TOP  470
  102 FORMAT (24I3)                                                     TOP  480
  104 FORMAT (6(I3,F9.3))                                               TOP  490
  105 FORMAT (15I5)                                                     TOP  500
  120 FORMAT (I4,2X,24I5)                                               TOP  510
  121 FORMAT (/)                                                        TOP  520
C                                                                       TOP  530
C                                                                       TOP  540
      IF(NGEOM .NE. 5) REWIND NGEOM                                     TOP  550
C                                                                       TOP  560
CARD C4-1                                                               TOP  570
C                                                                       TOP  580
      READ (NGEOM,102) IDUM,IRI,ND                                      TOP  590
C                                                                       TOP  600
      IR = IRI - 1                                                      TOP  610
C                                                                       TOP  620
C     RADIAL: RM  REPRAESENTATIVE MITTELPUNKTE IN DEN CITATION-INTER-   TOP  630
C     VALLEN                                                            TOP  640
C                                                                       TOP  650
CARD C4-2                                                               TOP  660
C                                                                       TOP  670
      READ (NGEOM,104) (N1(I),V1(I),I=1,IR)                             TOP  680
C                                                                       TOP  690
      F1 = 0.                                                           TOP  700
      RI = 0.                                                           TOP  710
      MR = 0                                                            TOP  720
      DO 2 I=1,IR                                                       TOP  730
        NI = N1(I)                                                      TOP  740
        RI = RI + V1(I)                                                 TOP  750
        F2 = RI**2                                                      TOP  760
        F0 = (F2-F1) / NI                                               TOP  770
        DO 1 N=1,NI                                                     TOP  780
          MR = MR + 1                                                   TOP  790
          FR(MR) = F0                                                   TOP  800
          RM(MR) = SQRT(F1+(2*N-1)*F0/2.)                               TOP  810
    1   CONTINUE                                                        TOP  820
        F1 = F2                                                         TOP  830
    2 CONTINUE                                                          TOP  840
C                                                                       TOP  850
C     AXIAL: ZM  MITTELPUNKTE IN CITATION-INTERVALLEN                   TOP  860
C                                                                       TOP  870
CARD C4-2                                                               TOP  880
C                                                                       TOP  890
      READ (NGEOM,104) (N2(N),V1(N),N=1,ND)                             TOP  900
C                                                                       TOP  910
      MZ = 0                                                            TOP  920
      F1 = 0.                                                           TOP  930
      RI = 0.                                                           TOP  940
      DO 4 N=1,ND                                                       TOP  950
        NN = N2(N)                                                      TOP  960
        RI = RI + V1(N)                                                 TOP  970
        F2 = RI                                                         TOP  980
        F0 = (F2-F1) / NN                                               TOP  990
        DO 3 M=1,NN                                                     TOP 1000
          MZ = MZ + 1                                                   TOP 1010
          FZ(MZ) = F0                                                   TOP 1020
          ZM(MZ) = F1 + (2*M-1) * F0 / 2.                               TOP 1030
    3   CONTINUE                                                        TOP 1040
        F1 = F2                                                         TOP 1050
    4 CONTINUE                                                          TOP 1060
C                                                                       TOP 1070
C     BELEGUNG DES CITATION-GITTERS MIT LAYER-ID.NUMMERN                TOP 1080
C                                                                       TOP 1090
CARD C5-1                                                               TOP 1100
C                                                                       TOP 1110
      READ (NGEOM,102) IDUM                                             TOP 1120
C                                                                       TOP 1130
      DO 5 N=1,ND                                                       TOP 1140
C                                                                       TOP 1150
CARD C5-2                                                               TOP 1160
C                                                                       TOP 1170
        READ (NGEOM,105) (NB(N,I),I=1,IR)                               TOP 1180
    5 CONTINUE                                                          TOP 1190
C                                                                       TOP 1200
      DO 6 J=1,9999                                                     TOP 1210
        VLY(J) = 0.                                                     TOP 1220
        TEMC(J) = 0.                                                    TOP 1230
    6 CONTINUE                                                          TOP 1240
      ILA = 0                                                           TOP 1250
      MZ = 0                                                            TOP 1260
      DO 13 N=1,ND                                                      TOP 1270
        NN = N2(N)                                                      TOP 1280
        DO 12 K=1,NN                                                    TOP 1290
          MZ = MZ + 1                                                   TOP 1300
          MR = 0                                                        TOP 1310
          DO 11 I=1,IR                                                  TOP 1320
            NI = N1(I)                                                  TOP 1330
            DO 10 J=1,NI                                                TOP 1340
              MR = MR + 1                                               TOP 1350
              LY = NB(N,I)                                              TOP 1360
              KOMC(MZ,MR) = LY                                          TOP 1370
              ILA = MAX0(ILA,LY)                                        TOP 1380
              FV = FZ(MZ) * FR(MR) * PI                                 TOP 1390
              VRZC(MZ,MR) = FV                                          TOP 1400
              VLY(LY) = VLY(LY) + FV                                    TOP 1410
   10       CONTINUE                                                    TOP 1420
   11     CONTINUE                                                      TOP 1430
   12   CONTINUE                                                        TOP 1440
   13 CONTINUE                                                          TOP 1450
      DO 50 I=1,MR                                                      TOP 1460
        DO 50 N=1,MZ                                                    TOP 1470
          LY = KOMC(N,I)                                                TOP 1480
          VRZC(N,I) = VRZC(N,I) / VLY(LY)                               TOP 1490
   50 CONTINUE                                                          TOP 1500
      IF(ITIK(9) .GT. 0 .OR. INTVAL .GT. 1 .OR. IPRINT .LT. 2) GOTO 15  TOP 1510
      MX = MR                                                           TOP 1520
      IF(MR .GT. 24) MX = 24                                            TOP 1530
      WRITE (6,121)                                                     TOP 1540
      DO 14 N=1,MZ                                                      TOP 1550
        WRITE (6,120) N,(KOMC(N,I),I=1,MX)                              TOP 1560
   14 CONTINUE                                                          TOP 1570
      IF(MX .EQ. MR) GOTO 15                                            TOP 1580
      MX = MX + 1                                                       TOP 1590
      GOTO 18                                                           TOP 1600
   17 CONTINUE                                                          TOP 1610
      MX = MXP + 1                                                      TOP 1620
   18 CONTINUE                                                          TOP 1630
      MXP = MX + 23                                                     TOP 1640
      IF(MR .LT. MXP) MXP = MR                                          TOP 1650
      WRITE (6,121)                                                     TOP 1660
      DO 16 N=1,MZ                                                      TOP 1670
        WRITE (6,120) N,(KOMC(N,I),I=MX,MXP)                            TOP 1680
   16 CONTINUE                                                          TOP 1690
      IF(MR .GT. MXP) GOTO 17                                           TOP 1700
   15 CONTINUE                                                          TOP 1710
      IF(NGEOM .NE. 5) REWIND NGEOM                                     TOP 1720
C                                                                       TOP 1730
C     O-P-F-DATEN AUF CITATION-NETZ DURCH LINEARE INTERPOLATION         TOP 1740
C     (SIEHE SUBR. LAGRAL)                                              TOP 1750
C                                                                       TOP 1760
      DO 19 N=1,MZ                                                      TOP 1770
        ZM(N) = ZM(N) - CIZET0                                          TOP 1780
   19 CONTINUE                                                          TOP 1790
      IJ = NMT                                                          TOP 1800
      DO 32 I=1,IMT                                                     TOP 1810
        DO 30 N=1,NMT                                                   TOP 1820
          ZGROB(N) = PHIT(N)                                            TOP 1830
          OPF = TT(I,N)                                                 TOP 1840
          POGROB(N) = OPF                                               TOP 1850
   30   CONTINUE                                                        TOP 1860
        DO 31 N=1,MZ                                                    TOP 1870
          UU = ZM(N)                                                    TOP 1880
C                                                                       TOP 1890
          CALL LAGRAL                                                   TOP 1900
C                                                                       TOP 1910
          FLD(N,I) = VV                                                 TOP 1920
   31   CONTINUE                                                        TOP 1930
   32 CONTINUE                                                          TOP 1940
      IJ = IMT                                                          TOP 1950
      DO 42 N=1,MZ                                                      TOP 1960
        DO 40 I=1,IMT                                                   TOP 1970
          ZGROB(I) = RADT(I)                                            TOP 1980
          POGROB(I) = FLD(N,I)                                          TOP 1990
   40   CONTINUE                                                        TOP 2000
        DO 41 I=1,MR                                                    TOP 2010
          UU = RM(I)                                                    TOP 2020
C                                                                       TOP 2030
          CALL LAGRAL                                                   TOP 2040
C                                                                       TOP 2050
          TC(N,I) = VV                                                  TOP 2060
   41   CONTINUE                                                        TOP 2070
   42 CONTINUE                                                          TOP 2080
C                                                                       TOP 2090
C     O-P-F IN DEN CITATION-COMPOSITIONS                                TOP 2100
C                                                                       TOP 2110
      DO 51 I=1,MR                                                      TOP 2120
        DO 51 N=1,MZ                                                    TOP 2130
          LY = KOMC(N,I)                                                TOP 2140
          TEMC(LY) = TEMC(LY) + VRZC(N,I) * TC(N,I)                     TOP 2150
   51 CONTINUE                                                          TOP 2160
      NPK1 = NENDK0                                                     TOP 2170
      NENDK1 = NPK1 + NXS                                               TOP 2180
C                                                                       TOP 2190
CFZJ042                                                       09.09.05  TOP 2200
      CALL REFTEM(N200,NDR,NXS,A(KA(LTMO)),TEMC,B(KX(INOPO)),           TOP 2210
     1 B(KX(INCOL)),VLY,B(NPK1),TEML)                                   TOP 2220
C                                                                       TOP 2230
CFZJ042                                                       09.09.05  TOP 2240
      CALL VOLMA2(TEML,N200,DU,TGRA,TFUL,KOM)                           TOP 2250
C                                                                       TOP 2260
CFZJ042                                                       09.09.05  TOP 2270
      CALL LSDTX(N200,NXS,NENDK1,NDR,MIX,VRP,VREG)                      TOP 2280
C                                                                       TOP 2290
      RETURN                                                            TOP 2300
      END                                                               TOP 2310
      SUBROUTINE LAGRAL                                                 GRA   10
C                                                                       GRA   20
C     INTERPOLATION (LINEAR)                                            GRA   30
C                                                                       GRA   40
      COMMON /FORALL/ IRAGRO,JINPO1,UU,VV,ZGROB(500),POGROB(500),IJ,    GRA   50
     1 FIM(100),IFZ,IWO,LLL(30),CASE(18),EXPO(15),IFZR,IREP,LLLL,LTAB,  GRA   60
     2 RADX(50),EPSIL,ZFEIN(60),MSPALT,NTYPEN                           GRA   70
C                                                                       GRA   80
C                                                                       GRA   90
      IP = IJ                                                           GRA  100
      IF(ZGROB(IJ)-UU) 9,1,1                                            GRA  110
    1 CONTINUE                                                          GRA  120
      DO 2 II=1,IJ                                                      GRA  130
        IP = II                                                         GRA  140
        IF(ZGROB(IP)-UU) 2,3,4                                          GRA  150
    2 CONTINUE                                                          GRA  160
    3 VV = POGROB(IP)                                                   GRA  170
      RETURN                                                            GRA  180
    4 CONTINUE                                                          GRA  190
      IF(IP .EQ. 1) IP = 2                                              GRA  200
C                                                                       GRA  210
C     LINEARE INTERPOLATION                                             GRA  220
C                                                                       GRA  230
    9 CONTINUE                                                          GRA  240
      VV = POGROB(IP-1) + (POGROB(IP)-POGROB(IP-1)) / (ZGROB(IP)-       GRA  250
     1 ZGROB(IP-1)) * (UU-ZGROB(IP-1))                                  GRA  260
      RETURN                                                            GRA  270
      END                                                               GRA  280
      SUBROUTINE LSDTX(N200,NXS,NENDK1,NDR,MIX,VRP,VREG)                LSD   10
C                                                                       LSD   20
CFZJ042                                                       09.09.05  LSD   30
C                                                                       LSD   40
C     STEUERPROGRAMM FUER REFLEKTORTEMP. TMO, ZENTRALTEMP.,             LSD   50
C                    MIX-CORE-TABELLEN                                  LSD   60
C                                                                       LSD   70
      COMMON /VARDIM/ A(8000000)                                        LSD   80
C                                                                       LSD   90
CFZJ048 enlarged dimension                                    11.04.07  LSD  100
      COMMON /VARDIT/ B(5000000)                                        LSD  110
C                                                                       LSD  120
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       LSD  130
C                                                                       LSD  140
CFZJ042                                                       09.09.05  LSD  150
      COMMON /ADDRT/ KX(240),KY(240),LZ(240),NENDPT                     LSD  160
C                                                                       LSD  170
      COMMON /VRT/ MIXM                                                 LSD  180
C                                                                       LSD  190
CFZJ042                                                       09.09.05  LSD  200
      DIMENSION MIX(NDR),VRP(NDR),VREG(NDR)                             LSD  210
C                                                                       LSD  220
      EQUIVALENCE(LI(97),LTFU),(LI(98),LTMO),(LZ(1),IVL),(LZ(2),IBUR),  LSD  230
     1 (LZ(3),IDOS),(LZ(8),ITEML),(LZ(10),IPF),(LI(26),LAGEF)           LSD  240
C                                                                       LSD  250
C                                                                       LSD  260
      NPK2 = NENDK1                                                     LSD  270
CFZJ042                                                       09.09.05  LSD  280
      DO 1 M=1,MIXM                                                     LSD  290
C                                                                       LSD  300
        CALL REGTEM(M,N200,NXS,B(KX(IVL)),B(KX(IBUR)),B(KX(IDOS)),      LSD  310
     1   B(KX(ITEML)),B(KX(IPF)),A(KA(LTFU)),A(KA(LTMO)),B(NPK2),NDR,MIXLSD  320
     2   ,A(KA(LAGEF)),VRP,VREG)                                        LSD  330
C                                                                       LSD  340
    1 CONTINUE                                                          LSD  350
CFZJ042                                                       09.09.05  LSD  360
      N38 = 38                                                          LSD  370
      REWIND N38                                                        LSD  380
      RETURN                                                            LSD  390
      END                                                               LSD  400
      SUBROUTINE REFTEM(N200,NDR,NXS,TMO,TEMC,NOPOW,NCOLA,VLY,VO,TEML)  REF   10
C                                                                       REF   20
C     ERSTELLEN DER MODERATORTEMPERATUREN TMO IN DEN REFLEKTOREN        REF   30
C                                                                       REF   40
      COMMON /NORM/ PDUMMY,IMX,ICI(9999),IVS(9999),VFV(9999),VFC(9999)  REF   50
C                                                                       REF   60
CFZJ042                                                       09.09.05  REF   70
      DIMENSION TMO(NXS),TEMC(9999),NOPOW(N200),NCOLA(NDR),VLY(9999),   REF   80
     1 VO(NXS),TEML(3,N200)                                             REF   90
C                                                                       REF  100
C                                                                       REF  110
      DO 1 I=1,NXS                                                      REF  120
        VO(I) = 0.                                                      REF  130
        TMO(I) = 0.                                                     REF  140
    1 CONTINUE                                                          REF  150
      NX = 0                                                            REF  160
      DO 12 K=1,IMX                                                     REF  170
        LY = ICI(K)                                                     REF  180
        LV = IVS(K)                                                     REF  190
        N = NCOLA(LV)                                                   REF  200
        NS = NOPOW(N)                                                   REF  210
        IF(NS .EQ. 0) GOTO 12                                           REF  220
        NX = MAX0(NX,NS)                                                REF  230
        W = VLY(LY) * VFC(K)                                            REF  240
        TMO(NS) = TMO(NS) + TEMC(LY) * W                                REF  250
        VO(NS) = VO(NS) + W                                             REF  260
        TEML(1,LV) = TEMC(LY)                                           REF  270
   12 CONTINUE                                                          REF  280
      DO 3 NS=1,NX                                                      REF  290
        IF(VO(NS) .EQ. 0.) GOTO 3                                       REF  300
        TMO(NS) = TMO(NS) / VO(NS)                                      REF  310
    3 CONTINUE                                                          REF  320
      RETURN                                                            REF  330
      END                                                               REF  340
      SUBROUTINE ALPHAK(DROHR,QTV,LTV,XGEO,T,TFL,IFB,FZQ,DZ,KOM,ALPHA,  ALP   10
     1 ALFISO,XVER,STROM,EPSIL,DHYD,RHO,FRQ,MZ,MR)                      ALP   20
C                                                                       ALP   30
C     BERECHNET ALPHA*F (---> STROM(I,N)) FUER DIE MASCHEN DES          ALP   40
C     KONVEK- (STROEMUNGS-) GITTERS                                     ALP   50
C                                                                       ALP   60
      COMMON /PRANDL/ PRAN                                              ALP   70
C                                                                       ALP   80
      COMMON /KUEL1/ NKUEL,KKOMK(4)                                     ALP   90
C                                                                       ALP  100
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      ALP  110
C                                                                       ALP  120
      COMMON /VERTI/ NVERTI,KKOMV(30)                                   ALP  130
C                                                                       ALP  140
CFZJ055                                                       25.09.07  ALP  150
C                                                                       ALP  160
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             ALP  170
C                                                                       ALP  180
      DIMENSION DROHR(KOMAX),QTV(KOMAX),T(KONN,KONI),TFL(KONN,KONI),    ALP  190
     1 IFB(KONN,KONI),FZQ(KONI),DZ(KONN),KOM(KONN,KONI),ALPHA(KOMAX),   ALP  200
     2 ALFISO(KOMAX),XVER(KONI,30),STROM(KONN,KONI),EPSIL(KOMAX),       ALP  210
     3 DHYD(KOMAX),RHO(KONN,KONI),FRQ(KONN,KONI)                        ALP  220
C                                                                       ALP  230
      REAL MZ(KONN,KONI),MR(KONN,KONI),LTV(KOMAX)                       ALP  240
C                                                                       ALP  250
      INTEGER XGEO(KOMAX)                                               ALP  260
C                                                                       ALP  270
      REAL NUS,NUSL,NUSLL,NUSL1,NUSL2,NUST,NUSLAM,NUSTUR                ALP  280
C                                                                       ALP  290
  200 FORMAT (/' I=',I3,'**N=',I3,'**NV=',I3,'**IFB1=',I3,'**KK=',I3,'**ALP  300
     1K=',I3,'**XVER=',E10.3)                                           ALP  310
C                                                                       ALP  320
C                                                                       ALP  330
C     STROM ENTSPRICHT HIER ALPHA*F                                     ALP  340
C                                                                       ALP  350
      IM = IM1 + 1                                                      ALP  360
      NM = NM1 + 1                                                      ALP  370
      DO 150 I=1,IM                                                     ALP  380
        DO 150 N=1,NM                                                   ALP  390
          STROM(I,N) = 0.                                               ALP  400
  150 CONTINUE                                                          ALP  410
      DO 100 I=2,IM1                                                    ALP  420
        DO 100 N=2,NM1                                                  ALP  430
          IFB1 = IFB(I,N) + 1                                           ALP  440
          GOTO(100,50,80,300),IFB1                                      ALP  450
          GOTO 70                                                       ALP  460
   50     CONTINUE                                                      ALP  470
          KK = KOM(I,N)                                                 ALP  480
          EPSI = EPSIL(KK)                                              ALP  490
          TEM1 = (TFL(I,N)+T(I,N)) / 2.                                 ALP  500
          TEM2 = (TFL(I-1,N)+T(I-1,N)) / 2.                             ALP  510
          TEM3 = (TFL(I,N-1)+T(I,N-1)) / 2.                             ALP  520
          TEM4 = (TFL(I-1,N-1)+T(I-1,N-1)) / 2.                         ALP  530
          TEM = (TEM1+TEM2+TEM3+TEM4) / 4.                              ALP  540
C                                                                       ALP  550
          XLH = XLHE(TEM)                                               ALP  560
C                                                                       ALP  570
          RE = REYN(I,N,T,TFL,RHO,IFB,KOM,EPSIL,DHYD,FZQ,FRQ,MZ,MR)     ALP  580
C                                                                       ALP  590
C     DIE IN DER FUNCTION REYN(I,N) BERECHNETE RE BEZIEHT SICH AUF DEN  ALP  600
C     HYDR. DURCHMESSER UND DIE WAHRE GESCHWINDIGKEIT, NOETIG IST HIER  ALP  610
C     ABER RE BEZOGEN AUF DEN KUGELDM. UND DIE GESCHW. IM LEER GEDACH-  ALP  620
C     TEN BEHAELTER                                                     ALP  630
C                                                                       ALP  640
          RE = RE * (1.-EPSI) * 1.5                                     ALP  650
          IF(RE .GE. 2.) GOTO 60                                        ALP  660
          NUS = 4.03                                                    ALP  670
          GOTO 65                                                       ALP  680
   60     CONTINUE                                                      ALP  690
C                                                                       ALP  700
C     NUSSELT-GESETZ NACH KTA-REGELENTWURF  (RE>100)                    ALP  710
C                                                                       ALP  720
C     NUS = 1.27 * PRAN**0.33 * RE**0.36 / EPSI**1.18 +                 ALP  730
C      0.033 * PRAN**0.5 * RE**0.86 / EPSI**1.07                        ALP  740
C                                                                       ALP  750
          FAC1= PRAN**0.33                                              ALP  760
          FAC2= RE**0.36                                                ALP  770
          FAC3= EPSI**1.18                                              ALP  780
          FAC4= PRAN**0.5                                               ALP  790
          FAC5= RE**0.86                                                ALP  800
          FAC6= EPSI**1.07                                              ALP  810
          NUS= 1.27 * FAC1 * FAC2 / FAC3 + 0.033 * FAC4 * FAC5 / FAC6   ALP  820
   65     CONTINUE                                                      ALP  830
          NUS = NUS * ALPHA(KK)                                         ALP  840
          STROM(I,N) = NUS * XLH / DKUG * 100.                          ALP  850
          STROM(I,N) = STROM(I,N) * FZQ(N) * DZ(I) * (1.-EPSI) * 600. / ALP  860
     1     DKUG                                                         ALP  870
          GOTO 100                                                      ALP  880
   70     CONTINUE                                                      ALP  890
          KK = KOM(I,N)                                                 ALP  900
          AL = ALPHA(KK)                                                ALP  910
          GOTO 85                                                       ALP  920
   80     CONTINUE                                                      ALP  930
          KK = KOM(I,N)                                                 ALP  940
          DO 81 NV=1,NVERTI                                             ALP  950
            K = KKOMV(NV)                                               ALP  960
            IF(K .NE. KK) GOTO 81                                       ALP  970
            GOTO 82                                                     ALP  980
   81     CONTINUE                                                      ALP  990
   82     CONTINUE                                                      ALP 1000
          IF(XVER(N,NV) .LE. 1.E-3 .OR. XVER(N,NV) .GT. 100.) WRITE (6, ALP 1010
     1     200) I,N,NV,IFB1,KK,K,XVER(N,NV)                             ALP 1020
          DL = DHYD(KK) / 100. / XVER(N,NV)                             ALP 1030
          EPSI = EPSIL(KK)                                              ALP 1040
          TEM1 = (TFL(I,N)+T(I,N)) / 2.                                 ALP 1050
          TEM2 = (TFL(I-1,N)+T(I-1,N)) / 2.                             ALP 1060
          TEM3 = (TFL(I,N-1)+T(I,N-1)) / 2.                             ALP 1070
          TEM4 = (TFL(I-1,N-1)+T(I-1,N-1)) / 2.                         ALP 1080
          TEM = (TEM1+TEM2+TEM3+TEM4) / 4.                              ALP 1090
C                                                                       ALP 1100
          XLH = XLHE(TEM)                                               ALP 1110
C                                                                       ALP 1120
          RE = REYN(I,N,T,TFL,RHO,IFB,KOM,EPSIL,DHYD,FZQ,FRQ,MZ,MR)     ALP 1130
C                                                                       ALP 1140
          REUG = 2300.                                                  ALP 1150
          REOG = 10000.                                                 ALP 1160
          IF(RE .LE. REOG) GOTO 91                                      ALP 1170
C                                                                       ALP 1180
          NUS = NUST(RE,PRAN,DL)                                        ALP 1190
C                                                                       ALP 1200
          GOTO 95                                                       ALP 1210
C                                                                       ALP 1220
   91     NUSL = NUSL1(RE,PRAN,DL)                                      ALP 1230
C                                                                       ALP 1240
          IF(DL .LE. 0.1) GOTO 92                                       ALP 1250
C                                                                       ALP 1260
          NUSLL = NUSL2(RE,PRAN,DL)                                     ALP 1270
C                                                                       ALP 1280
          IF(NUSLL .GT. NUSL) NUSL = NUSLL                              ALP 1290
   92     CONTINUE                                                      ALP 1300
          IF(RE .GE. REUG) GOTO 93                                      ALP 1310
          NUS = NUSL                                                    ALP 1320
          GOTO 95                                                       ALP 1330
   93     A = (RE-REUG) / (REOG-REUG)                                   ALP 1340
C                                                                       ALP 1350
          B = NUST(RE,PRAN,DL)                                          ALP 1360
C                                                                       ALP 1370
          NUS = (1.-A) * NUSL + A * B                                   ALP 1380
   95     CONTINUE                                                      ALP 1390
          AL = NUS * XLH / DHYD(KK) * 100.                              ALP 1400
          GOTO 85                                                       ALP 1410
  300     CONTINUE                                                      ALP 1420
          KK = KOM(I,N)                                                 ALP 1430
          DO 301 NK=1,NKUEL                                             ALP 1440
            K = KKOMK(NK)                                               ALP 1450
            IF(K .NE. KK) GOTO 301                                      ALP 1460
            GOTO 302                                                    ALP 1470
  301     CONTINUE                                                      ALP 1480
  302     CONTINUE                                                      ALP 1490
          TEM1 = (TFL(I,N)+T(I,N)) / 2.                                 ALP 1500
          TEM2 = (TFL(I-1,N)+T(I-1,N)) / 2.                             ALP 1510
          TEM3 = (TFL(I,N-1)+T(I,N-1)) / 2.                             ALP 1520
          TEM4 = (TFL(I-1,N-1)+T(I-1,N-1)) / 2.                         ALP 1530
          TEM = (TEM1+TEM2+TEM3+TEM4) / 4.                              ALP 1540
C                                                                       ALP 1550
          XLH = XLHE(TEM)                                               ALP 1560
C                                                                       ALP 1570
C     RE BEZUEGLICH DER UEBERSTROEMLAENGE*****                          ALP 1580
C                                                                       ALP 1590
          BREY = REYN(I,N,T,TFL,RHO,IFB,KOM,EPSIL,DHYD,FZQ,FRQ,MZ,MR)   ALP 1600
C                                                                       ALP 1610
          RE = BREY / (4.*QTV(KK)/3.1415-1.)                            ALP 1620
          IF(RE .GT. 1.) GOTO 1000                                      ALP 1630
C                                                                       ALP 1640
C     NUSSELT-GESETZE IN DURCHSTROEMTEN ROHRBUENDELN(VDI)               ALP 1650
C                                                                       ALP 1660
          E = RE * PRAN                                                 ALP 1670
          E = E**0.333                                                  ALP 1680
          NUS = 0.75 * E                                                ALP 1690
          GOTO 999                                                      ALP 1700
 1000     CONTINUE                                                      ALP 1710
          E = PRAN**.333                                                ALP 1720
          NUSLAM = .664 * SQRT(RE) * E                                  ALP 1730
          E1 = RE**.8                                                   ALP 1740
          E2 = RE**(-.1)                                                ALP 1750
          E3 = PRAN**.666                                               ALP 1760
          NUSTUR = (.037*PRAN*E1) / (1.+2.443*E2*(E3-1.))               ALP 1770
          E1 = NUSLAM**2                                                ALP 1780
          E2 = NUSTUR**2                                                ALP 1790
          NUS = .3 + SQRT(E1+E2)                                        ALP 1800
  999     F1 = 4. * QTV(KK) * EPSI / 3.1415                             ALP 1810
          IF(XGEO(KK) .EQ. 1) GOTO 1100                                 ALP 1820
          FPSI = 1. + (1.9-1.8/LTV(KK)) / (F1-.4)                       ALP 1830
          GOTO 1200                                                     ALP 1840
 1100     FPSI = 1. + (1.87-1.7/LTV(KK)) / (F1-.4)                      ALP 1850
          IF(F1 .LT. 3 .AND. FPSI .LT. 1.5) FPSI = 1.5                  ALP 1860
 1200     NUS = NUS * FPSI                                              ALP 1870
          AL = NUS * XLH / (1.571*DROHR(KK)) * 100.                     ALP 1880
   85     CONTINUE                                                      ALP 1890
          IF(ALPHA(KK) .NE. 0.) AL = ALPHA(KK)                          ALP 1900
          STROM(I,N) = AL * 4. * EPSI / DHYD(KK) * 100. * FZQ(N) * DZ(I)ALP 1910
          IF(ALFISO(KK) .NE. 0.) STROM(I,N) = STROM(I,N) * ALFISO(KK)   ALP 1920
  100 CONTINUE                                                          ALP 1930
      RETURN                                                            ALP 1940
      END                                                               ALP 1950
      SUBROUTINE ALGAS(ALGA,RHO,MZ,MR)                                  ALG   10
C                                                                       ALG   20
C     BERECHNET EXPONENTIALFUNKTIONEN FUER QUASI-ANALYT. GASTEMPERATUR- ALG   30
C     BERECHNUNG. DIE BERECHNUNG ERFOLGT MASCHENWEISE, DA DIE EXP.-     ALG   40
C     FUNKTION IN BEIDEN KOORDINATENRICHT. GLEICH IST, WENN DIE ALFA*F  ALG   50
C     MIT DEN MASSENSTROMKOMPONENTEN GEWICHTET WERDEN.                  ALG   60
C                                                                       ALG   70
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      ALG   80
C                                                                       ALG   90
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             ALG  100
C                                                                       ALG  110
      DIMENSION ALGA(KONN,KONI,2),RHO(KONN,KONI)                        ALG  120
C                                                                       ALG  130
      REAL MZ(KONN,KONI),MR(KONN,KONI),M(2)                             ALG  140
C                                                                       ALG  150
C                                                                       ALG  160
      IM = IM1 + 1                                                      ALG  170
      NM = NM1 + 1                                                      ALG  180
      DO 100 I=1,IM                                                     ALG  190
        DO 100 N=1,NM                                                   ALG  200
          M(1) = ABS(MR(I,N))                                           ALG  210
          M(2) = ABS(MZ(I,N))                                           ALG  220
          A = RHO(I,N)                                                  ALG  230
          XBEZ = 0.                                                     ALG  240
          DO 110 K=1,2                                                  ALG  250
            XBEZ = XBEZ + M(K)                                          ALG  260
  110     CONTINUE                                                      ALG  270
          A2 = 0.                                                       ALG  280
          IF(XBEZ .EQ. 0.) GOTO 150                                     ALG  290
          A2 = A / XBEZ / CP                                            ALG  300
  150     CONTINUE                                                      ALG  310
          ALGA(I,N,1) = 0.                                              ALG  320
          ALGA(I,N,2) = 0.                                              ALG  330
          IF(A2 .GT. 30.) GOTO 105                                      ALG  340
          ALGA(I,N,1) = EXP(-A2)                                        ALG  350
          A1 = -0.5 * A2                                                ALG  360
          ALGA(I,N,2) = EXP(A1)                                         ALG  370
  105     RHO(I,N) = A2                                                 ALG  380
  100 CONTINUE                                                          ALG  390
      RETURN                                                            ALG  400
      END                                                               ALG  410
      SUBROUTINE BEZUG(FZQ,DZ,STRBEZ,IFQROW,KOM,VOL,STZUK)              BEZ   10
C                                                                       BEZ   20
C     BESTIMMUNG DES NETTO-MASSENSTROMS FUER KONSISTENZKRITERIUM UND    BEZ   30
C     MARKIERUNG DER REIHEN MIT MASSENSTROMQUELLEN (IFQROW=2) WIRD BEI  BEZ   40
C     KOPPLUNG MIT NETZWERK AUFGERUFEN                                  BEZ   50
C                                                                       BEZ   60
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      BEZ   70
C                                                                       BEZ   80
      COMMON /ZEITVR/ IFZDR,ZTF(3,100),ZST(3,100),ZDR(100),ZVOR(100),IFZBEZ   90
     1 ,IZKOM(3),IZKO,IFAGL,IZAEHL                                      BEZ  100
C                                                                       BEZ  110
CFZJ006 enlarged dimensions common QVAR                       28.11.03  BEZ  120
      COMMON /QVAR/ DUM(1511),N61,URZ,ZLEKA,ABXEN                       BEZ  130
C                                                                       BEZ  140
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             BEZ  150
C                                                                       BEZ  160
CFZJ042                                                       09.09.05  BEZ  170
      COMMON /COUPL/ IPRINT                                             BEZ  180
C                                                                       BEZ  190
      DIMENSION FZQ(KONI),DZ(KONN),STRBEZ(KONN),IFQROW(KONN),           BEZ  200
     1 KOM(KONN,KONI),VOL(KOMAX),STZUK(KOMAX)                           BEZ  210
C                                                                       BEZ  220
   84 FORMAT (/5X,'HEAT SINK (COMP.',I2,') = ',1PE9.2,'  KG/SEC')       BEZ  230
C                                                                       BEZ  240
C                                                                       BEZ  250
      IF(IFAGL .EQ. 0) GOTO 26                                          BEZ  260
C                                                                       BEZ  270
C     BERECHNUNG DES MASSENSTROMS DIESER REGION AUS DEN ANDEREN         BEZ  280
C                                                                       BEZ  290
      STSUM = 0.                                                        BEZ  300
      DO 25 K=1,KM                                                      BEZ  310
        IF(K .EQ. IFAGL) GOTO 25                                        BEZ  320
        STSUM = STSUM + STZUK(K)                                        BEZ  330
   25 CONTINUE                                                          BEZ  340
      STZUK(IFAGL) = -STSUM                                             BEZ  350
C                                                                       BEZ  360
CFZJ042                                                       09.09.05  BEZ  370
      IF (IPRINT .GE. 1) WRITE (6,84) IFAGL,STZUK(IFAGL)                BEZ  380
   26 CONTINUE                                                          BEZ  390
      DO 51 KK=1,KM                                                     BEZ  400
        IF(VOL(KK) .NE. 0.) STZUK(KK) = STZUK(KK) / VOL(KK)             BEZ  410
   51 CONTINUE                                                          BEZ  420
      STRB = 0.                                                         BEZ  430
      DO 52 I=2,IM1                                                     BEZ  440
        IFQROW(I) = 1                                                   BEZ  450
        DO 53 N=2,NM1                                                   BEZ  460
          KK = KOM(I,N)                                                 BEZ  470
          IF(STZUK(KK) .EQ. 0.) GOTO 53                                 BEZ  480
          IFQROW(I) = 2                                                 BEZ  490
          STRB = STRB + STZUK(KK) * FZQ(N) * DZ(I)                      BEZ  500
   53   CONTINUE                                                        BEZ  510
        STRBEZ(I) = STRB                                                BEZ  520
   52 CONTINUE                                                          BEZ  530
      RETURN                                                            BEZ  540
      END                                                               BEZ  550
      FUNCTION DMCORE(N)                                                DMC   10
C                                                                       DMC   20
C     ZUR BERECHNUNG DER QUERLEITFAEHIGKEIT                             DMC   30
C     DMCORE IST DER DURCHMESSER DES CORES                              DMC   40
C                                                                       DMC   50
      COMMON /BLOTIK/ N197,HCORE,NRAD,POWER,IX,JZ,ISP,NLIB,LAYER,DELZ,  DMC   60
     1 TIN,TOUT,LAY(20),RAT(20),JR(20),RZ(2,50),IZ(2,50),Q(50,50,2),    DMC   70
     2 A(3,25,25),ILA                                                   DMC   80
C                                                                       DMC   90
C                                                                       DMC  100
      DMCORE = 2. * RAT(NRAD)                                           DMC  110
C                                                                       DMC  120
      RETURN                                                            DMC  130
      END                                                               DMC  140
      SUBROUTINE EINL(IFZW,ITM3,DROHR,QTV,LTV,XGEO,NRHLR,IFB,XKK,MR,P,  EIN   10
     1 KOM,PVOR,IFBR,ALPHA,IFBQ,ALFISO,R,Z,FZQ,FRQ,DR,DZ,FZQ1,FRQ1,ZP,RPEIN   20
     2 ,EPSIL,XKON,DHYD,STZUK,TFLVOR,IFZST,IFZTF,VOL,IOVER,IUVER,XVER,  EIN   30
     3 IOKUL,IUKUL,XKUL,XZSUM,XNSUM1,TFL,STRBEZ,IFQROW,IPAR,JPAR,NPAR)  EIN   40
C                                                                       EIN   50
C     EINLESEPROGRAMM FUER STROEMUNGS- UND GASTEMPERATURTEIL            EIN   60
C                                                                       EIN   70
CFZJ042                                                       09.09.05  EIN   80
      COMMON /KUEL1/ NKUEL,KKOMK(4)                                     EIN   90
C                                                                       EIN  100
      COMMON /VERTI/ NVERTI,KKOMV(30)                                   EIN  110
C                                                                       EIN  120
      COMMON /PRANDL/ PRAN                                              EIN  130
C                                                                       EIN  140
      COMMON /HOHLR/ KPAR(10),KKOM(10),NHLR,PHOHL(10),NML(10),NMR(10),  EIN  150
     1 IL(10),VOLU(10)                                                  EIN  160
C                                                                       EIN  170
      COMMON /TABB/ EPSI4                                               EIN  180
C                                                                       EIN  190
      COMMON /ITER/ IB,IT,IT1,IT2,ITM1,ITM2,EPSI1,EPSI2,IFSQ,OVM1,OVM2  EIN  200
C                                                                       EIN  210
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      EIN  220
C                                                                       EIN  230
      COMMON /GEO/ TITLE(20)                                            EIN  240
C                                                                       EIN  250
      COMMON /SPEZI/ FF                                                 EIN  260
C                                                                       EIN  270
      COMMON /ZEITVR/ IFZDR,ZTF(3,100),ZST(3,100),ZDR(100),ZVOR(100),IFZEIN  280
     1 ,IZKOM(3),IZKO,IFAGL,IZAEHL                                      EIN  290
C                                                                       EIN  300
      COMMON /RZCOMP/ IREARZ,MCR,MCZ,ICONVC,RR,ZZ,IR,NN,KKO             EIN  310
C                                                                       EIN  320
      COMMON /PRINT1/ TITEL(20)                                         EIN  330
C                                                                       EIN  340
      COMMON /OPT/ KENN,IOPUT,NIFKO,DZT,KONIN                           EIN  350
C                                                                       EIN  360
CFZJ006 enlarged dimensions common QVAR                       28.11.03  EIN  370
      COMMON /QVAR/ DUM(1820),TAU                                       EIN  380
C                                                                       EIN  390
      COMMON /MPUNKT/ IMPU,KMPU,EMPU,TTTEIN,TTTAUS                      EIN  400
C                                                                       EIN  410
      COMMON /MPUTA/ TEIMIN,TEIMAX,EMP0,TAU0,MPUTAU,QWU,DTAU,TEI0       EIN  420
C                                                                       EIN  430
      COMMON /SPECTI/ IDUM(30),TXNEW                                    EIN  440
C                                                                       EIN  450
      COMMON /TRANS/ IFINST                                             EIN  460
C                                                                       EIN  470
CFZJ055                                                       25.09.07  EIN  480
C                                                                       EIN  490
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             EIN  500
C                                                                       EIN  510
      COMMON /COUPL/ IPRINT                                             EIN  520
C                                                                       EIN  530
      DIMENSION DROHR(KOMAX),QTV(KOMAX),XGEO(KOMAX),NRHLR(KOMAX),       EIN  540
     1 IFB(KONN,KONI),XKK(KONN,KONI),P(KONN,KONI),KOM(KONN,KONI),       EIN  550
     2 PVOR(KOMAX),IFBR(KOMAX),ALPHA(KOMAX),IFBQ(KOMAX),ALFISO(KOMAX),  EIN  560
     3 R(KONI),Z(KONN),FZQ(KONI),FRQ(KONN,KONI),DR(KONI),DZ(KONN),      EIN  570
     4 FZQ1(KONI),FRQ1(KONN,KONI),ZP(KONN),RP(KONI),EPSIL(KOMAX),       EIN  580
     5 XKON(KOMAX),DHYD(KOMAX),STZUK(KOMAX),TFLVOR(KOMAX),IFZST(KOMAX), EIN  590
     6 IFZTF(KOMAX),VOL(KOMAX),IOVER(KONI,30),IUVER(KONI,30),           EIN  600
     7 XVER(KONI,30),IOKUL(KONI,4),IUKUL(KONI,4),XKUL(KONI,4),          EIN  610
     8 XZSUM(KOMAX),XNSUM1(KOMAX),TFL(KONN,KONI),STRBEZ(KONN),          EIN  620
     9 IFQROW(KONN),IPAR(KONI*2,10),JPAR(KONI*2,10),NPAR(KONI*2,10)     EIN  630
C                                                                       EIN  640
      REAL LTV(KOMAX),MR(KONN,KONI)                                     EIN  650
C                                                                       EIN  660
CFZJ042                                                       09.09.05  EIN  670
      CHARACTER*4 U(4)/'COMP','OSIT','IONS','    '/,TTL(2)/'KONV','EK :'EIN  680
     1 /,TITLE,TITEL                                                    EIN  690
C                                                                       EIN  700
    5 FORMAT (20A4)                                                     EIN  710
    6 FORMAT (1H1/35X,'***   K  O  N  V  E  K - INPUT CHECK   ***'///5X,EIN  720
     1 'CASE :  ',20A4//)                                               EIN  730
    8 FORMAT (//5X,'DKUG',7X,'EPSI',6X,'PRESS.',3X,'REVOR',8X,'CP',9X,  EIN  740
     1 'PRANDTL'/5X,'(CM)',17X,'(BAR)',15X,'(J/KG/K)',4X,'(J/KG/K)'/5X, EIN  750
     2 61(1H-))                                                         EIN  760
    9 FORMAT (7(F9.2,2X))                                               EIN  770
   10 FORMAT (4E10.3,4I5,F5.2,2I5)                                      EIN  780
   11 FORMAT (//5X,'GRID DATA:'//5X,I3,' AXIAL *',I4,' RADIAL GRID POINTEIN  790
     1S ,',I3,' COMPOSITIONS, REL. LOCATION OF GRID: Z0 =',F7.1,' , R0 =EIN  800
     2',F7.1)                                                           EIN  810
   20 FORMAT (2E10.3,I1,E9.2,3E10.3)                                    EIN  820
   22 FORMAT ('     DATA OUTPUT OF SPTRAN ON FT',I2//)                  EIN  830
   26 FORMAT (' **ERROR** MORE THAN 1 REGION WITH IFZST = 2')           EIN  840
   30 FORMAT (4I5,4E10.3)                                               EIN  850
   33 FORMAT (/5X,'TIME-DEPENDENT CALCULATION OF SYSTEM PRESSURE')      EIN  860
   35 FORMAT (' **WARNING** IFZDR=',I5,' IGNORED')                      EIN  870
   46 FORMAT (' **WARNING**: COMP.NO. ',I2,' IS A COUPLING COMPONENT FOREIN  880
     1 NETWORK --> MASS FLOW SOURCE ',F8.2,' KG/SEC IGNORED')           EIN  890
   62 FORMAT (/5X,'COMP',2X,'TYPE',16X,'XKR',5X,'PVOR',3X,'ALPHA*F',5X, EIN  900
     1 'EPSI',6X,'DHYD',5X,'QTV',4X,'LTV',4X,'DROHR',2X,'XGEO',4X,'SOURCEIN  910
     2E',6X,'TFL',6X,'IFBQ'/29X,'(1/CM)',2X,'(N/M**2)',2X,'(W/K)',16X,  EIN  920
     3 '(CM)',19X,'(CM)',11X,'(KG/S)',4X,'(DEG C)'/5X,126(1H-))         EIN  930
   68 FORMAT (4I10)                                                     EIN  940
   69 FORMAT (' **ERROR** AT IZK1')                                     EIN  950
   70 FORMAT (I5,I2,I3,4E10.3,E8.2,I3,E9.2,I1,E9.2)                     EIN  960
   71 FORMAT (8F10.6)                                                   EIN  970
   72 FORMAT (//5X,'TIME-DEPENDENT INPUT OF CONVECTION:',5X,I3,' TIME-STEIN  980
     1EPS ARE READ'//5X,'NO   TIME    PRESS.  COMP   SOURCE    TFL'/5X, EIN  990
     2 '    (MIN)   (BAR)          (KG/S)  (DEG C)'/5X,'----------------EIN 1000
     3---------------------------')                                     EIN 1010
   78 FORMAT (' **ERROR** MORE THAN 3 REGIONS WITH MASS FLOW SOURCES')  EIN 1020
   81 FORMAT (4X,I3,1X,1PE7.1,1X,E8.2,I5,4X,0PF7.2,1X,F7.2)             EIN 1030
   82 FORMAT (//5X,'COMP',37X,'ALFISO'/36X,'FACTOR TO CHANGE CALC.ALPHA'EIN 1040
     1 /5X,63(1H-))                                                     EIN 1050
   83 FORMAT (5X,I3,36X,'* ',F6.4)                                      EIN 1060
  105 FORMAT (1H+,T103,'TIME-L. VAR.')                                  EIN 1070
  107 FORMAT (1H+,T115,'TIME-L. VAR.')                                  EIN 1080
  110 FORMAT (5I6,7E6.0)                                                EIN 1090
  120 FORMAT (8F9.3)                                                    EIN 1100
  150 FORMAT (1H+,T103,'SINK = -SUM OF SOURCES')                        EIN 1110
  151 FORMAT (1H+,T103,'TIME-L.VAR. TIME-L.VAR.')                       EIN 1120
  153 FORMAT (' **ERROR** IFZST=',I3)                                   EIN 1130
  522 FORMAT (/5X,'CALCULATION OF SYSTEM PRESSURE BY MEANS OF INVENTORY EIN 1140
     1AND GAS TEMPERATURE ACCORDING TO IDEAL GAS LAW')                  EIN 1150
  601 FORMAT (1H1)                                                      EIN 1160
  811 FORMAT (23X,I6,4X,F7.2,1X,F7.2)                                   EIN 1170
 2002 FORMAT (/5X,'PROGRAM OPERATING DATA :'//5X,'EPSI1 =',1PE9.2,' , EPEIN 1180
     1SI2 =',1PE9.2,' , EPSI4 =',1PE9.2/5X,'OVM1  =',1PE9.2,' , OVM2  ='EIN 1190
     2 ,1PE9.2,' , IFSQ  =',I4/5X,'ITM1  =',I5,'     , ITM2  =',I5,'    EIN 1200
     3 , ITM3  =',I4)                                                   EIN 1210
 6007 FORMAT (' **ERROR** IFBR(',I2,') =',I3)                           EIN 1220
 6100 FORMAT (5X,I3,3X,'NO FLOW THROUGH')                               EIN 1230
 6101 FORMAT (5X,I3,3X,'BED OF PEBBLES  ',11X,F4.1,4X,'* ',F4.2,6X,F4.2,EIN 1240
     1 T129,I2)                                                         EIN 1250
 6102 FORMAT (5X,I3,3X,'VERT. FLOW-PIPES',F8.4,2X,F5.1,3X,E9.2,4X,F4.2, EIN 1260
     1 5X,F5.1,T129,I2)                                                 EIN 1270
 6103 FORMAT (5X,I3,3X,'CROSS.FL.PI-BUND',F8.4,2X,F5.1,16X,F4.2,5X,F5.1,EIN 1280
     1 3X,F4.2,3X,F4.2,4X,F5.2,F6.2,T129,I2)                            EIN 1290
 6104 FORMAT (5X,I3,3X,'PIPE-BUNDLE (2D)',8X,2X,F5.1,16X,F4.2,5X,F5.1,3XEIN 1300
     1 ,F4.2,3X,F4.2,4X,F5.2,F6.2,T129,I2)                              EIN 1310
 6105 FORMAT (5X,I3,3X,'HORIZONTAL VOID ',F8.4,2X,F5.1,3X,E9.2,4X,F4.2, EIN 1320
     1 5X,F5.1,T129,I2)                                                 EIN 1330
 6106 FORMAT (' **ERROR** TYPE 6 OF COMPOSITION DOES NOT EXIST')        EIN 1340
 6108 FORMAT (1H+,T106,F8.2)                                            EIN 1350
 6109 FORMAT (1H+,T106,F8.2,4X,F6.1)                                    EIN 1360
 6111 FORMAT (' **WARNING** ALPHA IN BED OF PEBBLES IS ZERO --> NO CONVEEIN 1370
     1CTIVE HEAT SOURCE')                                               EIN 1380
 6112 FORMAT (5X,I3,3X,'VERT. FLOW-PIPES',8X,2X,F5.1,6X,4X,6X,F4.2,5X,  EIN 1390
     1 F5.1,T129,I2)                                                    EIN 1400
 6115 FORMAT (5X,I3,3X,'HORIZONTAL VOID ',8X,2X,F5.1,6X,4X,6X,F4.2,5X,  EIN 1410
     1 F5.1,T129,I2)                                                    EIN 1420
 6116 FORMAT (' **ERROR** BED OF PEBBLES WITH 100% BLANK VOLUME --> CALCEIN 1430
     1ULATION OF PRESSURE FIELD IS IMPOSSIBLE')                         EIN 1440
 6119 FORMAT (1H+,T119,F6.1)                                            EIN 1450
 6122 FORMAT (5X,I3,3X,'VERT. FLOW-PIPES',F8.4,2X,F5.1,6X,4X,6X,F4.2,5X,EIN 1460
     1 F5.1,T129,I2)                                                    EIN 1470
 6123 FORMAT (5X,I3,3X,'CROSS.FL.PI-BUND',8X,2X,F5.1,16X,F4.2,5X,F5.1,3XEIN 1480
     1 ,F4.2,3X,F4.2,4X,F5.2,F6.2,T129,I2)                              EIN 1490
 6125 FORMAT (5X,I3,3X,'HORIZONTAL VOID ',8X,2X,F5.1,3X,E9.2,4X,F4.2,5X,EIN 1500
     1 F5.1,T129,I2)                                                    EIN 1510
 6132 FORMAT (5X,I3,3X,'VERT. FLOW-PIPES',8X,2X,F5.1,3X,E9.2,4X,F4.2,5X,EIN 1520
     1 F5.1,T129,I2)                                                    EIN 1530
 6135 FORMAT (5X,I3,3X,'HORIZONTAL VOID ',F8.4,2X,F5.1,6X,4X,6X,F4.2,5X,EIN 1540
     1 F5.1,T129,I2)                                                    EIN 1550
 6666 FORMAT (//5X,'DESCRIPTION OF COMPOSITIONS:')                      EIN 1560
CFZJ055                                                       25.09.07  EIN 1570
 6701 FORMAT (' **ERROR** COMPOSITION CARD NO.',I3,' LIES INCORRECT')   EIN 1580
C                                                                       EIN 1590
C                                                                       EIN 1600
      PI = 3.14159                                                      EIN 1610
      IT = 0                                                            EIN 1620
      TITLE(1) = TTL(1)                                                 EIN 1630
      TITLE(2) = TTL(2)                                                 EIN 1640
      DO 1 I=3,20                                                       EIN 1650
        TITLE(I) = TITEL(I)                                             EIN 1660
    1 CONTINUE                                                          EIN 1670
      WRITE (6,6) TITLE                                                 EIN 1680
CFZJ042                                                       09.09.05  EIN 1690
      IFSQ = 0                                                          EIN 1700
C                                                                       EIN 1710
C     KONVERGENZKRITERIUM FUER GASTEMPERATURBERECHNUNG                  EIN 1720
C                                                                       EIN 1730
      IF(EPSI1 .EQ. 0.) EPSI1 = 1.E-5                                   EIN 1740
C                                                                       EIN 1750
C     KONVERGENZKRITERIUM FUER STROEMUNGSBER.                           EIN 1760
C                                                                       EIN 1770
      IF(EPSI2 .EQ. 0.) EPSI2 = .01                                     EIN 1780
C                                                                       EIN 1790
C     EXTRAPOLATION                                                     EIN 1800
C                                                                       EIN 1810
      IF(OVM1 .EQ. 0.) OVM1 = .5                                        EIN 1820
C                                                                       EIN 1830
C     REL.-FAKTOR FUER GASTEMP. - UND STR.-FELD-BERECHNUNG              EIN 1840
C                                                                       EIN 1850
      OVM2 = 1.0                                                        EIN 1860
C                                                                       EIN 1870
C     MAX ITERATIONSZAHL FUER GASTEMPERATURBERECHNUNG                   EIN 1880
C                                                                       EIN 1890
      IF(ITM1 .EQ. 0) ITM1 = 100                                        EIN 1900
C                                                                       EIN 1910
C     MAX ITERATIONSZAHL FUER STROEMUNGSBER.                            EIN 1920
C                                                                       EIN 1930
      IF(ITM2 .EQ. 0) ITM2 = 500                                        EIN 1940
C                                                                       EIN 1950
C     MAX. ANZAHL DER STROEMUNGSFELD-GASTEMPERATURFELD-ITER.            EIN 1960
C                                                                       EIN 1970
      IF(ITM3 .EQ. 0) ITM3 = 5                                          EIN 1980
C                                                                       EIN 1990
C     ZUL. AENDERUNG DER MITLL. GASTEMP. PRO ITERATION DES STR.-FELDES  EIN 2000
C                                                                       EIN 2010
      IF(EPSI4 .EQ. 0.) EPSI4 = .02                                     EIN 2020
      FF = 1.                                                           EIN 2030
      WRITE (6,2002) EPSI1,EPSI2,EPSI4,OVM1,OVM2,IFSQ,ITM1,ITM2,ITM3    EIN 2040
      IFZ = 0                                                           EIN 2050
      REVOR = 0.                                                        EIN 2060
      IF(IFZDR .NE. 1) GOTO 32                                          EIN 2070
      IFZ = 1                                                           EIN 2080
      DRUCK = 1.E10                                                     EIN 2090
      GOTO 34                                                           EIN 2100
   32 CONTINUE                                                          EIN 2110
      IF(IFZDR .EQ. 0 .OR. IFZDR .EQ. 2) GOTO 34                        EIN 2120
      WRITE (6,35) IFZDR                                                EIN 2130
C                                                                       EIN 2140
      CALL ABEND(2)                                                     EIN 2150
C                                                                       EIN 2160
   34 CONTINUE                                                          EIN 2170
      IF(PRAN .EQ. 0.) PRAN = .66                                       EIN 2180
      IF(CP .EQ. 0.) CP = 5195.                                         EIN 2190
      IF(EPSI .EQ. 0.) EPSI = 0.39                                      EIN 2200
      IF(DKUG .EQ. 0.) DKUG = 6.                                        EIN 2210
      IF(REVOR .EQ. 0.) REVOR = 20000.                                  EIN 2220
      WRITE (6,8)                                                       EIN 2230
      IM1 = NN + 1                                                      EIN 2240
      NM1 = IR + 1                                                      EIN 2250
      R0 = RR                                                           EIN 2260
      Z0 = ZZ                                                           EIN 2270
      IFZW = 0                                                          EIN 2280
      KM = KKO                                                          EIN 2290
      WRITE (6,9) DKUG,EPSI,DRUCK,REVOR,CP,PRAN                         EIN 2300
      IF(IFZDR .EQ. 1) WRITE (6,33)                                     EIN 2310
      IF(IFZDR .EQ. 2) WRITE (6,522)                                    EIN 2320
      WRITE (6,11) IM1,NM1,KM,Z0,R0                                     EIN 2330
      IM = IM1 + 1                                                      EIN 2340
      NM = NM1 + 1                                                      EIN 2350
      IF(KONIN .GT. 0) GOTO 990                                         EIN 2360
      ZP(1) = Z0                                                        EIN 2370
      RP(1) = R0                                                        EIN 2380
      DO 55 I=2,IM                                                      EIN 2390
        ZP(I) = ZP(I-1) + DZ(I)                                         EIN 2400
   55 CONTINUE                                                          EIN 2410
      DO 56 N=2,NM                                                      EIN 2420
        RP(N) = RP(N-1) + DR(N)                                         EIN 2430
   56 CONTINUE                                                          EIN 2440
      DO 50 N=2,NM1                                                     EIN 2450
        FZQ(N) = (PI*2.*R(N)*DR(N)) * 1.E-4                             EIN 2460
        DO 50 I=2,IM1                                                   EIN 2470
          FRQ(I,N) = (PI*2.*R(N)*DZ(I)) * 1.E-4                         EIN 2480
          FRQ1(I,N) = (PI*R(N)*(DZ(I)+DZ(I+1))) * 1.E-4                 EIN 2490
   50 CONTINUE                                                          EIN 2500
      FZQ(1) = 0.                                                       EIN 2510
      FZQ(NM) = 0.                                                      EIN 2520
      DO 52 N=1,NM1                                                     EIN 2530
        FZQ1(N) = (FZQ(N)+FZQ(N+1)) / 2.                                EIN 2540
   52 CONTINUE                                                          EIN 2550
      DO 51 N=1,NM1                                                     EIN 2560
        FRQ1(1,N) = (PI*R(N)*DZ(2)) * 1.E-4                             EIN 2570
   51 CONTINUE                                                          EIN 2580
      DO 85 I=1,IM                                                      EIN 2590
        DO 85 N=1,NM                                                    EIN 2600
          IFB(I,N) = 1                                                  EIN 2610
   85 CONTINUE                                                          EIN 2620
      REK = .02 * DKUG / 3.                                             EIN 2630
      PK = 3. / (.04*DKUG)                                              EIN 2640
C                                                                       EIN 2650
      CALL SETK(IM,NM,KOM,KM,MR)                                        EIN 2660
C                                                                       EIN 2670
  990 CONTINUE                                                          EIN 2680
      WRITE (6,6666)                                                    EIN 2690
      WRITE (6,62)                                                      EIN 2700
      NVERTI = 0                                                        EIN 2710
      NHLR = 0                                                          EIN 2720
      NKUEL = 0                                                         EIN 2730
      IZKO = 0                                                          EIN 2740
      IFAGL = 0                                                         EIN 2750
      TEI0 = 0.                                                         EIN 2760
      DO 60 K=1,KM                                                      EIN 2770
        KR = K                                                          EIN 2780
        ALFISO(K) = 0.                                                  EIN 2790
        NRHLR(K) = 0                                                    EIN 2800
        IF(PVOR(KR) .EQ. -1.) PVOR(KR) = DRUCK                          EIN 2810
        IF(ALPHA(KR) .GE. 0.) GOTO 12                                   EIN 2820
        ALFISO(KR) = ABS(ALPHA(KR))                                     EIN 2830
        ALPHA(KR) = 0.                                                  EIN 2840
   12   CONTINUE                                                        EIN 2850
        IF(TFLVOR(KR) .NE. 0.) TEI0 = ABS(TFLVOR(KR))                   EIN 2860
        IF(TFLVOR(KR) .LT. 0.) TFLVOR(KR) = TAU                         EIN 2870
        IF(STZUK(KR) .LE. 0.) GOTO 61                                   EIN 2880
        KMPU = KR                                                       EIN 2890
        EMPU = STZUK(KR)                                                EIN 2900
        TTTEIN = TFLVOR(KR)                                             EIN 2910
        TTTAUS = TTTEIN + DTAU                                          EIN 2920
   61   CONTINUE                                                        EIN 2930
        IF(IFBR(KR) .EQ. 3 .OR. IFBR(K) .EQ. 4) GOTO 600                EIN 2940
        IF(IFZST(KR) .NE. 1 .AND. IFZTF(KR) .NE. 1) GOTO 31             EIN 2950
        IF(IFZST(KR) .EQ. 1) STZUK(KR) = 1.E10                          EIN 2960
        IF(IFZTF(KR) .EQ. 1) TFLVOR(KR) = 1.E10                         EIN 2970
        IFZ = 1                                                         EIN 2980
        IZKO = IZKO + 1                                                 EIN 2990
   31   CONTINUE                                                        EIN 3000
        IF(IFZST(KR) .NE. 2) GOTO 25                                    EIN 3010
        IF(IFAGL .EQ. 0) GOTO 8918                                      EIN 3020
        WRITE (6,26)                                                    EIN 3030
C                                                                       EIN 3040
        CALL ABEND(3)                                                   EIN 3050
C                                                                       EIN 3060
 8918   IFAGL = KR                                                      EIN 3070
   25   CONTINUE                                                        EIN 3080
        IF(IFBR(KR) .NE. 5) GOTO 64                                     EIN 3090
        NHLR = NHLR + 1                                                 EIN 3100
        NRHLR(KR) = NHLR                                                EIN 3110
        KKOM(NHLR) = KR                                                 EIN 3120
   64   CONTINUE                                                        EIN 3130
        IF(IFBR(KR) .NE. 2) GOTO 65                                     EIN 3140
        NVERTI = NVERTI + 1                                             EIN 3150
        KKOMV(NVERTI) = KR                                              EIN 3160
   65   CONTINUE                                                        EIN 3170
        IF(EPSIL(KR) .EQ. 0.) EPSIL(KR) = EPSI                          EIN 3180
        VOL(KR) = 0.                                                    EIN 3190
        IF(IFBR(KR) .NE. 0) GOTO 59                                     EIN 3200
        PVOR(KR) = 0.                                                   EIN 3210
        XKON(KR) = 0.                                                   EIN 3220
        GOTO 59                                                         EIN 3230
  600   CONTINUE                                                        EIN 3240
        XGEO(KR) = STZUK(KR)                                            EIN 3250
        IF(XGEO(KR) .EQ. 0. .AND. IFZST(KR) .EQ. 1) XGEO(KR) = IFZST(KR)EIN 3260
        IFZST(KR) = 0                                                   EIN 3270
        IFZTF(KR) = 0                                                   EIN 3280
        STZUK(KR) = 0.                                                  EIN 3290
        TFLVOR(KR) = 0.                                                 EIN 3300
        VOL(KR) = 0.                                                    EIN 3310
        DROHR(KR) = ALPHA(KR)                                           EIN 3320
        ALPHA(KR) = 0.                                                  EIN 3330
        QTV(KR) = EPSIL(KR)                                             EIN 3340
        LTV(KR) = DHYD(KR)                                              EIN 3350
        EPSIL(KR) = 1. - 3.1415 / (4.*QTV(KR)*LTV(KR))                  EIN 3360
        DHYD(KR) = (4*QTV(KR)/3.1415-1.) * DROHR(KR)                    EIN 3370
        IF(IFBR(KR) .EQ. 4) GOTO 59                                     EIN 3380
        NKUEL = NKUEL + 1                                               EIN 3390
        KKOMK(NKUEL) = KR                                               EIN 3400
   59   CONTINUE                                                        EIN 3410
        IF1 = IFBR(KR) + 1                                              EIN 3420
        GOTO(6000,6001,6002,6003,6004,6005,6006),IF1                    EIN 3430
        WRITE (6,6007) KR,IFBR(KR)                                      EIN 3440
C                                                                       EIN 3450
        CALL ABEND(3)                                                   EIN 3460
C                                                                       EIN 3470
        GOTO 60                                                         EIN 3480
 6000   WRITE (6,6100) KR                                               EIN 3490
        GOTO 60                                                         EIN 3500
 6001   WRITE (6,6101) KR,PVOR(KR),ALPHA(KR),EPSIL(KR),IFBQ(KR)         EIN 3510
        FF = 1. - EPSIL(KR)                                             EIN 3520
        IF(EPSIL(KR) .LT. 1.) GOTO 6114                                 EIN 3530
        WRITE (6,6116)                                                  EIN 3540
C                                                                       EIN 3550
        CALL ABEND(3)                                                   EIN 3560
C                                                                       EIN 3570
 6114   IF(ALPHA(KR) .NE. 0.) GOTO 58                                   EIN 3580
        WRITE (6,6111)                                                  EIN 3590
C                                                                       EIN 3600
        CALL ABEND(2)                                                   EIN 3610
C                                                                       EIN 3620
        GOTO 58                                                         EIN 3630
 6002   CONTINUE                                                        EIN 3640
C                                                                       EIN 3650
C     DIESES STATEMENT GILT NUR FUER BLOCK (CORE=VERT.STROMR.):         EIN 3660
C       FF = 1. - EPSIL(KR)                                             EIN 3670
C                                                                       EIN 3680
        IF(XKON(KR) .EQ. 0. .AND. ALPHA(KR) .EQ. 0.) GOTO 6151          EIN 3690
        GOTO 6161                                                       EIN 3700
 6151   WRITE (6,6112) KR,PVOR(KR),EPSIL(KR),DHYD(KR),IFBQ(KR)          EIN 3710
        GOTO 58                                                         EIN 3720
 6161   IF(ALPHA(KR) .EQ. 0.) WRITE (6,6122) KR,XKON(KR),PVOR(KR),      EIN 3730
     1   EPSIL(KR),DHYD(KR),IFBQ(KR)                                    EIN 3740
        IF(XKON(KR) .EQ. 0.) WRITE (6,6132) KR,PVOR(KR),ALPHA(KR),      EIN 3750
     1   EPSIL(KR),DHYD(KR),IFBQ(KR)                                    EIN 3760
        IF(XKON(KR) .EQ. 0. .OR. ALPHA(KR) .EQ. 0.) GOTO 58             EIN 3770
        WRITE (6,6102) KR,XKON(KR),PVOR(KR),ALPHA(KR),EPSIL(KR),DHYD(KR)EIN 3780
     1   ,IFBQ(KR)                                                      EIN 3790
        GOTO 58                                                         EIN 3800
 6003   CONTINUE                                                        EIN 3810
        IF(XKON(KR) .NE. 0.) GOTO 6113                                  EIN 3820
        WRITE (6,6123) KR,PVOR(KR),EPSIL(KR),DHYD(KR),QTV(KR),LTV(KR),  EIN 3830
     1   DROHR(KR),XGEO(KR),IFBQ(KR)                                    EIN 3840
        GOTO 58                                                         EIN 3850
 6113   WRITE (6,6103) KR,XKON(KR),PVOR(KR),EPSIL(KR),DHYD(KR),QTV(KR), EIN 3860
     1   LTV(KR),DROHR(KR),XGEO(KR),IFBQ(KR)                            EIN 3870
        GOTO 58                                                         EIN 3880
 6004   CONTINUE                                                        EIN 3890
        IF(XKON(KR) .NE. 0.) XKON(KR) = 0.                              EIN 3900
        WRITE (6,6104) KR,PVOR(KR),EPSIL(KR),DHYD(KR),QTV(KR),LTV(KR),  EIN 3910
     1   DROHR(KR),XGEO(KR),IFBQ(KR)                                    EIN 3920
        GOTO 58                                                         EIN 3930
 6005   CONTINUE                                                        EIN 3940
        IF(XKON(KR) .NE. 0. .AND. ALPHA(KR) .NE. 0.) GOTO 6505          EIN 3950
        IF(XKON(KR) .EQ. 0. .AND. ALPHA(KR) .EQ. 0.) GOTO 6555          EIN 3960
        GOTO 6556                                                       EIN 3970
 6555   WRITE (6,6115) KR,PVOR(KR),EPSIL(KR),DHYD(KR),IFBQ(KR)          EIN 3980
        GOTO 58                                                         EIN 3990
 6556   IF(ALPHA(KR) .EQ. 0.) WRITE (6,6135) KR,XKON(KR),PVOR(KR),      EIN 4000
     1   EPSIL(KR),DHYD(KR),IFBQ(KR)                                    EIN 4010
        IF(XKON(KR) .EQ. 0.) WRITE (6,6125) KR,PVOR(KR),ALPHA(KR),      EIN 4020
     1   EPSIL(KR),DHYD(KR),IFBQ(KR)                                    EIN 4030
        GOTO 58                                                         EIN 4040
 6505   WRITE (6,6105) KR,XKON(KR),PVOR(KR),ALPHA(KR),EPSIL(KR),DHYD(KR)EIN 4050
     1   ,IFBQ(KR)                                                      EIN 4060
        GOTO 58                                                         EIN 4070
 6006   WRITE (6,6106)                                                  EIN 4080
C                                                                       EIN 4090
        CALL ABEND(3)                                                   EIN 4100
C                                                                       EIN 4110
        GOTO 60                                                         EIN 4120
   58   CONTINUE                                                        EIN 4130
        IF(IFZST(KR) .GE. 0) GOTO 6668                                  EIN 4140
CFZJ055                                                       25.09.07  EIN 4150
        GOTO 106                                                        EIN 4160
 6668   IF(STZUK(KR) .EQ. 0.) GOTO 6110                                 EIN 4170
        IF(STZUK(KR) .EQ. 1.E10 .AND. TFLVOR(KR) .EQ. 1.E10) GOTO 6110  EIN 4180
        IF(STZUK(KR) .GT. 0.) GOTO 6120                                 EIN 4190
        WRITE (6,6108) STZUK(KR)                                        EIN 4200
        GOTO 6110                                                       EIN 4210
 6120   IF(STZUK(KR) .NE. 1.E10) GOTO 6121                              EIN 4220
        IF(TFLVOR(KR) .EQ. 1.E10) GOTO 6110                             EIN 4230
        WRITE (6,6119) TFLVOR(KR)                                       EIN 4240
        GOTO 6110                                                       EIN 4250
 6121   IF(TFLVOR(KR) .NE. 1.E10) GOTO 6195                             EIN 4260
        WRITE (6,6108) STZUK(KR)                                        EIN 4270
        GOTO 6110                                                       EIN 4280
 6195   WRITE (6,6109) STZUK(KR),TFLVOR(KR)                             EIN 4290
 6110   CONTINUE                                                        EIN 4300
        IF(IFZST(KR) .NE. 2) GOTO 101                                   EIN 4310
        WRITE (6,150)                                                   EIN 4320
        GOTO 104                                                        EIN 4330
  101   CONTINUE                                                        EIN 4340
        IF(IFZST(KR) .NE. 1 .OR. IFZTF(KR) .NE. 1) GOTO 102             EIN 4350
        WRITE (6,151)                                                   EIN 4360
        GOTO 104                                                        EIN 4370
  102   CONTINUE                                                        EIN 4380
        IF(IFZST(KR) .NE. 1) GOTO 103                                   EIN 4390
        WRITE (6,105)                                                   EIN 4400
        GOTO 104                                                        EIN 4410
  103   CONTINUE                                                        EIN 4420
        IF(IFZTF(KR) .NE. 1) GOTO 106                                   EIN 4430
        WRITE (6,107)                                                   EIN 4440
        GOTO 104                                                        EIN 4450
  106   CONTINUE                                                        EIN 4460
        IF(IFZST(KR) .LE. 2 .AND. IFZST(KR) .GE. -10) GOTO 104          EIN 4470
        WRITE (6,153) IFZST(KR)                                         EIN 4480
C                                                                       EIN 4490
        CALL ABEND(3)                                                   EIN 4500
C                                                                       EIN 4510
  104   CONTINUE                                                        EIN 4520
   60 CONTINUE                                                          EIN 4530
      DO 63 K=1,KM                                                      EIN 4540
        IF(ALFISO(K) .NE. 0.) GOTO 77                                   EIN 4550
   63 CONTINUE                                                          EIN 4560
      GOTO 89                                                           EIN 4570
   77 CONTINUE                                                          EIN 4580
      WRITE (6,82)                                                      EIN 4590
      DO 84 K=1,KM                                                      EIN 4600
        IF(ALFISO(K) .GT. 0.) WRITE (6,83) K,ALFISO(K)                  EIN 4610
   84 CONTINUE                                                          EIN 4620
   89 CONTINUE                                                          EIN 4630
      HUN = 100.                                                        EIN 4640
      IF(KONIN .GT. 0) HUN = 1.                                         EIN 4650
      IF(IZKO .LE. 3) GOTO 8923                                         EIN 4660
      WRITE (6,78)                                                      EIN 4670
C                                                                       EIN 4680
      CALL ABEND(3)                                                     EIN 4690
C                                                                       EIN 4700
 8923 IF(IFZ .EQ. 0) GOTO 74                                            EIN 4710
      IF(IZKO .EQ. 0) GOTO 66                                           EIN 4720
C                                                                       EIN 4730
CARD TX25                                                               EIN 4740
C                                                                       EIN 4750
      READ (5,68) IZK1,(IZKOM(J),J=1,IZK1)                              EIN 4760
C                                                                       EIN 4770
      IF(IZK1-IZKO) 67,66,67                                            EIN 4780
   67 WRITE (6,69)                                                      EIN 4790
C                                                                       EIN 4800
      CALL ABEND(3)                                                     EIN 4810
C                                                                       EIN 4820
   66 CONTINUE                                                          EIN 4830
      DO 79 I=1,100                                                     EIN 4840
C                                                                       EIN 4850
CARD TX26                                                               EIN 4860
C                                                                       EIN 4870
        READ (5,120) ZVOR(I),ZDR(I),(ZST(IZK,I),ZTF(IZK,I),IZK=1,IZK1)  EIN 4880
C                                                                       EIN 4890
        IF(ZVOR(I) .NE. 0.) GOTO 79                                     EIN 4900
        III = I                                                         EIN 4910
        GOTO 73                                                         EIN 4920
   79 CONTINUE                                                          EIN 4930
   73 IZAEHL = III - 1                                                  EIN 4940
      WRITE (6,72) IZAEHL                                               EIN 4950
      DO 76 I=1,IZAEHL                                                  EIN 4960
        WRITE (6,81) I,ZVOR(I),ZDR(I),IZKOM(1),ZST(1,I),ZTF(1,I)        EIN 4970
        IF(IZK1 .LT. 2) GOTO 76                                         EIN 4980
        DO 766 II=2,IZK1                                                EIN 4990
          WRITE (6,811) IZKOM(II),ZST(II,I),ZTF(II,I)                   EIN 5000
  766   CONTINUE                                                        EIN 5010
   76 CONTINUE                                                          EIN 5020
   74 CONTINUE                                                          EIN 5030
CFZJ042                                                       09.09.05  EIN 5040
      IF(IPRINT .GT. -2) WRITE (6,601)                                  EIN 5050
C                                                                       EIN 5060
      IF(IPRINT .GT. -2) CALL SCHREI(MR,U,IFB,ZP,RP)                    EIN 5070
C                                                                       EIN 5080
      DO 75 N=1,NM                                                      EIN 5090
        DR(N) = DR(N) / HUN                                             EIN 5100
        R(N) = R(N) / HUN                                               EIN 5110
   75 CONTINUE                                                          EIN 5120
      DO 80 I=1,IM                                                      EIN 5130
        DZ(I) = DZ(I) / HUN                                             EIN 5140
        Z(I) = Z(I) / HUN                                               EIN 5150
        DO 80 N=1,NM                                                    EIN 5160
          P(I,N) = 0.                                                   EIN 5170
          IFB(I,N) = 0                                                  EIN 5180
   80 CONTINUE                                                          EIN 5190
      DO 90 I=2,IM1                                                     EIN 5200
        DO 90 N=2,NM1                                                   EIN 5210
          KK = KOM(I,N)                                                 EIN 5220
          VOL(KK) = VOL(KK) + FZQ(N) * DZ(I)                            EIN 5230
          XKK(I,N) = XKON(KK)                                           EIN 5240
          IFB(I,N) = IFBR(KK)                                           EIN 5250
          P(I,N) = PVOR(KK)                                             EIN 5260
   90 CONTINUE                                                          EIN 5270
C                                                                       EIN 5280
      IF(NHLR .GT. 0) CALL VORHOL(KOM,FZQ,DZ,IPAR,JPAR,NPAR)            EIN 5290
C                                                                       EIN 5300
      IF(NVERTI .GT. 0) CALL VORVER(IOVER,IUVER,KOM,IFBR,DZ,XVER)       EIN 5310
C                                                                       EIN 5320
      IF(NKUEL .GT. 0) CALL VORKUL(IOKUL,IUKUL,XKUL,KOM,IFBR,DZ)        EIN 5330
C                                                                       EIN 5340
      RETURN                                                            EIN 5350
      END                                                               EIN 5360
      SUBROUTINE DRUCKE(IPRINT,T,TFL,RHO,ZP,RP,IFB)                     DRU   10
C                                                                       DRU   20
C     AUSGABE DER FLUIDTEMPERATUREN (KNOTENZENTRIERT) UND DER           DRU   30
C     ALPHA*F/VOL (MASCHENZENTRIERT)                                    DRU   40
C                                                                       DRU   50
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      DRU   60
C                                                                       DRU   70
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             DRU   80
C                                                                       DRU   90
      DIMENSION T(KONN,KONI),TFL(KONN,KONI),RHO(KONN,KONI),ZP(KONN),    DRU  100
     1 RP(KONI),IFB(KONN,KONI),FELD(KONN,KONI)                          DRU  110
C                                                                       DRU  120
      CHARACTER*4 U1(4)/'FLUI','DTEM','PERA','TURE'/,U2(4)/'ALPH','A *F'DRU  130
     1 ,'/VOL','    '/,U3(4)/'SOLI','D MA','T. T','EMP.'/               DRU  140
C                                                                       DRU  150
C                                                                       DRU  160
      DO 100 I=1,IM1                                                    DRU  170
        DO 100 N=1,NM1                                                  DRU  180
          FELD(I,N) = TFL(I,N)                                          DRU  190
  100 CONTINUE                                                          DRU  200
C                                                                       DRU  210
      CALL SCHGAS(FELD,U1,ZP,RP,IFB)                                    DRU  220
C                                                                       DRU  230
      DO 150 I=1,IM1                                                    DRU  240
        DO 150 N=1,NM1                                                  DRU  250
          FELD(I,N) = T(I,N)                                            DRU  260
  150 CONTINUE                                                          DRU  270
C                                                                       DRU  280
      IF(IPRINT .GE. 2) CALL SCHGAS(FELD,U3,ZP,RP,IFB)                  DRU  290
C                                                                       DRU  300
      DO 200 I=1,IM1                                                    DRU  310
        DO 200 N=1,NM1                                                  DRU  320
          FELD(I,N) = RHO(I,N)                                          DRU  330
  200 CONTINUE                                                          DRU  340
C                                                                       DRU  350
      IF(IPRINT .GE. 1) CALL SCHGAS(FELD,U2,ZP,RP,IFB)                  DRU  360
C                                                                       DRU  370
      RETURN                                                            DRU  380
      END                                                               DRU  390
      SUBROUTINE ELEM3(IFMK,P,PP,KX,STROM,ROG2,ROG4,MX,MY,STZU,IFMXY)   ELE   10
C                                                                       ELE   20
C     LOEST DIE IMPULSGLEICHUNG (KRAEFTEGLEICHGEWICHT).                 ELE   30
C     IMPULSAENDERUNGEN WERDEN VERNACHLAESSIGT (DU/DT=0).               ELE   40
C                                                                       ELE   50
      COMMON /UPERR/ OERR                                               ELE   60
C                                                                       ELE   70
      REAL MP(4),KX(4),MX,MY                                            ELE   80
C                                                                       ELE   90
      DIMENSION ROG(4),P(4)                                             ELE  100
C                                                                       ELE  110
C                                                                       ELE  120
      ROG(1) = 0.                                                       ELE  130
      ROG(2) = ROG2                                                     ELE  140
      ROG(3) = 0.                                                       ELE  150
      ROG(4) = ROG4                                                     ELE  160
      ROG(4) = -ROG4                                                    ELE  170
      XZ = 0.                                                           ELE  180
      XN = 0.                                                           ELE  190
      DO 70 I=1,4                                                       ELE  200
        IF(KX(I) .EQ. OERR) GOTO 70                                     ELE  210
        XZ = XZ + (P(I)+ROG(I)) / KX(I)                                 ELE  220
        XN = XN + 1. / KX(I)                                            ELE  230
   70 CONTINUE                                                          ELE  240
      XZ = XZ + STZU                                                    ELE  250
      PP = XZ / XN                                                      ELE  260
      GOTO(10,110),IFMXY                                                ELE  270
      GOTO 110                                                          ELE  280
   10 CONTINUE                                                          ELE  290
      DO 6 I=1,4                                                        ELE  300
        MP(I) = 0.                                                      ELE  310
        IF(KX(I) .EQ. OERR) GOTO 6                                      ELE  320
        MP(I) = (P(I)+ROG(I)-PP) / KX(I)                                ELE  330
    6 CONTINUE                                                          ELE  340
      STROM = 0.                                                        ELE  350
      MX = MP(1) - MP(3)                                                ELE  360
      MY = MP(2) - MP(4)                                                ELE  370
  110 CONTINUE                                                          ELE  380
      RETURN                                                            ELE  390
      END                                                               ELE  400
      SUBROUTINE ELEM3A(J,PPN,IFMK,OVREL,IFMXY,XZSUM,XNSUM1,ROGG,STZUK, LEM   10
     1 MZ,MR,P,XKR,XKZ,FZQ,DZ,IFB,KOM,IOVER,IUVER,SUMXK,SUMRG,IPAR,JPAR,LEM   20
     2 NPAR,XKP,ROGP)                                                   LEM   30
C                                                                       LEM   40
C     BERECHNUNG DES HOMOGENEN DRUCKS IN HOHLRAEUMEN MIT MAX. 50        LEM   50
C     ANSCHLUSSPARTNERN                                                 LEM   60
C                                                                       LEM   70
      COMMON /HOHLR/ KPAR(10),KKOM(10),NHLR,PHOHL(10),NML(10),NMR(10),  LEM   80
     1 IL(10),VOL(10)                                                   LEM   90
C                                                                       LEM  100
      COMMON /VERTI/ NVERTI,KKOMV(30)                                   LEM  110
C                                                                       LEM  120
CFZJ055                                                       25.09.07  LEM  130
C                                                                       LEM  140
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             LEM  150
C                                                                       LEM  160
      COMMON /UPERR/ OERR                                               LEM  170
C                                                                       LEM  180
      REAL M(4),MRL                                                     LEM  190
C                                                                       LEM  200
      DIMENSION XZSUM(KOMAX),XNSUM1(KOMAX),ROGG(KONN,KONI),STZUK(KOMAX),LEM  210
     1 P(KONN,KONI),XKR(KONN,KONI),XKZ(KONN,KONI),FZQ(KONI),DZ(KONN),   LEM  220
     2 IFB(KONN,KONI),KOM(KONN,KONI),IOVER(KONI,30),IUVER(KONI,30),     LEM  230
     3 SUMXK(KONI,30),SUMRG(KONI,30),IPAR(KONI*2,10),JPAR(KONI*2,10),   LEM  240
     4 NPAR(KONI*2,10),XKP(KONI*2),ROGP(KONI*2)                         LEM  250
C                                                                       LEM  260
      REAL MZ(KONN,KONI),MR(KONN,KONI)                                  LEM  270
C                                                                       LEM  280
C                                                                       LEM  290
C     DRUCKBERECHNUNG                                                   LEM  300
C                                                                       LEM  310
      KP = KPAR(J)                                                      LEM  320
      XZ = 0.                                                           LEM  330
      XN = 0.                                                           LEM  340
      DO 100 IP=1,KP                                                    LEM  350
        I = IPAR(IP,J)                                                  LEM  360
        N = NPAR(IP,J)                                                  LEM  370
        L = JPAR(IP,J)                                                  LEM  380
        GOTO(10,20,30,40),L                                             LEM  390
C                                                                       LEM  400
C     PARTNER LINKS                                                     LEM  410
C                                                                       LEM  420
   10   XK = XKR(I,N)                                                   LEM  430
        PB = P(I,N)                                                     LEM  440
        ROG = 0.                                                        LEM  450
        GOTO 50                                                         LEM  460
C                                                                       LEM  470
C     PARTNER OBERHALB                                                  LEM  480
C                                                                       LEM  490
   20   CONTINUE                                                        LEM  500
        IF(IFB(I,N) .EQ. 2) GOTO 21                                     LEM  510
        XK = XKZ(I,N)                                                   LEM  520
        ROG = ROGG(I,N)                                                 LEM  530
        PB = P(I,N)                                                     LEM  540
        GOTO 50                                                         LEM  550
   21   CONTINUE                                                        LEM  560
        KO = KOM(I,N)                                                   LEM  570
        DO 22 NV=1,NVERTI                                               LEM  580
          KO1 = KKOMV(NV)                                               LEM  590
          IF(KO .NE. KO1) GOTO 22                                       LEM  600
          IO = IOVER(N,NV) - 1                                          LEM  610
          XK = SUMXK(N,NV) + XKZ(IO,N)                                  LEM  620
          ROG = SUMRG(N,NV) + ROGG(IO,N)                                LEM  630
          PB = P(IO,N)                                                  LEM  640
          GOTO 50                                                       LEM  650
   22   CONTINUE                                                        LEM  660
C                                                                       LEM  670
C     PARTNER RECHTS                                                    LEM  680
C                                                                       LEM  690
   30   XK = XKR(I,N-1)                                                 LEM  700
        PB = P(I,N)                                                     LEM  710
        ROG = 0.                                                        LEM  720
        GOTO 50                                                         LEM  730
C                                                                       LEM  740
C     PARTNER UNTERHALB                                                 LEM  750
C                                                                       LEM  760
   40   CONTINUE                                                        LEM  770
        IF(IFB(I,N) .EQ. 2) GOTO 41                                     LEM  780
        XK = XKZ(I-1,N)                                                 LEM  790
        PB = P(I,N)                                                     LEM  800
        ROG = ROGG(I-1,N)                                               LEM  810
        ROG = -ROG                                                      LEM  820
        GOTO 50                                                         LEM  830
   41   CONTINUE                                                        LEM  840
        KO = KOM(I,N)                                                   LEM  850
        DO 42 NV=1,NVERTI                                               LEM  860
          KO1 = KKOMV(NV)                                               LEM  870
          IF(KO .NE. KO1) GOTO 42                                       LEM  880
          IO = IUVER(N,NV) + 1                                          LEM  890
          XK = SUMXK(N,NV) + XKZ(I-1,N)                                 LEM  900
          ROG = -(SUMRG(N,NV)+ROGG(I-1,N))                              LEM  910
          PB = P(IO,N)                                                  LEM  920
          GOTO 50                                                       LEM  930
   42   CONTINUE                                                        LEM  940
   50   CONTINUE                                                        LEM  950
        XKP(IP) = XK                                                    LEM  960
        IF(XK .EQ. OERR) GOTO 100                                       LEM  970
        ROGP(IP) = ROG + PB                                             LEM  980
        XZ = XZ + (PB+ROG) / XK                                         LEM  990
        XN = XN + 1. / XK                                               LEM 1000
  100 CONTINUE                                                          LEM 1010
      KK = KKOM(J)                                                      LEM 1020
      XZ = XZ + STZUK(KK) * VOL(J)                                      LEM 1030
      PP = XZ / XN                                                      LEM 1040
      NL = NML(J)                                                       LEM 1050
      NR = NMR(J)                                                       LEM 1060
      PPN = P(I,NL) + (PP-P(I,NL)) * OVREL                              LEM 1070
      I = IL(J)                                                         LEM 1080
      DO 80 N=NL,NR                                                     LEM 1090
        P(I,N) = PPN                                                    LEM 1100
   80 CONTINUE                                                          LEM 1110
      PHOHL(J) = PPN / 1.E5                                             LEM 1120
      GOTO(70,60),IFMXY                                                 LEM 1130
      GOTO 60                                                           LEM 1140
   70 CONTINUE                                                          LEM 1150
C                                                                       LEM 1160
C     MASSENSTROM-BERECHNUNG                                            LEM 1170
C                                                                       LEM 1180
      NL = NML(J)                                                       LEM 1190
      NR = NMR(J)                                                       LEM 1200
      MRL = 0.                                                          LEM 1210
      IF(XKP(1) .EQ. OERR) GOTO 310                                     LEM 1220
      MRL = (ROGP(1)-PP) / XKP(1)                                       LEM 1230
  310 CONTINUE                                                          LEM 1240
      NN = 0                                                            LEM 1250
      DO 400 N=NL,NR                                                    LEM 1260
        DO 405 I1=1,4                                                   LEM 1270
          M(I1) = 0.                                                    LEM 1280
  405   CONTINUE                                                        LEM 1290
        NN = NN + 1                                                     LEM 1300
        NPO = NN * 2                                                    LEM 1310
        NPU = NPO + 1                                                   LEM 1320
        M(1) = MRL                                                      LEM 1330
        IF(XKP(NPO) .EQ. OERR) GOTO 410                                 LEM 1340
        M(2) = (ROGP(NPO)-PP) / XKP(NPO)                                LEM 1350
  410   IF(XKP(NPU) .EQ. OERR) GOTO 420                                 LEM 1360
        M(4) = (ROGP(NPU)-PP) / XKP(NPU)                                LEM 1370
  420   M(3) = -M(1) - M(2) - M(4) - STZUK(KK) * FZQ(N) * DZ(I)         LEM 1380
        IF(IFMK .EQ. 1) GOTO 500                                        LEM 1390
        MR(I,N) = (M(1)-M(3)) / 2.                                      LEM 1400
        MZ(I,N) = (M(2)-M(4)) / 2.                                      LEM 1410
        GOTO 510                                                        LEM 1420
  500   MR(I,N) = -M(3) / 2.                                            LEM 1430
        MZ(I,N) = -M(4) / 2.                                            LEM 1440
  510   CONTINUE                                                        LEM 1450
        P(I,N) = PPN                                                    LEM 1460
        MRL = -M(3)                                                     LEM 1470
  400 CONTINUE                                                          LEM 1480
   60 CONTINUE                                                          LEM 1490
      RETURN                                                            LEM 1500
      END                                                               LEM 1510
      SUBROUTINE ELEM3B(DIFF,OVREL,IFMK,IOVER,IUVER,ROGG,MZ,MR,P,XKZ)   EM3   10
C                                                                       EM3   20
C     GEMEINSAME BERECHNUNG DES DRUCKVERLAUFS IN REGIONEN MIT SENKR.    EM3   30
C     STROMROEHREN.  ACHTUNG: REGION DARF KEINE QUELLEN ENTHALTEN !     EM3   40
C                                                                       EM3   50
      COMMON /VERTI/ NVERTI,KKOMV(30)                                   EM3   60
C                                                                       EM3   70
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      EM3   80
C                                                                       EM3   90
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             EM3  100
C                                                                       EM3  110
      DIMENSION IOVER(KONI,30),IUVER(KONI,30),ROGG(KONN,KONI),          EM3  120
     1 P(KONN,KONI),XKZ(KONN,KONI)                                      EM3  130
C                                                                       EM3  140
      REAL MZ(KONN,KONI),MR(KONN,KONI),M                                EM3  150
C                                                                       EM3  160
C                                                                       EM3  170
      DO 300 NV=1,NVERTI                                                EM3  180
        DO 300 N=2,NM1                                                  EM3  190
          IO = IOVER(N,NV)                                              EM3  200
          IF(IO .EQ. 0) GOTO 300                                        EM3  210
          IU = IUVER(N,NV)                                              EM3  220
          PO = P(IO-1,N)                                                EM3  230
          PU = P(IU+1,N)                                                EM3  240
          SROG = ROGG(IO-1,N)                                           EM3  250
          SXK = XKZ(IO-1,N)                                             EM3  260
          DO 100 I=IO,IU                                                EM3  270
            SROG = SROG + ROGG(I,N)                                     EM3  280
            SXK = SXK + XKZ(I,N)                                        EM3  290
  100     CONTINUE                                                      EM3  300
          M = (PO+SROG-PU) / SXK                                        EM3  310
          IF(IFMK .EQ. 1) M = M / 2.                                    EM3  320
          DO 200 I=IO,IU                                                EM3  330
            MR(I,N) = 0.                                                EM3  340
            MZ(I,N) = M                                                 EM3  350
            IF(IFMK .EQ. 1) GOTO 200                                    EM3  360
            PPN = PO + ROGG(I-1,N) - XKZ(I-1,N) * M                     EM3  370
            PO = PPN                                                    EM3  380
            P(I,N) = P(I,N) + (PPN-P(I,N)) * OVREL                      EM3  390
            IF(P(I,N) .LT. DIFF) DIFF = P(I,N)                          EM3  400
  200     CONTINUE                                                      EM3  410
  300 CONTINUE                                                          EM3  420
      RETURN                                                            EM3  430
      END                                                               EM3  440
      SUBROUTINE ELEM3C(DIFF,OVREL,IFMK,IOKUL,IUKUL,ROGG,MZ,MR,P,XKZ)   M3C   10
C                                                                       M3C   20
C     BERECHNUNG DES DRUCKVERLAUFS IN ALLEN ROEHRENKUEHLERN             M3C   30
C     MASSENSTROMQUELLEN SIND NICHT ZUGELASSEN !                        M3C   40
C                                                                       M3C   50
      COMMON /KUEL1/ NKUEL,KKOMK(4)                                     M3C   60
C                                                                       M3C   70
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      M3C   80
C                                                                       M3C   90
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             M3C  100
C                                                                       M3C  110
      DIMENSION IOKUL(KONI,4),IUKUL(KONI,4),ROGG(KONN,KONI),P(KONN,KONI)M3C  120
     1 ,XKZ(KONN,KONI)                                                  M3C  130
C                                                                       M3C  140
      REAL MZ(KONN,KONI),MR(KONN,KONI),M,KROG,KXK                       M3C  150
C                                                                       M3C  160
C                                                                       M3C  170
      DO 100 NK=1,NKUEL                                                 M3C  180
        DO 100 N=2,NM1                                                  M3C  190
          IO = IOKUL(N,NK)                                              M3C  200
          IF(IO .EQ. 0) GOTO 100                                        M3C  210
          IU = IUKUL(N,NK)                                              M3C  220
          PO = P(IO-1,N)                                                M3C  230
          PU = P(IU+1,N)                                                M3C  240
          KROG = ROGG(IO-1,N)                                           M3C  250
          KXK = XKZ(IO-1,N)                                             M3C  260
          DO 200 I=IO,IU                                                M3C  270
            KROG = KROG + ROGG(I,N)                                     M3C  280
            KXK = KXK + XKZ(I,N)                                        M3C  290
  200     CONTINUE                                                      M3C  300
          M = (PO+KROG-PU) / KXK                                        M3C  310
          IF(IFMK .EQ. 1) M = M / 2.                                    M3C  320
          DO 300 I=IO,IU                                                M3C  330
            MR(I,N) = 0.                                                M3C  340
            MZ(I,N) = M                                                 M3C  350
            IF(IFMK .EQ. 1) GOTO 300                                    M3C  360
            PPN = PO + ROGG(I-1,N) - XKZ(I-1,N) * M                     M3C  370
            PO = PPN                                                    M3C  380
            P(I,N) = P(I,N) + (PPN-P(I,N)) * OVREL                      M3C  390
            IF(P(I,N) .LT. DIFF) DIFF = P(I,N)                          M3C  400
  300     CONTINUE                                                      M3C  410
  100 CONTINUE                                                          M3C  420
      RETURN                                                            M3C  430
      END                                                               M3C  440
      SUBROUTINE ELEM4(I,N,PB,MB,PP,QUEL,TQUE,FELD,IFQUE,ALGA,T,TFL,RHO,EM4   10
     1 MZ,MR,XKR,XKZ)                                                   EM4   20
C                                                                       EM4   30
C     LOEST DIE ENERGIEGLEICHUNG FUER EINE GASTEM-MASCHE (KNOTEN-       EM4   40
C     ZENTRIERT). DER WAERMEUEBERGANG WIRD MASSENSTROMGEWICHTET         EM4   50
C     BERUECKSICHTIGT (---> SR ALGAS) UND ERFOLGT IN DEN GEDACHTEN      EM4   60
C     ROHREN (8-ROHR-MODELL). IN DER MASCHENMITTE FINDET DANN VOLL-     EM4   70
C     STAENDIGE MISCHUNG UNTER BERUECKSICHTIGUNG VON MASSENSTROM-       EM4   80
C     QUELLEN UND WAERMETRANSPORT DURCH LEITUNG (EFFEKTIVE QUERLEIT-    EM4   90
C     FAEHIGKEIT) STATT. DIE KONVEKTIVE WAERMEQUELLE WIRD IN DER SR     EM4  100
C     QUELL GEBILDET.                                                   EM4  110
C                                                                       EM4  120
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             EM4  130
C                                                                       EM4  140
      DIMENSION FELD(KONN,KONI),ALGA(KONN,KONI,2),T(KONN,KONI),         EM4  150
     1 TFL(KONN,KONI),RHO(KONN,KONI),XKR(KONN,KONI),XKZ(KONN,KONI),     EM4  160
     2 QUEL(4),TQUE(4),PB(4),PC(4),TEF1(4,2),TEF2(4,2),TEF3(4,2),A(4),  EM4  170
     3 IF1(4)                                                           EM4  180
C                                                                       EM4  190
      REAL MZ(KONN,KONI),MR(KONN,KONI),MB(4),MC(4,2),ME(4),MD(4)        EM4  200
C                                                                       EM4  210
C                                                                       EM4  220
      IF(N .EQ. 1) GOTO 10                                              EM4  230
      PB(1) = TFL(I,N-1)                                                EM4  240
      PC(1) = T(I,N-1)                                                  EM4  250
   30 IF(I .EQ. 1) GOTO 20                                              EM4  260
      PB(2) = TFL(I-1,N)                                                EM4  270
      PC(2) = T(I-1,N)                                                  EM4  280
   40 PB(3) = TFL(I,N+1)                                                EM4  290
      PC(3) = T(I,N+1)                                                  EM4  300
      PB(4) = TFL(I+1,N)                                                EM4  310
      PC(4) = T(I+1,N)                                                  EM4  320
      GOTO 50                                                           EM4  330
   10 PB(1) = T(I,N)                                                    EM4  340
      PC(1) = PB(1)                                                     EM4  350
      GOTO 30                                                           EM4  360
   20 PB(2) = T(I,N)                                                    EM4  370
      PC(2) = PB(2)                                                     EM4  380
      GOTO 40                                                           EM4  390
   50 CONTINUE                                                          EM4  400
      TB = T(I,N)                                                       EM4  410
      MB(1) = MR(I,N)                                                   EM4  420
      MB(2) = MZ(I,N)                                                   EM4  430
      MB(3) = MR(I,N+1)                                                 EM4  440
      MB(4) = MZ(I+1,N)                                                 EM4  450
      MC(1,1) = XKR(I,N)                                                EM4  460
      MC(1,2) = XKZ(I,N)                                                EM4  470
      MC(2,1) = -XKR(I,N+1)                                             EM4  480
      MC(2,2) = XKZ(I,N+1)                                              EM4  490
      MC(3,1) = XKR(I+1,N)                                              EM4  500
      MC(3,2) = -XKZ(I+1,N)                                             EM4  510
      MC(4,1) = -XKR(I+1,N+1)                                           EM4  520
      MC(4,2) = -XKZ(I+1,N+1)                                           EM4  530
      ME(1) = ALGA(I,N,1)                                               EM4  540
      ME(2) = ALGA(I,N+1,1)                                             EM4  550
      ME(3) = ALGA(I+1,N,1)                                             EM4  560
      ME(4) = ALGA(I+1,N+1,1)                                           EM4  570
      MD(1) = ALGA(I,N,2)                                               EM4  580
      MD(2) = ALGA(I,N+1,2)                                             EM4  590
      MD(3) = ALGA(I+1,N,2)                                             EM4  600
      MD(4) = ALGA(I+1,N+1,2)                                           EM4  610
      A(1) = RHO(I,N)                                                   EM4  620
      A(2) = RHO(I,N+1)                                                 EM4  630
      A(3) = RHO(I+1,N)                                                 EM4  640
      A(4) = RHO(I+1,N+1)                                               EM4  650
      DO 210 L=1,4                                                      EM4  660
        IF1(L) = 0                                                      EM4  670
        IF(ME(L) .EQ. 1.) GOTO 230                                      EM4  680
        GOTO 210                                                        EM4  690
  230   CONTINUE                                                        EM4  700
        ME(L) = 1.                                                      EM4  710
        MD(L) = 1.                                                      EM4  720
        IF1(L) = 2                                                      EM4  730
        A(L) = 1.                                                       EM4  740
  210 CONTINUE                                                          EM4  750
      TEF1(1,1) = PB(1) - PC(1)                                         EM4  760
      TEF1(1,2) = PB(2) - PC(2)                                         EM4  770
      TEF1(2,1) = PB(3) - PC(3)                                         EM4  780
      TEF1(2,2) = PB(2) - PC(2)                                         EM4  790
      TEF1(3,1) = PB(1) - PC(1)                                         EM4  800
      TEF1(3,2) = PB(4) - PC(4)                                         EM4  810
      TEF1(4,1) = PB(3) - PC(3)                                         EM4  820
      TEF1(4,2) = PB(4) - PC(4)                                         EM4  830
      TEF2(1,1) = PC(1)                                                 EM4  840
      TEF2(1,2) = PC(2)                                                 EM4  850
      TEF2(2,1) = PC(3)                                                 EM4  860
      TEF2(2,2) = PC(2)                                                 EM4  870
      TEF2(3,1) = PC(1)                                                 EM4  880
      TEF2(3,2) = PC(4)                                                 EM4  890
      TEF2(4,1) = PC(3)                                                 EM4  900
      TEF2(4,2) = PC(4)                                                 EM4  910
      TEF3(1,1) = (TB-PC(1)) / A(1)                                     EM4  920
      TEF3(1,2) = (TB-PC(2)) / A(1)                                     EM4  930
      TEF3(2,1) = (TB-PC(3)) / A(2)                                     EM4  940
      TEF3(2,2) = (TB-PC(2)) / A(2)                                     EM4  950
      TEF3(3,1) = (TB-PC(1)) / A(3)                                     EM4  960
      TEF3(3,2) = (TB-PC(4)) / A(3)                                     EM4  970
      TEF3(4,1) = (TB-PC(3)) / A(4)                                     EM4  980
      TEF3(4,2) = (TB-PC(4)) / A(4)                                     EM4  990
      DO 220 L=1,4                                                      EM4 1000
        DO 220 M=1,2                                                    EM4 1010
          XK = MC(L,M)                                                  EM4 1020
          IF(XK .LE. 0.) GOTO 220                                       EM4 1030
          TFE = TEF1(L,M) + TEF2(L,M)                                   EM4 1040
          IF(IF1(L) .EQ. 1) GOTO 250                                    EM4 1050
          IF(IF1(L) .EQ. 2) GOTO 270                                    EM4 1060
          IF(IFQUE .NE. 1) GOTO 240                                     EM4 1070
          TFH = TEF1(L,M) * MD(L) + TEF3(L,M) * (MD(L)-1.)              EM4 1080
          TFH = TFH + (TEF2(L,M)+TB) / 2.                               EM4 1090
  240     TEF2(L,M) = TEF1(L,M) * ME(L) + TEF3(L,M) * (ME(L)-1.)        EM4 1100
          TFA = TEF2(L,M) + TB                                          EM4 1110
          GOTO 260                                                      EM4 1120
  250     CONTINUE                                                      EM4 1130
          TFH = TEF1(L,M) * MD(L) - TEF3(L,M) / 2. + (TEF2(L,M)+TB) / 2.EM4 1140
          TEF2(L,M) = TEF1(L,M) * ME(L) - TEF3(L,M)                     EM4 1150
          TFA = TEF2(L,M) + TB                                          EM4 1160
          GOTO 260                                                      EM4 1170
  270     CONTINUE                                                      EM4 1180
          TFH = TFE                                                     EM4 1190
          TFA = TFE                                                     EM4 1200
          TEF2(L,M) = TFE - TB                                          EM4 1210
  260     IF(IFQUE .NE. 1) GOTO 220                                     EM4 1220
C                                                                       EM4 1230
          CALL QUELL(I,N,L,M,TFE,TFH,TFA,XK,FELD)                       EM4 1240
C                                                                       EM4 1250
  220 CONTINUE                                                          EM4 1260
      XZ = 0.                                                           EM4 1270
      XN = 0.                                                           EM4 1280
      DO 420 L=1,4                                                      EM4 1290
        TEF = PB(L) - TB                                                EM4 1300
        XZ = XZ + MB(L) * TEF                                           EM4 1310
        XN = XN + MB(L)                                                 EM4 1320
  420 CONTINUE                                                          EM4 1330
      DO 450 L=1,4                                                      EM4 1340
        DO 450 M=1,2                                                    EM4 1350
          IF(MC(L,M) .LE. 0.) GOTO 450                                  EM4 1360
          XZ = XZ + TEF2(L,M) * MC(L,M)                                 EM4 1370
          XN = XN + MC(L,M)                                             EM4 1380
  450 CONTINUE                                                          EM4 1390
      QUE1 = 0.                                                         EM4 1400
      QUE2 = 0.                                                         EM4 1410
      DO 300 L=1,4                                                      EM4 1420
        QUE1 = QUE1 + (QUEL(L)*(TQUE(L)-TB)) / 4.                       EM4 1430
        QUE2 = QUE2 + QUEL(L) / 4.                                      EM4 1440
  300 CONTINUE                                                          EM4 1450
      XZ = XZ + QUE1                                                    EM4 1460
      XN = XN + QUE2                                                    EM4 1470
      IF(XN .GT. 0.) GOTO 430                                           EM4 1480
      PP = TB                                                           EM4 1490
      GOTO 440                                                          EM4 1500
  430 CONTINUE                                                          EM4 1510
      PP = XZ / XN + TB                                                 EM4 1520
  440 CONTINUE                                                          EM4 1530
      RETURN                                                            EM4 1540
      END                                                               EM4 1550