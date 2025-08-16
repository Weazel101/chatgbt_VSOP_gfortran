      SUBROUTINE DATA2                                                  ATA   10
C                                                                       ATA   20
C                                                                       ATA   30
      COMMON /VARIAD/ KMAT,IACT                                         ATA   40
C                                                                       ATA   50
      COMMON /DATATA/ NXT29,NDA29,IPUO2                                 ATA   60
C                                                                       ATA   70
      DIMENSION K(200)                                                  ATA   80
C                                                                       ATA   90
C                                                                       ATA  100
      CALL WATCH(ENDE)                                                  ATA  110
C                                                                       ATA  120
      A = ENDE                                                          ATA  130
      NXT29 = 1                                                         ATA  140
      NDA29 = 29                                                        ATA  150
      NZUT = 0                                                          ATA  160
      NGAM = 0                                                          ATA  170
      NTHER = 0                                                         ATA  180
      IACT = 28                                                         ATA  190
      IU4 = 0                                                           ATA  200
C                                                                       ATA  210
      CALL DAT                                                          ATA  220
C                                                                       ATA  230
CFZJ036                                                       30.09.04  ATA  240
CARD D 1                                                                ATA  250
      READ (5,1002) KMAT,NHOM,NDANC                                     ATA  260
C                                                                       ATA  270
      IF(NHOM .NE. 1) GOTO 2                                            ATA  280
      IU4 = 1                                                           ATA  290
      KOSTDA = 0                                                        ATA  300
      NDANC = 0                                                         ATA  310
      GOTO 1                                                            ATA  320
    2 CONTINUE                                                          ATA  330
      KOSTDA = 1                                                        ATA  340
      NGAM = 1                                                          ATA  350
      NTHER = 1                                                         ATA  360
      NZUT = 1                                                          ATA  370
    1 CONTINUE                                                          ATA  380
C                                                                       ATA  390
      CALL SUBMAI(NGAM,NTHER,NZUT,NDANC,KOSTDA,IU4,K(1))                ATA  400
C                                                                       ATA  410
      CALL WATCH(ENDE)                                                  ATA  420
C                                                                       ATA  430
      CPU = ENDE - A                                                    ATA  440
      WRITE (6,1000) CPU/60.                                            ATA  450
C                                                                       ATA  460
 1000 FORMAT (///' "DATA2": EXECUTION TIME:',F10.3,' MIN.')             ATA  470
 1002 FORMAT (18I4)                                                     ATA  480
      RETURN                                                            ATA  490
      END                                                               ATA  500
      SUBROUTINE SUBMAI(NGAM,NTHER,NZUT,NDANC,KOSTDA,IU4,IMAT)          UBM   10
C                                                                       UBM   20
      COMMON /VARIAD/ KMAT,IACT                                         UBM   30
C                                                                       UBM   40
      COMMON /VNISA/ V(10),NIS(10),A(10,10)                             UBM   50
C                                                                       UBM   60
      COMMON /GAM/ CEG(33),AL1(33),IDSATZ(12),NOID,NTBD,NRES,IDRES,WTP, UBM   70
     1 WSTU,WAG,NOAG                                                    UBM   80
C                                                                       UBM   90
      COMMON /THER/ MTBL(20),LIM(30),ID,NX,NHET,NTHGR,MX,IDC,IDO,TTEMP, UBM  100
     1 MMX                                                              UBM  110
C                                                                       UBM  120
      COMMON /DATATA/ NXT29,NDA29,IPUO2,U53                             UBM  130
C                                                                       UBM  140
      COMMON /AAAA/ AAA(100),RADIU                                      UBM  150
C                                                                       UBM  160
      COMMON /COSTS/ KOSTDA,KOSTD2,FIMA,HK,AK,EK,CURCY,NC,NF,FC(6),FF(4),FRC(6)UBM  170
     1 ,FRF(4),FABKOS,AUFKOS,DK,S(31),IU8TH                             UBM  180
C                                                                       UBM  190
      COMMON /CPDAT/ ANR,INDBS,DU(21),BETA                              UBM  200
C                                                                       UBM  210
      COMMON /ANR4/ V4Z5(4),U5UG(4)                                     UBM  220
C                                                                       UBM  230
      COMMON /SMDEF/ IMA(30)                                            UBM  240
C                                                                       UBM  250
      COMMON /GRES/ ROBR,FAKMOL,VCP,VMOD,ROSM,VKUGEL,VSTAB,GSMCP,GSPCP, UBM  260
     1 GSM,GSP,ASM,BK,RKUGEL,DREF,DCORE,HCORE,FF1,FF2,R1,R2,QR0,RSH     UBM  270
C                                                                       UBM  280
CFZJ018   Common KUDAT added                                  12.12.03  UBM  290
      COMMON /KUDAT/ ROMTX,ROSCH,QR1,QR2,QROSM,QFF1,QINDBK,QBK,QVMOD    UBM  300
C                                                                       UBM  310
      CHARACTER*4 TITEL(18),ENDE/'stop'/                                UBM  320
C                                                                       UBM  330
      DIMENSION NRMAF(40),NRGAM(40),DEN(40),DENG(40),FRACT(15),RADI(15),UBM  340
     1 NFT(30),IMAT(KMAT+IACT)                                          UBM  350
C                                                                       UBM  360
C                                                                       UBM  370
      IU8T = 0                                                          UBM  380
      NS = NGAM + NTHER + NZUT + NDANC                                  UBM  390
      KOSTD2 = KOSTDA                                                   UBM  400
      FF2 = 0.61                                                        UBM  410
      N3 = 3                                                            UBM  420
      N10 = 10                                                          UBM  430
      N15 = 40                                                          UBM  440
      N30 = 30                                                          UBM  450
      NC = 1                                                            UBM  460
      NF = 1                                                            UBM  470
      TTEMP = 0.                                                        UBM  480
      DO 30 I=1,N30                                                     UBM  490
        NFT(I) = 0                                                      UBM  500
   30 CONTINUE                                                          UBM  510
      FABKOS = 0.                                                       UBM  520
      AUFKOS = 0.                                                       UBM  530
      IF(KMAT .EQ. 0) GOTO 21                                           UBM  540
C                                                                       UBM  550
CARD D 2                                                                UBM  560
C                                                                       UBM  570
      READ (5,1002) (IMAT(L+IACT),L=1,KMAT)                             UBM  580
C                                                                       UBM  590
      IF(IACT .LE. 0) GOTO 91                                           UBM  600
      DO 90 L=1,IACT                                                    UBM  610
        IF(IMA(L) .EQ. 9) IMAU4 = L                                     UBM  620
        IMAT(L) = IMA(L)                                                UBM  630
   90 CONTINUE                                                          UBM  640
   91 CONTINUE                                                          UBM  650
      KMA = KMAT + IACT                                                 UBM  660
      WRITE (6,1015) KMA                                                UBM  670
      WRITE (6,1002) (IMAT(L),L=1,KMA)                                  UBM  680
   21 CONTINUE                                                          UBM  690
      IF(KOSTDA .EQ. 0) GOTO 18                                         UBM  700
C                                                                       UBM  710
CARD D 3                                                                UBM  720
C                                                                       UBM  730
      READ (5,1012) CURCY,NC,NF,(FC(I),I=1,6),(FF(I),I=1,3),DK          UBM  740
C                                                                       UBM  750
      WRITE (6,1013) (I,I=1,NC)                                         UBM  760
      WRITE (6,1014) CURCY,(FC(I),I=1,NC)                               UBM  770
      WRITE (6,1016) (I,I=1,NF)                                         UBM  780
      WRITE (6,1017) CURCY,(FF(I),I=1,NF)                               UBM  790
      WRITE (6,1019) CURCY,DK                                           UBM  800
C                                                                       UBM  810
CARD D 4                                                                UBM  820
C                                                                       UBM  830
      READ (5,1001) HK,AK,EK                                            UBM  840
C                                                                       UBM  850
      WRITE (6,1018) CURCY,HK,CURCY,AK,CURCY,EK                         UBM  860
   18 CONTINUE                                                          UBM  870
      NRET = NGAM + NTHER                                               UBM  880
      NHET = NTHER - 1                                                  UBM  890
      IF(KMAT .EQ. 0) GOTO 7999                                         UBM  900
      WRITE (NDA29,REC=NXT29) KMAT,(IMAT(L+IACT),L=1,KMAT)              UBM  910
C                                                                       UBM  920
CARD D 5                                                                UBM  930
C                                                                       UBM  940
 7999 READ (5,1000) (TITEL(I),I=1,18)                                   UBM  950
C                                                                       UBM  960
      IF(TITEL(1) .EQ. ENDE) GOTO 8000                                  UBM  970
      IPUO2 = 0                                                         UBM  980
      DO 4567 JKL=1,90                                                  UBM  990
        AAA(JKL)=0.                                                     UBM 1000
 4567 CONTINUE                                                          UBM 1010
      DO 20 I=1,N15                                                     UBM 1020
        DEN(I) = 0.                                                     UBM 1030
        NRMAF(I) = 0                                                    UBM 1040
        NRGAM(I) = 0                                                    UBM 1050
        DENG(I) = 0.                                                    UBM 1060
        IF(I .GT. 15) GOTO 20                                           UBM 1070
        FRACT(I) = 0.                                                   UBM 1080
        RADI(I) = 0.                                                    UBM 1090
   20 CONTINUE                                                          UBM 1100
C                                                                       UBM 1110
CARD D 6                                                                UBM 1120
C                                                                       UBM 1130
      READ (5,1020) NTYP,NFUTP,NFCP,NFBZ,NZUS,FF3                       UBM 1140
C                                                                       UBM 1150
CFZJ018   Input change for Dummy Elements                     12.12.03  UBM 1160
      QINDBK = 0.                                                       UBM 1170
      QBK = 0.                                                          UBM 1180
      IF(NFUTP .GT. 0) GOTO 22                                          UBM 1190
      QINDBK = 1.                                                       UBM 1200
      QBK = 1.                                                          UBM 1210
      NFUTP = IABS(NFUTP)                                               UBM 1220
   22 CONTINUE                                                          UBM 1230
      IF(FF3 .NE. 0.) FF2 = FF3                                         UBM 1240
      IF(NFUTP .LE. 0) GOTO 40                                          UBM 1250
      MRK = MAX0(N3,NXT29)                                              UBM 1260
      NXT29 = 2                                                         UBM 1270
      NFT(MRK) = NFUTP                                                  UBM 1280
      WRITE (NDA29,REC=NXT29) N30,(NFT(I),I=1,N30)                      UBM 1290
      NXT29 = MRK                                                       UBM 1300
   40 CONTINUE                                                          UBM 1310
      NFU = NFUTP / 100                                                 UBM 1320
      NVR = NFUTP - (100*NFU)                                           UBM 1330
      WRITE (6,1011) (TITEL(I),I=1,18),NFU,NVR,NXT29                    UBM 1340
C                                                                       UBM 1350
      IF(NFCP .NE. 0) CALL CP(NFCP,IU8T)                                UBM 1360
C                                                                       UBM 1370
      NTYP = NTYP + 1                                                   UBM 1380
      IF(NFCP .EQ. 0 .AND. NFBZ .EQ. 0) GOTO 5                          UBM 1390
      GOTO(2,3),NTYP                                                    UBM 1400
C                                                                       UBM 1410
    2 CALL KUGEL(NFBZ,*5)                                               UBM 1420
C                                                                       UBM 1430
    3 CALL STAB(NFBZ,*5)                                                UBM 1440
C                                                                       UBM 1450
    5 CONTINUE                                                          UBM 1460
C                                                                       UBM 1470
      IF(NDANC.NE.0) CALL DANC3(NDANC)                                  UBM 1480
C                                                                       UBM 1490
      IF(NZUT.NE.0) CALL ZUT(NTYP,NRET,*7999)                           UBM 1500
C                                                                       UBM 1510
      IF(NGAM .NE. 0) CALL GAM1(NTYP)                                   UBM 1520
C                                                                       UBM 1530
      IF(NTHER .NE. 0) CALL THERMD(NTYP)                                UBM 1540
C                                                                       UBM 1550
      IF(KMAT .EQ. 0) GOTO 16                                           UBM 1560
      WRITE (6,1010)                                                    UBM 1570
      II = 0                                                            UBM 1580
      DO 11 I=1,N10                                                     UBM 1590
        IF(II .LT. 2 .AND. AAA(I) .GT. 0.) II = I                       UBM 1600
        NRGAM(I) = AAA(I)                                               UBM 1610
        DENG(I) = AAA(I+N10)                                            UBM 1620
   11 CONTINUE                                                          UBM 1630
      IF(NZUS .LE. 0) GOTO 13                                           UBM 1640
      DO 12 I=1,NZUS                                                    UBM 1650
C                                                                       UBM 1660
CARD(S) D 17                                                            UBM 1670
C                                                                       UBM 1680
        READ (5,1005) NRGAM(N10+I),DENG(N10+I)                          UBM 1690
C                                                                       UBM 1700
   12 CONTINUE                                                          UBM 1710
   13 CONTINUE                                                          UBM 1720
      NISO = 0                                                          UBM 1730
      DO 15 I=1,KMA                                                     UBM 1740
        IMI = 0                                                         UBM 1750
        DO 15 J=1,N15                                                   UBM 1760
          IF(IMAT(I) .NE. NRGAM(J)) GOTO 14                             UBM 1770
          NISO = NISO + 1                                               UBM 1780
          NISO = NISO - IMI                                             UBM 1790
          IMI = IMI + 1                                                 UBM 1800
          NRMAF(NISO) = I                                               UBM 1810
          DEN(NISO) = DENG(J)                                           UBM 1820
   14     CONTINUE                                                      UBM 1830
   15 CONTINUE                                                          UBM 1840
      IF(IU4 .GT. 0) GOTO 105                                           UBM 1850
      IF(INDBS .EQ. 7 .OR. INDBS .EQ. 8) GOTO 105                       UBM 1860
      IF(INDBS .GT. 3) AAA(71) = BETA                                   UBM 1870
      DO 100 I=1,4                                                      UBM 1880
        IF(AAA(71) .LE. U5UG(I)) GOTO 101                               UBM 1890
  100 CONTINUE                                                          UBM 1900
  101 CONTINUE                                                          UBM 1910
      J = I                                                             UBM 1920
      IF(AAA(71) .EQ. U5UG(J) .OR. J .EQ. 1) GOTO 102                   UBM 1930
      X2 = U5UG(J)                                                      UBM 1940
      X1 = U5UG(J-1)                                                    UBM 1950
      Y2 = V4Z5(J)                                                      UBM 1960
      Y1 = V4Z5(J-1)                                                    UBM 1970
      V45 = Y1 + ((Y2-Y1)/(X2-X1)) * (AAA(71)-X1)                       UBM 1980
      GOTO 103                                                          UBM 1990
  102 CONTINUE                                                          UBM 2000
      V45 = V4Z5(J)                                                     UBM 2010
  103 CONTINUE                                                          UBM 2020
      NISO = NISO + 1                                                   UBM 2030
      DO 104 I=NISO,2,-1                                                UBM 2040
        NRMAF(I) = NRMAF(I-1)                                           UBM 2050
        DEN(I) = DEN(I-1)                                               UBM 2060
  104 CONTINUE                                                          UBM 2070
      NRMAF(1) = IMAU4                                                  UBM 2080
      K = 2                                                             UBM 2090
      M = 3                                                             UBM 2100
      IF(INDBS .LE. 3 .OR. INDBS .GT. 6) GOTO 106                       UBM 2110
      K = 3                                                             UBM 2120
      M = 4                                                             UBM 2130
  106 CONTINUE                                                          UBM 2140
      IF(U53 .EQ. 1) V45 = 0.                                           UBM 2150
      DEN(1) = DEN(K) * V45                                             UBM 2160
      DEN(M) = DEN(M) - DEN(1)                                          UBM 2170
  105 CONTINUE                                                          UBM 2180
      IF(IPUO2 .EQ. 1) DEN(1) = 0.                                      UBM 2190
      IF(BK .LT. 0.99999) GOTO 111                                      UBM 2200
      DO 110 I=1,NISO                                                   UBM 2210
        IF(NRMAF(I) .LE. IACT) DEN(I) = 0.                              UBM 2220
  110 CONTINUE                                                          UBM 2230
  111 CONTINUE                                                          UBM 2240
      WRITE (6,1006) NFUTP,(NRMAF(I),DEN(I),I=1,NISO)                   UBM 2250
      JI = 0                                                            UBM 2260
      IZEUG = 0                                                         UBM 2270
      NZEUG = 2                                                         UBM 2280
      MX = MMX                                                          UBM 2290
      MXP1 = MMX + 1                                                    UBM 2300
      FRACOA = 1.                                                       UBM 2310
   19 CONTINUE                                                          UBM 2320
      DO 17 J=1,MX                                                      UBM 2330
        III = J + JI                                                    UBM 2340
        IIII = 10 + J * 10 + II                                         UBM 2350
        FRACT(III) = AAA(IIII)                                          UBM 2360
   17 CONTINUE                                                          UBM 2370
      FRACT(MXP1+JI) = FRACOA                                           UBM 2380
      IZEUG = IZEUG + 1                                                 UBM 2390
      FRACOA = 0.                                                       UBM 2400
      JI = JI + MXP1                                                    UBM 2410
      II = 1                                                            UBM 2420
      IF(IZEUG .LT. 2) GOTO 19                                          UBM 2430
      MXZEUG = NZEUG * MXP1                                             UBM 2440
      IF(NS .EQ. 0) GOTO 33                                             UBM 2450
CFZJ051                                                       11.05.07  UBM 2460
      IF(BK .LT. 0.99999) WRITE (6,1007) (I,I=1,MMX)                    UBM 2470
      I2 = 0                                                            UBM 2480
      DO 31 J=1,NZEUG                                                   UBM 2490
        I1 = I2 + 1                                                     UBM 2500
        I2 = I1 + MMX                                                   UBM 2510
CFZJ051                                                       11.05.07  UBM 2520
        IF(BK .LT. 0.99999) WRITE (6,1008) J,(FRACT(I),I=I1,I2)         UBM 2530
   31 CONTINUE                                                          UBM 2540
   33 CONTINUE                                                          UBM 2550
      RADI(1) = AAA(75)                                                 UBM 2560
      RADI(2) = AAA(72)                                                 UBM 2570
      RADI(3) = AAA(83)                                                 UBM 2580
      RADI(4) = AAA(88)                                                 UBM 2590
      DO 32 I=1,3                                                       UBM 2600
        RADI(4+I) = AAA(91+I)                                           UBM 2610
   32 CONTINUE                                                          UBM 2620
CFZJ051                                                       11.05.07  UBM 2630
      IF(NS .NE. 0 .AND. BK .LT. 0.99999) WRITE (6,1009) (RADI(I),I=1,7)UBM 2640
      WRITE (NDA29,REC=NXT29) NISO,(NRMAF(I),DEN(I),I=1,NISO),(RADI(I), UBM 2650
     1 I=1,7),NZEUG,MXP1,MXZEUG,(FRACT(I),I=1,MXZEUG),AAA(75),AAA(76),  UBM 2660
     2 AAA(89),AAA(90),(AAA(I),I=79,88),NFU,NVR,NFUTP,FABKOS,AUFKOS,    UBM 2670
     3 AAA(95),(IMAT(NRMAF(I)),I=1,NISO)                                UBM 2680
      NXT29 = NXT29 + 1                                                 UBM 2690
   16 CONTINUE                                                          UBM 2700
      GOTO 7999                                                         UBM 2710
 8000 RETURN                                                            UBM 2720
C                                                                       UBM 2730
 1000 FORMAT (18A4)                                                     UBM 2740
 1001 FORMAT (6E12.5)                                                   UBM 2750
 1002 FORMAT (18I4)                                                     UBM 2760
 1005 FORMAT (I4,4X,E12.5)                                              UBM 2770
 1006 FORMAT (' FUEL TYPE',I5,'   VSOP-NO.   ATOM DENSITY'/15(19X,I6,   UBM 2780
     1 E16.6/))                                                         UBM 2790
 1007 FORMAT (/' CELL COMP.   MAT.FRACT. IN ZONE',5(I2,12X))            UBM 2800
 1008 FORMAT (5X,I2,18X,6E14.6)                                         UBM 2810
 1009 FORMAT (/' FOR THERMOS:'/6X,'R1   =',E13.6/6X,'R2   =',E13.6/6X,  UBM 2820
     1 'F    =',E13.6/6X,'ALPH =',E13.6/6X,'R3   =',E13.6/6X,'R4   =',  UBM 2830
     2 E13.6/6X,'R5   =',E13.6//' FOR ZUT-DGL THE SAME DATA AS GIVEN ABOUBM 2840
     3VE'//)                                                            UBM 2850
 1010 FORMAT (//11X,'DATA-TRANSFER TO V.S.O.P.'/)                       UBM 2860
 1011 FORMAT ('1',130('=')//1X,18A4, 5X,'FUEL TYPE: ',I3,',  VARIANT NO:UBM 2870
     1 ',I3,',  DATA-2-SET NO.: ',I3//)                                 UBM 2880
 1012 FORMAT (A4,2I4,10F6.0)                                            UBM 2890
 1013 FORMAT (///' COST INPUT FABRICATION:'//' COATED PARTICLE TYPES',  UBM 2900
     1 I21,5I12)                                                        UBM 2910
 1014 FORMAT (' FABRICATION COSTS     ',A4,'/KG-HM = ',6E12.5)          UBM 2920
 1015 FORMAT ('1'//' GAM-LIBRARY IDENTIFICATION FOR THE',I4,' VSOP ISOTOUBM 2930
     1PES:'/)                                                           UBM 2940
 1016 FORMAT (/' FUEL ELEMENT TYPES',I24,3I12)                          UBM 2950
 1017 FORMAT (' FABRICATION COSTS      ',A4,'/BALL = ',4E12.5)          UBM 2960
 1018 FORMAT (' REPROCESSING:'/' SHIPPING, HEAD END     ',A4,'/KG-C = ',UBM 2970
     1 E12.5/' SOLVENT EXTRACTION,'/' PRODUCT CONVERSION    ',A4,'/KG-HMUBM 2980
     2 = ',E12.5/' WASTE TREATMENT, DIS-'/' POSAL, PER 10% FIMA   ',A4, UBM 2990
     3 '/KG-HM = ',E12.5)                                               UBM 3000
 1019 FORMAT (/' GRAPHITE BALLS         ',A4,'/BALL = ', E12.5//)       UBM 3010
 1020 FORMAT (5I4,E12.5)                                                UBM 3020
      END                                                               UBM 3030
      SUBROUTINE DAT                                                    AT    10
C                                                                       AT    20
      CHARACTER*2 DATG(4)                                               AT    30
C                                                                       AT    40
      CHARACTER*4 DATH                                                  AT    50
C                                                                       AT    60
      CHARACTER*8 DATF                                                  AT    70
C                                                                       AT    80
      EQUIVALENCE(DATG(1),DATF),(DATH,DATF)                             AT    90
C                                                                       AT   100
C                                                                       AT   110
      CALL DATE_AND_TIME(DATF)                                          AT   120
C                                                                       AT   130
      READ (UNIT=DATH,FMT=101) JAHR                                     AT   140
      READ (UNIT=DATG(3),FMT=100) IMON                                  AT   150
      READ (UNIT=DATG(4),FMT=100) ITAG                                  AT   160
      WRITE (6,2000)                                                    AT   170
      WRITE (6,20)                                                      AT   180
      WRITE (6,2001) ITAG,IMON,JAHR                                     AT   190
      WRITE (6,30)                                                      AT   200
C                                                                       AT   210
   20 FORMAT (//////35X,'**',5X,'**',7X,7('*'),8X,7('*'),7X,8('*')/35X, AT   220
     1 '**',5X,'**',3(6X,'**',5X,'**')/35X,'**',5X,'**',6X,'**',13X,'**'AT   230
     2 ,5X,'**',6X,8('*')/36X,'**',3X,'**',8X,7('*'),7X,'**',5X,'**',6X,AT   240
     3 '**'/37X,'**',1X,'**',4X,'**',2X,'*',6X,'**',2(2X,'**'),5X,'**', AT   250
     4 2(2X,'**'),9X,'**'/38X,'***',5X,'**',3X,7('*'),3X,'**',3X,7('*'),AT   260
     5 3X,'**',2X,'**',9X,'**'////)                                     AT   270
   30 FORMAT (//////////17X,'JAN. 2012 ',64X,'REPORT: V.S.O.P.(99/11)'/ AT   280
     1 99X,'JUEL - 4348'/17X,'JUNE 2010 ',64X,'REPORT: V.S.O.P.(99/09)'/AT   281
     1 99X,'JUEL - 4326'/99X,'SECTION 4.3')                             AT   290
  100 FORMAT (4I2)                                                      AT   300
  101 FORMAT (I4)                                                       AT   310
 2000 FORMAT ('1'////18X,12('*'),11X,'****',10X,13('*'),10X,'****',18X, AT   320
     1 11('*')/18X,13('*'),9X,6('*'),9X,13('*'),9X,6('*'),16X,13('*')/  AT   330
     2 18X,'***',6X,'****',8X,'***',2X,'***',13X,'***',13X,'***',2X,    AT   340
     3 '***',15X,'****',5X,'****'/18X,'***',7X,'***',8X,'***',2X,'***', AT   350
     4 2(13X,'***'),2X,'***',15X,'***',6X,'***'/18X,'***',2(7X,'***'),4XAT   360
     5 ,'***',2(12X,'***'),4X,'***',22X,'***'/18X,'***',2(7X,'***'),4X, AT   370
     6 '***',2(12X,'***'),4X,'***',4X,8('*'),9X,'***'/18X,'***',7X,'***'AT   380
     7 ,2(6X,'***'),2(11X,'***'),6X,'***',3X,8('*'),8X,'***'/18X,'***', AT   390
     8 7X,'***',2(6X,'***'),2(11X,'***'),6X,'***',18X,'***'/18X,'***',7XAT   400
     9 ,'***',5X,14('*'),10X,'***',10X,14('*'),16X,'***'/18X,'***',6X,  AT   410
     X '****',5X,14('*'),10X,'***',10X,14('*'),15X,'***',6X,'**'/18X,13 AT   420
     Y ('*'),5X,'***',8X,'***',2(10X,'***'),8X,'***',14X,12('*')/18X,12 AT   430
     Z ('*'),6X,'***',8X,'***',2(10X,'***'),8X,'***',13X,13('*'))       AT   440
 2001 FORMAT (55X,'( ',I2,'.',I2,'.',I4,' )')                           AT   450
      RETURN                                                            AT   460
      END                                                               AT   470
      BLOCK DATA                                                        BLO   10
C                                                                       BLO   20
      COMMON /CONST/ ANUKL,NGAM,GEW,AVO,PI                              BLO   30
C                                                                       BLO   40
      COMMON /FUEL/ BRST,COAT                                           BLO   50
C                                                                       BLO   60
      COMMON /GAMRES/ SPTH,SPU8,SPC,SPO,SPPU2,SPU5                      BLO   70
C                                                                       BLO   80
      COMMON /ANR4/ V4Z5,U5UG                                           BLO   90
C                                                                       BLO  100
      COMMON /SMDEF/ IMA                                                BLO  110
C                                                                       BLO  120
      CHARACTER*8 BRST(9) /'UO2     ','UC      ','UC2     ','THO2-UO2', BLO  130
     1 'THC-UC  ','THC2-UC2','PUO2    ','PU-TH-O2','PUO2-UO2'/          BLO  140
C                                                                       BLO  150
      CHARACTER*4 COAT(2)/'PYC ','SIC '/,ANUKL(10)/'C   ','TH  ','U-35',BLO  160
     1 'U-38','PU39','PU40','PU41','PU42','O   ','SI  '/                BLO  170
C                                                                       BLO  180
      REAL SPTH/12.6/,SPU8/11.29/,SPC/4.6/,SPO/3.8/,SPPU2/10.69/,SPU5   BLO  190
     1 /15./,AVO/0.6022045/,PI/3.14159/,V4Z5(4)/7.5E-3,9.86E-3,1.01E-2, BLO  200
     2 1.11E-2/,U5UG(4)/7.2E-3,0.1,0.167,0.93/,GEW(10)/12.011,232.111,  BLO  210
     3 235.117,238.125,239.127,240.129,241.131,242.134,16.0,40.101/     BLO  220
C                                                                       BLO  230
      INTEGER NGAM(10)/5,6,10,12,14,15,16,17,23,26/,IMA(30)/6,185,7,8,9,BLO  240
     1 10,11,132,12,186,133,187,13,188,177,14,15,16,17,189,178,180,179, BLO  250
     2 181,190,182,183,184,0,0/                                         BLO  260
      END                                                               BLO  270
      SUBROUTINE CP(NFOLG,IU8T)                                         CP    10
C                                                                       CP    20
      CHARACTER*4 ANUC3/'U-33'/,ANUC5/'U-35'/,COAT,ANUKL,QCOAT          CP    30
C                                                                       CP    40
      CHARACTER*8 BRST(9)                                               CP    50
C                                                                       CP    60
      COMMON /VNISA/ V(10),NIS(10),A(10,10)                             CP    70
C                                                                       CP    80
      COMMON /CONST/ ANUKL(10),NGAM(10),GEW(10),AVO,PI                  CP    90
C                                                                       CP   100
      COMMON /FUEL/ BRST,COAT(2)                                        CP   110
C                                                                       CP   120
      COMMON /CPDAT/ ANR,INDBS,NCT,NSIC1,NSIC2,K,PU(4),DCT(5),ROCT(5),RKCP   130
     1 ,ROBR1,ROBR2,BETA                                                CP   140
C                                                                       CP   150
      COMMON /GRES/ ROBR,FAKMOL,VCP,VMOD,ROSM,VKUGEL,VSTAB,GSMCP,GSPCP, CP   160
     1 GSM,GSP,ASM,BK,RKUGEL,DREF,DCORE,HCORE,FF1,FF2,R1,R2,QR0,RSH     CP   170
C                                                                       CP   180
      COMMON /AAAA/ AAA(100),RADIU                                      CP   190
C                                                                       CP   200
      COMMON /COSTS/ KOSTDA,KOSTD2,FIMA,HK,AK,EK,CURCY,NC,NF,FC(6),FF(4),FRC(6)CP   210
     1 ,FRF(4),FABKOS,AUFKOS,DK,S(31),IU8TH                             CP   220
C                                                                       CP   230
      COMMON /DATATA/ NXT29,NDA29,IPUO2,U53                             CP   240
C                                                                       CP   250
C                                                                       CP   260
      VOL(X1,X2) = (X2**3-X1**3) * 4. * PI / 3.                         CP   270
      DO 98 I=1,10                                                      CP   280
        NIS(I) = 0                                                      CP   290
        V(I) = 0.                                                       CP   300
        DO 98 J=1,10                                                    CP   310
          A(J,I) = 0.                                                   CP   320
   98 CONTINUE                                                          CP   330
C                                                                       CP   340
CARD D 7                                                                CP   350
C                                                                       CP   360
      READ (5,1001) ANR,U53,FIMA,(FRC(I),I=1,NC)                        CP   370
C                                                                       CP   380
C     CONVERSION FROM WEIGHT-% INTO NUMBER DENSITY-%                    CP   390
C                                                                       CP   400
      IF(ANR .LT. 0.) ANR = -ANR / (1.+(1.+ANR)*(GEW(3)/GEW(4)-1.))     CP   410
      IF(ANR .NE. 0.) WRITE (6,2007) ANR                                CP   420
      AAA(71) = ANR                                                     CP   430
      IU8TH = IU8T                                                      CP   440
      IF(NFOLG .EQ. 1) GOTO 99                                          CP   450
      ANUKL(3) = ANUC5                                                  CP   460
      NGAM(3) = 10                                                      CP   470
      GEW(3) = 235.117                                                  CP   480
C                                                                       CP   490
CARD D 8                                                                CP   500
C                                                                       CP   510
      READ (5,1000) INDBS,NCT,NSIC1,NSIC2                               CP   520
C                                                                       CP   530
C     HIER WIRD DAS TAUSCHGESCHEFT U-235 GEGEN U-233 GEMACHT            CP   540
C                                                                       CP   550
      IU8TH = 0.                                                        CP   560
      IU8T = IU8TH                                                      CP   570
      IF(U53 .EQ. 0.) GOTO 1                                            CP   580
      ANUKL(3) = ANUC3                                                  CP   590
      NGAM(3) = 8                                                       CP   600
      GEW(3) = 233.112                                                  CP   610
    1 K = INDBS                                                         CP   620
      WRITE (6,2000) BRST(INDBS)                                        CP   630
C                                                                       CP   640
CARD D 9                                                                CP   650
C                                                                       CP   660
      READ (5,1001) RK,ROBR1,ROBR2,BETA                                 CP   670
C                                                                       CP   680
      IF(K .NE. 7 .AND. K .NE. 8 .AND. K .NE. 9) GOTO 9999              CP   690
C                                                                       CP   700
CARD D 10                                                               CP   710
C                                                                       CP   720
      READ (5,1001) (PU(I),I=1,4)                                       CP   730
C                                                                       CP   740
      IF(K .NE. 7) GOTO 9999                                            CP   750
      ANR = (PU(1)+PU(3)) * 0.999999                                    CP   760
      K = 8                                                             CP   770
      INDBS = 8                                                         CP   780
      IPUO2 = 1                                                         CP   790
 9999 CONTINUE                                                          CP   800
C                                                                       CP   810
CARD D 11                                                               CP   820
C                                                                       CP   830
      READ (5,1001) (DCT(I),ROCT(I),I=1,NCT)                            CP   840
C                                                                       CP   850
      WRITE (6,2001) RK,ROBR1,ROBR2                                     CP   860
      IF(ROBR2 .EQ. 0.) ROBR2 = ROBR1                                   CP   870
      DO 12 I=1,NCT                                                     CP   880
        QCOAT = COAT(1)                                                 CP   890
        IF(I .EQ. NSIC1 .OR. I .EQ. NSIC2) QCOAT = COAT(2)              CP   900
        WRITE (6,2002) DCT(I),ROCT(I),QCOAT                             CP   910
   12 CONTINUE                                                          CP   920
      IF(K .EQ. 7 .OR. K .EQ. 8 .OR. K .EQ. 9) WRITE (6,2005) (PU(I),I= CP   930
     1 1,4)                                                             CP   940
   99 CONTINUE                                                          CP   950
      NIS(1) = 1                                                        CP   960
      GOTO(20,20,20,97,97,97,97,97,20),K                                CP   970
   97 CONTINUE                                                          CP   980
      NIS(2) = 1                                                        CP   990
      IF(IU8TH .EQ. 2) NIS(2) = 2                                       CP  1000
      IF(IU8TH .EQ. 3) NIS(2) = 3                                       CP  1010
      IF(IU8TH .EQ. 4) NIS(2) = 4                                       CP  1020
   20 CONTINUE                                                          CP  1030
      IF(K .NE. 7 .AND. K .NE. 8) NIS(3) = 1                            CP  1040
      IF(K .NE. 7 .AND. K .NE. 8) NIS(4) = 1                            CP  1050
      IF(K .NE. 7 .AND. K .NE. 8 .AND. K .NE. 9) GOTO 21                CP  1060
      NIS(5) = 1                                                        CP  1070
      NIS(6) = 1                                                        CP  1080
      NIS(7) = 1                                                        CP  1090
      NIS(8) = 1                                                        CP  1100
   21 CONTINUE                                                          CP  1110
      IF(K .EQ. 1 .OR. K .EQ. 4 .OR. K .EQ. 7 .OR. K .EQ. 8 .OR. K .EQ. CP  1120
     1 9) NIS(9) = 1                                                    CP  1130
      IF(NSIC1 .NE. 0 .OR. NSIC2 .NE. 0) NIS(10) = 1                    CP  1140
      RCP = RK                                                          CP  1150
      DO 14 I=1,NCT                                                     CP  1160
        RCP = RCP + DCT(I)                                              CP  1170
   14 CONTINUE                                                          CP  1180
C                                                                       CP  1190
C     AUSSERER RADIUS DER CP                                            CP  1200
C                                                                       CP  1210
      AAA(72) = RCP                                                     CP  1220
      V(1) = VOL(0.,RK)                                                 CP  1230
      V(2) = VOL(RK,RCP)                                                CP  1240
      SUM = 0.                                                          CP  1250
      VSIC = 0.                                                         CP  1260
      X1 = RK                                                           CP  1270
      DO 16 I=1,NCT                                                     CP  1280
        X2 = X1 + DCT(I)                                                CP  1290
        W = VOL(X1,X2)                                                  CP  1300
        IF(I .NE. NSIC1 .AND. I .NE. NSIC2) GOTO 15                     CP  1310
        VSIC = VSIC + W                                                 CP  1320
        W = W * GEW(1) / GEW(10)                                        CP  1330
   15   SUM = SUM + ROCT(I) * W                                         CP  1340
        X1 = X2                                                         CP  1350
   16 CONTINUE                                                          CP  1360
      A(1,2) = AVO * SUM / (GEW(1)*V(2))                                CP  1370
      IF(NSIC1 .NE. 0 .OR. NSIC2 .NE. 0) A(10,2) = AVO * ROCT(NSIC1) *  CP  1380
     1 VSIC / (GEW(10)*V(2))                                            CP  1390
      GOTO(100,200,300,400,500,600,700,800,900),INDBS                   CP  1400
  100 ALFA = 2.                                                         CP  1410
      ISTR = 9                                                          CP  1420
      GOTO 22                                                           CP  1430
  200 ALFA = 1.                                                         CP  1440
      ISTR = 1                                                          CP  1450
      GOTO 22                                                           CP  1460
  300 ALFA = 2.                                                         CP  1470
      ISTR = 1                                                          CP  1480
   22 GBR = GEW(3) * ANR + GEW(4) * (1.-ANR) + ALFA * GEW(ISTR)         CP  1490
      ROBR = ROBR1                                                      CP  1500
      FAKMOL = 1. - ALFA * GEW(ISTR) / GBR                              CP  1510
      ASM = AVO * ROBR1 / GBR                                           CP  1520
      A(3,1) = ASM * ANR                                                CP  1530
      A(4,1) = ASM * (1.-ANR)                                           CP  1540
      A(ISTR,1) = ALFA * ASM                                            CP  1550
      GOTO 17                                                           CP  1560
  400 ALFA = 2.                                                         CP  1570
      ISTR = 9                                                          CP  1580
      GOTO 23                                                           CP  1590
  500 ALFA = 1.                                                         CP  1600
      ISTR = 1                                                          CP  1610
      GOTO 23                                                           CP  1620
  600 ALFA = 2.                                                         CP  1630
      ISTR = 1                                                          CP  1640
   23 G1 = GEW(2) + ALFA * GEW(ISTR)                                    CP  1650
      Z1 = 1. / ANR - 1. / BETA                                         CP  1660
      Z2 = 1. / BETA - 1.                                               CP  1670
      GAMA1 = G1 * Z1                                                   CP  1680
      GAMA2 = GEW(3) + GEW(4) * Z2 + ALFA * GEW(ISTR) / BETA            CP  1690
      GAMA = GAMA1 / (GAMA1+GAMA2)                                      CP  1700
      ROBR = 1. / (GAMA/ROBR1+(1.-GAMA)/ROBR2)                          CP  1710
      GBR = ANR * (GEW(2)*Z1+GEW(3)+GEW(4)*Z2) + ALFA * GEW(ISTR)       CP  1720
      FAKMOL = 1. - ALFA * GEW(ISTR) / GBR                              CP  1730
      ASM = AVO * ROBR / GBR                                            CP  1740
      A(3,1) = ANR * ASM                                                CP  1750
      A(4,1) = Z2 * A(3,1)                                              CP  1760
      A(2,1) = Z1 * A(3,1)                                              CP  1770
      A(ISTR,1) = ALFA * ASM                                            CP  1780
      GOTO 17                                                           CP  1790
  700 ANR = PU(1) + PU(3)                                               CP  1800
      GBR = GEW(9) + GEW(9)                                             CP  1810
      DO 701 I=1,4                                                      CP  1820
        GBR = GBR + PU(I) * GEW(I+4)                                    CP  1830
  701 CONTINUE                                                          CP  1840
      FAKMOL = 1. - 2. * GEW(9) / GBR                                   CP  1850
      ASM = AVO * ROBR1 / GBR                                           CP  1860
      DO 703 I=1,4                                                      CP  1870
        A(I+4,1) = ASM * PU(I)                                          CP  1880
  703 CONTINUE                                                          CP  1890
      A(9,1) = ASM + ASM                                                CP  1900
      ROBR = ROBR1                                                      CP  1910
      GOTO 17                                                           CP  1920
  800 G1 = GEW(2) + 2. * GEW(9)                                         CP  1930
      G2 = 0.                                                           CP  1940
      DO 801 I=1,4                                                      CP  1950
        G2 = G2 + GEW(I+4) * PU(I)                                      CP  1960
  801 CONTINUE                                                          CP  1970
      GAMA1 = G1 * ((PU(1)+PU(3))/ANR-1.)                               CP  1980
      GAMA2 = G2 + 2. * GEW(9)                                          CP  1990
      GAMA = GAMA1 / (GAMA1+GAMA2)                                      CP  2000
      ROBR = 1. / (GAMA/ROBR1+(1.-GAMA)/ROBR2)                          CP  2010
      XXPU = ANR / (PU(1)+PU(3))                                        CP  2020
      GBR = G2 * XXPU + GEW(2) * (1.-XXPU) + 2. * GEW(9)                CP  2030
      FAKMOL = 1. - 2. * GEW(9) / GBR                                   CP  2040
      ASM = AVO * ROBR / GBR                                            CP  2050
      DO 802 I=1,4                                                      CP  2060
        A(I+4,1) = PU(I) * ASM * XXPU                                   CP  2070
  802 CONTINUE                                                          CP  2080
      A(2,1) = (1.-XXPU) * ASM                                          CP  2090
      A(9,1) = 2. * ASM                                                 CP  2100
      GOTO 17                                                           CP  2110
  900 CONTINUE                                                          CP  2120
      OMEGA = (ANR-BETA) / (PU(1)+PU(3)-ANR)                            CP  2130
      WRITE (6,2009) OMEGA,ANR,PU(1),PU(3),BETA                         CP  2140
      GPU = 0.                                                          CP  2150
      DO 901 I=1,4                                                      CP  2160
        GPU = GPU + PU(I) * (GEW(I+4)+2.*GEW(9))                        CP  2170
  901 CONTINUE                                                          CP  2180
      GU = BETA * (GEW(3)+2.*GEW(9)) + (1.-BETA) * (GEW(4)+2.*GEW(9))   CP  2190
      GAMPU = GPU / (GPU+GU/OMEGA)                                      CP  2200
      OO = OMEGA / (OMEGA+1.)                                           CP  2210
      ROBR = 1. / (GAMPU/ROBR1+(1.-GAMPU)/ROBR2)                        CP  2220
      GBR = GU / (1.+OMEGA) + GPU * OO                                  CP  2230
      ASM = AVO * ROBR / GBR                                            CP  2240
      DO 902 I=1,4                                                      CP  2250
        A(I+4,1) = ASM * OO * PU(I)                                     CP  2260
  902 CONTINUE                                                          CP  2270
      A(3,1) = ASM * (1.-OO) * BETA                                     CP  2280
      A(4,1) = ASM * (1.-OO) * (1.-BETA)                                CP  2290
      A(9,1) = (A(3,1)+A(4,1)+A(5,1)+A(6,1)+A(7,1)+A(8,1)) * 2.         CP  2300
      FAKMOL = 1. - 2. * GEW(9) / GBR                                   CP  2310
   17 WRITE (6,2003)                                                    CP  2320
      DO 18 I=1,10                                                      CP  2330
        IF(NIS(I) .EQ. 0) GOTO 18                                       CP  2340
        WRITE (6,2004) NGAM(I),ANUKL(I),(A(I,J),J=1,2)                  CP  2350
   18 CONTINUE                                                          CP  2360
      VCP = V(1) + V(2)                                                 CP  2370
      WRITE (6,2006) V(1),V(2)                                          CP  2380
      GSMCP = ASM * GBR * FAKMOL * V(1) / AVO                           CP  2390
      GSPCP = A(3,1) * GEW(3) * V(1) / AVO                              CP  2400
      IF(INDBS .EQ. 7 .OR. INDBS .EQ. 8) GSPCP = (A(5,1)*GEW(5)+A(7,1)* CP  2410
     1 GEW(7)) *V(1) / AVO                                              CP  2420
      IF(INDBS .EQ. 9) GSPCP = (A(5,1)*GEW(5)+A(7,1)*GEW(7)+A(3,1)*     CP  2430
     1 GEW(3)) *V(1) / AVO                                              CP  2440
      RETURN                                                            CP  2450
C                                                                       CP  2460
 1000 FORMAT (18I4)                                                     CP  2470
 1001 FORMAT (6E12.5)                                                   CP  2480
 2000 FORMAT (1H /1H ,10X,'FUEL = ',A8//11X,'COATED PARTICLES - DATA'//)CP  2490
 2001 FORMAT (20X,'PART.RADIUS (CM) = ',E12.5,10X,'RO1,RO2    (G/CC) = 'CP  2500
     1 ,2E12.5//20X,'COATING-LAYERS:')                                  CP  2510
 2002 FORMAT (20X,'THICKNESS   (CM) = ',E12.5,10X,'DENSITY    (G/CC) = 'CP  2520
     1 ,E12.5,5X,A4)                                                    CP  2530
 2003 FORMAT (1H /1H ,8X,'GAMNO  ISOTOP   PARTICLE       COATING'//)    CP  2540
 2004 FORMAT (I14,2X,A4,2E14.5)                                         CP  2550
 2005 FORMAT (/10X,'ATOMS-FRACTION OF PU: PU-239 = ',F6.4/32X,'PU-240 = CP  2560
     1',F6.4/32X,'PU-241 = ',F6.4/32X,'PU-242 = ',F6.4)                 CP  2570
 2006 FORMAT (1H /1H ,11X,'VOLUME  ',2E14.5//)                          CP  2580
 2007 FORMAT (1H /1H ,10X,'FISSILE ENRICHMENT (N-FISS/N-HM) =',E15.5//) CP  2590
 2009 FORMAT (1H ,'OMEGA=',E12.5,' TOT.ENRCH.=',E12.5,' PU39/PU41',E12.5CP  2600
     1 ,'/',E12.5,' U-ENRICH. =',E12.5//)                               CP  2610
      END                                                               CP  2620
      SUBROUTINE KUGEL(NFOLG,*)                                         KUG   10
C                                                                       KUG   20
      COMMON /VNISA/ V(10),NIS(10),A(10,10)                             KUG   30
C                                                                       KUG   40
      COMMON /CONST/ ANUKL(10),NGAM(10),GEW(10),AVO,PI                  KUG   50
C                                                                       KUG   60
      COMMON /KUDAT/ ROMTX,ROSCH,QR1,QR2,QROSM,QFF1,QINDBK,QBK,QVMOD    KUG   70
C                                                                       KUG   80
      COMMON /GRES/ ROBR,FAKMOL,VCP,VMOD,ROSM,VKUGEL,VSTAB,GSMCP,GSPCP, KUG   90
     1 GSM,GSP,ASM,BK,RKUGEL,DREF,DCORE,HCORE,FF1,FF2,R1,R2,QR0,RSH     KUG  100
C                                                                       KUG  110
      COMMON /AAAA/ AAA(100),RADIU                                      KUG  120
C                                                                       KUG  130
      COMMON /COSTS/ KOSTDA,KOSTD2,FIMA,HK,AK,EK,CURCY,NC,NF,FC(6),FF(4),FRC(6)KUG  140
     1 ,FRF(4),FABKOS,AUFKOS,DK,S(31),IU8TH                             KUG  150
C                                                                       KUG  160
      COMMON /RVERH/ ROVERH,CBK,ROBK                                    KUG  170
C                                                                       KUG  180
      CHARACTER*4 ANUKL                                                 KUG  190
C                                                                       KUG  200
C                                                                       KUG  210
      VOL(X1,X2) = (X2**3-X1**3) * 4. * PI / 3.                         KUG  220
      RAD(VV) = EXP(ALOG(VV*0.75/PI)/3.)                                KUG  230
      DO 98 I=3,10                                                      KUG  240
        V(I) = 0.                                                       KUG  250
        DO 98 J=1,10                                                    KUG  260
          A(J,I) = 0.                                                   KUG  270
   98 CONTINUE                                                          KUG  280
      CBK = 0.                                                          KUG  290
      WRITE (6,2000)                                                    KUG  300
      IF(NFOLG .EQ. 0) GOTO 99                                          KUG  310
C                                                                       KUG  320
CFZJ016 Delete QINDBK, QBK and ROBK from input                12.12.03  KUG  330
CFZJ029 Move QROSM from card D13 to card D12                  01.04.04  KUG  340
CARD D 12                                                               KUG  350
C                                                                       KUG  360
      READ (5,1001) QR1,QR2,QFF1,QVMOD,QROSM                            KUG  370
C                                                                       KUG  380
      ROBK=0.                                                           KUG  390
C                                                                       KUG  400
CARD D 13                                                               KUG  410
C                                                                       KUG  420
      READ (5,1001) ROMTX,ROSCH,QR0,(FRF(I),I=1,NF)                     KUG  430
      IF(QFF1 .EQ. 0. .AND. QVMOD .EQ. 0. .AND. QROSM .EQ. 0.) QROSM =  KUG  440
     1 1.E-5                                                            KUG  450
      IF(QR1 .EQ. 0. .AND. QVMOD .EQ. 0.) QR1 = 2.5                     KUG  460
      IF(QINDBK .EQ. 1.) ROBK = ROSCH                                   KUG  470
      WRITE (6,2001) ROMTX,ROSCH                                        KUG  480
      ROVERH = ROBK / ROSCH                                             KUG  490
   99 CONTINUE                                                          KUG  500
      R1 = QR1                                                          KUG  510
      R2 = QR2                                                          KUG  520
      ROSM = QROSM                                                      KUG  530
      FF1 = QFF1                                                        KUG  540
      BK = QBK                                                          KUG  550
      IF(BK .GE. 1.) BK = 0.99999                                       KUG  560
      VMOD = QVMOD                                                      KUG  570
      IF(QR1 .LE. 0. .OR. QR0 .LE. 0.) GOTO 20                          KUG  580
      R1 = (QR1**3.-QR0**3.)**(1./3.)                                   KUG  590
   20 CONTINUE                                                          KUG  600
      ALFA = FAKMOL * ROBR * V(1) / VCP                                 KUG  610
      A(1,3) = AVO * ROMTX / GEW(1)                                     KUG  620
      A(1,4) = AVO * ROSCH / GEW(1)                                     KUG  630
      IF(R1 .EQ. 0.) GOTO 2                                             KUG  640
      V(3) = VOL(0.,R1)                                                 KUG  650
      IF(R2 .EQ. 0.) GOTO 2                                             KUG  660
      V(4) = VOL(R1,R2)                                                 KUG  670
      IF(ROSM .EQ. 0. .AND. FF1 .EQ. 0.) GOTO 1                         KUG  680
      IF(ROSM .EQ. 0.) ROSM = ALFA * FF1                                KUG  690
      IF(FF1 .EQ. 0.) FF1 = ROSM / ALFA                                 KUG  700
      GAMA = A(1,2) * V(2) * V(3) * FF1 / VCP + A(1,3) * V(3) * (1.-FF1)KUG  710
     1 + A(1,4) * V(4)                                                  KUG  720
      IF(QINDBK .EQ. 0.) GOTO 5                                         KUG  730
      WRITE (6,2004)                                                    KUG  740
      BETA = BK / (1.-BK)                                               KUG  750
      VBK = V(3) + V(4)                                                 KUG  760
      ABK = AVO * ROBK / GEW(1)                                         KUG  770
      IF(QBK .EQ. 0.) GOTO 4                                            KUG  780
      GAMA = GAMA + BETA * ABK * VBK                                    KUG  790
      GOTO 5                                                            KUG  800
    4 IF(VMOD .NE. 0.) GOTO 6                                           KUG  810
      WRITE (6,2005)                                                    KUG  820
      STOP                                                              KUG  830
    6 CONTINUE                                                          KUG  840
      BETA = (VMOD*ASM*V(1)*FF1*V(3)/VCP-GAMA) / (VBK*ABK)              KUG  850
      BK = BETA / (1.+BETA)                                             KUG  860
      GOTO 10                                                           KUG  870
    5 VMOD = GAMA / (ASM*V(1)*FF1*V(3)/VCP)                             KUG  880
      GOTO 10                                                           KUG  890
    1 FF1 = (A(1,3)*V(3)+A(1,4)*V(4)) / ((VMOD*ASM*V(1)-A(1,2)*V(2))*   KUG  900
     1 V(3)/VCP+A(1,3)*V(3))                                            KUG  910
      ROSM = ALFA * FF1                                                 KUG  920
      GOTO 10                                                           KUG  930
    2 IF(ROSM .EQ. 0.) ROSM = ALFA * FF1                                KUG  940
      IF(FF1 .EQ. 0.) FF1 = ROSM / ALFA                                 KUG  950
      IF(R1 .EQ. 0.) GOTO 3                                             KUG  960
      V(4) = ((VMOD*ASM*V(1)-A(1,2)*V(2))*V(3)*FF1/VCP-A(1,3)*V(3)*     KUG  970
     1 (1.-FF1)) / A(1,4)                                               KUG  980
      BETA = V(4) + V(3)                                                KUG  990
      R2 = RAD(BETA)                                                    KUG 1000
      GOTO 10                                                           KUG 1010
    3 ALFA = VOL(0.,R2)                                                 KUG 1020
      V(3) = A(1,4) * ALFA / ((VMOD*ASM*V(1)-A(1,2)*V(2))*FF1/VCP+A(1,4)KUG 1030
     1 -A(1,3)*(1.-FF1))                                                KUG 1040
      R1 = RAD(V(3))                                                    KUG 1050
      V(4) = VOL(0.,R2) - V(3)                                          KUG 1060
   10 CONTINUE                                                          KUG 1070
      ALFA = FF1 * V(3) / VCP                                           KUG 1080
      GSM = ALFA * GSMCP                                                KUG 1090
      GSP = ALFA * GSPCP                                                KUG 1100
      VKUGEL = V(3) + V(4)                                              KUG 1110
      RKUGEL = R2                                                       KUG 1120
CFZJ051                                                       11.05.07  KUG 1130
      IF(BK .LT. 0.99999) WRITE (6,2009) GSM,GSP                        KUG 1140
      RSH = R1                                                          KUG 1150
      IF(QR0 .LE. 0.) GOTO 21                                           KUG 1160
      RSH = (R1**3.+QR0**3.)**(1./3.)                                   KUG 1170
      WRITE (6,1999) QR0                                                KUG 1180
   21 CONTINUE                                                          KUG 1190
CFZJ051                                                       11.05.07  KUG 1200
      IF(BK .LT. 0.99999) WRITE (6,2002) RSH,R2,ROSM,FF1,VMOD           KUG 1210
      IF(BK .EQ. 0.99999) WRITE (6,2007) R2                             KUG 1220
      ZKUKGS = 1000. / GSM                                              KUG 1230
      IF(KOSTDA) 9,9,8                                                  KUG 1240
C                                                                       KUG 1250
C     FABRIKATIONS- + AUFARBEITUNGSKOSTEN                               KUG 1260
C                                                                       KUG 1270
    8 CONTINUE                                                          KUG 1280
      GK = 0.                                                           KUG 1290
      DO 7 I=1,NF                                                       KUG 1300
        IF(FRF(I) .GT. 0.) GK = FF(I) * FRF(I)                          KUG 1310
    7 CONTINUE                                                          KUG 1320
      COST2 = 0.                                                        KUG 1330
      DO 11 I=1,NC                                                      KUG 1340
        COST2 = COST2 + FC(I) * FRC(I) * GSM / 1000.                    KUG 1350
   11 CONTINUE                                                          KUG 1360
      COST3 = BK * DK / (1.-BK)                                         KUG 1370
      FABRI = COST2 + GK + COST3                                        KUG 1380
      FABKOS = ZKUKGS * FABRI                                           KUG 1390
      GRGRKU = V(4) * ROSCH + V(3) * ROMTX * (1-FF1) + FF1 * V(3) * V(2)KUG 1400
     1 * A(1,2) * GEW(1) / (VCP*AVO)                                    KUG 1410
      AUFKOS = (HK*ZKUKGS*GRGRKU/1000.+AK+(EK*FIMA/0.1)) / (1.-FIMA)    KUG 1420
CFZJ051                                                       11.05.07  KUG 1430
      IF(BK .EQ. 0.99999) GOTO 66                                       KUG 1440
      WRITE (6,2003) V(3),V(4),GRGRKU,ZKUKGS,FIMA                       KUG 1450
      WRITE (6,3000) CURCY,FABRI,COST2,GK,COST3,CURCY,FABKOS,CURCY,     KUG 1460
     1 AUFKOS                                                           KUG 1470
   66 CONTINUE                                                          KUG 1480
C                                                                       KUG 1490
C     FUELLFAKTOR DER CP IN BRENNSTOFF -ZONE                            KUG 1500
C                                                                       KUG 1510
    9 AAA(73) = FF1                                                     KUG 1520
      RETURN 1                                                          KUG 1530
C                                                                       KUG 1540
 1001 FORMAT (6E12.5)                                                   KUG 1550
 1999 FORMAT (10X,'SHELL BALL, R0            (CM) =',E13.5)             KUG 1560
 2000 FORMAT (10X,'SPHERICAL ELEMENTS'/)                                KUG 1570
 2001 FORMAT (10X,'DENSITY C-MATRIX        (G/CC) =',E12.5/10X,'DENSITY KUG 1580
     1C-SHELL         (G/CC) =',E12.5/)                                 KUG 1590
 2002 FORMAT (10X,'RADIUS OF FUEL-ZONE       (CM) =',E13.5/10X,'RADIUS OKUG 1600
     1F BALL            (CM) =',E13.5/10X,'DENSITY HM IN FUEL-ZONE (G/CCKUG 1610
     2) =',E13.5/10X,'FFACTOR CP IN FUEL-ZONE        =',E13.5/10X,'MODERKUG 1620
     3ATION RATIO  (C-AT/HM-AT) =',E13.5)                               KUG 1630
 2003 FORMAT (10X,'VOLUME:                 MATRIX =',E13.5/10X,'        KUG 1640
     1                 SHELL =',E13.5/10X,'GRAPHITE PER BALL           GKUG 1650
     2R =',E13.5/10X,'BALLS PER KG-HM                =',E13.5/10X,'ASSUMKUG 1660
     3ED FIMA                   =',E13.5/)                              KUG 1670
CFZJ051                                                       11.05.07  KUG 1680
 2004 FORMAT (10X,'DUMMY BALLS'/)                                       KUG 1690
 2005 FORMAT (1H /1H /1H ,'IN INPUT VMOD OR BK IS MISSING')             KUG 1700
 2006 FORMAT (10X,'FRACT.OF DUMMY BALLS (NDB/NTOT)=',E13.5/)            KUG 1710
CFZJ051                                                       11.05.07  KUG 1720
 2007 FORMAT (10X,'RADIUS OF BALL (CM) =',E13.5)                        KUG 1730
 2009 FORMAT (10X,'ONE BALL CONTAINS ',E12.5,'  GR. HM AND ',E12.5,'  GRKUG 1740
     1. FISSILE MATERIAL'/)                                             KUG 1750
 3000 FORMAT (10X,'COSTS PER BALL      ',A4,'/BALL  =',E13.5/10X,'   COAKUG 1760
     1TED PARTICLES            =',E13.5/10X,'   FABRICATION OF FUEL-ELEMKUG 1770
     2.   =',E13.5/10X,'   FABRICATION OF DUMMY BALLS  =',E13.5///10X,  KUG 1780
     3 'COSTS OF FABRICATION ',A4,'/KG-HM=',E13.5/10X,'COSTS OF REPROCESKUG 1790
     4SING',A4,'/KG-HM=',E13.5)                                         KUG 1800
      END                                                               KUG 1810
      SUBROUTINE STAB(NFOLG,*)                                          STA   10
C                                                                       STA   20
      COMMON /VNISA/ V(10),NIS(10),A(10,10)                             STA   30
C                                                                       STA   40
      COMMON /CONST/ ANUKL(10),NGAM(10),GEW(10),AVO,PI                  STA   50
C                                                                       STA   60
      COMMON /STADAT/ R(7),QROSM,ROMTX,ROSTR,BETA,GKAN,ROHR             STA   70
C                                                                       STA   80
      COMMON /GRES/ ROBR,FAKMOL,VCP,VMOD,ROSM,VKUGEL,VSTAB,GSMCP,GSPCP, STA   90
     1 GSM,GSP,ASM,BK,RKUGEL,DREF,DCORE,HCORE,FF1,FF2,R1,R2,QR0,RSH     STA  100
C                                                                       STA  110
      COMMON /AAAA/ AAA(100),RADIU                                      STA  120
C                                                                       STA  130
CFZJ055                                                       25.09.07  STA  140
C                                                                       STA  150
      CHARACTER*4 ANUKL                                                 STA  160
C                                                                       STA  170
C                                                                       STA  180
      VOL(X1,X2) = PI * (X2**2-X1**2)                                   STA  190
      ITZAEL = 0                                                        STA  200
      ITFUEL = 0                                                        STA  210
      QR0 = 0.                                                          STA  220
      RSH = 0.                                                          STA  230
   22 CONTINUE                                                          STA  240
      DO 98 I=3,10                                                      STA  250
        V(I) = 0.                                                       STA  260
        DO 98 J=1,10                                                    STA  270
          A(J,I) = 0.                                                   STA  280
   98 CONTINUE                                                          STA  290
      IF(ITFUEL .NE. 0) GOTO 99                                         STA  300
      WRITE (6,2000)                                                    STA  310
      IF(NFOLG .EQ. 0) GOTO 99                                          STA  320
      DO 1 I=1,5                                                        STA  330
        R(I) = 0.                                                       STA  340
    1 CONTINUE                                                          STA  350
C                                                                       STA  360
CARD D 14                                                               STA  370
C                                                                       STA  380
      READ (5,1000) (R(I),I=1,6)                                        STA  390
C                                                                       STA  400
CARD D 15                                                               STA  410
C                                                                       STA  420
      READ (5,1000) FF1,VMOD,BETA,GKAN,FFUEL,ACTIV                      STA  430
C                                                                       STA  440
CARD D 16                                                               STA  450
C                                                                       STA  460
      READ (5,1000) QROSM,ROMTX,ROSTR,ROHR                              STA  470
C                                                                       STA  480
      IF(FF1 .GE. 0.) GOTO 20                                           STA  490
      ITFUEL = ITFUEL + 1                                               STA  500
      ITZAEL = ITFUEL                                                   STA  510
      FF1 = ABS(FF1)                                                    STA  520
      VMOD1 = VMOD                                                      STA  530
      VMOD = 0.                                                         STA  540
   20 CONTINUE                                                          STA  550
      BETA = 1. / (1.-BETA)                                             STA  560
      IF(FFUEL .EQ. 0.) GOTO 99                                         STA  570
      R(2) = R(1) + R(2)                                                STA  580
      R(3) = R(2) + R(3)                                                STA  590
      R(4) = SQRT(R(3)**2+FFUEL/PI)                                     STA  600
      R(5) = R(4) + R(5)                                                STA  610
      R(6) = R(5) + R(6)                                                STA  620
   99 CONTINUE                                                          STA  630
      IF(R(4) .EQ. 0. .OR. R(6) .EQ. 0.) GOTO 8000                      STA  640
      FFUEL = PI * (R(4)**2-R(3)**2)                                    STA  650
      ROSM = QROSM                                                      STA  660
      ALFA = FAKMOL * ROBR * V(1) / VCP                                 STA  670
      IF(ROSM .EQ. 0.) ROSM = ALFA * FF1                                STA  680
      IF(FF1 .EQ. 0.) FF1 = ROSM / ALFA                                 STA  690
      ALFA = AVO / GEW(1)                                               STA  700
      X1 = 0.                                                           STA  710
      DO 2 I=1,6                                                        STA  720
        X2 = R(I)                                                       STA  730
        IF(X2 .EQ. 0.) GOTO 13                                          STA  740
        V(I+2) = VOL(X1,X2)                                             STA  750
        IF(I .EQ. 2 .OR. I .EQ. 6) GOTO 13                              STA  760
        A(1,I+2) = ALFA * ROHR                                          STA  770
        IF(I .EQ. 4) A(1,I+2) = ALFA * ROMTX * (1.-FF1)                 STA  780
   13   X1 = X2                                                         STA  790
    2 CONTINUE                                                          STA  800
      IF(ITFUEL .GT. 0) GOTO 23                                         STA  810
      IF(NFOLG .NE. 0) WRITE (6,2001) ROMTX,ROSTR,ROHR                  STA  820
      IF(R(1) .EQ. 0.) GOTO 3                                           STA  830
      WRITE (6,2002)                                                    STA  840
      WRITE (6,2020) R(1)                                               STA  850
      IF(R(2) .EQ. 0.) GOTO 6                                           STA  860
      WRITE (6,2021) R(2)                                               STA  870
      WRITE (6,2022) R(3)                                               STA  880
      GOTO 6                                                            STA  890
    3 IF(R(2) .EQ. 0.) GOTO 4                                           STA  900
      WRITE (6,2003)                                                    STA  910
      WRITE (6,2021) R(2)                                               STA  920
      WRITE (6,2022) R(3)                                               STA  930
      GOTO 6                                                            STA  940
    4 IF(R(3) .EQ. 0.) GOTO 5                                           STA  950
      WRITE (6,2004)                                                    STA  960
      WRITE (6,2020) R(3)                                               STA  970
      GOTO 6                                                            STA  980
    5 WRITE (6,2005)                                                    STA  990
    6 WRITE (6,2023) R(4),R(5),R(6),FFUEL                               STA 1000
   23 VSTR = V(3) + V(5) + V(7)                                         STA 1010
      VSTAB = V(3) + V(4) + V(5) + V(6) + V(7)                          STA 1020
      A(1,6) = A(1,6) + (A(1,1)*V(1)+A(1,2)*V(2)) * FF1 / VCP           STA 1030
      FAKTOR = V(1) * FF1 / VCP                                         STA 1040
      DO 7 I=2,8                                                        STA 1050
        IF(NIS(I) .EQ. 0) GOTO 7                                        STA 1060
        A(I,6) = A(I,1) * FAKTOR                                        STA 1070
    7 CONTINUE                                                          STA 1080
      IF(NIS(9) .NE. 0) A(9,6) = A(9,1) * FAKTOR                        STA 1090
      IF(NIS(10) .NE. 0) A(10,6) = A(10,2) * V(2) * FF1 / VCP           STA 1100
      SUM = 0.                                                          STA 1110
      DO 9 I=2,8                                                        STA 1120
        SUM = SUM + A(I,6)                                              STA 1130
    9 CONTINUE                                                          STA 1140
      SUM = V(6) * SUM                                                  STA 1150
      SUMC = 0.                                                         STA 1160
      DO 10 J=3,7                                                       STA 1170
        SUMC = SUMC + A(1,J) * V(J)                                     STA 1180
   10 CONTINUE                                                          STA 1190
      A(1,9) = A(1,7) * ROSTR / ROHR                                    STA 1200
      IF(VMOD .EQ. 0.) GOTO 11                                          STA 1210
      V(9) = (VMOD*SUM-SUMC) * BETA / A(1,9)                            STA 1220
      VZELL = V(8) + VSTAB + V(9)                                       STA 1230
      GKAN = 1.E+04 / VZELL                                             STA 1240
      GOTO 12                                                           STA 1250
   11 VZELL = 1.E+04 / GKAN                                             STA 1260
      V(9) = VZELL - VSTAB - V(8)                                       STA 1270
      VMOD = (SUMC+A(1,9)*V(9)/BETA) / SUM                              STA 1280
   12 CONTINUE                                                          STA 1290
      IF(ITFUEL .LE. 0) GOTO 30                                         STA 1300
      VM = ABS((VMOD-VMOD1)/VMOD1)                                      STA 1310
      IF(VM .GT. 0.0001) GOTO 24                                        STA 1320
      ITFUEL = -1                                                       STA 1330
      VMOD = 0.                                                         STA 1340
      GOTO 22                                                           STA 1350
   24 FF = FF1                                                          STA 1360
      IF(ITFUEL .EQ. 1) GOTO 25                                         STA 1370
      VVERH = (VMOD1-VMOD) / (VMOD3-VMOD)                               STA 1380
      FF1 = FF + (FF3-FF) * VVERH                                       STA 1390
   25 FF3 = FF                                                          STA 1400
      VMOD3 = VMOD                                                      STA 1410
      VMOD = 0.                                                         STA 1420
      IF(ITFUEL .EQ. 1) FF1 = FF1 * 0.9                                 STA 1430
      ITFUEL = ITFUEL + 1                                               STA 1440
      ITZAEL = ITFUEL                                                   STA 1450
      GOTO 22                                                           STA 1460
   30 A(1,9) = A(1,9) / BETA                                            STA 1470
      V(10) = VZELL                                                     STA 1480
      RZELL = SQRT(VZELL/PI)                                            STA 1490
      R(7) = RZELL                                                      STA 1500
      WRITE (6,2006) ROSM,FF1,VMOD,RZELL,GKAN                           STA 1510
      AAA(73) = FF1                                                     STA 1520
      IF(ITZAEL .GT. 0) WRITE (6,3000) ITZAEL                           STA 1530
      WRITE (6,2007)                                                    STA 1540
      DO 8 I=1,10                                                       STA 1550
        IF(NIS(I) .EQ. 0) GOTO 8                                        STA 1560
        WRITE (6,2008) NGAM(I),ANUKL(I),(A(I,J),J=6,7),A(I,9)           STA 1570
    8 CONTINUE                                                          STA 1580
      WRITE (6,2009) V(6),VSTR                                          STA 1590
      GSM = GSMCP                                                       STA 1600
      GSP = GSPCP                                                       STA 1610
      RETURN 1                                                          STA 1620
 8000 WRITE (6,2800)                                                    STA 1630
      STOP                                                              STA 1640
C                                                                       STA 1650
 1000 FORMAT (6E12.5)                                                   STA 1660
 2000 FORMAT (1H /1H ,10X,'PRISMATIC FUEL ELEMENTS, TYPE DRAGON'///)    STA 1670
 2001 FORMAT (10X,'C-DENSITIES: MATRIX =',E12.5,4X,'MODERATOR =',E12.5, STA 1680
     1 4X,'ENVELOPE PIPE =',E12.5//)                                    STA 1690
 2002 FORMAT (10X,'HOLLOW ROD WITH CORE'//)                             STA 1700
 2003 FORMAT (10X,'HOLLOW ROD'//)                                       STA 1710
 2004 FORMAT (10X,'ANNULAR ROD'//)                                      STA 1720
 2005 FORMAT (10X,'SOLID ROD'//)                                        STA 1730
 2006 FORMAT (10X,'DENSITY HM IN FUEL-ZONE.(G/CC) =',E13.5/10X,'FFACTOR STA 1740
     1CP IN FUEL-ZONE        =',E13.5/10X,'MODERATION RATIO  (C-AT/HM-ATSTA 1750
     2) =',E13.5/10X,'CELL RADIUS               (CM) =',E13.5/10X,'NUMBESTA 1760
     3R OF CHANNELS/M**2        =',E13.5)                               STA 1770
 2007 FORMAT (1H /1H ,8X,'GAMNO  ISOTOP   FUEL-ZONE     ENV. PIPE     MOSTA 1780
     1DERATOR'//)                                                       STA 1790
 2008 FORMAT (I14,2X,A4,3E14.5)                                         STA 1800
 2009 FORMAT (1H /1H ,11X,'VOLUME  ',2E14.5//)                          STA 1810
 2020 FORMAT (10X,'RADIUS OF CORE            (CM) =',E13.5)             STA 1820
 2021 FORMAT (10X,'RADIUS OF INNER CHANNEL   (CM) =',E13.5)             STA 1830
 2022 FORMAT (10X,'RADIUS OF INNER ENV.PIPE  (CM) =',E13.5)             STA 1840
 2023 FORMAT (10X,'RADIUS OF FUEL ZONE       (CM) =',E13.5/10X,'RADIUS OSTA 1850
     1F EXTERN.ENV.PIPE (CM) =',E13.5/10X,'RADIUS OF EXTERN. CHANNEL (CMSTA 1860
     2) =',E13.5/10X,'FUEL - AREA (CM**2)            =',E13.5)          STA 1870
 2800 FORMAT (1H /1H /1H ,10X,'INPUT-ERROR IN SUBROUTINE "STAB"'/10X,   STA 1880
     1 'R(4) OR R(6) IS 0.')                                            STA 1890
 3000 FORMAT (/' (',I3,' ITERATION STEPS NEEDED TO CALCULATE THE FILLINGSTA 1900
     1 FACTOR FOR ABOVE NAMED MODERATION RATIO)')                       STA 1910
      END                                                               STA 1920
      SUBROUTINE GAM1(NTYP)                                             GAM   10
C                                                                       GAM   20
      COMMON /VNISA/ V(10),NIS(10),A(10,10)                             GAM   30
C                                                                       GAM   40
      COMMON /CONST/ ANUKL(10),NGAM(10),GEW(10),AVO,PI                  GAM   50
C                                                                       GAM   60
      COMMON /GRES/ ROBR,FAKMOL,VCP,VMOD,ROSM,VKUGEL,VSTAB,GSMCP,GSPCP, GAM   70
     1 GSM,GSP,ASM,BK,RKUGEL,DREF,DCORE,HCORE,FF1,FF2,R1,R2,QR0,RSH     GAM   80
C                                                                       GAM   90
      COMMON /STADAT/ R(7),QROSM,ROMTX,ROSTR,BETA,GKAN,ROHR             GAM  100
C                                                                       GAM  110
      COMMON /GAM/ CEG(33),AL1(33),IDSATZ(12),NOID,NTBD,NRES,IDRES,WTP, GAM  120
     1 WSTU,WAG,NOAG                                                    GAM  130
C                                                                       GAM  140
      COMMON /GAMRES/ SPTH,SPU8,SPC,SPO                                 GAM  150
C                                                                       GAM  160
      COMMON /AAAA/ AAA(100),RADIU                                      GAM  170
C                                                                       GAM  180
      COMMON /RVERH/ ROVERH,CBK,ROBK                                   GAM  190
C                                                                       GAM  200
      CHARACTER*4 ANUKL                                                 GAM  210
C                                                                       GAM  220
      DIMENSION AGAM(10)                                                GAM  230
C                                                                       GAM  240
C                                                                       GAM  250
CFZJ051                                                       11.05.07  GAM  260
      IF(BK .LT. 0.99999) WRITE (6,2000)                                GAM  270
      GOTO(1,2),NTYP                                                    GAM  280
    1 CONTINUE                                                          GAM  290
      A(1,3) = A(1,3) * (1.-FF1)                                        GAM  300
      DO 10 I=1,10                                                      GAM  310
        AGAM(I) = 0.                                                    GAM  320
        IF(NIS(I) .EQ. 0) GOTO 10                                       GAM  330
        DO 11 J=1,2                                                     GAM  340
          AGAM(I) = AGAM(I) + A(I,J) * V(J)                             GAM  350
   11   CONTINUE                                                        GAM  360
        AGAM(I) = AGAM(I) * FF1 * V(3) / VCP                            GAM  370
        DO 12 J=3,4                                                     GAM  380
          AGAM(I) = AGAM(I) + A(I,J) * V(J)                             GAM  390
   12   CONTINUE                                                        GAM  400
        AGAM(I) = AGAM(I) * FF2 / VKUGEL                                GAM  410
        AGAM(I) = AGAM(I) * (1.-BK)                                     GAM  420
        IF(I .EQ. 1) AGAM(I) = AGAM(I) + A(1,4) * FF2 * BK * ROVERH     GAM  430
   10 CONTINUE                                                          GAM  440
      A(1,3) = A(1,3) / (1.-FF1)                                        GAM  450
      GOTO 100                                                          GAM  460
    2 CONTINUE                                                          GAM  470
      VZELL = V(10)                                                     GAM  480
      DO 20 I=1,10                                                      GAM  490
        AGAM(I) = 0.                                                    GAM  500
        IF(NIS(I) .EQ. 0) GOTO 20                                       GAM  510
        DO 21 J=3,9                                                     GAM  520
          AGAM(I) = AGAM(I) + A(I,J) * V(J) / VZELL                     GAM  530
   21   CONTINUE                                                        GAM  540
   20 CONTINUE                                                          GAM  550
  100 CONTINUE                                                          GAM  560
CFZJ051                                                       11.05.07  GAM  570
      IF(BK .LT. 0.99999) WRITE (6,2003)                                GAM  580
      DO 200 I=1,10                                                     GAM  590
        IF(NIS(I) .EQ. 0) GOTO 200                                      GAM  600
CFZJ051                                                       11.05.07  GAM  610
        IF(BK .LT. 0.99999) WRITE (6,2004) NGAM(I),ANUKL(I),AGAM(I)     GAM  620
C                                                                       GAM  630
C     DIE GAM-NUMMERN IN REAL-DARSTELLUNG UND DIE GAM-DICHTEN           GAM  640
C                                                                       GAM  650
        AAA(I) = FLOAT(NGAM(I))                                         GAM  660
        AAA(I+10) = AGAM(I)                                             GAM  670
  200 CONTINUE                                                          GAM  680
      RETURN                                                            GAM  690
C                                                                       GAM  700
 2000 FORMAT (// 10X,'G A M - 1  -  D A T A'/)                          GAM  710
 2003 FORMAT (10X,'GAMNO  ISOTOP   AT/CM*BARN')                         GAM  720
 2004 FORMAT (I13,4X,A4,E15.5)                                          GAM  730
      END                                                               GAM  740
      SUBROUTINE THERMD(NTYP)                                           THE   10
C                                                                       THE   20
      COMMON /VNISA/ V(10),NIS(10),A(10,10)                             THE   30
C                                                                       THE   40
      COMMON /CONST/ ANUKL(10),NGAM(10),GEW(10),AVO,PI                  THE   50
C                                                                       THE   60
      COMMON /CPDAT/ ANR,INDBS,NCT,NSIC1,NSIC2,KQ,PU(4),DCT(5),ROCT(5), THE   70
     1 RK,ROBR1,ROBR2,BETA                                              THE   80
C                                                                       THE   90
      COMMON /STADAT/ R(7),DUMY1,ROMTX,DUMY3,DUMY4,DUMY5,DUMY6          THE  100
C                                                                       THE  110
      COMMON /GRES/ ROBR,FAKMOL,VCP,VMOD,ROSM,VKUGEL,VSTAB,GSMCP,GSPCP, THE  120
     1 GSM,GSP,ASM,BK,RKUGEL,DREF,DCORE,HCORE,FF1,FF2,R1,R2,QR0,RSH     THE  130
C                                                                       THE  140
      COMMON /THER/ MTBL(20),LIM(30),ID,NX,NHET,NTHGR,MX,IDC,IDO,TTEMP, THE  150
     1 MMX                                                              THE  160
C                                                                       THE  170
      COMMON /AAAA/ AAA(100),RADIU                                      THE  180
C                                                                       THE  190
      COMMON /RVERH/ ROVERH,CBK,ROBK                                    THE  200
C                                                                       THE  210
      CHARACTER*4 ANUKL                                                 THE  220
C                                                                       THE  230
      DIMENSION NISG(10),NIST(10),VVV(5),ATHER(10,5),D(5)               THE  240
C                                                                       THE  250
C                                                                       THE  260
      RAD(VV) = EXP(ALOG(VV*0.75/PI)/3.)                                THE  270
CFZJ051                                                       11.05.07  THE  280
      IF(BK .LT. 0.99999) WRITE (6,2000)                                THE  290
      IF(NHET .NE. 0) WRITE (6,2002)                                    THE  300
      NN = 0                                                            THE  310
      MSHK = 0                                                          THE  320
      N90 = 90                                                          THE  330
      IBZ1 = 0                                                          THE  340
      MMZ = 0                                                           THE  350
      FAKTOR = 1.                                                       THE  360
      DO 5 I=1,10                                                       THE  370
        IF(NIS(I) .EQ. 0)GOTO 5                                         THE  380
        NN = NN + 1                                                     THE  390
        NISG(NN) = NGAM(I)                                              THE  400
        NIST(NN) = NGAM(I)                                              THE  410
        IF(I .EQ. 1) NIST(NN) = IDC                                     THE  420
        IF(I .EQ. 9) NIST(NN) = IDO                                     THE  430
    5 CONTINUE                                                          THE  440
      NTHER = 1                                                         THE  450
 8000 CONTINUE                                                          THE  460
      DO 6 I=1,10                                                       THE  470
        DO 6 J=1,5                                                      THE  480
          ATHER(I,J) = 0.                                               THE  490
    6 CONTINUE                                                          THE  500
      IF(NHET .NE. 0 .AND. NTHER .EQ. 1) WRITE (6,2003)                 THE  510
      IF(NHET .NE. 0 .AND. NTHER .EQ. 2) WRITE (6,2004)                 THE  520
C                                                                       THE  530
C     ATOMKONZENTRATIONEN                                               THE  540
C                                                                       THE  550
      IF(NHET .GT. 0 .AND. NTHER .EQ. 1) GOTO 114                       THE  560
C                                                                       THE  570
C     EHR,2DHR                                                          THE  580
C                                                                       THE  590
      GOTO(1,2),NTYP                                                    THE  600
    1 CONTINUE                                                          THE  610
      FAKTOR = FF1 * V(1) / VCP                                         THE  620
      MOLD = 0                                                          THE  630
      IF(NHET .GT. 0) MOLD = 1                                          THE  640
      MNEW = MOLD                                                       THE  650
      IBZ = 1                                                           THE  660
      GOTO 100                                                          THE  670
    2 MOLD = 0                                                          THE  680
      MNEW = 0                                                          THE  690
      IBZ = 1                                                           THE  700
      IF(R(3) .NE. 0. .OR. R(1) .NE. 0.) IBZ = 2                        THE  710
      IF(NHET .EQ. 0) GOTO 100                                          THE  720
      MOLD = 1                                                          THE  730
      MNEW = IBZ                                                        THE  740
  100 IBZ1 = IBZ + 1                                                    THE  750
      DO 10 I=2,10                                                      THE  760
        IF(I .EQ. 9) GOTO 10                                            THE  770
        IF(NIS(I) .EQ. 0) GOTO 10                                       THE  780
        GOTO(101,102),NTYP                                              THE  790
  101   ATHER(I,1) = A(I,1) * FAKTOR                                    THE  800
        IF(I .EQ. 10) ATHER(I,1) = A(I,2) * FF1 * V(2) / VCP            THE  810
        GOTO 10                                                         THE  820
  102   ATHER(I,IBZ) = A(I,6)                                           THE  830
   10 CONTINUE                                                          THE  840
      IF(NIS(9) .EQ. 0) GOTO 11                                         THE  850
      IF(IDC .LT. IDO ) GOTO 11                                         THE  860
      NIDO = 0                                                          THE  870
   12 CONTINUE                                                          THE  880
      GOTO(104,105),NTYP                                                THE  890
  104 ATHER(9,1) = A(9,1) * FAKTOR                                      THE  900
      GOTO 13                                                           THE  910
  105 ATHER(9,IBZ) = A(9,6)                                             THE  920
   13 IF(NIDO .EQ. 1) GOTO 14                                           THE  930
   11 CONTINUE                                                          THE  940
      GOTO(107,108),NTYP                                                THE  950
  107 ATHER(1,1) = A(1,1) * FAKTOR + A(1,2) * FF1 * V(2) / VCP + A(1,3) THE  960
     1 * (1.-FF1)                                                       THE  970
      ATHER(1,2) = A(1,4)                                               THE  980
      GOTO 9                                                            THE  990
  108 IF(IBZ .EQ. 2) ATHER(1,1) = (A(1,3)*V(3)+A(1,5)*V(5)) / (V(3)+V(4)THE 1000
     1 +V(5))                                                           THE 1010
      ATHER(1,IBZ) = A(1,6)                                             THE 1020
      ATHER(1,IBZ1) = (A(1,7)*V(7)+A(1,9)*V(9)) / (V(7)+V(8)+V(9))      THE 1030
    9 IF(NIS(9) .EQ. 0) GOTO 14                                         THE 1040
      NIDO = 1                                                          THE 1050
      GOTO 12                                                           THE 1060
   14 CONTINUE                                                          THE 1070
C                                                                       THE 1080
C     GEOMETRIE EHR,2DHR                                                THE 1090
C                                                                       THE 1100
      GOTO(110,111),NTYP                                                THE 1110
  110 D(1) = RSH - QR0                                                  THE 1120
C                                                                       THE 1130
C     BLINKUGELN ZUR SCHALE ZUSCHLAGEN                                  THE 1140
C                                                                       THE 1150
      VCELL = VKUGEL / (1.-BK)                                          THE 1160
      IF(CBK .EQ. 1.) GO TO 120                                         THE 1170
C                                                                       THE 1180
C     EVTL. SCHALENKUGEL                                                THE 1190
C                                                                       THE 1200
      IF(QR0 .GT. 0.) MSHK = 1                                          THE 1210
      AAA(N90+1) = 0                                                    THE 1220
      AAA(N90+2) = QR0                                                  THE 1230
      AAA(N90+3) = RSH                                                  THE 1240
      AAA(N90+4) = RAD(VCELL)                                           THE 1250
      AAA(N90+5) = BK                                                   THE 1260
      MMX = IBZ1 + MSHK                                                 THE 1270
      DO 32 J=1,MMX                                                     THE 1280
        JM = MMX + 1 - J                                                THE 1290
        D(JM) = AAA(N90+5-J) - AAA(N90+4-J)                             THE 1300
        VVV(JM) = AAA(N90+5-J)**3. - AAA(N90+4-J)**3.                   THE 1310
        JK = JM - MSHK                                                  THE 1320
        IF(JK .EQ. 0) JK = MMX                                          THE 1330
        DO 31 I=1,10                                                    THE 1340
          ATHER(I,JM) = ATHER(I,JK)                                     THE 1350
   31   CONTINUE                                                        THE 1360
   32 CONTINUE                                                          THE 1370
      GOTO 30                                                           THE 1380
  120 CONTINUE                                                          THE 1390
C                                                                       THE 1400
C     BLINDKUGELN MIT ANDERER GRAPHITDICHTE ALS IN SCHALE               THE 1410
C                                                                       THE 1420
      MMX = IBZ1 + 1                                                    THE 1430
      MMZ = MMX                                                         THE 1440
      AAA(N90+1) = 0.                                                   THE 1450
      AAA(N90+2) = RSH                                                  THE 1460
      AAA(N90+3) = R2                                                   THE 1470
      AAA(N90+4) = RAD(VCELL)                                           THE 1480
      AAA(N90+5) = BK                                                   THE 1490
      DO 51 J=1,MMX                                                     THE 1500
        JM = MMX + 1 - J                                                THE 1510
        D(JM) = AAA(N90+5-J) - AAA(N90+4-J)                             THE 1520
        VVV(JM) = AAA(N90+5-J)**3. - AAA(N90+4-J)**3.                   THE 1530
   51 CONTINUE                                                          THE 1540
      DO 52 I=1,10                                                      THE 1550
        ATHER(I,MMX) = ATHER(I,IBZ1) * ROVERH                           THE 1560
   52 CONTINUE                                                          THE 1570
      GOTO 30                                                           THE 1580
  111 CONTINUE                                                          THE 1590
      AAA(N90+2) = R(3)                                                 THE 1600
      AAA(N90+3) = R(4)                                                 THE 1610
      AAA(N90+4) = R(7)                                                 THE 1620
      GOTO(112,113),IBZ                                                 THE 1630
  112 D(1) = R(4)                                                       THE 1640
      D(2) = R(7) - R(4)                                                THE 1650
      VVV(1) = R(4)**2                                                  THE 1660
      VVV(2) = R(7)**2 - VVV(1)                                         THE 1670
      GOTO 30                                                           THE 1680
  113 D(1) = R(3)                                                       THE 1690
      D(2) = R(4) - R(3)                                                THE 1700
      D(3) = R(7) - R(4)                                                THE 1710
      VVV(1) = R(3)**2                                                  THE 1720
      VVV(2) = R(4)**2 - VVV(1)                                         THE 1730
      VVV(3) = R(7)**2 - VVV(2) - VVV(1)                                THE 1740
      GOTO 30                                                           THE 1750
  114 CONTINUE                                                          THE 1760
C                                                                       THE 1770
C     1DHR                                                              THE 1780
C                                                                       THE 1790
      VCELL = VCP / FF1                                                 THE 1800
      DO 16 I=2,10                                                      THE 1810
        IF(I .EQ. 9) GOTO 16                                            THE 1820
        IF(NIS(I) .EQ. 0) GOTO 16                                       THE 1830
        ATHER(I,1) = A(I,1)                                             THE 1840
        IF(I .EQ. 10) ATHER(I,2) = A(I,2) * V(2) / (VCELL-V(1))         THE 1850
   16 CONTINUE                                                          THE 1860
      GOTO 17                                                           THE 1870
   19 CONTINUE                                                          THE 1880
      ATHER(9,1) = A(9,1)                                               THE 1890
      IF(NIDO .EQ. 1) GOTO 18                                           THE 1900
   17 CONTINUE                                                          THE 1910
      ATHER(1,1) = A(1,1)                                               THE 1920
      GOTO(115,116),NTYP                                                THE 1930
  115 ATHER(1,2) = (A(1,2)*V(2)+A(1,3)*(VCELL-VCP)) / (VCELL-V(1))      THE 1940
      GOTO 117                                                          THE 1950
  116 ATHER(1,2) = (A(1,2)*V(2)+(AVO*ROMTX/GEW(1))*(VCELL-VCP)) / (VCELLTHE 1960
     1 -V(1))                                                           THE 1970
  117 CONTINUE                                                          THE 1980
      IF(NIS(9) .EQ. 0) GOTO 18                                         THE 1990
      NIDO = 1                                                          THE 2000
      GOTO 19                                                           THE 2010
   18 CONTINUE                                                          THE 2020
C                                                                       THE 2030
C     GEOMETRIE 1DHR                                                    THE 2040
C                                                                       THE 2050
      D(1) = RK                                                         THE 2060
      D(2) = RAD(VCELL) - D(1)                                          THE 2070
      VVV(1) = RK**3                                                    THE 2080
      VVV(2) = RAD(VCELL)                                               THE 2090
      VVV(2) = VVV(2)**3 - VVV(1)                                       THE 2100
      GOTO 30                                                           THE 2110
 8001 NTHER = 2                                                         THE 2120
      GOTO 8000                                                         THE 2130
C                                                                       THE 2140
C     DATEN AUSSCHREIBEN                                                THE 2150
C                                                                       THE 2160
CFZJ051                                                       11.05.07  THE 2170
   30 IF(BK .LT. 0.99999) WRITE (6,2005)                                THE 2180
      MMX = IBZ1 + MSHK                                                 THE 2190
      IF(CBK .EQ. 1.) MMX = MMZ                                         THE 2200
      DO 40 J=1,MMX                                                     THE 2210
        K = 0                                                           THE 2220
        DO 40 I=1,10                                                    THE 2230
          CONCI = 0.0                                                   THE 2240
          DO 200 JJ=1,MMX                                               THE 2250
            CONCI = CONCI + ATHER(I,JJ) * VVV(JJ)                       THE 2260
  200     CONTINUE                                                      THE 2270
          IF(CONCI .NE. 0.0) CONCI = ATHER(I,J) * VVV(J) / CONCI        THE 2280
          IF(ATHER(I,J) .EQ. 0.) GOTO 40                                THE 2290
          IDNR = NGAM(I)                                                THE 2300
          K = K + 1                                                     THE 2310
          IF(K .EQ. 1) GOTO 41                                          THE 2320
CFZJ051                                                       11.05.07  THE 2330
          IF(BK .LT. 0.99999) WRITE (6,2006) ANUKL(I),IDNR,ATHER(I,J),  THE 2340
     1     CONCI                                                        THE 2350
          GOTO 39                                                       THE 2360
CFZJ051                                                       11.05.07  THE 2370
   41     IF(BK .LT. 0.99999) WRITE (6,2007) J,D(J),ANUKL(I),IDNR,      THE 2380
     1     ATHER(I,J),CONCI                                             THE 2390
   39     AAA(10+J*10+I) = CONCI                                        THE 2400
   40 CONTINUE                                                          THE 2410
      IF(NHET .NE. 0 .AND. NTHER .EQ. 1) GOTO 8001                      THE 2420
      RETURN                                                            THE 2430
C                                                                       THE 2440
 2000 FORMAT (//9X,'T H E R M O S  -  D A T A'/)                        THE 2450
 2002 FORMAT (10X,'DOUBLE-HETEROGENEOUS'//)                             THE 2460
 2003 FORMAT (10X,'1DHR - DATA'/10X,'==========='//)                    THE 2470
 2004 FORMAT (1H /1H /1H , 9X,'2DHR - DATA'/10X,'==========='//)        THE 2480
 2005 FORMAT (9X,'ZONE  THICKNESS(CM) ISOTOP   IDNO   ATOMS/CM*BARN   N(THE 2490
     1J)*V(J)/SUM(N(J)*V(J)) ')                                         THE 2500
 2006 FORMAT (30X,A4,I8,4E14.5)                                         THE 2510
 2007 FORMAT (10X,I2,E14.5,4X,A4,I8,2E14.5)                             THE 2520
      END                                                               THE 2530
      SUBROUTINE DANC3(NDANC)                                           DAN   10
C                                                                       DAN   20
      COMMON /VNISA/ V(10),NIS(10),A(10,10)                             DAN   30
C                                                                       DAN   40
      COMMON /STADAT/ R(7),QROSM,ROMTX,ROSTR,BETA,GKAN,ROHR             DAN   50
C                                                                       DAN   60
      COMMON /GAMRES/ SPTH,SPU8,SPC,SPO                                 DAN   70
C                                                                       DAN   80
C                                                                       DAN   90
      VCELL = V(10)                                                     DAN  100
      FAKTOR = 1.                                                       DAN  110
      IF(NDANC .EQ. 20) FAKTOR = 4. / SQRT(3.)                          DAN  120
      B = SQRT(VCELL*FAKTOR)                                            DAN  130
      QA = R(4)                                                         DAN  140
      DHUE = R(6) - R(4)                                                DAN  150
      SMOD = SPC * A(1,9)                                               DAN  160
      SHUE = SPC * A(1,7) * V(7) / (V(7)+V(8))                          DAN  170
      WRITE (6,2000)                                                    DAN  180
      WRITE (6,2001) NDANC,QA,B,DHUE,SMOD,SHUE                          DAN  190
      RETURN                                                            DAN  200
C                                                                       DAN  210
 2000 FORMAT (1H1,10X,'D A N C O F F - 3  -  DATA'///)                  DAN  220
 2001 FORMAT (10X,'LATTICE TYPE =',I5//10X,'A     =',E12.5/10X,'B     ='DAN  230
     1 ,E12.5/10X,'DHUE  =',E12.5/10X,'SMOD  =',E12.5/10X,'SHUE  =',    DAN  240
     2 E12.5)                                                           DAN  250
      END                                                               DAN  260
      SUBROUTINE ZUT(NTYP,NRET,*)                                       ZUT   10
C                                                                       ZUT   20
      COMMON /VNISA/ V(10),NIS(10),A(10,10)                             ZUT   30
C                                                                       ZUT   40
      COMMON /CONST/ ANUKL(10),NGAM(10),GEW(10),AVO,PI                  ZUT   50
C                                                                       ZUT   60
      COMMON /CPDAT/ ANR,INDBS,NCT,NSIC1,NSIC2,K,PU(4),DCT(5),ROCT(5),RKZUT   70
     1 ,ROBR1,ROBR2,DUMY                                                ZUT   80
C                                                                       ZUT   90
      COMMON /GRES/ ROBR,FAKMOL,VCP,VMOD,ROSM,VKUGEL,VSTAB,GSMCP,GSPCP, ZUT  100
     1 GSM,GSP,ASM,BK,RKUGEL,DREF,DCORE,HCORE,FF1,FF2,R1,R2,QR0,RSH     ZUT  110
C                                                                       ZUT  120
      COMMON /STADAT/ R(7),QROSM,ROMTX,ROSTR,BETA,GKAN,ROHR             ZUT  130
C                                                                       ZUT  140
      COMMON /GAMRES/ SPTH,SPU8,SPC,SPO,SPPU2,SPU5                      ZUT  150
C                                                                       ZUT  160
      COMMON /AAAA/ AAA(100),RADIU                                      ZUT  170
C                                                                       ZUT  180
      COMMON /RVERH/ ROVERH,CBK,ROBK                                    ZUT  190
C                                                                       ZUT  200
      COMMON /COSTS/ KOSTDA,KOSTD2,FIMA,HK,AK,EK,CURCY,NC,NF,FC(6),FF(4),FRC(6)ZUT  210
     1 ,FRF(4),FABKOS,AUFKOS,DK,S(31),IU8TH                             ZUT  220
C                                                                       ZUT  220
      CHARACTER*4 ANUKL                                                 ZUT  230
C                                                                       ZUT  240
C                                                                       ZUT  250
      IU81 = 0                                                          ZUT  260
      IF(IU8TH .EQ. 0) IU81 = 1                                         ZUT  270
      SPSI = 3.                                                         ZUT  280
      H = 0.                                                            ZUT  290
      SI5 = 0.                                                          ZUT  300
      RCP = RK                                                          ZUT  310
      ALPH = 0.                                                         ZUT  320
      FF3 = 0.                                                          ZUT  330
      R4 = 0.                                                           ZUT  340
      R5 = 0.                                                           ZUT  350
      SI4 = 0.                                                          ZUT  360
      SIGM1 = 0.                                                        ZUT  370
      SIGM2 = 0.                                                        ZUT  380
      DIQU1 = 0.                                                        ZUT  390
      DIQU2 = 0.                                                        ZUT  400
      DO 100 I=1,NCT                                                    ZUT  410
        RCP = RCP + DCT(I)                                              ZUT  420
  100 CONTINUE                                                          ZUT  430
      IF(IU8TH .EQ. 0) IU8TH = 1                                        ZUT  440
      GOTO(201,202,203,204),IU8TH                                       ZUT  450
  201 CONTINUE                                                          ZUT  460
      J = 2                                                             ZUT  470
      SIGPZ = SPTH                                                      ZUT  480
      GOTO 205                                                          ZUT  490
  202 CONTINUE                                                          ZUT  500
      J = 4                                                             ZUT  510
      SIGPZ = SPU8                                                      ZUT  520
      GOTO 205                                                          ZUT  530
  203 CONTINUE                                                          ZUT  540
      J = 8                                                             ZUT  550
      SIGPZ = SPPU2                                                     ZUT  560
      GOTO 205                                                          ZUT  570
  204 CONTINUE                                                          ZUT  580
      J = 3                                                             ZUT  590
      SIGPZ = SPU5                                                      ZUT  600
  205 CONTINUE                                                          ZUT  610
      ABAR = RK                                                         ZUT  620
      DZERO = A(J,1)                                                    ZUT  630
      IF(DZERO .LE. 0.) GOTO 5                                          ZUT  640
      SIGM1 = SPC * A(1,1) / DZERO                                      ZUT  650
      SIGM2 = SPO * A(9,1) / DZERO                                      ZUT  660
      DIQU1 = A(1,1) / DZERO                                            ZUT  670
      DIQU2 = A(9,1) / DZERO                                            ZUT  680
    5 CONTINUE                                                          ZUT  690
      SI2 = A(1,2) * SPC + A(10,2) * SPSI                               ZUT  700
      GOTO(1,2),NTYP                                                    ZUT  710
    1 CONTINUE                                                          ZUT  720
      FF3 = FF1                                                         ZUT  730
      SI4 = A(1,4) * SPC                                                ZUT  740
      SI5 = SI4 * ROVERH                                                ZUT  750
      ALPH = A(1,3) / (A(1,2)+A(10,2))                                  ZUT  760
      R4 = R1                                                           ZUT  770
      R5 = R2                                                           ZUT  780
      H = BK                                                            ZUT  790
      GOTO 10                                                           ZUT  800
    2 CONTINUE                                                          ZUT  810
      SI4 = 0.                                                          ZUT  820
      R4 = R(4)                                                         ZUT  830
      R5 = 0.                                                           ZUT  840
      FF3 = FF1 * V(6) / (V(3)+V(4)+V(5)+V(6))                          ZUT  850
      AVS = (A(1,3)*V(3)+A(1,5)*V(5)+AVO*ROMTX*V(6)*(1.-FF1)/GEW(1)) /  ZUT  860
     1 (V(3)+V(4)+V(5)+V(6)*(1.-FF1))                                   ZUT  870
      ALPH = AVS / (A(1,2)+A(10,2))                                     ZUT  880
   10 CONTINUE                                                          ZUT  890
CFZJ051                                                       11.05.07  ZUT  900
      AAA(74) = SIGPZ                                                   ZUT  910
      AAA(75) = ABAR                                                    ZUT  920
      AAA(76) = DZERO                                                   ZUT  930
      AAA(77) = SIGM1                                                   ZUT  940
      AAA(78) = SIGM2                                                   ZUT  950
      AAA(79) = RK                                                      ZUT  960
      AAA(80) = RCP                                                     ZUT  970
      AAA(81) = R4                                                      ZUT  980
      AAA(82) = R5                                                      ZUT  990
      AAA(83) = FF3                                                     ZUT 1000
      AAA(84) = H                                                       ZUT 1010
      AAA(85) = SI2                                                     ZUT 1020
      AAA(86) = SI4                                                     ZUT 1030
      AAA(87) = SI5                                                     ZUT 1040
      AAA(88) = ALPH                                                    ZUT 1050
      AAA(89) = DIQU1                                                   ZUT 1060
      AAA(90) = DIQU2                                                   ZUT 1070
      IF(NRET .NE. 0) RETURN                                            ZUT 1080
      RETURN 1                                                          ZUT 1090
CFZJ051                                                       11.05.07  ZUT 1100
      END                                                               ZUT 1110