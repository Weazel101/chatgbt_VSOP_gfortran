      SUBROUTINE NACHW(ZEITN,N200,QNW,VL,QNWR,E,RWCS,TB,PGES,ZF,DEN,KMATNAC   10
     1 ,PBET)                                                           NAC   20
C                                                                       NAC   30
C     CALCULATION OF DECAY POWER ACCORDING TO STANDARD DIN 25485        NAC   40
C                                                                       NAC   50
C     PGES:  LEISTUNG;                                                  NAC   60
CFZJ053                                                       19.07.07  NAC   70
C     ZF(1) --> ZF(4): LEISTUNGSANTEILE VON U-233, U-235, PU-239, PU-241NAC   80
C     ZF(5) --> ZF(6): EINFANGRATE IN TH-232 BZW. U-238 ZU SPALTRATE IN NAC   90
C                      SPALTSTOFFEN                                     NAC  100
CFZJ053                                                       19.07.07  NAC  110
C     ZF(7): LEISTUNG DER SPALTSTOFFE ZU GESAMTLEISTUNG                 NAC  120
C     PS:    NACHZERFALLSLEISTUNG DER SPALTPRODUKTE                     NAC  130
C     PB:    NACHZERFALLSLEISTUNG VON U-239 + NP-239 + TH-233 + PA-233  NAC  140
C     PA:    NACHZERFALLSLEISTUNG DER RESTLICHEN AKTINIDEN              NAC  150
C     PCS:   NACHZERFALLSLEISTUNG DES CS-134                            NAC  160
C     PE:    NACHZERFALLSLEISTUNG DER RESTL. AKTIVIERTEN SPALTPRODUKTE  NAC  170
C     QNW:   NACHWAERMELEISTUNG DER COMPOSITIONS                        NAC  180
C     N197:  ANZAHL DER LEISTUNGSERZEUGENDEN COMPOSITIONS               NAC  190
C     TB:    "GROSS" TK (NACH DIN)                                      NAC  200
C     TA:    "KLEIN" TK (NACH DIN)                                      NAC  210
C                                                                       NAC  220
      COMMON /NW1/ ALPHA(24,4),ALAMDA(24,4),TA(50),P(50,4)              NAC  230
C                                                                       NAC  240
      COMMON /NW2/ RU(50),RTH(50)                                       NAC  250
C                                                                       NAC  260
      COMMON /NW3/ PHI(50),S(50)                                        NAC  270
C                                                                       NAC  280
CFZJ048 enlarged dimension                                    11.04.07  NAC  290
      COMMON /NW6/ MMT(10000)                                           NAC  300
C                                                                       NAC  310
      COMMON /BLOTIK/ N197                                              NAC  320
C                                                                       NAC  330
      COMMON /OPT/ KENN,IOPUT                                           NAC  340
C                                                                       NAC  350
CFZJ006 enlarged dimensions common QVAR                       28.11.03  NAC  360
      COMMON /QVAR/ DUM(1511),N61,DUMM(315),RMAX,RMIN                   NAC  370
C                                                                       NAC  380
      COMMON /KSUM/ ITI,ZEIT1,SIG,VORZ,BKV,RHOKS,RL,SM,BURN,DUMMI,A0    NAC  390
C                                                                       NAC  400
      COMMON /STA/ IST,SB,TEX,JNS                                       NAC  410
C                                                                       NAC  420
      COMMON /FLUXN/ D(361),IACT                                        NAC  430
C                                                                       NAC  440
      DIMENSION QNW(N200),QNWR(N200),VL(N200),TB(50,N200),PGES(50,N200),NAC  450
     1 ZF(7,50,N200),E(24,4,N200),RWCS(N200),PBET(N200),DEN(KMAT,N200), NAC  460
     2 DENSM(IACT)                                                      NAC  470
C                                                                       NAC  480
C                                                                       NAC  490
      IW = 0                                                            NAC  500
      LW = 0                                                            NAC  510
      KW = 0                                                            NAC  520
      PTOT = 0.                                                         NAC  530
      PS = 0.                                                           NAC  540
      SPS = 0.                                                          NAC  550
      PB = 0.                                                           NAC  560
      SPB = 0.                                                          NAC  570
      PA = 0.                                                           NAC  580
      SPA = 0.                                                          NAC  590
      PCS = 0.                                                          NAC  600
      SPCS = 0.                                                         NAC  610
      PE = 0.                                                           NAC  620
      SPE = 0.                                                          NAC  630
      EU = 0.474                                                        NAC  640
      ENP = 0.419                                                       NAC  650
      ECS = 1.717                                                       NAC  660
      ULAMDA = 4.91E-4                                                  NAC  670
      PLAMDA = 3.41E-6                                                  NAC  680
      CS4LAM = 1.071E-8                                                 NAC  690
      Y = 0.0683                                                        NAC  700
      CS3SIG = 16.5E-24                                                 NAC  710
      VK = 113.1                                                        NAC  720
C                                                                       NAC  730
      IF(ITI .LE. 1) ZEIT = ZEITN                                       NAC  740
      IF(ITI .LE. 1) ZEIT1 = ZEITN                                      NAC  750
      IF(ITI .GT. 1) ZEIT = ZEITN - ZEIT1                               NAC  760
      IF(ZEITN .GT. 1.E+09) WRITE (6,999)                               NAC  770
      IF(SIG .LE. 0.) SIG = 1.                                          NAC  780
      IF(ITI .GT. 1) GOTO 10                                            NAC  790
      OPEN(UNIT=61,FORM='UNFORMATTED',FILE='nakure')                    NAC  800
      WRITE (6,924) SIG                                                 NAC  810
C                                                                       NAC  820
C     READ POWER-HISTOGRAM                                              NAC  830
C                                                                       NAC  840
      DO 11 IR=1,N197                                                   NAC  850
        READ (61,END=990) MMT1,DUMMY,(TB(MMT1+1-M,IR),PGES(MMT1+1-M,IR),NAC  860
     1   (ZF(I,MMT1+1-M,IR),I=1,7),M=1,MMT1)                            NAC  870
        MMT(IR) = MMT1                                                  NAC  880
   11 CONTINUE                                                          NAC  890
      DO 12 IR=1,N197                                                   NAC  900
        DO 12 M=1,MMT(IR)                                               NAC  910
          TB(M,IR) = TB(M,IR) * 86400.                                  NAC  920
   12 CONTINUE                                                          NAC  930
   10 CONTINUE                                                          NAC  940
      DO 25 IR=1,N197                                                   NAC  950
        DO 35 I=1,IACT                                                  NAC  960
          DENSM(I) = DEN(I,IR) * VL(IR) * 1.E24                         NAC  970
   35   CONTINUE                                                        NAC  980
        QNW(IR) = 0.                                                    NAC  990
        QNWR(IR) = 0.                                                   NAC 1000
        PBET(IR) = 0.                                                   NAC 1010
        RNK = VL(IR) * RHOKS / VK                                       NAC 1020
        IRNK = IFIX(RNK)                                                NAC 1030
        IF(IR .GT. 1) GOTO 49                                           NAC 1040
        IF(ITI .LE. 1) WRITE (6,923) IRNK,IR,A0                         NAC 1050
        GOTO 50                                                         NAC 1060
   49   CONTINUE                                                        NAC 1070
        EPSI = (VL(IR)-VL(IR-1)) / VL(IR)                               NAC 1080
        EPSI = ABS(EPSI)                                                NAC 1090
        IF(EPSI .GT. 0.001 .AND. ITI .LE. 1) WRITE (6,923) IRNK,IR,A0   NAC 1100
   50   CONTINUE                                                        NAC 1110
        MMT2 = MMT(IR)                                                  NAC 1120
        DO 100 I=1,MMT2                                                 NAC 1130
          TA(I) = 0.                                                    NAC 1140
          RU(I) = 0.                                                    NAC 1150
          RTH(I) = 0.                                                   NAC 1160
          DO 100 J=1,4                                                  NAC 1170
            P(I,J) = 0.                                                 NAC 1180
  100   CONTINUE                                                        NAC 1190
        PBETR = PGES(MMT2,IR)                                           NAC 1200
        IF(PBETR .LE. 0. OR. IRNK .LT. 1) GOTO 25                       NAC 1210
        PTOT = PTOT + PBETR                                             NAC 1220
        DO 360 M=1,MMT2                                                 NAC 1230
          S(M) = 0.                                                     NAC 1240
          P(M,1) = (ZF(1,M,IR)+ZF(2,M,IR)) * PGES(M,IR)                 NAC 1250
          P(M,2) = (1.-ZF(7,M,IR)) * PGES(M,IR)                         NAC 1260
          P(M,3) = ZF(3,M,IR) * PGES(M,IR)                              NAC 1270
          P(M,4) = ZF(4,M,IR) * PGES(M,IR)                              NAC 1280
          RTH(M) = ZF(5,M,IR) * ZF(7,M,IR)                              NAC 1290
          RU(M) = ZF(6,M,IR) * ZF(7,M,IR)                               NAC 1300
          S(M) = PGES(M,IR)                                             NAC 1310
  360   CONTINUE                                                        NAC 1320
        TA(MMT2) = ZEITN                                                NAC 1330
        IF(MMT2 .LT. 2) GOTO 366                                        NAC 1340
        DO 365 II=2,MMT2                                                NAC 1350
          I = MMT2 + 1 - II                                             NAC 1360
          TA(I) = TA(I+1) + TB(I+1,IR)                                  NAC 1370
  365   CONTINUE                                                        NAC 1380
  366   CONTINUE                                                        NAC 1390
        IRR = IR                                                        NAC 1400
C                                                                       NAC 1410
        CALL SRPS(PS,ITI,MMT2,IRR,ZEIT,N200,E,TB)                       NAC 1420
C                                                                       NAC 1430
        CALL SRPB(PB,MMT2,ENP,EU,PLAMDA,ULAMDA,N200,IRR,TB)             NAC 1440
C                                                                       NAC 1450
        CALL SRPE(PE,ZEITN,PS)                                          NAC 1460
C                                                                       NAC 1470
        CALL SRPA(PA,DENSM)                                             NAC 1480
C                                                                       NAC 1490
        IF(ITI .GT. 1) GOTO 20                                          NAC 1500
C                                                                       NAC 1510
        CALL SRPCS(PCS1,MMT2,A0,CS3SIG,CS4LAM,ECS,RNK,SM,Y,N200,IRR,TB) NAC 1520
C                                                                       NAC 1530
        RWCS(IR) = PCS1                                                 NAC 1540
   20   CONTINUE                                                        NAC 1550
        PCS = RWCS(IR) * EXP(-CS4LAM*ZEITN)                             NAC 1560
        SPS = SPS + PS                                                  NAC 1570
        SPB = SPB + PB                                                  NAC 1580
        SPA = SPA + PA                                                  NAC 1590
        SPE = SPE + PE                                                  NAC 1600
        SPCS = SPCS + PCS                                               NAC 1610
        SUM = PS + PE + PCS + PA + PB                                   NAC 1620
        SUM = SUM * SIG                                                 NAC 1630
        QNW(IR) = SUM                                                   NAC 1640
        IF(PBETR .NE. 0.) QNWR(IR) = SUM / PBETR                        NAC 1650
        PBET(IR) = PBETR                                                NAC 1660
   25 CONTINUE                                                          NAC 1670
      REWIND 61                                                         NAC 1680
      RMAX = 0.                                                         NAC 1690
      RMIN = 1.                                                         NAC 1700
      DO 80 I=1,N197                                                    NAC 1710
        IF(QNWR(I) .LE. RMAX) GOTO 70                                   NAC 1720
        RMAX = QNWR(I)                                                  NAC 1730
        IMAX = I                                                        NAC 1740
   70   CONTINUE                                                        NAC 1750
        IF(QNWR(I) .GE. RMIN .OR. QNWR(I) .LE. 0.) GOTO 80              NAC 1760
        RMIN = QNWR(I)                                                  NAC 1770
        IMIN = I                                                        NAC 1780
   80 CONTINUE                                                          NAC 1790
      IF(IOPUT .GT. 0 .AND. JNS .GT. 0) WRITE (6,901) RMAX,IMAX,RMIN,   NAC 1800
     1 IMIN                                                             NAC 1810
      PNGES = 0.                                                        NAC 1820
      DO 130 I=1,N197                                                   NAC 1830
        PNGES = PNGES + QNW(I)                                          NAC 1840
  130 CONTINUE                                                          NAC 1850
      SPS = SPS * SIG                                                   NAC 1860
      SPA = SPA * SIG                                                   NAC 1870
      SPB = SPB * SIG                                                   NAC 1880
      SPE = SPE * SIG                                                   NAC 1890
      SPCS = SPCS * SIG                                                 NAC 1900
      IF(IOPUT .LE. 0 .OR. JNS .LE. 0) GOTO 140                         NAC 1910
      WRITE (6,904) SPS,SPB,SPA,SPCS,SPE,PNGES                          NAC 1920
  140 CONTINUE                                                          NAC 1930
      ITI = 2                                                           NAC 1940
      RETURN                                                            NAC 1950
  990 WRITE(6,*) (' NAKURE-LIB missing or uncomplete')                  NAC 1960
      STOP                                                              NAC 1970
C                                                                       NAC 1980
  901 FORMAT (//1X,'MAXIMUM OF RELATIVE DECAY HEAT POWER:',E12.5,1X,'IN NAC 1990
     1VSOP-BATCH:',I4,1X,'     MINIMUM:',E12.5,1X,'IN VSOP-BATCH:',I4)  NAC 2000
  904 FORMAT (///1X,'DECAY HEAT POWER OF FISSION PRODUCTS:',14X,E12.5,  NAC 2010
     1 ' WATTS'/21X,'U-239 and NP239:  ',13X,E12.5,' WATTS'/21X,        NAC 2020
     2 'REST-ACTINIDES:',16X,E12.5,' WATTS'/21X,'CS-134:',24X,E12.5,' WANAC 2030
     3TTS'/21X,'REST-NEUTRON CAPTURE PRODUCTS:',1X,E12.5,' WATTS'//1X,  NAC 2040
     4 'TOTAL DECAY HEAT POWER:',28X,E12.5,' WATTS'//)                  NAC 2050
  923 FORMAT (1X,'** CALCULATION OF',I7,' PEBBLES/BATCH, STARTING WITH BNAC 2060
     1ATCH NO.',I5,' (E-FISS (A0) = ',F5.2,' %)')                       NAC 2070
  924 FORMAT (/1X,'** ALL DECAY HEAT VALUES ARE MULTIPLIED BY FACTOR',  NAC 2080
     1 E11.4,' **'/)                                                    NAC 2090
  999 FORMAT (1X,'***** ATTENTION *****   PROGRAM OUT OF LIMITS, (NZEIT,NAC 2100
     110**9 SEC) EXCEEDED TIME LIMITS'/////)                            NAC 2110
      END                                                               NAC 2120
      BLOCK DATA                                                        LOC   10
C                                                                       LOC   20
      COMMON /NW1/ ALPHA(24,4),ALAMDA(24,4),TA(50),P(50,4)              LOC   30
C                                                                       LOC   40
      COMMON /NW4/ TIMENW(10),FH(10),FA(10),A(44)                       LOC   50
C                                                                       LOC   60
      COMMON /NW5/ BETA(24,4),DELTAQ(4)                                 LOC   70
C                                                                       LOC   80
      COMMON /ADRS/ KA(10),KL(10),ARRAY(10)                             LOC   90
C                                                                       LOC  100
      COMMON /NW7/ Q(4)                                                 LOC  110
C                                                                       LOC  120
      CHARACTER*5 ARRAY                                                 LOC  130
C                                                                       LOC  140
CFZJ053                                                       19.07.07  LOC  150
      DATA Q/202.2,205.5,211.2,213.7/,DELTAQ/0.5,1.0,0.7,0.7/           LOC  160
C                                                                       LOC  170
      DATA ALPHA/0.,6.5057E-1,5.1264E-1,2.438E-1,1.3850E-1,5.5440E-2,   LOC  180
     1 2.2225E-2,3.3088E-3,9.3015E-4,8.0943E-4,1.9567E-4,3.2535E-5,     LOC  190
     2 7.5595E-6,2.5232E-6,4.9948E-7,1.8531E-7,2.6608E-8,2.2398E-9,     LOC  200
     3 8.1641E-12,8.7797E-11,2.5131E-14,3.2176E-16,4.5038E-17,7.4791E-17LOC  210
     4 ,0.,1.2311,1.1486,7.0701E-1,2.5209E-1,7.1870E-2,2.829E-2,        LOC  220
     5 6.8382E-3,1.2322E-3,6.8409E-4,1.6975E-4,2.4182E-5,6.6356E-6,     LOC  230
     6 1.0075E-6,4.9894E-7,1.6352E-7,2.3355E-8,2.8094E-9,3.6236E-11,    LOC  240
     7 6.4577E-11,4.4963E-14,3.6654E-16,5.6293E-17,7.1602E-17,0.,       LOC  250
     8 2.0830E-1,3.8530E-1,2.2130E-1,9.4600E-2,3.531E-2,2.2920E-2,      LOC  260
     9 3.9460E-3,1.317E-3,7.0520E-4,1.4320E-4,1.7650E-5,7.3470E-6,      LOC  270
     X 1.7470E-6,5.4810E-7,1.6710E-7,2.1120E-8,2.9960E-9,5.1070E-11,    LOC  280
     Y 5.7300E-11,4.1380E-14,1.0880E-15,2.4540E-17,7.5570E-17,0.,       LOC  290
     Z 6.9719E-1,4.9499E-1,1.4422E-1,6.2519E-2,2.9637E-2,4.9236E-3,     LOC  300
     Z 7.0004E-4,1.2989E-3,-2.354E-4,5.8466E-4,6.5066E-5,-5.184E-5,     LOC  310
     Z 5.6861E-5,1.8962E-6,4.4108E-7,1.6460E-7,4.2263E-10,1.6772E-8,    LOC  320
     Z -4.632E-10,3.8784E-9,1.0481E-10,-1.791E-12,5.2476E-11/           LOC  330
C                                                                       LOC  340
      DATA ALAMDA/2.4990,2.2138E+1,5.1587E-1,1.9594E-1,1.0314E-1,       LOC  350
     1 3.3656E-2,1.1681E-2,3.5870E-3,1.3930E-3,6.2630E-4,1.8906E-4,     LOC  360
     2 5.4988E-5,2.0958E-5,1.0010E-5,2.5438E-6,6.6361E-7,1.2290E-7,     LOC  370
     3 2.7213E-8,4.3714E-9,7.5780E-10,2.4786E-10,2.2384E-13,2.4600E-14, LOC  380
     4 1.5699E-14,2.9055,3.2881,9.3805E-1,3.7073E-1,1.1118E-1,3.6143E-2,LOC  390
     5 1.3272E-2,5.0133E-3,1.3655E-3,5.5158E-4,1.7873E-4,4.9032E-5,     LOC  400
     6 1.7058E-5,7.0465E-6,2.3190E-6,6.4480E-7,1.2649E-7,2.5548E-8,     LOC  410
     7 8.4782E-9,7.5130E-10,2.4188E-10,2.2739E-13,9.0536E-14,5.6098E-15,LOC  420
     8 2.1836,1.002E+1,6.4330E-1,2.1860E-1,1.0040E-1,3.7280E-2,1.4350E-2LOC  430
     9 ,4.5490E-3,1.3280E-3,5.3560E-4,1.7300E-4,4.8810E-5,2.0060E-5,    LOC  440
     X 8.3190E-6,2.3580E-6,6.4500E-7,1.2780E-7,2.4660E-8,9.3780E-9,     LOC  450
     Y 7.4500E-10,2.4260E-10,2.2100E-13,2.6400E-14,1.3800E-14,2.2,      LOC  460
     Z 1.0223,2.8135E-1,1.092E-1,4.2857E-2,1.4286E-2,5.1913E-3,1.5686E-3LOC  470
     Z ,1.0694E-3,5.3883E-4,3.6154E-4,9.2159E-5,3.4793E-5,3.1132E-5,    LOC  480
     Z 7.9226E-6,2.2522E-6,6.2943E-7,2.0419E-7,1.2453E-7,4.1941E-8,     LOC  490
     Z 2.4791E-8,1.1547E-8,3.8759E-9,7.444E-10/                         LOC  500
C                                                                       LOC  510
      DATA BETA/2.9640E 00,2.5739E-01,3.8948E-02,9.6897E-03,4.6536E-03, LOC  520
     1 1.1353E-03,3.9893E-04,6.8056E-05,1.7065E-05,1.4139E-05,4.0322E-06LOC  530
     2 ,5.0468E-07,3.7017E-08,5.4362E-08,1.0741E-08,3.6042E-09,         LOC  540
     3 5.3327E-10,4.4836E-11,1.6314E-13,1.7608E-12,4.9856E-16,6.4033E-18LOC  550
     4 ,9.1122E-19,1.4982E-18,1.7096E-01,2.2850E-01,2.8887E-01,         LOC  560
     5 1.5385E-01,4.5971E-02,1.5754E-02,2.9260E-03,4.2720E-04,7.9935E-05LOC  570
     6 ,3.2309E-05,1.0408E-05,1.2033E-06,3.2115E-07,4.0651E-08,         LOC  580
     7 1.7640E-08,5.7770E-09,8.0103E-10,1.1941E-10,3.2620E-12,3.2213E-12LOC  590
     8 ,2.2560E-15,1.8358E-17,2.8107E-18,3.5750E-18,2.3195E 00,         LOC  600
     9 1.1261E-01,2.1063E-02,1.1341E-02,5.8010E-03,1.3538E-03,8.7608E-04LOC  610
     X ,1.6360E-04,5.3738E-05,2.2605E-05,7.0454E-06,8.4819E-07,         LOC  620
     Y 2.9721E-07,9.9509E-08,2.7086E-08,8.3527E-09,1.0569E-09,1.4978E-10LOC  630
     Z ,2.5521E-12,2.8608E-12,2.0722E-15,5.4206E-17,1.2268E-18,         LOC  640
     Z 3.9291E-18,0.,3.4860E-02,2.4750E-02,7.2110E-03,3.1260E-03,       LOC  650
     Z 1.4819E-03,2.4618E-04,3.5002E-05,6.4945E-05,-1.1770E-05,         LOC  660
     Z 2.9233E-05,3.2533E-06,-2.5920E-06,2.8431E-06,9.4810E-08,         LOC  670
     Z 2.2054E-08,8.2300E-09,2.1131E-11,8.3860E-10,-2.3160E-11,         LOC  680
     Z 1.9392E-10,5.2405E-12,-8.9550E-14,2.6238E-12/                    LOC  690
C                                                                       LOC  700
      DATA TIMENW/0.,1.,2.,3.,4.,5.,6.,7.,8.,9./,FA/0.006,0.007,0.010,  LOC  710
     1 0.016,0.033,0.058,0.079,0.151,0.284,0.776/,FH/0.023,0.025,0.022, LOC  720
     2 0.027,0.043,0.062,0.047,0.046,0.067,0.016/,ARRAY/'QNW','QNWR','E'LOC  730
     3 ,'PREG','QREG','QREGR','RWCS','QSD','ESA','SDREG'/               LOC  740
C                                                                       LOC  750
      DATA A/2.532E-2,-2.463E-3,6.775E-5,-9.548,2.0521,-0.04015,1.02153,LOC  760
     1 -1.0203E-2,2.321E-5,-14.641,2.5467,1.7897,-3.8927E-1,2.7495E-2,  LOC  770
     2 -5.3864E-2,2.5038E-2,-1.2473E-4,-15.298,3.6409,8.733E-3,8.6225E-4LOC  780
     3 ,-2.5036E-1,3.0578E-2,-1.5199E-4,-8.1223,2.1345,-0.376,0.0804,   LOC  790
     4 -8.9862,2.1398,1.0148,-0.3121,-1.0632E-1,-5.4357E-5,5.7339E-5,   LOC  800
     5 -9.0506,0.8344,-0.03845,-7.3227,1.6505,-0.2092,8.4386E-3,        LOC  810
     6 -1.07296E-3,0.16/                                                LOC  820
      END                                                               LOC  830
      SUBROUTINE SRPS(SUM1,ITI,MMT2,IRR,ZEIT,N200,E,TB)                 SRP   10
C                                                                       SRP   20
C     BERECHNUNG DER NACHZERFALLSLEISTUG DER SPALTPRODUKTE              SRP   30
C                                                                       SRP   40
      COMMON /NW1/ ALPHA(24,4),ALAMDA(24,4),TA(50),P(50,4)              SRP   50
C                                                                       SRP   60
      COMMON /NW7/ Q(4)                                                 SRP   70
C                                                                       SRP   80
      DIMENSION E(24,4,N200),TB(50,N200)                                SRP   90
C                                                                       SRP  100
C                                                                       SRP  110
      IR = IRR                                                          SRP  120
      SUM1 = 0.                                                         SRP  130
      IF(ITI .GT. 1) GOTO 5                                             SRP  140
      DO 6 I=1,4                                                        SRP  150
        DO 7 J=1,24                                                     SRP  160
          A = ALPHA(J,I) / ALAMDA(J,I)                                  SRP  170
          SUM2 = 0.                                                     SRP  180
          DO 3 K=1,MMT2                                                 SRP  190
            B = 1. - EXP(-ALAMDA(J,I)*TB(K,IR))                         SRP  200
            C = EXP(-ALAMDA(J,I)*TA(K))                                 SRP  210
            D = P(K,I) / Q(I)                                           SRP  220
            SUM2 = SUM2 + (B*C*D)                                       SRP  230
    3     CONTINUE                                                      SRP  240
          E(J,I,IR) = A * SUM2                                          SRP  250
          SUM1 = SUM1 + A * SUM2                                        SRP  260
    7   CONTINUE                                                        SRP  270
    6 CONTINUE                                                          SRP  280
      GOTO 4                                                            SRP  290
    5 CONTINUE                                                          SRP  300
      DO 8 I=1,4                                                        SRP  310
        DO 8 J=1,24                                                     SRP  320
          SUM1 = SUM1 + E(J,I,IR) * EXP(-ALAMDA(J,I)*ZEIT)              SRP  330
    8 CONTINUE                                                          SRP  340
    4 CONTINUE                                                          SRP  350
      RETURN                                                            SRP  360
      END                                                               SRP  370
      SUBROUTINE SRPB(S1,MMT2,ENP,EU,PLAMDA,ULAMDA,N200,IR,TB)          RPB   10
C                                                                       RPB   20
C     BERECHNUNG DER NACHZERFALLSLEISTUG VON U-239 + NP-239 NACH        RPB   30
C     DIN 25485                                                         RPB   40
C                                                                       RPB   50
      COMMON/ NW1/ ALPHA(24,4),ALAMDA(24,4),TA(50),P(50,4)              RPB   60
C                                                                       RPB   70
      COMMON /NW2/ RU(50),RTH(50)                                       RPB   80
C                                                                       RPB   90
      COMMON /NW7/ Q(4)                                                 RPB  100
C                                                                       RPB  110
      DIMENSION TB(50,N200)                                             RPB  120
C                                                                       RPB  130
C                                                                       RPB  140
      S1 = 0.                                                           RPB  150
      DO 2 K=1,MMT2                                                     RPB  160
        FU = EU * RU(K) * (1.-EXP(-ULAMDA*TB(K,IR))) * EXP(-ULAMDA*     RPB  170
     1   TA(K))                                                         RPB  180
        F1 = (ULAMDA/(ULAMDA-PLAMDA)) * (1.-EXP(-PLAMDA*TB(K,IR))) *    RPB  190
     1   EXP(-PLAMDA*TA(K))                                             RPB  200
        F2 = (PLAMDA/(ULAMDA-PLAMDA)) * (1.-EXP(-ULAMDA*TB(K,IR))) *    RPB  210
     1   EXP(-ULAMDA*TA(K))                                             RPB  220
        FNP = ENP * RU(K) * (F1-F2)                                     RPB  230
        POW = 0.                                                        RPB  240
        DO 21 L=1,4                                                     RPB  250
          POW = POW + P(K,L) / Q(L)                                     RPB  260
   21   CONTINUE                                                        RPB  270
        S1 = S1 + POW * (FU+FNP)                                        RPB  280
    2 CONTINUE                                                          RPB  290
      RETURN                                                            RPB  300
      END                                                               RPB  310
      SUBROUTINE SRPCS(PCS1,MMT2,A0,CS3SIG,CS4LAM,ECS,RNK,SM,Y,N200,IR, RPC   10
     1 TB)                                                              RPC   20
C                                                                       RPC   30
C     BEITRAG DES CS-134                                                RPC   40
C                                                                       RPC   50
      COMMON /NW1/ ALPHA(24,4),ALAMDA(24,4),TA(50),P(50,4)              RPC   60
C                                                                       RPC   70
      COMMON /NW3/ PHI(50),S(50)                                        RPC   80
C                                                                       RPC   90
      COMMON /NW7/ Q(4)                                                 RPC  100
C                                                                       RPC  110
      DIMENSION TB(50,N200)                                             RPC  120
C                                                                       RPC  130
C                                                                       RPC  140
C     BERECHNUNG DES TOTALEN NEUTRONENFLUSSES                           RPC  150
C                                                                       RPC  160
      AE = (-0.00278*SM+0.04865) * A0 + 0.03663 * SM - 0.02434          RPC  170
      CS4SIG = (19.2+4.*(12.-SM)) * 1.E-24                              RPC  180
      CS31 = 0.                                                         RPC  190
      CS41 = 0.                                                         RPC  200
      DO 2 K=1,MMT2                                                     RPC  210
        SS = 0.                                                         RPC  220
        KK = K                                                          RPC  230
        DO 23 M=1,KK                                                    RPC  240
          SK = 1.E-03 * S(M) / RNK                                      RPC  250
          SS = SS + SK * TB(M,IR) / 86400.                              RPC  260
   23   CONTINUE                                                        RPC  270
        PK = 1.E-03 * S(K)                                              RPC  280
        PHI(K) = 1.E+14 * (PK/RNK) / (AE-0.0138*SS/SM**2.)              RPC  290
C                                                                       RPC  300
C     NACHWAERME CS-134                                                 RPC  310
C                                                                       RPC  320
        POW = 0.                                                        RPC  330
        DO 24 L=1,4                                                     RPC  340
          POW = POW + P(K,L) / Q(L)                                     RPC  350
   24   CONTINUE                                                        RPC  360
        SOURCE = Y * POW                                                RPC  370
        SINK3 = CS3SIG * PHI(K)                                         RPC  380
        SINK4 = CS4SIG * PHI(K) + CS4LAM                                RPC  390
CFZJ052                                                       14.05.07  RPC  400
        IF(SINK3 .GT. 0.) CS32 = SOURCE / SINK3 + (CS31-SOURCE/SINK3) * RPC  410
     1   EXP(-SINK3*TB(K,IR))                                           RPC  420
        CS42 = CS41 * EXP(-SINK4*TB(K,IR)) + (SOURCE/SINK4) * (1.-      RPC  430
     1   EXP(-SINK4*TB(K,IR))) + ((SINK3*CS31-SOURCE)/(SINK4-SINK3)) *  RPC  440
     2   (EXP(-SINK3*TB(K,IR))-EXP(-SINK4*TB(K,IR)))                    RPC  450
        CS31 = CS32                                                     RPC  460
        CS41 = CS42                                                     RPC  470
    2 CONTINUE                                                          RPC  480
      PCS1 = CS4LAM * ECS * CS41                                        RPC  490
      RETURN                                                            RPC  500
      END                                                               RPC  510
      SUBROUTINE SRPE(PE,ZEITN,SUM1)                                    RPE   10
C                                                                       RPE   20
C     CONTRIBUTION OF ACTIVATED FISSION PRODUCTS                        RPE   30
C                                                                       RPE   40
      COMMON /NW4/ TIMENW(10),FH(10),FA(10),A(44)                       RPE   50
C                                                                       RPE   60
C                                                                       RPE   70
      IF(ZEITN .LE. 0.) ZEITN = 1.E-6                                   RPE   80
      ZEIT = ALOG10(ZEITN)                                              RPE   90
      IF(ZEIT .LT. TIMENW(1)) ZEIT = TIMENW(1)                          RPE  100
      IF(ZEIT .GT. TIMENW(10)) ZEIT = TIMENW(10)                        RPE  110
      DO 1 I=1,9                                                        RPE  120
        IF(ZEIT .LT. TIMENW(I) .OR. ZEIT .GT. TIMENW(I+1)) GOTO 1       RPE  130
        ZEIT1 = TIMENW(I)                                               RPE  140
        ZEIT2 = TIMENW(I+1)                                             RPE  150
        DELP1 = FH(I+1) - FH(I)                                         RPE  160
        RITP = (ZEIT-ZEIT1) / (ZEIT2-ZEIT1)                             RPE  170
        PE = (FH(I)+(DELP1*RITP)) * SUM1                                RPE  180
    1 CONTINUE                                                          RPE  190
      RETURN                                                            RPE  200
      END                                                               RPE  210
      SUBROUTINE SRPA(PA,DENSM)                                         RPA   10
C                                                                       RPA   20
C     CONTRIBUTION OF HEAVY METAL ISOTOPES                              RPA   30
C                                                                       RPA   40
      COMMON /FLUXN/ D(361),IACT                                        RPA   50
C                                                                       RPA   60
      COMMON /NUKDAT/ DDLAM(30),IIU(30),FFB1(30),FFP(30),FFP1(30),      RPA   70
     1 FFT(30),FFA(30),FFSF(30),FFNG1(30),FFN2N1(30),QMEV(30)           RPA   80
C                                                                       RPA   90
      COMMON /DECAYT/ DECY(30)                                          RPA  100
C                                                                       RPA  110
      DIMENSION DENSM(IACT)                                             RPA  120
C                                                                       RPA  130
C                                                                       RPA  140
      PA = 0.                                                           RPA  150
      DO 1 I=1,IACT                                                     RPA  160
        IF(I .EQ. 10 .OR. I .EQ. 13) GOTO 1                             RPA  170
C                                                                       RPA  180
CFZJ053                                                       19.07.07  RPA  190
        PA = PA + DENSM(I) * DECY(I) * QMEV(I) * 1.6022E-13             RPA  200
    1 CONTINUE                                                          RPA  210
      RETURN                                                            RPA  220
      END                                                               RPA  230