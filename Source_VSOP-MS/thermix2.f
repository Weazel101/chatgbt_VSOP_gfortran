      FUNCTION DDF1(RA,RI,PR,PL,INDGEO,RM)                              DDF   10
C                                                                       DDF   20
C                                                                       DDF   30
      DDF = (RA-RI) * (PR-PL)                                           DDF   40
C                                                                       DDF   50
      DDF1 = DDF                                                        DDF   60
C                                                                       DDF   70
      IF(INDGEO .EQ. 0) RETURN                                          DDF   80
      DDF = DDF * RM                                                    DDF   90
C                                                                       DDF  100
      DDF1 = DDF                                                        DDF  110
C                                                                       DDF  120
      RETURN                                                            DDF  130
      END                                                               DDF  140
      SUBROUTINE EINL1(ITLAM,IFKON,TDIFF,NLOOP,QNORM,IFRED,NHET,CP0,    INL   10
     1 IFTEST,IFDF,IEXPR,HEPS,HKUG,DI,NHMAT1,NHMAT2,NHZON,XFWQZ,IFHET,  INL   20
     2 IFWKT,IFLT,WPR,DOS,IFBER,WI,PHIP,RADP,DR,DPH,RAD,PHI,KOM,LAM,TVORINL   30
     3 ,ALP,REF,IFTV,KART,RHO,C,EPS,IDIR,IFANIS,ZEIV,TKV,NTVAR,MFR,MFZ, INL   40
     4 ITM3,PVOR,IFBR,ALPHA,IFBQ,EPSIL,XKON,DHYD,STZUK,TFLVOR,IFZST,    INL   50
     5 IFZTF,ITK,KOC,KG,FFTHX,IRII)                                     INL   60
CFZJ023                                                       02.02.04  INL   70
CFZJ042                                                       09.09.05  INL   80
C                                                                       INL   90
C     EINLESEPROGRAMM FUER WAERMELEITPROGRAMMTEIL                       INL  100
C                                                                       INL  110
      COMMON /VAIZ/ AIZ,BEM                                             INL  120
C                                                                       INL  130
      COMMON /DYKOP1/ ANIN(21),NDYN,TMOD(10),TFUEL(10),DZDYN(10),PT(10),INL  140
     1 ZDYN(10)                                                         INL  150
C                                                                       INL  160
      COMMON /DYKOP3/ NREF,TREF(5),IOREF(5),IUREF(5),JLREF(5),JRREF(5)  INL  170
C                                                                       INL  180
      COMMON /HET1/ F                                                   INL  190
C                                                                       INL  200
      COMMON /RSTRT/ IFRSTA,ZRST,ILOGR                                  INL  210
C                                                                       INL  220
      COMMON /WKAPT/ IFWKPT                                             INL  230
C                                                                       INL  240
      COMMON /REDUZR/ VFLIES,T0,Z0,ZU                                   INL  250
C                                                                       INL  260
      COMMON /LAMT/ IFLAMT                                              INL  270
C                                                                       INL  280
      COMMON /BEZEI/ BI,BN                                              INL  290
C                                                                       INL  300
      COMMON /ITER1/ TRMAX,MIT,TN,OVREL,ITMAX,T1,T2,TRELA,NTMAX,IFKOR,  INL  310
     1 ETHA,ORMIN,MITMAX,IKORM,IFREL                                    INL  320
C                                                                       INL  330
CFZJ042                                                       09.09.05  INL  340
      COMMON /KOPPLG/ DTVOR                                             INL  350
C                                                                       INL  360
      COMMON /COUPL/ IPRINT,IPRS                                        INL  370
C                                                                       INL  380
CFZJ042                                                       09.09.05  INL  390
      COMMON /PLOT/ IPUN                                                INL  400
C                                                                       INL  410
      COMMON /BEZ/ PKOM(12),UET(12)                                     INL  420
C                                                                       INL  430
      COMMON /PRINT1/ TITLE(20),INDGEO                                  INL  440
C                                                                       INL  450
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    INL  460
C                                                                       INL  470
      COMMON /KOMP1/ KMAX                                               INL  480
C                                                                       INL  490
      COMMON /KES/ KS                                                   INL  500
C                                                                       INL  510
CFZJ004 enlarged dimensions common trans                      28.11.03  INL  520
      COMMON /TRANS/ IFINST,INTVAL,DZEIT(300),ZEI(300),NPRINT(300),     INL  530
     1 NKONV(300)                                                       INL  540
C                                                                       INL  550
      COMMON /ERROR/ ABTEXT(20),IERRM,IERRL,IERRF(5),NHIN,NWARN,NFEHL   INL  560
C                                                                       INL  570
      COMMON /TVAR/ IFTKV                                               INL  580
C                                                                       INL  590
      COMMON /RZCOMP/ IREARZ,MCR,MCZ,ICONVC                             INL  600
C                                                                       INL  610
      COMMON /SPECTI/ ITIK(10),DUM(18),NREST,IREST,TXNEW                INL  620
C                                                                       INL  630
      COMMON /OPT/ KENN,IOPUT,NIFKO,DZT                                 INL  640
C                                                                       INL  650
CFZJ006 enlarged dimensions common QVAR                       28.11.03  INL  660
      COMMON /QVAR/ DUMM(2),QVOLLL,DUN(1508),N61,URZ,DUY(303),PSPALT,   INL  670
     1 D(2),JREST,TAU,DD(11),ST(300)                                    INL  680
C                                                                       INL  690
CFZJ042                                                       09.09.05  INL  700
      COMMON /BLINDL/ TMITL,M24,NGEOM,CIZET0                            INL  710
C                                                                       INL  720
      COMMON /KOMVAK/ KOMVAR,KONVAR,XKSUMK,NQVAR,TSTAT,XKSTAT,DTDDX,    INL  730
     1 TWUNS(10),QWUNS(10),TVSEK                                        INL  740
C                                                                       INL  750
      COMMON /MPUTA/ TEIMIN,TEIMAX,EMP0,TAU0,MPUTAU,QWU,DTAU,TEI0,NVR   INL  760
C                                                                       INL  770
      COMMON /KSUM/ DU(4),BKV,RHOKS,DA(3),ZEITMI                        INL  780
C                                                                       INL  790
      COMMON /BIRTER/ NTHX,N1,NMAXC,N2                                  INL  800
C                                                                       INL  810
CFZJ005 enlarged dimensions common SPEIKO                     28.11.03  INL  820
      COMMON /SPEIKO/ DUMMY(2701),MM                                    INL  830
C                                                                       INL  840
      COMMON /EPTI/ EPSST,ZEITNW                                        INL  850
C                                                                       INL  860
CFZJ055                                                       25.09.07  INL  870
C                                                                       INL  880
      COMMON /DL/ ILAYV,DLAY(101)                                       INL  890
C                                                                       INL  900
      COMMON /BLOTIK/ N197,HCORE                                        INL  910
C                                                                       INL  920
CFZJ048                                                       11.04.07  INL  930
      COMMON /VARDIT/ B(5000000)                                        INL  940
C                                                                       INL  950
CFZJ042                                                       09.09.05  INL  960
      COMMON /ADDRT/ KX(240),KY(240),LZ(240),NENDPT                     INL  970
C                                                                       INL  980
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             INL  990
C                                                                       INL 1000
      COMMON /PHI98/ PHIA,PHIE,IOPEN                                    INL 1010
C                                                                       INL 1020
CFZJ042                                                       09.09.05  INL 1030
      COMMON /BLF/ IPASS                                                INL 1040
C                                                                       INL 1050
      COMMON /BLOCK1/ IDUM(21),JTPE7                                    INL 1060
C                                                                       INL 1070
      COMMON /BSH/ NHZ                                                  INL 1080
C                                                                       INL 1090
      COMMON /TABB/ EPSI4                                               INL 1100
C                                                                       INL 1110
      COMMON /ITER/ IB,IT,IT1,IT2,ITM1,ITM2,EPSI1,EPSI2,IFSQ,OVM1,OVM2  INL 1120
C                                                                       INL 1130
      COMMON /PRANDL/ PRAN                                              INL 1140
C                                                                       INL 1150
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      INL 1160
C                                                                       INL 1170
      COMMON /ZEITVR/ IFZDR,ZTF(3,100),ZST(3,100),ZDR(100),ZVOR(100)    INL 1180
     1 ,IFZ,IZKOM(3),IZKO,IFAGL,IZAEHL                                  INL 1190
C                                                                       INL 1200
CFZJ026                                                       16.03.04  INL 1210
      COMMON /YEAR/ IYEAR,RLAM32                                        INL 1220
C                                                                       INL 1230
CFZJ042                                                       09.09.05  INL 1240
      COMMON /KITK/ ITKK(1000),KTX9(5)                                  INL 1250
C                                                                       INL 1260
      DIMENSION HEPS(KMAZ),HKUG(KMAZ),DI(KMAZ,5),NHMAT1(KMAZ,5),        INL 1270
     1 NHMAT2(KMAZ,5),NHZON(KMAZ),XFWQZ(KMAZ,5),IFHET(KMAZ),IFWKT(KMAZ),INL 1280
     2 IFLT(KMAZ),WPR(KMAZ),DOS(KMAZ),IFBER(IMAZ,NMAZ),WI(IMAZ,NMAZ),   INL 1290
     3 PHIP(NMAZ),RADP(IMAZ),DR(IMAZ),DPH(NMAZ),RAD(IMAZ+1),PHI(NMAZ+1),INL 1300
     4 KOM(IMAZ,NMAZ),TVOR(KMAZ),ALP(KMAZ),REF(KMAZ),IFTV(KMAZ),        INL 1310
     5 KART(KMAZ),RHO(KMAZ),C(KMAZ),EPS(KMAZ),IDIR(KMAZ),IFANIS(KMAZ),  INL 1320
     6 ZEIV(10,KMAZ),TKV(10,KMAZ),NTVAR(KMAZ),MFR(IMAZ),MFZ(NMAZ),      INL 1330
     7 TUM(10),ZUM(10),NLAMT(100),NRHOC(100),A1(9),ICODEF(5),EPS1(1000),INL 1340
     8 EPS2(1000),DELR(100),MPR(100),DELZ(200),MPZ(200),NOP(200),       INL 1350
     9 PVOR(KOMAX),IFBR(KOMAX),ALPHA(KOMAX),IFBQ(KOMAX),EPSIL(KOMAX),   INL 1360
     X XKON(KOMAX),DHYD(KOMAX),STZUK(KOMAX),TFLVOR(KOMAX),IFZST(KOMAX), INL 1370
     Y IFZTF(KOMAX),ITK(KMAZ),KOC(IMAZ,NMAZ),KG(IMAZ,NMAZ),JDUM3(200),  INL 1380
     Z RDUM3(200),FFTHX(IMAZ,NMAZ),FFTHXX(IMAZ,NMAZ),IYEAR(1000)        INL 1390
C                                                                       INL 1400
      REAL LAM(KMAZ),LAM0                                               INL 1410
C                                                                       INL 1420
      CHARACTER*4 BEZN(3)/' X= ','PHI=',' Z= '/,BEZI(3)/' Y= ',' R= ',  INL 1430
     1 ' R= '/,TTL(3)/'THER','MIX:','    '/,BI,BN,TITLE,AIZ(100,2,200,2)INL 1440
     2 ,PKOM                                                            INL 1450
      CHARACTER*3 BV/'HET'/,BC/'COR'/,DB/'DBH'/,BEM(1000),ENDE/'END'/   INL 1460
C                                                                       INL 1470
      EQUIVALENCE(LZ(32),INKOM),(LZ(80),IIKOM),(LZ(81),IR),(LZ(82),IZ), INL 1480
     1 (LZ(151),IDRK),(LZ(84),IDZ),(LZ(141),IKON),(IMAX,A1(1))          INL 1490
C                                                                       INL 1500
    5 FORMAT (18A4)                                                     INL 1510
   10 FORMAT (4E10.3,6I5,2(I2,I3))                                      INL 1520
   12 FORMAT (F6.1,2I2,3E10.3)                                          INL 1530
   14 FORMAT (4I5,I2,I3,F5.1,3E10.3,2F5.1,I2,I2,I1,F5.1)                INL 1540
   20 FORMAT (2I5,I10,2I5,2E10.3,5I5,I2,I2,I1)                          INL 1550
   29 FORMAT (/5X,'IFRSTA=',I2,5(1H.),' TEMPERATURE-RESTART , RESTART ININL 1560
     1PUT DATA IS READ FROM UNIT 19.')                                  INL 1570
   30 FORMAT (/5X,'PROGRAM OPERATING DATA :'//5X,'ETHA=',1PE8.1,' , MITMINL 1580
     1AX=',I6,' , OVREL-MIN=',0PF5.2,' , OVREL-MAX=',F5.2,' , IKOR-MAX='INL 1590
     2 ,I4,' , RELAX.IND.=',I3,' , IERR-MAX=',I5)                       INL 1600
   32 FORMAT (20X,'MAXIMAL NLOOP=',I3,' THERMIX/KONVEK - LOOPS'/20X,'ALLINL 1610
     1OWABLE REL. CHANGE IN TEMPERATURE TDIFF=',2PF5.3,' %')            INL 1620
   33 FORMAT (' **WARNING** STEADY STATE CALCULATION ONLY FOR IFKON= -1 INL 1630
     1POSSIBLE, PROGRAM SETS IFKON= -1')                                INL 1640
   40 FORMAT (//5X,'DATA OF GRID:'//5X,I2,' ROWS, ',I5,' COLUMNS, ',I3, INL 1650
     1 ' DIFFERENT REGIONS, X0-R0=',F6.2,', Y0-PHI0=',F10.2)            INL 1660
   44 FORMAT (/5X,'NUMBER OF REACTOR-ZONES:',I3,' , CORE SURFACE AT Z=',INL 1670
     1 F6.2,' CM'//5X,'ZONE    ZO      ZU'/5X,20(1H-))                  INL 1680
   47 FORMAT (5X,I3,3X,F6.2,2X,F6.2)                                    INL 1690
   70 FORMAT (A3,I2,I5,2E10.3,4F5.2,E10.3,I2,I3,I2,I3,F5.2,I4,I1)       INL 1700
   71 FORMAT (I6,2I3,I6,E6.0,4E12.5)                                    INL 1710
   72 FORMAT (14F5.2)                                                   INL 1720
   76 FORMAT (6E12.5)                                                   INL 1730
   80 FORMAT (' **ERROR** CARD OF COMPOSITION NO.',I3,' IS INCORRECT')  INL 1740
   81 FORMAT (3I3)                                                      INL 1750
   83 FORMAT (15I5)                                                     INL 1760
   84 FORMAT (6(F9.3,I3))                                               INL 1770
   86 FORMAT (6(F8.3,I3,I1))                                            INL 1780
   87 FORMAT (6(I3,F9.3))                                               INL 1790
   90 FORMAT (///5X,'DESCRIPTION OF COMPOSITIONS :'//5X,'COMP.',5X,'TYPEINL 1800
     1',13X,'TEMPERATURE',2X,'ALPHA',4X,'LAMBDA',2X,'LAM(T)    DOSIS   SINL 1810
     2OL.MATTER',3X,'C',5X,'C(T)',4X,'WPR   EPSILON DIRECT. IFTV'/34X,'(INL 1820
     3DEG C)',2X,'(W/CM2/K)',1X,'(W/CM/K)',7X,' (EDN*1.E21)  -PART',2X, INL 1830
     4 '(J/CM3/K)'/5X,128('-'))                                         INL 1840
   91 FORMAT (/5X,'IN CASE OF TRANSIENT CALC. MAX. + AVERAGE TEMPERATUREINL 1850
     1S OF COMPOSITION',I3,' WILL BE DISPLAYED AS *T-MAX PV/T-AVG PV*') INL 1860
   96 FORMAT (//5X,'TIME VARIATION OF BOUNDARY CONDITION OF TEMPERATURESINL 1870
     1:'/)                                                              INL 1880
   98 FORMAT (5X,'REGION',I2,',',5(F8.3,' H',F7.1,' DEG.')/18X,5(F8.3,  INL 1890
     1 ' H',F7.1,' DEG.'))                                              INL 1900
  102 FORMAT (7E8.0,4I4)                                                INL 1910
  111 FORMAT (///5X,'THERE IS A REFLECTING BOUNDARY')                   INL 1920
  112 FORMAT (36X,'INSIDE')                                             INL 1930
  113 FORMAT (36X,'OUTSIDE')                                            INL 1940
  114 FORMAT (36X,'LEFT')                                               INL 1950
  115 FORMAT (36X,'RIGHT')                                              INL 1960
  119 FORMAT (//5X,'NEW CALCULATION OF HEAT CONDUCTIVITY EVERY',I3,' ITEINL 1970
     1RATIONS OR FOR EACH NEW TIME STEP')                               INL 1980
  121 FORMAT (20X,'COUPLING BY MEANS OF CONVECTIVE HEAT SOURCES')       INL 1990
  122 FORMAT (20X,'COUPLING BY MEANS OF GAS TEMPERATURES AND ALPHA*F')  INL 2000
  123 FORMAT (20X,'THE PROGRAM CONTROLS COUPLING BY ITSELF VIA CHANGES IINL 2010
     1N THE GAS TEMPERATURE')                                           INL 2020
  124 FORMAT (/5X,'IFKON=',I3,5(1H.),' CALCULATION OF FLOW FIELD AND GASINL 2030
     1 TEMPERATURES , KONVEK-INPUT')                                    INL 2040
  125 FORMAT (2I6,6E6.0,I6)                                             INL 2050
  129 FORMAT (24I3)                                                     INL 2060
  130 FORMAT (18I4)                                                     INL 2070
  140 FORMAT (10F6.1,E12.5)                                             INL 2080
  160 FORMAT (A3,6I3,10E5.0,I1)                                         INL 2090
  441 FORMAT (/5X,'NUMBER OF REFLECTOR-ZONES:',I3//5X,'ZONE  IO  IU  JL INL 2100
     1 JR'/5X,'--------------------')                                   INL 2110
  443 FORMAT (5X,I3,3X,4(I2,2X))                                        INL 2120
  619 FORMAT (' **WARNING** DZEIT(1) NOT OK, DZEIT(1) = 1 SEC SET BY PROINL 2130
     1GRAM.')                                                           INL 2140
  631 FORMAT (//5X,'INTERNAL CALCULATION OF THERMAL CAPACITY OF FOLLOWININL 2150
     1G MATERIALS'//5X,'NO.    NAME / TYPE                              INL 2160
     2     VOLUMETRIC THERMAL CAPACITY RHO*C (J/CM3/K) = F(T(CELSIUS))',INL 2170
     37X,'TMIN TMAX'/5X,128(1H-))                                       INL 2180
  632 FORMAT (//5X,'INTERNAL CALCULATION OF THERMAL CONDUCTIVITY OF FOLLINL 2190
     1OWING MATERIALS'//5X,'NO.    NAME / TYPE                          INL 2200
     2         THERMAL CONDUCTIVITY LAMBDA (W/CM/K) = F(T(CELSIUS))',14XINL 2210
     3,'TMIN TMAX'/5X,128(1H-))                                         INL 2220
  633 FORMAT (' **WARNING** PLEASE USE UPDATE OF LINKED FUNCTION  W K P INL 2230
     1T  FROM 4.6.82')                                                  INL 2240
  634 FORMAT (' **WARNING** PLEASE USE UPDATE OF LINKED FUNCTION  X L A INL 2250
     1M T  FROM 4.6.82')                                                INL 2260
  639 FORMAT (/5X,'THE EFFECTIVE VOLUMETRIC THERMAL CAPACITIES ARE CALCUINL 2270
     1LATED BY MULTIPLICATION WITH THE AMOUNT OF SOLID MATTER')         INL 2280
  700 FORMAT (E10.3,I5)                                                 INL 2290
  703 FORMAT (E10.3,2I5,E10.3)                                          INL 2300
  711 FORMAT (1X////5X,'COMPOSITIONS WITH BALL-SHELL CALCULATION :'//5X,INL 2310
     1 'COMP',5X,'EPSI',5X,'DIAM.(CM)',2X,'ZONES',6X,'DI (CM)',3X,'LAM(TINL 2320
     2)',3X,'C(T)',2X,'REL. POWER DENSITY'/5X,83('-'))                  INL 2330
  712 FORMAT (3X,I5,5X,2(F5.2,6X),I2)                                   INL 2340
  713 FORMAT (1X,33X,'NO.',I2,6X,F5.2,5X,'NO.',I2,4X,'NO.',I2,5X,1PE10.3INL 2350
     1 )                                                                INL 2360
  802 FORMAT (' **FATAL ERROR** CALCULATION OF CONVECTION ONLY IN R-Z-GEINL 2370
     1OMETRY, INDGEO=',I3,', IFKON=',I3)                                INL 2380
  805 FORMAT (/5X,'INDGEO=',I2,5(1H.),' CALC. OF TEMP. OF SOLID MATTER IINL 2390
     1N X-Y GEOMETRY')                                                  INL 2400
  806 FORMAT (/5X,'INDGEO=',I2,5(1H.),' CALC. OF TEMP. OF SOLID MATTER IINL 2410
     1N R-PHI GEOMETRY')                                                INL 2420
  807 FORMAT (/5X,'INDGEO=',I2,5(1H.),' CALC. OF TEMP. OF SOLID MATTER IINL 2430
     1N R-Z GEOMETRY')                                                  INL 2440
  809 FORMAT (/5X,'INTVAL= 0',5(1H.),' STEADY STATE CALCULATION')       INL 2450
  811 FORMAT (/5X,'INTVAL=',I2,5(1H.),' TRANSIENT CALCULATION , AUTOMATIINL 2460
     1C TIME-CONTROL ACC. (DT/T)-MAX =',2PF6.2,' %   ( "DTVOR" )')      INL 2470
  812 FORMAT (//5X,'TIME-CONTROL :'//5X,'INTV    END-TIME   DZEIT    NPRINL 2480
     1INT  NKONV'/5X,'          (H)      (SEC)'/5X,41('-'))             INL 2490
  814 FORMAT (5X,I3,4X,1PE9.2,0PF8.1,5X,I3,4X,I3)                       INL 2500
  902 FORMAT (/5X,'IPUN=',I4,5(1H.),' LAST FIELD OF TEMPERATURES IS STORINL 2510
     1ED FOR RESTART ON UNIT',I4)                                       INL 2520
  903 FORMAT (/5X,'IFRSTA=',I2,5(1H.),' UNIT-RESTART, DATA FROM RESTART-INL 2530
     1FILE, UNIT 02 MUST BE DEFINED')                                   INL 2540
  909 FORMAT (' **ERROR** TIME INTERVAL ',I2,', INCORRECT FINAL-TIME INPINL 2550
     1UT')                                                              INL 2560
  910 FORMAT (' **WARNING** CALCULATION OF TIME-INTERVAL NO.',I2,' NEEDSINL 2570
     1 MORE THAN 1000 TIME STEPS')                                      INL 2580
 1114 FORMAT (' **CONSIDERATION** IFRFI=1 IS IGNORED, BECAUSE CENTRAL TEINL 2590
     1MPERATURE FOR A SOLID CYLINDER IS CALCULATED')                    INL 2600
 2001 FORMAT (1H1/35X,'***   T  H  E  R  M  I  X  -  INPUT CHECK   ***'/INL 2610
     1 //5X,'CASE :  ',20A4//)                                          INL 2620
 7101 FORMAT (' **FATAL ERROR** PLEASE CHANGE INPUT OF "HET"-ZONES'//17XINL 2630
     1 ,'USING PROGRAM VERSION *VSOP(99/2)* RELATIVE POWER RATES CANNOT INL 2640
     2BE USED ANY LONGER,'/17X,'RELATIVE POWER  D E N S I T I E S  MUST INL 2650
     3BE APPLIED'/17X,'==> CARD(S) TX11 , CHANGE VARIABLE "XFWQZ"'/17X, INL 2660
     4 'PROGRAM IS STOPPED, IF SUM (XFWQZ) = 1.')                       INL 2670
 7102 FORMAT(//5X,'*** INPUT ERROR ON CARD TX8: BOTH (!) EPS1 AND EPS2 MINL 2680
     1UST BE EITHER EQUAL OR GREATER THAN ZERO !!! ***'//)              INL 2690
C                                                                       INL 2700
      IF(IRII .GT. 0) GOTO 203                                          INL 2710
      IPRS = 0                                                          INL 2720
      NLMIN = -100                                                      INL 2730
      NRMIN = -100                                                      INL 2740
      NLA = 0                                                           INL 2750
      NRO = 0                                                           INL 2760
      ILAYV = 0                                                         INL 2770
C                                                                       INL 2780
CARD TX1                                                                INL 2790
C                                                                       INL 2800
      READ(5,5) (TITLE(I),I=3,20)                                       INL 2810
C                                                                       INL 2820
      TITLE(1) = TTL(1)                                                 INL 2830
      TITLE(2) = TTL(2)                                                 INL 2840
C                                                                       INL 2850
C     PROGRAMMFUNKTION                                                  INL 2860
C                                                                       INL 2870
      PI = 3.14159                                                      INL 2880
C                                                                       INL 2890
CARD TX2                                                                INL 2900
C                                                                       INL 2910
CFZJ042                                                       09.09.05  INL 2920
      READ (5,130) IFKON,IPRINT,IPUN,IFRSTA,INTVAL,IFRED,MITMAX,IKORM,  INL 2930
     1 IFREL,ITLAM,NLOOP,IEXPR,(ICODEF(I),I=1,5),IPASS                  INL 2940
C                                                                       INL 2950
      IF(IPRINT .GT.1) IPASS = 0                                        INL 2960
      IPRINT = IPRINT + 1                                               INL 2970
      GOTO(1,2,3,4) IPRINT                                              INL 2980
    1 IPRINT = -3                                                       INL 2990
      GOTO 6                                                            INL 3000
    2 IPRINT = -2                                                       INL 3010
      GOTO 6                                                            INL 3020
    3 IPRINT = 1                                                        INL 3030
      GOTO 6                                                            INL 3040
    4 IPRINT = 2                                                        INL 3050
    6 CONTINUE                                                          INL 3060
CFZJ020                                                       28.01.04  INL 3070
      IF(IEXPR .EQ. 2) OPEN(98,FILE='tempinst')                         INL 3080
      IF(IEXPR .EQ. 1) OPEN(59,FILE='tempstat')                         INL 3090
      IF(IEXPR .EQ. 2) IEXPR = 98                                       INL 3100
      IF(IEXPR .EQ. 1) IEXPR = 59                                       INL 3110
      IF(IFRED .GT. 0) IFRED = 3                                        INL 3120
CFZJ042                                                       09.09.05  INL 3130
      ILOGR = 0                                                         INL 3140
      KOMVAR = 0                                                        INL 3150
      IREST = 0                                                         INL 3160
      IF(IPUN .GT. 0 .OR. IFRSTA .GT. 0) IREST = 19                     INL 3170
      IF(IOPEN .EQ. 0 .AND. IREST .GT. 0) OPEN(IREST,FORM='UNFORMATTED',INL 3180
     1 FILE='thermix')                                                  INL 3190
      IF(IEXPR .NE. 98) GOTO 1000                                       INL 3200
C                                                                       INL 3210
CARD TX3                                                                INL 3220
C                                                                       INL 3230
      READ (5,76) PHIA,PHIE                                             INL 3240
C                                                                       INL 3250
 1000 CONTINUE                                                          INL 3260
C                                                                       INL 3270
CARD TX4                                                                INL 3280
C                                                                       INL 3290
      READ (5,140) QNORM,ETHA,OVREL,ORMIN,TDIFF,EFAK,DTVOR,ZEITMI,EPSST,INL 3300
     1 ZEITNW,FDOSE                                                     INL 3310
C                                                                       INL 3320
      Z0 = 0.                                                           INL 3330
      ZU = HCORE                                                        INL 3340
      T0 = 0.                                                           INL 3350
      VFLIES = 0.                                                       INL 3360
      QNORM = QNORM * 1.E6                                              INL 3370
      QVOLLL = QNORM                                                    INL 3380
      DZT = 0.                                                          INL 3390
      IF(INTVAL .EQ. 1) INTVAL = 2                                      INL 3400
      IF(IFRSTA .GT. 0) NREST = 2                                       INL 3410
      IF(IREST .EQ. 0) NREST = 0                                        INL 3420
      IF(NREST .EQ. 1 .AND. IREST .GT. 0) JREST = IREST                 INL 3430
      IF(DTVOR .EQ. 0.) DTVOR = .05                                     INL 3440
      IF(ETHA .EQ. 0.) ETHA = 1.E-2                                     INL 3450
      IF(OVREL .EQ. 0.) OVREL = 1.7                                     INL 3460
      IF(ORMIN .EQ. 0.) ORMIN = .6                                      INL 3470
      IF(EFAK .EQ. 0.) EFAK = 1.                                        INL 3480
      IF(ILOGR .EQ. 0) ILOGR = 5                                        INL 3490
      IF(MITMAX .EQ. 0) MITMAX = 2000                                   INL 3500
      IF(IKORM .EQ. 0) IKORM = 100                                      INL 3510
      IF(TDIFF .EQ. 0.) TDIFF = .0005                                   INL 3520
      IF(NLOOP .EQ. 0) NLOOP = 100                                      INL 3530
      IF(ITLAM .EQ. 0) ITLAM = 10                                       INL 3540
CFZJ014                                                       08.12.03  INL 3550
      IF(ZEITMI .EQ. 0) ZEITMI = 60.                                    INL 3560
      EFAK = EFAK * IERRM                                               INL 3570
      IERRM = IFIX(EFAK)                                                INL 3580
      IF(INTVAL .GT. 0) GOTO 15                                         INL 3590
      INTVAL = 1                                                        INL 3600
      NPRINT(1) = 1                                                     INL 3610
      NKONV(1) = 0                                                      INL 3620
      DZEIT(1) = 0.                                                     INL 3630
      ZEI(1) = 0.                                                       INL 3640
      IFINST = 0                                                        INL 3650
C                                                                       INL 3660
CARD TX5                                                                INL 3670
C                                                                       INL 3680
   15 CONTINUE                                                          INL 3690
      READ (5,102) EPSI1,EPSI2,OVM1,EPSI4,CP,PRAN,DRUCK,IFZDR,ITM1,ITM2,INL 3700
     1 ITM3                                                             INL 3710
C                                                                       INL 3720
      IF(INTVAL .EQ. 1) GOTO 16                                         INL 3730
C                                                                       INL 3740
CARD TX6                                                                INL 3750
C                                                                       INL 3760
      READ (5,12) DZEIT(1),NPRINT(1),NKONV(1),ZEI(1),DZEIT(2),PSPALT    INL 3770
C                                                                       INL 3780
      NPRINT(2) = 0                                                     INL 3790
      NKONV(2) = 0                                                      INL 3800
      ZEI(2) = 0.                                                       INL 3810
      IF(ZEI(2) .GT. 0.) GOTO 17                                        INL 3820
      DZT = DZEIT(2)                                                    INL 3830
      INTVAL = 1                                                        INL 3840
   17 CONTINUE                                                          INL 3850
      IFINST = 1                                                        INL 3860
      DO 13 J=1,INTVAL                                                  INL 3870
        IF(NPRINT(J) .EQ. 0) NPRINT(J) = 50                             INL 3880
        IF(NKONV(J) .EQ. 0) NKONV(J) = 1                                INL 3890
   13 CONTINUE                                                          INL 3900
   16 CONTINUE                                                          INL 3910
      IF(IFINST .EQ. 0) IFRED = 0                                       INL 3920
C                                                                       INL 3930
C     GITTER                                                            INL 3940
C                                                                       INL 3950
      IFREF = 0                                                         INL 3960
      IF(NREST .GT. 1) GOTO 50                                          INL 3970
C                                                                       INL 3980
CARD TX7                                                                INL 3990
C                                                                       INL 4000
      READ (5,130) IFRFI,IFRFA,IFRFL,IFRFR                              INL 4010
C                                                                       INL 4020
      RAD0 = 0.                                                         INL 4030
      IMAX = 0                                                          INL 4040
      NMAX = 0                                                          INL 4050
CFZJ042                                                       09.09.05  INL 4060
      INDGEO = 2                                                        INL 4070
      NDYN = 0                                                          INL 4080
      NREF = 0                                                          INL 4090
      IFTEST = 0                                                        INL 4100
      IREARZ = 1                                                        INL 4110
      REWIND NGEOM                                                      INL 4120
      READ (NGEOM,81) JDUM,JDUM1,JDUM2                                  INL 4130
      READ (NGEOM,87) (JDUM3(I),RDUM3(I),I=1,JDUM1-1)                   INL 4140
      READ (NGEOM,87) (JDUM3(I),RDUM3(I),I=1,JDUM2)                     INL 4150
      READ (NGEOM,81) JDUM                                              INL 4160
      DO 82 N=1,JDUM2                                                   INL 4170
        READ (NGEOM,83) (JDUM3(I),I=1,JDUM1-1)                          INL 4180
   82 CONTINUE                                                          INL 4190
      READ (NGEOM,81) JDUM                                              INL 4200
      READ (NGEOM,83) ITOT,NTOT,N1,NMAXC,N2                             INL 4210
      READ (NGEOM,84) (DELR(I),MPR(I),I=1,ITOT)                         INL 4220
      READ (NGEOM,86) (DELZ(N),MPZ(N),NOP(N),N=1,NTOT)                  INL 4230
CFZJ023                                                       02.02.04  INL 4240
CFZJ040 Initialization of arrays FFTHX and FFTHXX             12.01.05  INL 4250
  203 CONTINUE                                                          INL 4260
      IF(INTVAL .GT. 1) GOTO 201                                        INL 4270
      DO 202 I=1,IMAZ                                                   INL 4280
        DO 202 N=1,NMAZ                                                 INL 4290
          FFTHXX(I,N) = 0.                                              INL 4300
          FFTHXX(I,N) = FFTHX(I,N)                                      INL 4310
          FFTHX(I,N) = 0.                                               INL 4320
  202 CONTINUE                                                          INL 4330
      DO 200 I=1,IMAZ                                                   INL 4340
        DO 200 N=1,NMAZ                                                 INL 4350
          IF(FFTHXX(I,N) .LE. 0.) GOTO 200                              INL 4360
          FFTHX(I,N+N1-N2) = FFTHXX(I,N) * 3.15576E+07 * FDOSE          INL 4370
  200 CONTINUE                                                          INL 4380
  201 CONTINUE                                                          INL 4390
      IF(IRII .GT. 0) RETURN                                            INL 4400
      PHI0 = 0.                                                         INL 4410
      DO 1001 N=1,NTOT                                                  INL 4420
        IF(NOP(N) .GT. 0) GOTO 1001                                     INL 4430
        NTX = N - 1                                                     INL 4440
        GOTO 1002                                                       INL 4450
 1001 CONTINUE                                                          INL 4460
 1002 CONTINUE                                                          INL 4470
      DO 1003 N=1,NTX                                                   INL 4480
        PHI0 = PHI0 + DELZ(N)                                           INL 4490
 1003 CONTINUE                                                          INL 4500
      PHI0 = -PHI0                                                      INL 4510
C                                                                       INL 4520
      CALL READRZ(DR,RAD,IMAX,RAD0,1,MFR,ITOT,DELR,MPR)                 INL 4530
C                                                                       INL 4540
      CALL READRZ(DPH,PHI,NMAX,PHI0,2,MFZ,NTOT,DELZ,MPZ)                INL 4550
C                                                                       INL 4560
      IF(NREST .EQ. 1) WRITE(IREST) A1,DR,DPH,RAD,PHI,IREARZ,INDGEO,NDYNINL 4570
     1 ,NREF,IFTEST,NTHX                                                INL 4580
   50 CONTINUE                                                          INL 4590
CFZJ042                                                       09.09.05  INL 4600
      IF(NREST .GT. 1) READ(IREST) A1,DR,DPH,RAD,PHI,IREARZ,INDGEO,NDYN,INL 4610
     1 NREF,IFTEST,NTHX                                                 INL 4620
      IF(NDYN .EQ. 0) NDYN = 1                                          INL 4630
      BI = BEZI(INDGEO+1)                                               INL 4640
      BN = BEZN(INDGEO+1)                                               INL 4650
      IF(IFRFI .NE. 0 .OR. IFRFA .NE. 0 .OR. IFRFL .NE. 0 .OR. IFRFR    INL 4660
     1 .NE. 0) IFREF = 1                                                INL 4670
      IM1 = IMAX - 1                                                    INL 4680
      NM1 = NMAX - 1                                                    INL 4690
C                                                                       INL 4700
CFZJ042                                                       09.09.05  INL 4710
      IF(IPRINT .GT. -3) CALL BILD(0,TITLE,CP0)                         INL 4720
C                                                                       INL 4730
      WRITE (6,2001) TITLE                                              INL 4740
      WRITE (6,30) ETHA,MITMAX,ORMIN,OVREL,IKORM,IFREL,IERRM            INL 4750
      IF(IFRSTA .GT. 0) WRITE (6,29) IFRSTA                             INL 4760
CFZJ042                                                       09.09.05  INL 4770
      IF(IFRSTA .LT. 0) WRITE (6,903) IFRSTA                            INL 4780
      IF(IPUN .NE. 0) WRITE (6,902) IPUN,IREST                          INL 4790
CFZJ042                                                       09.09.05  INL 4800
      IF(IFKON .NE. 0) WRITE (6,124) IFKON                              INL 4810
      IF(IFKON .EQ. 1) WRITE (6,121)                                    INL 4820
      IF(IFKON .EQ. -1) WRITE (6,122)                                   INL 4830
      IF(IFKON .EQ. 2) WRITE (6,123)                                    INL 4840
      IF(INDGEO .EQ. 0) WRITE (6,805) INDGEO                            INL 4850
      IF(INDGEO .EQ. 1) WRITE (6,806) INDGEO                            INL 4860
      IF(INDGEO .EQ. 2) WRITE (6,807) INDGEO                            INL 4870
      IF(IFKON .NE. 0 .AND. INDGEO .NE. 2) GOTO 800                     INL 4880
      GOTO 801                                                          INL 4890
  800 WRITE (6,802) INDGEO,IFKON                                        INL 4900
C                                                                       INL 4910
      CALL ABEND(5)                                                     INL 4920
C                                                                       INL 4930
  801 CONTINUE                                                          INL 4940
      IF(IFINST .NE. 0) GOTO 810                                        INL 4950
      WRITE (6,809)                                                     INL 4960
      IF(IFKON .EQ. 0) GOTO 820                                         INL 4970
      WRITE (6,32) NLOOP,TDIFF                                          INL 4980
      IF(IFKON .EQ. -1) GOTO 820                                        INL 4990
      WRITE (6,33)                                                      INL 5000
C                                                                       INL 5010
      CALL ABEND(2)                                                     INL 5020
C                                                                       INL 5030
      IFKON = -1                                                        INL 5040
      GOTO 820                                                          INL 5050
  810 WRITE (6,811) INTVAL,DTVOR                                        INL 5060
      IF(ITIK(10) .EQ. 3) GOTO 820                                      INL 5070
      WRITE (6,812)                                                     INL 5080
      D1 = 0.                                                           INL 5090
      IF(DZEIT(1) .GT. 0.) GOTO 617                                     INL 5100
      WRITE (6,619)                                                     INL 5110
C                                                                       INL 5120
      CALL ABEND(2)                                                     INL 5130
C                                                                       INL 5140
      DZEIT(1) = 1.                                                     INL 5150
  617 CONTINUE                                                          INL 5160
      DO 813 I=1,INTVAL                                                 INL 5170
        IF(I .GT. 1) D1 = ZEI(I-1)                                      INL 5180
        IF(D1 .LT. ZEI(I)) GOTO 907                                     INL 5190
        WRITE (6,909) I                                                 INL 5200
C                                                                       INL 5210
        CALL ABEND(3)                                                   INL 5220
C                                                                       INL 5230
  907   IF(DZEIT(I) .EQ. 0.) GOTO 908                                   INL 5240
        D1 = (ZEI(I)-D1) / 3600.                                        INL 5250
        D1 = ABS(D1/DZEIT(I))                                           INL 5260
        IF(D1 .LT. 1000.) GOTO 908                                      INL 5270
        WRITE (6,910) I                                                 INL 5280
C                                                                       INL 5290
        CALL ABEND(2)                                                   INL 5300
C                                                                       INL 5310
  908   CONTINUE                                                        INL 5320
        WRITE (6,814) I,ZEI(I),DZEIT(I),NPRINT(I),NKONV(I)              INL 5330
  813 CONTINUE                                                          INL 5340
  820 CONTINUE                                                          INL 5350
      ICONVC = IFKON                                                    INL 5360
      IF(INDGEO .NE. 1) GOTO 315                                        INL 5370
      DO 310 N=1,NM1                                                    INL 5380
        PHI(N) = PHI(N) * PI / 180.                                     INL 5390
        DPH(N) = DPH(N) * PI / 180.                                     INL 5400
  310 CONTINUE                                                          INL 5410
      PHI(NMAX) = PHI(NMAX) * PI / 180.                                 INL 5420
  315 CONTINUE                                                          INL 5430
      NQVAR = 0                                                         INL 5440
C                                                                       INL 5450
C     ZONENEIGENSCHAFT                                                  INL 5460
C                                                                       INL 5470
      IFTKV = 0                                                         INL 5480
      IFDF = 0                                                          INL 5490
      IFWKPT = 0                                                        INL 5500
      IFLAMT = 0                                                        INL 5510
      NHET = 0                                                          INL 5520
      KS = 0                                                            INL 5530
      M24 = 0                                                           INL 5540
      MM = 1                                                            INL 5550
      K = 0                                                             INL 5560
      KR = 0                                                            INL 5570
CFZJ042                                                       09.09.05  INL 5580
      KTX9Z = 0                                                         INL 5590
   60 CONTINUE                                                          INL 5600
      K = K + 1                                                         INL 5610
      IFHET(K) = 0                                                      INL 5620
      IF(NREST .GT. 1) GOTO 51                                          INL 5630
      NHMAT1(K,1) = 0                                                   INL 5640
C                                                                       INL 5650
CARD TX8                                                                INL 5660
C                                                                       INL 5670
      READ (5,160) BEM(K),K1,IFTV(K),IFWKT(K),IFLT(K),IDIR(K),NTVAR(K), INL 5680
     1 RHO(K),C(K),LAM(K),LAM0,EPS1(K),EPS2(K),R1R2,TVOR(K),WPR(K),     INL 5690
     2 ALP(K),ISTR                                                      INL 5700
C                                                                       INL 5710
      IF(ISTR .EQ. 0) GOTO 319                                          INL 5720
      KR = KR + 1                                                       INL 5730
CFZJ042                                                       09.09.05  INL 5740
      IFZTF(KR) = 0                                                     INL 5750
      IFZST(KR) = 0                                                     INL 5760
C                                                                       INL 5770
CARD TX9                                                                INL 5780
C                                                                       INL 5790
      READ (5,125) IFBQ(KR),IFBR(KR),PVOR(KR),XKON(KR),ALPHA(KR),       INL 5800
     1 DHYD(KR),STZUK(KR),TFLVOR(KR),IFZST1                             INL 5810
C                                                                       INL 5820
      IF(STZUK(KR) .GE. 0.) GOTO 317                                    INL 5830
      IFZST(KR) = 2                                                     INL 5840
      STZUK(KR) = 0.                                                    INL 5850
  317 CONTINUE                                                          INL 5860
      ITK(K) = KR                                                       INL 5870
      ITKK(K) = KR                                                      INL 5880
      EPSIL(KR) = 1. - RHO(K)                                           INL 5890
  319 CONTINUE                                                          INL 5900
      EPS(K) = 0.                                                       INL 5910
      IF(EPS1(K)+EPS2(K) .LE. 0.) GOTO 320                              INL 5920
      IF(EPS1(K) .GT. 0. .AND. EPS2(K) .GT. 0.) GOTO 321                INL 5930
      WRITE (6,7102)                                                    INL 5940
      STOP                                                              INL 5950
 321  CONTINUE                                                          INL 5960
      IF(IDIR(K) .EQ. 1 .OR. IDIR(K) .EQ. 11) R1R2 = 1.                 INL 5970
      EPS(K)= 2. / (1./EPS1(K)+R1R2*(1./EPS2(K)-1.)+1.)                 INL 5980
 320  CONTINUE                                                          INL 5990
      IFANIS(K) = 0                                                     INL 6000
      DOS(K) = 0.                                                       INL 6010
      IREF = 0                                                          INL 6020
      IF(LAM0 .GT. 0.) LAM(K) = LAM0                                    INL 6030
      IF(NREST .EQ. 1) WRITE (IREST) BEM(K),K1,IFTV(K),LAM(K),ALP(K),   INL 6040
     1 TVOR(K),WPR(K),RHO(K),C(K),DOS(K),NTVAR(K),IREF,IFWKT(K),IFLT(K),INL 6050
     2 EPS(K),IFANIS(K),IDIR(K),EPS1(K),EPS2(K)                         INL 6060
   51 CONTINUE                                                          INL 6070
      IF(NREST .GT. 1) READ (IREST) BEM(K),K1,IFTV(K),LAM(K),ALP(K),    INL 6080
     1 TVOR(K),WPR(K),RHO(K),C(K),DOS(K),NTVAR(K),IREF,IFWKT(K),IFLT(K),INL 6090
     2 EPS(K),IFANIS(K),IDIR(K),EPS1(K),EPS2(K)                         INL 6100
      IF(EPS(K) .GT. 0.) IPRS = 1                                       INL 6110
      IF(NREST .LE. 1 .OR. K1 .EQ. 0) GOTO 401                          INL 6120
CFZJ042                                                       09.09.05  INL 6130
      DO 400 I=1,5                                                      INL 6140
        IF(ICODEF(I) .NE. K1) GOTO 400                                  INL 6150
C                                                                       INL 6160
C     POSSIBLE REDEFINITION OF CARD TX8 AND TX9 IN A TRANSIENT RUN      INL 6170
C                                                                       INL 6180
CARD TX8                                                                INL 6190
C                                                                       INL 6200
        READ (5,160) BEM(K),K1,IFTV(K),IFWKT(K),IFLT(K),IDIR(K),NTVAR(K)INL 6210
     1   ,RHO(K),C(K),LAM(K),LAM0,EPS1(K),EPS2(K),R1R2,TVOR(K),WPR(K),  INL 6220
     2   ALP(K),ISTR                                                    INL 6230
C                                                                       INL 6240
        IF(ISTR .EQ. 0) GOTO 402                                        INL 6250
        KR = ITKK(K)                                                    INL 6260
C                                                                       INL 6270
CARD TX9                                                                INL 6280
C                                                                       INL 6290
        READ (5,125) IFBQ(KR),IFBR(KR),PVOR(KR),XKON(KR),ALPHA(KR),     INL 6300
     1   DHYD(KR),STZUK(KR),TFLVOR(KR),IFZST1                           INL 6310
C                                                                       INL 6320
        KTX9Z = KTX9Z + 1                                               INL 6330
        KTX9(KTX9Z) = KR                                                INL 6340
        EPSIL(KR) = 1. - RHO(K)                                         INL 6350
        IFZST(KR) = 0                                                   INL 6360
        IFZTF(KR) = 0                                                   INL 6370
        IF(IFZST1 .NE. 1) GOTO 318                                      INL 6380
        IFZST(KR) = 1                                                   INL 6390
        IFZTF(KR) = 1                                                   INL 6400
  318   CONTINUE                                                        INL 6410
        WRITE (44) IFBQ(KR),IFBR(KR),PVOR(KR),XKON(KR),ALPHA(KR),       INL 6420
     1   DHYD(KR),IFZST(KR),IFZTF(KR),EPSIL(KR)                         INL 6430
  402   CONTINUE                                                        INL 6440
        EPS(K) = 0.                                                     INL 6450
        IF(EPS1(K)+EPS2(K) .LE. 0.) GOTO 500                            INL 6460
        IF(EPS1(K) .GT. 0. .AND. EPS2(K) .GT. 0.) GOTO 501              INL 6470
        WRITE (6,7102)                                                  INL 6480
        STOP                                                            INL 6490
  501   CONTINUE                                                        INL 6500
        IF(IDIR(K) .EQ. 1 .OR. IDIR(K) .EQ. 11) R1R2 = 1.               INL 6510
        EPS(K)= 2. / (1./EPS1(K)+R1R2*(1./EPS2(K)-1.)+1.)               INL 6520
  500   CONTINUE                                                        INL 6530
  400 CONTINUE                                                          INL 6540
  401 CONTINUE                                                          INL 6550
      IF(BEM(K) .EQ. ENDE) GOTO 25                                      INL 6560
      IF(IFLT(K) .EQ. 4 .AND. PSPALT .NE. 0.) LAM(K) = PSPALT           INL 6570
      IF(K .NE. KOMVAR) GOTO 77                                         INL 6580
      TSTAT = TVOR(K)                                                   INL 6590
CFZJ055                                                       25.09.07  INL 6600
   77 CONTINUE                                                          INL 6610
      IF(DOS(K) .LT. 0.) IFDF = 1                                       INL 6620
      IF(IFWKT(K) .EQ. 0) GOTO 61                                       INL 6630
      NRO = NRO + 1                                                     INL 6640
      NRHOC(NRO) = IFWKT(K)                                             INL 6650
   61 IF(IFLT(K) .EQ. 0) GOTO 62                                        INL 6660
      NLA = NLA + 1                                                     INL 6670
      NLAMT(NLA) = IFLT(K)                                              INL 6680
   62 CONTINUE                                                          INL 6690
CFZJ001                                                       28.11.03  INL 6700
      IF(BEM(K) .NE. BV) GOTO 7000                                      INL 6710
      KS = K1                                                           INL 6720
      RHOKS = RHO(K)                                                    INL 6730
      F = RHO(K)                                                        INL 6740
CFZJ001                                                       28.11.03  INL 6750
      NHET = NHET + 1                                                   INL 6760
      IF(NREST .GT. 1) GOTO 52                                          INL 6770
C                                                                       INL 6780
CARD TX10                                                               INL 6790
C                                                                       INL 6800
      READ (5,700) HKUG(K),NHZON(K)                                     INL 6810
C                                                                       INL 6820
      HEPS(K) = 1. - RHO(K)                                             INL 6830
      IF(NREST .EQ. 1) WRITE (IREST) HEPS(K),HKUG(K),NHZON(K)           INL 6840
   52 CONTINUE                                                          INL 6850
      IF(NREST .GT. 1) READ (IREST) HEPS(K),HKUG(K),NHZON(K)            INL 6860
      DKUG = HKUG(K)                                                    INL 6870
      EPSI = HEPS(K)                                                    INL 6880
      NHZ = NHZON(K)                                                    INL 6890
      DO 701 NH=1,NHZ                                                   INL 6900
        IF(NREST .GT. 1) GOTO 53                                        INL 6910
C                                                                       INL 6920
CARD TX11                                                               INL 6930
C                                                                       INL 6940
        READ (5,703) DI(K,NH),NHMAT1(K,NH),NHMAT2(K,NH),XFWQZ(K,NH)     INL 6950
C                                                                       INL 6960
        IF(NREST .EQ. 1) WRITE (IREST) DI(K,NH),NHMAT1(K,NH),           INL 6970
     1   NHMAT2(K,NH),XFWQZ(K,NH)                                       INL 6980
   53   CONTINUE                                                        INL 6990
        IF(NREST .GT. 1) READ (IREST) DI(K,NH),NHMAT1(K,NH),NHMAT2(K,NH)INL 7000
     1   ,XFWQZ(K,NH)                                                   INL 7010
        NLA = NLA + 1                                                   INL 7020
        NRO = NRO + 1                                                   INL 7030
        NLAMT(NLA) = NHMAT1(K,NH)                                       INL 7040
        NRHOC(NRO) = NHMAT2(K,NH)                                       INL 7050
        IF(NLAMT(NLA) .EQ. 24) M24 = 24                                 INL 7060
  701 CONTINUE                                                          INL 7070
      IFHET(K) = 2                                                      INL 7080
 7000 CONTINUE                                                          INL 7090
      IF(BEM(K) .EQ. DB) MM = K1                                        INL 7100
      IF(IFWKT(K) .NE. 0) IFWKPT = 1                                    INL 7110
      IF(NTVAR(K) .EQ. 0 .OR. IFINST .NE. 1) GOTO 75                    INL 7120
      IFTKV = 1                                                         INL 7130
      NTVR = NTVAR(K)                                                   INL 7140
      IF(KOMVAR .EQ. K) GOTO 55                                         INL 7150
      IF(NREST .GT. 1) GOTO 54                                          INL 7160
   55 CONTINUE                                                          INL 7170
C                                                                       INL 7180
CARD TX12                                                               INL 7190
C                                                                       INL 7200
      READ (5,72) (TKV(J,K),ZEIV(J,K),J=1,NTVR)                         INL 7210
C                                                                       INL 7220
      IF(NREST .EQ. 1) WRITE (IREST) (TKV(J,K),ZEIV(J,K),J=1,NTVR)      INL 7230
      GOTO 56                                                           INL 7240
   54 CONTINUE                                                          INL 7250
      IF(NREST .GT. 1) READ (IREST) (TKV(J,K),ZEIV(J,K),J=1,NTVR)       INL 7260
   56 JJ = NTVAR(K)                                                     INL 7270
      NTVAR(K) = NTVAR(K) + 1                                           INL 7280
      DO 73 J1=1,JJ                                                     INL 7290
        IF(ZEIV(J1,K) .EQ. 0.)ZEIV(J1,K) = TVOR(K)                      INL 7300
        TUM(J1) = TKV(J1,K)                                             INL 7310
        ZUM(J1) = ZEIV(J1,K)                                            INL 7320
   73 CONTINUE                                                          INL 7330
      JJ = NTVAR(K)                                                     INL 7340
      ZEIV(1,K) = 0.                                                    INL 7350
      TKV(1,K) = TVOR(K)                                                INL 7360
      DO 74 J1=2,JJ                                                     INL 7370
        TKV(J1,K) = TUM(J1-1)                                           INL 7380
        ZEIV(J1,K) = ZUM(J1-1)                                          INL 7390
   74 CONTINUE                                                          INL 7400
   75 CONTINUE                                                          INL 7410
      IF(K .EQ. K1) GOTO 891                                            INL 7420
      WRITE (6,80) K1                                                   INL 7430
C                                                                       INL 7440
      CALL ABEND(3)                                                     INL 7450
C                                                                       INL 7460
  891 REF(K) = FLOAT(IREF)                                              INL 7470
      IF(IFLT(K) .NE. 0 .OR. EPS(K) .NE. 0.) IFLAMT = 1                 INL 7480
      GOTO 60                                                           INL 7490
   25 CONTINUE                                                          INL 7500
C                                                                       INL 7510
C     ZONENEINTEILUNG                                                   INL 7520
C                                                                       INL 7530
CARDS TX13                                                              INL 7540
C                                                                       INL 7550
      CALL SETK1(DR,DPH,RAD,PHI,B(KX(IIKOM)),B(KX(INKOM)),KOM,B(KX(IR)),INL 7560
     1 B(KX(IZ)),B(KX(IDRK)),B(KX(IDZ)),B(KX(IKON)),MFR,MFZ,ITK,KOC,KG) INL 7570
C                                                                       INL 7580
      KRT = KR + 1                                                      INL 7590
      IF(NREST .EQ. 1) WRITE (IREST) KRT,IPRINT                         INL 7600
      IF(NREST .GT. 1) READ (IREST) KRT,IPRINT                          INL 7610
CFZJ042                                                       09.09.05  INL 7620
      REWIND 44                                                         INL 7630
      ST(1) = 0.                                                        INL 7640
      DO 900 KR=1,KRT                                                   INL 7650
        IF(NREST .EQ. 1) WRITE (IREST) IFBQ(KR),IFBR(KR),IFZST(KR),     INL 7660
     1   IFZTF(KR),PVOR(KR),XKON(KR),ALPHA(KR),DHYD(KR),STZUK(KR),      INL 7670
     2   TFLVOR(KR),EPSIL(KR)                                           INL 7680
        IF(NREST .GT. 1) READ (IREST) IFBQ(KR),IFBR(KR),IFZST(KR),      INL 7690
     1   IFZTF(KR),PVOR(KR),XKON(KR),ALPHA(KR),DHYD(KR),STZUK(KR),      INL 7700
     2   TFLVOR(KR),EPSIL(KR)                                           INL 7710
        ST(1) = ST(1) + STZUK(KR)                                       INL 7720
        IF(NREST .EQ. 1) GOTO 900                                       INL 7730
CFZJ042                                                       09.09.05  INL 7740
        IF(KTX9Z .LE. 0) GOTO 904                                       INL 7750
        DO 901 I=1,KTX9Z                                                INL 7760
          IF(KR .EQ. KTX9(I)) READ (44) IFBQ(KR),IFBR(KR),PVOR(KR),     INL 7770
     1     XKON(KR),ALPHA(KR),DHYD(KR),IFZST(KR),IFZTF(KR),EPSIL(KR)    INL 7780
  901   CONTINUE                                                        INL 7790
  904   CONTINUE                                                        INL 7800
        IF(IFZST(KR) .EQ. 1) GOTO 900                                   INL 7810
        STZUK(KR) = 0.                                                  INL 7820
        TFLVOR(KR) = 0.                                                 INL 7830
  900 CONTINUE                                                          INL 7840
      KMAX = K - 1                                                      INL 7850
      WRITE (6,40) IMAX,NMAX,KMAX,RAD0,PHI0                             INL 7860
CFZJ026                                                       16.03.04  INL 7870
CARD TX14                                                               INL 7880
C                                                                       INL 7890
      IF(IFINST .EQ. 0) READ(5,129) (IYEAR(I),I=1,KMAX)                 INL 7900
C                                                                       INL 7910
      IF(IFINST .EQ. 1) WRITE (6,91) MM                                 INL 7920
      WRITE (6,90)                                                      INL 7930
C                                                                       INL 7940
      CALL PREIN(IFWKT,IFLT,WPR,DOS,RHO,C,LAM,TVOR,ALP,REF,IFTV,KART,EPSINL 7950
     1 ,IDIR,IFANIS,EPS1,EPS2)                                          INL 7960
C                                                                       INL 7970
      IF(NHET .EQ. 0) GOTO 777                                          INL 7980
      WRITE (6,711)                                                     INL 7990
      DO 710 K=1,KMAX                                                   INL 8000
        IF(IFHET(K) .NE. 2) GOTO 710                                    INL 8010
        WRITE (6,712) K,HEPS(K),HKUG(K),NHZON(K)                        INL 8020
        NZ = NHZON(K)                                                   INL 8030
        SUM = 0.                                                        INL 8040
        DO 7100 N=1,NZ                                                  INL 8050
          WRITE (6,713) N,DI(K,N),NHMAT1(K,N),NHMAT2(K,N),XFWQZ(K,N)    INL 8060
          SUM = SUM + XFWQZ(K,N)                                        INL 8070
 7100   CONTINUE                                                        INL 8080
        IF(SUM .GT. 1.01 .OR. SUM .LT. 0.99) GOTO 710                   INL 8090
        WRITE (6,7101)                                                  INL 8100
C                                                                       INL 8110
        CALL ABEND(5)                                                   INL 8120
C                                                                       INL 8130
  710 CONTINUE                                                          INL 8140
  777 CONTINUE                                                          INL 8150
      IF(IFTKV .EQ. 0) GOTO 95                                          INL 8160
      WRITE (6,96)                                                      INL 8170
      DO 97 K=1,KMAX                                                    INL 8180
        IF(NTVAR(K) .EQ. 0) GOTO 97                                     INL 8190
        NTVR = NTVAR(K)                                                 INL 8200
        WRITE (6,98) K,(ZEIV(J,K),TKV(J,K),J=1,NTVR)                    INL 8210
   97 CONTINUE                                                          INL 8220
   95 CONTINUE                                                          INL 8230
      DO 100 N=1,NM1                                                    INL 8240
        PHIP(N) = (PHI(N)+DPH(N)/2.)                                    INL 8250
  100 CONTINUE                                                          INL 8260
      DO 110 I=1,IM1                                                    INL 8270
        IF(INDGEO .EQ. 2) GOTO 101                                      INL 8280
        RADP(I) = RAD(I) + DR(I) / 2.                                   INL 8290
        GOTO 110                                                        INL 8300
  101   RA1 = RAD(I+1)**2.                                              INL 8310
        RA = RAD(I)**2.                                                 INL 8320
        RADP(I) = SQRT((RA1+RA)/2.)                                     INL 8330
  110 CONTINUE                                                          INL 8340
      IF(IFREF .EQ. 0) GOTO 118                                         INL 8350
      WRITE (6,111)                                                     INL 8360
      IF(IFRFA .EQ. 1) WRITE (6,113)                                    INL 8370
      IF(IFRFL .EQ. 1) WRITE (6,114)                                    INL 8380
      IF(IFRFI .EQ. 1) WRITE (6,112)                                    INL 8390
      IF(IFRFR .EQ. 1) WRITE (6,115)                                    INL 8400
      IF(IFRFI .EQ. 1 .AND. INDGEO .EQ. 2 .AND. RAD0 .EQ. 0.) GOTO 1112 INL 8410
      GOTO 118                                                          INL 8420
 1112 IFRFI = 0                                                         INL 8430
      WRITE (6,1114)                                                    INL 8440
C                                                                       INL 8450
      CALL ABEND(1)                                                     INL 8460
C                                                                       INL 8470
      IF(IFRFI .NE. 0 .OR. IFRFA .NE. 0 .OR. IFRFL .NE. 0 .OR. IFRFR    INL 8480
     1 .NE. 0) IFREF = 1                                                INL 8490
  118 CONTINUE                                                          INL 8500
      IFPRNT = 1                                                        INL 8510
      IF(IFTEST .EQ. 1) IFPRNT = 2                                      INL 8520
      IF(IFTEST .EQ. 1) NRO = 100                                       INL 8530
      IF(NRO .EQ. 0) GOTO 69                                            INL 8540
      WRITE (6,631)                                                     INL 8550
   65 CONTINUE                                                          INL 8560
      NM = 1000                                                         INL 8570
      DO 64 N=1,NRO                                                     INL 8580
        NN = NRHOC(N)                                                   INL 8590
        IF(IFTEST .EQ. 1) NN = N                                        INL 8600
        IF(NN .LT. NM .AND. NN .GT. NRMIN) NM = NN                      INL 8610
   64 CONTINUE                                                          INL 8620
      IF(NM .GT. 900) GOTO 63                                           INL 8630
      NRMIN = NM                                                        INL 8640
      IFPRNT = 1                                                        INL 8650
      IF(IFTEST .EQ. 1) IFPRNT = 2                                      INL 8660
C                                                                       INL 8670
      FWKPT = WKPT(D1,NM,IFPRNT)                                        INL 8680
C                                                                       INL 8690
      GOTO 65                                                           INL 8700
   63 CONTINUE                                                          INL 8710
      WRITE (6,639)                                                     INL 8720
      IF(IFPRNT .EQ. -1) GOTO 69                                        INL 8730
      WRITE (6,633)                                                     INL 8740
C                                                                       INL 8750
      CALL ABEND(2)                                                     INL 8760
C                                                                       INL 8770
   69 IF(IFTEST .EQ. 1) NLA = 100                                       INL 8780
      IF(NLA .EQ. 0) GOTO 661                                           INL 8790
      WRITE (6,632)                                                     INL 8800
   68 CONTINUE                                                          INL 8810
      NM = 1000                                                         INL 8820
      DO 67 N=1,NLA                                                     INL 8830
        NN = NLAMT(N)                                                   INL 8840
        IF(IFTEST .EQ. 1) NN = N                                        INL 8850
        IF(NN .LT. NM .AND. NN .GT. NLMIN) NM = NN                      INL 8860
   67 CONTINUE                                                          INL 8870
      IF(NM .GT. 900) GOTO 66                                           INL 8880
      NLMIN = NM                                                        INL 8890
      IFPRNT = 1                                                        INL 8900
      IF(IFTEST .EQ. 1) IFPRNT = 2                                      INL 8910
      TIRR = 950.                                                       INL 8920
CFZJ026 Heat conductivity = f(irradiation temp. TIRR + dose)  15.03.04  INL 8930
C                                                                       INL 8940
      CALL SLAMT(D1,D1,NM,D1,IFPRNT,XLAMT,TIRR)                         INL 8950
C                                                                       INL 8960
      XL = XLAMT                                                        INL 8970
      GOTO 68                                                           INL 8980
   66 CONTINUE                                                          INL 8990
      IF(IFPRNT .EQ. -1) GOTO 661                                       INL 9000
      WRITE (6,634)                                                     INL 9010
C                                                                       INL 9020
      CALL ABEND(2)                                                     INL 9030
C                                                                       INL 9040
  661 IF(IFLAMT .NE. 0) WRITE (6,119) ITLAM                             INL 9050
C                                                                       INL 9060
      CALL MARK(0,KOM,KART)                                             INL 9070
C                                                                       INL 9080
      DO 120 I=1,IM1                                                    INL 9090
        DO 120 N=1,NM1                                                  INL 9100
          IFBER(I,N) = 1                                                INL 9110
          K = KOM(I,N)                                                  INL 9120
          KZ = KART(K)                                                  INL 9130
          WI(I,N) = FLOAT(10*K+KZ) / 10.                                INL 9140
  120 CONTINUE                                                          INL 9150
C                                                                       INL 9160
      CALL PRFELD(WI,IM1,NM1,PKOM,0,PHI,RAD,1)                          INL 9170
C                                                                       INL 9180
      RETURN                                                            INL 9190
      END                                                               INL 9200
      SUBROUTINE READRZ(DX,X,IMAX,X0,KRZ,MF,I2,C,IC)                    REA   10
C                                                                       REA   20
      COMMON /RZCOMP/ IREARZ,MCR,MCZ                                    REA   30
C                                                                       REA   40
CFZJ049                                                       23.04.07  REA   50
      DIMENSION DX(1),X(1),MF(1),IC(200),C(200),R(200)                  REA   60
C                                                                       REA   70
C                                                                       REA   80
      J2 = 0                                                            REA   90
      X(1) = X0                                                         REA  100
CFZJ049                                                       23.04.07  REA  110
      R(1) = 0.                                                         REA  120
      DO 5 I=1,I2                                                       REA  130
        IC(I) = IABS(IC(I))                                             REA  140
CFZJ049                                                       23.04.07  REA  150
        IF(KRZ .EQ. 2) GOTO 1                                           REA  160
        R(I+1) = R(I) + C(I)                                            REA  170
        DF = (R(I+1)**2.-R(I)**2.) / FLOAT(IC(I))                       REA  180
        J1 = J2 + 1                                                     REA  190
        J2 = J1 + IC(I) - 1                                             REA  200
        DO 6 J=J1,J2                                                    REA  210
          X(J+1) = SQRT(X(J)**2.+DF)                                    REA  220
          DX(J) = X(J+1) - X(J)                                         REA  230
    6   CONTINUE                                                        REA  240
        GOTO 7                                                          REA  250
    1   CONTINUE                                                        REA  260
        D = C(I) / FLOAT(IC(I))                                         REA  270
        J1 = J2 + 1                                                     REA  280
        J2 = J1 + IC(I) - 1                                             REA  290
        DO 4 J=J1,J2                                                    REA  300
          DX(J) = D                                                     REA  310
          X(J+1) = X(J) + D                                             REA  320
    4   CONTINUE                                                        REA  330
    7   CONTINUE                                                        REA  340
        MF(I) = J2                                                      REA  350
    5 CONTINUE                                                          REA  360
      IF(KRZ .EQ. 1) MCR = I2                                           REA  370
      IF(KRZ .EQ. 2) MCZ = I2                                           REA  380
      IMAX = J2 + 1                                                     REA  390
      RETURN                                                            REA  400
      END                                                               REA  410
      SUBROUTINE GFIT(D,T,F)                                            GFI   10
C                                                                       GFI   20
C     BERECHNUNG DER BESTRAHLUNGSINDUZIERTEN AENDERUNG DER              GFI   30
C     WAERMELEITFAEHIGKEIT                                              GFI   40
C                                                                       GFI   50
C                                                                       GFI   60
      IF(T .LT. 300.) T = 300.                                          GFI   70
      IF(T .GT. 800.) T = 800.                                          GFI   80
      X = T / 1000.                                                     GFI   90
      Y = D / 10.                                                       GFI  100
      F1 = -0.54705E-02 + 0.38214E-03 * X + 0.13487 * X * X             GFI  110
      F2 = -0.13951E-01 + 0.12064 * X - 0.32955 * X * X                 GFI  120
      F3 = -0.7264E-01 + 0.41459 * X + 0.23149 * X * X                  GFI  130
      F = F1 / (Y-F2) + F3                                              GFI  140
      RETURN                                                            GFI  150
      END                                                               GFI  160
      FUNCTION IPLOG(T,Z,P,IM)                                          IPL   10
C                                                                       IPL   20
C     LOGARARITHMISCHE INTERPOLATION BEI KONSTANTEN WERTEN AUSSERHALB   IPL   30
C     DES DEFINITIONSBEREICHES                                          IPL   40
C                                                                       IPL   50
      DIMENSION Z(1),P(1)                                               IPL   60
C                                                                       IPL   70
      REAL IPLOG,ITPL                                                   IPL   80
C                                                                       IPL   90
CFZJ055                                                       25.09.07  IPL  100
C                                                                       IPL  110
C                                                                       IPL  120
      DO 10 I=1,IM                                                      IPL  130
        I1 = I                                                          IPL  140
        IF(T-Z(I)) 20,20,10                                             IPL  150
   10 CONTINUE                                                          IPL  160
      I2 = IM                                                           IPL  170
      GOTO 40                                                           IPL  180
   20 I2 = I - 1                                                        IPL  190
      IF(I2 .LE. 0) I2 = 1                                              IPL  200
   40 Z1 = Z(I1)                                                        IPL  210
      Z2 = Z(I2)                                                        IPL  220
      P1 = ALOG(P(I1))                                                  IPL  230
      P2 = ALOG(P(I2))                                                  IPL  240
C                                                                       IPL  250
      IPLOG = ITPL(T,Z1,Z2,P1,P2)                                       IPL  260
C                                                                       IPL  270
      IPLOG = EXP(IPLOG)                                                IPL  280
C                                                                       IPL  290
      RETURN                                                            IPL  300
      END                                                               IPL  310
      FUNCTION ITPL(X,X1,X2,Y1,Y2)                                      ITP   10
C                                                                       ITP   20
C     LINEARE INTERPOLATION                                             ITP   30
C                                                                       ITP   40
      REAL ITPL                                                         ITP   50
C                                                                       ITP   60
C                                                                       ITP   70
      IF(X1-X2) 10,20,10                                                ITP   80
C                                                                       ITP   90
   10 ITPL = (X-X1) / (X2-X1) * (Y2-Y1) + Y1                            ITP  100
C                                                                       ITP  110
      RETURN                                                            ITP  120
C                                                                       ITP  130
   20 ITPL = Y1                                                         ITP  140
C                                                                       ITP  150
      RETURN                                                            ITP  160
      END                                                               ITP  170
      SUBROUTINE KONST1(IFGG,IFBER,EPS,IDIR,IFANIS,HKUG,DI,NHZON,IFHET, ONS   10
     1 WWK,DR,DPH,RAD,KOM,ALP,KART,WI,WL,WT,AR,RADP,ISO,ISU,NSL,NSR,KSTRONS   20
     2 ,DOS,ZDOS,IFLT,T,PHI,LAM,DOSI,FFTHX,TCOND)                       ONS   30
C                                                                       ONS   40
C     BERECHNET DIE MASCHEN-WAERMELEITFAEHIGKEITEN UND GEOMETRIE-       ONS   50
C     FUNKTIONEN FUER DIE KUGELZONEN                                    ONS   60
C                                                                       ONS   70
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                ONS   80
C                                                                       ONS   90
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    ONS  100
C                                                                       ONS  110
      COMMON /KOMP1/ KMAX                                               ONS  120
C                                                                       ONS  130
      COMMON /PRINT1/ TITLE(20),INDGEO                                  ONS  140
C                                                                       ONS  150
CFZJ055                                                       25.09.07  ONS  160
C                                                                       ONS  170
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             ONS  180
C                                                                       ONS  190
CFZJ026                                                       16.03.04  ONS  200
      DIMENSION IFBER(IMAZ,NMAZ),EPS(KMAZ),IDIR(KMAZ),IFANIS(KMAZ),     ONS  210
     1 HKUG(KMAZ),DI(KMAZ,5),NHZON(KMAZ),IFHET(KMAZ),WWK(KMAZ,5),       ONS  220
     2 DR(IMAZ),DPH(NMAZ),RAD(IMAZ+1),KOM(IMAZ,NMAZ),ALP(KMAZ),         ONS  230
     3 KART(KMAZ),WI(IMAZ,NMAZ),WL(IMAZ,NMAZ),WT(IMAZ,NMAZ),            ONS  240
     4 AR(IMAZ,NMAZ),RADP(IMAZ),ISO(NMAZ,19),ISU(NMAZ,19),NSL(IMAZ,19), ONS  250
     5 NSR(IMAZ,19),KSTR(KMAZ),DOS(KMAZ),ZDOS(NMAZ),IFLT(KMAZ),         ONS  260
     6 T(IMAZ,NMAZ),PHI(NMAZ+1),DOSI(IMAZ,NMAZ),FFTHX(IMAZ,NMAZ),       ONS  270
     7 TCOND(IMAZ,NMAZ)                                                 ONS  280
C                                                                       ONS  290
      REAL LAM(KMAZ)                                                    ONS  300
C                                                                       ONS  310
    2 FORMAT (/' *** IN SR KONST1 THE THERMAL CONDUCTIVITY (XLIN) IN COMONS  320
     1POSITION',I3,' POSITION I:',I3,' N:',I3,' = 0 (T =',E12.5,') -->  ONS  330
     2STOP. ***')                                                       ONS  340
    3 FORMAT (/' *** IN SR KONST1 THE THERMAL CONDUCTIVITY (XLIN1) IN COONS  350
     1MPOSITION',I3,' POSITION I:',I3,' N:',I3,' = 0 (T =',E12.5,') --> ONS  360
     2 STOP. ***')                                                      ONS  370
 9999 FORMAT (' ***',6I10,4X,4E12.5)                                    ONS  380
C                                                                       ONS  390
C                                                                       ONS  400
      IM1 = IMAX - 1                                                    ONS  410
      NM1 = NMAX - 1                                                    ONS  420
      DO 110 N=1,NM1                                                    ONS  430
        DO 100 I=1,IM1                                                  ONS  440
          IF(RAD(I) .EQ. 0 .AND. INDGEO .EQ. 1) RAD(I) = RAD(I+1) / 10. ONS  450
          K = KOM(I,N)                                                  ONS  460
          NN = N - NDIFF                                                ONS  470
          I1 = KART(K)                                                  ONS  480
          IF1 = IFBER(I,N)                                              ONS  490
          IF1 = IABS(IF1)                                               ONS  500
          IF2 = 0                                                       ONS  510
          IF(I .EQ. IM1) GOTO 11                                        ONS  520
          IF2 = IFBER(I+1,N)                                            ONS  530
          IF2 = IABS(IF2)                                               ONS  540
   11     CONTINUE                                                      ONS  550
          IF(IF1 .LT. 2 .AND. IF2 .LT. 2) GOTO 100                      ONS  560
          IF(I1 .EQ. 2) GOTO 10                                         ONS  570
          I1 = 0                                                        ONS  580
C                                                                       ONS  590
C     FESTSTOFFZONE MIT WAERMEWIEDERSTAND                               ONS  600
C                                                                       ONS  610
CFZJ026                                                       16.03.04  ONS  620
          XLIN = XLAM(I,N,XLAMS,ISO,ISU,NSL,NSR,KSTR,DOS,ZDOS,EPS,IDIR, ONS  630
     1     IFLT,RADP,T,RAD,PHI,KOM,LAM,DOSI,FFTHX,TCOND)                ONS  640
C                                                                       ONS  650
          XLIN1 = XLIN                                                  ONS  660
          IF(XLIN .GT. 0.) GOTO 7                                       ONS  670
C                                                                       ONS  680
C     NEGATIVE WAERMELEITFAEHIGKEIT BEI ANISOTROPER WAERMELEITUNG       ONS  690
C     XLIN1 IN I - RICHTUNG                                             ONS  700
C     XLIN  IN N - RICHTUNG                                             ONS  710
C                                                                       ONS  720
          XLIN = ABS(XLIN)                                              ONS  730
C                                                                       ONS  740
          XLIN1 = XLAM1(I,N,DOS,ZDOS,IFLT,T,KOM,LAM)                    ONS  750
C                                                                       ONS  760
          IF(XLIN .NE. 0.) GOTO 1                                       ONS  770
          WRITE (6,2) K,I,N,T(I,N)                                      ONS  780
          STOP 111                                                      ONS  790
    1     CONTINUE                                                      ONS  800
          IF(XLIN1 .NE. 0.) GOTO 7                                      ONS  810
          WRITE (6,3) K,I,N,T(I,N)                                      ONS  820
          STOP 222                                                      ONS  830
    7     CONTINUE                                                      ONS  840
          IF(EPS(K) .EQ. 0.) GOTO 8                                     ONS  850
          IF(IFANIS(K) .GT. 0) GOTO 9                                   ONS  860
          XLIN = XLIN + XLAMS                                           ONS  870
          XLIN1 = XLIN1 + XLAMS                                         ONS  880
          GOTO 8                                                        ONS  890
    9     CONTINUE                                                      ONS  900
          IF(IDIR(K) .EQ. 0) XLIN1 = XLIN1 + XLAMS                      ONS  910
          IF(IDIR(K) .EQ. 1) XLIN = XLIN + XLAMS                        ONS  920
    8     CONTINUE                                                      ONS  930
          AUIN = DPH(N) * 2. / (XLIN*DR(I))                             ONS  940
          AR(I,N) = DR(I) * 2. / (XLIN1*DPH(N))                         ONS  950
          BUIN = AUIN                                                   ONS  960
          IF(INDGEO-1) 350,12,13                                        ONS  970
   12     CONTINUE                                                      ONS  980
          AR(I,N) = AR(I,N) / DR(I) * ALOG(RAD(I+1)/RAD(I))             ONS  990
          AUIN = AUIN * RAD(I)                                          ONS 1000
          BUIN = BUIN * RAD(I+1)                                        ONS 1010
          GOTO 350                                                      ONS 1020
   13     CONTINUE                                                      ONS 1030
          AUIN = DPH(N) / (XLIN*3.1416*(RADP(I)*RADP(I)-RAD(I)*RAD(I))) ONS 1040
          BUIN = DPH(N) / (XLIN*3.1416*(RAD(I+1)*RAD(I+1)-RADP(I)*      ONS 1050
     1     RADP(I)))                                                    ONS 1060
          IF(RAD(I) .NE. 0.) GOTO 3350                                  ONS 1070
          AR(I,N) = 1. / (6.283*XLIN1*DPH(N))                           ONS 1080
          GOTO 350                                                      ONS 1090
 3350     AR(I,N) = AR(I,N) / DR(I) * ALOG(RAD(I+1)/RAD(I)) / 6.283     ONS 1100
  350     CONTINUE                                                      ONS 1110
          GOTO 20                                                       ONS 1120
   10     CONTINUE                                                      ONS 1130
C                                                                       ONS 1140
C     FLUIDZONE                                                         ONS 1150
C                                                                       ONS 1160
          AUIN = 2. / (ALP(K)*DR(I))                                    ONS 1170
          BUIN = AUIN                                                   ONS 1180
          AR(I,N) = 2. / (ALP(K)*DPH(N))                                ONS 1190
          IF(INDGEO .EQ. 0) GOTO 360                                    ONS 1200
          AR(I,N) = AR(I,N) / RADP(I)                                   ONS 1210
          IF(INDGEO .NE. 2) GOTO 360                                    ONS 1220
          AR(I,N) = AR(I,N) / 6.283                                     ONS 1230
          AUIN = 1. / 3.1415 / (RADP(I)*RADP(I)-RAD(I)*RAD(I)) / ALP(K) ONS 1240
          BUIN = 1. / 3.1415 / (RAD(I+1)*RAD(I+1)-RAD(I)*RAD(I)) /      ONS 1250
     1     ALP(K)                                                       ONS 1260
  360     CONTINUE                                                      ONS 1270
   20     CONTINUE                                                      ONS 1280
          IF(N .EQ. 1) GOTO 50                                          ONS 1290
          IF(IF1 .LT. 3 .OR. IF1 .GT. 6) GOTO 50                        ONS 1300
          K = KOM(I,N-1)                                                ONS 1310
          I2 = KART(K)                                                  ONS 1320
          IF(I2 .NE. 2) I2 = 0                                          ONS 1330
          IF(I1-I2) 54,52,51                                            ONS 1340
   51     WI(I,N) = AR(I,N-1)                                           ONS 1350
          GOTO 53                                                       ONS 1360
   52     A1 = AR(I,N-1)                                                ONS 1370
          A2 = AR(I,N)                                                  ONS 1380
          A3 = A1 + A2                                                  ONS 1390
          IF(A1 .LE. 1.E-20 .OR. A2 .LE. 1.E-20 .OR. A3 .GT. 1.E+20)    ONS 1400
     1     WRITE (6,9999) I,N,K,I1,I2,IF1,A1,A2,A3,XLIN1                ONS 1410
          A1 = A1 / A3                                                  ONS 1420
          WI(I,N) = A1 * A2                                             ONS 1430
          GOTO 53                                                       ONS 1440
   54     WI(I,N) = AR(I,N)                                             ONS 1450
   53     WI(I,N) = 1. / WI(I,N)                                        ONS 1460
   50     CONTINUE                                                      ONS 1470
          IF(I .EQ. 1 .AND. INDGEO .EQ. 2 .AND. RAD0 .EQ. 0.) GOTO 44   ONS 1480
          IF(I .EQ. 1) GOTO 40                                          ONS 1490
          IF(IF1 .EQ. 1 .OR. IF1 .EQ. 4 .OR. IF1 .EQ. 6 .OR. IF1 .EQ. 8)ONS 1500
     1     GOTO 40                                                      ONS 1510
          K = KOM(I-1,N)                                                ONS 1520
          I3 = KART(K)                                                  ONS 1530
          IF(I3 .NE. 2) I3 = 0                                          ONS 1540
          IF(I1-I3) 44,42,41                                            ONS 1550
   41     WL(I,N) = BUIN1                                               ONS 1560
          GOTO 43                                                       ONS 1570
   42     WL(I,N) = (AUIN*BUIN1) / (AUIN+BUIN1)                         ONS 1580
          GOTO 43                                                       ONS 1590
   44     WL(I,N) = AUIN                                                ONS 1600
   43     WL(I,N) = 1. / WL(I,N)                                        ONS 1610
   40     CONTINUE                                                      ONS 1620
          BUIN1 = BUIN                                                  ONS 1630
  100   CONTINUE                                                        ONS 1640
  110 CONTINUE                                                          ONS 1650
      DO 210 I=2,IM1                                                    ONS 1660
        DO 210 N=2,NM1                                                  ONS 1670
          IF(IFBER(I,N) .LE. 0) GOTO 210                                ONS 1680
          WT(I,N) = WI(I,N) + WL(I,N) + WI(I-1,N) + WL(I,N-1)           ONS 1690
  210 CONTINUE                                                          ONS 1700
      DO 209 N=2,NM1                                                    ONS 1710
        WT(1,N) = WI(1,N) + WL(1,N) + WL(1,N-1)                         ONS 1720
  209 CONTINUE                                                          ONS 1730
      IF(IFGG .NE. 1) RETURN                                            ONS 1740
      DO 211 K=1,KMAX                                                   ONS 1750
        IF4 = IFHET(K)                                                  ONS 1760
        IF(IF4 .NE. 2) GOTO 211                                         ONS 1770
        NZ = NHZON(K) - 1                                               ONS 1780
        WWKA = 1. / HKUG(K)                                             ONS 1790
        DO 200 NN=1,NZ                                                  ONS 1800
C                                                                       ONS 1810
C     BERECHNUNG D.W.-LEITF.VON ZONE NN NACH NN+1 DURCH LAMBDA          ONS 1820
C     ERFOLGT KOMP.-WEISE,DA REINE GEOMETRIEFKT.                        ONS 1830
C                                                                       ONS 1840
          WWKI = 1. / DI(K,NN)                                          ONS 1850
          WWK(K,NN) = 6.28 / (WWKI-WWKA)                                ONS 1860
          WWKA = WWKI                                                   ONS 1870
  200   CONTINUE                                                        ONS 1880
  211 CONTINUE                                                          ONS 1890
      RETURN                                                            ONS 1900
      END                                                               ONS 1910
      SUBROUTINE KUEHLK(NPR,QX,IFINST,DTEM1,DR,DPH,RAD,AR,KOM,KART,T,WI,KUE   10
     1 WL,WT,IFBER)                                                     KUE   20
C                                                                       KUE   30
C     ERSTELLT KUEHLKANAL-WAERMEBILANZ                                  KUE   40
C                                                                       KUE   50
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    KUE   60
C                                                                       KUE   70
      COMMON /PRINT1/ TITLE(20),INDGEO                                  KUE   80
C                                                                       KUE   90
      COMMON /KOMP1/ KMAX                                               KUE  100
C                                                                       KUE  110
CFZJ006 enlarged dimensions common QVAR                       28.11.03  KUE  120
      COMMON /QVAR/ DUM(1511),N61,URZ,ZLEKA,ABXEN                       KUE  130
C                                                                       KUE  140
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             KUE  150
C                                                                       KUE  160
      DIMENSION DR(IMAZ),DPH(NMAZ),RAD(IMAZ+1),AR(IMAZ,NMAZ),           KUE  170
     1 KOM(IMAZ,NMAZ),KART(KMAZ),T(IMAZ,NMAZ),WI(IMAZ,NMAZ),            KUE  180
     2 WL(IMAZ,NMAZ),WT(IMAZ,NMAZ),IFBER(IMAZ,NMAZ),QX(KMAZ),WKFL(KMAZ),KUE  190
     3 QP(KMAZ)                                                         KUE  200
C                                                                       KUE  210
      CHARACTER*4 BEZ1(3)/' CM ',' CM ','M**2'/,BEZ2(3)/'W/CM','W/CM',  KUE  220
     1 ' KW '/,BE1,BE2                                                  KUE  230
C                                                                       KUE  240
    8 FORMAT (///10X,'THERMAL BALANCE OF COOLING CHANNELS :'/)          KUE  250
   90 FORMAT (10X,'REGION ',I2,', EFFECTIVE BOUNDARY = ',1PE8.2,' ',A4, KUE  260
     1 ',HEAT FLOW = ',1PE10.3,' ',A4)                                  KUE  270
   91 FORMAT (69X,'----------'/69X,1PE10.3)                             KUE  280
C                                                                       KUE  290
C                                                                       KUE  300
      BE1 = BEZ1(INDGEO+1)                                              KUE  310
      BE2 = BEZ2(INDGEO+1)                                              KUE  320
      IM1 = IMAX - 1                                                    KUE  330
      NM1 = NMAX - 1                                                    KUE  340
      DO 5 K=1,KMAX                                                     KUE  350
        IF(KART(K) .EQ. 2) GOTO 6                                       KUE  360
    5 CONTINUE                                                          KUE  370
      RETURN                                                            KUE  380
    6 IF(NPR .EQ. 0) WRITE (6,8)                                        KUE  390
      IF(IFINST .EQ. 0) GOTO 16                                         KUE  400
      DO 15 I=1,IMAX                                                    KUE  410
        DO 15 N=1,NMAX                                                  KUE  420
          WT(I,N) = (T(I,N)+AR(I,N)) / 2.                               KUE  430
   15 CONTINUE                                                          KUE  440
      GOTO 17                                                           KUE  450
   16 CONTINUE                                                          KUE  460
      DO 18 I=1,IMAX                                                    KUE  470
        DO 18 N=1,NMAX                                                  KUE  480
          WT(I,N) = T(I,N)                                              KUE  490
   18 CONTINUE                                                          KUE  500
   17 CONTINUE                                                          KUE  510
      DO 11 K=1,KMAX                                                    KUE  520
        WKFL(K) = 0.                                                    KUE  530
        QP(K) = 0.                                                      KUE  540
   11 CONTINUE                                                          KUE  550
      DO 20 I=2,IM1                                                     KUE  560
        DO 20 N=2,NM1                                                   KUE  570
          IF(IFBER(I,N) .NE. -1) GOTO 20                                KUE  580
          K1 = KOM(I,N)                                                 KUE  590
          IF(KART(K1) .NE. 2) GOTO 20                                   KUE  600
          K = K1                                                        KUE  610
          FRR = (DR(I)+DR(I-1)) * 0.5                                   KUE  620
          FPP = (DPH(N)+DPH(N-1)) * 0.5                                 KUE  630
          IF(INDGEO .EQ. 1) FPP = FPP * RAD(I)                          KUE  640
          IF(INDGEO .NE. 2) GOTO 100                                    KUE  650
          FPP = FPP * 6.283 * RAD(I)                                    KUE  660
          FRR = FRR * 6.283 * RAD(I)                                    KUE  670
  100     CONTINUE                                                      KUE  680
          TV = WT(I,N)                                                  KUE  690
          IF(IFBER(I,N+1) .LT. 1) GOTO 40                               KUE  700
          QP(K) = QP(K) + WL(I,N) * (TV-WT(I,N+1))                      KUE  710
          WKFL(K) = WKFL(K) + FRR                                       KUE  720
   40     IF(IFBER(I+1,N) .LT. 1) GOTO 30                               KUE  730
          QP(K) = QP(K) + WI(I,N) * (TV-WT(I+1,N))                      KUE  740
          WKFL(K) = WKFL(K) + FPP                                       KUE  750
   30     IF(IFBER(I,N-1) .LT. 1) GOTO 60                               KUE  760
          QP(K) = QP(K) - WL(I,N-1) * (WT(I,N-1)-TV)                    KUE  770
          WKFL(K) = WKFL(K) + FRR                                       KUE  780
   60     IF(IFBER(I-1,N) .LT. 1) GOTO 70                               KUE  790
          QP(K) = QP(K) - WI(I-1,N) * (WT(I-1,N)-TV)                    KUE  800
          WKFL(K) = WKFL(K) + FPP                                       KUE  810
   70     CONTINUE                                                      KUE  820
   20 CONTINUE                                                          KUE  830
      QPSUM = 0.                                                        KUE  840
      DO 10 K=1,KMAX                                                    KUE  850
        IF(KART(K) .NE. 2) GOTO 10                                      KUE  860
        IF(INDGEO .NE. 2) GOTO 21                                       KUE  870
        WKFL(K) = WKFL(K) / 1.E4                                        KUE  880
        QP(K) = QP(K) / 1000.                                           KUE  890
   21   CONTINUE                                                        KUE  900
        IF(NPR .EQ. 0) WRITE (6,90) K,WKFL(K),BE1,QP(K),BE2             KUE  910
        QPSUM = QPSUM + QP(K)                                           KUE  920
   10 CONTINUE                                                          KUE  930
      IF(NPR .EQ. 0) WRITE (6,91) QPSUM                                 KUE  940
      IF(IFINST .NE. 1) GOTO 95                                         KUE  950
      DO 96 K=1,KMAX                                                    KUE  960
        QX(K) = QX(K) + QP(K) * DTEM1                                   KUE  970
   96 CONTINUE                                                          KUE  980
   95 CONTINUE                                                          KUE  990
      RETURN                                                            KUE 1000
      END                                                               KUE 1010
      SUBROUTINE MARK(IND,KOM,KART)                                     MAR   10
C                                                                       MAR   20
C     MARKIERT DIE KOMPOSITIONSGRENZEN BEI GROSSEM GITTER-OUTPUT        MAR   30
C                                                                       MAR   40
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    MAR   50
C                                                                       MAR   60
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             MAR   70
C                                                                       MAR   80
      COMMON /VAIZ/ AIZ                                                 MAR   90
C                                                                       MAR  100
      DIMENSION KOM(IMAZ,NMAZ),KART(KMAZ)                               MAR  110
C                                                                       MAR  120
      CHARACTER*4 ST1/'*  *'/,ST2/'  * '/,STR1/'-  -'/,STR2/'  | '/,    MAR  130
     1 BL/'    '/,STW/'****'/,STS/'  * '/,STRW/'----'/,STRS/'  | '/,    MAR  140
     2 SYM1,SYM11,SYM2,SYM21,AIZ(100,2,200,2)                           MAR  150
C                                                                       MAR  160
C                                                                       MAR  170
      IM1 = IMAX - 1                                                    MAR  180
      NM1 = NMAX - 1                                                    MAR  190
      DO 5 I=1,IMAX                                                     MAR  200
        DO 5 I1 =1,2                                                    MAR  210
          DO 5 N=1,NMAX                                                 MAR  220
            DO 5 N1=1,2                                                 MAR  230
              AIZ(I,I1,N,N1) = BL                                       MAR  240
    5 CONTINUE                                                          MAR  250
      DO 200 I=1,IM1                                                    MAR  260
        DO 200 N=1,NM1                                                  MAR  270
          K1 = KOM(I,N)                                                 MAR  280
          IF(I .EQ. 1) GOTO 100                                         MAR  290
          K2 = KOM(I-1,N)                                               MAR  300
          IF(K1 .EQ. K2) GOTO 100                                       MAR  310
          IF(KART(K1) .NE. 2 .AND. KART(K2) .NE. 2) GOTO 40             MAR  320
          SYM1 = ST1                                                    MAR  330
          SYM11 = STW                                                   MAR  340
          GOTO 50                                                       MAR  350
   40     SYM1 = STR1                                                   MAR  360
          SYM11 = STRW                                                  MAR  370
   50     CONTINUE                                                      MAR  380
          IF(IND .EQ. 1) GOTO 60                                        MAR  390
          AIZ(I-1,2,N,2) = SYM1                                         MAR  400
          GOTO 100                                                      MAR  410
   60     AIZ(I,1,N,1) = SYM11                                          MAR  420
  100     IF(N .EQ. 1) GOTO 200                                         MAR  430
          K3 = KOM(I,N-1)                                               MAR  440
          IF(K1 .EQ. K3) GOTO 200                                       MAR  450
          IF(KART(K1) .NE. 2 .AND. KART(K3) .NE. 2) GOTO 140            MAR  460
          SYM2 = ST2                                                    MAR  470
          SYM21 = STS                                                   MAR  480
          GOTO 150                                                      MAR  490
  140     SYM2 = STR2                                                   MAR  500
          SYM21 = STRS                                                  MAR  510
  150     CONTINUE                                                      MAR  520
          IF(IND .EQ. 1) GOTO 160                                       MAR  530
          AIZ(I,2,N,1) = SYM2                                           MAR  540
          AIZ(I,1,N,1) = SYM2                                           MAR  550
          GOTO 200                                                      MAR  560
  160     AIZ(I,2,N,2) = SYM21                                          MAR  570
          AIZ(I+1,1,N,2) = SYM21                                        MAR  580
  200 CONTINUE                                                          MAR  590
      RETURN                                                            MAR  600
      END                                                               MAR  610
      SUBROUTINE PRAIZ(IMAX,NMAX,KOM,KART)                              PRA   10
C                                                                       PRA   20
      COMMON /PRINT1/ TITLE(20),INDGEO                                  PRA   30
C                                                                       PRA   40
      COMMON /BEZEI/ BI,BN                                              PRA   50
C                                                                       PRA   60
      COMMON /VAIZ/ AIZ                                                 PRA   70
C                                                                       PRA   80
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             PRA   90
C                                                                       PRA  100
      DIMENSION KOM(IMAZ,NMAZ),KART(KMAZ)                               PRA  110
C                                                                       PRA  120
      CHARACTER*4 AIZ(100,2,200,2)                                      PRA  130
C                                                                       PRA  140
C                                                                       PRA  150
      CALL MARK(1,KOM,KART)                                             PRA  160
C                                                                       PRA  170
      WRITE (1) BI,BN                                                   PRA  180
      WRITE (1) ((((AIZ(J1,J2,J3,J4),J1=1,IMAX),J2=1,2),J3=1,NMAX),J4=1,PRA  190
     1 2)                                                               PRA  200
      RETURN                                                            PRA  210
      END                                                               PRA  220
      SUBROUTINE PREIN(IFWKT,IFLT,WPR,DOS,RHO,C,LAM,TVOR,ALP,REF,IFTV,  PRE   10
     1 KART,EPS,IDIR,IFANIS,EPS1,EPS2)                                  PRE   20
C                                                                       PRE   30
C     KONTROLLE UND AUSGABE DER KOMPOSITIONSEIGENSCHAFTEN               PRE   40
C                                                                       PRE   50
      COMMON /KOMP1/ KMAX                                               PRE   60
C                                                                       PRE   70
      COMMON /VAIZ/ AIZ,BEM                                             PRE   80
C                                                                       PRE   90
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             PRE  100
C                                                                       PRE  110
      DIMENSION IFWKT(KMAZ),IFLT(KMAZ),WPR(KMAZ),DOS(KMAZ),RHO(KMAZ),   PRE  120
     1 C(KMAZ),TVOR(KMAZ),ALP(KMAZ),REF(KMAZ),IFTV(KMAZ),KART(KMAZ),    PRE  130
     2 EPS(KMAZ),IDIR(KMAZ),IFANIS(KMAZ),EPS1(1000),EPS2(1000)          PRE  140
C                                                                       PRE  150
      REAL LAM(KMAZ)                                                    PRE  160
C                                                                       PRE  170
      CHARACTER*3 BEM(1000)                                             PRE  180
C                                                                       PRE  190
      CHARACTER*4 AIZ(100,2,200,2)                                      PRE  200
C                                                                       PRE  210
  110 FORMAT (1X,'**ERROR** LAMDA(',I3,') IS 0')                        PRE  220
  130 FORMAT (1X,'**ERROR** ALPHA(',I3,' ) IS NOT 0')                   PRE  230
  209 FORMAT (1H+,T71,F7.4)                                             PRE  240
  210 FORMAT (5X,I2,3X,A3,2X,'SOLID-MAT.ZONE',5X,F7.2,9X,F10.4,21X,F7.4,PRE  250
     1 2X,F7.4,9X,F5.2,19X,I3)                                          PRE  260
  211 FORMAT (5X,I2,3X,A3,2X,'SOLID-MAT.ZONE',5X,F7.2,9X,F10.4,2X,'NO.',PRE  270
     1 I2,14X,F7.4,2X,F7.4,9X,F5.2,19X,I3)                              PRE  280
  212 FORMAT (5X,I2,3X,A3,2X,'SOLID-MAT.ZONE',5X,F7.2,9X,F10.4,2X,'NO.',PRE  290
     1 I2,14X,F7.4,2X,F7.4,2X,'NO.',I2,2X,F5.2,19X,I3)                  PRE  300
  213 FORMAT (5X,I2,3X,A3,2X,'SOLID-MAT.ZONE',5X,F7.2,9X,F10.4,21X,F7.4,PRE  310
     1 2X,F7.4,2X,'NO.',I2,2X,F5.2,19X,I3)                              PRE  320
  220 FORMAT (5X,'REGION',I3,' IS REFLECT. BOUNDARY ZONE')              PRE  330
  230 FORMAT (5X,I2,3X,A3,2X,'FLUID ZONE',9X,F7.2,3X,F7.4,30X,F7.4,2X,  PRE  340
     1 F7.4,34X,I2)                                                     PRE  350
  240 FORMAT (5X,I3,3X,A3,2X,'SOLID-MAT.ZONE',5X,F7.2,11X,F10.4,35X,F5.2PRE  360
     1 ,17X,I3)                                                         PRE  370
  250 FORMAT (5X,I2,3X,A3,2X,'RADIATING GAP    ',2X,F7.2,9X,F10.4,2X,'NOPRE  380
     1.',I2,14X,F7.4,2X,F7.4,9X,F5.2,2X,F3.1,'/',F3.1,4X,2I1,I7)        PRE  390
  251 FORMAT (5X,I2,3X,A3,2X,'RADIATING GAP    ',2X,F7.2,9X,F10.4,2X,'NOPRE  400
     1.',I2,14X,F7.4,2X,F7.4,2X,'NO.',I2,2X,F5.2,2X,F3.1,'/',F3.1,4X,2I1PRE  410
     2 ,I7)                                                             PRE  420
  252 FORMAT (5X,I2,3X,A3,2X,'RADIATING GAP    ',2X,F7.2,9X,F10.4,21X,  PRE  430
     1 F7.4,2X,F7.4,2X,'NO.',I2,2X,F5.2,2X,F3.1,'/',F3.1,4X,2I1,I7)     PRE  440
  260 FORMAT (5X,I2,3X,A3,2X,'RADIATING GAP    ',2X,F7.2,9X,F10.4,21X,  PRE  450
     1 F7.4,2X,F7.4,9X,F5.2,2X,F3.1,'/',F3.1,4X,2I1,I7)                 PRE  460
C                                                                       PRE  470
C                                                                       PRE  480
      DO 100 K=1,KMAX                                                   PRE  490
        IF(EPS(K) .EQ. 0.) GOTO 30                                      PRE  500
        IF(IFLT(K) .EQ. 0) GOTO 25                                      PRE  510
        IF(IFWKT(K) .NE. 0) GOTO 26                                     PRE  520
        WRITE (6,250) K,BEM(K),TVOR(K),LAM(K),IFLT(K),RHO(K),C(K),WPR(K)PRE  530
     1   ,EPS1(K),EPS2(K),IFANIS(K),IDIR(K),IFTV(K)                     PRE  540
        GOTO 82                                                         PRE  550
   26   WRITE (6,251) K,BEM(K),TVOR(K),LAM(K),IFLT(K),RHO(K),C(K),      PRE  560
     1   IFWKT(K),WPR(K),EPS1(K),EPS2(K),IFANIS(K),IDIR(K),IFTV(K)      PRE  570
        GOTO 82                                                         PRE  580
   25   IF(IFWKT(K) .NE. 0) GOTO 27                                     PRE  590
        WRITE (6,260) K,BEM(K),TVOR(K),LAM(K),RHO(K),C(K),WPR(K),EPS1(K)PRE  600
     1   ,EPS2(K),IFANIS(K),IDIR(K),IFTV(K)                             PRE  610
        GOTO 82                                                         PRE  620
   27   WRITE (6,252) K,BEM(K),TVOR(K),LAM(K),RHO(K),C(K),IFWKT(K),     PRE  630
     1   WPR(K),EPS1(K),EPS2(K),IFANIS(K),IDIR(K),IFTV(K)               PRE  640
        GOTO 82                                                         PRE  650
   30   IF(REF(K) .NE. 0.) GOTO 20                                      PRE  660
        IF(IFTV(K) .GT. 0) GOTO 10                                      PRE  670
        IF(LAM(K) .NE. 0 .OR. IFLT(K) .NE. 0)  GOTO 41                  PRE  680
        WRITE (6,110) K                                                 PRE  690
C                                                                       PRE  700
        CALL ABEND(3)                                                   PRE  710
C                                                                       PRE  720
   41   IS = 0                                                          PRE  730
   40   IF(ALP(K) .NE. 0) GOTO 60                                       PRE  740
        IF(IS .NE. 0) GOTO 80                                           PRE  750
        IF(IFLT(K) .EQ. 0) GOTO 81                                      PRE  760
        IF(IFWKT(K) .NE. 0) GOTO 83                                     PRE  770
        WRITE (6,211) K,BEM(K),TVOR(K),LAM(K),IFLT(K),RHO(K),C(K),WPR(K)PRE  780
     1   ,IFTV(K)                                                       PRE  790
        GOTO 82                                                         PRE  800
   83   WRITE (6,212) K,BEM(K),TVOR(K),LAM(K),IFLT(K),RHO(K),C(K),      PRE  810
     1   IFWKT(K),WPR(K),IFTV(K)                                        PRE  820
        GOTO 82                                                         PRE  830
   81   IF(IFWKT(K) .NE. 0) GOTO 84                                     PRE  840
        WRITE (6,210) K,BEM(K),TVOR(K),LAM(K),RHO(K),C(K),WPR(K),IFTV(K)PRE  850
        GOTO 82                                                         PRE  860
   84   WRITE (6,213) K,BEM(K),TVOR(K),LAM(K),RHO(K),C(K),IFWKT(K),     PRE  870
     1   WPR(K),IFTV(K)                                                 PRE  880
   82   KART(K) = 3                                                     PRE  890
        GOTO 99                                                         PRE  900
   10   IS = 1                                                          PRE  910
        GOTO 40                                                         PRE  920
   20   WRITE (6,220) K                                                 PRE  930
        KART(K) = 1                                                     PRE  940
        IF(LAM(K) .NE. 0.) GOTO 99                                      PRE  950
        WRITE (6,110) K                                                 PRE  960
C                                                                       PRE  970
        CALL ABEND(3)                                                   PRE  980
C                                                                       PRE  990
        GOTO 99                                                         PRE 1000
   60   IF(IS .NE. 0) GOTO 46                                           PRE 1010
        WRITE (6,130) K                                                 PRE 1020
C                                                                       PRE 1030
        CALL ABEND(3)                                                   PRE 1040
C                                                                       PRE 1050
   46   WRITE (6,230) K,BEM(K),TVOR(K),ALP(K),RHO(K),C(K),IFTV(K)       PRE 1060
        KART(K) = 2                                                     PRE 1070
        GOTO 99                                                         PRE 1080
   80   IF(LAM(K) .NE. 0.) GOTO 44                                      PRE 1090
        WRITE (6,110)                                                   PRE 1100
C                                                                       PRE 1110
        CALL ABEND(3)                                                   PRE 1120
C                                                                       PRE 1130
   44   WRITE (6,240) K,BEM(K),TVOR(K),LAM(K),WPR(K),IFTV(K)            PRE 1140
        KART(K) = 4                                                     PRE 1150
   99   IF(ALP(K) .EQ. 0. .AND. DOS(K) .NE. 0.) WRITE (6,209) DOS(K)    PRE 1160
  100 CONTINUE                                                          PRE 1170
      RETURN                                                            PRE 1180
      END                                                               PRE 1190
      SUBROUTINE PRFELD(FELD,IMAX,NMAX,UEBER,IFVER,PHI,RAD,IPR)         PRF   10
C                                                                       PRF   20
C     AUSGABE DES GROSSEN GITTERS                                       PRF   30
C                                                                       PRF   40
      COMMON /PRINT1/ TITLE(20),INDGEO                                  PRF   50
C                                                                       PRF   60
      COMMON /BEZEI/ BI,BN                                              PRF   70
C                                                                       PRF   80
CFZJ006 enlarged dimensions common QVAR                       28.11.03  PRF   90
      COMMON /QVAR/ DUM(1511),N61,URZ,ZLEKA,ABXEN                       PRF  100
C                                                                       PRF  110
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             PRF  120
C                                                                       PRF  130
CFZJ042                                                       09.09.05  PRF  140
      COMMON /BLF/ IPASS,IDIFFA,IDIFFE,NDIFFA,NDIFFE                    PRF  150
C                                                                       PRF  160
      COMMON /VAIZ/ AIZ                                                 PRF  170
C                                                                       PRF  180
CFZJ042                                                       09.09.05  PRF  190
      COMMON /COUPL/ IPRINT                                             PRF  200
C                                                                       PRF  210
      DIMENSION PHI(NMAZ+1),RAD(IMAZ+1),FELD(IMAZ,NMAZ)                 PRF  220
C                                                                       PRF  230
      CHARACTER*4 AIZ(100,2,200,2),UEBER(12),BI,BN                      PRF  240
C                                                                       PRF  250
   20 FORMAT (1H1,40X,20A4///40X,12A4//)                                PRF  260
   30 FORMAT (//6X,A4,15(F8.2))                                         PRF  270
   35 FORMAT (//9X,A4,15(F8.2,2X))                                      PRF  280
   40 FORMAT (1X,A4//1X,F7.2)                                           PRF  290
   60 FORMAT (1X,7X,3X,15(2X,F6.1))                                     PRF  300
   61 FORMAT (1X,F7.2,2X,15(2X,F6.3))                                   PRF  310
   70 FORMAT (11X,30A4)                                                 PRF  320
   90 FORMAT (1X,F7.2,3X,30A4)                                          PRF  330
  170 FORMAT (8X,12(6X,A4))                                             PRF  340
  175 FORMAT (1X,F7.2,5X,12(F6.1,A4))                                   PRF  350
  176 FORMAT (/)                                                        PRF  360
C                                                                       PRF  370
C                                                                       PRF  380
      IF(IFVER .EQ. 1) GOTO 200                                         PRF  390
      NMAX1 = NMAX + 1                                                  PRF  400
      IF(INDGEO .NE. 1) GOTO 81                                         PRF  410
      DO 80 N=1,NMAX1                                                   PRF  420
        PHI(N) = PHI(N) * 180. / 3.1416                                 PRF  430
   80 CONTINUE                                                          PRF  440
   81 CONTINUE                                                          PRF  450
      IF(IPR .EQ. 2)  GOTO 300                                          PRF  460
CFZJ042                                                       09.09.05  PRF  470
      IF(IPRINT .LE. -3)  GOTO 110                                      PRF  480
      N1 = 1                                                            PRF  490
   10 N2 = N1 + 13                                                      PRF  500
      IF(N2 .GT. NMAX) N2 = NMAX                                        PRF  510
      N3 = N2 + 1                                                       PRF  520
      WRITE (6,20) (TITLE(J),J=1,20),(UEBER(J),J=1,12)                  PRF  530
      WRITE (6,30) BN,(PHI(N),N=N1,N3)                                  PRF  540
      WRITE (6,40) BI,RAD(1)                                            PRF  550
      DO 100 I=1,IMAX                                                   PRF  560
        IP1 = I + 1                                                     PRF  570
        WRITE (6,70) ((AIZ(I,1,N,NX),NX=1,2),N=N1,N2)                   PRF  580
        WRITE (6,60) (FELD(I,N),N=N1,N2)                                PRF  590
        WRITE (6,90) RAD(IP1),((AIZ(I,2,N,NX),NX=1,2),N=N1,N2)          PRF  600
  100 CONTINUE                                                          PRF  610
      IF(N2 .EQ. NMAX) GOTO 110                                         PRF  620
      N1 = N2 + 1                                                       PRF  630
      GOTO 10                                                           PRF  640
  110 CONTINUE                                                          PRF  650
      DO 95 N=1,NMAX1                                                   PRF  660
        IF(INDGEO .EQ. 1) PHI(N) = PHI(N) * 3.1416 / 180.               PRF  670
   95 CONTINUE                                                          PRF  680
      GOTO 350                                                          PRF  690
  300 CONTINUE                                                          PRF  700
      N1 = NDIFFA                                                       PRF  710
  315 N2 = N1 + 13                                                      PRF  720
      IF(N2 .GT. NDIFFE) N2 = NDIFFE                                    PRF  730
CFZJ042                                                       09.09.05  PRF  740
      IF(IPRINT .LE. -3)  GOTO 350                                      PRF  750
      WRITE (6,20) (TITLE(J),J=1,20),(UEBER(J),J=1,12)                  PRF  760
      WRITE (6,30) BN,(PHI(N),N=N1,N2)                                  PRF  770
      WRITE (6,40) BI                                                   PRF  780
      DO 320 I=IDIFFA,IDIFFE                                            PRF  790
        WRITE (6,61) RAD(I),(FELD(I,N),N=N1,N2)                         PRF  800
        WRITE (6,176)                                                   PRF  810
  320 CONTINUE                                                          PRF  820
      IF(N2 .EQ. NDIFFE) GOTO 350                                       PRF  830
      N1 = N2 + 1                                                       PRF  840
      GOTO 315                                                          PRF  850
  350 RETURN                                                            PRF  860
  200 CONTINUE                                                          PRF  870
      DO 180 N=1,NMAX                                                   PRF  880
        IF(INDGEO .EQ. 1) PHI(N) = PHI(N) * 180. / 3.1416               PRF  890
  180 CONTINUE                                                          PRF  900
CFZJ042                                                       09.09.05  PRF  910
      IF(IPRINT .LE. -3)  GOTO 210                                      PRF  920
      N1 = 1                                                            PRF  930
  310 N2 = N1 + 11                                                      PRF  940
      IF(N2 .GT. NMAX) N2 = NMAX                                        PRF  950
      WRITE (6,20) (TITLE(J),J=1,20),(UEBER(J),J=1,12)                  PRF  960
      WRITE (6,35) BN,(PHI(N),N=N1,N2)                                  PRF  970
      WRITE (6,40) BI                                                   PRF  980
      DO 220 I=1,IMAX                                                   PRF  990
        WRITE (6,170) (AIZ(I,1,N,2),N=N1,N2)                            PRF 1000
        WRITE (6,175) RAD(I),(FELD(I,N),AIZ(I,1,N,1),N=N1,N2)           PRF 1010
        WRITE (6,170) (AIZ(I,2,N,2),N=N1,N2)                            PRF 1020
  220 CONTINUE                                                          PRF 1030
      IF(N2 .EQ. NMAX) GOTO 210                                         PRF 1040
      N1 = N2 + 1                                                       PRF 1050
      GOTO 310                                                          PRF 1060
  210 CONTINUE                                                          PRF 1070
      DO 190 N=1,NMAX                                                   PRF 1080
        IF(INDGEO .EQ. 1) PHI(N) = PHI(N) * 3.1416 / 180.               PRF 1090
  190 CONTINUE                                                          PRF 1100
      RETURN                                                            PRF 1110
      END                                                               PRF 1120
      SUBROUTINE PRINTT(ZEITH,ZEITS,DTEM1,FELD,IFPH,RAD,PHI,IFHET,KOM,  RIN   10
     1 THETNEW)                                                         RIN   20
C                                                                       RIN   30
CFZJ042                                                       09.09.05  RIN   40
C                                                                       RIN   50
C     AUSGABE DES KLEINEN GITTERS                                       RIN   60
C                                                                       RIN   70
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                RIN   80
C                                                                       RIN   90
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    RIN  100
C                                                                       RIN  110
      COMMON /PRINT1/ TITLE(20),INDGEO                                  RIN  120
C                                                                       RIN  130
      COMMON /BEZEI/ BI,BN                                              RIN  140
C                                                                       RIN  150
CFZJ006 enlarged dimensions common QVAR                       28.11.03  RIN  160
      COMMON /QVAR/ DUM(1511),N61,URZ,ZLEKA,ABXEN                       RIN  170
C                                                                       RIN  180
CFZJ042                                                       09.09.05  RIN  190
      COMMON /BLINDL/ TMITL,M24,NGEOM,CIZET0                            RIN  200
C                                                                       RIN  210
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             RIN  220
C                                                                       RIN  230
CFZJ042                                                       09.09.05  RIN  240
      COMMON /BLF/ IPASS                                                RIN  250
C                                                                       RIN  260
      COMMON /BSH/ NHZ                                                  RIN  270
C                                                                       RIN  280
CFZJ042                                                       09.09.05  RIN  290
      COMMON /VRT/ MIXM                                                 RIN  300
C                                                                       RIN  310
CFZJ042                                                       09.09.05  RIN  320
      COMMON /IPRP/ IPR,MIXTH                                           RIN  330
C                                                                       RIN  340
CFZJ042                                                       09.09.05  RIN  350
      COMMON /COUPL/ IPRINT,IPRS                                        RIN  360
C                                                                       RIN  370
CFZJ042                                                       09.09.05  RIN  380
      COMMON /TRANS/ IFINST,INTVAL                                      RIN  390
C                                                                       RIN  400
CFZJ042                                                       09.09.05  RIN  410
      DIMENSION FELD(IMAZ,NMAZ),RAD(IMAZ+1),PHI(NMAZ+1),IFHET(KMAZ),    RIN  420
     1 KOM(IMAZ,NMAZ),TPR(15),THETNEW(ICO,NCO,5,15)                     RIN  430
C                                                                       RIN  440
   34 FORMAT (//7X,A4,17F7.1)                                           RIN  450
   35 FORMAT (//8X,A4,17F8.1)                                           RIN  460
   36 FORMAT (1H1,15X,'TEMPERATURES IN SOLID MATTER AT TIME',I5,' H , ',RIN  470
     1I2 ,' MIN,',F5.1,' S ,  DT=',0PF8.1,' S')                         RIN  480
   37 FORMAT (/F8.2,' Z.1 ',15(F6.1,2X))                                RIN  490
   38 FORMAT (8X,' Z.',I1,1X,15(F6.1,2X))                               RIN  500
   40 FORMAT (2X,A4)                                                    RIN  510
CFZJ042                                                       09.09.05  RIN  520
   41 FORMAT (1H1,10X,'TEMPERATURES OF FUEL ELEMENTS (OUTER BOUNDARY OF RIN  530
     1SHELLS), BATCH NO.',I3)                                           RIN  540
   95 FORMAT (//5X)                                                     RIN  550
  175 FORMAT (1X,F7.1,3X,17F7.1)                                        RIN  560
C                                                                       RIN  570
C                                                                       RIN  580
      IZH = IFIX(ZEITH)                                                 RIN  590
      IZMIN = IFIX((ZEITH-IZH)*60.)                                     RIN  600
      ZS = (ZEITH-IZH) * 3600. - IZMIN * 60.                            RIN  610
CFZJ042                                                       09.09.05  RIN  620
      IPR = IPR + 1                                                     RIN  630
      IF(IPR .LE. 1 .AND. IFINST .EQ. 0) RETURN                         RIN  640
      IF(IPR .GT. 1 .AND. IFINST .EQ. 1) RETURN                         RIN  650
      IF(IPRINT .LE. -3) RETURN                                         RIN  660
      WRITE (6,36) IZH,IZMIN,ZS,DTEM1                                   RIN  670
      IF(INDGEO .NE. 1) GOTO 81                                         RIN  680
      DO 80 N=1,NMAX                                                    RIN  690
        PHI(N) = PHI(N) * 180. / 3.1416                                 RIN  700
   80 CONTINUE                                                          RIN  710
   81 CONTINUE                                                          RIN  720
      I1 = 1                                                            RIN  730
  310 I2 = I1 + 16                                                      RIN  740
      IF(I2 .GT. IMAX) I2 = IMAX                                        RIN  750
      WRITE (6,34) BI,(RAD (I),I=I1,I2)                                 RIN  760
      WRITE (6,40) BN                                                   RIN  770
      DO 220 N=1,NMAX                                                   RIN  780
        WRITE (6,175) PHI(N),(FELD(I,N),I=I1,I2)                        RIN  790
  220 CONTINUE                                                          RIN  800
      IF(I2 .EQ. IMAX) GOTO 210                                         RIN  810
      I1 = I2 + 1                                                       RIN  820
      GOTO 310                                                          RIN  830
  210 CONTINUE                                                          RIN  840
      IF(IFPH .NE. 1) GOTO 211                                          RIN  850
      IF(NMH .LT. 1 .OR. IMH .LT. 1) GOTO 211                           RIN  860
CFZJ042                                                       09.09.05  RIN  870
      MPASSA = 1                                                        RIN  880
      IF(IPASS .GT. 0) MPASSA = IPASS                                   RIN  890
      MPASSE = MIXM                                                     RIN  900
      IF (IPASS .GT. 0) MPASSE = IPASS                                  RIN  910
      DO 222 ID = MPASSA,MPASSE                                         RIN  920
        WRITE (6,41) ID                                                 RIN  930
        N1 = 1                                                          RIN  940
  221   N2 = N1 + 14                                                    RIN  950
        IF(N2 .GT. NMH) N2 = NMH                                        RIN  960
        NA1 = N1 + NDIFF                                                RIN  970
        NA2 = N2 + NDIFF                                                RIN  980
        WRITE (6,35) BN,(PHI(N),N=NA1,NA2)                              RIN  990
        DO 223 I=1,IMH                                                  RIN 1000
          IA = I + IDIFF                                                RIN 1010
          WRITE (6,40) BI                                               RIN 1020
          NN = 0                                                        RIN 1030
CFZJ042                                                       09.09.05  RIN 1040
          DO 900 N=N1,N2                                                RIN 1050
            NN = NN + 1                                                 RIN 1060
            TPR(NN) = THETNEW(I,N,1,ID)                                 RIN 1070
  900     CONTINUE                                                      RIN 1080
          NE = NN                                                       RIN 1090
          WRITE (6,37) RAD(IA),(TPR(NN),NN=1,NE)                        RIN 1100
          DO 223 NZ=2,NHZ                                               RIN 1110
            NN = 0                                                      RIN 1120
CFZJ042                                                       09.09.05  RIN 1130
            DO 902 N=N1,N2                                              RIN 1140
              NN = NN + 1                                               RIN 1150
              TPR(NN) = THETNEW(I,N,NZ,ID)                              RIN 1160
  902       CONTINUE                                                    RIN 1170
            NE = NN                                                     RIN 1180
            WRITE (6,38) NZ,(TPR(NN),NN=1,NE)                           RIN 1190
  223   CONTINUE                                                        RIN 1200
        IF(N2 .EQ. NMH) GOTO 212                                        RIN 1210
        N1 = N2 + 1                                                     RIN 1220
        GOTO 221                                                        RIN 1230
  212   CONTINUE                                                        RIN 1240
  222 CONTINUE                                                          RIN 1250
  211 CONTINUE                                                          RIN 1260
      DO 190 N=1,NMAX                                                   RIN 1270
        IF(INDGEO .EQ. 1) PHI(N) = PHI(N) * 3.1416 / 180.               RIN 1280
  190 CONTINUE                                                          RIN 1290
      WRITE (6,95)                                                      RIN 1300
      RETURN                                                            RIN 1310
      END                                                               RIN 1320