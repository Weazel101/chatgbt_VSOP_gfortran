      SUBROUTINE KOSTUT(TCHG1,N240,NTYP1,ET,ORE1,ORE2,SEP1,SEP2,U3IN,   STU   10
     1 U3EX,U5IN,U5EX,THIN,THEX,U8IN,U8EX,SM1,SM2,U6IN,U6EX,PU39IN,     STU   20
     2 PU39EX,PU40IN,PU40EX,PU41IN,PU41EX,PU42IN,PU42EX,PA33IN,PA33EX,  STU   30
     3 U4IN,U4EX,NP37IN,NP37EX,PU38IN,PU38EX,AM41IN,AM41EX,AM2MIN,AM2MEXSTU   40
     4 ,AM42IN,AM42EX,AM43IN,AM43EX,CM42IN,CM42EX,CM43IN,CM43EX,CM44IN, STU   50
     5 CM44EX)                                                          STU   60
C                                                                       STU   70
C     PRINTS OUTPUT DATA                                                STU   80
C                                                                       STU   90
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    STU  100
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    STU  110
     2 POWES,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PISTU  120
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 STU  130
C                                                                       STU  140
CFZJ043                                                       23.09.05  STU  150
      COMMON /BLOCKK/ JMAF,B,TBURN,AVO,NRCHG,VAUF,NDUM,Z1,Z2,MXTYP,     STU  160
     1 NRTP1(10),NRTP2(10),IALT(10),INEU(10),KSM(10),ETA,IQ,GD,F,GLD,   STU  170
     2 IPRINT,N250,T1,NEXT,DD,TCUM,XLOSS1,NMAF,ND2O,XLOSS2,NCAN,FEED,   STU  180
     3 TAIL,CU3O8,CO8F6,CTRENN,TORE(2,10),TFAB(2,10),TAUF(10),CF,FIF,FITSTU  190
     4 ,UNEED(12),SEP(12),EU5(12),CHIU3(10),CHITH(10),CHIPU(10),CHIU(10)STU  200
     5 ,CU3(10),CU5(10),CTH(10),CU8(10),CPU(10),CFAB(10),CAUF(10),CHID2OSTU  210
     6 ,CDNEU,CDALT,ZD,SD,VD,YPA,ZL,SS,GMZ,SMOL(10),RES,PUFD,DOUTIN,NULLSTU  220
     7 ,Q,QXQ,QQ,CPUST(10),TIN(2,10),TEX(10),SMTYP(10),SMIN(10),SMEX(10)STU  230
     8 ,POWER,Z3,Z4,Z5,ZIN(2,10),ZFB(2,10),ZEX(10),ZAU(10),ORE(2,10),   STU  240
     9 MZ,NR,JEQQ,XDV,YBV,YPU,YD2O,XFA,YDV,YTH,E,XAU,XTH,YFA,XBAR,YU,BZKSTU  250
     X ,XBV,YAU,USUM,XD2O,BZKD2O,PUSUM,SMTOT,VERL,NEWCO,IESC,TOUT       STU  260
C                                                                       STU  270
      INTEGER*4 Q,QXQ,QQ                                                STU  280
C                                                                       STU  290
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, STU  300
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAS,BRUCH,KUGL,JTYP,KLASSE(10),        STU  310
     2 FIMAKL(20),NOPILE,MREP,MARX(10),NAJB(10),FOJB(10),NFUL(10),NBTOT,STU  320
     3 NB0,NCY                                                          STU  330
C                                                                       STU  340
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300),FABC(10),REPC(10)    STU  350
C                                                                       STU  360
      COMMON /NUCNAM/ T2(200),T3(200)                                   STU  370
C                                                                       STU  380
CFZJ044                                                       26.09.05  STU  390
      COMMON /IFA/ FA0,FA1,FA2,IEND                                     STU  400
C                                                                       STU  410
      EQUIVALENCE(NDUM,MM),(JTPE3,N6)                                   STU  420
C                                                                       STU  430
      CHARACTER*4 BUMOL(8)/'TH  ','THO2','THC ','THC2','U   ','UO2 ',   STU  440
     1 'UC  ','UC2 '/,T2,T3                                             STU  450
C                                                                       STU  460
      DIMENSION TCHG1(N240),NTYP1(N240),ET(MMAF),ORE1(MMAF),ORE2(MMAF), STU  470
     1 SEP1(MMAF),SEP2(MMAF),U3IN(N240),U3EX(N240),U5IN(N240),U5EX(N240)STU  480
     2 ,THIN(N240),THEX(N240),U8IN(N240),U8EX(N240),SM1(N240),SM2(N240),STU  490
     3 U6IN(N240),U6EX(N240),PU39IN(N240),PU39EX(N240),PU40IN(N240),    STU  500
     4 PU40EX(N240),PU41IN(N240),PU41EX(N240),PU42IN(N240),PU42EX(N240),STU  510
     5 PA33IN(N240),PA33EX(N240),U4IN(N240),U4EX(N240),PU38IN(N240),    STU  520
     6 PU38EX(N240),AM41IN(N240),AM41EX(N240),AM2MIN(N240),AM2MEX(N240),STU  530
     7 AM42IN(N240),AM42EX(N240),AM43IN(N240),AM43EX(N240),CM42IN(N240),STU  540
     8 CM42EX(N240),CM43IN(N240),CM43EX(N240),CM44IN(N240),CM44EX(N240) STU  550
C                                                                       STU  560
      REAL*4 NP37IN(N240),NP37EX(N240)                                  STU  570
C                                                                       STU  580
 2000 FORMAT (100X,' CYCLE NO',I4,'  YEAR',F6.2 ///)                    STU  590
 2001 FORMAT (40X,' MASS FLOW BALANCE (KG)     IN-CORE FUEL'//)         STU  600
 2002 FORMAT (//10X,'BATCH NO ',4(I4,'   START',1X,I4,'     END',1X))   STU  610
 2003 FORMAT (//10X,'LENGTH OF OPERATION (DAYS) FOR LAST',I4,' CYCLES'/(STU  620
     1 12X,10F11.1))                                                    STU  630
 2004 FORMAT (//10X,'AGE (FULL-POWER-DAYS) OF FUEL BATCHES'/(12X,10F11.1STU  640
     1 ))                                                               STU  650
 2005 FORMAT (///50X,'COST PARAMETER INPUT DATA'/50X,25('=')////10X,'ANNSTU  660
     1UAL LOAD FACTOR .................................',G15.4/10X,'NET STU  670
     2ELECTRICAL EFFICIENCY OF REACTOR ...............',G15.4/10X,'RESERSTU  680
     3VE FUEL FACTOR ................................',G15.4/10X,'FRACTISTU  690
     4ON OF PA233-U233 DECAY DURING REPROCESSING ...',G15.4/10X,'REPROCESTU  700
     5SSING RECOVERY FACTOR .......................',G15.4/10X,'LENGTH OSTU  710
     6F OUT-OF-PILE STORAGE.......................',G15.4/10X,'IF NEGATISTU  720
     7VE SAME AS CYCLE LENGTH ...................')                     STU  730
 2006 FORMAT (////' FUEL TYPE ...',I2,'......',A4,'...',10X,'REFERENCE ESTU  740
     1NRICHMENT',F10.3,'% U235'/10X,'URANIUM ORE REQUIREMENT       ',   STU  750
     2 G12.5,'KG U/KG U',10X,'SEPARATIVE WORK REQUIREMENT   ',G12.5,'SWUSTU  760
     3 /KG U'/10X,'FUEL FABRICATION COST         ',G12.5,A4,'/KGHM',10X,STU  770
     4 'REPROCESSING COST             ',G12.5,A4,'/KGHM'/10X,'THORIUM-23STU  780
     52 PRICE             ',G12.5,A4,'/  KG',10X,'URANIUM-233 PRICE     STU  790
     6        ',G12.5,A4,'/  KG'/10X,'URANIUM-235 PRICE             ',  STU  800
     7 G12.5, A4,'/  KG',10X,'URANIUM-238 PRICE             ',G12.5,A4, STU  810
     8 '/  KG'/10X,'PLUTONIUM FISSILE PRICE       ',G12.5,A4,'/  KG')   STU  820
 2007 FORMAT (///20X,'INTEREST RATES AND TAXES (PER ANNUM)'//10X,'INTERESTU  830
     1ST ON PRE-IRRADIATION FUEL EXPENDITURES ......',G15.4/10X,'INTERESSTU  840
     2T ON PRE-IRRADIATION FABRICATION EXPENDITURES',G15.4/10X,'INTERESTSTU  850
     3 ON ALL CAPITAL DURING IRRADIATION .........',G15.4/10X,'INTEREST STU  860
     4ON POST-IRRADIATION FUEL CREDITS ..........',G15.4/10X,'INTEREST OSTU  870
     5N POST-IRRADIATION REPROCESSING EXPENDITS.',G15.4/10X,'DISCOUNT RASTU  880
     6TE FOR LEVILIZING ALL ITEMS TO START-UP .',G15.4/10X,'TAX RATE ON STU  890
     7ALL FISSILE MATERIAL ...................',G15.4)                  STU  900
 2008 FORMAT (////20X,'INPUT DATA FOR D2O CALCULATION'/10X,'PRICE FOR NESTU  910
     1W D2O ............',G12.5,A4,'/  KG',10X,'PRICE FOR OLD D2O ......STU  920
     2......',G12.5,A4,'/  KG'/10X,'INTEREST RATE ON INVESTMENTS .',    STU  930
     3 G12.5,' PER ANNUM',10X,'TAX RATE ON INVESTMENTS ......',G12.5,' PSTU  940
     4ER ANNUM'/10X,'ANNUAL D2O LOSS RATE .........',G12.5,' FRACTION', STU  950
     5 10X,'TOTAL D2O INVENTORY ..........',G12.5,'       KG')          STU  960
 2009 FORMAT (40X,' TOTAL FUEL CYCLE          ',G15.5,A4//40X,' TOTAL ELSTU  970
     1ECTRICITY PRODUCED',G15.5,'KWHE')                                 STU  980
 2010 FORMAT (///40X,' PRESENT WORTH FUEL CYCLE EXPENSES AND CREDITS'/  STU  990
     1 40X,' (WORKING CAPITALS INCLUDED)                 :'//40X,' FISSISTU 1000
     2LE MATERIALS COST    ' ,G15.5,A4/40X,' FERTILE MATERIALS COST    'STU 1010
     3 ,G15.5,A4/40X,' FABRICATION COST          ',G15.5,A4/40X,' REPROCSTU 1020
     4ESSING COST         ',G15.5,A4/40X,' PLUTONIUM CREDIT          ', STU 1030
     5 G15.5,A4/40X,' URANIUM CREDIT            ',G15.5,A4)             STU 1040
 2011 FORMAT (///40X,' BREAK-DOWN OF FUEL CYCLE COSTS AND CREDITS  : '//STU 1050
     1 40X,' FISSILE COST          ',G15.5,A4,'/KWH'/40X,' FERTILE COST STU 1060
     2         ',G15.5,A4,'/KWH'/40X,' FABRICATION           ',G15.5,A4,STU 1070
     3 '/KWH'/40X,' REPROCESSING          ',G15.5,A4,'/KWH'/40X,' PLUTONSTU 1080
     4IUM CREDIT      ',G15.5,A4,'/KWH'/40X,' URANIUM CREDIT        ',  STU 1090
     5 G15.5,A4,'/KWH')                                                 STU 1100
 2012 FORMAT (///63X,27('*')/40X,' FUEL CYCLE COST       *',G14.5,A4,'/KSTU 1110
     1WH   *'/63X,27('*'))                                              STU 1120
 2013 FORMAT (40X,' D2O REPLACEMENT COST      ',G15.5,A4/40X,' D2O AMORTSTU 1130
     1IZATION COST     ',G15.5,A4)                                      STU 1140
 2015 FORMAT (10X,'DEPRECIATION FACTOR U233      ',G12.5,' FRACTION',10XSTU 1150
     1 ,'DEPRECIATION FACTOR U235      ',G12.5,' FRACTION'/10X,'DEPRECIASTU 1160
     2TION FACTOR U+TH      ',G12.5,' FRACTION',10X,'DEPRECIATION FACTORSTU 1170
     3 PU-F      ',G12.5,' FRACTION'/10X,'THE FACTORS IN THE LIST ABOVE STU 1180
     4ARE USED TO DETERMINE'/10X,'THE CREDIT FOR DISCHARGED FUEL .......STU 1190
     5.............')                                                   STU 1200
 2016 FORMAT (/63X,27('*')/40X,' FCC WITH D2O AMORT    *',G14.5,A4,'/KWHSTU 1210
     1   *'/63X,27('*'))                                                STU 1220
 2017 FORMAT (40X,' D2O REPLACEMENT       ',G15.5,A4,'/KWH'/40X,' D2O AMSTU 1230
     1ORTIZATION      ',G15.5,A4,'/KWH')                                STU 1240
 2018 FORMAT (10X,'NO OF PAYMENTS OF ELECTRICITY REVENUES .............'STU 1250
     1 ,I11)                                                            STU 1260
 2019 FORMAT (10X,'PA - 233 ',E12.5,7E13.5)                             STU 1270
 2020 FORMAT (10X,'U  - 233 ',E12.5,7E13.5)                             STU 1280
 2021 FORMAT (10X,'U  - 235 ',E12.5,7E13.5)                             STU 1290
 2022 FORMAT (10X,'TH - 232 ',E12.5,7E13.5)                             STU 1300
 2023 FORMAT (///10X,'FUEL TYPE                 ',5I13)                 STU 1310
 2024 FORMAT (10X,'THERMAL REACTOR POWER                (MW)',G15.5/10X,STU 1320
     1 'LENGTH OF CYCLE         (FULL-POWER-DAYS)',G15.5/10X,'CYCLE BURNSTU 1330
     2UP                   (MWD/T.HM)',G15.5)                           STU 1340
 2025 FORMAT (10X,'FUEL TYPE',4(I4,9X,I4,9X))                           STU 1350
 2026 FORMAT (///10X,'HEAVY-METAL IN CORE START OF CYCLE   (KG)',G15.5) STU 1360
 2027 FORMAT (10X,'HEAVY-METAL LOADED      (KG)',6X,5E13.5)             STU 1370
 2028 FORMAT (10X,'HEAVY-METAL DISCHARGED  (KG)',6X,5E13.5)             STU 1380
 2029 FORMAT (10X,'U  - 238 ',E12.5,7E13.5)                             STU 1390
 2030 FORMAT (10X,'TOTAL HM ',E12.5,7E13.5)                             STU 1400
 2031 FORMAT (//40X,' MASS FLOW BALANCE (KG)     OUT-OF-PILE FUEL'//)   STU 1410
 2032 FORMAT (//40X,' MASS FLOW BALANCE (KG)     SCRAP FUEL (REMOVED)'//STU 1420
     1 )                                                                STU 1430
 2033 FORMAT (//25X,78('*')/25X,'*     COST UNIT IS "',A4,'"    ENERGY CSTU 1440
     1OST AS A ',I4,'-TH FRACTION IN "',A4,'"     *'/25X,78('*')//)     STU 1450
 2034 FORMAT (/10X,'LEAD TIME FOR PAYMENT OF URANIUM ORE .........',F8.1STU 1460
     1 ,' DAYS',10X,'FOR INITIAL CORE ...........',F8.1,' DAYS'/10X,'LEASTU 1470
     2D TIME FOR PAYMENT OF ENRICHMENT SERVICE ..',F8.1,' DAYS',10X,'FORSTU 1480
     3 INITIAL CORE ...........',F8.1,' DAYS'/10X,'LEAD TIME FOR PAYMENTSTU 1490
     4 OF FUEL FABRICATION ....',F8.1,' DAYS',10X,'FOR INITIAL CORE ....STU 1500
     5.......',F8.1,' DAYS'/10X,'LAG TIME FOR CREDIT OF DISCHARGED FUEL STU 1510
     6.......',F8.1,' DAYS'/10X,'LAG TIME FOR PAYMENT OF REPROCESSING ..STU 1520
     7.......',F8.1,' DAYS')                                            STU 1530
 2035 FORMAT (10X,'U  - 234 ',E12.5,7E13.5)                             STU 1540
 2036 FORMAT (10X,'U  - 236 ',E12.5,7E13.5)                             STU 1550
 2037 FORMAT (///10X,'NEWCO=0: FINANCING COST OF FRESH FUEL STORE'/10X,'STU 1560
     1IS NEGLECTED IN CALCULATION ...............')                     STU 1570
 2038 FORMAT (///10X,'NEWCO=1: FINANCING COST OF FRESH FUEL STORE'/10X,'STU 1580
     1IS EXPLICITLY ACCOUNTED FOR IN CALCULATION.')                     STU 1590
 2039 FORMAT (10X,'PU - 239 ',E12.5,7E13.5)                             STU 1600
 2040 FORMAT (10X,'PU - 240 ',E12.5,7E13.5)                             STU 1610
 2041 FORMAT (10X,'PU - 241 ',E12.5,7E13.5)                             STU 1620
 2042 FORMAT (10X,'PU - 242 ',E12.5,7E13.5)                             STU 1630
 2043 FORMAT (10X,'NP - 237 ',E12.5,7E13.5)                             STU 1640
 2044 FORMAT (10X,'PU - 238 ',E12.5,7E13.5)                             STU 1650
 2045 FORMAT (10X,'AM - 241 ',E12.5,7E13.5)                             STU 1660
 2046 FORMAT (10X,'AM - 242M',E12.5,7E13.5)                             STU 1670
 2047 FORMAT (10X,'AM - 242 ',E12.5,7E13.5)                             STU 1680
 2048 FORMAT ('1',20X,'DATA FOR URANIUM ORE AND ENRICHMENT'/10X,'ORE PRISTU 1690
     1CE                             ',G15.4,A4,'/LB U3O8'/10X,'CONVERSISTU 1700
     2ON U3O8-UF6 COSTS             ',G15.4,A4,'/KG UMET'/10X,'CONVERSIOSTU 1710
     3N U3O8-UF6 LOSSES            ',G15.4,4X,'FRACTION'/10X,'SEPARATIVESTU 1720
     4 WORK                       ',G15.4,A4,'/KG  SWU'/10X,'FEED ENRICHSTU 1730
     5MENT                       ',G15.4,4X,'FRACTION'/10X,'TAIL ENRICHMSTU 1740
     6ENT                       ',G15.4,4X,'FRACTION'/10X,'CONVERSION UFSTU 1750
     76-UO2 AND FAB.LOSSES     ',G15.4,4X,'FRACTION')                   STU 1760
 2049 FORMAT (//10X,'REQUIREMENTS FOR URANIUM ORE AND SEPARATIVE WORK FOSTU 1770
     1R FUEL LOADED'/10X,'URANIUM ORE .........',G15.6,'KG UNAT'/10X,'SESTU 1780
     2PARATIVE WORK .....',G15.6,'KG SWU'/)                             STU 1790
 2050 FORMAT (//10X,'EQUIVALENT URANIUM ORE AND SEPARATIVE WORK CONTENT STU 1800
     1OF FUEL DISCHARGED'/10X,'URANIUM ORE .........',G15.6,'KG UNAT'/  STU 1810
     2 10X,'SEPARATIVE WORK .....',G15.6,'KG SWU')                      STU 1820
 2052 FORMAT (10X,'AM - 243 ',E12.5,7E13.5)                             STU 1830
 2053 FORMAT (10X,'CM - 242 ',E12.5,7E13.5)                             STU 1840
 2054 FORMAT (10X,'CM - 243 ',E12.5,7E13.5)                             STU 1850
 2055 FORMAT (10X,'CM - 244 ',E12.5,7E13.5)                             STU 1860
C                                                                       STU 1870
C                                                                       STU 1880
      CALL HEAD(2)                                                      STU 1890
C                                                                       STU 1900
      IF(NULL .GT. 0) GOTO 7                                            STU 1910
C                                                                       STU 1920
C     PRINT INPUT AND INITIAL DATA                                      STU 1930
C                                                                       STU 1940
      NULL = 1                                                          STU 1950
      WRITE (N6,2033) Q,QXQ,QQ                                          STU 1960
      WRITE (N6,2005) F,ETA,RES,YPA,VAUF,TOUT                           STU 1970
      WRITE (N6,2007) Z1,Z2,Z4,Z3,Z5,ZL,SS                              STU 1980
      WRITE (N6,2018) MZ                                                STU 1990
      IF(NEWCO .EQ. 0) WRITE (N6,2037)                                  STU 2000
      IF(NEWCO .EQ. 1) WRITE (N6,2038)                                  STU 2010
      IF(NCAN .EQ. 0) GOTO 44                                           STU 2020
      WRITE (N6,2048) CU3O8,Q,CO8F6,Q,XLOSS1,CTRENN,Q,FEED,TAIL,XLOSS2  STU 2030
   44 CONTINUE                                                          STU 2040
      DO 45 K=1,MXTYP                                                   STU 2050
        IF(CFAB(K) .LT. 0) CFAB(K) = FABC(K)                            STU 2060
        IF(CAUF(K) .LT. 0) CAUF(K) = REPC(K)                            STU 2070
        NSM = KSM(K)                                                    STU 2080
        WRITE (N6,2006) K,BUMOL(NSM),EU5(K),UNEED(K),SEP(K),CFAB(K),Q,  STU 2090
     1   CAUF(K),Q,CTH(K),Q,CU3(K),Q,CU5(K),Q,CU8(K),Q,CPU(K),Q         STU 2100
        WRITE (N6,2015) CHIU3(K),CHIU(K),CHITH(K),CHIPU(K)              STU 2110
        WRITE (N6,2034) TORE(2,K),TORE(1,K),TIN(2,K),TIN(1,K),TFAB(2,K),STU 2120
     1   TFAB(1,K),TEX(K),TAUF(K)                                       STU 2130
   45 CONTINUE                                                          STU 2140
      IF(ND2O .NE. 0) WRITE (N6,2008) CDNEU,Q,CDALT,Q,ZD,SD,VD,GD       STU 2150
      RETURN                                                            STU 2160
C                                                                       STU 2170
C     PRINT DATA FOR EACH CYCLE                                         STU 2180
C                                                                       STU 2190
    7 CONTINUE                                                          STU 2200
      WRITE (N6,2000) MM,TCUM                                           STU 2210
      IF(IPRINT .EQ. 0) GOTO 23                                         STU 2220
      IF(N6 .NE. 6) GOTO 23                                             STU 2230
      IF(IPRINT .GT. 1 .AND. IPRINT .NE. MM) GOTO 23                    STU 2240
      NDO = NRCHG                                                       STU 2250
      WRITE (6,2001)                                                    STU 2260
      N2 = 0                                                            STU 2270
   20 N1 = N2 + 1                                                       STU 2280
      N2 = N1 + 3                                                       STU 2290
      IF(N2 .GT. NDO) N2 = NDO                                          STU 2300
      WRITE (6,2002) (N,N,N=N1,N2)                                      STU 2310
      WRITE (6,2025) (NTYP1(N),NTYP1(N),N=N1,N2)                        STU 2320
      WRITE (6,2022) (THIN(N),THEX(N),N=N1,N2)                          STU 2330
      WRITE (6,2019) (PA33IN(N),PA33EX(N),N=N1,N2)                      STU 2340
      WRITE (6,2020) (U3IN(N),U3EX(N),N=N1,N2)                          STU 2350
      WRITE (6,2035) (U4IN(N),U4EX(N),N=N1,N2)                          STU 2360
      WRITE (6,2021) (U5IN(N),U5EX(N),N=N1,N2)                          STU 2370
      WRITE (6,2036) (U6IN(N),U6EX(N),N=N1,N2)                          STU 2380
      WRITE (6,2029) (U8IN(N),U8EX(N),N=N1,N2)                          STU 2390
      WRITE (6,2043) (NP37IN(N),NP37EX(N),N=N1,N2)                      STU 2400
      WRITE (6,2044) (PU38IN(N),PU38EX(N),N=N1,N2)                      STU 2410
      WRITE (6,2039) (PU39IN(N),PU39EX(N),N=N1,N2)                      STU 2420
      WRITE (6,2040) (PU40IN(N),PU40EX(N),N=N1,N2)                      STU 2430
      WRITE (6,2041) (PU41IN(N),PU41EX(N),N=N1,N2)                      STU 2440
      WRITE (6,2042) (PU42IN(N),PU42EX(N),N=N1,N2)                      STU 2450
      WRITE (6,2045) (AM41IN(N),AM41EX(N),N=N1,N2)                      STU 2460
      WRITE (6,2046) (AM2MIN(N),AM2MEX(N),N=N1,N2)                      STU 2470
      WRITE (6,2047) (AM42IN(N),AM42EX(N),N=N1,N2)                      STU 2480
      WRITE (6,2052) (AM43IN(N),AM43EX(N),N=N1,N2)                      STU 2490
      WRITE (6,2053) (CM42IN(N),CM42EX(N),N=N1,N2)                      STU 2500
      WRITE (6,2054) (CM43IN(N),CM43EX(N),N=N1,N2)                      STU 2510
      WRITE (6,2055) (CM44IN(N),CM44EX(N),N=N1,N2)                      STU 2520
      WRITE (6,2030) (SM1(N),SM2(N),N=N1,N2)                            STU 2530
      IF(N2 .LT. NDO) GOTO 20                                           STU 2540
      IF(KUGL .EQ. 0) GOTO 29                                           STU 2550
      IF(N2 .NE. NRCHG) GOTO 28                                         STU 2560
      NDO = NRCHG + NOPILE                                              STU 2570
      WRITE (6,2031)                                                    STU 2580
      GOTO 20                                                           STU 2590
   28 CONTINUE                                                          STU 2600
      IF(N2 .NE. NRCHG+NOPILE) GOTO 29                                  STU 2610
      NDO = NR                                                          STU 2620
      WRITE (6,2032)                                                    STU 2630
      GOTO 20                                                           STU 2640
   29 CONTINUE                                                          STU 2650
C                                                                       STU 2660
      CALL HEAD(2)                                                      STU 2670
C                                                                       STU 2680
   23 CONTINUE                                                          STU 2690
      N1 = -4                                                           STU 2700
   22 N1 = N1 + 5                                                       STU 2710
      N2 = N1 + 4                                                       STU 2720
      IF(N2 .GT. MXTYP) N2 = MXTYP                                      STU 2730
      WRITE (N6,2023) (N,N=N1,N2)                                       STU 2740
      WRITE (N6,2027) (SMIN(N),N=N1,N2)                                 STU 2750
      WRITE (N6,2028) (SMEX(N),N=N1,N2)                                 STU 2760
      IF(N2 .LT. MXTYP) GOTO 22                                         STU 2770
      WRITE (N6,2003) MM,(ET(J),J=1,MM)                                 STU 2780
      IF(IPRIN(2) .GE. 1) WRITE (N6,2004) (TCHG1(N),N=1,NR)             STU 2790
      WRITE (N6,2026) SMTOT                                             STU 2800
      WRITE (N6,2024) POWER,TBURN,B                                     STU 2810
      WRITE (N6,2049) ORE1(MM),SEP1(MM)                                 STU 2820
      WRITE (N6,2050) ORE2(MM),SEP2(MM)                                 STU 2830
C                                                                       STU 2840
      ENTRY FCCUT                                                       STU 2850
C                                                                       STU 2860
      CALL HEAD(2)                                                      STU 2870
C                                                                       STU 2880
      WRITE (N6,2000) MM,TCUM                                           STU 2890
      WRITE (N6,2010) XBV,Q,XTH,Q,XFA,Q,XAU,Q,PUSUM,Q,USUM,Q            STU 2900
      IF(ND2O .NE. 0) WRITE (N6,2013) XDV,Q,XD2O,Q                      STU 2910
      WRITE (N6,2009) XBAR,Q,E                                          STU 2920
      WRITE (N6,2011) YBV,QQ,YTH,QQ,YFA,QQ,YAU,QQ,YPU,QQ,YU,QQ          STU 2930
      IF(ND2O .NE. 0) WRITE (N6,2017) YDV,QQ,YD2O,QQ                    STU 2940
      WRITE (N6,2012) BZK,QQ                                            STU 2950
      IF(ND2O .NE. 0) WRITE (6,2016) BZKD2O,QQ                          STU 2960
      RETURN                                                            STU 2970
      END                                                               STU 2980
      SUBROUTINE PUEQ(UKOST,BZK,CPU,ITPU,Q,QQ,*,*)                      PUE   10
C                                                                       PUE   20
CFZJ055                                                       25.09.07  PUE   30
C                                                                       PUE   40
 2000 FORMAT (///50X,'PLUTONIUM EQUIVALENCE VALUE'//10X,'FCC FOR URANIUMPUE   50
     1 CYCLE .................',G15.5,A4,'/KWH'/10X,'EQUIVALENT PRICE FOPUE   60
     2R PU-FISSILE .......',G15.5,A4,'/ KG')                            PUE   70
 2001 FORMAT (///10('?'),' PU-EQUIVALENCE VALUE NOT FOUND BETWEEN   0. APUE   80
     1ND',G15.5,A4,'/KG   AFTER 20 ITERATIONS ',10('?')//)              PUE   90
 3000 FORMAT (//40X,I5,' ITERATIONS')                                   PUE  100
C                                                                       PUE  110
C                                                                       PUE  120
      IF(CPU .EQ. 0.) RETURN                                            PUE  130
      IF(ITPU .EQ. 0) N = 0                                             PUE  140
      ITPU = 1                                                          PUE  150
      EPS = 0.0001                                                      PUE  160
      N = N + 1                                                         PUE  170
      IF(N .GT. 20) GOTO 5                                              PUE  180
      DIF = UKOST - BZK                                                 PUE  190
      ADIF = ABS(DIF)                                                   PUE  200
      IF(ADIF .LE. EPS) GOTO 3                                          PUE  210
      IF(DIF) 1,3,2                                                     PUE  220
    1 IF(N .EQ. 1) CPUINF = 0.                                          PUE  230
      CPUSUP = CPU                                                      PUE  240
      GOTO 4                                                            PUE  250
    2 IF(N .EQ. 1) CPUSUP = 10. * CPU                                   PUE  260
      CPUINF = CPU                                                      PUE  270
    4 CPU = (CPUSUP-CPUINF) * 0.5 + CPUINF                              PUE  280
      RETURN 1                                                          PUE  290
    3 CONTINUE                                                          PUE  300
      WRITE (6,2000) UKOST,QQ,CPU,Q                                     PUE  310
      WRITE (6,3000) N                                                  PUE  320
      UKOST = 0.                                                        PUE  330
      ITPU = 0                                                          PUE  340
      RETURN 2                                                          PUE  350
    5 WRITE (6,2001) CPUSUP,Q                                           PUE  360
      RETURN                                                            PUE  370
      END                                                               PUE  380
      SUBROUTINE ZINS(Z,T,ZF)                                           ZIN   10
C                                                                       ZIN   20
      REAL*8 DZ,DT,DZF                                                  ZIN   30
C                                                                       ZIN   40
C                                                                       ZIN   50
      R = T / 365.                                                      ZIN   60
      DT = DBLE(R)                                                      ZIN   70
      DZ = DBLE(Z)                                                      ZIN   80
      DZF = (1.D0+DZ)**DT                                               ZIN   90
      ZF = SNGL(DZF)                                                    ZIN  100
      RETURN                                                            ZIN  110
      END                                                               ZIN  120
      SUBROUTINE ZIN1(Z,T,ZF1)                                          IN1   10
C                                                                       IN1   20
      REAL*8 DZ,DT,DZF                                                  IN1   30
C                                                                       IN1   40
C                                                                       IN1   50
      R = T / 365.                                                      IN1   60
      DT = DBLE(R)                                                      IN1   70
      DZ = DBLE(Z)                                                      IN1   80
      DZF = (1.D0+DZ)**DT                                               IN1   90
      ZF1 = SNGL(1.D0-DZF)                                              IN1  100
      RETURN                                                            IN1  110
      END                                                               IN1  120
      SUBROUTINE ENRICO(ENU235,CP,K,NN)                                 ENR   10
C                                                                       ENR   20
C     CALCULATE ENRICHED URANIUM PRICE                                  ENR   30
C                                                                       ENR   40
CFZJ043                                                       23.09.05  ENR   50
      COMMON /BLOCKK/ JMAF,B,TBURN,AVO,NRCHG,VAUF,NDUM,Z1,Z2,MXTYP,     ENR   60
     1 NRTP1(10),NRTP2(10),IALT(10),INEU(10),KSM(10),ETA,IQ,GD,F,GLD,   ENR   70
     2 IPRINT,N250,T1,NEXT,DD,TCUM,XLOSS1,NMAF,ND2O,XLOSS2,NCAN,FEED,   ENR   80
     3 TAIL,CU3O8,CO8F6,CTRENN,TORE(2,10),TFAB(2,10),TAUF(10),CF,FIF,FITENR   90
     4 ,UNEED(12),SEP(12),EU5(12),CHIU3(10),CHITH(10),CHIPU(10),CHIU(10)ENR  100
     5 ,CU3(10),CU5(10),CTH(10),CU8(10),CPU(10),CFAB(10),CAUF(10),CHID2OENR  110
     6 ,CDNEU,CDALT,ZD,SD,VD,YPA,ZL,SS,GMZ,SMOL(10),RES,PUFD,DOUTIN,NULLENR  120
     7 ,Q,QXQ,QQ,CPUST(10),TIN(2,10),TEX(10),SMTYP(10),SMIN(10),SMEX(10)ENR  130
     8 ,POWER,Z3,Z4,Z5,ZIN(2,10),ZFB(2,10),ZEX(10),ZAU(10),ORE(2,10),   ENR  140
     9 MZ,NR,JEQQ,XDV,YBV,YPU,YD2O,XFA,YDV,YTH,E,XAU,XTH,YFA,XBAR,YU,BZKENR  150
     X ,XBV,YAU,USUM,XD2O,BZKD2O,PUSUM,SMTOT,VERL,NEWCO,IESC,TOUT       ENR  160
C                                                                       ENR  170
      INTEGER*4 Q,QXQ,QQ                                                ENR  180
C                                                                       ENR  190
      REAL*8 FEEDD,FIFD,FITD,CFD,ENU23D,Y,UF,X,FIP,A99                  ENR  200
C                                                                       ENR  210
C                                                                       ENR  220
      A99 = 0.9999D0                                                    ENR  230
C                                                                       ENR  240
C     MAKE DOUBLE                                                       ENR  250
C                                                                       ENR  260
      FEEDD = FEED                                                      ENR  270
      FIFD = FIF                                                        ENR  280
      FITD = FIT                                                        ENR  290
      CFD = CF                                                          ENR  300
      ENU23D = ENU235                                                   ENR  310
      IF(CFD .GT. 0.D0) GOTO 100                                        ENR  320
      FEEDD = DMIN1(FEEDD,A99)                                          ENR  330
      FIFD = (2.D0*FEEDD-1.D0) * DLOG(FEEDD/(1.D0-FEEDD))               ENR  340
      FITD = (2.D0*DBLE(TAIL)-1.D0) * DLOG(DBLE(TAIL)/(1.D0-DBLE(TAIL)))ENR  350
      CFD = 842.21D0 / 714.21D0 * 2.20462D0                             ENR  360
  100 CONTINUE                                                          ENR  370
      UF = CFD * DBLE(ORE(NN,K)) + DBLE(CO8F6)                          ENR  380
      Y = (ENU23D-DBLE(TAIL)) / (FEEDD-DBLE(TAIL))                      ENR  390
      IF(Y .LT. 0.D0) Y = 0.D0                                          ENR  400
      ENU23D = DMIN1(ENU23D,A99)                                        ENR  410
      FIP = (2.D0*ENU23D-1.D0) * DLOG(ENU23D/(1.D0-ENU23D))             ENR  420
      X = FIP - Y * FIFD + (Y-1.D0) * FITD                              ENR  430
      IF(X .LT. 0.D0) X = 0.D0                                          ENR  440
      Y = Y / ((1.D0-DBLE(XLOSS1))*(1.D0-DBLE(XLOSS2)))                 ENR  450
      X = X / (1.D0-DBLE(XLOSS2))                                       ENR  460
      CP = DBLE(CTRENN) * X + Y * UF                                    ENR  470
      IF(K .EQ. 0) GOTO 110                                             ENR  480
      UNEED(K) = Y                                                      ENR  490
      SEP(K) = X                                                        ENR  500
  110 CONTINUE                                                          ENR  510
C                                                                       ENR  520
C     STORE SINGLE                                                      ENR  530
C                                                                       ENR  540
      FIF = FIFD                                                        ENR  550
      FIT = FITD                                                        ENR  560
      CF = CFD                                                          ENR  570
      ENU235 = ENU23D                                                   ENR  580
      FEED = FEEDD                                                      ENR  590
      RETURN                                                            ENR  600
      END                                                               ENR  610
      SUBROUTINE AVCOST(XREST,RAUF,OREST,SEPRST,AK,DK,FK,BK,QK,EKWH,ET, AVC   10
     1 ORE1,ORE2,SEP1,SEP2,MMAF)                                        AVC   20
C                                                                       AVC   30
C     CALCULATE TOTAL PRESENT WORTH OF EXPENDITURES AND REVENUES        AVC   40
C     DURING RUNNING-IN, EQUILIBRIUM AND REACTOR LIFETIME               AVC   50
C                                                                       AVC   60
CFZJ043                                                       23.09.05  AVC   70
      COMMON /BLOCKK/ JMAF,B,TBURN,AVO,NRCHG,VAUF,NDUM,Z1,Z2,MXTYP,     AVC   80
     1 NRTP1(10),NRTP2(10),IALT(10),INEU(10),KSM(10),ETA,IQ,GD,F,GLD,   AVC   90
     2 IPRINT,N250,T1,NEXT,DD,TCUM,XLOSS1,NMAF,ND2O,XLOSS2,NCAN,FEED,   AVC  100
     3 TAIL,CU3O8,CO8F6,CTRENN,TORE(2,10),TFAB(2,10),TAUF(10),CF,FIF,FITAVC  110
     4 ,UNEED(12),SEP(12),EU5(12),CHIU3(10),CHITH(10),CHIPU(10),CHIU(10)AVC  120
     5 ,CU3(10),CU5(10),CTH(10),CU8(10),CPU(10),CFAB(10),CAUF(10),CHID2OAVC  130
     6 ,CDNEU,CDALT,ZD,SD,VD,YPA,ZL,SS,GMZ,SMOL(10),RES,PUFD,DOUTIN,NULLAVC  140
     7 ,Q,QXQ,QQ,CPUST(10),TIN(2,10),TEX(10),SMTYP(10),SMIN(10),SMEX(10)AVC  150
     8 ,POWER,Z3,Z4,Z5,ZIN(2,10),ZFB(2,10),ZEX(10),ZAU(10),ORE(2,10),   AVC  160
     9 MZ,NR,JEQQ,XDV,YBV,YPU,YD2O,XFA,YDV,YTH,E,XAU,XTH,YFA,XBAR,YU,BZKAVC  170
     X ,XBV,YAU,USUM,XD2O,BZKD2O,PUSUM,SMTOT,VERL,NEWCO,IESC,TOUT       AVC  180
C                                                                       AVC  190
CFZJ055                                                       25.09.07  AVC  200
C                                                                       AVC  210
      INTEGER*4 Q,QXQ,QQ                                                AVC  220
C                                                                       AVC  230
      DIMENSION AK(MMAF),DK(MMAF),FK(MMAF),BK(MMAF),QK(MMAF),EKWH(MMAF),AVC  240
     1 ET(MMAF),ORE1(MMAF),ORE2(MMAF),SEP1(MMAF),SEP2(MMAF)             AVC  250
C                                                                       AVC  260
 2000 FORMAT (/48('*'),' LIFETIME AVERAGE FUEL CYCLE COST ',48('*')/48  AVC  270
     1 ('*'),' LEVELIZED TO THE REACTOR STARTUP ',48('*')/)             AVC  280
 2001 FORMAT (///10X,'RESULTS FOR ',I4,' RUNNING-IN CYCLES AND',I5,' EQUAVC  290
     1ILIBRIUM CYCLES'//10X,'RUNNING-IN PERIOD                         'AVC  300
     2 ,F14.3,1X,'YEAR'/10X,'EFFECTIVE REACTOR LIFETIME                'AVC  310
     3 ,F14.3,1X,'YEAR'/10X,'TARGET LIFETIME (D2O AMORTIZATION)        'AVC  320
     4 ,F14.3,1X,'YEAR'/10X,'DISCOUNT RATE (FOR ALL COMPONENTS)        'AVC  330
     5 ,F14.3,1X,'P.A.'/10X,'ANNUAL LOAD FACTOR                        'AVC  340
     6 ,F14.3,1X,'----')                                                AVC  350
 2002 FORMAT (///47X,28('*')/10X,'AVERAGE LIFETIME FUEL CYCLE COST     *AVC  360
     1',G15.5,A4,'/KWH   *'/47X,28('*'))                                AVC  370
 2003 FORMAT (///47X,28('*')/10X,'D2O AMORTIZATION COST                *AVC  380
     1',G15.5,A4,'/KWH   *'/10X,'AVERAGE LIFETIME FCC WITH D2O        *'AVC  390
     2 ,G15.5,A4,'/KWH   *'/47X,28('*'))                                AVC  400
 2004 FORMAT ('1',30X,'PRESENT WORTH OF EXPENDITURES AND ENERGY PRODUCTIAVC  410
     1ON DURING RUNNING-IN PERIOD'/30X,78('*')///10X,'FUEL COST ........AVC  420
     2..........................',G12.5,A4/10X,'FABRICATION COST .......AVC  430
     3....................',G12.5,A4/10X,'REPROCESSING COST ............AVC  440
     4..............',G12.5,A4/10X,'D2O LOSSES .........................AVC  450
     5........',G12.5,A4/10X,'D2O AMORTIZATION .........................AVC  460
     6..',G12.5,A4/10X,'ELECTRICITY*MONEY-UNITS ....................',  AVC  470
     7 G12.5,'XKWH'/)                                                   AVC  480
 2005 FORMAT ('1',30X,'PRESENT WORTH OF EXPENDITURES AND ENERGY PRODUCTIAVC  490
     1ON DURING WHOLE REACTOR LIFE'/30X,78('*')///10X,'FUEL COST .......AVC  500
     2...........................',G12.5,A4/10X,'FABRICATION COST ......AVC  510
     3.....................',G12.5,A4/10X,'REPROCESSING COST ...........AVC  520
     4...............',G12.5,A4/10X,'D2O LOSSES ........................AVC  530
     5.........',G12.5,A4/10X,'D2O AMORTIZATION ........................AVC  540
     6...',G12.5,A4/10X,'ELECTRICITY*MONEY-UNITS ....................', AVC  550
     7 G12.5,'XKWH'/10X,'RESTCORE CREDIT CORRECTION .................', AVC  560
     8 G12.5,A4/10X,'RESTCORE REPROCESSING ......................',G12.5AVC  570
     9 ,A4/10X,'TOTAL PRESENT WORTH OF FUEL CYCLE ..........',G12.5,A4/)AVC  580
 2006 FORMAT (///10X,'AVERAGED EQUILIBRIUM COSTS ',I4,' CYCLES'//10X,'CYAVC  590
     1CLE LENGTH.............................',G12.5,4X,'DAYS'/10X,'FUELAVC  600
     2 CYCLE COST..........................',G12.5,A4,'/KWH'/10X,'FCC WIAVC  610
     3TH D2O AMORTIZATION................',G12.5,A4,'/KWH')             AVC  620
 2007 FORMAT (//10X,'BREAK DOWN OF COST COMPONENTS FOR EQUILIBRIUM CYCLEAVC  630
     1'//10X,'FUEL COST ...............................',G12.5,A4,'/KWH'AVC  640
     2 /10X,'FABRICATION COST ........................',G12.5,A4,'/KWH'/AVC  650
     3 10X,'REPROCESSING COST .......................',G12.5,A4,'/KWH'/ AVC  660
     4 10X,'D2O LOSSES ..............................',G12.5,A4,'/KWH'/ AVC  670
     5 10X,'D2O AMORTIZATION.........................',G12.5,A4,'/KWH') AVC  680
 2008 FORMAT (//10X,'AVERAGE FCC DURING RUNNING-IN PERIOD ...',G12.5,A4,AVC  690
     1 '/KWH'/10X,'AVERAGE FCC WITH D2O AMORTIZATION ......',G12.5,A4,'/AVC  700
     2KWH'/)                                                            AVC  710
 2049 FORMAT (//10X,'REQUIREMENTS FOR URANIUM ORE AND SEPARATIVE WORK FOAVC  720
     1R FUEL LOADED'/10X,'URANIUM ORE .........',G15.6,'KG UNAT'/10X,'SEAVC  730
     2PARATIVE WORK .....',G15.6,'KG SWU'/)                             AVC  740
 2050 FORMAT (//10X,'EQUIVALENT URANIUM ORE AND SEPARATIVE WORK CONTENT AVC  750
     1OF FUEL DISCHARGED'/10X,'URANIUM ORE .........',G15.6,'KG UNAT'/  AVC  760
     2 10X,'SEPARATIVE WORK .....',G15.6,'KG SWU')                      AVC  770
C                                                                       AVC  780
C                                                                       AVC  790
C     NO. OF EQUILIBRIUM CYCLES                                         AVC  800
C                                                                       AVC  810
      GLG = 0.                                                          AVC  820
      TTOT = TCUM                                                       AVC  830
      TT = ET(NMAF) / 365.                                              AVC  840
C                                                                       AVC  850
C     AVERAGE EQUILIBRIUM CYCLE OF LAST IQ CYCLES                       AVC  860
C                                                                       AVC  870
      IF(IQ .LE. 0) GOTO 1                                              AVC  880
      TTAV = 0.                                                         AVC  890
      AKAV = 0.                                                         AVC  900
      BKAV = 0.                                                         AVC  910
      DKAV = 0.                                                         AVC  920
      FKAV = 0.                                                         AVC  930
      QKAV = 0.                                                         AVC  940
      EKAV = 0.                                                         AVC  950
      TAU = 0.                                                          AVC  960
      OREAV1 = 0.                                                       AVC  970
      SEPAV1 = 0.                                                       AVC  980
      OREAV2 = 0.                                                       AVC  990
      SEPAV2 = 0.                                                       AVC 1000
      Z = 1.                                                            AVC 1010
      DO 10 I=1,IQ                                                      AVC 1020
        J = NMAF - IQ + I                                               AVC 1030
        TTAV = TTAV + ET(J)                                             AVC 1040
        OREAV1 = OREAV1 + ORE1(J)                                       AVC 1050
        OREAV2 = OREAV2 + ORE2(J)                                       AVC 1060
        SEPAV1 = SEPAV1 + SEP1(J)                                       AVC 1070
        SEPAV2 = SEPAV2 + SEP2(J)                                       AVC 1080
        AKAV = AKAV + AK(J) * Z                                         AVC 1090
        BKAV = BKAV + BK(J) * Z                                         AVC 1100
        DKAV = DKAV + DK(J) * Z                                         AVC 1110
        FKAV = FKAV + FK(J) * Z                                         AVC 1120
        QKAV = QKAV + QK(J) * Z                                         AVC 1130
        EKAV = EKAV + EKWH(J) * Z                                       AVC 1140
        TAU = -TTAV                                                     AVC 1150
C                                                                       AVC 1160
        CALL ZINS(ZL,TAU,Z)                                             AVC 1170
C                                                                       AVC 1180
   10 CONTINUE                                                          AVC 1190
      ORE1(NMAF+1) = OREAV1 / IQ                                        AVC 1200
      ORE2(NMAF+1) = OREAV2 / IQ                                        AVC 1210
      SEP1(NMAF+1) = SEPAV1 / IQ                                        AVC 1220
      SEP2(NMAF+1) = SEPAV2 / IQ                                        AVC 1230
      FAKTOR = 1. - Z                                                   AVC 1240
      TTAV = TTAV / IQ                                                  AVC 1250
      ET(NMAF+1) = TTAV                                                 AVC 1260
      TAU = -TTAV                                                       AVC 1270
C                                                                       AVC 1280
      CALL ZINS(ZL,TAU,Z)                                               AVC 1290
C                                                                       AVC 1300
      FAKTOR = (1.-Z) / FAKTOR                                          AVC 1310
      AK(NMAF+1) = AKAV * FAKTOR                                        AVC 1320
      BK(NMAF+1) = BKAV * FAKTOR                                        AVC 1330
      DK(NMAF+1) = DKAV * FAKTOR                                        AVC 1340
      FK(NMAF+1) = FKAV * FAKTOR                                        AVC 1350
      QK(NMAF+1) = QKAV * FAKTOR                                        AVC 1360
      EKWH(NMAF+1) = EKAV * FAKTOR                                      AVC 1370
      TOT = AK(NMAF+1) + BK(NMAF+1) + DK(NMAF+1) + FK(NMAF+1)           AVC 1380
      FCC0 = TOT / EKWH(NMAF+1)                                         AVC 1390
      FCCD = (TOT+QK(NMAF+1)) / EKWH(NMAF+1)                            AVC 1400
      TT = TTAV / 365.                                                  AVC 1410
    1 TTOT = TTOT + TT                                                  AVC 1420
      GLG = GLG + 1.                                                    AVC 1430
      IF(TTOT .LT. GLD) GOTO 1                                          AVC 1440
      TSUP = TTOT                                                       AVC 1450
      TINF = TTOT - TT                                                  AVC 1460
      TDIF1 = TSUP - GLD                                                AVC 1470
      TDIF2 = GLD - TINF                                                AVC 1480
      IF(TDIF1 .LE. TDIF2) GOTO 2                                       AVC 1490
      TTOT = TINF                                                       AVC 1500
      GLG = GLG - 1.                                                    AVC 1510
    2 CONTINUE                                                          AVC 1520
      LG = GLG                                                          AVC 1530
C                                                                       AVC 1540
      CALL HEAD(2)                                                      AVC 1550
C                                                                       AVC 1560
      WRITE (6,2000)                                                    AVC 1570
      WRITE (6,2001) NMAF,LG,TCUM,TTOT,GLD,ZL,F                         AVC 1580
      IF(IQ .EQ. 0) GOTO 5                                              AVC 1590
      WRITE (6,2006) IQ,TTAV,FCC0,QQ,FCCD,QQ                            AVC 1600
      J = NMAF + 1                                                      AVC 1610
      AKAV = AK(J) / EKWH(J)                                            AVC 1620
      BKAV = BK(J) / EKWH(J)                                            AVC 1630
      DKAV = DK(J) / EKWH(J)                                            AVC 1640
      FKAV = FK(J) / EKWH(J)                                            AVC 1650
      QKAV = QK(J) / EKWH(J)                                            AVC 1660
      WRITE (6,2007) BKAV,QQ,FKAV,QQ,AKAV,QQ,DKAV,QQ,QKAV,QQ            AVC 1670
      WRITE (6,2049) ORE1(J),SEP1(J)                                    AVC 1680
      WRITE (6,2050) ORE2(J),SEP2(J)                                    AVC 1690
C                                                                       AVC 1700
C     PRESENT WORTH OF EXPENDITURES AND REVENUES DURING RUNNING-IN      AVC 1710
C                                                                       AVC 1720
    5 CONTINUE                                                          AVC 1730
      QTOT = 0.                                                         AVC 1740
      ATOT = 0.                                                         AVC 1750
      FTOT = 0.                                                         AVC 1760
      GTOT = 0.                                                         AVC 1770
      DTOT = 0.                                                         AVC 1780
      EPSI = 0.                                                         AVC 1790
      OTOT1 = 0.                                                        AVC 1800
      OTOT2 = 0.                                                        AVC 1810
      STOT1 = 0.                                                        AVC 1820
      STOT2 = 0.                                                        AVC 1830
      TAU = 0.                                                          AVC 1840
      DO 3 I=1,NMAF                                                     AVC 1850
C                                                                       AVC 1860
        CALL ZINS(ZL,TAU,Z)                                             AVC 1870
C                                                                       AVC 1880
        GTOT = GTOT + BK(I) * Z                                         AVC 1890
        ATOT = ATOT + AK(I) * Z                                         AVC 1900
        FTOT = FTOT + FK(I) * Z                                         AVC 1910
        DTOT = DTOT + DK(I) * Z                                         AVC 1920
        QTOT = QTOT + QK(I) * Z                                         AVC 1930
        EPSI = EPSI + EKWH(I) * Z                                       AVC 1940
        OTOT1 = OTOT1 + ORE1(I)                                         AVC 1950
        OTOT2 = OTOT2 + ORE2(I)                                         AVC 1960
        STOT1 = STOT1 + SEP1(I)                                         AVC 1970
        STOT2 = STOT2 + SEP2(I)                                         AVC 1980
        TAU = TAU - ET(I)                                               AVC 1990
    3 CONTINUE                                                          AVC 2000
      WRITE (6,2004) GTOT,Q,FTOT,Q,ATOT,Q,DTOT,Q,QTOT,Q,EPSI            AVC 2010
      FCC0 = (GTOT+ATOT+FTOT+DTOT) / EPSI                               AVC 2020
      FCCD = FCC0 + QTOT / EPSI                                         AVC 2030
      WRITE (6,2008) FCC0,QQ,FCCD,QQ                                    AVC 2040
      WRITE (6,2049) OTOT1,STOT1                                        AVC 2050
      WRITE (6,2050) OTOT2,STOT2                                        AVC 2060
C                                                                       AVC 2070
C     PRESENT WORTH OF EXPENDITURES AND REVENUES IN EQUILIBRIUM         AVC 2080
C                                                                       AVC 2090
      CALL ZINS(ZL,TAU,Z)                                               AVC 2100
C                                                                       AVC 2110
      IF(IQ .GT. 0) NMAF = NMAF + 1                                     AVC 2120
      TT = -GLG * ET(NMAF)                                              AVC 2130
C                                                                       AVC 2140
      CALL ZINS (ZL,TT,Z1)                                              AVC 2150
C                                                                       AVC 2160
      TT = -ET(NMAF)                                                    AVC 2170
C                                                                       AVC 2180
      CALL ZINS (ZL,TT,Z2)                                              AVC 2190
C                                                                       AVC 2200
      FAKTOR = (1.-Z1) / (1.-Z2)                                        AVC 2210
      ATOT = ATOT + AK(NMAF) * Z * FAKTOR                               AVC 2220
      FTOT = FTOT + FK(NMAF) * Z * FAKTOR                               AVC 2230
      GTOT = GTOT + BK(NMAF) * Z * FAKTOR                               AVC 2240
      DTOT = DTOT + DK(NMAF) * Z * FAKTOR                               AVC 2250
      QTOT = QTOT + QK(NMAF) * Z * FAKTOR                               AVC 2260
      EPSI = EPSI + EKWH(NMAF) * Z * FAKTOR                             AVC 2270
      OTOT1 = OTOT1 + ORE1(NMAF) * GLG                                  AVC 2280
      OTOT2 = OTOT2 + ORE2(NMAF) * GLG                                  AVC 2290
      STOT1 = STOT1 + SEP1(NMAF) * GLG                                  AVC 2300
      STOT2 = STOT2 + SEP2(NMAF) * GLG                                  AVC 2310
C                                                                       AVC 2320
C     REST CORE                                                         AVC 2330
C                                                                       AVC 2340
      IF(IQ .LE. 0) GOTO 20                                             AVC 2350
      TT = ET(NMAF-1)                                                   AVC 2360
C                                                                       AVC 2370
      CALL ZINS(Z4,TT,ZFJ)                                              AVC 2380
C                                                                       AVC 2390
      TT = -ET(NMAF)                                                    AVC 2400
C                                                                       AVC 2410
      CALL ZINS(Z4,TT,ZF)                                               AVC 2420
C                                                                       AVC 2430
      XREST = XREST * ZFJ * ZF                                          AVC 2440
      RAUF = RAUF * ZFJ * ZF                                            AVC 2450
   20 CONTINUE                                                          AVC 2460
      TT = TAU - (GLG-1.) * ET(NMAF)                                    AVC 2470
C                                                                       AVC 2480
      CALL ZINS (ZL,TT,Z)                                               AVC 2490
C                                                                       AVC 2500
      PWREST = XREST * Z                                                AVC 2510
      PWRAUF = RAUF * Z                                                 AVC 2520
      PW = GTOT - PWREST + PWRAUF + FTOT + ATOT + DTOT                  AVC 2530
      WRITE (6,2005) GTOT,Q,FTOT,Q,ATOT,Q,DTOT,Q,QTOT,Q,EPSI,PWREST,Q,  AVC 2540
     1 PWRAUF,Q,PW,Q                                                    AVC 2550
      OTOT2 = OTOT2 + OREST                                             AVC 2560
      STOT2 = STOT2 + SEPRST                                            AVC 2570
      WRITE (6,2049) OTOT1,STOT1                                        AVC 2580
      WRITE (6,2050) OTOT2,STOT2                                        AVC 2590
C                                                                       AVC 2600
C     AVERAGE FUEL COST                                                 AVC 2610
C                                                                       AVC 2620
      COSTAV = PW / EPSI                                                AVC 2630
      WRITE (6,2002) COSTAV,QQ                                          AVC 2640
      IF(ND2O .EQ. 0) RETURN                                            AVC 2650
C                                                                       AVC 2660
C     D2O COST                                                          AVC 2670
C                                                                       AVC 2680
      CD2O = QTOT / EPSI                                                AVC 2690
      COSTAV = COSTAV + CD2O                                            AVC 2700
      WRITE (6,2003) CD2O,QQ,COSTAV,QQ                                  AVC 2710
      RETURN                                                            AVC 2720
      END                                                               AVC 2730
      SUBROUTINE FUMMEL(DEN,TCHG2,KRESHZ,LRZN,NTYP2,HMETAV,NCOL,DAI,SGA,UMM   10
     1 SGTR,V1,FKEN,NFUMM,SS)                                           UMM   20
C                                                                       UMM   30
C     F U M M E L   ITERATES ON ATOM DENSITIES TO PERFORM A KEFF SEARCH UMM   40
C                                                                       UMM   50
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    UMM   60
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    UMM   70
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIUMM   80
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 UMM   90
C                                                                       UMM  100
      EQUIVALENCE(JTPE3,NT),(JTPE2,NS)                                  UMM  110
C                                                                       UMM  120
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1UMM  130
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         UMM  140
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    UMM  150
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)UMM  160
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  UMM  170
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    UMM  180
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         UMM  190
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,UMM  200
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            UMM  210
C                                                                       UMM  220
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, UMM  230
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        UMM  240
     2 FIMAKL(20),NOPILE,MREP,MARX(10),NAJB(10),FOJB(10),NFUL(10),NBTOT,UMM  250
     3 NB0,NCY                                                          UMM  260
C                                                                       UMM  270
      COMMON /FLUXN/ D(361),IACT                                        UMM  280
C                                                                       UMM  290
CFZJ055                                                       25.09.07  UMM  300
C                                                                       UMM  310
      COMMON /BUC/ BU(6,200)                                            UMM  320
C                                                                       UMM  330
      CHARACTER*4 BU                                                    UMM  340
C                                                                       UMM  350
CFZJ011 Increase dimensions of arrays SGA, SGTR, FKEN         01.12.03  UMM  360
      DIMENSION IDUM(28),COMDUM(28),IDIT(28),COMPIT(28),DEN(KMAT,N200), UMM  370
     1 TCHG2(N200),KRESHZ(N200),LRZN(N200),NTYP2(N200),DAI(N200),       UMM  380
     2 HMETAV(N200),NCOL(N200),SGA(20,N26),SGTR(20,N26,N26),            UMM  390
     3 V1(201,N26,3),FKEN(20,N26),SS(N26,N200)                          UMM  400
C                                                                       UMM  410
  500 FORMAT (18I4)                                                     UMM  420
  520 FORMAT (4(I6,E12.5))                                              UMM  430
  521 FORMAT (I6,E12.5)                                                 UMM  440
 2001 FORMAT (I8,12X,G20.6)                                             UMM  450
 2003 FORMAT ('1','K-EFF IS ADJUSTED TO ',G14.5/' ITERATION ON MATERIAL UMM  460
     1CONCENTRATION IS PERFORMED IN FOLLOWING BATCHES :'/' BATCH NO ',  UMM  470
     2 10X,' HEAVY METAL DENSITY')                                      UMM  480
 2013 FORMAT ('0','LIST OF MATERIALS USED IN ITERATION SEARCH FOR K-EFF'UMM  490
     1 //' MATERIAL',32X,'FRACTION')                                    UMM  500
CFZJ059                                                       04.11.09  UMM  510
 2014 FORMAT (' ',4A4,18X,G14.5)                                        UMM  520
CFZJ059                                                       04.11.09  UMM  530
 2015 FORMAT ('0',4A4,8X,' USED AS MAKEUP MATERIAL')                    UMM  540
CFZJ059                                                       04.11.09  UMM  550
 2152 FORMAT (///13H THERE IS NO ,4A4)                                  UMM  560
 2504 FORMAT (///31H CONVERGENCY NOT ACHIEVED AFTER,I5,11H ITERATIONS)  UMM  570
 2525 FORMAT (///' ITERATION ',I4,15X,'      BATCH NO ',I4//' MATERIAL',UMM  580
     1 26X,' CONCENTRATION')                                            UMM  590
 2601 FORMAT (/' CONIT2 =',E12.6,21('.'),'K-EFF =',F12.7)               UMM  600
C                                                                       UMM  610
C                                                                       UMM  620
      JARIT = 0                                                         UMM  630
      IF(NRSTRT .EQ. 1 .OR. NRSTRT .EQ. 3) GOTO 2560                    UMM  640
C                                                                       UMM  650
C     K-EFF SEARCH. ITERATE ON NUCLIDE CONCENTRATIONS                   UMM  660
C                                                                       UMM  670
      JS = 0                                                            UMM  680
      JN = 0                                                            UMM  690
C                                                                       UMM  700
CARD R28                                                                UMM  710
C                                                                       UMM  720
      READ (NS,500) JARIT,(NCOL(IR),IR=1,JARIT),ITVAR,IR16              UMM  730
C                                                                       UMM  740
      WRITE (NT,500) JARIT,(NCOL(IR),IR=1,JARIT),ITVAR,IR16             UMM  750
 3001 CONTINUE                                                          UMM  760
      IF(JARIT .LE. 0) GOTO 2560                                        UMM  770
      IF(JS .GT. 0) ITVAR = 0                                           UMM  780
C                                                                       UMM  790
CARD R29                                                                UMM  800
C                                                                       UMM  810
      READ (NS,520) ITMAT,XKEFF,MAKEUP,XTYPE                            UMM  820
C                                                                       UMM  830
      WRITE (NT,520) ITMAT,XKEFF,MAKEUP,XTYPE                           UMM  840
C                                                                       UMM  850
CARD R30                                                                UMM  860
C                                                                       UMM  870
      READ (NS,520) (IDIT(M),COMPIT(M),M=1,ITMAT)                       UMM  880
C                                                                       UMM  890
      WRITE (NT,520) (IDIT(M),COMPIT(M),M=1,ITMAT)                      UMM  900
      IDIT(ITMAT+1) = MAKEUP                                            UMM  910
      IF(XKEFF .GT. 0.) SPECK(1) = XKEFF                                UMM  920
C                                                                       UMM  930
C     XKEFF < 0 : FIND INITIAL K TO GIVE MIN REACTIVITY AT END OF CYCLE UMM  940
C     REALNN(2) : K-EFF AT TIME STEP JNSTOP                             UMM  950
C     REALNN(3) : K-EFF MINIMUM INCL. XE-OVERRIDE I.E. ZKFIND+XROMAX    UMM  960
C                                                                       UMM  970
      IF(REALNN(3) .LE. 0.) REALNN(3) = ZKFIND                          UMM  980
      IF(XKEFF .LT. 0.) SPECK(1) = SPECK(1) - REALNN(2) + REALNN(3) *   UMM  990
     1 ABS(XKEFF)                                                       UMM 1000
      IF(COMPIT(1) .GT. 0.) GOTO 2200                                   UMM 1010
C                                                                       UMM 1020
C     USE PRESENT COMPOSITION DURING ITERATION                          UMM 1030
C                                                                       UMM 1040
      NARIT = NCOL(1)                                                   UMM 1050
      NCX2 = KRESHZ(NARIT)                                              UMM 1060
      NCX = LRZN(NCX2)                                                  UMM 1070
      SUMCOM = 0.                                                       UMM 1080
      DO 2030 M=1,ITMAT                                                 UMM 1090
        MATIT = IDIT(M)                                                 UMM 1100
        SUMCOM = SUMCOM + DEN(MATIT,NCX)                                UMM 1110
 2030 CONTINUE                                                          UMM 1120
      DO 2050 M=1,ITMAT                                                 UMM 1130
        MATIT = IDIT(M)                                                 UMM 1140
        COMPIT(M) = DEN(MATIT,NCX) / SUMCOM                             UMM 1150
 2050 CONTINUE                                                          UMM 1160
 2200 CONTINUE                                                          UMM 1170
      IF(IR16 .EQ. 0) GOTO 2250                                         UMM 1180
      DO 2249 I=1,IR16                                                  UMM 1190
C                                                                       UMM 1200
CARD R31                                                                UMM 1210
C                                                                       UMM 1220
        READ (NS,521) IR,HMETAV(IR)                                     UMM 1230
C                                                                       UMM 1240
        WRITE (NT,521) IR,HMETAV(IR)                                    UMM 1250
 2249 CONTINUE                                                          UMM 1260
 2250 CONTINUE                                                          UMM 1270
      IPRIN2 = IPRIN(2)                                                 UMM 1280
      IPRIN3 = IPRIN(3)                                                 UMM 1290
      IPRIN(2) = 0                                                      UMM 1300
      IPRIN(3) = 0                                                      UMM 1310
      WRITE (NT,2003) SPECK(1)                                          UMM 1320
      DO 2300 I=1,JARIT                                                 UMM 1330
        IR = NCOL(I)                                                    UMM 1340
        WRITE (NT,2001) IR,HMETAV(IR)                                   UMM 1350
        TCHG2(IR) = 0.                                                  UMM 1360
        IF(XTYPE .NE. 0.) NTYP2(IR) = XTYPE                             UMM 1370
 2300 CONTINUE                                                          UMM 1380
      IF(JSMAX) 2550,2550,2551                                          UMM 1390
 2550 JSMAX = 50                                                        UMM 1400
 2551 CONTINUE                                                          UMM 1410
      WRITE (NT,2013)                                                   UMM 1420
      DO 2555 M=1,ITMAT                                                 UMM 1430
        MATIT = IDIT(M)                                                 UMM 1440
CFZJ059                                                       04.11.09  UMM 1450
        WRITE (NT,2014) (BU(N,MATIT),N=1,4),COMPIT(M)                   UMM 1460
 2555 CONTINUE                                                          UMM 1470
      IF(MAKEUP .LE. 0) GOTO 2560                                       UMM 1480
CFZJ059                                                       04.11.09  UMM 1490
      WRITE (NT,2015) (BU(N,MAKEUP),N=1,4)                              UMM 1500
C                                                                       UMM 1510
C     ZERO OUT CONTROL POISON (AND XENON)                               UMM 1520
C                                                                       UMM 1530
 2560 CONTINUE                                                          UMM 1540
      NK = IACT + 3 + NO + NLUM                                         UMM 1550
      NL = NK - 1 + NC                                                  UMM 1560
      DO 2006 IR=1,N200                                                 UMM 1570
        IF(JSER .GT. 0) GOTO 2005                                       UMM 1580
        IF(NXE .EQ. 1 .AND. JARIT .EQ. 0) GOTO 2005                     UMM 1590
        DO 2004 ICC=NK,NL                                               UMM 1600
          DEN(ICC,IR) = 0.                                              UMM 1610
 2004   CONTINUE                                                        UMM 1620
 2005   CONTINUE                                                        UMM 1630
        IF(NXE .EQ. 1) GOTO 2006                                        UMM 1640
        DEN(IACT+1,IR) = 0.                                             UMM 1650
 2006 CONTINUE                                                          UMM 1660
      IF(JARIT .EQ. 0) RETURN                                           UMM 1670
      JS = 0                                                            UMM 1680
      MATIT1 = IDIT(1)                                                  UMM 1690
      NCX1 = 1                                                          UMM 1700
      IT = 0                                                            UMM 1710
      DO 2230 IR1=1,JARIT                                               UMM 1720
        NARIT = NCOL(IR1)                                               UMM 1730
        WRITE (NT,2525) JS,NARIT                                        UMM 1740
        IF(NARIT .GT. 1) NCX1 = KRESHZ(NARIT-1) + 1                     UMM 1750
        NCX2 = KRESHZ(NARIT)                                            UMM 1760
        NCX = LRZN(NCX1)                                                UMM 1770
        DAI(IR1) = DEN(MATIT1,NCX)                                      UMM 1780
        IF(IR1 .EQ. 1) CONIT1 = DAI(IR1)                                UMM 1790
        DO 2220 IR=NCX1,NCX2                                            UMM 1800
          NCX = LRZN(IR)                                                UMM 1810
          DO 2210 M=1,ITMAT                                             UMM 1820
            MATIT = IDIT(M)                                             UMM 1830
            DEN(MATIT,NCX) = DEN(MATIT1,NCX) * COMPIT(M) / COMPIT(1)    UMM 1840
            IF(IT .GT. 0) GOTO 2210                                     UMM 1850
CFZJ059                                                       04.11.09  UMM 1860
            WRITE (NT,2014) (BU(N,MATIT),N=1,4),DEN(MATIT,NCX)          UMM 1870
 2210     CONTINUE                                                      UMM 1880
          IT = 1                                                        UMM 1890
 2220   CONTINUE                                                        UMM 1900
 2230 CONTINUE                                                          UMM 1910
C                                                                       UMM 1920
      CALL CITA(1,SGA,SGTR,V1,FKEN,NFUMM,SS)                            UMM 1930
C                                                                       UMM 1940
      WRITE (NT,2601) CONIT1,REACT(2)                                   UMM 1950
      IF(ABS(SPECK(1)-REACT(2))-SERCON) 2099,2099,2151                  UMM 1960
 2150 CONTINUE                                                          UMM 1970
CFZJ059                                                       04.11.09  UMM 1980
      WRITE (NT,2152) (BU(N,MATIT1),N=1,4)                              UMM 1990
      RETURN                                                            UMM 2000
 2151 CONTINUE                                                          UMM 2010
      JS = 1                                                            UMM 2020
      CONN = 0.75                                                       UMM 2030
      IF(REACT(2) .LT. SPECK(1)) CONN = 1.25                            UMM 2040
      IF(CONN .LT. 1. .AND. ITVAR  .NE. 0) GOTO 3001                    UMM 2050
      IF(ITVAR .EQ. 0) GOTO 2160                                        UMM 2060
C                                                                       UMM 2070
CARD R29                                                                UMM 2080
C                                                                       UMM 2090
      READ (NS,520) ITDUM,XKDUM,MAKDUM,XTDUM                            UMM 2100
C                                                                       UMM 2110
      WRITE (NT,520) ITDUM,XKDUM,MAKDUM,XTDUM                           UMM 2120
C                                                                       UMM 2130
CARD R30                                                                UMM 2140
C                                                                       UMM 2150
      READ (NS,520) (IDUM(M),COMDUM(M),M=1,ITDUM)                       UMM 2160
C                                                                       UMM 2170
      WRITE (NT,520) (IDUM(M),COMDUM(M),M=1,ITDUM)                      UMM 2180
 2160 CONTINUE                                                          UMM 2190
      CONIT2 = CONN * CONIT1                                            UMM 2200
      NCX1 = 1                                                          UMM 2210
      DO 2700 IR1=1,JARIT                                               UMM 2220
        NARIT = NCOL(IR1)                                               UMM 2230
        IF(NARIT .GT. 1) NCX1 =KRESHZ(NARIT-1) + 1                      UMM 2240
        NCX2 = KRESHZ(NARIT)                                            UMM 2250
        DO 2500 IR=NCX1,NCX2                                            UMM 2260
          NCX = LRZN(IR)                                                UMM 2270
          DO 2400 M=1,ITMAT                                             UMM 2280
            MATIT = IDIT(M)                                             UMM 2290
            DEN(MATIT,NCX) = CONIT2 * DAI(IR1) / DAI(1) * COMPIT(M) /   UMM 2300
     1       COMPIT(1)                                                  UMM 2310
 2400     CONTINUE                                                      UMM 2320
          IF(MAKEUP .LE. 0) GOTO 2500                                   UMM 2330
          HMETX = 0.                                                    UMM 2340
          DO 2450 M=1,IACT                                              UMM 2350
            HMETX = HMETX + DEN(M,NCX)                                  UMM 2360
 2450     CONTINUE                                                      UMM 2370
          DEN(MAKEUP,NCX) = DEN(MAKEUP,NCX) + HMETAV(NARIT) - HMETX     UMM 2380
 2500   CONTINUE                                                        UMM 2390
 2700 CONTINUE                                                          UMM 2400
      SERK1 = REACT(2)                                                  UMM 2410
 2506 CONTINUE                                                          UMM 2420
C                                                                       UMM 2430
      CALL CITA(1,SGA,SGTR,V1,FKEN,NFUMM,SS)                            UMM 2440
C                                                                       UMM 2450
      WRITE (NT,2601) CONIT2,REACT(2)                                   UMM 2460
      IF(ABS(SPECK(1)-REACT(2))-SERCON) 2099,2099,2501                  UMM 2470
 2501 IF(JS-JSMAX) 2503,2502,2502                                       UMM 2480
 2502 WRITE (NT,2504) JSMAX                                             UMM 2490
C                                                                       UMM 2500
      CALL EXIT                                                         UMM 2510
C                                                                       UMM 2520
 2503 CONTINUE                                                          UMM 2530
      JS = JS + 1                                                       UMM 2540
      SERK2 = REACT(2)                                                  UMM 2550
      ABW = SPECK(1) - SERK2                                            UMM 2560
      IF(ABW .LT. 0. .AND. CONIT2 .LE. 0.) GOTO 2150                    UMM 2570
      ADJ = (SPECK(1)-SERK2) / (SERK1-SERK2)                            UMM 2580
      CONIT3 = CONIT2                                                   UMM 2590
      CONIT2 = ADJ * (CONIT1-CONIT2) + CONIT2                           UMM 2600
      IF(CONIT2) 2110,2110,2111                                         UMM 2610
 2110 CONIT2 = 0.                                                       UMM 2620
      IF(CONIT3 .LE. 0.) GOTO 2150                                      UMM 2630
 2111 CONTINUE                                                          UMM 2640
      NCX1 = 1                                                          UMM 2650
      DO 2701 IR1=1,JARIT                                               UMM 2660
        NARIT = NCOL(IR1)                                               UMM 2670
        IF(NARIT .GT. 1) NCX1 = KRESHZ(NARIT-1) + 1                     UMM 2680
        NCX2 = KRESHZ(NARIT)                                            UMM 2690
        DO 2505 IR=NCX1,NCX2                                            UMM 2700
          NCX = LRZN(IR)                                                UMM 2710
          DO 2401 M=1,ITMAT                                             UMM 2720
            MATIT = IDIT(M)                                             UMM 2730
            DEN(MATIT,NCX) = CONIT2 * DAI(IR1) / DAI(1) * COMPIT(M) /   UMM 2740
     1       COMPIT(1)                                                  UMM 2750
 2401     CONTINUE                                                      UMM 2760
          IF(MAKEUP .LE. 0) GOTO 2505                                   UMM 2770
          HMETX = 0.                                                    UMM 2780
          DO 2451 M=1,IACT                                              UMM 2790
            HMETX = HMETX + DEN(M,NCX)                                  UMM 2800
 2451     CONTINUE                                                      UMM 2810
          DEN(MAKEUP,NCX) = DEN(MAKEUP,NCX) + HMETAV(NARIT) - HMETX     UMM 2820
 2505   CONTINUE                                                        UMM 2830
 2701 CONTINUE                                                          UMM 2840
      SERK1 = SERK2                                                     UMM 2850
      CONIT1 = CONIT3                                                   UMM 2860
      GOTO 2506                                                         UMM 2870
C                                                                       UMM 2880
C     PRINT RESULTS                                                     UMM 2890
C                                                                       UMM 2900
 2099 CONTINUE                                                          UMM 2910
      IF(MAKEUP .GT. 0) ITMAT = ITMAT + 1                               UMM 2920
      NCX1 = 1                                                          UMM 2930
      DO 2799 IR1=1,JARIT                                               UMM 2940
        NARIT = NCOL(IR1)                                               UMM 2950
        WRITE (NT,2525) JS,NARIT                                        UMM 2960
        IF(NARIT .GT. 1) NCX1 = KRESHZ(NARIT-1) + 1                     UMM 2970
        NCX2 = KRESHZ(NARIT)                                            UMM 2980
        NCX = LRZN(NCX1)                                                UMM 2990
        DO 2499 M=1,ITMAT                                               UMM 3000
          MATIT = IDIT(M)                                               UMM 3010
CFZJ059                                                       04.11.09  UMM 3020
          WRITE (NT,2014) (BU(N,MATIT),N=1,4),DEN(MATIT,NCX)            UMM 3030
 2499   CONTINUE                                                        UMM 3040
 2799 CONTINUE                                                          UMM 3050
      IPRIN(2) = IPRIN2                                                 UMM 3060
      IPRIN(3) = IPRIN3                                                 UMM 3070
      RETURN                                                            UMM 3080
      END                                                               UMM 3090
      SUBROUTINE FUREAD(XREPRO,KRESHZ,LRZN,NTYP1,N240,DAV,NFTV,NCOL,    FUR   10
     1 JAD11,AWT)                                                       FUR   20
C                                                                       FUR   30
C     F U R E A D   READS INITIAL FUEL MANAGEMENT INPUT DATA            FUR   40
C                                                                       FUR   50
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    FUR   60
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    FUR   70
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIFUR   80
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 FUR   90
C                                                                       FUR  100
      EQUIVALENCE(JTPE2,NS),(JTPE3,NT)                                  FUR  110
C                                                                       FUR  120
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1FUR  130
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         FUR  140
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    FUR  150
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)FUR  160
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  FUR  170
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    FUR  180
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         FUR  190
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,FUR  200
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            FUR  210
C                                                                       FUR  220
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, FUR  230
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        FUR  240
     2 FIMAKL(20),NOPILE,MREP,MARX(10),NAJB(10),FOJB(10),NFUL(10),NBTOT,FUR  250
     3 NB0,NCY                                                          FUR  260
C                                                                       FUR  270
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), FUR  280
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10FUR  290
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11FUR  300
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         FUR  310
C                                                                       FUR  320
      COMMON /HILF/ JMAT,JABOX                                          FUR  330
C                                                                       FUR  340
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300),FABC(10),REPC(10)    FUR  350
C                                                                       FUR  360
      COMMON /BLOCKK/ DU(3),AVO                                         FUR  370
C                                                                       FUR  380
      COMMON /FLUXN/ D(361),IACT                                        FUR  390
C                                                                       FUR  400
      DIMENSION NFT(30),XREPRO(KMAT),KRESHZ(N200),LRZN(N200),NTYP1(N240)FUR  410
     1 ,NFTV(N200),NCOL(N200),DAV(KMAT),JAD11(JD11),AWT(IACT)           FUR  420
C                                                                       FUR  430
  110 FORMAT ('0'//' THERE IS A REPROCESSING-PLANT WITH THE FOLLOWING CHFUR  440
     1ARACTERISTICS :')                                                 FUR  450
  111 FORMAT (//' MATERIAL NUMBER     ',I5,14I7/' REPROCESSING FACTOR ',FUR  460
     1 15F7.3)                                                          FUR  470
  500 FORMAT (18I4)                                                     FUR  480
  501 FORMAT ('1'/' LAYOUT OF REFUELLING BATCHES. THERE IS A TOTAL OF', FUR  490
     1 I5,' BATCHES.'//)                                                FUR  500
  502 FORMAT (15X,'BATCH',I5,' CONTAINS FUEL TYPE',I3)                  FUR  510
  503 FORMAT ('0','KUGL....',I4,'    NO OF FUEL TYPES....',I4)          FUR  520
  504 FORMAT ('0FUEL TYPE ',I2,' HAS A FRESH FUEL RESERVOIR OF VOLUME ',FUR  530
     1 E12.5,' CM3'/' CONTAINING THE FOLLOWING ISOTOPIC COMPOSITION...')FUR  540
  506 FORMAT (////30X,'FUEL DISCHARGED FROM THE REACTOR AND SCRAPPED'/  FUR  550
     1 30X,'WILL BE COMBINED INTO',I2,' REPROCESSING MIXTURES'/30X,'AND FUR  560
     2MAY BE CALLED ON FOR RELOADING ON CARDS R21')                     FUR  570
  507 FORMAT (//' SCRAP OF FUEL TYPE NO .......',10I6/' REPROCESSING MIXFUR  580
     1TURE NO .....',10I6)                                              FUR  590
  508 FORMAT (' (DATA-2-COMPOSITION NO.: ',I5,')')                      FUR  600
  509 FORMAT (//' REPROCESSING MIXTURE:          ',10I6)                FUR  610
  510 FORMAT (' NO. OF AGING PLUS JUMBLE BOXES:',10I6)                  FUR  620
  520 FORMAT (4(I6,E12.5))                                              FUR  630
  555 FORMAT (6E12.5)                                                   FUR  640
  556 FORMAT (7E10.3)                                                   FUR  650
  560 FORMAT (' STORAGE TIME FOR OUT-OF-PILE FUEL...',F12.0,' DAYS'/' REFUR  660
     1PROCESSING AND COOLING TIME ......',F12.0,' DAYS'/' ANNUAL LOAD FAFUR  670
     2CTOR .................',F16.4/' FAILURE RATE OF FUEL .............FUR  680
     3..',E12.3,' 1/CYCLE')                                             FUR  690
  561 FORMAT (///' SHUT DOWN TIME FOR RELOADING .......',F12.0,' DAYS') FUR  700
  562 FORMAT (///' SHUT DOWN TIME FOR RELOADING .......',F12.4,' PER CYCFUR  710
     1LE LENGTH')                                                       FUR  720
  565 FORMAT (//' *** PROGRAM STOPS BECAUSE "MREP" IS TOO SMALL.'/' CHECFUR  730
     1K THE INPUT OF CARD V1 IN CONNECTION WITH CARD R3. ***')          FUR  740
  566 FORMAT (//' *** PROGRAM STOPS BECAUSE "MREP" IS TOO LARGE.'/' CHECFUR  750
     1K THE INPUT OF CARD V1 IN CONNECTION WITH CARD R3. ***')          FUR  760
  567 FORMAT (//' *** PROGRAM STOPS BECAUSE "JABOX" IS TOO SMALL.'/' CHEFUR  770
     1CK THE INPUT OF CARD V1 IN CONNECTION WITH CARD R5. ***')         FUR  780
  568 FORMAT (//' *** PROGRAM STOPS BECAUSE "JABOX" IS TOO LARGE.'/' CHEFUR  790
     1CK THE INPUT OF CARD V1 IN CONNECTION WITH CARD R5. ***')         FUR  800
 2152 FORMAT (///' CARDS R3 AND PROBABLY R4 IN WRONG ORDER. THE FOLLOWINFUR  810
     1G PRINTED DATA WILL BE USED........')                             FUR  820
C                                                                       FUR  830
C                                                                       FUR  840
      JEEP = 0                                                          FUR  850
      NWRITE = 0                                                        FUR  860
      NNNN = 1                                                          FUR  870
      NBTOT = 0                                                         FUR  880
      NB0 = 0                                                           FUR  890
      NCY = 0                                                           FUR  900
      DO 14 I=1,10                                                      FUR  910
        NFUL(I) = 0                                                     FUR  920
   14 CONTINUE                                                          FUR  930
      NRESHZ = N200                                                     FUR  940
      NXT29 = N33                                                       FUR  950
C                                                                       FUR  960
      CALL WRDA(IREAD,NDA29,NXT29,L29,NFTV,N200)                        FUR  970
C                                                                       FUR  980
      DO 7 I=1,NRESHZ                                                   FUR  990
        NTYP1(I) = NFTV(I) / 100                                        FUR 1000
        LRZN(I) = I                                                     FUR 1010
        NCOL(I) = 1                                                     FUR 1020
    7 CONTINUE                                                          FUR 1030
      IF(IPRIN(2) .GE. 1) WRITE (NT,501) NRESHZ                         FUR 1040
      KRESHZ(1) = NCOL(1)                                               FUR 1050
      DO 10 IR=1,NRESHZ                                                 FUR 1060
        IF(IR .EQ. 1) GOTO 5                                            FUR 1070
        KRESHZ(IR) = KRESHZ(IR-1) + NCOL(IR)                            FUR 1080
    5   CONTINUE                                                        FUR 1090
        IF(IPRIN(2) .GE. 1) WRITE (NT,502) IR,NTYP1(IR)                 FUR 1100
   10 CONTINUE                                                          FUR 1110
      IF(NRSTRT .LT. 3) GOTO 21                                         FUR 1120
C                                                                       FUR 1130
CARD R1                                                                 FUR 1140
C                                                                       FUR 1150
      READ (NS,555) (XREPRO(I),I=1,KMAT)                                FUR 1160
C                                                                       FUR 1170
      WRITE (NT,110)                                                    FUR 1180
      XMAL = KMAT / 15                                                  FUR 1190
      IMAL = INT(XMAL) + 1                                              FUR 1200
      ITIT = 15 * (IMAL-1)                                              FUR 1210
      IF(ITIT .GE. KMAT) IMAL = IMAL - 1                                FUR 1220
      DO 20 L=1,IMAL                                                    FUR 1230
        II = 15 * (L-1) + 1                                             FUR 1240
        III = 15 * L                                                    FUR 1250
        JJJ = III                                                       FUR 1260
        IF(III .GT. KMAT) III = KMAT                                    FUR 1270
        WRITE (NT,111) (I,I=II,JJJ),(XREPRO(I),I=II,III)                FUR 1280
   20 CONTINUE                                                          FUR 1290
   21 CONTINUE                                                          FUR 1300
C                                                                       FUR 1310
CARD R2                                                                 FUR 1320
C                                                                       FUR 1330
      READ (NS,556) XKUGL,TDOWN,TSTORE,TREPRO,AAAA,BRUCH,AGEBOX         FUR 1340
C                                                                       FUR 1350
      KUGL = IFIX(XKUGL)                                                FUR 1360
      IF(AAAA .EQ. 0.) AAAA = 1.                                        FUR 1370
      TDW = -TDOWN                                                      FUR 1380
      IF(TDOWN .GE. 0.) WRITE (NT,561) TDOWN                            FUR 1390
      IF(TDOWN .LT. 0.) WRITE (NT,562) TDW                              FUR 1400
      WRITE (NT,560) TSTORE,TREPRO,AAAA,BRUCH                           FUR 1410
      DO 90 K=1,JTYP                                                    FUR 1420
        KLASSE(K) = 0                                                   FUR 1430
   90 CONTINUE                                                          FUR 1440
      WRITE (NT,503) KUGL,JTYP                                          FUR 1450
      IF(KUGL .LE. 0) GOTO 2                                            FUR 1460
      KL = 0                                                            FUR 1470
      NREP = 0                                                          FUR 1480
      NULL = 0                                                          FUR 1490
      NOP = 0                                                           FUR 1500
      J = KMAT + 2                                                      FUR 1510
      DO 35 K=1,JTYP                                                    FUR 1520
        KLZ = KLASSE(K)                                                 FUR 1530
        IF(KLZ .GT. 0) GOTO 24                                          FUR 1540
        KLZ = 1                                                         FUR 1550
        KLASSE(K) = 1                                                   FUR 1560
        FIMAKL(KL+1) = 0.                                               FUR 1570
        GOTO 25                                                         FUR 1580
   24   CONTINUE                                                        FUR 1590
C                                                                       FUR 1600
        READ (NS,555) (FIMAKL(KL+I),I=1,KLZ)                            FUR 1610
C                                                                       FUR 1620
   25   CONTINUE                                                        FUR 1630
        KL = KL + KLZ                                                   FUR 1640
        DO 26 M=1,KMAT                                                  FUR 1650
          DAV(M) = 0.                                                   FUR 1660
   26   CONTINUE                                                        FUR 1670
C                                                                       FUR 1680
CARD R3                                                                 FUR 1690
C                                                                       FUR 1700
        READ (NS,520) NTP1,PARVL1,NISO,XMARX                            FUR 1710
C                                                                       FUR 1720
        IF(NTP1 .NE. K) WRITE (NT,2152)                                 FUR 1730
        NTP1 = K                                                        FUR 1740
        MARX(K) = IFIX(XMARX)                                           FUR 1750
        IF(MARX(K) .GT. NREP)  NREP = MARX(K)                           FUR 1760
        WRITE (NT,504) NTP1,PARVL1                                      FUR 1770
        IF(NISO) 9,27,1                                                 FUR 1780
    9   CONTINUE                                                        FUR 1790
        IF(NISO .GT. -100) GOTO 4                                       FUR 1800
        NISO = IABS(NISO)                                               FUR 1810
        NXT29 = 2                                                       FUR 1820
        READ (NDA29,REC=NXT29) N30,(NFT(L),L=1,N30)                     FUR 1830
        NXT29 = NXT29 + 1                                               FUR 1840
        DO 11 L=1,N30                                                   FUR 1850
          NXT29 = L                                                     FUR 1860
          IF(NISO .EQ. NFT(L)) GOTO 12                                  FUR 1870
   11   CONTINUE                                                        FUR 1880
   12   CONTINUE                                                        FUR 1890
        WRITE (NT,508) NISO                                             FUR 1900
        GOTO 13                                                         FUR 1910
    4   NXT29 = NTP1 + 1                                                FUR 1920
   13   CONTINUE                                                        FUR 1930
        READ (NDA29,REC=NXT29) NISO,(L,DAV(L),M=1,NISO),(DUM,M=1,9),MM, FUR 1940
     1   (DUM,M=1,MM),(DUM,M=1,17),FABC(K),REPC(K)                      FUR 1950
        NXT29 = NXT29 + 1                                               FUR 1960
        GOTO 27                                                         FUR 1970
    1   CONTINUE                                                        FUR 1980
C                                                                       FUR 1990
CARD R4                                                                 FUR 2000
C                                                                       FUR 2010
        READ (NS,520) (L,DAV(L),M=1,NISO)                               FUR 2020
C                                                                       FUR 2030
   27   CONTINUE                                                        FUR 2040
        HMDEN = 0.                                                      FUR 2050
        HMGRM = 0.                                                      FUR 2060
        DO 28 M=1,IACT                                                  FUR 2070
          HMDEN = HMDEN + DAV(M)                                        FUR 2080
          HMGRM = HMGRM + DAV(M) * AWT(M)                               FUR 2090
   28   CONTINUE                                                        FUR 2100
        HMGRM = HMGRM / AVO * PARVL1                                    FUR 2110
        WRITE (NT,520) (L,DAV(L),L=1,KMAT)                              FUR 2120
        XCHG1 = 0.                                                      FUR 2130
        DO 30 N=1,KLZ                                                   FUR 2140
          IKL1 = N                                                      FUR 2150
          NOPI = NOP + N                                                FUR 2160
          JSATZ = NRESHZ + 2 + MBOX + NOPI                              FUR 2170
          IF(JAD11(JSATZ) .EQ. 0) JAD11(JSATZ) = JSUM11                 FUR 2180
          NXT11 = JAD11(JSATZ)                                          FUR 2190
          IF(N .GT. 1) GOTO 32                                          FUR 2200
          WRITE (NDA11,REC=NXT11) (DAV(L),L=1,KMAT),PARVL1,XCHG1,NTP1,  FUR 2210
     1     IKL1,NULL,NULL,HMGRM,HMDEN                                   FUR 2220
          NXT11 = NXT11 + 1                                             FUR 2230
          GOTO 33                                                       FUR 2240
   32     CONTINUE                                                      FUR 2250
          WRITE (NDA11,REC=NXT11) (NULL,L=1,J),NTP1,IKL1,NULL,NULL,NULL,FUR 2260
     1     NULL                                                         FUR 2270
          NXT11 = NXT11 + 1                                             FUR 2280
   33     CONTINUE                                                      FUR 2290
          IF(JSUM11 .LT. NXT11) JSUM11 = NXT11                          FUR 2300
   30   CONTINUE                                                        FUR 2310
        NOP = NOP + KLZ                                                 FUR 2320
   35 CONTINUE                                                          FUR 2330
      IF(NREP .EQ. MREP) GOTO 37                                        FUR 2340
      IF(NREP .GT. MREP) WRITE (NT,565)                                 FUR 2350
      IF(NREP .LT. MREP) WRITE (NT,566)                                 FUR 2360
      STOP                                                              FUR 2370
   37 CONTINUE                                                          FUR 2380
      IF(MREP .EQ. 0) GOTO 2                                            FUR 2390
      IF(AGEBOX .LE. 0.) GOTO 15                                        FUR 2400
C                                                                       FUR 2410
CARD R5                                                                 FUR 2420
C                                                                       FUR 2430
      READ (NS,500) (NAJB(L),L=1,MREP)                                  FUR 2440
C                                                                       FUR 2450
      DO 3 L=1,MREP                                                     FUR 2460
        NBTOT = NBTOT + NAJB(L)                                         FUR 2470
    3 CONTINUE                                                          FUR 2480
      IF(NBTOT .EQ. JABOX) GOTO 15                                      FUR 2490
      IF(NBTOT .GT. JABOX) WRITE (NT,567)                               FUR 2500
      IF(NBTOT .LT. JABOX) WRITE (NT,568)                               FUR 2510
      STOP                                                              FUR 2520
   15 CONTINUE                                                          FUR 2530
      WRITE (NT,506) MREP                                               FUR 2540
      WRITE (NT,507) (L,L=1,10),(MARX(K),K=1,JTYP)                      FUR 2550
      IF(AGEBOX .GT. 0.) WRITE (NT,509) (K,K=1,MREP)                    FUR 2560
      IF(AGEBOX .GT. 0.) WRITE (NT,510) (NAJB(K),K=1,MREP)              FUR 2570
    2 CONTINUE                                                          FUR 2580
      RETURN                                                            FUR 2590
      END                                                               FUR 2600
      SUBROUTINE FULOAD(MANAGE,IX,RX)                                   FUL   10
C                                                                       FUL   20
C     MANAGES 'OUT-OF-PILE-BOXES' (OPB) AND 'THIS-BATCH-POSITIONS' (TBP)FUL   30
C                                                                       FUL   40
      DIMENSION IX(9),RX(3)                                             FUL   50
C                                                                       FUL   60
C                                                                       FUL   70
      I7 = IABS(IX(7))                                                  FUL   80
      I8 = IABS(IX(8))                                                  FUL   90
      R1 = RX(1)                                                        FUL  100
      R2 = RX(2)                                                        FUL  110
      R3 = RX(3)                                                        FUL  120
C                                                                       FUL  130
C     MANAGE = 1: OPB IS A REPROCESSING MIXTURE WHICH CAN OPTIONALLY BE FUL  140
C                 LOADED FROM A JUMBLE BOX                              FUL  150
C                                                                       FUL  160
      IX(7) = -I8                                                       FUL  170
      IX(8) = -I7                                                       FUL  180
      RX(1) = R1                                                        FUL  190
      RX(2) = R2                                                        FUL  200
      RX(3) = 0.                                                        FUL  210
      IF(R3 .EQ. 0.) GOTO 9                                             FUL  220
      IF(R3 .NE. 1.) GOTO 4                                             FUL  230
      RX(2) = R3                                                        FUL  240
      GOTO 9                                                            FUL  250
    4 CONTINUE                                                          FUL  260
      IF(R3 .LT. 0.) GOTO 5                                             FUL  270
      RX(3) = -R3                                                       FUL  280
      GOTO 9                                                            FUL  290
    5 CONTINUE                                                          FUL  300
      RX(3) = R3                                                        FUL  310
      RX(1) = -R1                                                       FUL  320
    9 RETURN                                                            FUL  330
      END                                                               FUL  340
      SUBROUTINE REMI(MM,VR,DDAY,JNST,NPRINT,KMAT6,JAD11,D,DMAT,AWT)    REM   10
C                                                                       REM   20
C     TRANSFER OF AGING AND JUMBLE BOXES                                REM   30
C                                                                       REM   40
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    REM   50
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    REM   60
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIREM   70
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 REM   80
C                                                                       REM   90
      EQUIVALENCE(JTPE3,NT)                                             REM  100
C                                                                       REM  110
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, REM  120
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        REM  130
     2 FIMAKL(20),NOPILE,MREP,MARX(10),NAJB(10),FOJB(10),NFUL(10),NBTOT,REM  140
     3 NB0,NCY                                                          REM  150
C                                                                       REM  160
      COMMON /FLUXN/ DU(361),IACT                                       REM  170
C                                                                       REM  180
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300),FABC(10),REPC(10),NNEREM  190
     1 ,LISTEQ                                                          REM  200
C                                                                       REM  210
      COMMON /BUC/ BU(6,200)                                            REM  220
C                                                                       REM  230
      COMMON /CVX/ VX                                                   REM  240
C                                                                       REM  250
      CHARACTER*4 BU                                                    REM  260
C                                                                       REM  270
      DIMENSION VR(1000),DI(4,30),D(3,KMAT6),JAD11(JD11),AWT(IACT),     REM  280
     1 DMAT(IACT+1,4,4),VX(1000)                                        REM  290
C                                                                       REM  300
  100 FORMAT (/' STATUS OF THE RECYCLING BOXES OF REPROCESSING MIXTURE 'REM  310
     1 ,I3,':')                                                         REM  320
  101 FORMAT (/' FOR REP.MIXT.',I3,' NUMBER OF AGING BOXES IS TOO LOW.'/REM  330
     1 ' MATERIAL OF AGING BOX',I3,' IS SUBMITTED TO JUMBLE BOX.')      REM  340
  102 FORMAT (/21X,'LEFT OVER AND RETURNED',8X,'SCRAP FUEL, SUBMITTED TOREM  350
     1',8X,'CONTENT OF THE'/25X,'TO JUMBLE BOX:',14X,'THE FIRST AGING BOREM  360
     2X:',11X,'JUMBLE BOX:')                                            REM  370
  103 FORMAT (' VOLUME     CM**3:',7X,E12.5,19X,E12.5,15X,E12.5)        REM  380
  104 FORMAT (' HEAVY METAL   GR:',7X,E12.5,19X,E12.5,15X,E12.5)        REM  390
  105 FORMAT (/' AGING BOXES:            VOLUME     HEAVY METAL     AGE REM  400
     1AFTER SUBSEQ. CYCLE')                                             REM  410
  106 FORMAT (10X,I2,9X,E12.5, E14.5,E22.5)                             REM  420
  107 FORMAT (' ISOTOPES      KG:')                                     REM  430
  108 FORMAT (' ',4A4,8X,E12.5,19X,E12.5,15X,E12.5)                     REM  440
  109 FORMAT (/' THE FRACTION',E12.5,' OF THE JUMBLE BOX WILL BE PROVIDEREM  450
     1D FOR RELOADING AFTER THE SUBSEQUENT OPERATING CYCLE.')           REM  460
C                                                                       REM  470
C                                                                       REM  480
      NBM = NAJB(MM)                                                    REM  490
      NFM = NFUL(MM)                                                    REM  500
      NB2 = NB0 + MREP                                                  REM  510
      KM = KMAT                                                         REM  520
      K1 = KM + 1                                                       REM  530
      K5 = KM + 5                                                       REM  540
      K6 = KM + 6                                                       REM  550
      VOL = 0.                                                          REM  560
      DO 1 M=1,MM                                                       REM  570
        NB2 = NB2 + NAJB(M)                                             REM  580
    1 CONTINUE                                                          REM  590
      NB1 = NB2 - NBM + 1                                               REM  600
      DAYJ = DDAY * JNST                                                REM  610
C                                                                       REM  620
C     J = 1: REPROCESSING MIXTURE                                       REM  630
C     J = 2: JUMBLE BOX                                                 REM  640
C     J = 3: AGING BOXES                                                REM  650
C                                                                       REM  660
C     +1 ... +3 = READ .....                                            REM  670
C     -1 ... -3 = WRITE .....                                           REM  680
C                                                                       REM  690
      J = 1                                                             REM  700
      NB = NB0 + MM                                                     REM  710
C                                                                       REM  720
      CALL RWD11(J,NB,KM,NFTP,D,KMAT6,JAD11)                            REM  730
C                                                                       REM  740
      J = 2                                                             REM  750
      NB = NB2                                                          REM  760
C                                                                       REM  770
      CALL RWD11(J,NB,KM,NFTP,D,KMAT6,JAD11)                            REM  780
C                                                                       REM  790
      DO 6 I=1,IACT                                                     REM  800
        DI(4,I) = 0.                                                    REM  810
        DI(1,I) = D(2,I)                                                REM  820
    6 CONTINUE                                                          REM  830
      DI(4,IACT+1) = 0.                                                 REM  840
      DI(1,IACT+1) = VR(MM)                                             REM  850
      DI(1,IACT+2) = VR(MM+10)                                          REM  860
      D(2,K1) = D(2,K1) + VR(MM)                                        REM  870
      D(2,K5) = D(2,K5) + VR(MM+10)                                     REM  880
      DO 11 M=1,NBM                                                     REM  890
        IF(NFM) 12,2,4                                                  REM  900
    2   CONTINUE                                                        REM  910
        DO 3 I=1,K6                                                     REM  920
          D(3,I) = D(1,I)                                               REM  930
    3   CONTINUE                                                        REM  940
        D(3,KM+4) = 0.                                                  REM  950
        DO 15 I=1,IACT                                                  REM  960
          DI(2,I) = D(3,I)                                              REM  970
   15   CONTINUE                                                        REM  980
        DI(2,IACT+1) = D(3,K1)                                          REM  990
        DI(2,IACT+2) = D(3,K5)                                          REM 1000
        GOTO 5                                                          REM 1010
    4   CONTINUE                                                        REM 1020
        J = 3                                                           REM 1030
        NB = NB1 + NFM - 1                                              REM 1040
C                                                                       REM 1050
        CALL RWD11(J,NB,KM,NFTP,D,KMAT6,JAD11)                          REM 1060
C                                                                       REM 1070
        IF(D(3,K1) .LE. 0. .AND. VOL .LE. 0.) GOTO 10                   REM 1080
    5   CONTINUE                                                        REM 1090
        VOL = VOL + D(3,K1)                                             REM 1100
        IF(TREPRO-DAYJ .LT. D(3,KM+4)) GOTO 8                           REM 1110
        IF(NBM .EQ. 1) GOTO 8                                           REM 1120
        IF(NFM+1 .LT. NBM) GOTO 7                                       REM 1130
        WRITE (NT,101) MM,NFM                                           REM 1140
        GOTO 8                                                          REM 1150
    7   CONTINUE                                                        REM 1160
C                                                                       REM 1170
C     HOCHBUCHEN DER AGING BOXES                                        REM 1180
C                                                                       REM 1190
        D(3,KM+4) = D(3,KM+4) + DAYJ                                    REM 1200
        J = -3                                                          REM 1210
        NB = NB1 + NFM                                                  REM 1220
        IF(NFUL(MM) .LE. NFM) NFUL(MM) = NFUL(MM) + 1                   REM 1230
C                                                                       REM 1240
        CALL RWD11(J,NB,KM,NFTP,D,KMAT6,JAD11)                          REM 1250
C                                                                       REM 1260
        DO 19 I=1,IACT                                                  REM 1270
          DI(4,I) = DI(4,I) + D(3,I) * D(3,K1)                          REM 1280
   19   CONTINUE                                                        REM 1290
        DI(4,IACT+1) = DI(4,IACT+1) + D(3,K1)                           REM 1300
        GOTO 10                                                         REM 1310
    8   CONTINUE                                                        REM 1320
C                                                                       REM 1330
C     AUFFUELLEN DER JUMBLE BOX                                         REM 1340
C                                                                       REM 1350
        V = D(3,K1) + D(2,K1)                                           REM 1360
        IF(V .LE. 0.) GOTO 10                                           REM 1370
        V3 = D(3,K1) / V                                                REM 1380
        V2 = D(2,K1) / V                                                REM 1390
        DO 9 K=1,KM                                                     REM 1400
          D(2,K) = D(3,K) * V3 + D(2,K) * V2                            REM 1410
    9   CONTINUE                                                        REM 1420
        D(2,K6) = D(3,K6) * V3 + D(2,K6) * V2                           REM 1430
        D(2,K5) = D(3,K5) + D(2,K5)                                     REM 1440
        D(2,K1) = D(2,K1) + D(3,K1)                                     REM 1450
        IF(NFUL(MM) .GT. NFM) NFUL(MM) = NFM                            REM 1460
   10   CONTINUE                                                        REM 1470
        NFM = NFM - 1                                                   REM 1480
   11 CONTINUE                                                          REM 1490
   12 CONTINUE                                                          REM 1500
      DO 16 I=1,IACT                                                    REM 1510
        IF(DI(4,IACT+1) .GT. 0.) DI(4,I) = DI(4,I) / DI(4,IACT+1)       REM 1520
        DI(3,I) = D(2,I)                                                REM 1530
   16 CONTINUE                                                          REM 1540
      DI(3,IACT+1) = D(2,K1)                                            REM 1550
      DI(3,IACT+2) = D(2,K5)                                            REM 1560
C                                                                       REM 1570
C     AUFFUELLEN DER REP.MIXT. AUS JUMBLE BOX MIT ANTEIL FOJB(MM)       REM 1580
C                                                                       REM 1590
      FOJBM = FOJB(MM)                                                  REM 1600
      IF(D(2,K1) .LE. 0.) FOJBM = ABS(FOJBM)                            REM 1610
      IF(FOJBM .LT. 0.) FOJBM = ABS(FOJBM) * D(1,K1) / D(2,K1)          REM 1620
      DO 13 K=1,KM                                                      REM 1630
        D(1,K) = D(2,K)                                                 REM 1640
   13 CONTINUE                                                          REM 1650
      D(1,K6) = D(2,K6)                                                 REM 1660
      D(1,K5) = D(2,K5) * FOJBM                                         REM 1670
      D(2,K5) = D(2,K5) - D(1,K5)                                       REM 1680
      D(1,K1) = D(2,K1) * FOJBM                                         REM 1690
      D(2,K1) = D(2,K1) - D(1,K1)                                       REM 1700
      D(1,KM+2) = 0.                                                    REM 1710
      D(1,KM+3) = 0.                                                    REM 1720
      D(1,KM+4) = 0.                                                    REM 1730
      J = -1                                                            REM 1740
      NB = NB0 + MM                                                     REM 1750
C                                                                       REM 1760
      CALL RWD11(J,NB,KM,NFTP,D,KMAT6,JAD11)                            REM 1770
C                                                                       REM 1780
      J = -2                                                            REM 1790
      NB = NB2                                                          REM 1800
C                                                                       REM 1810
      CALL RWD11(J,NB,KM,NFTP,D,KMAT6,JAD11)                            REM 1820
C                                                                       REM 1830
      CALL KILO(DI,AWT)                                                 REM 1840
C                                                                       REM 1850
      DO 20 I=1,IACT+1                                                  REM 1860
        DMAT(I,1,MM) = DI(1,I)                                          REM 1870
        DMAT(I,2,MM) = DI(2,I)                                          REM 1880
        DMAT(I,3,MM) = DI(3,I) + DI(4,I)                                REM 1890
        DMAT(I,4,MM) = DI(3,I) * FOJBM                                  REM 1900
   20 CONTINUE                                                          REM 1910
      WRITE (NT,100) MM                                                 REM 1920
      WRITE (NT,102)                                                    REM 1930
      WRITE (NT,103) (DI(K,IACT+1),K=1,3)                               REM 1940
      WRITE (NT,104) (DI(K,IACT+2),K=1,3)                               REM 1950
      IF(NPRINT .LT. -1) GOTO 18                                        REM 1960
      WRITE (NT,107)                                                    REM 1970
      DO 17 K=1,IACT                                                    REM 1980
        WRITE (NT,108) (BU(I,K),I=1,4),(DI(I,K),I=1,3)                  REM 1990
   17 CONTINUE                                                          REM 2000
   18 CONTINUE                                                          REM 2010
      WRITE (NT,105)                                                    REM 2020
      NFM = NFUL(MM)                                                    REM 2030
      DO 14 K=1,NFM                                                     REM 2040
        J = 3                                                           REM 2050
        NB = NB1 - 1 + K                                                REM 2060
C                                                                       REM 2070
        CALL RWD11(J,NB,KM,NFTP,D,KMAT6,JAD11)                          REM 2080
C                                                                       REM 2090
        WRITE (NT,106) K,D(3,K1),D(3,K5),D(3,KM+4)                      REM 2100
   14 CONTINUE                                                          REM 2110
      WRITE (NT,109) FOJBM                                              REM 2120
      RETURN                                                            REM 2130
      END                                                               REM 2140
      SUBROUTINE RWD11(J,NB,KM,NFTP,D,KMAT6,JAD11)                      RWD   10
C                                                                       RWD   20
C     READ AND WRITE REPROCESSING MIXTURES, AGING + JUMBLE BOXES        RWD   30
C                                                                       RWD   40
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    RWD   50
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    RWD   60
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIRWD   70
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 RWD   80
C                                                                       RWD   90
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), RWD  100
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10RWD  110
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11RWD  120
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         RWD  130
C                                                                       RWD  140
      DIMENSION D(3,KMAT6),JAD11(JD11)                                  RWD  150
C                                                                       RWD  160
C                                                                       RWD  170
      NULL = 0                                                          RWD  180
      K2 = KM + 2                                                       RWD  190
      K3 = KM + 3                                                       RWD  200
      K6 = KM + 6                                                       RWD  210
      NXT11 = JAD11(NB)                                                 RWD  220
      IF(J .LT. 0) GOTO 1                                               RWD  230
      READ (NDA11,REC=NXT11) (D(J,L),L=1,K2),NFTP,NULL,(D(J,L),L=K3,K6) RWD  240
      NXT11 = NXT11 + 1                                                 RWD  250
      GOTO 9                                                            RWD  260
    1 CONTINUE                                                          RWD  270
      J = IABS(J)                                                       RWD  280
      WRITE (NDA11,REC=NXT11) (D(J,L),L=1,K2),NFTP,NULL,(D(J,L),L=K3,K6)RWD  290
      NXT11 = NXT11 + 1                                                 RWD  300
    9 CONTINUE                                                          RWD  310
      RETURN                                                            RWD  320
      END                                                               RWD  330
      SUBROUTINE KILO(DI,AWT)                                           KIL   10
C                                                                       KIL   20
      COMMON /BLOCKK/ DU(3),AVO                                         KIL   30
C                                                                       KIL   40
      COMMON /FLUXN/ D(361),IACT                                        KIL   50
C                                                                       KIL   60
      DIMENSION DI(4,30),AWT(IACT)                                      KIL   70
C                                                                       KIL   80
C                                                                       KIL   90
      DO 1 I=1,4                                                        KIL  100
        V = 0.001 * DI(I,IACT+1) / AVO                                  KIL  110
        DO 1 J=1,IACT                                                   KIL  120
          DI(I,J) = DI(I,J) * AWT(J) * V                                KIL  130
    1 CONTINUE                                                          KIL  140
      RETURN                                                            KIL  150
      END                                                               KIL  160
      SUBROUTINE LISTE(IMAT)                                            LIS   10
C                                                                       LIS   20
C     ZUSAMMENGESTELLTE DATEN FUER GESCHLOSSENEN OUTPUT                 LIS   30
C                                                                       LIS   40
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300),FABC(10),REPC(10),NNELIS   50
     1 ,LISTEQ,NTIK,K2,NRC,NRL,NRO,LASTPR                               LIS   60
C                                                                       LIS   70
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    LIS   80
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    LIS   90
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PILIS  100
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 LIS  110
C                                                                       LIS  120
      COMMON /NUCNAM/ T1(200),T2(200)                                   LIS  130
C                                                                       LIS  140
      COMMON /FLUXN/ D(361),IACT                                        LIS  150
C                                                                       LIS  160
      CHARACTER*4 T1,T2                                                 LIS  170
C                                                                       LIS  180
      DIMENSION IMAT(KMAT)                                              LIS  190
C                                                                       LIS  200
   10 FORMAT ('1')                                                      LIS  210
   20 FORMAT (81X,'DATE: ',14X,I2,'.',I2,'.',I4/' SUMMARY OUTPUT OF CASELIS  220
     1:               ',42X,'OPERATIONEL PERIOD:',I4/' ',23('-'),57X,'TILIS  230
     2MESTEP:',10X,I4)                                                  LIS  240
  890 FORMAT (//' PERFORMANCE DATA OF TIMESTEP NO.:',I3/' ',36('-'))    LIS  250
   30 FORMAT (/' GLOBAL DATA:')                                         LIS  260
   40 FORMAT ('   K-EFF                                 ',F15.4)        LIS  270
  881 FORMAT ('   FISSIONS/ENERGY         E+10 (FISS/WS)',F14.3)        LIS  280
   90 FORMAT ('   POWER PEAKING MAX./AVG.               ',F13.2)        LIS  290
  100 FORMAT ('   MAX. POWER PER BALL            KW/BALL',F13.2)        LIS  300
  110 FORMAT (/' NEUTRON DOSIS:'/'   FAST NEUTRON EXPOSURE (ENERGY GROUPLIS  310
     1 1)')                                                             LIS  320
  130 FORMAT ('     MAX. UPPER EDGE      E+21/(CM2*365D)',F13.2)        LIS  330
  140 FORMAT ('     MAX. LOWER EDGE      E+21/(CM2*365D)',F13.2)        LIS  340
  150 FORMAT ('     MAX. OUTER EDGE      E+21/(CM2*365D)',F13.2)        LIS  350
  155 FORMAT ('     MAX. INNER EDGE      E+21/(CM2*365D)',F13.2)        LIS  360
  160 FORMAT ('   THERMAL NEUTRON FLUX  (ENERGY GROUP',I2,')')          LIS  370
  170 FORMAT ('     MAX. UPPER EDGE       E+14/(CM2*SEC)',F13.2)        LIS  380
  180 FORMAT ('     MAX. LOWER EDGE       E+14/(CM2*SEC)',F13.2)        LIS  390
  190 FORMAT ('     MAX. OUTER EDGE       E+14/(CM2*SEC)',F13.2)        LIS  400
  195 FORMAT ('     MAX. INNER EDGE       E+14/(CM2*SEC)',F13.2)        LIS  410
  196 FORMAT ('   AVG. THERMAL FLUX       E+14/(CM2*SEC)',F13.2)        LIS  420
  197 FORMAT ('   AVG. TOTAL FLUX         E+14/(CM2*SEC)',F13.2)        LIS  430
  200 FORMAT (/' REACTIVITY EFFECTS:')                                  LIS  440
  210 FORMAT ('   XE-OVERRIDE/ ',I2,'%-LOAD FRACTION DELTA K-EFF',F14.3)LIS  450
  220 FORMAT (/' NEUTRON BALANCE:'/'   FRACTIONAL NEUTRON PRODUCTIONS BYLIS  460
     1')                                                                LIS  470
  230 FORMAT ('              ',2A4,'                  %',F13.2)         LIS  480
  360 FORMAT (/'   NEUTRON LOSSES IN HEAVY METALS       %',F13.2)       LIS  490
  370 FORMAT ('     ESP. IN FISSILE ISOTOPES           %',F13.2)        LIS  500
  380 FORMAT ('     ESP. IN ',2A4,'                   %',F13.2)         LIS  510
  510 FORMAT ('     IN FISSION PRODUCTS                %',F13.2)        LIS  520
  520 FORMAT ('     ESP. IN XE-135                     %',F13.2)        LIS  530
  530 FORMAT ('     IN AXIAL LEAKAGE                   %',F13.2)        LIS  540
  545 FORMAT (/'     CORE-LEAKAGE                       %',F13.2)       LIS  550
  540 FORMAT ('     IN RADIAL LEAKAGE                  %',F13.2)        LIS  560
  900 FORMAT (' PERFORMANCE DATA OF CYCLE NO.:',I4/' ',34('-'))         LIS  570
   50 FORMAT ('   AVG. FISSILE ENRICHMENT              %',F13.2)        LIS  580
   60 FORMAT ('   AVG. FUEL RESIDENCE TIME          DAYS',F12.1)        LIS  590
   70 FORMAT ('   AVG. BURNUP                      MWD/T',F12.1)        LIS  600
   80 FORMAT ('   CONVERSION RATIO                      ',F14.3)        LIS  610
  870 FORMAT ('   SOURCE NEUTR./FISSILE ABS.            ',F14.3)        LIS  620
  880 FORMAT ('   CAPTURE/FISSION IN FISS.MAT.          ',F14.3)        LIS  630
  120 FORMAT ('   FAST DOSIS SPENT FUEL ELEM.   E+21/CM2',F13.2)        LIS  640
  550 FORMAT (/' FUEL INVENTORY (KG/GW(TH)):')                          LIS  650
  570 FORMAT ('              TH-232                     ',F13.2)        LIS  660
  551 FORMAT ('              TH-233                     ',F13.2)        LIS  670
  800 FORMAT ('              PA-233                     ',F13.2)        LIS  680
  580 FORMAT ('              U -233                     ',F13.2)        LIS  690
  810 FORMAT ('              U -234                     ',F13.2)        LIS  700
  590 FORMAT ('              U -235                     ',F13.2)        LIS  710
  820 FORMAT ('              U -236                     ',F13.2)        LIS  720
  821 FORMAT ('              U -237                     ',F13.2)        LIS  730
  790 FORMAT ('              U -238                     ',F13.2)        LIS  740
  791 FORMAT ('              U -239                     ',F13.2)        LIS  750
  860 FORMAT ('              NP-237                     ',F13.2)        LIS  760
  861 FORMAT ('              NP-238                     ',F13.2)        LIS  770
  830 FORMAT ('              NP-239                     ',F13.2)        LIS  780
  831 FORMAT ('              NP-240                     ',F13.2)        LIS  790
  832 FORMAT ('              PU-238                     ',F13.2)        LIS  800
  600 FORMAT ('              PU-239                     ',F13.2)        LIS  810
  840 FORMAT ('              PU-240                     ',F13.2)        LIS  820
  610 FORMAT ('              PU-241                     ',F13.2)        LIS  830
  850 FORMAT ('              PU-242                     ',F13.2)        LIS  840
  851 FORMAT ('              PU-243                     ',F13.2)        LIS  850
  852 FORMAT ('              AM-241                     ',F13.2)        LIS  860
  853 FORMAT ('              AM-242M                    ',F13.2)        LIS  870
  854 FORMAT ('              AM-242                     ',F13.2)        LIS  880
  855 FORMAT ('              AM-243                     ',F13.2)        LIS  890
  856 FORMAT ('              AM-244                     ',F13.2)        LIS  900
  857 FORMAT ('              CM-242                     ',F13.2)        LIS  910
  858 FORMAT ('              CM-243                     ',F13.2)        LIS  920
  859 FORMAT ('              CM-244                     ',F13.2)        LIS  930
  560 FORMAT ('              HEAVY METAL                ',F13.2)        LIS  940
  620 FORMAT (/' FUEL SUPPLY - DISCHARGE (KG/GWD(TH)):')                LIS  950
  670 FORMAT ('              TH-232                     ',F8.4,' -',F8.4LIS  960
     1)                                                                 LIS  970
  671 FORMAT ('              PA-233                     ',F8.4,' -',F8.4LIS  980
     1)                                                                 LIS  990
  630 FORMAT ('              U -233                     ',F8.4,' -',F8.4LIS 1000
     1)                                                                 LIS 1010
  631 FORMAT ('              U -234                     ',F8.4,' -',F8.4LIS 1020
     1)                                                                 LIS 1030
  640 FORMAT ('              U -235                     ',F8.4,' -',F8.4LIS 1040
     1)                                                                 LIS 1050
  690 FORMAT ('              U -236                     ',F8.4,' -',F8.4LIS 1060
     1)                                                                 LIS 1070
  680 FORMAT ('              U -238                     ',F8.4,' -',F8.4LIS 1080
     1)                                                                 LIS 1090
  681 FORMAT ('              NP-237                     ',F8.4,' -',F8.4LIS 1100
     1)                                                                 LIS 1110
  682 FORMAT ('              PU-238                     ',F8.4,' -',F8.4LIS 1120
     1)                                                                 LIS 1130
  650 FORMAT ('              PU-239                     ',F8.4,' -',F8.4LIS 1140
     1)                                                                 LIS 1150
  700 FORMAT ('              PU-240                     ',F8.4,' -',F8.4LIS 1160
     1)                                                                 LIS 1170
  660 FORMAT ('              PU-241                     ',F8.4,' -',F8.4LIS 1180
     1)                                                                 LIS 1190
  710 FORMAT ('              PU-242                     ',F8.4,' -',F8.4LIS 1200
     1)                                                                 LIS 1210
  711 FORMAT ('              AM-241                     ',F8.4,' -',F8.4LIS 1220
     1)                                                                 LIS 1230
  712 FORMAT ('              AM-242M                    ',F8.4,' -',F8.4LIS 1240
     1)                                                                 LIS 1250
  713 FORMAT ('              AM-242                     ',F8.4,' -',F8.4LIS 1260
     1)                                                                 LIS 1270
  714 FORMAT ('              AM-243                     ',F8.4,' -',F8.4LIS 1280
     1)                                                                 LIS 1290
  715 FORMAT ('              CM-242                     ',F8.4,' -',F8.4LIS 1300
     1)                                                                 LIS 1310
  716 FORMAT ('              CM-243                     ',F8.4,' -',F8.4LIS 1320
     1)                                                                 LIS 1330
  717 FORMAT ('              CM-244                     ',F8.4,' -',F8.4LIS 1340
     1)                                                                 LIS 1350
  720 FORMAT (/' U3O8 REQUIREMENT               KG/GWD(T)',F7.1,2X,'-', LIS 1360
     1 F6.1)                                                            LIS 1370
  730 FORMAT (' SEPARATIVE WORK            KG SWU/GWD(T)',F7.1,2X,'-',  LIS 1380
     1 F6.1)                                                            LIS 1390
  740 FORMAT (/' ECONOMY:')                                             LIS 1400
  750 FORMAT ('   FISSILE AND FERTILE MATERIAL ',A4,'/KWHE',F14.3)      LIS 1410
  760 FORMAT ('   FABRICATION, REPROCESSING    ',A4,'/KWHE',F14.3)      LIS 1420
  770 FORMAT ('   (REPROCESSING ONLY           ',A4,'/KWHE',F14.3,' )') LIS 1430
  780 FORMAT ('   FUEL CYCLE COSTS             ',A4,'/KWHE',F14.3)      LIS 1440
C                                                                       LIS 1450
C                                                                       LIS 1460
      IF(PRO(59) .NE. 0.) PRO(191) = 100. / PRO(59)                     LIS 1470
      DO 1 I=1,2                                                        LIS 1480
        WRITE (6,10)                                                    LIS 1490
        WRITE (6,20) (INZW(J),J=5,7),INZW(3),INZW(4)                    LIS 1500
C                                                                       LIS 1510
        CALL HEAD(3)                                                    LIS 1520
C                                                                       LIS 1530
        WRITE (6,890) INZW(4)                                           LIS 1540
        WRITE (6,30)                                                    LIS 1550
        WRITE (6,40) PRO(28)                                            LIS 1560
        WRITE (6,881) PRO(176)                                          LIS 1570
        WRITE (6,90) PRO(1)                                             LIS 1580
        IF(PRO(46) .GT. 0.) WRITE (6,100) PRO(46)                       LIS 1590
        WRITE (6,110)                                                   LIS 1600
        IF(PRO(40) .LE. 0.) GOTO 5                                      LIS 1610
        WRITE (6,130) PRO(40)                                           LIS 1620
        WRITE (6,140) PRO(41)                                           LIS 1630
        WRITE (6,150) PRO(42)                                           LIS 1640
        IF(PRO(205) .GT. 0.) WRITE (6,155) PRO(205)                     LIS 1650
        WRITE (6,160) N26                                               LIS 1660
        WRITE (6,170) PRO(43)                                           LIS 1670
        WRITE (6,180) PRO(44)                                           LIS 1680
        WRITE (6,190) PRO(45)                                           LIS 1690
        IF(PRO(206) .GT. 0.) WRITE (6,195) PRO(206)                     LIS 1700
        IF(PRO(207) .GT. 0.) WRITE (6,196) PRO(207)                     LIS 1710
        IF(PRO(208) .GT. 0.) WRITE (6,197) PRO(208)                     LIS 1720
    5   CONTINUE                                                        LIS 1730
        IF(PRO(4) .EQ. 0.) GOTO 2                                       LIS 1740
        WRITE (6,200)                                                   LIS 1750
        WRITE (6,210) LASTPR,PRO(4)                                     LIS 1760
    2   CONTINUE                                                        LIS 1770
        WRITE (6,220)                                                   LIS 1780
        L = 126                                                         LIS 1790
        DO 240 K=1,IACT                                                 LIS 1800
          M = IMAT(K)                                                   LIS 1810
          IF(PRO(L+K) .GE. 0.01) WRITE (6,230) T1(M),T2(M),PRO(L+K)     LIS 1820
  240   CONTINUE                                                        LIS 1830
        WRITE (6,360) PRO(20)                                           LIS 1840
        WRITE (6,370) PRO(59)                                           LIS 1850
        L = 96                                                          LIS 1860
        DO 390 K=1,IACT                                                 LIS 1870
          M = IMAT(K)                                                   LIS 1880
          IF(PRO(L+K) .GE. 0.01) WRITE (6,380) T1(M),T2(M),PRO(L+K)     LIS 1890
  390   CONTINUE                                                        LIS 1900
        WRITE (6,510) PRO(21)                                           LIS 1910
        WRITE (6,520) PRO(19)                                           LIS 1920
        IF(PRO(2) .LE. 0.) GOTO 6                                       LIS 1930
        WRITE (6,530) PRO(2)                                            LIS 1940
        WRITE (6,540) PRO(3)                                            LIS 1950
        GOTO 7                                                          LIS 1960
    6   CONTINUE                                                        LIS 1970
        WRITE (6,545) PRO(3)                                            LIS 1980
    7   CONTINUE                                                        LIS 1990
        WRITE (6,10)                                                    LIS 2000
        WRITE (6,900) INZW(3)                                           LIS 2010
        WRITE (6,30)                                                    LIS 2020
        WRITE (6,50) PRO(27)                                            LIS 2030
        WRITE (6,60) PRO(37)                                            LIS 2040
        WRITE (6,70) PRO(38)                                            LIS 2050
        WRITE (6,80) PRO(26)                                            LIS 2060
        WRITE (6,870) PRO(191)                                          LIS 2070
        WRITE (6,880) PRO(190)                                          LIS 2080
        WRITE (6,120) PRO(39)                                           LIS 2090
        WRITE (6,550)                                                   LIS 2100
        IF(PRO(211) .GE. 0.01) WRITE (6,570) PRO(211)                   LIS 2110
        IF(PRO(212) .GE. 0.01) WRITE (6,551) PRO(212)                   LIS 2120
        IF(PRO(213) .GE. 0.01) WRITE (6,800) PRO(213)                   LIS 2130
        IF(PRO(214) .GE. 0.01) WRITE (6,580) PRO(214)                   LIS 2140
        IF(PRO(215) .GE. 0.01) WRITE (6,810) PRO(215)                   LIS 2150
        IF(PRO(216) .GE. 0.01) WRITE (6,590) PRO(216)                   LIS 2160
        IF(PRO(217) .GE. 0.01) WRITE (6,820) PRO(217)                   LIS 2170
        IF(PRO(218) .GE. 0.01) WRITE (6,821) PRO(218)                   LIS 2180
        IF(PRO(219) .GE. 0.01) WRITE (6,790) PRO(219)                   LIS 2190
        IF(PRO(220) .GE. 0.01) WRITE (6,791) PRO(220)                   LIS 2200
        IF(PRO(221) .GE. 0.01) WRITE (6,860) PRO(221)                   LIS 2210
        IF(PRO(222) .GE. 0.01) WRITE (6,861) PRO(222)                   LIS 2220
        IF(PRO(223) .GE. 0.01) WRITE (6,830) PRO(223)                   LIS 2230
        IF(PRO(224) .GE. 0.01) WRITE (6,831) PRO(224)                   LIS 2240
        IF(PRO(225) .GE. 0.01) WRITE (6,832) PRO(225)                   LIS 2250
        IF(PRO(226) .GE. 0.01) WRITE (6,600) PRO(226)                   LIS 2260
        IF(PRO(227) .GE. 0.01) WRITE (6,840) PRO(227)                   LIS 2270
        IF(PRO(228) .GE. 0.01) WRITE (6,610) PRO(228)                   LIS 2280
        IF(PRO(229) .GE. 0.01) WRITE (6,850) PRO(229)                   LIS 2290
        IF(PRO(230) .GE. 0.01) WRITE (6,851) PRO(230)                   LIS 2300
        IF(PRO(231) .GE. 0.01) WRITE (6,852) PRO(231)                   LIS 2310
        IF(PRO(232) .GE. 0.01) WRITE (6,853) PRO(232)                   LIS 2320
        IF(PRO(233) .GE. 0.01) WRITE (6,854) PRO(233)                   LIS 2330
        IF(PRO(234) .GE. 0.01) WRITE (6,855) PRO(234)                   LIS 2340
        IF(PRO(235) .GE. 0.01) WRITE (6,856) PRO(235)                   LIS 2350
        IF(PRO(236) .GE. 0.01) WRITE (6,857) PRO(236)                   LIS 2360
        IF(PRO(237) .GE. 0.01) WRITE (6,858) PRO(237)                   LIS 2370
        IF(PRO(238) .GE. 0.01) WRITE (6,859) PRO(238)                   LIS 2380
        WRITE (6,560) PRO(95)                                           LIS 2390
        WRITE (6,620)                                                   LIS 2400
        IF(PRO(61) .GE. 0.0001 .OR. PRO(241) .GE. 0.0001) WRITE(6,670)  LIS 2410
     1   PRO(61),PRO(241)                                               LIS 2420
        IF(PRO(62) .GE. 0.0001 .OR. PRO(242) .GE. 0.0001) WRITE(6,671)  LIS 2430
     1   PRO(62),PRO(242)                                               LIS 2440
        IF(PRO(63) .GE. 0.0001 .OR. PRO(243) .GE. 0.0001) WRITE(6,630)  LIS 2450
     1   PRO(63),PRO(243)                                               LIS 2460
        IF(PRO(64) .GE. 0.0001 .OR. PRO(244) .GE. 0.0001) WRITE(6,631)  LIS 2470
     1   PRO(64),PRO(244)                                               LIS 2480
        IF(PRO(65) .GE. 0.0001 .OR. PRO(245) .GE. 0.0001) WRITE(6,640)  LIS 2490
     1   PRO(65),PRO(245)                                               LIS 2500
        IF(PRO(66) .GE. 0.0001 .OR. PRO(246) .GE. 0.0001) WRITE(6,690)  LIS 2510
     1   PRO(66),PRO(246)                                               LIS 2520
        IF(PRO(67) .GE. 0.0001 .OR. PRO(247) .GE. 0.0001) WRITE(6,680)  LIS 2530
     1   PRO(67),PRO(247)                                               LIS 2540
        IF(PRO(68) .GE. 0.0001 .OR. PRO(248) .GE. 0.0001) WRITE(6,681)  LIS 2550
     1   PRO(68),PRO(248)                                               LIS 2560
        IF(PRO(69) .GE. 0.0001 .OR. PRO(249) .GE. 0.0001) WRITE(6,682)  LIS 2570
     1   PRO(69),PRO(249)                                               LIS 2580
        IF(PRO(70) .GE. 0.0001 .OR. PRO(250) .GE. 0.0001) WRITE(6,650)  LIS 2590
     1   PRO(70),PRO(250)                                               LIS 2600
        IF(PRO(71) .GE. 0.0001 .OR. PRO(251) .GE. 0.0001) WRITE(6,700)  LIS 2610
     1   PRO(71),PRO(251)                                               LIS 2620
        IF(PRO(72) .GE. 0.0001 .OR. PRO(252) .GE. 0.0001) WRITE(6,660)  LIS 2630
     1   PRO(72),PRO(252)                                               LIS 2640
        IF(PRO(73) .GE. 0.0001 .OR. PRO(253) .GE. 0.0001) WRITE(6,710)  LIS 2650
     1   PRO(73),PRO(253)                                               LIS 2660
        IF(PRO(74) .GE. 0.0001 .OR. PRO(254) .GE. 0.0001) WRITE(6,711)  LIS 2670
     1   PRO(74),PRO(254)                                               LIS 2680
        IF(PRO(75) .GE. 0.0001 .OR. PRO(255) .GE. 0.0001) WRITE(6,712)  LIS 2690
     1   PRO(75),PRO(255)                                               LIS 2700
        IF(PRO(76) .GE. 0.0001 .OR. PRO(256) .GE. 0.0001) WRITE(6,713)  LIS 2710
     1   PRO(76),PRO(256)                                               LIS 2720
        IF(PRO(77) .GE. 0.0001 .OR. PRO(257) .GE. 0.0001) WRITE(6,714)  LIS 2730
     1   PRO(77),PRO(257)                                               LIS 2740
        IF(PRO(78) .GE. 0.0001 .OR. PRO(258) .GE. 0.0001) WRITE(6,715)  LIS 2750
     1   PRO(78),PRO(258)                                               LIS 2760
        IF(PRO(79) .GE. 0.0001 .OR. PRO(259) .GE. 0.0001) WRITE(6,716)  LIS 2770
     1   PRO(79),PRO(259)                                               LIS 2780
        IF(PRO(80) .GE. 0.0001 .OR. PRO(260) .GE. 0.0001) WRITE(6,717)  LIS 2790
     1   PRO(80),PRO(260)                                               LIS 2800
        WRITE (6,720) PRO(82),PRO(83)                                   LIS 2810
        WRITE (6,730) PRO(81),PRO(96)                                   LIS 2820
        WRITE (6,740)                                                   LIS 2830
        WRITE (6,750) LISTEQ,PRO(23)                                    LIS 2840
        WRITE (6,760) LISTEQ,PRO(24)                                    LIS 2850
        WRITE (6,770) LISTEQ,PRO(60)                                    LIS 2860
        WRITE (6,780) LISTEQ,PRO(25)                                    LIS 2870
    1 CONTINUE                                                          LIS 2880
      WRITE (6,10)                                                      LIS 2890
      DO 3 J=2,4                                                        LIS 2900
        INZW(J+6) = 0                                                   LIS 2910
        IF(J .NE. 3) INZW(J) = 0                                        LIS 2920
    3 CONTINUE                                                          LIS 2930
      DO 4 J=1,300                                                      LIS 2940
        IF(J .EQ. 38 .OR. J .EQ. 46 .OR. J .EQ. 200 .OR. J .EQ. 299)    LIS 2950
     1   GOTO 4                                                         LIS 2960
        PRO(J) = 0.                                                     LIS 2970
    4 CONTINUE                                                          LIS 2980
      INZWX = 0                                                         LIS 2990
      INZWXX = 0                                                        LIS 3000
      RETURN                                                            LIS 3010
      END                                                               LIS 3020
      SUBROUTINE RAFLUC                                                 RAF   10
C                                                                       RAF   20
C     MAXIMUM NEUTRON FLUX AT THE REFLECTORS IN CASE OF CITATION        RAF   30
C                                                                       RAF   40
CFZJ039 Increase dimensions of arrays NER,INNO,XTR1,XTR2,NXTR1,NXTR2    RAF   50
C                                                             03.01.05  RAF   60
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,RAF   70
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   RAF   80
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), RAF   90
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    RAF  100
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    RAF  110
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   RAF  120
     6 IXPUT(9999),XPUT(9999)                                           RAF  130
C                                                                       RAF  140
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    RAF  150
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    RAF  160
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIRAF  170
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 RAF  180
C                                                                       RAF  190
      EQUIVALENCE(JTPE2,NS),(JTPE3,NT)                                  RAF  200
C                                                                       RAF  210
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300),FABC(10),REPC(10),NNERAF  220
     1 ,LISTEQ,NTIK,K2,NRC,NRL,NRO                                      RAF  230
C                                                                       RAF  240
      COMMON /ILRTB/ ITR,IBR,JRR,JIR                                    RAF  250
C                                                                       RAF  260
      DIMENSION IT(8)                                                   RAF  270
C                                                                       RAF  280
      REAL*8 F(100,200),FT(8)                                           RAF  290
C                                                                       RAF  300
   10 FORMAT (12I6)                                                     RAF  310
   20 FORMAT (' TOP    REFLECTOR BOUNDARY BETWEEN THE ROWS   ',I4,' AND'RAF  320
     1 ,I4,' AT THE COLUMN',I4,' GRP. 1:',G12.5)                        RAF  330
   30 FORMAT (' TOP    REFLECTOR BOUNDARY BETWEEN THE ROWS   ',I4,' AND'RAF  340
     1 ,I4,' AT THE COLUMN',I4,' GRP.',I2,':',G12.5)                    RAF  350
   40 FORMAT (' BOTTOM REFLECTOR BOUNDARY BETWEEN THE ROWS   ',I4,' AND'RAF  360
     1 ,I4,' AT THE COLUMN',I4,' GRP. 1:',G12.5)                        RAF  370
   50 FORMAT (' BOTTOM REFLECTOR BOUNDARY BETWEEN THE ROWS   ',I4,' AND'RAF  380
     1 ,I4,' AT THE COLUMN',I4,' GRP.',I2,':',G12.5)                    RAF  390
   60 FORMAT (' OUTER  REFLECTOR BOUNDARY BETWEEN THE COLUMNS',I4,' AND'RAF  400
     1 ,I4,' AT THE ROW   ',I4,' GRP. 1:',G12.5)                        RAF  410
   70 FORMAT (' OUTER  REFLECTOR BOUNDARY BETWEEN THE COLUMNS',I4,' AND'RAF  420
     1 ,I4,' AT THE ROW   ',I4,' GRP.',I2,':',G12.5)                    RAF  430
   85 FORMAT (' INNER  REFLECTOR BOUNDARY BETWEEN THE COLUMNS',I4,' AND'RAF  440
     1 ,I4,' AT THE ROW   ',I4,' GRP. 1:',G12.5)                        RAF  450
   90 FORMAT (' INNER  REFLECTOR BOUNDARY BETWEEN THE COLUMNS',I4,' AND'RAF  460
     1 ,I4,' AT THE ROW   ',I4,' GRP.',I2,':',G12.5)                    RAF  470
   80 FORMAT (/5X,'MAXIMUM NEUTRON FLUX:'/)                             RAF  480
C                                                                       RAF  490
C                                                                       RAF  500
C     ITR = LOWEST ROW OF THE TOP REFLECTOR                             RAF  510
C     IBR = HIGHEST ROW OF THE BOTTOM REFLECTOR                         RAF  520
C     JIR = OUTER COLUMN OF THE CENTRAL RADIAL REFLECTOR                RAF  530
C     JRR = INNER COLUMN OF THE OUTER RADIAL REFLECTOR                  RAF  540
C                                                                       RAF  550
      REWIND IOFLX                                                      RAF  560
      IF(INZWX .EQ. 0) GOTO 99                                          RAF  570
      ITC = ITR + 1                                                     RAF  580
      IBC = IBR - 1                                                     RAF  590
      JRC = JRR - 1                                                     RAF  600
      JIC = JIR + 1                                                     RAF  610
C                                                                       RAF  620
C     FLUX INPUT FROM UNIT IOFLX                                        RAF  630
C                                                                       RAF  640
      DO 1 M=1,8                                                        RAF  650
        FT(M) = 0.                                                      RAF  660
        IT(M) = 0                                                       RAF  670
    1 CONTINUE                                                          RAF  680
      DO 3 K=1,KMAX                                                     RAF  690
        N = 0                                                           RAF  700
        IF(K .EQ. 1) N = -1                                             RAF  710
        READ (IOFLX) ((F(J,I),J=1,JMAX),I=1,IMAX)                       RAF  720
        IF(K .NE. 1 .AND. K .NE. KMAX) GOTO 3                           RAF  730
C                                                                       RAF  740
C     MAXIMUM AT THE TOP REFLECTOR                                      RAF  750
C                                                                       RAF  760
        N = N + 2                                                       RAF  770
        DO 2 J=1,JMAX                                                   RAF  780
          IF(F(J,ITC) .LT. FT(N)) GOTO 2                                RAF  790
          FT(N) = F(J,ITC)                                              RAF  800
          IT(N) = J                                                     RAF  810
    2   CONTINUE                                                        RAF  820
        IM = IT(N)                                                      RAF  830
        FT(N) = (FT(N)+F(IM,ITR)) / 2.                                  RAF  840
C                                                                       RAF  850
C     MAXIMUM AT THE BOTTOM REFLECTOR                                   RAF  860
C                                                                       RAF  870
        N = N + 2                                                       RAF  880
        DO 4 J=1,JMAX                                                   RAF  890
          IF(F(J,IBC) .LT. FT(N)) GOTO 4                                RAF  900
          FT(N) = F(J,IBC)                                              RAF  910
          IT(N) = J                                                     RAF  920
    4   CONTINUE                                                        RAF  930
        IM = IT(N)                                                      RAF  940
        FT(N) = (FT(N)+F(IM,IBR)) / 2.                                  RAF  950
C                                                                       RAF  960
C     MAXIMUM AT THE RADIAL REFLECTOR                                   RAF  970
C                                                                       RAF  980
        N = N + 2                                                       RAF  990
        DO 5 I=1,IMAX                                                   RAF 1000
          IF(F(JRC,I) .LT. FT(N)) GOTO 5                                RAF 1010
          FT(N) = F(JRC,I)                                              RAF 1020
          IT(N) = I                                                     RAF 1030
    5   CONTINUE                                                        RAF 1040
        IM = IT(N)                                                      RAF 1050
        FT(N) = (FT(N)+F(JRR,IM)) / 2.                                  RAF 1060
C                                                                       RAF 1070
C     MAXIMUM AT THE INNER COLUMN                                       RAF 1080
C                                                                       RAF 1090
        IF(JIR .EQ. 0) GOTO 3                                           RAF 1100
        N = N + 2                                                       RAF 1110
        DO 7 I=1,IMAX                                                   RAF 1120
          IF(F(JIC,I) .LT. FT(N)) GOTO 7                                RAF 1130
          FT(N) = F(JIC,I)                                              RAF 1140
          IT(N) = I                                                     RAF 1150
    7   CONTINUE                                                        RAF 1160
        IM = IT(N)                                                      RAF 1170
        FT(N) = (FT(N)+F(JIR,IM)) / 2.                                  RAF 1180
    3 CONTINUE                                                          RAF 1190
C                                                                       RAF 1200
C     OUTPUT                                                            RAF 1210
C                                                                       RAF 1220
      WRITE (NT,80)                                                     RAF 1230
      WRITE (NT,20) ITR,ITC,IT(1),FT(1)                                 RAF 1240
      WRITE (NT,30) ITR,ITC,IT(2),N26,FT(2)                             RAF 1250
      WRITE (NT,40) IBC,IBR,IT(3),FT(3)                                 RAF 1260
      WRITE (NT,50) IBC,IBR,IT(4),N26,FT(4)                             RAF 1270
      WRITE (NT,60) JRC,JRR,IT(5),FT(5)                                 RAF 1280
      WRITE (NT,70) JRC,JRR,IT(6),N26,FT(6)                             RAF 1290
      IF(JIR .EQ. 0) GOTO 8                                             RAF 1300
      WRITE (NT,85) JIR,JIC,IT(7),FT(7)                                 RAF 1310
      WRITE (NT,90) JIR,JIC,IT(8),N26,FT(8)                             RAF 1320
    8 CONTINUE                                                          RAF 1330
C                                                                       RAF 1340
C     BOOKING FOR *LISTE*                                               RAF 1350
C                                                                       RAF 1360
      DOSJAR = 3600. * 24. * 365.                                       RAF 1370
      A = DOSJAR / 10.**21                                              RAF 1380
      C = 10.**14                                                       RAF 1390
      PRO(40) = FT(1) * A                                               RAF 1400
      PRO(41) = FT(3) * A                                               RAF 1410
      PRO(42) = FT(5) * A                                               RAF 1420
      PRO(205) = FT(7) * A                                              RAF 1430
      PRO(43) = FT(2) / C                                               RAF 1440
      PRO(44) = FT(4) / C                                               RAF 1450
      PRO(45) = FT(6) / C                                               RAF 1460
      PRO(206) = FT(8) / C                                              RAF 1470
   99 CONTINUE                                                          RAF 1480
      RETURN                                                            RAF 1490
      END                                                               RAF 1500
      SUBROUTINE DATEMS                                                 EMS   10
C                                                                       EMS   20
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300)                      EMS   30
C                                                                       EMS   40
      COMMON /JAMOTA/ JAHR,MON,ITAG                                     EMS   50
C                                                                       EMS   60
      CHARACTER*2 DATG(4)                                               EMS   70
C                                                                       EMS   80
      CHARACTER*4 DATH                                                  EMS   90
C                                                                       EMS  100
      CHARACTER*8 DATF                                                  EMS  110
C                                                                       EMS  120
      EQUIVALENCE(DATG(1),DATF),(DATH,DATF)                             EMS  130
C                                                                       EMS  140
C                                                                       EMS  150
      CALL DATE_AND_TIME(DATF)                                          EMS  160
C                                                                       EMS  170
      READ (UNIT=DATH,FMT=101) JAHR                                     EMS  180
      READ (UNIT=DATG(3),FMT=100) MON                                   EMS  190
      READ (UNIT=DATG(4),FMT=100) ITAG                                  EMS  200
      INZW(5) = ITAG                                                    EMS  210
      INZW(6) = MON                                                     EMS  220
      INZW(7) = JAHR                                                    EMS  230
      RETURN                                                            EMS  240
  100 FORMAT (4I2)                                                      EMS  250
  101 FORMAT (I4)                                                       EMS  260
      END                                                               EMS  270