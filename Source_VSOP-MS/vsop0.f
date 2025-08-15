      BLOCK DATA                                                        K     10
C                                                                       K     20
      COMMON /NUCNAM/ T1(200),T2(200),GR(2),PU,FL,MATNR(200),WGN        K     30
C                                                                       K     40
      COMMON /LABEL/ ELE,STA                                            K     50
C                                                                       K     60
      COMMON /VSDA4/ NISO(30),IGAM,NRGAM(30),IDORG(200),THEB10,RESB10,  K     70
     1 RFAST                                                            K     80
C                                                                       K     90
      CHARACTER*4 T1 /'H - ','H - ','BE- ','B -(','C   ','TH-2','PA-2', K    100
     1 'U -2','U -2','U -2','U -2','U -2','NP-2','PU-2','PU-2','PU-2',  K    110
     2 'PU-2','FISS','FP-U','FP-U','FP-P','N - ','O - ','MG  ','AL- ',  K    120
     3 'SI  ','CR  ','MN- ','FE-(','CO- ','NI  ','CU  ','SE- ','BR- ',  K    130
     4 'KR- ','KR- ','KR- ','KR- ','RB- ','RB- ','SR- ','SR- ','Y - ',  K    140
     5 'ZR  ','ZR- ','ZR- ','ZR- ','ZR- ','ZR- ','ZR- ','MO  ','MO- ',  K    150
     6 'MO- ','MO- ','MO- ','MO-1','TC- ','RU-1','RU-1','RU-1','RU-1',  K    160
     7 'RH-1','PD-1','PD-1','PD-1','PD-1','PD-1','PD-1','AG-1','IN-1',  K    170
     8 'CD  ','CD-1','CD-1','CD-1','CD-1','CD-1','TE-1','TE-1','TE-1',  K    180
     9 'I -1','I -1','XE-1','XE-1','XE-1','XE-1','XE-1','XE-1','XE-1',  K    190
     X 'CS-1','CS-1','CS-1','BA-1','BA-1','BA-1','BA-1','LA-1','CE-1',  K    200
     Y 'CE-1','PR-1','ND-1','ND-1','ND-1','ND-1','ND-1','ND-1','ND-1',  K    210
     Z 'PM-1','SM-1','SM-1','SM-1','SM-1','SM-1','SM-1','SM-1','EU-1',  K    220
     Z 'EU-1','EU-1','EU-1','GD-1','GD-1','GD-1','GD-1','GD-1','TB-1',  K    230
     Z 'AU-1','PB  ','BI-2','LI- ','LI- ','B - ','N - ','U -2','NP-2',  K    240
     Z 'V+CR','V   ','NB+Z','TI  ','ZRY ','AG-1','NB- ','WOLF','RU-1',  K    250
     Z 'RH-1','CS-1','CE-1','PR-1','PM-1','PM-1','ZR- ','C -P','RU-1',  K    260
     Z 'XE-1','CE-1','PR-1','PM-1','I -1','    ','    ','    ','FP44',  K    270
     Z 'FP39','FP34','FP29','B - ','HF-1','HF-1','HF-1','HF-1','HF-1',  K    280
     Z 'HF-1','W -1','W -1','W -1','W -1','PM-1','U -2','PU-2','AM-2',  K    290
     Z 'AM-2','AM-2','AM-2','CM-2','CM-2','CM-2','TH-2','U -2','NP-2',  K    300
     Z 'NP-2','PU-2','AM-2','    ','    ','    ','    ','    ','    ',  K    310
     Z '    ','    ','    ','    '/                                     K    320
C                                                                       K    330
      CHARACTER*4 T2 /' 1  ',' 2  ',' 9  ','NAT)','    ','32  ','33  ', K    340
     1 '33  ','34  ','35  ','36  ','38  ','39  ','39  ','40  ','41  ',  K    350
     2 '42  ','.PR.','235 ','233 ','U239','14  ','16  ','    ','27  ',  K    360
     3 '    ','    ','55  ','NAT)','59  ','    ','    ','82  ','81  ',  K    370
     4 '83  ','84  ','85  ','86  ','85  ','87  ','88  ','90  ','89  ',  K    380
     5 '    ','90  ','91  ','92  ','93  ','94  ','96  ','    ','95  ',  K    390
     6 '96  ','97  ','98  ','00  ','99  ','00  ','01  ','02  ','04  ',  K    400
     7 '03  ','04  ','05  ','06  ','07  ','08  ','10  ','09  ','15  ',  K    410
     8 '    ','10  ','11  ','12  ','13  ','14  ','26  ','28  ','30  ',  K    420
     9 '27  ','29  ','28  ','30  ','31  ','32  ','34  ','35  ','36  ',  K    430
     X '33  ','35  ','37  ','34  ','36  ','37  ','38  ','39  ','40  ',  K    440
     Y '42  ','41  ','42  ','43  ','44  ','45  ','46  ','48  ','50  ',  K    450
     Z '47  ','47  ','48  ','49  ','50  ','51  ','52  ','54  ','51  ',  K    460
     Z '53  ','54  ','55  ','54  ','55  ','56  ','57  ','58  ','59  ',  K    470
     Z '97  ','    ','09  ',' 6  ',' 7  ','10  ','14  ','37  ','37  ',  K    480
     Z '-STR','    ','R-ST','    ','= ZR','07  ','93  ','RAM ','05  ',  K    490
     Z '05  ','34  ','44  ','42  ','48G ','48M ','95  ','OIS.','03  ',  K    500
     Z '33  ','41  ','43  ','49  ','31  ','    ','    ','    ','    ',  K    510
     Z '    ','    ','    ','11  ','74  ','76  ','77  ','78  ','79  ',  K    520
     Z '80  ','82  ','83  ','84  ','86  ','51  ','32  ','38  ','41  ',  K    530
     Z '42  ','42M ','43  ','42  ','43  ','44  ','33  ','39  ','38  ',  K    540
     Z '40  ','43  ','44  ','    ','    ','    ','    ','    ','    ',  K    550
     Z '    ','    ','    ','    '/                                     K    560
C                                                                       K    570
      CHARACTER*4 GR /' GRO','UP  '/,PU /'....'/,FL /'FLUX'/            K    580
C                                                                       K    590
      CHARACTER*2 ELE(99),STA(2)                                        K    600
C                                                                       K    610
      DATA ELE /' H','HE','LI','BE',' B',' C',' N',' O',' F','NE','NA', K    620
     1 'MG','AL','SI',' P',' S','CL','AR',' K','CA','SC','TI',' V','CR',K    630
     2 'MN','FE','CO','NI','CU','ZN','GA','GE','AS','SE','BR','KR','RB',K    640
     3 'SR',' Y','ZR','NB','MO','TC','RU','RH','PD','AG','CD','IN','SN',K    650
     4 'SB','TE',' I','XE','CS','BA','LA','CE','PR','ND','PM','SM','EU',K    660
     5 'GD','TB','DY','H0','ER','TM','YB','LU','HF','TA',' W','RE','OS',K    670
     6 'IR','PT','AU','HG','TL','PB','BI','PO','AT','RN','FR','RA','AC',K    680
     7 'TH','PA',' U','NP','PU','AM','CM','BK','CF','ES'/               K    690
C                                                                       K    700
      DATA STA /'  ','M '/                                              K    710
C                                                                       K    720
      DATA NRGAM /6,185,7,8,9,10,11,132,12,186,133,187,13,188,177,14,15,K    730
     1 16,17,189,178,180,179,181,190,182,183,184,0,0/                   K    740
C                                                                       K    750
C     THE ARRAY "IDORG" CONTAINS IDENTIFICATION NUMBERS OF ISOTOPES     K    760
C     IN "ORIGEN"-NOTATION. THE INDEX "I" OF ARRAY "IDORG(I)" EQUALS    K    770
C     THE IDENTIFICATION NUMBER OF THE CORRESPONDING ISOTOPE WITHIN     K    780
C     THE "GAM"-CROSS SECTION LIBRARY.                                  K    790
C     A NEGATIVE NUMBER INDICATES AN ELEMENT INSTEAD OF AN ISOTOPE.     K    800
C                                                                       K    810
      DATA IDORG /10010,  10020,  40090, -50000,  60120, 902320, 912330,K    820
     1   922330, 922340, 922350, 922360, 922380, 932390, 942390, 942400,K    830
     2   942410, 942420,      0,      0,      0,      0,  70140,  80160,K    840
     3  -120000, 130270,-140000,-240000, 250550,-260000, 270590,-280000,K    850
     4  -290000, 340820, 350810, 360830, 360840, 360850, 360860, 370850,K    860
     5   370870, 380880, 380900, 390890,-400000, 400900, 400910, 400920,K    870
     6   400930, 400940, 400960,-420000, 420950, 420960, 420970, 420980,K    880
     7   421000, 430990, 441000, 441010, 441020, 441040, 451030, 461040,K    890
     8   461050, 461060, 461070, 461080, 461100, 471090, 491150,-480000,K    900
     9   481100, 481110, 481120, 481130, 481140, 521260, 521280, 521300,K    910
     X   531270, 531290, 541280, 541300, 541310, 541320, 541340, 541350,K    920
     Y   541360, 551330, 551350, 551370, 561340, 561360, 561370, 561380,K    930
     Z   571390, 581400, 581420, 591410, 601420, 601430, 601440, 601450,K    940
     Z   601460, 601480, 601500, 611470, 621470, 621480, 621490, 621500,K    950
     Z   621510, 621520, 621540, 631510, 631530, 631540, 631550, 641540,K    960
     Z   641550, 641560, 641570, 641580, 651590, 791970,-820000, 832090,K    970
     Z    30060,  30070,  50100,  70140, 922370, 932370,      0,-230000,K    980
     Z        0,-220000,      0, 471070, 410930,      0, 441050, 451050,K    990
     Z   551340, 581440, 591420, 611480, 611481, 400950,      0, 441030,K   1000
     Z   541330, 581410, 591430, 611490, 531310,      0,      0,      0,K   1010
     Z        0,      0,      0,      0,  50110, 721740, 721760, 721770,K   1020
     Z   721780, 721790, 721800, 741820, 741830, 741840, 741860, 611510,K   1030
     Z   922320, 942380, 952410, 952420, 952421, 952430, 962420, 962430,K   1040
     Z   962440, 902330, 922390, 932380, 932400, 942430, 952440,      0,K   1050
     Z        0,      0,      0,      0,      0,      0,      0,      0,K   1060
     Z        0/                                                        K   1070
C                                                                       K   1080
      REAL WGN(200) /5*0.,232.111,233.114,233.112,234.114,235.117,      K   1090
     1 236.120,238.125,239.123,239.127,240.120,241.131,242.134,114*0.,  K   1100
     2 237.,237.,43*0.,238.214,241.,242.,242.,243.,242.,243.,244.,233., K   1110
     3 239.,238.,240.,243.,244.,10*0./                                  K   1120
      END                                                               K   1130
      PROGRAM VSOP_99_11                                                VSO   10
C     (REPORT JUEL-4348, JAN. 2012)                                     VSO   11
C     (REPORT JUEL-4326, JUNE 2010)                                     VSO   20
C                                                                       VSO   30
C     MAIN PROGRAM *** V.S.O.P.                                         VSO   40
C                                                                       VSO   50
C                                                                       VSO   60
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), VSO   70
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10VSO   80
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11VSO   90
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13,JSUM29                  VSO  100
C                                                                       VSO  110
      COMMON /CPU/ T                                                    VSO  120
C                                                                       VSO  130
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    VSO  140
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    VSO  150
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIVSO  160
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D,NLAYP,ITTT, VSO  170
     4 LIMT                                                             VSO  180
C                                                                       VSO  190
      EQUIVALENCE(JTPE2,NS),(JTPE3,NT)                                  VSO  200
C                                                                       VSO  210
      COMMON /VARDIM/ A(8000000)                                        VSO  220
C                                                                       VSO  230
CFZJ048 enlarged dimension                                    11.04.07  VSO  240
      COMMON /VARDIT/ B(5000000)                                        VSO  250
C                                                                       VSO  260
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       VSO  270
C                                                                       VSO  280
CFZJ031                                                       28.05.04  VSO  290
CFZJ062                                                        4.05.11  VSO  300
      COMMON /ORIGEN/ LOB,NOR,VOR(100),ISPK,KFISS,N200C,NXSC,KGR,       VSO  310
     1 LOBN,IEZ,FSP(33),IDOP,JNEU(15),LMAT(15),N34                      VSO  320
C                                                                       VSO  330
CFZJ055                                                       25.09.07  VSO  340
C                                                                       VSO  350
      COMMON /PROZ/ INZWX,INZWXX,INZW(10),PRO(300)                      VSO  360
C                                                                       VSO  370
CFZJ058                                                       05.11.08  VSO  380
      COMMON /KEFFT/ TKEFF(5000),RKEFF(5000),IPKEFF                     VSO  390
C                                                                       VSO  400
      COMMON /CITINP/ INPCIT                                            VSO  410
C                                                                       VSO  420
CFZJ042                                                       09.09.05  VSO  430
      COMMON /IWAGA/ IWAGAM                                             VSO  440
C                                                                       VSO  450
CFZJ042                                                       09.09.05  VSO  460
      COMMON /IPRP/ IPR,MIXTH                                           VSO  470
C                                                                       VSO  480
CFZJ048 enlarged dimension                                    11.04.07  VSO  490
      DIMENSION JDUM3(200),RDUM3(200),JDUM(4000,2),RDUM(4000)           VSO  500
C                                                                       VSO  510
      CHARACTER*4 MODE1/'vsop'/,MODE2/'geom'/,MODE3/'fuel'/,MODE        VSO  520
C                                                                       VSO  530
      CHARACTER*72 A66                                                  VSO  540
C                                                                       VSO  550
      EQUIVALENCE(LI(1),LIMAT),(LI(32),LSS),(LI(3),LOUSI),(LI(4),LTOSI),VSO  560
     1 (LI(5),LABSI),(LI(6),LFISI),(LI(7),LXNU),(LI(10),LIREG),         VSO  570
     2 (LI(17),LSSLU),(LI(28),LNHOT),(LI(29),LVOL),(LI(31),LDEN),       VSO  580
     3 (LI(51),LNOPO),(LI(52),LHMET),(LI(53),LHMNU),(LI(62),LBUCK),     VSO  590
     4 (LI(63),LNDES),(LI(64),LTEMZ),(LI(65),LTCEL),(LI(91),LSGA),      VSO  600
     5 (LI(92),LSGTR),(LI(93),LV1),(LI(94),LFKEN),(LI(95),LBUCL),       VSO  610
     6 (LI(90),LNHOD),(LI(9),LVOLR),(LI(30),LVPAR),(LI(40),LHM),        VSO  620
     7 (LI(41),LTHBU),(LI(42),LFADO),(LI(54),LTCH2),(LI(56),LKRES),     VSO  630
     8 (LI(57),LLRZN),(LI(58),LNTY2),(LI(59),LTCH1),(LI(61),LNTY1),     VSO  640
     9 (LI(146),LJAD1),(LI(148),LKDES),(LI(150),LDMAT),(LI(152),LGS),   VSO  650
     X (LI(183),LMIX)                                                   VSO  660
C                                                                       VSO  670
   11 FORMAT ('1',40X,'OK')                                             VSO  680
   12 FORMAT (' ........ START OF CH-',I2,',  EXECUTION TIME',F10.3,' MIVSO  690
     1NUTES'/)                                                          VSO  700
   40 FORMAT (A72)                                                      VSO  710
   81 FORMAT (4I3)                                                      VSO  720
   83 FORMAT (15I5)                                                     VSO  730
   85 FORMAT (6(F9.3,I3))                                               VSO  740
   86 FORMAT (3E12.5,I5)                                                VSO  750
   88 FORMAT (3(2I6,E12.5))                                             VSO  760
   89 FORMAT (6(I3,F9.3))                                               VSO  770
   90 FORMAT (6(F8.3,I3,I1))                                            VSO  780
   98 FORMAT (//' ***** VARIABLE "MODE" (CARD S) OUT OF RANGE *****'//' VSO  790
     1*****         PROGRAM STOPS                 *****')               VSO  800
  111 FORMAT (18I4)                                                     VSO  810
CFZJ037                                                       30.09.04  VSO  820
  222 FORMAT (A4,3I4)                                                   VSO  830
  555 FORMAT (1H1)                                                      VSO  840
                                                                        VSO  850
C                                                                       VSO  860
      CALL WATCH(ENDE)                                                  VSO  870
C                                                                       VSO  880
      T = ENDE                                                          VSO  890
      NLAYP = 0                                                         VSO  900
      IV = 8000000                                                      VSO  910
CFZJ048 enlarged dimension                                    11.04.07  VSO  920
      IT = 5000000                                                      VSO  930
      INPCIT = 0                                                        VSO  940
CFZJ042                                                       09.09.05  VSO  950
      MIXTH = 0                                                         VSO  960
      IWAGAM = 0                                                        VSO  970
C                                                                       VSO  980
      CALL AJAX(A(1),B(1),IV,IT)                                        VSO  990
C                                                                       VSO 1000
      PRO(200) = 1.E+21                                                 VSO 1010
      JTPE2 = 5                                                         VSO 1020
      JTPE3 = 6                                                         VSO 1030
      IWRITE = 1                                                        VSO 1040
      IREAD = 2                                                         VSO 1050
      OPEN(5,FILE=' ')                                                  VSO 1060
      OPEN(6,FILE=' ')                                                  VSO 1070
C                                                                       VSO 1080
      CALL INPLIST                                                      VSO 1090
C                                                                       VSO 1100
      REWIND 5                                                          VSO 1110
C                                                                       VSO 1120
CARD S1  (STEERING THE EXECUTION MODE)                                  VSO 1130
CFZJ037                                                       30.09.04  VSO 1140
      READ (NS,222) MODE,JSER,I3D,ITTT                                  VSO 1150
C                                                                       VSO 1160
      IF(MODE .EQ. MODE1 .OR. MODE .EQ. MODE2 .OR. MODE .EQ. MODE3) GOTOVSO 1170
     1 99                                                               VSO 1180
      WRITE (6,98)                                                      VSO 1190
      STOP                                                              VSO 1200
   99 CONTINUE                                                          VSO 1210
C                                                                       VSO 1220
CARD S2                                                                 VSO 1230
C                                                                       VSO 1240
      CALL HEAD(1)                                                      VSO 1250
C                                                                       VSO 1260
      IF(MODE .NE. MODE1) GOTO 112                                      VSO 1270
      OPEN(1,STATUS='SCRATCH',FORM='UNFORMATTED')                       VSO 1280
      OPEN(2,STATUS='SCRATCH',FORM='UNFORMATTED')                       VSO 1290
      OPEN(3,STATUS='SCRATCH',FORM='UNFORMATTED')                       VSO 1300
      OPEN(4,STATUS='SCRATCH')                                          VSO 1310
C     OPEN(4,FILE='NUL')                                                VSO 1320
      L8 = 5040                                                         VSO 1330
      OPEN(8,STATUS='SCRATCH',ACCESS='DIRECT',RECL=L8*4)                VSO 1340
      L9 = 5040                                                         VSO 1350
      OPEN(9,STATUS='SCRATCH',ACCESS='DIRECT',RECL=L9*4)                VSO 1360
      L10 = 2520                                                        VSO 1370
      OPEN(10,STATUS='SCRATCH',ACCESS='DIRECT',RECL=L10*4)              VSO 1380
      L11 = 208                                                         VSO 1390
      OPEN(11,STATUS='SCRATCH',ACCESS='DIRECT',RECL=L11*4)              VSO 1400
      OPEN(12,STATUS='SCRATCH',FORM='UNFORMATTED')                      VSO 1410
      L13 = 14                                                          VSO 1420
      OPEN(13,STATUS='SCRATCH',ACCESS='DIRECT',RECL=L13*4)              VSO 1430
      OPEN(21,STATUS='SCRATCH',FORM='UNFORMATTED')                      VSO 1440
      OPEN(22,STATUS='SCRATCH',FORM='UNFORMATTED')                      VSO 1450
      OPEN(23,STATUS='SCRATCH',FORM='UNFORMATTED')                      VSO 1460
      OPEN(24,STATUS='SCRATCH',FORM='UNFORMATTED')                      VSO 1470
      OPEN(26,STATUS='SCRATCH',FORM='UNFORMATTED')                      VSO 1480
      OPEN(27,STATUS='SCRATCH',FORM='UNFORMATTED')                      VSO 1490
      L30 = 73                                                          VSO 1500
      OPEN(30,ACCESS='DIRECT',RECL=L30*4,FILE='Libraries\resint')       VSO 1510
      OPEN(32,STATUS='SCRATCH',FORM='UNFORMATTED')                      VSO 1520
      OPEN(34,STATUS='SCRATCH')                                         VSO 1530
      OPEN(36,STATUS='SCRATCH',FORM='UNFORMATTED')                      VSO 1540
      OPEN(37,FILE='geom')                                              VSO 1550
      OPEN(38,STATUS='SCRATCH',FORM='UNFORMATTED')                      VSO 1560
      OPEN(39,STATUS='SCRATCH',FORM='UNFORMATTED')                      VSO 1570
      OPEN(44,STATUS='SCRATCH',FORM='UNFORMATTED')                      VSO 1580
      OPEN(51,STATUS='SCRATCH',FORM='UNFORMATTED')                      VSO 1590
      OPEN(54,STATUS='SCRATCH',FORM='UNFORMATTED')                      VSO 1600
      OPEN(55,STATUS='SCRATCH',FORM='UNFORMATTED')                      VSO 1610
      OPEN(60,STATUS='SCRATCH',FORM='UNFORMATTED')                      VSO 1620
      OPEN(63,FILE='Libraries\adage')                                   VSO 1630
      OPEN(66,STATUS='SCRATCH')                                         VSO 1640
C                                                                       VSO 1650
CARD S3                                                                 VSO 1660
C                                                                       VSO 1670
      READ (NS,111) JTPE7,JTPE9,IRR9,IPKEFF                             VSO 1680
C                                                                       VSO 1690
      IF(JTPE7 .GT. 0) IPRIN(8) = 14                                    VSO 1700
      IF(JTPE9 .GT. 0) IPRIN(9) = 15                                    VSO 1710
      IF(JTPE7 .GT. 0 .OR. JTPE9 .GT. 0) IPRIN(11) = 20                 VSO 1720
      IF(JTPE7 .GT. 0) OPEN(14,FORM='UNFORMATTED',FILE='rstold')        VSO 1730
      IF(JTPE9 .GT. 0) OPEN(15,FORM='UNFORMATTED',FILE='rstnew')        VSO 1740
  112 CONTINUE                                                          VSO 1750
      IF(NLAYP .LE. 0) NLAYP = 1                                        VSO 1760
      IF(MODE .EQ. MODE3) GOTO 113                                      VSO 1770
C                                                                       VSO 1780
      IF(JTPE7 .LE. 0 .AND. I3D .EQ. 0) CALL BIRGIT(MODE,MODE2,ITTT)    VSO 1790
C                                                                       VSO 1800
      IF(JTPE7 .LE. 0 .AND. I3D .GT. 0) CALL TRIGIT(MODE,MODE2,NLP,I3D) VSO 1810
C                                                                       VSO 1820
      IF(JTPE7 .LE. 0 .AND. I3D .GT. 0) NLAYP = NLP                     VSO 1830
      IF(MODE .EQ. MODE2) STOP                                          VSO 1840
  113 CONTINUE                                                          VSO 1850
      IF(JTPE7 .GT. 0) GOTO 114                                         VSO 1860
      L29 = 200                                                         VSO 1870
      OPEN(29,STATUS='SCRATCH',ACCESS='DIRECT',RECL=L29*4)              VSO 1880
C                                                                       VSO 1890
      CALL DATA2                                                        VSO 1900
C                                                                       VSO 1910
      IF(MODE .EQ. MODE3) STOP                                          VSO 1920
  114 CONTINUE                                                          VSO 1930
C                                                                       VSO 1940
      IF(JTPE7 .LE. 0) CALL DIMDEF(1,IV,IT)                             VSO 1950
C                                                                       VSO 1960
      K08 = 4500                                                        VSO 1970
      K10 = 5000                                                        VSO 1980
      NDA8 = 8                                                          VSO 1990
      NDA9 = 9                                                          VSO 2000
      NDA10 = 10                                                        VSO 2010
      NDA11 = 11                                                        VSO 2020
      NDA13 = 13                                                        VSO 2030
      NDA29 = 29                                                        VSO 2040
      JSUM8 = 1                                                         VSO 2050
      JSUM9 = 1                                                         VSO 2060
      JSUM10 = 1                                                        VSO 2070
      JSUM11 = 1                                                        VSO 2080
      NXT28 = 1                                                         VSO 2090
      NXT29 = 1                                                         VSO 2100
      NXT30 = 1                                                         VSO 2110
      NXT13 = 1                                                         VSO 2120
      N14 = 14                                                          VSO 2130
      ND13 = 360                                                        VSO 2140
      N34 = 34                                                          VSO 2150
      WRITE (NT,555)                                                    VSO 2160
C                                                                       VSO 2170
      CALL DATEMS                                                       VSO 2180
C                                                                       VSO 2190
      CALL COVER(NT)                                                    VSO 2200
C                                                                       VSO 2210
      IF(JTPE7 .LE. 0) GOTO 13                                          VSO 2220
C                                                                       VSO 2230
C     READ RESTART DATA FOR JTPE7 > 0                                   VSO 2240
C                                                                       VSO 2250
      REWIND 37                                                         VSO 2260
      IF(I3D .GT. 0) GOTO 91                                            VSO 2270
      READ (37,81) JDUM4,JDUM1,JDUM2                                    VSO 2280
      READ (37,89) (JDUM3(I),RDUM3(I),I=1,JDUM1-1)                      VSO 2290
      READ (37,89) (JDUM3(I),RDUM3(I),I=1,JDUM2)                        VSO 2300
      READ (37,81) JDUM4                                                VSO 2310
      DO 82 N=1,JDUM2                                                   VSO 2320
        READ (37,83) (JDUM3(I),I=1,JDUM1-1)                             VSO 2330
   82 CONTINUE                                                          VSO 2340
      READ (37,81) JDUM4                                                VSO 2350
      GOTO 93                                                           VSO 2360
   91 CONTINUE                                                          VSO 2370
      READ (37,81) JDUM4,JDUM1,JDUM2,JDUM5                              VSO 2380
      READ (37,89) (JDUM3(I),RDUM3(I),I=1,JDUM1)                        VSO 2390
      READ (37,89) (JDUM3(I),RDUM3(I),I=1,JDUM2)                        VSO 2400
      READ (37,89) (JDUM3(I),RDUM3(I),I=1,JDUM5)                        VSO 2410
      READ (37,81) JDUM4                                                VSO 2420
      DO 92 I=1,JDUM5                                                   VSO 2430
        DO 92 J=1,JDUM2                                                 VSO 2440
          READ (37,83) (JDUM3(K),K=1,JDUM1)                             VSO 2450
   92 CONTINUE                                                          VSO 2460
   93 CONTINUE                                                          VSO 2470
      IF(I3D .GT. 0) GOTO 51                                            VSO 2480
      READ (37,83) IDUM1,IDUM2                                          VSO 2490
      READ (37,85) (RDUM3(I),JDUM3(I),I=1,IDUM1)                        VSO 2500
      READ (37,90) (RDUM3(I),JDUM3(I),JDUM3(I),I=1,IDUM2)               VSO 2510
      READ (37,86) RDUM4,RDUM4,RDUM4,K0                                 VSO 2520
      READ (37,88) (JDUM(I,2),JDUM(I,1),RDUM(I),I=1,K0)                 VSO 2530
   51 READ (37,40,END=52) A66                                           VSO 2540
      WRITE (66,40) A66                                                 VSO 2550
      GOTO 51                                                           VSO 2560
   52 CONTINUE                                                          VSO 2570
      REWIND 66                                                         VSO 2580
      REWIND 37                                                         VSO 2590
      INPCIT = 1                                                        VSO 2600
C                                                                       VSO 2610
      CALL START2                                                       VSO 2620
C                                                                       VSO 2630
      CALL START3(A(KA(LJAD1)))                                         VSO 2640
C                                                                       VSO 2650
      CALL DIMDEF(2,IV,IT)                                              VSO 2660
C                                                                       VSO 2670
   13 CONTINUE                                                          VSO 2680
      MAFIA = 1                                                         VSO 2690
   10 CONTINUE                                                          VSO 2700
C                                                                       VSO 2710
      CALL WATCH(ENDE)                                                  VSO 2720
C                                                                       VSO 2730
      TTIME = ENDE - T                                                  VSO 2740
      IF(IPRIN(1) .GE. 0 .AND. IPRIN(3) .GE. -1) GOTO 115               VSO 2750
      IF(IPRIN(7) .GT. 0) GOTO 116                                      VSO 2760
  115 WRITE (6,12) MAFIA,TTIME/60.                                      VSO 2770
  116 GOTO(1,2,3,4,5,6,7,8,9),MAFIA                                     VSO 2780
    1 CONTINUE                                                          VSO 2790
      NP1 = NENDP + 1                                                   VSO 2800
C                                                                       VSO 2810
      CALL CH1(MAFIA,A(KA(LIREG)),A(KA(LSSLU)),A(KA(LNHOT)),A(KA(LVOL)),VSO 2820
     1 A(KA(LDEN)),A(KA(LNOPO)),A(KA(LHMET)),A(KA(LHMNU)),A(KA(LJAD1)), VSO 2830
     2 A(NP1),A(KA(LMIX)))                                              VSO 2840
C                                                                       VSO 2850
      GOTO 10                                                           VSO 2860
C                                                                       VSO 2870
    2 CONTINUE                                                          VSO 2880
C                                                                       VSO 2890
      CALL CH2(MAFIA,A(KA(LIMAT)),A(KA(LNDES)),A(KA(LTEMZ)),A(KA(LNHOD))VSO 2900
     1 ,A(KA(LKDES)),A(NP1))                                            VSO 2910
C                                                                       VSO 2920
      GOTO 10                                                           VSO 2930
C                                                                       VSO 2940
    3 CONTINUE                                                          VSO 2950
C                                                                       VSO 2960
      CALL CH3(MAFIA,A(KA(LIMAT)),A(KA(LNDES)),A(KA(LTEMZ)),A(KA(LTCEL))VSO 2970
     1 ,A(KA(LNHOD)),A(KA(LBUCL)))                                      VSO 2980
C                                                                       VSO 2990
      GOTO 10                                                           VSO 3000
C                                                                       VSO 3010
    4 CONTINUE                                                          VSO 3020
C                                                                       VSO 3030
      CALL CH4(MAFIA,A(KA(LIMAT)),A(KA(LBUCK)),A(KA(LNDES)),A(KA(LTEMZ))VSO 3040
     1 ,A(KA(LJAD1)),A(KA(LKDES)),A(NP1))                               VSO 3050
C                                                                       VSO 3060
      GOTO 10                                                           VSO 3070
C                                                                       VSO 3080
    5 CONTINUE                                                          VSO 3090
      NP2 = NP1 + KMAT                                                  VSO 3100
      NP3 = NP2 + KMAT * 10                                             VSO 3110
C                                                                       VSO 3120
      CALL CH5(MAFIA,A(KA(LIMAT)),A(KA(LOUSI)),KL(LOUSI),A(KA(LTOSI)),  VSO 3130
     1 A(KA(LABSI)),A(KA(LFISI)),KL(LFISI),A(KA(LXNU)),A(KA(LTCEL)),    VSO 3140
     2 A(KA(LNHOD)),A(KA(LJAD1)),A(NP1),A(NP2),A(NP3))                  VSO 3150
C                                                                       VSO 3160
      IF(LOBN .EQ. 2) KGR = 1                                           VSO 3170
      GOTO 10                                                           VSO 3180
C                                                                       VSO 3190
    6 CONTINUE                                                          VSO 3200
      IF(ITTT .GT. 0) GOTO 14                                           VSO 3210
C                                                                       VSO 3220
      CALL CH6(MAFIA,A(KA(LIREG)),A(KA(LSSLU)),A(KA(LNHOT)),A(KA(LVOL)),VSO 3230
     1 A(KA(LDEN)),A(KA(LSGA)),A(KA(LSGTR)),A(KA(LV1)),A(KA(LFKEN)),    VSO 3240
     2 A(KA(LJAD1)),A(KA(LGS)),A(KA(LHMET)),A(NP1),A(KA(LSS)),          VSO 3250
     3 A(KA(LMIX)))                                                     VSO 3260
C                                                                       VSO 3270
      GOTO 10                                                           VSO 3280
   14 CONTINUE                                                          VSO 3290
      KMAT30 = KMAT + 30                                                VSO 3300
C                                                                       VSO 3310
C     SUBR. CH6TTT ONLY FOR PREPARING A NEW THERMOS-LIBRARY OUT OF      VSO 3320
C     THERMALIZATION                                                    VSO 3330
C                                                                       VSO 3340
      CALL CH6TTT(MAFIA,A(KA(LIMAT)),KMAT30,A(NP1))                     VSO 3350
C                                                                       VSO 3360
      GOTO 10                                                           VSO 3370
C                                                                       VSO 3380
    7 CONTINUE                                                          VSO 3390
C                                                                       VSO 3400
CFZJ012   New identification numbers for THERMOS-cell         02.12.03  VSO 3410
C         definitions for the spectrum zones                  02.12.03  VSO 3420
      NP2 = NP1 + KMAT                                                  VSO 3430
      NP3 = NP2 + KMAT * 10                                             VSO 3440
      CALL CH7(MAFIA,A(KA(LVOLR)),A(KA(LIREG)),A(KA(LSSLU)),A(KA(LNHOT))VSO 3450
     1 ,A(KA(LVOL)),A(KA(LVPAR)),A(KA(LDEN)),A(KA(LHM)),A(KA(LTHBU)),   VSO 3460
     2 A(KA(LFADO)),A(KA(LHMET)),A(KA(LTCH2)),A(KA(LKRES)),A(KA(LLRZN)),VSO 3470
     3 A(KA(LNTY2)),A(KA(LTCH1)),KL(LTCH1),A(KA(LNTY1)),A(KA(LJAD1)),   VSO 3480
     4 A(KA(LDMAT)),A(NP1),A(KA(LSS)),A(NP3))                           VSO 3490
C                                                                       VSO 3500
      GOTO 10                                                           VSO 3510
    8 CONTINUE                                                          VSO 3520
      WRITE (NT,11)                                                     VSO 3530
    9 CONTINUE                                                          VSO 3540
      STOP                                                              VSO 3550
      END                                                               VSO 3560
      SUBROUTINE WATCH(ENDE)                                            WAT   10
C                                                                       WAT   20
C     DEFINITION OF TIME                                                WAT   30
C                                                                       WAT   40
      COMMON /HOUMIN/ IH,IM                                             WAT   50
C                                                                       WAT   60
      CHARACTER*2 TIMF(5)                                               WAT   70
C                                                                       WAT   80
      CHARACTER*6 SC                                                    WAT   90
C                                                                       WAT  100
      CHARACTER*8 DATF                                                  WAT  110
C                                                                       WAT  120
      CHARACTER*10 TIMT                                                 WAT  130
C                                                                       WAT  140
      EQUIVALENCE(TIMF(1),TIMT),(TIMF(3),SC)                            WAT  150
C                                                                       WAT  160
C                                                                       WAT  170
      CALL DATE_AND_TIME(DATF,TIMT)                                     WAT  180
C                                                                       WAT  190
      READ (UNIT=TIMF(1),FMT=100) IH                                    WAT  200
      READ (UNIT=TIMF(2),FMT=100) IM                                    WAT  210
      READ (UNIT=SC,FMT=101) SEC                                        WAT  220
C                                                                       WAT  230
      ENDE = FLOAT(IH) * 3600. + FLOAT(IM) * 60. + SEC                  WAT  240
      RETURN                                                            WAT  250
C                                                                       WAT  260
  100 FORMAT (I2)                                                       WAT  270
  101 FORMAT (F6.3)                                                     WAT  280
      END                                                               WAT  290
      SUBROUTINE AJAX(PUTZV,PUTZVT,IV,IT)                               AJA   10
C                                                                       AJA   20
C     COMMON PUTZEN . AJAX MACHT BLANK WIE AM ERSTEN TAG                AJA   30
C     PUTZT JETZT AUCH DEN FINSTERSTEN WINKEL                           AJA   40
C                                                                       AJA   50
      DIMENSION PUTZV(IV),PUTZVT(IT)                                    AJA   60
C                                                                       AJA   70
      COMMON /BLOCK1/ PUTZ1(89)                                         AJA   80
C                                                                       AJA   90
      COMMON /BLOCK3/ PUTZ3(2188)                                       AJA  100
C                                                                       AJA  110
      COMMON /BLOCKR/ PUTZR(91)                                         AJA  120
C                                                                       AJA  130
      COMMON /BLKXEN/ PUTZXE(769)                                       AJA  140
C                                                                       AJA  150
CFZJ035                                                       14.09.04  AJA  160
CFZJ063                                                       26.07.11  AJA  170
      COMMON /GRENZE/ PUTZGR(393)                                       AJA  180
C                                                                       AJA  190
      COMMON /TERFLU/ PUTZTF(16)                                        AJA  200
C                                                                       AJA  210
      COMMON /DABLCK/ PUTZDA(228)                                       AJA  220
C                                                                       AJA  230
      COMMON /PROZ/ PUTZPR(340)                                         AJA  240
C                                                                       AJA  250
      COMMON /ORIGEN/ PUTZOR(202)                                       AJA  260
C                                                                       AJA  270
      COMMON /SPECTI/ PUTZSP(29)                                        AJA  280
C                                                                       AJA  290
CFZJ043                                                       23.09.05  AJA  300
      COMMON /BLOCKK/ PUTZK(489)                                        AJA  310
C                                                                       AJA  320
      COMMON /ADDR/ PUTZAD(601)                                         AJA  330
C                                                                       AJA  340
CFZJ042                                                       09.09.05  AJA  350
      COMMON /ADDRT/ PUTZAT(721)                                        AJA  360
C                                                                       AJA  370
      COMMON /NORM/ PUTZN(39998)                                        AJA  380
C                                                                       AJA  390
CFZJ055                                                       25.09.07  AJA  400
C                                                                       AJA  410
      COMMON /FLUXN/ PUTZFL(362)                                        AJA  420
C                                                                       AJA  430
      COMMON /IPO/ PUTZI(4)                                             AJA  440
C                                                                       AJA  450
CFZJ035                                                       14.09.04  AJA  460
CFZJ063                                                       26.07.11  AJA  470
      COMMON /RI/ PUTZRI(1981)                                          AJA  480
C                                                                       AJA  490
CFZJ006 enlarged dimensions common QVAR                       28.11.03  AJA  500
      COMMON /QVAR/ PUTZQ(2133)                                         AJA  510
C                                                                       AJA  520
      COMMON /VARTK/ PUTZVA(8)                                          AJA  530
C                                                                       AJA  540
CFZJ058                                                       05.11.08  AJA  550
      COMMON /KEFFT/ PUTZKE(45007)                                      AJA  560
C                                                                       AJA  570
CFZJ046                                                       26.09.06  AJA  580
C                                                                       AJA  590
      COMMON /CVX/ PUTZCV(1000)                                         AJA  600
C                                                                       AJA  610
CFZJ042                                                       09.09.05  AJA  620
      COMMON /KITK/ PUTZIT(1005)                                        AJA  630
C                                                                       AJA  640
C                                                                       AJA  650
      DO 10 I=1,89                                                      AJA  660
        PUTZ1(I) = 0.                                                   AJA  670
   10 CONTINUE                                                          AJA  680
      DO 30 I=1,2188                                                    AJA  690
        PUTZ3(I) = 0.                                                   AJA  700
   30 CONTINUE                                                          AJA  710
      DO 50 I = 1,769                                                   AJA  720
        PUTZXE(I) = 0.0                                                 AJA  730
   50 CONTINUE                                                          AJA  740
      DO 80 I = 1,16                                                    AJA  750
        PUTZTF(I) = 0.0                                                 AJA  760
   80 CONTINUE                                                          AJA  770
      DO 110 I=1,91                                                     AJA  780
        PUTZR(I) = 0.                                                   AJA  790
  110 CONTINUE                                                          AJA  800
      DO 120 I=1,228                                                    AJA  810
        PUTZDA(I) = 0.0                                                 AJA  820
  120 CONTINUE                                                          AJA  830
      DO 130 I=1,340                                                    AJA  840
        PUTZPR(I) = 0.                                                  AJA  850
  130 CONTINUE                                                          AJA  860
CFZJ035                                                       14.09.04  AJA  870
CFZJ063                                                       26.07.11  AJA  880
      DO 140 I=1,393                                                    AJA  890
        PUTZGR(I) = 0.                                                  AJA  900
  140 CONTINUE                                                          AJA  910
      DO 150 I=1,202                                                    AJA  920
        PUTZOR(I) = 0.                                                  AJA  930
  150 CONTINUE                                                          AJA  940
      DO 160 I=1,29                                                     AJA  950
        PUTZSP(I) = 0.                                                  AJA  960
  160 CONTINUE                                                          AJA  970
CFZJ043                                                       23.09.05  AJA  980
      DO 170 I=1,489                                                    AJA  990
        PUTZK(I) = 0.                                                   AJA 1000
  170 CONTINUE                                                          AJA 1010
      DO 180 I=1,IV                                                     AJA 1020
        PUTZV(I) = 0.                                                   AJA 1030
  180 CONTINUE                                                          AJA 1040
      DO 190 I=1,601                                                    AJA 1050
        PUTZAD(I) = 0.                                                  AJA 1060
  190 CONTINUE                                                          AJA 1070
      DO 200 I=1,IT                                                     AJA 1080
        PUTZVT(I) = 0.                                                  AJA 1090
  200 CONTINUE                                                          AJA 1100
      DO 210 I=1,721                                                    AJA 1110
        PUTZAT(I) = 0.                                                  AJA 1120
  210 CONTINUE                                                          AJA 1130
      DO 220 I=1,39998                                                  AJA 1140
        PUTZN(I) = 0.                                                   AJA 1150
  220 CONTINUE                                                          AJA 1160
CFZJ055                                                       25.09.07  AJA 1170
      DO 240 I=1,362                                                    AJA 1180
        PUTZFL(I) = 0.                                                  AJA 1190
  240 CONTINUE                                                          AJA 1200
CFZJ035                                                       14.09.04  AJA 1210
CFZJ063                                                       26.07.11  AJA 1220
      DO 250 I=1,1981                                                   AJA 1230
        PUTZRI(I) = 0.                                                  AJA 1240
  250 CONTINUE                                                          AJA 1250
CFZJ006 enlarged dimensions common QVAR                       28.11.03  AJA 1260
      DO 260 I=1,2133                                                   AJA 1270
        PUTZQ(I) = 0.                                                   AJA 1280
  260 CONTINUE                                                          AJA 1290
      DO 270 I=1,4                                                      AJA 1300
        PUTZI(I) = 0.                                                   AJA 1310
  270 CONTINUE                                                          AJA 1320
      DO 280 I=1,8                                                      AJA 1330
        PUTZVA(I) = 0.                                                  AJA 1340
  280 CONTINUE                                                          AJA 1350
CFZJ058                                                       05.11.08  AJA 1360
      DO 290 I=1,45007                                                  AJA 1370
        PUTZKE(I) = 0.                                                  AJA 1380
  290 CONTINUE                                                          AJA 1390
CFZJ046                                                       26.09.06  AJA 1400
      DO 310 I=1,1000                                                   AJA 1410
        PUTZCV(I) = 0.                                                  AJA 1420
  310 CONTINUE                                                          AJA 1430
CFZJ042                                                       09.09.05  AJA 1440
      DO 320 I=1,1005                                                   AJA 1450
        PUTZIT(I) = 0.                                                  AJA 1460
  320 CONTINUE                                                          AJA 1470
      RETURN                                                            AJA 1480
      END                                                               AJA 1490
      SUBROUTINE WRDACC                                                 WRD   10
C                                                                       WRD   20
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), WRD   30
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10WRD   40
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11WRD   50
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13,JSUM29                  WRD   60
C                                                                       WRD   70
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    WRD   80
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    WRD   90
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIWRD  100
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D,NLAYP,ITTT, WRD  110
     4 LIMT                                                             WRD  120
C                                                                       WRD  130
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1WRD  140
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         WRD  150
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    WRD  160
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)WRD  170
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  WRD  180
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TXME(192),STORE(7,96),NSTO(96),    WRD  190
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         WRD  200
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,WRD  210
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW,CIZET0,    WRD  220
     9 NMAXC,DLAY(101)                                                  WRD  230
C                                                                       WRD  240
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, WRD  250
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        WRD  260
     2 FIMAKL(20),NOPILE,MREP,MARX(10)                                  WRD  270
C                                                                       WRD  280
      COMMON /TERFLU/ SPARTF(16)                                        WRD  290
C                                                                       WRD  300
CFZJ035                                                       14.09.04  WRD  310
CFZJ063                                                       26.07.11  WRD  320
      COMMON /GRENZE/ SPARGZ(393)                                       WRD  330
C                                                                       WRD  340
      COMMON /BLKXEN/ SPARXN(769)                                       WRD  350
C                                                                       WRD  360
CFZJ043                                                       23.09.05  WRD  370
      COMMON /BLOCKK/ SPARK(489)                                        WRD  380
C                                                                       WRD  390
      COMMON /NORM/ SPARN(39998)                                        WRD  400
C                                                                       WRD  410
      COMMON /ORIGEN/ SPAROR(202)                                       WRD  420
C                                                                       WRD  430
CFZJ035                                                       14.09.04  WRD  440
CFZJ063                                                       23.07.11  WRD  450
      COMMON /RI/ SPARRI(1981)                                          WRD  460
C                                                                       WRD  470
      COMMON /FLUXN/ D(361),IACT                                        WRD  480
C                                                                       WRD  490
      COMMON /VARTK/ SPARVA(8)                                          WRD  500
C                                                                       WRD  510
      COMMON /FLEX/ FLUX,MMN,MOUT,INDEX,QXN,AXN,ERR,NDUMMY,MZERO,ITMAX, WRD  520
     1 ILMAX,IAMAX,IFMAX,IZMAX                                          WRD  530
C                                                                       WRD  540
      COMMON /VARDIM/ A(8000000)                                        WRD  550
C                                                                       WRD  560
CFZJ046                                                       26.09.06  WRD  570
C                                                                       WRD  580
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       WRD  590
C                                                                       WRD  600
      COMMON /BUC/ BU                                                   WRD  610
C                                                                       WRD  620
      COMMON /CVX/ SPARCV(1000)                                         WRD  630
C                                                                       WRD  640
CFZJ042                                                       09.09.05  WRD  650
      COMMON /KITK/ ITKK(1000)                                          WRD  660
C                                                                       WRD  670
      CHARACTER*4 BU(6,200)                                             WRD  680
C                                                                       WRD  690
      REAL*4 RECORD(5040)                                               WRD  700
C                                                                       WRD  710
      DIMENSION SPAR3(2188),SPARAD(601),SPARB1(89),SPARDA(228),SPAR4(91)WRD  720
C                                                                       WRD  730
      EQUIVALENCE(SPAR3(1),JNN),(IPRIN(1),SPARB1(1)),(NDA10,SPARDA(1)), WRD  740
     1 (SPARAD(1),KA(1)),(NRESHZ,SPAR4(1)),(LI(1),LIMAT),(JTPE3,NT)     WRD  750
C                                                                       WRD  760
10001 FORMAT (' ... JSATZ ',I4,' ON DATA SET ',I4,' STARTS ON RECORD NO'WRD  770
     1 ,I6,' ... ')                                                     WRD  780
C                                                                       WRD  790
C                                                                       WRD  800
C     WRITE DIRECT ACCESS NDA9                                          WRD  810
C                                                                       WRD  820
      ENTRY WRDA9(JSATZ)                                                WRD  830
C                                                                       WRD  840
      IF(JAD9(JSATZ) .NE. 0) GOTO 300                                   WRD  850
      JAD9(JSATZ) = JSUM9                                               WRD  860
      WRITE (NT,10001) JSATZ,NDA9,JAD9(JSATZ)                           WRD  870
  300 CONTINUE                                                          WRD  880
      NXT9 = JAD9(JSATZ)                                                WRD  890
      IF(JSATZ .EQ. 1) GOTO 310                                         WRD  900
      IF(JSATZ .EQ. 2) GOTO 320                                         WRD  910
      IF(JSATZ .EQ. 3) GOTO 330                                         WRD  920
      IF(JSATZ .EQ. 14) GOTO 410                                        WRD  930
      GOTO 1199                                                         WRD  940
  310 CONTINUE                                                          WRD  950
      WRITE (NDA9,REC=NXT9) JTPE9,JAD9                                  WRD  960
      NXT9 = NXT9 + 1                                                   WRD  970
      GOTO 500                                                          WRD  980
  320 CONTINUE                                                          WRD  990
      JSUM = JSUM13 - 1                                                 WRD 1000
      N13 = JSUM / ND13 + 1                                             WRD 1010
CFZJ063                                                       26.07.11  WRD 1020
      WRITE (NDA9,REC=NXT9) SPARDA,SPARB1,SPAR3,SPAR4,SPARK,SPARAD,     WRD 1030
     1 SPARTF,SPARGZ,SPARXN                                             WRD 1040
      NXT9 = NXT9 + 1                                                   WRD 1050
      WRITE (NDA9,REC=NXT9) SPAROR,IACT,SPARVA,ERR,ITKK                 WRD 1060
      NXT9 = NXT9 + 1                                                   WRD 1070
CFZJ042                                                       09.09.05  WRD 1080
CFZJ046                                                       26.09.06  WRD 1090
      WRITE (NDA9,REC=NXT9) SPARRI,BU,SPARCV                            WRD 1100
      NXT9 = NXT9 + 1                                                   WRD 1110
C                                                                       WRD 1120
      CALL WRDA(IWRITE,NDA9,NXT9,L9,SPARN,39998)                        WRD 1130
C                                                                       WRD 1140
      GOTO 500                                                          WRD 1150
  330 CONTINUE                                                          WRD 1160
C                                                                       WRD 1170
      CALL SPA(1,A(KA(LIMAT)),KL(200))                                  WRD 1180
C                                                                       WRD 1190
      GOTO 500                                                          WRD 1200
C                                                                       WRD 1210
C     SATZ 14 IN FUMAN AUF NDA13 GESCHRIEBEN                            WRD 1220
C                                                                       WRD 1230
  410 CONTINUE                                                          WRD 1240
      NXT13 = 1                                                         WRD 1250
      DO 412 I=1,N13                                                    WRD 1260
        J2 = 0                                                          WRD 1270
        DO 411 J=1,ND13                                                 WRD 1280
          J1 = J2 + 1                                                   WRD 1290
          J2 = J1 + N14 - 1                                             WRD 1300
          READ (NDA13,REC=NXT13) (RECORD(K),K=J1,J2)                    WRD 1310
          NXT13 = NXT13 + 1                                             WRD 1320
          IF(NXT13 .GT. (JSUM13-1)) GOTO 413                            WRD 1330
  411   CONTINUE                                                        WRD 1340
  413   CONTINUE                                                        WRD 1350
        WRITE (NDA9,REC=NXT9) RECORD                                    WRD 1360
        NXT9 = NXT9 + 1                                                 WRD 1370
  412 CONTINUE                                                          WRD 1380
      J1 = 0                                                            WRD 1390
      J2 = 0                                                            WRD 1400
  500 CONTINUE                                                          WRD 1410
      IF(JSUM9 .LT. NXT9) JSUM9 = NXT9                                  WRD 1420
      GOTO 1199                                                         WRD 1430
C                                                                       WRD 1440
C     READ DIRECT ACCESS NDA8                                           WRD 1450
C                                                                       WRD 1460
      ENTRY RDDA8(JSATZ)                                                WRD 1470
C                                                                       WRD 1480
      WRITE (NT,10001) JSATZ,NDA8,JAD8(JSATZ)                           WRD 1490
      NXT8 = JAD8(JSATZ)                                                WRD 1500
      IF(JSATZ .EQ. 1) GOTO 610                                         WRD 1510
      IF(JSATZ .EQ. 2) GOTO 620                                         WRD 1520
      IF(JSATZ .EQ. 3) GOTO 630                                         WRD 1530
      IF(JSATZ .EQ. 14) GOTO 710                                        WRD 1540
      GOTO 800                                                          WRD 1550
  610 CONTINUE                                                          WRD 1560
      READ (NDA8,REC=NXT8) JTPE7,JAD8                                   WRD 1570
      NXT8 = NXT8 + 1                                                   WRD 1580
      GOTO 800                                                          WRD 1590
  620 CONTINUE                                                          WRD 1600
      NXT8H = NXT8                                                      WRD 1610
CFZJ063                                                       26.07.11  WRD 1620
      READ (NDA8,REC=NXT8) SPARDA,SPARB1,SPAR3,SPAR4,SPARK,SPARAD,SPARTFWRD 1630
     1 ,SPARGZ,SPARXN                                                   WRD 1640
      NXT8 = NXT8H+ 1                                                   WRD 1650
      READ (NDA8,REC=NXT8) SPAROR,IACT,SPARVA,ERR,ITKK                  WRD 1660
      NXT8 = NXT8 + 1                                                   WRD 1670
CFZJ042                                                       09.09.05  WRD 1680
CFZJ046                                                       26.09.06  WRD 1690
      READ (NDA8,REC=NXT8) SPARRI,BU,SPARCV                             WRD 1700
      NXT8 = NXT8 + 1                                                   WRD 1710
C                                                                       WRD 1720
      CALL WRDA(IREAD,NDA8,NXT8,L8,SPARN,39998)                         WRD 1730
C                                                                       WRD 1740
      NXT8 = 1                                                          WRD 1750
      GOTO 610                                                          WRD 1760
  630 CONTINUE                                                          WRD 1770
C                                                                       WRD 1780
      CALL SPA(2,A(KA(LIMAT)),KL(200))                                  WRD 1790
C                                                                       WRD 1800
      GOTO 800                                                          WRD 1810
  710 CONTINUE                                                          WRD 1820
      NXT13 = 1                                                         WRD 1830
      DO 712 I=1,N13                                                    WRD 1840
        READ (NDA8,REC=NXT8) RECORD                                     WRD 1850
        NXT8 = NXT8 + 1                                                 WRD 1860
        J2 = 0                                                          WRD 1870
        DO 711 J=1,ND13                                                 WRD 1880
          J1 = J2 + 1                                                   WRD 1890
          J2 = J1 + N14 - 1                                             WRD 1900
          WRITE (NDA13,REC=NXT13) (RECORD(K),K=J1,J2)                   WRD 1910
          NXT13 = NXT13 + 1                                             WRD 1920
          IF(NXT13 .GT. N200) GOTO 713                                  WRD 1930
  711   CONTINUE                                                        WRD 1940
  712 CONTINUE                                                          WRD 1950
  713 CONTINUE                                                          WRD 1960
      J1 = 0                                                            WRD 1970
      J2 = 0                                                            WRD 1980
  800 CONTINUE                                                          WRD 1990
      IF(JSUM8 .LT. NXT8) JSUM8 = NXT8                                  WRD 2000
 1199 CONTINUE                                                          WRD 2010
      RETURN                                                            WRD 2020
      END                                                               WRD 2030
      SUBROUTINE WRDA(IWR,NDA,NXT,LR,A,LF)                              RDA   10
C                                                                       RDA   20
      DIMENSION A(LF)                                                   RDA   30
C                                                                       RDA   40
C                                                                       RDA   50
      LTOT = (LF+LR-1) / LR                                             RDA   60
      L2 = 0                                                            RDA   70
      DO 1 I=1,LTOT                                                     RDA   80
        L1 = L2 + 1                                                     RDA   90
        L2 = L1 + LR - 1                                                RDA  100
        L2 = MIN0(L2,LF)                                                RDA  110
        IF(IWR .EQ. 1) WRITE (NDA,REC=NXT) (A(L),L=L1,L2)               RDA  120
        IF(IWR .EQ. 2) READ (NDA,REC=NXT) (A(L),L=L1,L2)                RDA  130
        NXT = NXT + 1                                                   RDA  140
    1 CONTINUE                                                          RDA  150
      RETURN                                                            RDA  160
      END                                                               RDA  170
      SUBROUTINE SPA(IWR,B,KLB)                                         SPA   10
C                                                                       SPA   20
C     SCHREIBEN BZW. LESEN DES KOMPLETTEN VARIABLEN A-FELDES DES COMMON SPA   30
C     ** VARDIM **                                                      SPA   40
C                                                                       SPA   50
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), SPA   60
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10SPA   70
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11SPA   80
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         SPA   90
C                                                                       SPA  100
      DIMENSION B(KLB)                                                  SPA  110
C                                                                       SPA  120
C                                                                       SPA  130
      IF(IWR .EQ. 1) CALL WRDA(IWRITE,NDA9,NXT9,L9,B,KLB)               SPA  140
C                                                                       SPA  150
      IF(IWR .EQ. 2) CALL WRDA(IREAD,NDA8,NXT8,L8,B,KLB)                SPA  160
C                                                                       SPA  170
      RETURN                                                            SPA  180
      END                                                               SPA  190
      SUBROUTINE START1(JAD11)                                          TAR   10
C                                                                       TAR   20
C     PROGRAM ORGANIZES CREATION AND RETRIEVAL                          TAR   30
C     OF RESTART DATA ON DATA SETS NDA9  AND NDA8                       TAR   40
C                                                                       TAR   50
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    TAR   60
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    TAR   70
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PITAR   80
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 TAR   90
C                                                                       TAR  100
      EQUIVALENCE(JTPE3,NT)                                             TAR  110
C                                                                       TAR  120
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1TAR  130
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         TAR  140
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    TAR  150
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)TAR  160
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  TAR  170
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TINE(192),STORE(7,96),NSTO(96),    TAR  180
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         TAR  190
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,TAR  200
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            TAR  210
C                                                                       TAR  220
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, TAR  230
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        TAR  240
     2 FIMAKL(20),NOPILE,MREP,MARX(10)                                  TAR  250
C                                                                       TAR  260
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), TAR  270
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10TAR  280
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11TAR  290
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         TAR  300
C                                                                       TAR  310
      COMMON /TERFLU/ KANN,NUTTE,NEUSPK,NSAEFL,A39,B39,C39,A40,B40,C40, TAR  320
     1 AD39,AD40,AD39R,AD40R,SSPU39,SSPU40                              TAR  330
C                                                                       TAR  340
      COMMON /VARDIM/ A(8000000)                                        TAR  350
C                                                                       TAR  360
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       TAR  370
C                                                                       TAR  380
CFZJ055                                                       25.09.07  TAR  390
C                                                                       TAR  400
      REAL*4 RECORD(5040)                                               TAR  410
C                                                                       TAR  420
      DIMENSION JAD11(JD11)                                             TAR  430
C                                                                       TAR  440
10002 FORMAT (///' ... DATA SET 9 WITH FILE ID.NO.',I6,' NOW CONTAINS RETAR  450
     1START DATA'/' ... CORRESPONDING TO THE CONDITIONS AT START OF TIMETAR  460
     2 STEP',I4,' AFTER RESHUFFLE NO.',I4/' ... TRANSMISSION TIME ',    TAR  470
     3 F12.3,' SEC')                                                    TAR  480
10006 FORMAT ('0=====RESTART DATA CONSISTING OF',I6,' RECORDS HAVE BEEN TAR  490
     1TRANSFERRED FROM DATA SET',I4,' TO DATA SET "rstnew"'/' =====TRANSTAR  500
     2MISSION TIME ',F12.3,' SEC'///)                                   TAR  510
C                                                                       TAR  520
C                                                                       TAR  530
      ENDE = 0.                                                         TAR  540
C                                                                       TAR  550
C     WRITE RESTART DATA ON NDA9 WITH FILE ID.NO. JTPE9                 TAR  560
C                                                                       TAR  570
      N69 = KMAT + 8                                                    TAR  580
      IF(JNSTOP .GE. 0 .AND. IPRIN(9) .GT. 0) GOTO 191                  TAR  590
C                                                                       TAR  600
      CALL WATCH(ENDE)                                                  TAR  610
C                                                                       TAR  620
      TO = ENDE                                                         TAR  630
C                                                                       TAR  640
      CALL WRDA9(1)                                                     TAR  650
C                                                                       TAR  660
      J2 = NENDP                                                        TAR  670
      DO 110 N=1,NXS                                                    TAR  680
        J1 = J2 + 1                                                     TAR  690
        J2 = J2 + N69                                                   TAR  700
        JSATZ = 2 + N                                                   TAR  710
        NXT11 = JAD11(JSATZ)                                            TAR  720
        READ (NDA11,REC=NXT11) (A(J),J=J1,J2)                           TAR  730
        NXT11 = NXT11 + 1                                               TAR  740
  110 CONTINUE                                                          TAR  750
      IF(KUGL .EQ. 0) GOTO 125                                          TAR  760
      JOPILE = NOPILE + JTYP                                            TAR  770
      DO 120 N=1,JOPILE                                                 TAR  780
        J1 = J2 + 1                                                     TAR  790
        J2 = J2 + N69                                                   TAR  800
        JSATZ = NRESHZ + 2 + MBOX + N                                   TAR  810
        NXT11 = JAD11(JSATZ)                                            TAR  820
        READ (NDA11,REC=NXT11) (A(J),J=J1,J2)                           TAR  830
        NXT11 = NXT11 + 1                                               TAR  840
  120 CONTINUE                                                          TAR  850
  121 CONTINUE                                                          TAR  860
      JSATZ = JSATZ + 1                                                 TAR  870
      IF(JAD11(JSATZ) .EQ. 0) GOTO 125                                  TAR  880
      NXT11 = JAD11(JSATZ)                                              TAR  890
      J1 = J2 + 1                                                       TAR  900
      J2 = J2 + N69                                                     TAR  910
      READ (NDA11,REC=NXT11) (A(J),J=J1,J2)                             TAR  920
      NXT11 = NXT11 + 1                                                 TAR  930
      GOTO 121                                                          TAR  940
  125 CONTINUE                                                          TAR  950
      JSATZ = 20                                                        TAR  960
      NEXT = JAD10(JSATZ)                                               TAR  970
      IF(NEXT .LE. 0) GOTO 140                                          TAR  980
      DO 130 N=1,NXS                                                    TAR  990
        J1 = J2 + 1                                                     TAR 1000
        J2 = J2 + 600                                                   TAR 1010
        NXT10 = NEXT + NSAEFL * (N-1)                                   TAR 1020
        READ (NDA10,REC=NXT10) (A(J),J=J1,J2)                           TAR 1030
        NXT10 = NXT10 + 1                                               TAR 1040
  130 CONTINUE                                                          TAR 1050
  140 CONTINUE                                                          TAR 1060
      DO 150 N=1,MBOX                                                   TAR 1070
        JSATZ = NRESHZ + 2 + N                                          TAR 1080
        IF(JAD11(JSATZ) .EQ. 0) GOTO 150                                TAR 1090
        NXT11 = JAD11(JSATZ)                                            TAR 1100
        J1 = J2 + 1                                                     TAR 1110
        J2 = J2 + N69                                                   TAR 1120
        READ (NDA11,REC=NXT11) (A(J),J=J1,J2)                           TAR 1130
        NXT11 = NXT11 + 1                                               TAR 1140
  150 CONTINUE                                                          TAR 1150
      KL(200) = J2                                                      TAR 1160
C                                                                       TAR 1170
      CALL WRDA9(2)                                                     TAR 1180
C                                                                       TAR 1190
      CALL WRDA9(3)                                                     TAR 1200
C                                                                       TAR 1210
      CALL WRDA9(14)                                                    TAR 1220
C                                                                       TAR 1230
      CALL WRDA9(1)                                                     TAR 1240
C                                                                       TAR 1250
      CALL WATCH(ENDE)                                                  TAR 1260
C                                                                       TAR 1270
      TTIME = ENDE - TO                                                 TAR 1280
      J10 = JNN                                                         TAR 1290
      J20 = IPRIN(15)                                                   TAR 1300
      WRITE (NT,10002) JTPE9,J10,J20,TTIME                              TAR 1310
  191 CONTINUE                                                          TAR 1320
CFZJ012   New identification numbers for THERMOS-cell         02.12.03  TAR 1330
C         definitions for the spectrum zones                  02.12.03  TAR 1340
      IF(JNSTOP .GE. 0) GOTO 199                                        TAR 1350
      IF(IPRIN(11) .LE. 0) GOTO 195                                     TAR 1360
      GOTO 600                                                          TAR 1370
  195 CONTINUE                                                          TAR 1380
      IF(IPRIN(9) .LE. 0) GOTO 199                                      TAR 1390
      GOTO 800                                                          TAR 1400
  199 CONTINUE                                                          TAR 1410
      RETURN                                                            TAR 1420
C                                                                       TAR 1430
C     WRITE CONTENT OF DIRECT ACCESS DATA SET NDA10 ON RESTART UNIT     TAR 1440
C     IPRIN(9)                                                          TAR 1450
C                                                                       TAR 1460
  600 CONTINUE                                                          TAR 1470
C                                                                       TAR 1480
      CALL WATCH(ENDE)                                                  TAR 1490
C                                                                       TAR 1500
      TO = ENDE                                                         TAR 1510
      JTP = IPRIN(9)                                                    TAR 1520
      NXT10 = 1                                                         TAR 1530
      JSUM = JSUM10 - 1                                                 TAR 1540
CFZJ028       DROP WRITING DATA SET "rstlib"                  31.03.04  TAR 1550
      J2 = 0                                                            TAR 1560
      JSUMJTP = JSUM10 / 2                                              TAR 1570
      WRITE (JTP) JSUMJTP                                               TAR 1580
      DO 610 J=1,JSUM                                                   TAR 1590
  609   CONTINUE                                                        TAR 1600
        J1 = J2 + 1                                                     TAR 1610
        J2 = J2 + L10                                                   TAR 1620
        IF(J2 .LE. L9) GOTO 611                                         TAR 1630
        WRITE (JTP) RECORD                                              TAR 1640
        J2 = 0                                                          TAR 1650
        GOTO 609                                                        TAR 1660
  611   CONTINUE                                                        TAR 1670
        READ (NDA10,REC=NXT10) (RECORD(I),I=J1,J2)                      TAR 1680
        NXT10 = NXT10 + 1                                               TAR 1690
        IF(J .EQ. JSUM) WRITE (JTP) RECORD                              TAR 1700
  610 CONTINUE                                                          TAR 1710
      NXT = NXT10 - 1                                                   TAR 1720
      NXT10 = 1                                                         TAR 1730
C                                                                       TAR 1740
      CALL WATCH(ENDE)                                                  TAR 1750
C                                                                       TAR 1760
      TTIME = ENDE - TO                                                 TAR 1770
      WRITE (NT,10006) NXT,NDA10,TTIME                                  TAR 1780
      GOTO 195                                                          TAR 1790
C                                                                       TAR 1800
C     WRITE CONTENT OF DIRECT ACCESS DATA SET NDA9  ON RESTART UNIT     TAR 1810
C     IPRIN(9)                                                          TAR 1820
C                                                                       TAR 1830
  800 CONTINUE                                                          TAR 1840
C                                                                       TAR 1850
      CALL WATCH(ENDE)                                                  TAR 1860
C                                                                       TAR 1870
      TO = ENDE                                                         TAR 1880
      JTP = IPRIN( 9)                                                   TAR 1890
      NXT9 = 1                                                          TAR 1900
      JSUM = JSUM9 - 1                                                  TAR 1910
      DO 810 J=1,JSUM                                                   TAR 1920
        READ (NDA9,REC=NXT9 ) RECORD                                    TAR 1930
        NXT9 = NXT9 + 1                                                 TAR 1940
        WRITE (JTP) RECORD                                              TAR 1950
  810 CONTINUE                                                          TAR 1960
C                                                                       TAR 1970
      CALL WATCH(ENDE)                                                  TAR 1980
C                                                                       TAR 1990
      TTIME = ENDE - TO                                                 TAR 2000
      NXT = NXT9 - 1                                                    TAR 2010
      NXT9 = 1                                                          TAR 2020
      WRITE (NT,10006) NXT,NDA9,TTIME                                   TAR 2030
      GOTO 199                                                          TAR 2040
      END                                                               TAR 2050
      SUBROUTINE START2                                                 ART   10
C                                                                       ART   20
C     PROGRAM ORGANIZES CREATION AND RETRIEVAL                          ART   30
C     OF RESTART DATA ON DATA SETS NDA9  AND NDA8                       ART   40
C                                                                       ART   50
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    ART   60
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    ART   70
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIART   80
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 ART   90
C                                                                       ART  100
      EQUIVALENCE(JTPE3,NT)                                             ART  110
C                                                                       ART  120
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1ART  130
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         ART  140
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    ART  150
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)ART  160
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  ART  170
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TINE(192),STORE(7,96),NSTO(96),    ART  180
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         ART  190
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,ART  200
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            ART  210
C                                                                       ART  220
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, ART  230
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        ART  240
     2 FIMAKL(20),NOPILE,MREP,MARX(10)                                  ART  250
C                                                                       ART  260
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), ART  270
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10ART  280
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11ART  290
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         ART  300
C                                                                       ART  310
      COMMON /TERFLU/ KANN,NUTTE,NEUSPK,NSAEFL,A39,B39,C39,A40,B40,C40, ART  320
     1 AD39,AD40,AD39R,AD40R,SSPU39,SSPU40                              ART  330
C                                                                       ART  340
      COMMON /VARDIM/ A(8000000)                                        ART  350
C                                                                       ART  360
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       ART  370
C                                                                       ART  380
      REAL*4 RECORD(5040)                                               ART  390
C                                                                       ART  400
10001 FORMAT ('1'//'.....WRONG RESTART DATA FILE SPECIFIED:',I5,'. DATA ART  410
     1SET',I4,' HAS ID.NO.',I6//' ..... CASE TERMINATED .....')         ART  420
10005 FORMAT ('1=====RESTART DATA CONSISTING OF ',I6,' RECORDS HAVE BEENART  430
     1 TRANSFERRED FROM DATA SET',I4/' =====TO DIRECT ACCESS DATA SET', ART  440
     2 I4,' . TRANSMISSION TIME ',F12.3,' SEC'/' =====(MAY BE THERE ARE ART  450
     3FEW DUMMY RECORDS AT THE END, (NO PROBLEM))'///)                  ART  460
10006 FORMAT (' =====RESTART DATA CONSISTING OF ',I6,' RECORDS HAVE BEENART  470
     1 TRANSFERRED FROM DATA SET',I4/' =====TO DIRECT ACCESS DATA SET', ART  480
     2 I4,' . TRANSMISSION TIME ',F12.3,' SEC'///)                      ART  490
C                                                                       ART  500
C                                                                       ART  510
C     READ RESTART DATA ON NDA8 WITH FILE ID.NO. JTPE7                  ART  520
C                                                                       ART  530
      ENDE = 0.                                                         ART  540
      IF(IPRIN(11) .GT. 0) GOTO 500                                     ART  550
  202 CONTINUE                                                          ART  560
      IF(IPRIN(8) .GT. 0) GOTO 700                                      ART  570
  203 CONTINUE                                                          ART  580
C                                                                       ART  590
      CALL WATCH(ENDE)                                                  ART  600
C                                                                       ART  610
      TO = ENDE                                                         ART  620
      NFILE = JTPE7                                                     ART  630
      JFILE = JTPE9                                                     ART  640
      IPR8 = IPRIN(8)                                                   ART  650
      IPR9 = IPRIN(9)                                                   ART  660
      IRRR9 = IRR9                                                      ART  670
      JAD8(1) = 1                                                       ART  680
C                                                                       ART  690
      CALL RDDA8(1)                                                     ART  700
C                                                                       ART  710
      IF(NFILE .EQ. JTPE7) GOTO 201                                     ART  720
      WRITE (NT,10001) NFILE,NDA8,JTPE7                                 ART  730
      STOP                                                              ART  740
  201 CONTINUE                                                          ART  750
C                                                                       ART  760
      CALL RDDA8(2)                                                     ART  770
C                                                                       ART  780
      CALL RDDA8(3)                                                     ART  790
C                                                                       ART  800
      JTPE7 = NFILE                                                     ART  810
      JTPE9 = JFILE                                                     ART  820
      IPRIN(8) = IPR8                                                   ART  830
      IPRIN(9) = IPR9                                                   ART  840
      IRR9 = IRRR9                                                      ART  850
      RETURN                                                            ART  860
C                                                                       ART  870
C     READ RESTART UNIT IPRIN(8) CONTAINING THE DATA FOR DIRECT ACCESS  ART  880
C     DATA SET NDA10                                                    ART  890
C                                                                       ART  900
  500 CONTINUE                                                          ART  910
C                                                                       ART  920
      CALL WATCH(ENDE)                                                  ART  930
C                                                                       ART  940
      TO = ENDE                                                         ART  950
      JTP = IPRIN(8)                                                    ART  960
CFZJ028     DROP READING DATA SET "rstlib"                    31.03.04  ART  970
      READ (JTP) JSUMJTP                                                ART  980
      NXT10 = 1                                                         ART  990
      DO 510 J=1,JSUMJTP                                                ART 1000
        J2 = 0                                                          ART 1010
        READ (JTP) RECORD                                               ART 1020
  509   CONTINUE                                                        ART 1030
        J1 = J2 + 1                                                     ART 1040
        J2 = J2 + L10                                                   ART 1050
        IF(J2 .GT. L9) GOTO 510                                         ART 1060
        WRITE (NDA10,REC=NXT10) (RECORD(I),I=J1,J2)                     ART 1070
        NXT10 = NXT10 + 1                                               ART 1080
        GOTO 509                                                        ART 1090
  510 CONTINUE                                                          ART 1100
      NXT = NXT10 - 2                                                   ART 1110
      NXT10 = 1                                                         ART 1120
C                                                                       ART 1130
      CALL WATCH(ENDE)                                                  ART 1140
C                                                                       ART 1150
      TTIME = ENDE - TO                                                 ART 1160
      WRITE (NT,10005) NXT,JTP,NDA10,TTIME                              ART 1170
      GOTO 202                                                          ART 1180
C                                                                       ART 1190
C     READ RESTART UNIT IPRIN(8) CONTAINING THE DATA FOR DIRECT ACCESS  ART 1200
C     DATA SET NDA8                                                     ART 1210
C                                                                       ART 1220
  700 CONTINUE                                                          ART 1230
C                                                                       ART 1240
      CALL WATCH(ENDE)                                                  ART 1250
C                                                                       ART 1260
      TO = ENDE                                                         ART 1270
      JTP = IPRIN(8)                                                    ART 1280
      NXT8 = 1                                                          ART 1290
      DO 710 J=1,K08                                                    ART 1300
        READ (JTP,END=705) RECORD                                       ART 1310
        WRITE (NDA8,REC=NXT8) RECORD                                    ART 1320
        NXT8 = NXT8 + 1                                                 ART 1330
  710 CONTINUE                                                          ART 1340
  705 CONTINUE                                                          ART 1350
C                                                                       ART 1360
      CALL WATCH(ENDE)                                                  ART 1370
C                                                                       ART 1380
      TTIME = ENDE - TO                                                 ART 1390
      NXT = NXT8 - 1                                                    ART 1400
      NXT8 = 1                                                          ART 1410
      WRITE (NT,10006) NXT,JTP,NDA8,TTIME                               ART 1420
      GOTO 203                                                          ART 1430
      END                                                               ART 1440
      SUBROUTINE START3(JAD11)                                          RT3   10
C                                                                       RT3   20
C     PROGRAM ORGANIZES CREATION AND RETRIEVAL                          RT3   30
C     OF RESTART DATA ON DATA SETS NDA9  AND NDA8                       RT3   40
C                                                                       RT3   50
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    RT3   60
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    RT3   70
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PIRT3   80
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP                 RT3   90
C                                                                       RT3  100
      EQUIVALENCE(JTPE3,NT)                                             RT3  110
C                                                                       RT3  120
      COMMON /BLOCK3/ JNN,IRSUB,KREG,N71,CRT,DELDAY,DELSEC,NK,NL,XYZ,ZF1RT3  130
     1 ,ZF2,ZF3,ZF4,ZJNUM,VOLUME,FIWATT,DIRAC(49,5),YIELD1(49),         RT3  140
     2 YIELD2(49),YIELD3(49),YIELD4(49),NP237,XNORM,N27,N28,N29,N31,    RT3  150
     3 N41(20),SERCON,CORE,RADR(20),NTYPE,REACT(2),XPPNEW(95),YPPNEW(95)RT3  160
     4 ,LSER,ZKFIND,HCORE,JS,JSS,KSS,KSIM,LSIM,JSIM,LBUMP,NBUMP,NQUIT,  RT3  170
     5 JSMAX,JSSMAX,JSPEC,SPECK(192),TINE(192),STORE(7,96),NSTO(96),    RT3  180
     6 NSWIT,JNUM,JNSTOP,NLT,JNSP,ISPEKT(20),FIFA,HMTOT,HMANFG,         RT3  190
     7 REALNN(10),YMNEW,XMNEW,FITOT1,XPNEW1,YPNEW1,LAYER(20),FF(4),NBOX,RT3  200
     8 IVSP(30),XVSP(3),IDIFF(20),CO1S,CO2S,ZPPNEW(95),ZMNEW            RT3  210
C                                                                       RT3  220
      COMMON /BLOCKR/ NRESHZ,MAKEUP,NSPALT,XSPALT,AAAA,NNNN,NWRITE,NKT, RT3  230
     1 JEEP,TDOWN,TSTORE,TREPRO,TFAB,BRUCH,KUGL,JTYP,KLASSE(10),        RT3  240
     2 FIMAKL(20),NOPILE,MREP,MARX(10)                                  RT3  250
C                                                                       RT3  260
      COMMON /DABLCK/ NDA10,NXT10,JSUM10,NDA11,NXT11,JSUM11,JAD10(150), RT3  270
     1 NXT30,NXT40,NDA8,NXT8,JSUM8,JAD8(20),NDA9,NXT9,JSUM9,JAD9(20),K10RT3  280
     2 ,K08,NDA29,NXT29,N33,NXT13,N13,NDA13,ND13,N14,NXT28,L8,L9,L10,L11RT3  290
     3 ,L13,L28,L29,L30,L40,IWRITE,IREAD,JSUM13                         RT3  300
C                                                                       RT3  310
      COMMON /TERFLU/ KANN,NUTTE,NEUSPK,NSAEFL,A39,B39,C39,A40,B40,C40, RT3  320
     1 AD39,AD40,AD39R,AD40R,SSPU39,SSPU40                              RT3  330
C                                                                       RT3  340
      COMMON /VARDIM/ A(8000000)                                        RT3  350
C                                                                       RT3  360
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       RT3  370
C                                                                       RT3  380
CFZJ055                                                       25.09.07  RT3  390
C                                                                       RT3  400
      DIMENSION JAD11(JD11)                                             RT3  410
C                                                                       RT3  420
10003 FORMAT ('1'///' ... RESTART DATA WITH FILE ID.NO.',I6,' HAS BEEN RRT3  430
     1EAD FROM DATA SET 8 '/' ... PROGRAM CONDITIONS NOW CORRESPOND TO SRT3  440
     2TART OF TIME STEP ',I4,' AFTER RESHUFFLE NO. ',I4/' ... TRANSMISSIRT3  450
     3ON TIME ',F12.3,' SEC')                                           RT3  460
C                                                                       RT3  470
C                                                                       RT3  480
      ENDE = 0.                                                         RT3  490
C                                                                       RT3  500
      CALL WATCH(ENDE)                                                  RT3  510
C                                                                       RT3  520
      TO = ENDE                                                         RT3  530
      N69 = KMAT + 8                                                    RT3  540
      J2 = NENDP                                                        RT3  550
      DO 210 N=1,NXS                                                    RT3  560
        J1 = J2 + 1                                                     RT3  570
        J2 = J2 + N69                                                   RT3  580
        JSATZ = 2 + N                                                   RT3  590
        NXT11 = JAD11(JSATZ)                                            RT3  600
        WRITE (NDA11,REC=NXT11) (A(J),J=J1,J2)                          RT3  610
        NXT11 = NXT11 + 1                                               RT3  620
  210 CONTINUE                                                          RT3  630
      IF(KUGL .EQ. 0) GOTO 225                                          RT3  640
      JOPILE = NOPILE + JTYP                                            RT3  650
      DO 220 N=1,JOPILE                                                 RT3  660
        J1 = J2 + 1                                                     RT3  670
        J2 = J2 + N69                                                   RT3  680
        JSATZ = NRESHZ + 2 + MBOX + N                                   RT3  690
        NXT11 = JAD11(JSATZ)                                            RT3  700
        WRITE (NDA11,REC=NXT11) (A(J),J=J1,J2)                          RT3  710
        NXT11 = NXT11 + 1                                               RT3  720
  220 CONTINUE                                                          RT3  730
  221 CONTINUE                                                          RT3  740
      JSATZ = JSATZ + 1                                                 RT3  750
      IF(JAD11(JSATZ) .EQ. 0) GOTO 225                                  RT3  760
      NXT11 = JAD11(JSATZ)                                              RT3  770
      J1 = J2 + 1                                                       RT3  780
      J2 = J2 + N69                                                     RT3  790
      WRITE (NDA11,REC=NXT11) (A(J),J=J1,J2)                            RT3  800
      NXT11 = NXT11 + 1                                                 RT3  810
      GOTO 221                                                          RT3  820
  225 CONTINUE                                                          RT3  830
      JSATZ = 20                                                        RT3  840
      NEXT = JAD10(JSATZ)                                               RT3  850
      IF(NEXT .LE. 0) GOTO 240                                          RT3  860
      DO 230 N=1,NXS                                                    RT3  870
        J1 = J2 + 1                                                     RT3  880
        J2 = J2 + 600                                                   RT3  890
        NXT10 = NEXT + NSAEFL * (N-1)                                   RT3  900
        WRITE (NDA10,REC=NXT10) (A(J),J=J1,J2)                          RT3  910
        NXT10 = NXT10 + 1                                               RT3  920
  230 CONTINUE                                                          RT3  930
  240 CONTINUE                                                          RT3  940
      DO 250 N=1,MBOX                                                   RT3  950
        JSATZ = NRESHZ + 2 + N                                          RT3  960
        IF(JAD11(JSATZ) .EQ. 0) GOTO 250                                RT3  970
        J1 = J2 + 1                                                     RT3  980
        J2 = J2 + N69                                                   RT3  990
        NXT11 = JAD11(JSATZ)                                            RT3 1000
        WRITE (NDA11,REC=NXT11) (A(J),J=J1,J2)                          RT3 1010
        NXT11 = NXT11 + 1                                               RT3 1020
  250 CONTINUE                                                          RT3 1030
C                                                                       RT3 1040
      CALL RDDA8(14)                                                    RT3 1050
C                                                                       RT3 1060
      JNSTOP = IABS(JNSTOP)                                             RT3 1070
C                                                                       RT3 1080
      CALL WATCH(ENDE)                                                  RT3 1090
C                                                                       RT3 1100
      TTIME = ENDE - TO                                                 RT3 1110
      J10 = JNN                                                         RT3 1120
      J20 = IPRIN(15)                                                   RT3 1130
      WRITE (NT,10003) JTPE7,J10,J20,TTIME                              RT3 1140
      RETURN                                                            RT3 1150
      END                                                               RT3 1160
      SUBROUTINE KINF(SIGA,SIGS,SIGFNU,SIG,PHI,RKINF,PHIN,ISP,NHOT,DEN, NF    10
     1 TRSIG,LT0,VOL,SIN)                                               NF    20
C                                                                       NF    30
      COMMON /BLOCK1/ IPRIN(15),JTPE1,JTPE2,JTPE3,JTPE4,JTPE5,JTPE6,    NF    40
     1 JTPE7,JTPE8,JTPE9,JTPE10,NXS,N26,KMAT,JN,N200,NDR,NO,NLUM,NC,    NF    50
     2 POWER,JSER,NXE,NRSTRT,KINIT,MUHU(30),K96,K42,IRR9,KFR,MMAF,NLB,PINF    60
     3 ,N20,KROT,NEND33,MBOX,IAVC,JD11,RINN,IBUCK,NVSOP,I3D,NLAYP,ITTT, NF    70
     4 LIMT                                                             NF    80
C                                                                       NF    90
      EQUIVALENCE(JTPE3,NT)                                             NF   100
C                                                                       NF   110
CFZJ031                                                       28.05.04  NF   120
CFZJ062                                                        4.05.11  NF   130
      COMMON /ORIGEN/ LOB,NOR,VOR(100),ISPK,KFISS,N200C,NXSC,KGR,       NF   140
     1 LOBN,IEZ,FSP(33)                                                 NF   150
C                                                                       NF   160
      DIMENSION SIGA(N26),SIGS(N26),SIGFNU(N26),PHI(N26),SIG(N26),      NF   170
     1 PHIN(N26),TRSIG(LT0),NHOT(N200),DEN(KMAT,N200),SIN(N26),VOL(N200)NF   180
C                                                                       NF   190
  100 FORMAT (//1X,'K-INF DID NOT CONVERGE AFTER 100 ITERATIONS.'//)    NF   200
  200 FORMAT (/' *** FLUXES:',8E12.5)                                   NF   210
  300 FORMAT (' *** K-INF: ',8E12.5)                                    NF   220
C                                                                       NF   230
C                                                                       NF   240
      RLAM = 1.                                                         NF   250
      RKINF = 0.                                                        NF   260
      PHT = 0.                                                          NF   270
      N = 0                                                             NF   280
      DO 1 I=1,N26                                                      NF   290
        SIG(I) = SIGA(I) + SIGS(I)                                      NF   300
    1 CONTINUE                                                          NF   310
    2 N = N + 1                                                         NF   320
      PHI(1) = RLAM / SIG(1) * FSP(1)                                   NF   330
      DO 3 I=2,N26                                                      NF   340
        SIN(I) = 0.                                                     NF   350
        N25 = N26 - 1                                                   NF   360
        RV = 0.                                                         NF   370
        DO 12 IR=1,N200                                                 NF   380
          IF(NHOT(IR) .NE. ISP) GOTO 12                                 NF   390
          RV = RV + VOL(IR)                                             NF   400
          DO 11 N=1,KMAT                                                NF   410
            ITM = ((NHOT(IR)-1)*KMAT+(N-1)) * LIMT + I - 1              NF   420
            DO 10 J=1,I-1                                               NF   430
              SIN(I) = SIN(I) + PHI(J) * DEN(N,IR) * VOL(IR) *          NF   440
     1         TRSIG(ITM)                                               NF   450
              ITM = ITM + N25 - J                                       NF   460
   10       CONTINUE                                                    NF   470
   11     CONTINUE                                                      NF   480
   12   CONTINUE                                                        NF   490
        SIN(I) = SIN(I) / RV                                            NF   500
        PHI(I) = (SIN(I) + RLAM * FSP(I)) / SIG(I)                      NF   510
    3 CONTINUE                                                          NF   520
      Q = 0.                                                            NF   530
      DO 4 I=1,N26                                                      NF   540
        Q = Q + SIGFNU(I) * PHI(I)                                      NF   550
    4 CONTINUE                                                          NF   560
      IF(Q .LE. 0.) GOTO 5                                              NF   570
      RRLAM = RLAM / Q                                                  NF   580
      RKINF = 1. / RRLAM                                                NF   590
      DELT = (RRLAM-RLAM) / RRLAM                                       NF   600
      IF(ABS(DELT) .LE. 1.E-04) GOTO 5                                  NF   610
      IF(N .GE. 100) GOTO 6                                             NF   620
      RLAM = RRLAM                                                      NF   630
      GOTO 2                                                            NF   640
    5 CONTINUE                                                          NF   650
      DO 7 I=1,N26                                                      NF   660
        PHT = PHT + PHI(I)                                              NF   670
    7 CONTINUE                                                          NF   680
      DO 8 I=1,N26                                                      NF   690
        PHIN(I) = PHI(I) / PHT                                          NF   700
    8 CONTINUE                                                          NF   710
      IF(MUHU(28) .LE. 0) GOTO 9                                        NF   720
      WRITE (NT,200) (PHIN(I),I=1,N26)                                  NF   730
      WRITE (NT,300) RKINF                                              NF   740
    9 RETURN                                                            NF   750
    6 CONTINUE                                                          NF   760
      WRITE (NT,100)                                                    NF   770
      RETURN                                                            NF   780
      END                                                               NF   790