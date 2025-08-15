      SUBROUTINE PRTEIN(IMAX,NMAX,PHI,RAD,T)                            PRT   10
C                                                                       PRT   20
      COMMON /BEZEI/ BI,BN                                              PRT   30
C                                                                       PRT   40
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             PRT   50
C                                                                       PRT   60
      DIMENSION PHI(1),RAD(1),T(IMAZ,NMAZ)                              PRT   70
C                                                                       PRT   80
   35 FORMAT (//9X,A4,15(F7.2,1X))                                      PRT   90
   40 FORMAT (2X,A4)                                                    PRT  100
   95 FORMAT (//5X)                                                     PRT  110
  175 FORMAT (1X,F7.2,5X,15(F6.1,2X))                                   PRT  120
C                                                                       PRT  130
C                                                                       PRT  140
      N1 = 1                                                            PRT  150
  310 N2 = N1 + 14                                                      PRT  160
      IF(N2 .GT. NMAX) N2 = NMAX                                        PRT  170
      WRITE (6,35) BN,(PHI (N),N=N1,N2)                                 PRT  180
      WRITE (6,40) BI                                                   PRT  190
      DO 220 I=1,IMAX                                                   PRT  200
        WRITE (6,175) RAD(I),(T(I,N),N=N1,N2)                           PRT  210
  220 CONTINUE                                                          PRT  220
      IF(N2 .EQ. NMAX) GOTO 210                                         PRT  230
      N1 = N2 + 1                                                       PRT  240
      GOTO 310                                                          PRT  250
  210 CONTINUE                                                          PRT  260
      WRITE (6,95)                                                      PRT  270
      RETURN                                                            PRT  280
      END                                                               PRT  290
      SUBROUTINE REDUZ(ZEITS,DTEM1,REDEG,REDHAL,TEG,THAL)               RED   10
C                                                                       RED   20
CFZJ042                                                       09.09.05  RED   30
C                                                                       RED   40
C     STEUERUNG DER NACHWAERMELEISTUNGS-BERECHNUNG                      RED   50
C                                                                       RED   60
      COMMON /RSTRT/ IFRSTA,ZRST,ILOGR                                  RED   70
C                                                                       RED   80
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    RED   90
C                                                                       RED  100
CFZJ006 enlarged dimensions common QVAR                       28.11.03  RED  110
      COMMON /QVAR/ DUM(1511),N61,URZ,ZLEKA,ABXEN                       RED  120
C                                                                       RED  130
      COMMON /REDUZR/ VFLIES,T0,Z0,ZU                                   RED  140
C                                                                       RED  150
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             RED  160
C                                                                       RED  170
      REAL IPLOG                                                        RED  180
C                                                                       RED  190
CFZJ042                                                       09.09.05  RED  200
      DIMENSION PEG(4),PHAL(4),ZEG(4),ZHAL(4)                           RED  210
C                                                                       RED  220
      DATA PEG/9.9,7.9,5.4,1.6/,PHAL/21.6,18.6,25.3,6.6/,ZEG/1.,100.,   RED  230
     1 1.E4,1.E6/,ZHAL/1.,100.,1.E4,1.E6/                               RED  240
C                                                                       RED  250
CFZJ042                                                       09.09.05  RED  260
   70 FORMAT (5X,'AMOUNT OF NOBLE-GAS ',F6.2,' %'/5X,'AMOUNT OF HALOGENSRED  270
     1  ',F6.2,' %')                                                    RED  280
C                                                                       RED  290
C                                                                       RED  300
      NM1 = NMAX - 1                                                    RED  310
      T = ZEITS - DTEM1 / 2.                                            RED  320
      IF(IFRSTA .EQ. 1 .OR. IFRSTA .EQ. 3) T = T + ZRST * 3600.         RED  330
C                                                                       RED  340
C     NACHWAERMELEISTUNGS ANTEIL DER EDELGASE UND HALOGENE              RED  350
C     TEG BZW THAL SIND DIE GRENZTEMP. FUER DIE FREISETZUNG AUS DEM CP  RED  360
C                                                                       RED  370
      THAL = 4000.                                                      RED  380
      TEG = 4000.                                                       RED  390
C                                                                       RED  400
      REDEG = IPLOG(T,ZEG,PEG,4)                                        RED  410
C                                                                       RED  420
      REDHAL = IPLOG(T,ZHAL,PHAL,4)                                     RED  430
C                                                                       RED  440
CFZJ042                                                       09.09.05  RED  450
      WRITE (6,70) REDEG,REDHAL                                         RED  460
      RETURN                                                            RED  470
      END                                                               RED  480
      SUBROUTINE REFL(T)                                                EFL   10
C                                                                       EFL   20
C     SETZEN VON ADIABATEN RANDBEDINGUNGEN (AM GITTERRAND)              EFL   30
C     HIERZU WIRD DIE JEWEILIGE NACHBARTEMPERATUR GESETZT               EFL   40
C     --->  KEIN WAERMETRANSPORT AN DIESE MASCHE                        EFL   50
C                                                                       EFL   60
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    EFL   70
C                                                                       EFL   80
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             EFL   90
C                                                                       EFL  100
      DIMENSION T(IMAZ,NMAZ)                                            EFL  110
C                                                                       EFL  120
C                                                                       EFL  130
      DO 10 I=1,IMAX                                                    EFL  140
        IF(IFRFL .EQ. 0) GOTO 20                                        EFL  150
        T(I,1) = T(I,2)                                                 EFL  160
   20   IF(IFRFR .EQ. 0) GOTO 10                                        EFL  170
        T(I,NMAX) = T(I,NMAX-1)                                         EFL  180
   10 CONTINUE                                                          EFL  190
      DO 30 N=1,NMAX                                                    EFL  200
        IF(IFRFI .EQ. 0) GOTO 40                                        EFL  210
        T(1,N) = T(2,N)                                                 EFL  220
   40   IF(IFRFA .EQ. 0) GOTO 30                                        EFL  230
        T(IMAX,N) = T(IMAX-1,N)                                         EFL  240
   30 CONTINUE                                                          EFL  250
      RETURN                                                            EFL  260
      END                                                               EFL  270
      SUBROUTINE REIPO(NGG,XGG,YGG,NGS,XGS,YGS,IPOL)                    REI   10
C                                                                       REI   20
C     INTERPOLATIONSPROGRAMM (ZUR GITTERUEBERTRAGUNG)                   REI   30
C                                                                       REI   40
      DIMENSION XGG(1),YGG(1),XGS(1),YGS(1)                             REI   50
C                                                                       REI   60
C                                                                       REI   70
      IF(NGG .GT. 1) GOTO 15                                            REI   80
      DO 16 NS=1,NGS                                                    REI   90
        YGS(NS) = YGG(1)                                                REI  100
   16 CONTINUE                                                          REI  110
      RETURN                                                            REI  120
   15 DO 20 NS=1,NGS                                                    REI  130
        DO 10 NG=1,NGG                                                  REI  140
          IF(XGS(NS)-XGG(NG)) 70,70,10                                  REI  150
   10   CONTINUE                                                        REI  160
        YGS(NS) = YGG(NGG)                                              REI  170
        GOTO 20                                                         REI  180
   70   IF(NG .NE. 1) GOTO 100                                          REI  190
        YGS(NS) = YGG(1)                                                REI  200
        GOTO 20                                                         REI  210
  100   IG1 = NG                                                        REI  220
        FAK = (XGS(NS)-XGG(IG1-1)) / (XGG(IG1)-XGG(IG1-1))              REI  230
        IF(IPOL .EQ. 1) GOTO 80                                         REI  240
   75   YGS(NS) = FAK * (YGG(IG1)-YGG(IG1-1)) + YGG(IG1-1)              REI  250
        GOTO 20                                                         REI  260
   80   IF(YGG(IG1) .EQ. 0. .OR. YGG(IG1-1) .EQ. 0.) GOTO 75            REI  270
        YL1 = ALOG(YGG(IG1))                                            REI  280
        YL2 = ALOG(YGG(IG1-1))                                          REI  290
        YGS(NS) = FAK * (YL1-YL2) + YL2                                 REI  300
        YGS(NS) = EXP(YGS(NS))                                          REI  310
   20 CONTINUE                                                          REI  320
      RETURN                                                            REI  330
      END                                                               REI  340
      SUBROUTINE SECURE(NHET,ZEITH,IFSEC,N200,POV,ZF,NRY,QNW,DELDZ,QTHX,SEC   10
     1 WTGINT,RAD,PHI,T,WI,IFBER,IFBH,KKB,NHZON,FFTHX,TCOND,VOLPT,POWPT,SEC   20
     2 DOSPT,THETNEW)                                                   SEC   30
C                                                                       SEC   40
CFZJ042                                                       09.09.05  SEC   50
C                                                                       SEC   60
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    SEC   70
C                                                                       SEC   80
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                SEC   90
C                                                                       SEC  100
      COMMON /DYTRAN/ TITEL(20),ANS(21),ANT(21),SIPHI(10),DXE(10),Q(10),SEC  110
     1 OMEGA(3),XNORM                                                   SEC  120
C                                                                       SEC  130
      COMMON /SPECTI/ ITIK(10),DUM(18),NREST,IREST                      SEC  140
C                                                                       SEC  150
CFZJ006 enlarged dimensions common QVAR                       28.11.03  SEC  160
      COMMON /QVAR/ DUMM(1819),JREST,TAU                                SEC  170
C                                                                       SEC  180
      COMMON /MPUTA/ TEIMIN,TEIMAX,EMP0,TAU0,MPUTAU,QWU,DTAU,TEI0       SEC  190
C                                                                       SEC  200
      COMMON /KSUM/ DU(3),VORZ                                          SEC  210
C                                                                       SEC  220
      COMMON /STA/ IST,SB,TEX,JNS,RMI,RMA,TKI,TKA,TC                    SEC  230
C                                                                       SEC  240
CFZJ008 enlarged dimension common STAF                        28.11.03  SEC  250
      COMMON /STAF/ TF,TFC(300)                                         SEC  260
C                                                                       SEC  270
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             SEC  280
C                                                                       SEC  290
CFZJ026                                                       16.03.04  SEC  300
CFZJ042                                                       09.09.05  SEC  310
      COMMON /YEAR/ IYEAR                                               SEC  320
C                                                                       SEC  330
CFZJ042                                                       09.09.05  SEC  340
      COMMON /COUPL/ IPRINT                                             SEC  350
C                                                                       SEC  360
      COMMON /VRT/ MIXM                                                 SEC  370
C                                                                       SEC  380
      COMMON /BSH/ NHZ                                                  SEC  390
C                                                                       SEC  400
      COMMON /TETA/ TTEIN,TTAUS                                         SEC  410
C                                                                       SEC  420
CFZJ042                                                       09.09.05  SEC  430
      DIMENSION POV(N200),ZF(7,N200),NRY(N200),QNW(N200),RAD(IMAZ+1),   SEC  440
     1 PHI(NMAZ+1),T(IMAZ,NMAZ),WI(IMAZ,NMAZ),IFBER(IMAZ,NMAZ),         SEC  450
     2 IFBH(ICO,NCO),KKB(ICO,NCO),NHZON(KMAZ),FFTHX(IMAZ,NMAZ),         SEC  460
     3 TCOND(IMAZ,NMAZ),IYEAR(1000),VOLPT(IMAZ,NMAZ,15),                SEC  470
     4 POWPT(IMAZ,NMAZ,15),DOSPT(IMAZ,NMAZ,15),THETNEW(ICO,NCO,5,15)    SEC  480
C                                                                       SEC  490
      CHARACTER*4 TEXT(20)/'PROG','RAM ','STOP',' BEC','AUSE',' CPU',   SEC  500
     1 '-LIM','IT I','S AL','MOST',' REA','CHED','. RE','STAR',         SEC  510
     2 'T PR','ESER','VED ','AT U','NIT ','7   '/                       SEC  520
C                                                                       SEC  530
    1 FORMAT (/' THERMIX-RESTART: GENERATION OF T AND THETNEW AT TIME', SEC  540
     1 F10.3,' H ON UNIT',I4/)                                          SEC  550
C                                                                       SEC  560
C                                                                       SEC  570
      DO 213 I=1,IMAX                                                   SEC  580
        DO 213 N=1,NMAX                                                 SEC  590
          III = IFBER(I,N)                                              SEC  600
          WI(I,N) = -1.                                                 SEC  610
          IF(III .GT. 0) WI(I,N) = T(I,N)                               SEC  620
  213 CONTINUE                                                          SEC  630
      IF(JREST .LE. 0) GOTO 98                                          SEC  640
      IF(NREST .EQ. 1) GOTO 97                                          SEC  650
      WRITE (JREST) POV,ZF,NRY,QNW,DELDZ,QTHX,TAU,DTAU                  SEC  660
   97 CONTINUE                                                          SEC  670
      ZEITI = ZEITH + VORZ                                              SEC  680
CFZJ026                                                       16.03.04  SEC  690
CFZJ042                                                       09.09.05  SEC  700
      WRITE (JREST) ZEITI,T,THETNEW,WTGINT,TEX,TC,TF,FFTHX,IYEAR,VOLPT, SEC  710
     1 POWPT,DOSPT,MIXM,NHZ,TTEIN,TTAUS                                 SEC  720
      ENDFILE JREST                                                     SEC  730
      REWIND JREST                                                      SEC  740
      WRITE (6,1) ZEITI,JREST                                           SEC  750
   98 CONTINUE                                                          SEC  760
      IF(IFSEC .EQ. 0) RETURN                                           SEC  770
C                                                                       SEC  780
      CALL ABEND(0)                                                     SEC  790
C                                                                       SEC  800
      CP0 = 0.                                                          SEC  810
C                                                                       SEC  820
CFZJ042                                                       09.09.05  SEC  830
      IF(IPRINT .GT. -3) CALL BILD(1,TEXT,CP0)                          SEC  840
C                                                                       SEC  850
      CALL EXIT                                                         SEC  860
C                                                                       SEC  870
      RETURN                                                            SEC  880
      END                                                               SEC  890
      SUBROUTINE SETBER(I1,I2,I3,I4,KZ)                                 ETB   10
C                                                                       ETB   20
      DIMENSION KZ(4),IZ(4),MERK(4)                                     ETB   30
C                                                                       ETB   40
C                                                                       ETB   50
      IZ(1) = I1                                                        ETB   60
      IZ(2) = I2                                                        ETB   70
      IZ(3) = I3                                                        ETB   80
      IZ(4) = I4                                                        ETB   90
      IM = 0                                                            ETB  100
      DO 20 J=1,4                                                       ETB  110
        DO 10 K=1,4                                                     ETB  120
          IF(KZ(K) .NE. IZ(J)) GOTO 10                                  ETB  130
          IM = IM + 1                                                   ETB  140
          MERK(IM) = KZ(K)                                              ETB  150
          GOTO 20                                                       ETB  160
   10   CONTINUE                                                        ETB  170
   20 CONTINUE                                                          ETB  180
      DO 30 K=1,4                                                       ETB  190
        DO 40 I=1,IM                                                    ETB  200
          IF(KZ(K) .EQ. MERK(I)) GOTO 30                                ETB  210
   40   CONTINUE                                                        ETB  220
        KZ(K) = 0                                                       ETB  230
   30 CONTINUE                                                          ETB  240
      RETURN                                                            ETB  250
      END                                                               ETB  260
      SUBROUTINE SETE(QNORM,N200,NDR,DELDZ,QTHX,KMAT,PHIP,RADP,KOM,KART,ETE   10
     1 WPR,T,WQ,WQR,E,TQ,BU1,RAD,PHI,RE,PHE,WG,WS,XE,IFHET,MIX,NRG,     ETE   20
     2 PDTHERM,POWPART,VOLPART,POWPT,VOLPT,AGEFAC,VL,DOSD,DOSPART,DOSPT,ETE   30
     3 VREG)                                                            ETE   40
C                                                                       ETE   50
CFZJ042                                                       09.09.05  ETE   60
C                                                                       ETE   70
C     SETZEN DER LEISTUNGSDICHTE                                        ETE   80
C                                                                       ETE   90
      COMMON /PRINT1/ TITLE(20),INDGEO                                  ETE  100
C                                                                       ETE  110
      COMMON /CALC/ IFWQ                                                ETE  120
C                                                                       ETE  130
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    ETE  140
C                                                                       ETE  150
      COMMON /SPECTI/ ITIK(10)                                          ETE  160
C                                                                       ETE  170
      COMMON /KONTHX/ FALAST,NEU                                        ETE  180
C                                                                       ETE  190
CFZJ006 enlarged dimensions common QVAR                       28.11.03  ETE  200
      COMMON /QVAR/ DUM(1511),N61,URZ,ZLEKA,ABXEN,D(617),NJ             ETE  210
C                                                                       ETE  220
CFZJ042                                                       09.09.05  ETE  230
CFZJ048 enlarged dimension                                    11.04.07  ETE  240
      COMMON /VARDIT/ B(5000000)                                        ETE  250
C                                                                       ETE  260
CFZJ042                                                       09.09.05  ETE  270
      COMMON /ADDRT/ KX(240),KY(240),LZ(240),NENDPT                     ETE  280
C                                                                       ETE  290
      COMMON /VARDIM/ A(8000000)                                        ETE  300
C                                                                       ETE  310
      COMMON /ADDR/ KA(200),KL(200),LI(200),NENDP                       ETE  320
C                                                                       ETE  330
CFZJ055                                                       25.09.07  ETE  340
C                                                                       ETE  350
      COMMON /TRANS/ IFINST,INTVAL                                      ETE  360
C                                                                       ETE  370
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             ETE  380
C                                                                       ETE  390
CFZJ042                                                       09.09.05  ETE  400
      COMMON /BLF/ IPASS,IDIFFA,IDIFFE,NDIFFA,NDIFFE                    ETE  410
C                                                                       ETE  420
      COMMON /BIRTER/ NTHX,N1,NMAXC,N2                                  ETE  430
CFZJ038                                                       14.12.04  ETE  440
      COMMON /PDTX/ PDTHX,RENORM,WQSUM,QNORM1,INORM                     ETE  450
C                                                                       ETE  460
CFZJ042                                                       09.09.05  ETE  470
      COMMON /BLOTIK/ DUMMY(8),LAYER                                    ETE  480
C                                                                       ETE  490
      COMMON /VRT/ MIXM                                                 ETE  500
C                                                                       ETE  510
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                ETE  520
C                                                                       ETE  530
      COMMON /COUPL/ IPRINT                                             ETE  540
C                                                                       ETE  550
CFZJ042                                                       09.09.05  ETE  560
      DIMENSION PHIP(NMAZ),RADP(IMAZ),KOM(IMAZ,NMAZ),KART(KMAZ),        ETE  570
     1 WPR(KMAZ),T(IMAZ,NMAZ),WQ(IMAZ,NMAZ),WQR(IMAZ,NMAZ),E(IMAZ,NMAZ),ETE  580
     2 TQ(IMAZ,NMAZ),BU1(IMAZ,NMAZ),RAD(IMAZ+1),PHI(NMAZ+1),RE(IMAZ),   ETE  590
     3 PHE(NMAZ),WG(NMAZ),WS(NMAZ),XE(NMAZ),IFHET(KMAZ),MIX(NDR),       ETE  600
     4 NRG(N200),PDTHERM(IMAZ,NMAZ),POWPART(NDR,15),VOLPART(NDR,15),    ETE  610
     5 POWPT(IMAZ,NMAZ,15),VOLPT(IMAZ,NMAZ,15),VL(N200),POWPAR(N200),   ETE  620
     6 VOLPAR(N200),DOSPAR(N200),DFELD(IMAZ,NMAZ),AGEFAC(N200),         ETE  630
     7 VFELD(IMAZ,NMAZ),PFELD(IMAZ,NMAZ),DOSPART(NDR,15),DOSD(N200),    ETE  640
     8 DOSPT(IMAZ,NMAZ,15),VREG(NDR)                                    ETE  650
C                                                                       ETE  660
CFZJ042                                                       09.09.05  ETE  670
      EQUIVALENCE(LZ(2),IBUR),(LZ(3),IDOS),(LZ(4),IPOW),(LZ(5),IJZON),  ETE  680
     1 (LZ(7),IPOWL),(LZ(9),IV),(LZ(10),IPF),(LZ(101),IVQ),(LZ(103),IDU)ETE  690
     2 ,(LZ(11),INOPO),(LZ(13),INCOL),(LZ(14),IVCOL),(LZ(20),IPOV),     ETE  700
     3 (LZ(21),IZF),(LZ(22),INRY),(LZ(23),IQNW),(LZ(24),IHM),           ETE  710
     4 (LZ(28),IIREG),(LZ(100),IDOSI)                                   ETE  720
C                                                                       ETE  730
      CHARACTER*4 BEZ1(3)/'QCM ','QCM ','M**3'/,BEZ2(3)/'W/CM','W/CM',  ETE  740
     1 ' KW '/,EPR(12)/'HEAT',' SOU','RCES',' (W/','CCM)',7*'    '/,    ETE  750
     2 FDE(12)/'FRAC','T. D','UMMY',' ELE','MENT','S OR',' REF','LECT', ETE  760
     3 'OR M','ATER','IAL ','    '/,BE1,BE2                             ETE  770
C                                                                       ETE  780
    3 FORMAT (/' ** FACTOR VSOP-POWER/THERMIX-FULL-POWER:',E12.5,' **'/)ETE  790
    4 FORMAT (1H1)                                                      ETE  800
   11 FORMAT (2I5,6E10.3)                                               ETE  810
   13 FORMAT (20A4)                                                     ETE  820
   16 FORMAT (1H0,10X,'POWER DENSITY OF CASE:',20A4/)                   ETE  830
   25 FORMAT (7E10.3)                                                   ETE  840
  240 FORMAT (////5X,'TOTAL HEAT PRODUCTION IN GRID =',1PE10.3,A4,' IN',ETE  850
     1 E10.3,' ',A4)                                                    ETE  860
  268 FORMAT (///10X,'*** ATTENTION, SCALE-FACTOR FOR OUTPUT OF HEAT SOUETE  870
     1RCES IS',F7.1,' ***')                                             ETE  880
C                                                                       ETE  890
C                                                                       ETE  900
CFZJ042                                                       09.09.05  ETE  910
      N51 = 51                                                          ETE  920
      BE1 = BEZ1(INDGEO+1)                                              ETE  930
      BE2 = BEZ2(INDGEO+1)                                              ETE  940
      IM1 = IMAX - 1                                                    ETE  950
      NM1 = NMAX - 1                                                    ETE  960
      DO 7 I=1,IMAX                                                     ETE  970
        DO 7 N=1,NMAX                                                   ETE  980
          T(I,N) = 0.                                                   ETE  990
          WQ(I,N) = 0.                                                  ETE 1000
          WQR(I,N) = 0.                                                 ETE 1010
    7 CONTINUE                                                          ETE 1020
C                                                                       ETE 1030
CFZJ042                                                       09.09.05  ETE 1040
      CALL POWTHX(QNORM,N200,NDR,VL,B(KX(IBUR)),B(KX(IDOS)),B(KX(IPOW)),ETE 1050
     1 B(KX(IJZON)),B(KX(IPOWL)),B(KX(IV)),B(KX(IPF)),B(KX(INOPO)),     ETE 1060
     2 B(KX(INCOL)),B(KX(IVCOL)),B(KX(IPOV)),B(KX(IZF)),B(KX(INRY)),    ETE 1070
     3 B(KX(IQNW)),B(KX(IHM)),B(KX(IIREG)),DELDZ,QTHX,KMAT,T,WQ,RAD,PHI,ETE 1080
     4 B(KX(IDOSI)),B(KX(IVQ)),KOM,IFHET,MIX,NRG,POWPT,VOLPT,DOSPT,DOSD)ETE 1090
C                                                                       ETE 1100
      FALAST = FALAST / QNORM                                           ETE 1110
CFZJ042                                                       09.09.05  ETE 1120
      IF(NJ .NE. 3 .AND. IPRINT .LE. -3) WRITE (6,4)                    ETE 1130
      IF(NJ .NE. 3) WRITE (6,3) FALAST                                  ETE 1140
      IF(ITIK(9) .NE. 1) GOTO 9                                         ETE 1150
      DO 8 I=1,IMAX                                                     ETE 1160
        DO 8 N=1,NMAX                                                   ETE 1170
          BU1(I,N) = TQ(I,N)                                            ETE 1180
    8 CONTINUE                                                          ETE 1190
    9 CONTINUE                                                          ETE 1200
      IF(ITIK(9) .EQ. 2) GOTO 99                                        ETE 1210
      KIND = 1                                                          ETE 1220
CFZJ042                                                       09.09.05  ETE 1230
      ID = 1                                                            ETE 1240
C                                                                       ETE 1250
      CALL VOLMA1(KIND,B(KX(IPOWL)),B(KX(IQNW)),N200,T,B(KX(IDU)),POWPT,ETE 1260
     1 VOLPT,DOSPT,NDR,ID)                                              ETE 1270
C                                                                       ETE 1280
      MIXMAX = 1                                                        ETE 1290
      DO 310 I=1,NDR                                                    ETE 1300
        MIXM = MAX0(MIXMAX,MIX(I))                                      ETE 1310
        MIXMAX = MIXM                                                   ETE 1320
  310 CONTINUE                                                          ETE 1330
      DO 301 ID=1,MIXM                                                  ETE 1340
        IBS = 0                                                         ETE 1350
        DO 300 K=1,LAYER                                                ETE 1360
          IS = IBS + ID                                                 ETE 1370
          IBS = IBS + MIX(K)                                            ETE 1380
          IF(ID .GT. MIX(K)) GOTO 300                                   ETE 1390
          POWPART(K,ID) = AGEFAC(IS)                                    ETE 1400
          DOSPART(K,ID) = DOSD(IS)                                      ETE 1410
          VOLPART(K,ID) = 1.                                            ETE 1420
          IF(MIXM .GT. 1) VOLPART(K,ID) = VL(IS) / VREG(K)              ETE 1430
  300   CONTINUE                                                        ETE 1440
  301 CONTINUE                                                          ETE 1450
      DO 302 ID=1,MIXM                                                  ETE 1460
        DO 307 I=1,LAYER                                                ETE 1470
          POWPAR(I) = POWPART(I,ID)                                     ETE 1480
          VOLPAR(I) = VOLPART(I,ID)                                     ETE 1490
          DOSPAR(I) = DOSPART(I,ID)                                     ETE 1500
  307   CONTINUE                                                        ETE 1510
        KIND = 5                                                        ETE 1520
C                                                                       ETE 1530
        CALL VOLMA1(KIND,POWPAR,B(KX(IQNW)),N200,T,B(KX(IDU)),POWPT,    ETE 1540
     1   VOLPT,DOSPT,NDR,ID)                                            ETE 1550
C                                                                       ETE 1560
        KIND = 6                                                        ETE 1570
C                                                                       ETE 1580
        CALL VOLMA1(KIND,VOLPAR,B(KX(IQNW)),N200,T,B(KX(IDU)),POWPT,    ETE 1590
     1   VOLPT,DOSPT,NDR,ID)                                            ETE 1600
C                                                                       ETE 1610
        KIND = 7                                                        ETE 1620
C                                                                       ETE 1630
        CALL VOLMA1(KIND,DOSPAR,B(KX(IQNW)),N200,T,B(KX(IDU)),POWPT,    ETE 1640
     1   VOLPT,DOSPT,NDR,ID)                                            ETE 1650
C                                                                       ETE 1660
        DO 303 N=1,NMAX                                                 ETE 1670
          DO 303 I=1,IMAX                                               ETE 1680
            IF(I .EQ. IDIFFE) VOLPT(I,N,ID) = VOLPT(I-1,N,ID)           ETE 1690
            IF(I .EQ. IDIFFE) POWPT(I,N,ID) = POWPT(I-1,N,ID)           ETE 1700
            IF(I .EQ. IDIFFE) DOSPT(I,N,ID) = DOSPT(I-1,N,ID)           ETE 1710
            IF(N .EQ. NDIFFE) VOLPT(I,N,ID) = VOLPT(I,N-1,ID)           ETE 1720
            IF(N .EQ. NDIFFE) DOSPT(I,N,ID) = DOSPT(I,N-1,ID)           ETE 1730
  303   CONTINUE                                                        ETE 1740
  302 CONTINUE                                                          ETE 1750
      DO 933 ID=1,MIXM                                                  ETE 1760
        DO 932 N=NMH,2,-1                                               ETE 1770
          DO 931 I=1,IMH                                                ETE 1780
            POWPT(I+IDIFF,N+NDIFF,ID) = POWPT(I+IDIFF,N+NDIFF-1,ID)     ETE 1790
  931     CONTINUE                                                      ETE 1800
  932   CONTINUE                                                        ETE 1810
  933 CONTINUE                                                          ETE 1820
      DO 308 N=1,NMAX                                                   ETE 1830
        DO 308 I=1,IMAX                                                 ETE 1840
          VSUM = 0.                                                     ETE 1850
          PSUM = 0.                                                     ETE 1860
          DO 304 ID=1,MIXM                                              ETE 1870
            IF(VOLPT(I,N,ID) .LT. 0.01) VOLPT(I,N,ID) = 0.              ETE 1880
            IF(VOLPT(I,N,ID) .LE. 0.) POWPT(I,N,ID) = 0.                ETE 1890
            VSUM = VSUM + VOLPT(I,N,ID)                                 ETE 1900
            PSUM = PSUM + POWPT(I,N,ID)                                 ETE 1910
  304     CONTINUE                                                      ETE 1920
C                                                                       ETE 1930
C     RENORMALIZATION                                                   ETE 1940
C                                                                       ETE 1950
          DO 306 ID=1,MIXM                                              ETE 1960
            IF(VSUM .GT. 0.) VOLPT(I,N,ID) = VOLPT(I,N,ID) / VSUM       ETE 1970
            IF(PSUM .GT. 0.) POWPT(I,N,ID) = POWPT(I,N,ID) / PSUM       ETE 1980
  306     CONTINUE                                                      ETE 1990
  308 CONTINUE                                                          ETE 2000
      DO 63 I=1,IMAX                                                    ETE 2010
        DO 63 N=1,NMAX                                                  ETE 2020
          TQ(I,N) = 0.                                                  ETE 2030
          E(I,N) = 0.                                                   ETE 2040
   63 CONTINUE                                                          ETE 2050
      DO 65 I=1,IM1                                                     ETE 2060
        DO 65 N=1,NM1                                                   ETE 2070
          K = KOM(I,N)                                                  ETE 2080
          IF(KART(K) .EQ. 2) WPR(K) = 0.                                ETE 2090
          IF(WPR(K) .EQ. -1) GOTO 64                                    ETE 2100
          T(I,N) = WPR(K)                                               ETE 2110
   64     CONTINUE                                                      ETE 2120
          IF(T(I,N) .EQ. 0.) GOTO 62                                    ETE 2130
          IFWQ = 1                                                      ETE 2140
   62     CONTINUE                                                      ETE 2150
          N1C = N1 + NMAXC                                              ETE 2160
          IF(INTVAL .GT. 1) E(I,N) = T(I,N)                             ETE 2170
          IF(N .GT. N1C) GOTO 66                                        ETE 2180
          IF(INTVAL .EQ. 1) E(I,N+N1) = PDTHERM(I,N+N2)                 ETE 2190
   66     CONTINUE                                                      ETE 2200
          IF(WPR(K) .GT. 0) E(I,N) = WPR(K)                             ETE 2210
   65 CONTINUE                                                          ETE 2220
      WFGES = 0.                                                        ETE 2230
      DO 220 I=1,IM1                                                    ETE 2240
        DO 220 N=2,NM1                                                  ETE 2250
          DF = 0.                                                       ETE 2260
          IF(I .NE. 1) R1 = (RAD(I-1)+RAD(I)) / 2.                      ETE 2270
          R2 = RAD(I)                                                   ETE 2280
          R3 = (RAD(I)+RAD(I+1)) / 2.                                   ETE 2290
          P1 = PHIP(N-1)                                                ETE 2300
          P2 = PHI(N)                                                   ETE 2310
          P3 = PHIP(N)                                                  ETE 2320
          K = KOM(I,N)                                                  ETE 2330
          EIN = E(I,N)                                                  ETE 2340
C                                                                       ETE 2350
          DFH = DDF1(R3,R2,P3,P2,INDGEO,R3)                             ETE 2360
C                                                                       ETE 2370
          IF(KART(K) .NE. 2) DF = DF + DFH                              ETE 2380
          IF(EIN .NE. 0.) WFGES = WFGES + DFH                           ETE 2390
          TQ(I,N) = TQ(I,N) + DFH * EIN                                 ETE 2400
          IF(I .EQ. 1) GOTO 221                                         ETE 2410
          K = KOM(I-1,N)                                                ETE 2420
          EIN = E(I-1,N)                                                ETE 2430
C                                                                       ETE 2440
          DFH = DDF1(R2,R1,P3,P2,INDGEO,R1)                             ETE 2450
C                                                                       ETE 2460
          IF(KART(K) .NE. 2) DF = DF + DFH                              ETE 2470
          IF(EIN .NE. 0.) WFGES = WFGES + DFH                           ETE 2480
          TQ(I,N) = TQ(I,N) + DFH * EIN                                 ETE 2490
          K = KOM(I-1,N-1)                                              ETE 2500
          EIN = E(I-1,N-1)                                              ETE 2510
C                                                                       ETE 2520
          DFH = DDF1(R2,R1,P2,P1,INDGEO,R1)                             ETE 2530
C                                                                       ETE 2540
          IF(KART(K) .NE. 2) DF = DF + DFH                              ETE 2550
          IF(EIN .NE. 0.) WFGES = WFGES + DFH                           ETE 2560
          TQ(I,N) = TQ(I,N) + DFH * EIN                                 ETE 2570
  221     CONTINUE                                                      ETE 2580
          K = KOM(I,N-1)                                                ETE 2590
          EIN = E(I,N-1)                                                ETE 2600
C                                                                       ETE 2610
          DFH = DDF1(R3,R2,P2,P1,INDGEO,R3)                             ETE 2620
C                                                                       ETE 2630
          IF(KART(K) .NE. 2) DF = DF + DFH                              ETE 2640
          IF(EIN .NE. 0.) WFGES = WFGES + DFH                           ETE 2650
          TQ(I,N) = TQ(I,N) + DFH * EIN                                 ETE 2660
          IF(INDGEO .NE. 2) GOTO 68                                     ETE 2670
          DF = DF * 6.283                                               ETE 2680
          TQ(I,N) = TQ(I,N) * 6.283                                     ETE 2690
   68     T(I,N) = DF                                                   ETE 2700
          IF(DF .EQ. 0.) E(I,N) = 0.                                    ETE 2710
  220 CONTINUE                                                          ETE 2720
      IF(INDGEO .EQ. 2) WFGES = WFGES * 6.283                           ETE 2730
      IF(IFWQ .EQ. 0) RETURN                                            ETE 2740
      IFNORM = 0                                                        ETE 2750
      IF(INDGEO .NE. 1 .OR. RAD0 .NE. 0.) GOTO 211                      ETE 2760
      DO 212 N=2,NM1                                                    ETE 2770
        DPHE = (PHI(N+1)-PHI(N-1)) / 2.                                 ETE 2780
        RAD1 = RADP(1)**2.                                              ETE 2790
        WFGES = WFGES + DPHE * RAD1 / 2.                                ETE 2800
        RAD2 = RADP(2)**2.                                              ETE 2810
        TQ(2,N) = E(2,N) * RAD2 / 2. * DPHE                             ETE 2820
  212 CONTINUE                                                          ETE 2830
  211 CONTINUE                                                          ETE 2840
      TQGES = 0.                                                        ETE 2850
      DO 230 I=1,IM1                                                    ETE 2860
        DO 230 N=2,NM1                                                  ETE 2870
          TQGES = TQGES + TQ(I,N)                                       ETE 2880
  230 CONTINUE                                                          ETE 2890
      IF(IFNORM .EQ. 1) GOTO 263                                        ETE 2900
      IF(QNORM .EQ. 0.) GOTO 263                                        ETE 2910
      FAK = QNORM / TQGES                                               ETE 2920
      IF(ITIK(1) .GT. 1) FAK = FAK * FALAST                             ETE 2930
CFZJ038                                                       14.12.04  ETE 2940
      QNORM1 = QNORM * FAK                                              ETE 2950
      DO 265 I=1,IMAX                                                   ETE 2960
        DO 265 N=1,NMAX                                                 ETE 2970
          TQ(I,N) = TQ(I,N) * FAK                                       ETE 2980
          E(I,N) = E(I,N) * FAK                                         ETE 2990
  265 CONTINUE                                                          ETE 3000
      IFNORM = 1                                                        ETE 3010
      REWIND N51                                                        ETE 3020
      WRITE (N51) E                                                     ETE 3030
      GOTO 211                                                          ETE 3040
  263 CONTINUE                                                          ETE 3050
      EMAX = 0.                                                         ETE 3060
      DO 290 I=1,IMAX                                                   ETE 3070
        DO 290 N=1,NMAX                                                 ETE 3080
          EIN = E(I,N)                                                  ETE 3090
          AEIN = ABS(EIN)                                               ETE 3100
          IF(AEIN .GT. EMAX) EMAX = AEIN                                ETE 3110
  290 CONTINUE                                                          ETE 3120
      SKFAK = 1.                                                        ETE 3130
      IF(EMAX .LT. 10.) SKFAK = 10.                                     ETE 3140
      IF(EMAX .LT. 1.) SKFAK = 1000.                                    ETE 3150
      DO 266 I=1,IMAX                                                   ETE 3160
        DO 266 N=1,NMAX                                                 ETE 3170
          E(I,N) = E(I,N) * SKFAK                                       ETE 3180
  266 CONTINUE                                                          ETE 3190
CFZJ042                                                       09.09.05  ETE 3200
      IF(SKFAK .NE. 1. .AND. IPRINT. GT. -3) WRITE (6,268) SKFAK        ETE 3210
      IF(INDGEO .NE. 2) GOTO 235                                        ETE 3220
      TQGES = TQGES / 1000.                                             ETE 3230
      WFGES = WFGES / 1000000.                                          ETE 3240
  235 CONTINUE                                                          ETE 3250
C                                                                       ETE 3260
      CALL MARK(0,KOM,KART)                                             ETE 3270
C                                                                       ETE 3280
      IF(INTVAL .GT. 1) GOTO 100                                        ETE 3290
C                                                                       ETE 3300
C     TABLE OF HEAT SOURCES                                             ETE 3310
C                                                                       ETE 3320
      CALL PRFELD(E,IM1,NM1,EPR,0,PHI,RAD,1)                            ETE 3330
C                                                                       ETE 3340
      WRITE (6,240) TQGES,BE2,WFGES,BE1                                 ETE 3350
  100 CONTINUE                                                          ETE 3360
CFZJ042                                                       09.09.05  ETE 3370
      DO 269 I=1,IMAX                                                   ETE 3380
        DO 269 N=1,NMAX                                                 ETE 3390
          E(I,N) = E(I,N) / SKFAK                                       ETE 3400
  269 CONTINUE                                                          ETE 3410
   99 RETURN                                                            ETE 3420
      END                                                               ETE 3430
      SUBROUTINE POWTHX(QNORM,N200,NDR,VL,BUR,DOS,POW,JZON,POWL,V,PF,   POW   10
     1 NOPOW,NCOLA,VCOLA,POV,ZF,NRY,QNW,HM,IREG,DELDZ,QTHX,KMAT,T,WQ,RADPOW   20
     2 ,PHI,DOSI,VQ,KOM,IFHET,MIX,NRG,POWPT,VOLPT,DOSPT,DOSD)           POW   30
C                                                                       POW   40
CFZJ042                                                       09.09.05  POW   50
C                                                                       POW   60
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    POW   70
C                                                                       POW   80
      COMMON /BLOTIK/ N197,HCORE,NRAD,POWER,IX,JZ,ISP,NLIB,LAYER,DELZ,  POW   90
     1 TIN,TOUT,LAY(20),RAT(20),JR(20),RZ(2,50),IZ(2,50),Q(50,50,2),    POW  100
     2 A(3,25,25),ILA                                                   POW  110
C                                                                       POW  120
CFZJ017 Delete unused variable BK (BUR 7190, BUR 7200)        12.12.03  POW  130
      COMMON /ZEUG/ ITM3,IFZW,CRLSTG,IAX,IRA,MRZ,MERZ(5),RADI(50),      POW  140
     1 TQ(5,50,20),SD(60,50,30),AB(20),DENKGL,NRUN,R3,R4,PIE,HCOR,ISPALTPOW  150
     2 ,JIC,BRENKU(50),IB(19),RR,KRUN,POWBAL,TR2,FADO,TT,TDR,WL0,WL2,   POW  160
     3 TDRR2,TINTE,WIFI,IBU,N6,TG,TM,TGAS,FRACKU,ISUBRG,DELDAY          POW  170
C                                                                       POW  180
      COMMON /FORALL/ IRAGRO,JINPO1,UU,VV,ZGROB(500),POGROB(500),IJ     POW  190
C                                                                       POW  200
CFZJ004 enlarged dimensions common trans                      28.11.03  POW  210
      COMMON /TRANS/ IFINST,INTVAL,DZEIT(300),ZEI(300),NPRINT(300),     POW  220
     1 NKONV(300)                                                       POW  230
C                                                                       POW  240
      COMMON /SPECTI/ ITIK(10),DUN(18),NREST,IREST                      POW  250
C                                                                       POW  260
      COMMON /KONTHX/ FALAST,NEU                                        POW  270
C                                                                       POW  280
      COMMON /OPT/ KENN,IOPUT,NIFKO,DZT                                 POW  290
C                                                                       POW  300
CFZJ006 enlarged dimensions common QVAR                       28.11.03  POW  310
      COMMON /QVAR/ IVAR,ENDKEF,QVOLLL,QREDUZ,QREMAX,EPQ,EPC,DQDDC,DELC,POW  320
     1 DCN,SBU,TE(4,300),TA(300),N61,DMY(306),JRESTR,JREST,TAU          POW  330
C                                                                       POW  340
CFZJ042                                                       09.09.05  POW  350
      COMMON /BLINDL/ TMITL,M24,NGEOM,CIZET0                            POW  360
C                                                                       POW  370
      COMMON /MPUTA/ TEIMIN,TEIMAX,EMP0,TAU0,MPUTAU,QWU,DTAU,TEI0       POW  380
C                                                                       POW  390
      COMMON /RABI/ RADR(20)                                            POW  400
C                                                                       POW  410
      COMMON /DL/ ILAYV,DLAY(101)                                       POW  420
C                                                                       POW  430
CFZJ055                                                       25.09.07  POW  440
C                                                                       POW  450
CFZJ048 enlarged dimension                                    11.04.07  POW  460
      COMMON /VARDIT/ B(5000000)                                        POW  470
C                                                                       POW  480
CFZJ042                                                       09.09.05  POW  490
      COMMON /ADDRT/ KX(240),KY(240),LZ(240),NENDPT                     POW  500
C                                                                       POW  510
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             POW  520
C                                                                       POW  530
CFZJ042                                                       09.09.05  POW  540
      COMMON /BLF/ IPASS,IDIFFA,IDIFFE,NDIFFA,NDIFFE                    POW  550
C                                                                       POW  560
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                POW  570
C                                                                       POW  580
CFZJ042                                                       09.09.05  POW  590
      DIMENSION VL(N200),BUR(N200),DOS(N200),POW(N200),JZON(N200),      POW  600
     1 POWL(N200),V(15,N200),PF(15,N200),NOPOW(N200),NCOLA(NDR),        POW  610
     2 VCOLA(NDR),POV(N200),ZF(7,N200),NRY(N200),QNW(N200),HM(N200),    POW  620
     3 IREG(NDR),T(IMAZ,NMAZ),WQ(IMAZ,NMAZ),RAD(IMAZ+1),PHI(NMAZ+1),    POW  630
     4 DOSI(IMAZ,NMAZ),VQ(NMAZ+1),KOM(IMAZ,NMAZ),IFHET(KMAZ),MIX(NDR),  POW  640
     5 NRG(N200),POWPT(IMAZ,NMAZ,15),VOLPT(IMAZ,NMAZ,15),               POW  650
     6 DOSPT(IMAZ,NMAZ,15),DOSD(N200)                                   POW  660
C                                                                       POW  670
CFZJ042                                                       09.09.05  POW  680
      EQUIVALENCE(LZ(103),IDU),(LZ(104),JTGRA),(LZ(105),ITFUL),         POW  690
     1 (LZ(108),IRT),(LZ(109),IZT),(LZ(110),IRVT)                       POW  700
C                                                                       POW  710
C                                                                       POW  720
CFZJ042                                                       09.09.05  POW  730
      A3600 = 3600.                                                     POW  740
      A24 = 24.                                                         POW  750
      N38 = 38                                                          POW  760
      KENN = 0                                                          POW  770
CFZJ042                                                       09.09.05  POW  780
      DO 15 I=1,N200                                                    POW  790
        DOS(I) = 0.                                                     POW  800
   15 CONTINUE                                                          POW  810
CFZJ017 Delete unused variable BK                             12.12.03  POW  820
      READ (N38) RINN,NRAD,HCORE,POWER,FF1,RR,R3,R4,DELDAY,(LAY(I),     POW  830
     1 IREG(I),RAT(I),I=1,NRAD),(VL(I),BUR(I),DOS(I),POW(I),JZON(I),I=1,POW  840
     2 N200),(NOPOW(I),I=1,N200),RADR,CIZET0,NMAXC,(DLAY(I),I=1,NMAXC)  POW  850
      DO 19 I=1,N200                                                    POW  860
        IF(NOPOW(I) .GT. 1) NOPOW(I) = 1                                POW  870
   19 CONTINUE                                                          POW  880
      RINN = RUND(RINN)                                                 POW  890
      D24 = DELDAY                                                      POW  900
      IF(D24 .NE. 0.) D24 = D24 * A24                                   POW  910
      D25 = D24 * A3600                                                 POW  920
      DZU = ABS(DZT)                                                    POW  930
      IF(DZT .LT. 0.) DZU = -AMIN1(DZU,D25)                             POW  940
      IF(ITIK(9) .GT. 0) GOTO 8                                         POW  950
CFZJ042                                                       09.09.05  POW  960
      READ (N38) ((ZF(I,N),I=1,7),N=1,N200),(NRY(N),N=1,N200),(HM(N),N=1POW  970
     1 ,N200)                                                           POW  980
      QTHX = QNORM                                                      POW  990
      IF(QNORM .LT. 0.) QTHX = ABS(QTHX) * 0.001                        POW 1000
      QNORM = ABS(QNORM)                                                POW 1010
      BACKSPACE N38                                                     POW 1020
      BACKSPACE N38                                                     POW 1030
      PO = 0.                                                           POW 1040
      VO = 0.                                                           POW 1050
      DO 50 I=1,N200                                                    POW 1060
        IF(NOPOW(I) .EQ. 1) GOTO 50                                     POW 1070
        PO = PO + POW(I) * VL(I)                                        POW 1080
        VO = VO + VL(I)                                                 POW 1090
   50 CONTINUE                                                          POW 1100
      PO = PO / VO                                                      POW 1110
    8 CONTINUE                                                          POW 1120
      IF(ITIK(9) .EQ. 2) READ (N38) DUM                                 POW 1130
      FALAST = POWER                                                    POW 1140
      IF(ITIK(10) .NE. 3) GOTO 2                                        POW 1150
      IF(ITIK(9) .LT. 0 .AND. INTVAL .NE. 1) GOTO 2                     POW 1160
      IF(ITIK(9) .LT. 0) INTVAL = 0                                     POW 1170
    9 CONTINUE                                                          POW 1180
      INTVAL = INTVAL + 1                                               POW 1190
      IF(INTVAL .EQ. 1) GOTO 1                                          POW 1200
      NPRINT(INTVAL) = NPRINT(INTVAL-1)                                 POW 1210
      NKONV(INTVAL) = NKONV(INTVAL-1)                                   POW 1220
      DZEIT(INTVAL) = DZU                                               POW 1230
      ZEI(INTVAL) = ZEI(INTVAL-1) + D24                                 POW 1240
      IF(KENN .EQ. 1) GOTO 17                                           POW 1250
      GOTO 2                                                            POW 1260
    1 CONTINUE                                                          POW 1270
      IF(ZEI(INTVAL) .LE. 0.) GOTO 17                                   POW 1280
      KENN = 1                                                          POW 1290
      GOTO 9                                                            POW 1300
   17 CONTINUE                                                          POW 1310
      ZEI(INTVAL) = D24                                                 POW 1320
    2 CONTINUE                                                          POW 1330
      IF(ITIK(9) .NE. 2) GOTO 998                                       POW 1340
      DO 997 I=1,N200                                                   POW 1350
        NOPOW(I) = NOPOW(I) * JZON(I)                                   POW 1360
  997 CONTINUE                                                          POW 1370
  998 CONTINUE                                                          POW 1380
      IF(ITIK(9) .EQ. 2) GOTO 99                                        POW 1390
      KR = 1                                                            POW 1400
      N197 = 0                                                          POW 1410
      LY = 0                                                            POW 1420
      IM = 0                                                            POW 1430
      VS = 0.                                                           POW 1440
      DO 20 I=1,N200                                                    POW 1450
        NREG = NRG(I)                                                   POW 1460
        IF(NOPOW(I) .GT. 0) GOTO 21                                     POW 1470
        IM = IM + 1                                                     POW 1480
        VS = VS + VL(I)                                                 POW 1490
        IF(IM .LT. MIX(NREG)) GOTO 20                                   POW 1500
        IM = 0                                                          POW 1510
        GOTO 22                                                         POW 1520
   21   VS = VL(I) + VS                                                 POW 1530
        IF(NOPOW(I) .EQ. 1) GOTO 22                                     POW 1540
        NOPOW(I) = 1                                                    POW 1550
        GOTO 20                                                         POW 1560
   22   CONTINUE                                                        POW 1570
        LY = LY + 1                                                     POW 1580
C                                                                       POW 1590
C     NCOLA = LETZTE VSOP-COMPOSITION DER "LY"-TEN VSOP-REGION          POW 1600
C                                                                       POW 1610
        NCOLA(LY) = I                                                   POW 1620
        VCOLA(LY) = VS                                                  POW 1630
        VS = 0.                                                         POW 1640
   20 CONTINUE                                                          POW 1650
      DO 13 I=1,N200                                                    POW 1660
        NOPOW(I) = NOPOW(I) * JZON(I)                                   POW 1670
        IF(NOPOW(I) .EQ. 0) GOTO 11                                     POW 1680
        LAY(KR) = LAY(KR) - 1                                           POW 1690
C                                                                       POW 1700
C     FALLS REFLEKTORREGIONS IN MEHRERE COMPOSITIONS UNTERTEILT         POW 1710
C                                                                       POW 1720
        IF(LAY(KR) .LT. 0) LAY(KR) = 0                                  POW 1730
        DO 12 K=KR,NRAD                                                 POW 1740
          IREG(K) = IREG(K) - 1                                         POW 1750
   12   CONTINUE                                                        POW 1760
        GOTO 40                                                         POW 1770
   11   CONTINUE                                                        POW 1780
        N197 = N197 + 1                                                 POW 1790
        VL(N197) = VL(I)                                                POW 1800
        BUR(N197) = BUR(I)                                              POW 1810
        DOS(N197) = DOS(I)                                              POW 1820
CFZJ042                                                       09.09.05  POW 1830
        DOSD(N197) = DOS(I)                                             POW 1840
        POW(N197) = POW(I)                                              POW 1850
        JZON(N197) = JZON(I)                                            POW 1860
        IF(ITIK(9) .GT. 0) GOTO 40                                      POW 1870
        POV(N197) = POW(N197) * VL(N197)                                POW 1880
        HM(N197) = HM(I)                                                POW 1890
        NRY(N197) = NRY(I)                                              POW 1900
        IF(NRY(N197) .NE. 0) NRY(N197) = NRY(N197) - (I-N197)           POW 1910
        DO 39 J=1,7                                                     POW 1920
          ZF(J,N197) = ZF(J,I)                                          POW 1930
   39   CONTINUE                                                        POW 1940
   40   CONTINUE                                                        POW 1950
        IF(IREG(KR) .EQ. N197) KR = KR + 1                              POW 1960
   13 CONTINUE                                                          POW 1970
      N198 = N197 + 1                                                   POW 1980
      DO 16 I=N198,N200                                                 POW 1990
        NRY(I) = 0                                                      POW 2000
        DO 16 J=1,7                                                     POW 2010
          ZF(J,I) = 0.                                                  POW 2020
   16 CONTINUE                                                          POW 2030
      LAYER = 0                                                         POW 2040
      KR = 0                                                            POW 2050
      DO 14 I=1,NRAD                                                    POW 2060
        LAYER = LAYER + LAY(I)                                          POW 2070
        IF(LAYER .EQ. 0) RINN = RAT(I)                                  POW 2080
        IF(LAY(I) .EQ. 0) GOTO 14                                       POW 2090
        KR = KR + 1                                                     POW 2100
        LAY(KR) = LAY(I)                                                POW 2110
        IREG(KR) = IREG(I)                                              POW 2120
        RAT(KR) = RAT(I)                                                POW 2130
   14 CONTINUE                                                          POW 2140
      NRAD = KR                                                         POW 2150
C                                                                       POW 2160
C     LESEN DER VOLUMENMATRIX VSOP-THERMIX                              POW 2170
C                                                                       POW 2180
CFZJ042                                                       09.09.05  POW 2190
      CALL VOLMAT(RINN,N200,RAD,PHI,T,B(KX(IDU)),B(KX(JTGRA)),          POW 2200
     1 B(KX(ITFUL)),KOM,B(KX(IRT)),B(KX(IZT)),B(KX(IRVT)),NDR)          POW 2210
C                                                                       POW 2220
C     VSOP-DOSIS + VOLUME IN COMPOSITIONS AND REGIONS                   POW 2230
C                                                                       POW 2240
      I = 0                                                             POW 2250
      DO 43 L=1,LAYER                                                   POW 2260
        VP = 0.                                                         POW 2270
        VO = 0.                                                         POW 2280
        DO 42 M=1,MIX(L)                                                POW 2290
          I = I + 1                                                     POW 2300
          VP = VP + VL(I) * DOS(I)                                      POW 2310
          VO = VO + VL(I)                                               POW 2320
   42   CONTINUE                                                        POW 2330
        POWL(L) = VP / VO                                               POW 2340
   43 CONTINUE                                                          POW 2350
      LP1 = LAYER + 1                                                   POW 2360
      DO 45 L=LP1,N200                                                  POW 2370
        POWL(L) = 0.                                                    POW 2380
   45 CONTINUE                                                          POW 2390
      KIND = 1                                                          POW 2400
C                                                                       POW 2410
CFZJ042                                                       09.09.05  POW 2420
      CALL VOLMA1(KIND,POWL,QNW,N200,T,B(KX(IDU)),POWPT,VOLPT,DOSPT,NDR,POW 2430
     1 ID)                                                              POW 2440
C                                                                       POW 2450
      DO 44 I=1,IMAX                                                    POW 2460
        DO 44 N=1,NMAX                                                  POW 2470
          DOSI(I,N) = T(I,N)                                            POW 2480
          T(I,N) = 0.                                                   POW 2490
   44 CONTINUE                                                          POW 2500
CFZJ042                                                       09.09.05  POW 2510
      IF(IFINST .EQ. 1 .OR. ITIK(1) .GT. 1) GOTO 105                    POW 2520
CFZJ042                                                       09.09.05  POW 2530
      IDIFFA = IDIFF + 1                                                POW 2540
      IDIFFE = IDIFF + IMH                                              POW 2550
      NDIFFA = NDIFF + 1                                                POW 2560
      NDIFFE = NDIFF + NMH                                              POW 2570
  105 CONTINUE                                                          POW 2580
C                                                                       POW 2590
C     VSOP-POWER + VOLUME IN BATCHES AND REGIONS                        POW 2600
C                                                                       POW 2610
      I = 0                                                             POW 2620
CFZJ042                                                       09.09.05  POW 2630
      DO 33 L=1,LAYER                                                   POW 2640
        VP = 0.                                                         POW 2650
        VO = 0.                                                         POW 2660
        IS = I                                                          POW 2670
        DO 31 M=1,MIX(L)                                                POW 2680
          I = I + 1                                                     POW 2690
          VP = VP + VL(I) * POW(I)                                      POW 2700
          VO = VO + VL(I)                                               POW 2710
   31   CONTINUE                                                        POW 2720
        PP = VP / VO                                                    POW 2730
        POWL(L) = PP                                                    POW 2740
        DO 32 M=1,MIX(L)                                                POW 2750
          IS = IS + 1                                                   POW 2760
          V(M,L) = VL(IS) / VO                                          POW 2770
          IF(PP .EQ. 0.) GOTO 32                                        POW 2780
          PF(M,L) = POW(IS) / PP                                        POW 2790
   32   CONTINUE                                                        POW 2800
   33 CONTINUE                                                          POW 2810
   99 RETURN                                                            POW 2820
      END                                                               POW 2830
      SUBROUTINE VOLMAT(RINN,N200,RAD,PHI,T,DU,TGRA,TFUL,KOM,RT,ZT,RVT, OLM   10
     1 NDR)                                                             OLM   20
C                                                                       OLM   30
CFZJ042                                                       09.09.05  OLM   40
C                                                                       OLM   50
C     VOLUMENMATRIX VSOP-THERMIX AUS SR *BIRGIT*                        OLM   60
C                                                                       OLM   70
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    OLM   80
C                                                                       OLM   90
      COMMON /BLOTIK/ N197,HCOR,NRAD,POWER,IX,JZ,ISP,NLIB,LAYER,DELZ,   OLM  100
     1 TIN,TOUT,LAY(20),RADR(20),JR(20),RZ(2,50),IZ(2,50),Q(50,50,2),   OLM  110
     2 A(3,25,25),ILA                                                   OLM  120
C                                                                       OLM  130
CFZJ048 enlarged dimension                                    11.04.07  OLM  140
      COMMON /KONTHX/ F,NEU,JT0,JT1,JT2,NT0,NT1,NT2,K0,IADVT(4000,2),   OLM  150
     1 VOLVT(4000),PI                                                   OLM  160
C                                                                       OLM  170
      COMMON /SPEZI/ FF                                                 OLM  180
C                                                                       OLM  190
      COMMON /BIRTER/ NTHX,N1                                           OLM  200
C                                                                       OLM  210
      COMMON /SPECTI/ ITIK(10)                                          OLM  220
C                                                                       OLM  230
      COMMON /RABI/ RAT(20)                                             OLM  240
C                                                                       OLM  250
CFZJ002  Common KOMP1 changed, commons KES, BSH added         28.11.03  OLM  260
      COMMON /KOMP1/ KMAX                                               OLM  270
C                                                                       OLM  280
      COMMON /KES/ KS                                                   OLM  290
C                                                                       OLM  300
      COMMON /BSH/ NHZ                                                  OLM  310
C                                                                       OLM  320
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                OLM  330
C                                                                       OLM  340
CFZJ055                                                       25.09.07  OLM  350
C                                                                       OLM  360
      COMMON /DL/ ILAYV,DLAY(101)                                       OLM  370
C                                                                       OLM  380
      COMMON /TRANS/ IFINST,INTVAL                                      OLM  390
C                                                                       OLM  400
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             OLM  410
C                                                                       OLM  420
      COMMON /COUPL/ IPRINT                                             OLM  430
C                                                                       OLM  440
CFZJ042                                                       09.09.05  OLM  450
      COMMON /BLINDL/ TMITL,M24,NGEOM,CIZET0                            OLM  460
C                                                                       OLM  470
CFZJ042                                                       09.09.05  OLM  480
CFZJ048 enlarged dimension                                    11.04.07  OLM  490
      DIMENSION POWL(N200),QNW(N200),TEML(3,N200),RAD(IMAZ+1),          OLM  500
     1 PHI(NMAZ+1),T(IMAZ,NMAZ),DU(IMAZ,NMAZ),TGRA(IMAZ,NMAZ),          OLM  510
     2 TFUL(IMAZ,NMAZ),KOM(IMAZ,NMAZ),RT(ICO+1),ZT(NCO+1),RVT(ICO*2),   OLM  520
     3 RV(21),ZVT(1000),JADVT(4000,2),RDUM3(200),JDUM3(200),            OLM  530
     4 POWPT(IMAZ,NMAZ,15),VOLPT(IMAZ,NMAZ,15),DOSPT(IMAZ,NMAZ,15)      OLM  540
C                                                                       OLM  550
   81 FORMAT (3I3)                                                      OLM  560
   83 FORMAT (15I5)                                                     OLM  570
   85 FORMAT (6(F9.3,I3))                                               OLM  580
   86 FORMAT (3E12.5,I5)                                                OLM  590
   88 FORMAT (3(2I6,E12.5))                                             OLM  600
   89 FORMAT (6(I3,F9.3))                                               OLM  610
   90 FORMAT (6(F8.3,I3,I1))                                            OLM  620
   93 FORMAT (3I10,5X,E13.6)                                            OLM  630
   94 FORMAT (//' SUM OF VOLUME OF MATRIX:',E13.6,' JT2:',I6,' JT0:',I6,OLM  640
     1 ' NT2:',I6,' NT0:',I6)                                           OLM  650
  100 FORMAT (/' *** PROGRAM STOP, BECAUSE VOLUME-MATRIX VSOP-THERMIX ISOLM  660
     1 TOO LARGE ***'/)                                                 OLM  670
  300 FORMAT (15X,'MATRIX OF VOLUMES:'/)                                OLM  680
  501 FORMAT (/' *** PROGRAM STOPS BECAUSE THE DIMENSION OF FIELD *ZVT* OLM  690
     1EXCEEDS THE LIMIT OF 1000. ***')                                  OLM  700
C                                                                       OLM  710
C                                                                       OLM  720
      E = 1.E-5                                                         OLM  730
      KDIM = 2000                                                       OLM  740
      PI = 3.141592654                                                  OLM  750
      HCORE = HCOR                                                      OLM  760
C                                                                       OLM  770
C     VOLUMEN-MATRIX VSOP - THERMIX VON SR *BIRGIT*                     OLM  780
C                                                                       OLM  790
      REWIND NGEOM                                                      OLM  800
      READ (NGEOM,81) JDUM,JDUM1,JDUM2                                  OLM  810
      READ (NGEOM,89) (JDUM3(I),RDUM3(I),I=1,JDUM1-1)                   OLM  820
      READ (NGEOM,89) (JDUM3(I),RDUM3(I),I=1,JDUM2)                     OLM  830
      READ (NGEOM,81) JDUM                                              OLM  840
      DO 82 N=1,JDUM2                                                   OLM  850
        READ (NGEOM,83) (JDUM3(I),I=1,JDUM1-1)                          OLM  860
   82 CONTINUE                                                          OLM  870
      READ (NGEOM,81) JDUM                                              OLM  880
      READ (NGEOM,83) IDUM1,IDUM2                                       OLM  890
      READ (NGEOM,85) (RDUM3(I),JDUM3(I),I=1,IDUM1)                     OLM  900
      READ (NGEOM,90) (RDUM3(I),JDUM3(I),JDUM3(I),I=1,IDUM2)            OLM  910
      READ (NGEOM,86) RTI,RTA,HCO,K0                                    OLM  920
      READ (NGEOM,88) (JADVT(I,2),JADVT(I,1),VOLVT(I),I=1,K0)           OLM  930
      RTI = RUND(RTI)                                                   OLM  940
      RTA = RUND(RTA)                                                   OLM  950
      JT2 = 0                                                           OLM  960
      DO 95 K=1,K0                                                      OLM  970
        IADVT(K,1) = JADVT(K,1)                                         OLM  980
        IADVT(K,2) = JADVT(K,2)                                         OLM  990
   95 CONTINUE                                                          OLM 1000
      DO 91 I=1,IMAX                                                    OLM 1010
        X = RAD(I)                                                      OLM 1020
        X = RUND(X)                                                     OLM 1030
        IF(X .LT. RTI) GOTO 91                                          OLM 1040
        IF(X .GT. RTA) GOTO 91                                          OLM 1050
        IF(JT2 .EQ. 0) JT2 = I                                          OLM 1060
        JT0 = I - 1                                                     OLM 1070
   91 CONTINUE                                                          OLM 1080
      NT2 = 0                                                           OLM 1090
      Y = HCO                                                           OLM 1100
      Y = RUND(Y)                                                       OLM 1110
      DO 92 N=1,NMAX                                                    OLM 1120
        X = PHI(N)                                                      OLM 1130
        X = RUND(X)                                                     OLM 1140
        IF(X .LT. 0.) GOTO 92                                           OLM 1150
        IF(X .GT. Y) GOTO 92                                            OLM 1160
        IF(NT2 .EQ. 0) NT2 = N                                          OLM 1170
        NT0 = N - 1                                                     OLM 1180
   92 CONTINUE                                                          OLM 1190
C                                                                       OLM 1200
C     OUTPUT VOLUMEN-MATRIX                                             OLM 1210
C                                                                       OLM 1220
      IF(ITIK(1) .GT. 1) GOTO 99                                        OLM 1230
      VOLVTS = 0.                                                       OLM 1240
      IF(INTVAL .LE. 1 .AND. IPRINT .EQ. 2) WRITE (6,300)               OLM 1250
      DO 98 K=1,K0                                                      OLM 1260
        VOLVTS = VOLVTS + VOLVT(K)                                      OLM 1270
        IF(INTVAL .LE. 1 .AND. IPRINT .EQ. 2) WRITE (6,93) K,IADVT(K,1),OLM 1280
     1   IADVT(K,2),VOLVT(K)                                            OLM 1290
   98 CONTINUE                                                          OLM 1300
      WRITE (6,94) VOLVTS,JT2,JT0,NT2,NT0                               OLM 1310
      GOTO 99                                                           OLM 1320
C                                                                       OLM 1330
CFZJ042                                                       09.09.05  OLM 1340
      ENTRY VOLMA1(KIND,POWL,QNW,N200,T,DU,POWPT,VOLPT,DOSPT,NDR,ID)    OLM 1350
C                                                                       OLM 1360
      K1 = 0                                                            OLM 1370
      DO 60 I=JT2,JT0                                                   OLM 1380
        DO 59 N=NT2,NT0                                                 OLM 1390
          K1 = K1 + 1                                                   OLM 1400
          VK = 0.                                                       OLM 1410
          QK = 0.                                                       OLM 1420
          DO 57 K=1,K0                                                  OLM 1430
            IF(IADVT(K,2) .NE. K1) GOTO 57                              OLM 1440
            NV = IADVT(K,1)                                             OLM 1450
            VK = VK + VOLVT(K)                                          OLM 1460
CFZJ042                                                       09.09.05  OLM 1470
            IF(KIND .EQ. 1 .OR. KIND .EQ. 5 .OR. KIND .EQ. 6 .OR. KIND  OLM 1480
     1       .EQ. 7) PO = POWL(NV)                                      OLM 1490
            IF(KIND .EQ. 2) PO = QNW(NV)                                OLM 1500
            QK = QK + VOLVT(K) * PO                                     OLM 1510
   57     CONTINUE                                                      OLM 1520
          QK = QK / VK                                                  OLM 1530
          IF(KIND .EQ. 1) T(I,N) = QK                                   OLM 1540
          IF(KIND .EQ. 2) DU(I,N) = QK                                  OLM 1550
          IF(KIND .EQ. 5) POWPT(I,N,ID) = QK                            OLM 1560
          IF(KIND .EQ. 6) VOLPT(I,N,ID) = QK                            OLM 1570
          IF(KIND .EQ. 7) DOSPT(I,N,ID) = QK                            OLM 1580
   59   CONTINUE                                                        OLM 1590
   60 CONTINUE                                                          OLM 1600
      GOTO 99                                                           OLM 1610
C                                                                       OLM 1620
CFZJ042                                                       09.09.05  OLM 1630
      ENTRY VOLMA2(TEML,N200,DU,TGRA,TFUL,KOM)                          OLM 1640
C                                                                       OLM 1650
      KNT = 0                                                           OLM 1660
      DO 67 K=1,K0                                                      OLM 1670
        IADV = IADVT(K,2)                                               OLM 1680
        KNT = MAX0(KNT,IADV)                                            OLM 1690
   67 CONTINUE                                                          OLM 1700
      KT = (JT0-JT2+1) * (NT0-NT2+1)                                    OLM 1710
      DO 70 L=1,LAYER                                                   OLM 1720
        VB = 0.                                                         OLM 1730
        VK = 0.                                                         OLM 1740
        TF = 0.                                                         OLM 1750
        TG = 0.                                                         OLM 1760
CFZJ045                                                       11.08.06  OLM 1770
        QK = 0.                                                         OLM 1780
        DO 69 K=1,K0                                                    OLM 1790
C                                                                       OLM 1800
C     VSOP-LAYER                                                        OLM 1810
C                                                                       OLM 1820
          IF(IADVT(K,1) .NE. L) GOTO 69                                 OLM 1830
C                                                                       OLM 1840
C     THERMIX-MESHES                                                    OLM 1850
C                                                                       OLM 1860
          NT = IADVT(K,2)                                               OLM 1870
          K1 = 0                                                        OLM 1880
          DO 68 I=JT2,JT0                                               OLM 1890
            DO 68 N=NT2,NT0                                             OLM 1900
              K1 = K1 + 1                                               OLM 1910
              IF(K1 .NE. NT) GOTO 68                                    OLM 1920
              VK = VK + VOLVT(K)                                        OLM 1930
              QK = QK + VOLVT(K) * DU(I,N)                              OLM 1940
              TG = TG + VOLVT(K) * 0.25 * (TGRA(I,N) + TGRA(I+1,N) +    OLM 1950
     1         TGRA(I,N+1) + TGRA(I+1,N+1))                             OLM 1960
CFZJ042                                                       09.09.05  OLM 1970
              BG = 0.                                                   OLM 1980
              TF1 = TFUL(I,N)                                           OLM 1990
              IF(TFUL(I,N) .GT. 0.) BG = BG + 1                         OLM 2000
              TF1 = TF1 + TFUL(I+1,N)                                   OLM 2010
              IF(TFUL(I+1,N) .GT. 0.) BG = BG + 1                       OLM 2020
              TF1 = TF1 + TFUL(I,N+1)                                   OLM 2030
              IF(TFUL(I,N+1) .GT. 0.) BG = BG + 1                       OLM 2040
              TF1 = TF1 + TFUL(I+1,N+1)                                 OLM 2050
              IF(TFUL(I+1,N+1) .GT. 0.) BG = BG + 1                     OLM 2060
              IF(BG .GT. 0.) TF = TF + TF1 * VOLVT(K) / BG              OLM 2070
   68     CONTINUE                                                      OLM 2080
   69   CONTINUE                                                        OLM 2090
        QK = QK / VK                                                    OLM 2100
        TEML(3,L) = QK / FF                                             OLM 2110
        TEML(1,L) = TG / VK                                             OLM 2120
CFZJ042                                                       09.09.05  OLM 2130
        TEML(2,L) = TF / VK                                             OLM 2140
   70 CONTINUE                                                          OLM 2150
   99 CONTINUE                                                          OLM 2160
      RETURN                                                            OLM 2170
      END                                                               OLM 2180
      FUNCTION RUND(X)                                                  RUN   10
C                                                                       RUN   20
C                                                                       RUN   30
      TAU = 100.                                                        RUN   40
      Z = X * TAU + 0.5                                                 RUN   50
      K = INT(Z)                                                        RUN   60
C                                                                       RUN   70
      RUND = FLOAT(K) / TAU                                             RUN   80
C                                                                       RUN   90
      RETURN                                                            RUN  100
      END                                                               RUN  110
      SUBROUTINE LAGRAS                                                 RAS   10
C                                                                       RAS   20
C     INTERPOLATION NACH LAGRANGE                                       RAS   30
C                                                                       RAS   40
      COMMON /FORALL/ IRAGRO,JINPO1,UU,VV,ZGROB(500),POGROB(500),IJ,    RAS   50
     1 FIM(100),IFZ,IWO,LLL(30),CASE(18),EXPO(15),IFZR,IREP,LLLL,LTAB,  RAS   60
     2 RADX(50),EPSIL,ZFEIN(60),MSPALT,NTYPEN                           RAS   70
C                                                                       RAS   80
CFZJ055                                                       25.09.07  RAS   90
C                                                                       RAS  100
CFZJ064                                                       15.06.11  RAS  110
      REAL*8 A1,A2,A3,A,B,C,D,B1,B2,B3,C1,C2,C3,D1,D2,D3,FX1,FX2,FX3,FX4RAS  120
C                                                                       RAS  130
      IF(ZGROB(IJ)-UU) 9,1,1                                            RAS  140
    1 CONTINUE                                                          RAS  150
      DO 2 II=1,IJ                                                      RAS  160
        IP = II                                                         RAS  170
        IF(ZGROB(IP)-UU) 2,3,4                                          RAS  180
    2 CONTINUE                                                          RAS  190
    3 VV = POGROB(IP)                                                   RAS  200
      RETURN                                                            RAS  210
    4 IP = IP - 1                                                       RAS  220
      IF(IP-1) 10,5,6                                                   RAS  230
    5 IP = IP + 1                                                       RAS  240
      GOTO 8                                                            RAS  250
    6 IF(IJ-IP-2) 7,8,8                                                 RAS  260
    7 IP = IP - 1                                                       RAS  270
C                                                                       RAS  280
C     FAKTOREN FUER DEN ZAEHLER                                         RAS  290
C                                                                       RAS  300
    8 A = UU - ZGROB(IP-1)                                              RAS  310
      B = UU - ZGROB(IP)                                                RAS  320
      C = UU - ZGROB(IP+1)                                              RAS  330
      D = UU - ZGROB(IP+2)                                              RAS  340
C                                                                       RAS  350
C     FAKTOREN FUER DEN NENNER                                          RAS  360
C                                                                       RAS  370
      A1 = ZGROB(IP-1) - ZGROB(IP)                                      RAS  380
      A2 = ZGROB(IP-1) - ZGROB(IP+1)                                    RAS  390
      A3 = ZGROB(IP-1) - ZGROB(IP+2)                                    RAS  400
      B1 = -A1                                                          RAS  410
      B2 = ZGROB(IP) - ZGROB(IP+1)                                      RAS  420
      B3 = ZGROB(IP) - ZGROB(IP+2)                                      RAS  430
      C1 = -A2                                                          RAS  440
      C2 = -B2                                                          RAS  450
      C3 = ZGROB(IP+1) - ZGROB(IP+2)                                    RAS  460
      D1 = -A3                                                          RAS  470
      D2 = -B3                                                          RAS  480
      D3 = -C3                                                          RAS  490
      FX1 = (B*C*D*POGROB(IP-1)) / (A1*A2*A3)                           RAS  500
      FX2 = (A*C*D*POGROB(IP)) / (B1*B2*B3)                             RAS  510
      FX3 = (A*B*D*POGROB(IP+1)) / (C1*C2*C3)                           RAS  520
      FX4 = (A*B*C*POGROB(IP+2)) / (D1*D2*D3)                           RAS  530
      VV = FX1 + FX2 + FX3 + FX4                                        RAS  540
      RETURN                                                            RAS  550
C                                                                       RAS  560
C     LINEARE EXTRAPOLATION                                             RAS  570
C                                                                       RAS  580
    9 IP = IJ                                                           RAS  590
      VV = POGROB(IP-1) + (POGROB(IP)-POGROB(IP-1)) / (ZGROB(IP)-       RAS  600
     1ZGROB(IP-1)) * (UU-ZGROB(IP-1))                                   RAS  610
      RETURN                                                            RAS  620
   10 VV = (POGROB(2)*(ZGROB(1)-UU)-(POGROB(1)*(ZGROB(2)-UU))) /        RAS  630
     1 (ZGROB(1)-ZGROB(2))                                              RAS  640
      RETURN                                                            RAS  650
      END                                                               RAS  660
      SUBROUTINE SETK1(DR,DPH,RAD,PHI,IKOM,NKOM,KOM,R,Z,ER,DZ,LOM,MFR,  ETK   10
     1 MFZ,ITK,KOC,KG)                                                  ETK   20
C                                                                       ETK   30
C     BELEGEN DES THERMIX/KONVEK-GITTERS MIT KOMPOSITIONEN              ETK   40
C                                                                       ETK   50
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    ETK   60
C                                                                       ETK   70
      COMMON /KOMP1/ KMAX                                               ETK   80
C                                                                       ETK   90
      COMMON /RZCOMP/ IREARZ,MCR,MCZ,ICONVC,RR,ZZ,III,NN,KK             ETK  100
C                                                                       ETK  110
      COMMON /SPECTI/ ITIK(10),DUM(18),NREST,IREST                      ETK  120
C                                                                       ETK  130
CFZJ055                                                       25.09.07  ETK  140
C                                                                       ETK  150
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             ETK  160
C                                                                       ETK  170
      DIMENSION DR(IMAZ),DPH(NMAZ),RAD(IMAZ+1),PHI(NMAZ+1),IKOM(KMAZ),  ETK  180
     1 NKOM(KMAZ),KOM(IMAZ,NMAZ),R(KONI),Z(KONN),ER(KONI),DZ(KONN),     ETK  190
     2 LOM(KONN,KONI),MFR(IMAZ),MFZ(NMAZ),KOC(IMAZ,NMAZ),A3(9),ITK(KMAZ)ETK  200
     3 ,KG(IMAZ,NMAZ)                                                   ETK  210
C                                                                       ETK  220
      CHARACTER*4 TTL(4)/' THE','RMIX','KONV','EK  '/                   ETK  230
C                                                                       ETK  240
      EQUIVALENCE(IREARZ,A3(1))                                         ETK  250
C                                                                       ETK  260
   80 FORMAT (1X,'**ERROR** IMAX OR NMAX EXCEEDED, OR MESH (I,N =',2I3, ETK  270
     1 ') NOT OCCUPIED BY ANY COMPOSITION')                             ETK  280
  300 FORMAT (24I3)                                                     ETK  290
  305 FORMAT (40I3)                                                     ETK  300
  310 FORMAT (//' PLACING ZONE COMPOSITIONS IN ',2A4/)                  ETK  310
C                                                                       ETK  320
C                                                                       ETK  330
      IF(NREST .GT. 1) GOTO 98                                          ETK  340
      DO 10 I=1,IMAX                                                    ETK  350
        DO 10 N=1,NMAX                                                  ETK  360
          KOM(I,N) = 0                                                  ETK  370
   10 CONTINUE                                                          ETK  380
      NMAX = NMAX - 1                                                   ETK  390
      IMAX = IMAX - 1                                                   ETK  400
C                                                                       ETK  410
C     KOMPOSITIONSBELEGUNG FUER THERMIX (GROB)                          ETK  420
C                                                                       ETK  430
      WRITE (6,310) (TTL(I),I=1,2)                                      ETK  440
      K2 = 0                                                            ETK  450
      DO 1 NZ=1,MCZ                                                     ETK  460
C                                                                       ETK  470
CARD TX13                                                               ETK  480
C                                                                       ETK  490
        READ (5,300) (KOC(NR,NZ),NR=1,MCR)                              ETK  500
C                                                                       ETK  510
        WRITE (6,305) (KOC(NR,NZ),NR=1,MCR)                             ETK  520
        K1 = K2 + 1                                                     ETK  530
        K2 = MFZ(NZ)                                                    ETK  540
        DO 1 N=K1,K2                                                    ETK  550
          J2 = 0                                                        ETK  560
C                                                                       ETK  570
C     KOMPOSITIONSBELEGUNG FUER THERMIX (FEIN)                          ETK  580
C                                                                       ETK  590
          DO 1 NR=1,MCR                                                 ETK  600
            J1 = J2 + 1                                                 ETK  610
            J2 = MFR(NR)                                                ETK  620
            K = KOC(NR,NZ)                                              ETK  630
            DO 1 I=J1,J2                                                ETK  640
              KOM(I,N) = K                                              ETK  650
    1 CONTINUE                                                          ETK  660
      DO 2 I=1,IMAX                                                     ETK  670
        DO 2 N=1,NMAX                                                   ETK  680
          K = KOM(I,N)                                                  ETK  690
          IKOM(K) = I                                                   ETK  700
          NKOM(K) = N                                                   ETK  710
    2 CONTINUE                                                          ETK  720
      DO 90 I=1,IMAX                                                    ETK  730
        DO 90 N=1,NMAX                                                  ETK  740
          IF(KOM(I,N) .GT. 0) GOTO 90                                   ETK  750
          WRITE (6,80) I,N                                              ETK  760
C                                                                       ETK  770
          CALL ABEND(3)                                                 ETK  780
C                                                                       ETK  790
   90 CONTINUE                                                          ETK  800
C                                                                       ETK  810
C     KOMP.-BELEGUNG FUER KONVEK (GROB)                                 ETK  820
C                                                                       ETK  830
      ITKMAX = 0                                                        ETK  840
      DO 50 N=1,MCZ                                                     ETK  850
        DO 50 I=1,MCR                                                   ETK  860
          K = KOC(I,N)                                                  ETK  870
          KG(I,N) = ITK(K)                                              ETK  880
          ITKMAX = MAX0(ITKMAX,ITK(K))                                  ETK  890
   50 CONTINUE                                                          ETK  900
      ITKMAX = ITKMAX + 1                                               ETK  910
      WRITE (6,310) (TTL(I),I=3,4)                                      ETK  920
      DO 60 N=1,MCZ                                                     ETK  930
        DO 70 I=1,MCR                                                   ETK  940
          IF(KG(I,N) .LE. 0) KG(I,N) = ITKMAX                           ETK  950
   70   CONTINUE                                                        ETK  960
        WRITE (6,305) (KG(I,N),I=1,MCR)                                 ETK  970
   60 CONTINUE                                                          ETK  980
      DO 30 I=1,IMAX                                                    ETK  990
        DO 30 N=1,NMAX                                                  ETK 1000
          K = KOM(I,N)                                                  ETK 1010
          KOC(I,N) = ITK(K)                                             ETK 1020
   30 CONTINUE                                                          ETK 1030
      DO 40 I=1,IMAX                                                    ETK 1040
        DO 40 N=1,NMAX                                                  ETK 1050
          IF(KOC(I,N) .EQ. 0) KOC(I,N) = ITKMAX                         ETK 1060
   40 CONTINUE                                                          ETK 1070
      IHOHL = 0                                                         ETK 1080
      NN = 0                                                            ETK 1090
      DO 5 N=1,NMAX                                                     ETK 1100
        NN = NN + 1                                                     ETK 1110
        DO 6 I=1,IMAX                                                   ETK 1120
          IF(KOC(I,N) .NE. 0) GOTO 7                                    ETK 1130
    6   CONTINUE                                                        ETK 1140
        NN = NN - 1                                                     ETK 1150
        GOTO 5                                                          ETK 1160
    7   CONTINUE                                                        ETK 1170
        IF(NN .GT. 1) GOTO 9                                            ETK 1180
        DZ(NN) = 0.                                                     ETK 1190
        ZZ = PHI(N)                                                     ETK 1200
        Z(NN) = ZZ                                                      ETK 1210
    9   CONTINUE                                                        ETK 1220
        J = 0                                                           ETK 1230
        DO 20 I=1,IMAX                                                  ETK 1240
          IF(KOC(I,N) .GE. 0) GOTO 20                                   ETK 1250
          J = J + 1                                                     ETK 1260
          IF(J .EQ. 1) IHOHL = IHOHL + 1                                ETK 1270
          KOMIN = KOC(I,N)                                              ETK 1280
          KOC(I,N) = IABS(KOMIN)                                        ETK 1290
   20   CONTINUE                                                        ETK 1300
        IF(J .EQ. 0) IHOHL = 0                                          ETK 1310
        IF(IHOHL .LE. 1) DZKON = 0.                                     ETK 1320
        DZKON = DZKON + DPH(N)                                          ETK 1330
        IF(IHOHL .GT. 1) NN = NN - 1                                    ETK 1340
        NH = IHOHL - 1                                                  ETK 1350
        NH = MAX0(0,NH)                                                 ETK 1360
        Z(NN+1) = PHI(N-NH) + DZKON / 2.                                ETK 1370
        DZ(NN+1) = DZKON                                                ETK 1380
        DO 8 I=1,IMAX                                                   ETK 1390
          KOC(I,NN) = KOC(I,N)                                          ETK 1400
    8   CONTINUE                                                        ETK 1410
    5 CONTINUE                                                          ETK 1420
      III = 0                                                           ETK 1430
      KK = 0                                                            ETK 1440
      DO 15 I=1,IMAX                                                    ETK 1450
        III = III + 1                                                   ETK 1460
        DO 16 N=1,NN                                                    ETK 1470
          IF(KOC(I,N) .GT. 0) GOTO 17                                   ETK 1480
   16   CONTINUE                                                        ETK 1490
        III = III - 1                                                   ETK 1500
        GOTO 15                                                         ETK 1510
   17   CONTINUE                                                        ETK 1520
        IF(III .GT. 1) GOTO 19                                          ETK 1530
        ER(III) = 0.                                                    ETK 1540
        RR = RAD(I)                                                     ETK 1550
        R(III) = RR                                                     ETK 1560
   19   CONTINUE                                                        ETK 1570
        R(III+1) = RAD(I) + DR(I) / 2.                                  ETK 1580
        ER(III+1) = DR(I)                                               ETK 1590
        DO 18 N=1,NN                                                    ETK 1600
          KO = KOC(I,N)                                                 ETK 1610
          KK = MAX0(KK,KO)                                              ETK 1620
          LOM(N+1,III+1) = KO                                           ETK 1630
   18   CONTINUE                                                        ETK 1640
   15 CONTINUE                                                          ETK 1650
      IMAX = IMAX + 1                                                   ETK 1660
      NMAX = NMAX + 1                                                   ETK 1670
      IF(NREST .EQ. 1) WRITE (IREST) IKOM,NKOM,KOM,A3,MFR,MFZ,R,Z,ER,DZ,ETK 1680
     1 LOM                                                              ETK 1690
   98 CONTINUE                                                          ETK 1700
      IF(NREST .GT. 1) READ (IREST) IKOM,NKOM,KOM,A3,MFR,MFZ,R,Z,ER,DZ, ETK 1710
     1 LOM                                                              ETK 1720
      RETURN                                                            ETK 1730
      END                                                               ETK 1740
      SUBROUTINE SETSTR(KOM,EPS,IDIR,ISO,ISU,NSL,NSR,KSTR)              ETS   10
C                                                                       ETS   20
C     IDENTIFIZIEREN UND PRUEFEN DER STRAHLENDEN SPALTE (MAX. 19)       ETS   30
C                                                                       ETS   40
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    ETS   50
C                                                                       ETS   60
      COMMON /KOMP1/ KMAX                                               ETS   70
C                                                                       ETS   80
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             ETS   90
C                                                                       ETS  100
      DIMENSION KOM(IMAZ,NMAZ),EPS(KMAZ),IDIR(KMAZ),ISO(NMAZ,19),       ETS  110
     1 ISU(NMAZ,19),NSL(IMAZ,19),NSR(IMAZ,19),KSTR(KMAZ)                ETS  120
C                                                                       ETS  130
  160 FORMAT (' **WARNING** CHECK DIRECTION OF RADIATION IN MESH (I,N) =ETS  140
     1 (',I2,',',I2,')')                                                ETS  150
  390 FORMAT (1X,'**ERROR** RADIATING WALLS OF COMPOSITION NO. ',I2,' ARETS  160
     1E NOT DEFINED')                                                   ETS  170
  391 FORMAT (1X,'**ERROR** COMP. ',I2,' IS USED SEVERAL TIMES IN RADIATETS  180
     1ION DIRECTION')                                                   ETS  190
  395 FORMAT (1X,'**ERROR** LIMITS OF RADIATION OF COMPOSITION NO. ',I2,ETS  200
     1 ' ARE INCORRECT')                                                ETS  210
C                                                                       ETS  220
C                                                                       ETS  230
      IFSTR = 0                                                         ETS  240
      DO 10 K=1,KMAX                                                    ETS  250
        KSTR(K) = 0                                                     ETS  260
        IF(EPS(K) .EQ. 0.) GOTO 10                                      ETS  270
        IFSTR = IFSTR + 1                                               ETS  280
        KSTR(K) = IFSTR                                                 ETS  290
   10 CONTINUE                                                          ETS  300
      IF(IFSTR .EQ. 0) RETURN                                           ETS  310
      DO 6 M=1,19                                                       ETS  320
        DO 5 N=1,NMAZ                                                   ETS  330
          ISO(N,M) = 0                                                  ETS  340
          ISU(N,M) = 0                                                  ETS  350
    5   CONTINUE                                                        ETS  360
        DO 6 N=1,IMAZ                                                   ETS  370
          NSL(N,M) = 0                                                  ETS  380
          NSR(N,M) = 0                                                  ETS  390
    6 CONTINUE                                                          ETS  400
      NM1 = NMAX - 1                                                    ETS  410
      IM1 = IMAX - 1                                                    ETS  420
      DO 300 I=1,IM1                                                    ETS  430
        DO 300 N=1,NM1                                                  ETS  440
          K1 = KOM(I,N)                                                 ETS  450
          K2 = K1                                                       ETS  460
          K3 = K1                                                       ETS  470
          IF(N .LT. NM1) K2 = KOM(I,N+1)                                ETS  480
          IF(I .LT. IM1) K3 = KOM(I+1,N)                                ETS  490
          KP1 = KSTR(K1)                                                ETS  500
          KP2 = KSTR(K2)                                                ETS  510
          KP3 = KSTR(K3)                                                ETS  520
          IF(KP1 .EQ. KP2) GOTO 400                                     ETS  530
          IF(KP1 .NE. 0) GOTO 110                                       ETS  540
          NSL(I,KP2) = N + 1                                            ETS  550
          GOTO 400                                                      ETS  560
  110     IF(KP2 .NE. 0) GOTO 120                                       ETS  570
          NSR(I,KP1) = N + 1                                            ETS  580
          GOTO 400                                                      ETS  590
  120     IDFEHL = 1                                                    ETS  600
          ID1 = IDIR(K1)                                                ETS  610
          ID2 = IDIR(K2)                                                ETS  620
          GOTO 100                                                      ETS  630
  400     IF(KP1 .EQ. KP3) GOTO 300                                     ETS  640
          IF(KP1 .NE. 0) GOTO 140                                       ETS  650
          ISO(N,KP3) = I + 1                                            ETS  660
          GOTO 300                                                      ETS  670
  140     IF(KP3 .NE. 0) GOTO 150                                       ETS  680
          ISU(N,KP1) = I + 1                                            ETS  690
          GOTO 300                                                      ETS  700
  150     IDFEHL = 0                                                    ETS  710
          ID1 = IDIR(K1)                                                ETS  720
          ID2 = IDIR(K3)                                                ETS  730
  100     IF(ID1 .NE. IDFEHL .AND. ID2 .NE. IDFEHL) GOTO 130            ETS  740
          WRITE (6,160) I,N                                             ETS  750
C                                                                       ETS  760
          CALL ABEND(2)                                                 ETS  770
C                                                                       ETS  780
          RETURN                                                        ETS  790
  130     IF(IDFEHL .EQ. 1) GOTO 400                                    ETS  800
  300 CONTINUE                                                          ETS  810
C                                                                       ETS  820
C     PRUEFEN DER STRAHLUNGSGRENZEN                                     ETS  830
C                                                                       ETS  840
      DO 310 K=1,KMAX                                                   ETS  850
        IF(EPS(K) .EQ. 0.) GOTO 310                                     ETS  860
        KS = KSTR(K)                                                    ETS  870
        IF(IDIR(K) .EQ. 1) GOTO 350                                     ETS  880
        DO 320 N=1,NM1                                                  ETS  890
          DO 325 I=1,IM1                                                ETS  900
            IF(KOM(I,N) .EQ. K) GOTO 326                                ETS  910
  325     CONTINUE                                                      ETS  920
          GOTO 320                                                      ETS  930
  326     CONTINUE                                                      ETS  940
          I1 = ISO(N,KS)                                                ETS  950
          I2 = ISU(N,KS)                                                ETS  960
          IF(I1 .NE. 0 .AND. I2 .NE. 0) GOTO 901                        ETS  970
          WRITE (6,390) K                                               ETS  980
C                                                                       ETS  990
          CALL ABEND(3)                                                 ETS 1000
C                                                                       ETS 1010
  901     IF(I .GE. I1 .AND. I .LT. I2) GOTO 902                        ETS 1020
          WRITE (6,391) K                                               ETS 1030
C                                                                       ETS 1040
          CALL ABEND(3)                                                 ETS 1050
C                                                                       ETS 1060
  902     IF(I1 .NE. I2 .AND. I1 .LE. IM1 .AND. I2 .LE. IM1) GOTO 320   ETS 1070
          WRITE (6,395) K                                               ETS 1080
C                                                                       ETS 1090
          CALL ABEND(3)                                                 ETS 1100
C                                                                       ETS 1110
  320   CONTINUE                                                        ETS 1120
        GOTO 310                                                        ETS 1130
  350   DO 330 I=1,IM1                                                  ETS 1140
          DO 425 N=1,NM1                                                ETS 1150
            IF(KOM(I,N) .EQ. K) GOTO 426                                ETS 1160
  425     CONTINUE                                                      ETS 1170
          GOTO 330                                                      ETS 1180
  426     CONTINUE                                                      ETS 1190
          N1 = NSL(I,KS)                                                ETS 1200
          N2 = NSR(I,KS)                                                ETS 1210
          IF(N1 .NE. 0 .AND. N2 .NE. 0) GOTO 907                        ETS 1220
          WRITE (6,390) K                                               ETS 1230
C                                                                       ETS 1240
          CALL ABEND(3)                                                 ETS 1250
C                                                                       ETS 1260
  907     IF(N .GE. N1 .AND. N .LT. N2) GOTO 908                        ETS 1270
          WRITE (6,391) K                                               ETS 1280
C                                                                       ETS 1290
          CALL ABEND(3)                                                 ETS 1300
C                                                                       ETS 1310
  908     IF(N1 .NE. N2 .AND. N1 .LE. NM1 .AND. N2 .LE. NM1) GOTO 330   ETS 1320
          WRITE (6,395) K                                               ETS 1330
C                                                                       ETS 1340
          CALL ABEND(3)                                                 ETS 1350
C                                                                       ETS 1360
  330   CONTINUE                                                        ETS 1370
  310 CONTINUE                                                          ETS 1380
      RETURN                                                            ETS 1390
      END                                                               ETS 1400
      SUBROUTINE SETT(NHET,WTGINT,NHZON,IFBH,KKB,T,WT,KOM,TVOR,RAD,PHI, ETT   10
     1 BU,BR,RE,PHE,WG,WS,FFTHX,TCOND,VOLPT,POWPT,DOSPT,THETNEW)        ETT   20
C                                                                       ETT   30
CFZJ042                                                       09.09.05  ETT   40
C                                                                       ETT   50
C     SETZEN DER ANFANGSTEMPERATUREN                                    ETT   60
C                                                                       ETT   70
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                ETT   80
C                                                                       ETT   90
      COMMON /RSTRT/ IFRSTA,ZRST,ILOGR                                  ETT  100
C                                                                       ETT  110
      COMMON /KOMP1/ KMAX                                               ETT  120
C                                                                       ETT  130
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    ETT  140
C                                                                       ETT  150
      COMMON /DYKOP1/ ANIN(21),NDYN,TMOD(10),TFUEL(10),DZDYN(10),PT(10),ETT  160
     1 ZDYN(10)                                                         ETT  170
C                                                                       ETT  180
      COMMON /SPECTI/ ITIK(10),DUM(18),NREST,IREST,TXNEW                ETT  190
C                                                                       ETT  200
      COMMON /STA/ IST,SB,TEX,JNS,RMI,RMA,TKI,TKA,TC                    ETT  210
C                                                                       ETT  220
CFZJ008 enlarged dimension common STAF                        28.11.03  ETT  230
      COMMON /STAF/ TF,TFC(300)                                         ETT  240
C                                                                       ETT  250
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             ETT  260
C                                                                       ETT  270
CFZJ042                                                       09.09.05  ETT  280
      COMMON /YEAR/ IYEAR                                               ETT  290
C                                                                       ETT  300
CFZJ042                                                       09.09.05  ETT  310
      COMMON /VRT/ MIXM                                                 ETT  320
C                                                                       ETT  330
      COMMON /IPRP/ IPR,MIXTH                                           ETT  340
C                                                                       ETT  350
CFZJ006 enlarged dimensions common QVAR                       28.11.03  ETT  360
      COMMON /QVAR/ DUMY(1211),TA(300),N61,URZ,ZLEKA,ABXEN,TI(300),DUMM,ETT  370
     1 TAUFH,JRESTW,JRESTR,JREST,TAU,D(11),ST(300)                      ETT  380
C                                                                       ETT  390
CFZJ024   NEW ARRAY TCOND                                     02.02.04  ETT  400
CFZJ026                                                       16.03.04  ETT  410
CFZJ042                                                       09.09.05  ETT  420
      DIMENSION NHZON(KMAZ),IFBH(ICO,NCO),KKB(ICO,NCO),T(IMAZ,NMAZ),    ETT  430
     1 WT(IMAZ,NMAZ),KOM(IMAZ,NMAZ),TVOR(KMAZ),RAD(IMAZ+1),PHI(NMAZ+1), ETT  440
     2 BU(IMAZ,NMAZ),BR(IMAZ,NMAZ),RE(IMAZ),PHE(NMAZ),WG(NMAZ),WS(NMAZ),ETT  450
     3 TCOND(IMAZ,NMAZ),FFTHX(IMAZ,NMAZ),IYEAR(1000),VOLPT(IMAZ,NMAZ,15)ETT  460
     4 ,POWPT(IMAZ,NMAZ,15),DOSPT(IMAZ,NMAZ,15),THETNEW(ICO,NCO,5,15)   ETT  470
C                                                                       ETT  480
   25 FORMAT (/25X,I5,5X,I2,5X,F5.1)                                    ETT  490
   26 FORMAT (///2X,80('*')/30X,'CALCULATION OF DECAY HEAT POWER STARTS ETT  500
     1AT')                                                              ETT  510
   30 FORMAT (4(E10.3,4I2,2X))                                          ETT  520
   36 FORMAT (30X,'TIME',I5,' H , ',I2,' MIN,',F5.1,' S'/2X,80('*')///) ETT  530
  225 FORMAT (13F6.1)                                                   ETT  540
  300 FORMAT (4(E20.13))                                                ETT  550
C                                                                       ETT  560
C                                                                       ETT  570
      IF(NREST .LE. 1) GOTO 1                                           ETT  580
      KREST = IREST                                                     ETT  590
CFZJ042                                                       09.09.05  ETT  600
      READ (KREST) ZRST,T,THETNEW,WTGINT,TEX,TC,TF,FFTHX,IYEAR,VOLPT,   ETT  610
     1 POWPT,DOSPT,MIXM,NHZ,TI(1),TA(1)                                 ETT  620
      REWIND KREST                                                      ETT  630
CFZJ042                                                       09.09.05  ETT  640
      DO 13 I=1,IMH                                                     ETT  650
        DO 13 N=1,NMH                                                   ETT  660
          DO 12 NZ=1,NHZ                                                ETT  670
            A = 0.                                                      ETT  680
            DO 11 ID=1,MIXM                                             ETT  690
              A = A + VOLPT(I+IDIFF,N+NDIFF,ID) * THETNEW(I,N,NZ,ID)    ETT  700
   11       CONTINUE                                                    ETT  710
            THETNEW(I,N,NZ,1) = A                                       ETT  720
   12     CONTINUE                                                      ETT  730
          VOLPT(I+IDIFF,N+NDIFF,1) = 1.                                 ETT  740
          POWPT(I+IDIFF,N+NDIFF,1) = 1.                                 ETT  750
   13 CONTINUE                                                          ETT  760
      MIXTH = 1                                                         ETT  770
      MIXM = 1                                                          ETT  780
      GOTO 99                                                           ETT  790
    1 CONTINUE                                                          ETT  800
      NM1 = NMAX - 1                                                    ETT  810
      IM1 = IMAX - 1                                                    ETT  820
      DO 6 K1=1,KMAX                                                    ETT  830
        DO 6 I=1,IM1                                                    ETT  840
          DO 6 N=1,NM1                                                  ETT  850
            K = KOM(I,N)                                                ETT  860
            IF(K1 .NE. K) GOTO 6                                        ETT  870
            T(I,N) = TVOR(K)                                            ETT  880
            T(I+1,N) = TVOR(K)                                          ETT  890
            T(I+1,N+1) = TVOR(K)                                        ETT  900
            T(I,N+1) = TVOR(K)                                          ETT  910
    6 CONTINUE                                                          ETT  920
      IF(NHET .EQ. 0) GOTO 200                                          ETT  930
      IMZ = IMH - 1                                                     ETT  940
      NMZ = NMH - 1                                                     ETT  950
      DO 5 K=1,KMAX                                                     ETT  960
        DO 5 I=1,IMZ                                                    ETT  970
          DO 5 N=1,NMZ                                                  ETT  980
            IF3 = IFBH(I,N)                                             ETT  990
            IF(IF3 .EQ. 0) GOTO 5                                       ETT 1000
            KK1 = KKB(I,N)                                              ETT 1010
            IF(KK1 .NE. K) GOTO 5                                       ETT 1020
            NZZ = NHZON(K)                                              ETT 1030
            DO 4 NZ=1,NZZ                                               ETT 1040
CFZJ042                                                       09.09.05  ETT 1050
              DO 14 ID=1,MIXM                                           ETT 1060
                THETNEW(I,N,NZ,ID) = TVOR(K)                            ETT 1070
                THETNEW(I,N+1,NZ,ID) = TVOR(K)                          ETT 1080
                THETNEW(I+1,N,NZ,ID) = TVOR(K)                          ETT 1090
                THETNEW(I+1,N+1,NZ,ID) = TVOR(K)                        ETT 1100
   14         CONTINUE                                                  ETT 1110
    4       CONTINUE                                                    ETT 1120
    5 CONTINUE                                                          ETT 1130
  200 CONTINUE                                                          ETT 1140
C                                                                       ETT 1150
CARDS TX15 - TX17                                                       ETT 1160
C                                                                       ETT 1170
      CALL SETZT1(IMAX,NMAX,RAD,PHI,BU,IE,NE,RE,PHE,BR,1,WT,WG,WS)      ETT 1180
C                                                                       ETT 1190
      DO 10 I=1,IM1                                                     ETT 1200
        DO 10 N=2,NM1                                                   ETT 1210
          IF(T(I,N) .NE. 0. .AND. IE .GT. 0) GOTO 10                    ETT 1220
          T(I,N) = BU(I,N)                                              ETT 1230
   10 CONTINUE                                                          ETT 1240
   99 CONTINUE                                                          ETT 1250
CFZJ024                                                       02.02.04  ETT 1260
      DO 100 N=1,NMAX                                                   ETT 1270
      DO 100 I=1,IMAX                                                   ETT 1280
        TCOND(I,N) = T(I,N)                                             ETT 1290
  100 CONTINUE                                                          ETT 1300
      RETURN                                                            ETT 1310
      END                                                               ETT 1320
      SUBROUTINE SETZT1(IMAX,NMAX,RAD,PHI,T,IE,NE,RE,PHE,WQ,IFRD,WQR,WG,TZT   10
     1 WS)                                                              TZT   20
C                                                                       TZT   30
C     UEBERTRAGEN DER MIT HILFSGITTER EINGELESEN ANFANGSTEMPERATUREN    TZT   40
C                                                                       TZT   50
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             TZT   60
C                                                                       TZT   70
      DIMENSION T(IMAZ,NMAZ),WQ(IMAZ,NMAZ),WQR(IMAZ,NMAZ),WG(NMAZ),     TZT   80
     1 WS(NMAZ),RAD(IMAZ+1),PHI(NMAZ+1),RE(1),PHE(1)                    TZT   90
C                                                                       TZT  100
   11 FORMAT (2I5,6E10.3)                                               TZT  110
   12 FORMAT (/////10X,'INPUT OF PROFILE OF INITIAL TEMPERATURES'/)     TZT  120
   25 FORMAT (7E10.3)                                                   TZT  130
 3002 FORMAT (200E12.5)                                                 TZT  140
 3003 FORMAT (12X,200E12.5)                                             TZT  150
C                                                                       TZT  160
C                                                                       TZT  170
      IF(IFRD .EQ. 0) GOTO 21                                           TZT  180
C                                                                       TZT  190
CARD TX15                                                               TZT  200
C                                                                       TZT  210
      READ (5,11) IPOLI,IE,(RE(I),I=1,6)                                TZT  220
C                                                                       TZT  230
CFZJ021                                                       28.01.04  TZT  240
      DO 7 I=1,IMAX                                                     TZT  250
        DO 7 N=1,NMAX                                                   TZT  260
          T(I,N) = 0.                                                   TZT  270
          WQ(I,N) = 0.                                                  TZT  280
          WQR(I,N) = 0.                                                 TZT  290
    7 CONTINUE                                                          TZT  300
      IF(IE .LE. 0) GOTO 90                                             TZT  310
C                                                                       TZT  320
CARD TX15  CONTINUATION                                                 TZT  330
C                                                                       TZT  340
      IF(IE .GT. 6) READ (5,25) (RE(I),I=7,IE)                          TZT  350
C                                                                       TZT  360
CARD TX16                                                               TZT  370
C                                                                       TZT  380
      READ (5,11) IPOLN,NE,(PHE(N),N=1,6 )                              TZT  390
C                                                                       TZT  400
CARD TX16  CONTINUATION                                                 TZT  410
C                                                                       TZT  420
      IF(NE .GT. 6) READ(5,25) (PHE(N),N=7,NE)                          TZT  430
C                                                                       TZT  440
      DO 20 N=1,NE                                                      TZT  450
C                                                                       TZT  460
CARD TX17                                                               TZT  470
C                                                                       TZT  480
        READ (5,25) (WQ(I,N),I=1,IE)                                    TZT  490
C                                                                       TZT  500
   20 CONTINUE                                                          TZT  510
      WRITE (6,12)                                                      TZT  520
C                                                                       TZT  530
      CALL PRTEIN(IE,NE,PHE,RE,WQ)                                      TZT  540
C                                                                       TZT  550
   21 CONTINUE                                                          TZT  560
      DO 30 NGE=1,NE                                                    TZT  570
        DO 40 I=1,IE                                                    TZT  580
          WG(I) = WQ(I,NGE)                                             TZT  590
   40   CONTINUE                                                        TZT  600
C                                                                       TZT  610
        CALL REIPO(IE,RE,WG,IMAX,RAD,WS,IPOLI)                          TZT  620
C                                                                       TZT  630
        DO 50 I=1,IMAX                                                  TZT  640
          WQR(I,NGE) = WS(I)                                            TZT  650
   50   CONTINUE                                                        TZT  660
   30 CONTINUE                                                          TZT  670
      DO 60 I=1,IMAX                                                    TZT  680
        DO 70 N=1,NE                                                    TZT  690
          WG(N) = WQR(I,N)                                              TZT  700
   70   CONTINUE                                                        TZT  710
C                                                                       TZT  720
        CALL REIPO(NE,PHE,WG,NMAX,PHI,WS,IPOLN)                         TZT  730
C                                                                       TZT  740
        DO 80 N=1,NMAX                                                  TZT  750
          T(I,N) = WS(N)                                                TZT  760
   80   CONTINUE                                                        TZT  770
   60 CONTINUE                                                          TZT  780
      GOTO 91                                                           TZT  790
CFZJ021                                                       28.01.04  TZT  800
   90 CONTINUE                                                          TZT  810
      OPEN (59,FILE='tempstat')                                         TZT  820
      REWIND 59                                                         TZT  830
      READ (59,3003) (RAD(I),I=1,IMAX)                                  TZT  840
      DO 3004 N=1,NMAX                                                  TZT  850
        READ (59,3002) PHI(N),(T(I,N),I=1,IMAX)                         TZT  860
 3004 CONTINUE                                                          TZT  870
   91 CONTINUE                                                          TZT  880
      RETURN                                                            TZT  890
      END                                                               TZT  900