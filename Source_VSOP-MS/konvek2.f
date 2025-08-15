      SUBROUTINE EXTREM(FELD,J,IG,IFB,R,Z,FZQ,DZ)                       XTR   10
C                                                                       XTR   20
C     BERECHNET MITTELWERTE UND EXTREMWERTE FUER KURZINFO               XTR   30
C                                                                       XTR   40
      COMMON /EXTX/ WMAX(5),WMIN(5),WMIT(5),RWMAX(5),RWMIN(5),ZWMAX(5), XTR   50
     1 ZWMIN(5)                                                         XTR   60
C                                                                       XTR   70
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      XTR   80
C                                                                       XTR   90
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             XTR  100
C                                                                       XTR  110
      DIMENSION FELD(KONN,KONI),IFB(KONN,KONI),R(KONI),Z(KONN),FZQ(KONI)XTR  120
     1 ,DZ(KONN)                                                        XTR  130
C                                                                       XTR  140
C                                                                       XTR  150
C     IG=1 FUER STROMFUNKTION, J=ZAEHLER                                XTR  160
C                                                                       XTR  170
      I1 = 2                                                            XTR  180
      N1 = 2                                                            XTR  190
      I2 = IM1                                                          XTR  200
      N2 = NM1                                                          XTR  210
      IF(IG .NE. 1) GOTO 10                                             XTR  220
      I1 = 1                                                            XTR  230
      N1 = 1                                                            XTR  240
      I2 = I2 + 1                                                       XTR  250
      N2 = N2 + 1                                                       XTR  260
   10 CONTINUE                                                          XTR  270
      VOL = 0.                                                          XTR  280
      FMIT = 0.                                                         XTR  290
      FMAX = -1.E20                                                     XTR  300
      FMIN = 1.E20                                                      XTR  310
      DO 20 I=I1,I2                                                     XTR  320
        DO 20 N=N1,N2                                                   XTR  330
          IF(IFB(I,N) .EQ. 0 .AND. IG .NE. 1) GOTO 20                   XTR  340
          IF(FMAX .GT. FELD(I,N)) GOTO 30                               XTR  350
          FMAX = FELD(I,N)                                              XTR  360
          RWMAX(J) = R(N) * 100.                                        XTR  370
          ZWMAX(J) = Z(I) * 100.                                        XTR  380
   30     IF(FMIN .LT. FELD(I,N)) GOTO 40                               XTR  390
          FMIN = FELD(I,N)                                              XTR  400
          RWMIN(J) = R(N) * 100.                                        XTR  410
          ZWMIN(J) = Z(I) * 100.                                        XTR  420
   40     CONTINUE                                                      XTR  430
          DVOL = FZQ(N) * DZ(I)                                         XTR  440
          FMIT = FMIT + FELD(I,N) * DVOL                                XTR  450
          VOL = VOL + DVOL                                              XTR  460
   20 CONTINUE                                                          XTR  470
      WMIT(J) = FMIT / VOL                                              XTR  480
      WMAX(J) = FMAX                                                    XTR  490
      WMIN(J) = FMIN                                                    XTR  500
      RETURN                                                            XTR  510
      END                                                               XTR  520
      SUBROUTINE GASKON(RHO,IFB,DR,DZ,MZ,MR,XKR,XKZ,STROM,FZQ1,FRQ1,    GAS   10
     1 LAMTUR,DROHR,QTV,LTV,XGEO,T,TFL,FZQ,KOM,ALPHA,ALFISO,XVER,EPSIL, GAS   20
     2 DHYD,FRQ,XKON,ALGA)                                              GAS   30
C                                                                       GAS   40
C     BERECHNET DIE KONSTANTEN FUER DIE GASTEMPERATURBERECHNUNG         GAS   50
C                                                                       GAS   60
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      GAS   70
C                                                                       GAS   80
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             GAS   90
C                                                                       GAS  100
      DIMENSION RHO(KONN,KONI),IFB(KONN,KONI),DR(KONI),DZ(KONN),        GAS  110
     1 FZQ1(KONI),XKR(KONN,KONI),XKZ(KONN,KONI),STROM(KONN,KONI),       GAS  120
     2 DROHR(KOMAX),FRQ1(KONN,KONI),QTV(KOMAX),XGEO(KOMAX),T(KONN,KONI),GAS  130
     3 TFL(KONN,KONI),FZQ(KONI),KOM(KONN,KONI),ALPHA(KOMAX),            GAS  140
     4 ALFISO(KOMAX),XVER(KONI,30),EPSIL(KOMAX),DHYD(KOMAX),            GAS  150
     5 FRQ(KONN,KONI),XKON(KOMAX),ALGA(KONN,KONI,2)                     GAS  160
C                                                                       GAS  170
      REAL MZ(KONN,KONI),MR(KONN,KONI),LAMTUR(KONN,KONI),LTV(KOMAX)     GAS  180
C                                                                       GAS  190
C                                                                       GAS  200
C     RHO ENTSPRICHT HIER ALPHA*F                                       GAS  210
C                                                                       GAS  220
      IM = IM1 + 1                                                      GAS  230
      NM = NM1 + 1                                                      GAS  240
C                                                                       GAS  250
      CALL ALPHAK(DROHR,QTV,LTV,XGEO,T,TFL,IFB,FZQ,DZ,KOM,ALPHA,ALFISO, GAS  260
     1 XVER,STROM,EPSIL,DHYD,RHO,FRQ,MZ,MR)                             GAS  270
C                                                                       GAS  280
      CALL XLTURB(LAMTUR,TFL,RHO,IFB,KOM,XKON,FZQ,FRQ,MZ,MR)            GAS  290
C                                                                       GAS  300
      DO 101 I=1,IM                                                     GAS  310
        DO 101 N=1,NM                                                   GAS  320
          RHO(I,N) = STROM(I,N)                                         GAS  330
  101 CONTINUE                                                          GAS  340
C                                                                       GAS  350
      CALL ALGAS(ALGA,RHO,MZ,MR)                                        GAS  360
C                                                                       GAS  370
      DO 100 I=1,IM                                                     GAS  380
        DO 100 N=1,NM                                                   GAS  390
          IFB1 = IFB(I,N)                                               GAS  400
          IF(IFB1 .EQ. 0) GOTO 90                                       GAS  410
          GOTO 99                                                       GAS  420
   90     CONTINUE                                                      GAS  430
          XKR(I,N) = 0.                                                 GAS  440
          XKZ(I,N) = 0.                                                 GAS  450
          RHO(I,N) = 0.                                                 GAS  460
          GOTO 100                                                      GAS  470
   99     CONTINUE                                                      GAS  480
          XKR(I,N) = MR(I,N) * CP / 2.                                  GAS  490
          XKZ(I,N) = MZ(I,N) * CP / 2.                                  GAS  500
  100 CONTINUE                                                          GAS  510
      DO 105 I=1,IM1                                                    GAS  520
        DO 105 N=1,NM1                                                  GAS  530
C                                                                       GAS  540
          IFB1 = IFBG(I,N,IFB)                                          GAS  550
C                                                                       GAS  560
          IF(IFB1 .EQ. 0) GOTO 91                                       GAS  570
          GOTO 991                                                      GAS  580
   91     CONTINUE                                                      GAS  590
          MR(I,N) = 0.                                                  GAS  600
          MZ(I,N) = 0.                                                  GAS  610
          RHO(I,N) = 0.                                                 GAS  620
          GOTO 105                                                      GAS  630
  991     CONTINUE                                                      GAS  640
          XLH = LAMTUR(I,N)                                             GAS  650
          XLHN1 = LAMTUR(I,N+1)                                         GAS  660
          XLHI1 = LAMTUR(I+1,N)                                         GAS  670
          MR(I,N) = 0.                                                  GAS  680
          IF(XLH .LE. 0. .AND. XLHI1 .LE. 0.) GOTO 151                  GAS  690
          MR(I,N) = 2. * DR(N) / (FRQ1(I,N)*(XLH+XLHI1))                GAS  700
          MR(I,N) = 1. / MR(I,N)                                        GAS  710
  151     CONTINUE                                                      GAS  720
          MZ(I,N) = 0.                                                  GAS  730
          IF(XLH .LE. 0. .AND. XLHN1 .LE. 0.) GOTO 105                  GAS  740
          MZ(I,N) = 2. * DZ(I) / (FZQ1(N)*(XLH+XLHN1))                  GAS  750
          MZ(I,N) = 1. / MZ(I,N)                                        GAS  760
          IF(I .EQ. 1 .OR. I .EQ. IM1) MR(I,N) = MR(I,N) * 2.           GAS  770
          IF(N .EQ. 1 .OR. N .EQ. NM1) MZ(I,N) = MZ(I,N) * 2.           GAS  780
  105 CONTINUE                                                          GAS  790
      RETURN                                                            GAS  800
      END                                                               GAS  810
      SUBROUTINE GASTEM(IFGO,IFZW,FELD,TFL,IFB,FZQ,DZ,KOM,STROM,STZUK,  AST   10
     1 TFLVOR,RHO,DR,MZ,MR,XKR,XKZ,FZQ1,FRQ1,LAMTUR,DROHR,QTV,LTV,XGEO,TAST   20
     2 ,ALPHA,ALFISO,XVER,EPSIL,DHYD,FRQ,XKON,ALGA,XZSUM,XNSUM1,P,IFBR, AST   30
     3 VOL,STRBEZ,IFQROW)                                               AST   40
C                                                                       AST   50
C     STEUERT DIE ITERATIVE BERECHNUNG DER GASTEMPERATUREN              AST   60
C                                                                       AST   70
      COMMON /ITPARM/ IFGO1,IFGO3,IFGO4,OVLOOP                          AST   80
C                                                                       AST   90
      COMMON /TABB/ EPSI4                                               AST  100
C                                                                       AST  110
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      AST  120
C                                                                       AST  130
      COMMON /ITER/ IB,IT,IT1,IT2,ITM1,ITM2,EPSI1,EPSI2,IFSQ1,OVM1,OVM2 AST  140
C                                                                       AST  150
      COMMON /ITE1/ OVREL,PREL,PMR,IME,NME,ZRG,NKORR,GASREL,DGASM,IFGG  AST  160
C                                                                       AST  170
CFZJ055                                                       25.09.07  AST  180
C                                                                       AST  190
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             AST  200
C                                                                       AST  210
      DIMENSION FELD(KONN,KONI),TFL(KONN,KONI),IFB(KONN,KONI),FZQ(KONI),AST  220
     1 DZ(KONN),KOM(KONN,KONI),STROM(KONN,KONI),STZUK(KOMAX),           AST  230
     2 TFLVOR(KOMAX),RHO(KONN,KONI),DR(KONI),XKR(KONN,KONI),            AST  240
     3 XKZ(KONN,KONI),FZQ1(KONI),FRQ1(KONN,KONI),DROHR(KOMAX),QTV(KOMAX)AST  250
     4 ,XGEO(KOMAX),T(KONN,KONI),ALPHA(KOMAX),ALFISO(KOMAX),            AST  260
     5 XVER(KONI,30),EPSIL(KOMAX),DHYD(KOMAX),FRQ(KONN,KONI),XKON(KOMAX)AST  270
     6 ,ALGA(KONN,KONI,2),XZSUM(KOMAX),XNSUM1(KOMAX),P(KONN,KONI),      AST  280
     7 IFBR(KOMAX),VOL(KOMAX),STRBEZ(KONN),IFQROW(KONN),QUEL(4),TQUE(4),AST  290
     8 PB(4),KQ(4),VQ(4)                                                AST  300
C                                                                       AST  310
      REAL MZ(KONN,KONI),MR(KONN,KONI),LAMTUR(KONN,KONI),LTV(KOMAX),    AST  320
     1 MB(4)                                                            AST  330
C                                                                       AST  340
      DATA IFMIN/0/                                                     AST  350
C                                                                       AST  360
C                                                                       AST  370
      IFGO3 = 0                                                         AST  380
      IF(OVLOOP .EQ. 0.) OVLOOP = 1.                                    AST  390
      IF(OVLOOP .LT. 1. .AND. IFMIN .NE. 1) OVLOOP = OVLOOP + 0.025 *   AST  400
     1 IFZW                                                             AST  410
      IF(OVLOOP .GT. 1. .OR. IB .EQ. 1) OVLOOP = 1.                     AST  420
      IFGO4 = 0                                                         AST  430
      OVREL = 1.                                                        AST  440
      IFQUE = 1                                                         AST  450
      IT = 100                                                          AST  460
      IFGG = 0                                                          AST  470
      IM = IM1 + 1                                                      AST  480
      NM = NM1 + 1                                                      AST  490
C                                                                       AST  500
      CALL GASKON(RHO,IFB,DR,DZ,MZ,MR,XKR,XKZ,STROM,FZQ1,FRQ1,LAMTUR,   AST  510
     1 DROHR,QTV,LTV,XGEO,T,TFL,FZQ,KOM,ALPHA,ALFISO,XVER,EPSIL,DHYD,FRQAST  520
     2 ,XKON,ALGA)                                                      AST  530
C                                                                       AST  540
      DO 10 I=1,IM1                                                     AST  550
        DO 10 N=1,NM1                                                   AST  560
          IF(IFB(I,N) .EQ. 0) GOTO 9                                    AST  570
          KK = KOM(I,N)                                                 AST  580
          QUE1 = STZUK(KK)                                              AST  590
          IF(QUE1 .LE. 0.) GOTO 9                                       AST  600
          IFB(I,N) = IFB(I,N) + 20                                      AST  610
    9     STROM(I,N) = TFL(I,N)                                         AST  620
   10 CONTINUE                                                          AST  630
C                                                                       AST  640
      CALL WATCH(ENDE)                                                  AST  650
C                                                                       AST  660
      START = ENDE                                                      AST  670
      IT1 = 0                                                           AST  680
  201 IF(IFQUE .EQ. 0) GOTO 200                                         AST  690
      IFQUE = 0                                                         AST  700
      DO 19 I=1,IM                                                      AST  710
        DO 19 N=1,NM                                                    AST  720
          FELD(I,N) = 0.                                                AST  730
   19 CONTINUE                                                          AST  740
  200 CONTINUE                                                          AST  750
      IT1 = IT1 + 1                                                     AST  760
      PREL = 0.                                                         AST  770
      DO 100 I=1,IM1                                                    AST  780
        DO 100 N=1,NM1                                                  AST  790
C                                                                       AST  800
          IFB1 = IFBG(I,N,IFB)                                          AST  810
C                                                                       AST  820
          IF(IFB1 .EQ. 0) GOTO 100                                      AST  830
          PP = TFL(I,N)                                                 AST  840
          KQ(1) = KOM(I,N)                                              AST  850
          KQ(2) = KOM(I+1,N)                                            AST  860
          KQ(3) = KOM(I,N+1)                                            AST  870
          KQ(4) = KOM(I+1,N+1)                                          AST  880
          VQ(1) = DZ(I) * FZQ(N)                                        AST  890
          VQ(2) = DZ(I+1) * FZQ(N)                                      AST  900
          VQ(3) = DZ(I) * FZQ(N+1)                                      AST  910
          VQ(4) = DZ(I+1) * FZQ(N+1)                                    AST  920
          DO 300 L=1,4                                                  AST  930
            QUEL(L) = 0.                                                AST  940
            TQUE(L) = 0.                                                AST  950
C                                                                       AST  960
            IF(IFBG(I,N,IFB) .LT. 20) GOTO 218                          AST  970
C                                                                       AST  980
            KK = KQ(L)                                                  AST  990
            VV = VQ(L)                                                  AST 1000
            IF(KK .EQ. 0) GOTO 218                                      AST 1010
            QUEL(L) = STZUK(KK) * VV * CP                               AST 1020
            TQUE(L) = TFLVOR(KK)                                        AST 1030
  218       CONTINUE                                                    AST 1040
  300     CONTINUE                                                      AST 1050
C                                                                       AST 1060
          CALL ELEM4(I,N,PB,MB,PP,QUEL,TQUE,FELD,IFQUE,ALGA,T,TFL,RHO,MZAST 1070
     1     ,MR,XKR,XKZ)                                                 AST 1080
C                                                                       AST 1090
          IF(PP .EQ. 0.) GOTO 217                                       AST 1100
          PRELB = ABS(TFL(I,N)/PP-1.)                                   AST 1110
  217     CONTINUE                                                      AST 1120
          TFL(I,N) = PP                                                 AST 1130
          IF(PRELB .LT. PREL) GOTO 770                                  AST 1140
          PREL = PRELB                                                  AST 1150
  770     CONTINUE                                                      AST 1160
  100 CONTINUE                                                          AST 1170
      IF((PREL .GT. EPSI1 .OR. IFGO3 .EQ. 1) .AND. IT1 .LT. ITM1) GOTO  AST 1180
     1 201                                                              AST 1190
      IF(IFQUE .EQ. 1) GOTO 24                                          AST 1200
      IFQUE = 1                                                         AST 1210
      GOTO 200                                                          AST 1220
   24 IFGO = 1                                                          AST 1230
      GRELA = GASREL                                                    AST 1240
      GASREL = 0.                                                       AST 1250
      DGASM = 0.                                                        AST 1260
      IZZ = 0                                                           AST 1270
C                                                                       AST 1280
      CALL WATCH(ENDE)                                                  AST 1290
C                                                                       AST 1300
      ZRG = ENDE-START                                                  AST 1310
      DO 20 I=1,IM1                                                     AST 1320
        DO 20 N=1,NM1                                                   AST 1330
C                                                                       AST 1340
          IF(IFBG(I,N,IFB) .EQ. 0) GOTO 20                              AST 1350
C                                                                       AST 1360
          IF(IFB(I,N) .LT. 10) GOTO 25                                  AST 1370
          IFB(I,N) = IFB(I,N) - 20                                      AST 1380
   25     CONTINUE                                                      AST 1390
          IF(TFL(I,N) .EQ. 0.) GOTO 20                                  AST 1400
          GASR = ABS(STROM(I,N)/TFL(I,N)-1.)                            AST 1410
          IZZ = IZZ + 1                                                 AST 1420
          DGASM = DGASM + GASR                                          AST 1430
          IF(GASR .LT. GASREL) GOTO 20                                  AST 1440
          IME = I                                                       AST 1450
          NME = N                                                       AST 1460
          PMR = TFL(I,N)                                                AST 1470
          GASREL = GASR                                                 AST 1480
   20 CONTINUE                                                          AST 1490
      DGASM = DGASM / IZZ                                               AST 1500
      IF(DGASM .LT. EPSI4 .AND. IFGO4 .EQ. 0) IFGO = 0                  AST 1510
      DGASM = DGASM * 100.                                              AST 1520
      GASREL = GASREL * 100.                                            AST 1530
      IF(IT1 .GE. ITM1) IFGG = 1                                        AST 1540
      IFMIN = 0                                                         AST 1550
      IF(IFZW .EQ. 0) RETURN                                            AST 1560
      IF(GRELA .EQ. 0.) RETURN                                          AST 1570
      IF(GASREL .LT. GRELA) RETURN                                      AST 1580
      OVLOOP = OVLOOP - 0.2 * IFZW                                      AST 1590
      IF(IB .EQ. 1) OVLOOP = 1.                                         AST 1600
      IFMIN = 1                                                         AST 1610
      IF(OVLOOP .LT. 0.5) OVLOOP = 0.5                                  AST 1620
      DO 500 I=1,IM1                                                    AST 1630
        DO 500 N=1,NM1                                                  AST 1640
          TFL(I,N) = STROM(I,N) - (STROM(I,N)-TFL(I,N)) * OVLOOP        AST 1650
  500 CONTINUE                                                          AST 1660
      EP4 = 100. * EPSI4                                                AST 1670
      IF(IFGO .EQ. 0 .AND. GASREL .LT. EP4) RETURN                      AST 1680
      IFGO = 1                                                          AST 1690
      RETURN                                                            AST 1700
      END                                                               AST 1710
      FUNCTION NUSL1(RE,PR,DL)                                          NUS   10
C                                                                       NUS   20
C     NUSSELTZAHL FUER LAMINARE ROHRSTROEMUNG IN LANGEN ROHREN          NUS   30
C     (VDI-GB).DL ---> DHYD / GESAMTLAENGE (KLEINER/GLEICH 0.1)         NUS   40
C                                                                       NUS   50
      REAL NUSL1,NUS                                                    NUS   60
C                                                                       NUS   70
C                                                                       NUS   80
      A = 3.66**3.                                                      NUS   90
      B = 1.61**3.                                                      NUS  100
      NUS = A + B * RE * PR * DL                                        NUS  110
C                                                                       NUS  120
      NUSL1 = NUS**0.3333                                               NUS  130
C                                                                       NUS  140
      RETURN                                                            NUS  150
      END                                                               NUS  160
      FUNCTION NUSL2(RE,PR,DL)                                          USL   10
C                                                                       USL   20
C     NUSSELTZAHL FUER LAMINARE ROHRSTROEMUNG IN KURZEN ROHREN          USL   30
C     (VDI/GB). DL ---> DHYD / GESAMTLAENGE  (GROESSER 0.1)             USL   40
C                                                                       USL   50
      REAL NUSL2,NU1,NU2                                                USL   60
C                                                                       USL   70
C                                                                       USL   80
      NU1 = PR**0.3333                                                  USL   90
      NU2 = RE * DL                                                     USL  100
      NU2 = NU2**0.5                                                    USL  110
C                                                                       USL  120
      NUSL2 = 0.664 * NU1 * NU2                                         USL  130
C                                                                       USL  140
      RETURN                                                            USL  150
      END                                                               USL  160
      FUNCTION NUST(RE,PR,DL)                                           UST   10
C                                                                       UST   20
C     NUSSELTZAHL FUER TURBULENTE ROHRSTROEMUNG (VDI-GB)                UST   30
C     DL BEDEUTET DHYD / GESAMTLAENGE                                   UST   40
C                                                                       UST   50
      REAL NUST,NU1,NU2,NU3                                             UST   60
C                                                                       UST   70
C                                                                       UST   80
      NU1 = RE**0.8                                                     UST   90
      NU2 = PR**0.4                                                     UST  100
      NU3 = DL**0.6667                                                  UST  110
C                                                                       UST  120
      NUST = 0.0214 * (NU1-100.) * NU2 * (1.+NU3)                       UST  130
C                                                                       UST  140
      IF(NUST .LT. 3.667) NUST = 3.667                                  UST  150
C                                                                       UST  160
      RETURN                                                            UST  170
      END                                                               UST  180
      SUBROUTINE PRIN(FELD,IPRINT,IFPPP,EPSIL,ROGG,MZ,MR,P,KOM,STROM,T, PRI   10
     1 TFL,RHO,IFB,DHYD,FZQ,FRQ,R,Z,DZ,ZP,RP)                           PRI   20
C                                                                       PRI   30
C     STEUERT DIE AUSGABE DER ERGEBNISSE DER STROEMUNGSRECHNUNG         PRI   40
C                                                                       PRI   50
      COMMON /SCHR/ IF0                                                 PRI   60
C                                                                       PRI   70
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      PRI   80
C                                                                       PRI   90
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             PRI  100
C                                                                       PRI  110
      DIMENSION FELD(KONN,KONI),EPSIL(KOMAX),ROGG(KONN,KONI),           PRI  120
     1 P(KONN,KONI),KOM(KONN,KONI),STROM(KONN,KONI),T(KONN,KONI),       PRI  130
     2 TFL(KONN,KONI),RHO(KONN,KONI),IFB(KONN,KONI),DHYD(KOMAX),        PRI  140
     3 FZQ(KONI),FRQ(KONN,KONI),R(KONI),Z(KONN),DZ(KONN),ZP(KONN),      PRI  150
     4 RP(KONI)                                                         PRI  160
C                                                                       PRI  170
      REAL MZ(KONN,KONI),MR(KONN,KONI)                                  PRI  180
C                                                                       PRI  190
      CHARACTER*4 U1(4)/'REYN','OLDS','    ','    '/,U2(4)/'MASS',' FLO'PRI  200
     1 ,'W   ',' MR '/,U3(4)/'MASS',' FLO','W   ',' MZ '/,U4(4)/'VELO', PRI  210
     2 'CITY','    ','    '/,U5(4)/'REL.',' COR','R. P','RESS'/,U6(4)/  PRI  220
     3 'STRE','AMIN','G FU','NCT.'/,U7(4)/'PRES','SURE',' (+S','TAT)'/  PRI  230
C                                                                       PRI  240
C                                                                       PRI  250
      IM = IM1 + 1                                                      PRI  260
      NM = NM1 + 1                                                      PRI  270
      DO 20 I=2,IM1                                                     PRI  280
        DO 20 N=2,NM1                                                   PRI  290
C                                                                       PRI  300
          FELD(I,N) = REYN(I,N,T,TFL,RHO,IFB,KOM,EPSIL,DHYD,FZQ,FRQ,MZ, PRI  310
     1     MR)                                                          PRI  320
C                                                                       PRI  330
   20 CONTINUE                                                          PRI  340
C                                                                       PRI  350
      CALL EXTREM(FELD,1,0,IFB,R,Z,FZQ,DZ)                              PRI  360
C                                                                       PRI  370
      IF(IPRINT .GE. 1 .AND. IFPPP .EQ. 1) CALL SCHREI(FELD,U1,IFB,ZP,RPPRI  380
     1 )                                                                PRI  390
C                                                                       PRI  400
      IF(IPRINT .GE. 0 .AND. IFPPP .EQ. 1) CALL SCHREI(MR,U2,IFB,ZP,RP) PRI  410
C                                                                       PRI  420
      IF(IPRINT .GE. -2 .AND. IFPPP .EQ. 1) CALL SCHREI(MZ,U3,IFB,ZP,RP)PRI  430
C                                                                       PRI  440
      DO 30 I=2,IM1                                                     PRI  450
        DO 30 N=2,NM1                                                   PRI  460
          KK = KOM(I,N)                                                 PRI  470
C                                                                       PRI  480
          AVAU = VAU(I,N,FZQ,FRQ,MZ,MR,RHO)                             PRI  490
C                                                                       PRI  500
          FELD(I,N) = AVAU * 100. / EPSIL(KK)                           PRI  510
   30 CONTINUE                                                          PRI  520
C                                                                       PRI  530
      CALL EXTREM(FELD,2,0,IFB,R,Z,FZQ,DZ)                              PRI  540
C                                                                       PRI  550
      IF(IPRINT .GE. 1 .AND. IFPPP .EQ. 1) CALL SCHREI(FELD,U4,IFB,ZP,RPPRI  560
     1 )                                                                PRI  570
C                                                                       PRI  580
      DO 40 I=2,IM1                                                     PRI  590
        DO 40 N=2,NM1                                                   PRI  600
          FELD(I,N) = P(I,N) / 1.E5                                     PRI  610
   40 CONTINUE                                                          PRI  620
C                                                                       PRI  630
      CALL EXTREM(FELD,3,0,IFB,R,Z,FZQ,DZ)                              PRI  640
C                                                                       PRI  650
      IF(IPRINT .GE. 1 .AND. IFPPP .EQ. 1) CALL SCHREI(FELD,U7,IFB,ZP,RPPRI  660
     1 )                                                                PRI  670
C                                                                       PRI  680
      DO 41 I=3,IM1                                                     PRI  690
        II = I - 1                                                      PRI  700
        DO 41 N=2,NM1                                                   PRI  710
          ROGG(II,N) = ROGG(II,N) + ROGG(II-1,N)                        PRI  720
          FELD(I,N) = (P(I,N)-ROGG(II,N)) / 1.E05                       PRI  730
   41 CONTINUE                                                          PRI  740
C                                                                       PRI  750
      IF(IPRINT .GE. 1 .AND. IFPPP .EQ. 1) CALL SCHREI(FELD,U5,IFB,ZP,RPPRI  760
     1 )                                                                PRI  770
C                                                                       PRI  780
      IF0V = IF0                                                        PRI  790
      IF0 = 0                                                           PRI  800
C                                                                       PRI  810
      CALL EXTREM(STROM,4,1,IFB,R,Z,FZQ,DZ)                             PRI  820
C                                                                       PRI  830
      IF(IPRINT .GE. -1 .AND. IFPPP .EQ. 1) CALL SCHREI(STROM,U6,IFB,ZP,PRI  840
     1 RP)                                                              PRI  850
C                                                                       PRI  860
      DO 50 I=1,IM                                                      PRI  870
        DO 50 N=1,NM                                                    PRI  880
          FELD(I,N) = STROM(I,N)                                        PRI  890
   50 CONTINUE                                                          PRI  900
      IF0=IF0V                                                          PRI  910
      RETURN                                                            PRI  920
      END                                                               PRI  930
      SUBROUTINE QUELL(I,N,L,M,TFE,TFH,TFA,XK,FELD)                     QUE   10
C                                                                       QUE   20
C     BERECHNET DIE KONVEKTIVE WAERMEQUELLE AM GITTERPUNKT  I,N  ALS    QUE   30
C     SUMME DER 8 TEILBETRAEGE (8-ROHR-MODELL,---> SR ELEM4)            QUE   40
C                                                                       QUE   50
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             QUE   60
C                                                                       QUE   70
      DIMENSION FELD(KONN,KONI)                                         QUE   80
C                                                                       QUE   90
C                                                                       QUE  100
      GOTO(10,20,30,40),L                                               QUE  110
   10 IF(M .EQ. 1) GOTO 101                                             QUE  120
      IQ = I - 1                                                        QUE  130
      NQ = N                                                            QUE  140
      GOTO 500                                                          QUE  150
  101 IQ = I                                                            QUE  160
      NQ = N - 1                                                        QUE  170
      GOTO 500                                                          QUE  180
   20 IF(M .EQ. 1) GOTO 201                                             QUE  190
      IQ = I - 1                                                        QUE  200
      NQ = N                                                            QUE  210
      GOTO 500                                                          QUE  220
  201 IQ = I                                                            QUE  230
      NQ = N + 1                                                        QUE  240
      GOTO 500                                                          QUE  250
   30 IF(M .EQ. 1) GOTO 301                                             QUE  260
      IQ = I + 1                                                        QUE  270
      NQ = N                                                            QUE  280
      GOTO 500                                                          QUE  290
  301 IQ = I                                                            QUE  300
      NQ = N - 1                                                        QUE  310
      GOTO 500                                                          QUE  320
   40 IF(M .EQ. 1) GOTO 401                                             QUE  330
      IQ = I + 1                                                        QUE  340
      NQ = N                                                            QUE  350
      GOTO 500                                                          QUE  360
  401 IQ = I                                                            QUE  370
      NQ = N + 1                                                        QUE  380
  500 CONTINUE                                                          QUE  390
      FELD(I,N) = FELD(I,N) + XK * (TFH-TFA)                            QUE  400
      FELD(IQ,NQ) = FELD(IQ,NQ) + XK * (TFE-TFH)                        QUE  410
      RETURN                                                            QUE  420
      END                                                               QUE  430
      SUBROUTINE QUELLE(FELD,ZEITS,GEOFAK,DFAK,IFSTOP,IFINST,EPSIL,T,TFLUEL   10
     1 ,RHO,IFB,FZQ,DZ,P,XKR,XKZ,KOM,IFBR,IFBQ,STROM,XZSUM,XNSUM1,STZUK,UEL   20
     2 TFLVOR,VOL,STRBEZ,IFQROW,XKSUM,XKINT,XKSUMA,TFMIT,XH)            UEL   30
C                                                                       UEL   40
C     BILANZIERT DIE KONVEKTIVEN WAERMEQUELLEN                          UEL   50
C     STROM ---> VON SR QUELL BERECHNETE QUELLE                         UEL   60
C                                                                       UEL   70
CFZJ042                                                       09.09.05  UEL   80
      COMMON /ZEITVR/ IFZDR,ZTF(3,100),ZST(3,100),ZDR(100),ZVOR(100),IFZUEL   90
     1 ,IZKOM(3),IZKO,IFAGL,IZAEHL                                      UEL  100
C                                                                       UEL  110
      COMMON /PRESYS/ SYSINV,SYSUM1,SYSUM2,DELINV,PRMINV,PRSUM1,PRSUM2  UEL  120
C                                                                       UEL  130
      COMMON /BILANZ/ TGASM,IZAEL                                       UEL  140
C                                                                       UEL  150
      COMMON /HOHLR/ KPAR(10),KKOM(10),NHLR,PHOHL(10),NML(10),NMR(10),  UEL  160
     1 IL(10),VOLU(10)                                                  UEL  170
C                                                                       UEL  180
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      UEL  190
C                                                                       UEL  200
      COMMON /ITE2/ SMAX,TDIFF,SROWM,IRA,ZRS,NVIT,WTRANS,WSUM,WREST,IFGSUEL  210
C                                                                       UEL  220
CFZJ006 enlarged dimensions common QVAR                       28.11.03  UEL  230
      COMMON /QVAR/ DUM(1511),N61,URZ,ZLEKA,ABXEN                       UEL  240
C                                                                       UEL  250
      COMMON /KOMVAK/ KOMVAR,KONVAR,XKSUMK                              UEL  260
C                                                                       UEL  270
CFZJ055                                                       25.09.07  UEL  280
C                                                                       UEL  290
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             UEL  300
C                                                                       UEL  310
CFZJ042                                                       09.09.05  UEL  320
      COMMON /COUPL/ IPRINT                                             UEL  330
C                                                                       UEL  340
      DIMENSION XKSUM(KOMAX),XKINT(KOMAX),XKSUMA(KOMAX),TFMIT(KOMAX),   UEL  350
     1 XH(KOMAX),FELD(KONN,KONI),EPSIL(KOMAX),T(KONN,KONI),             UEL  360
     2 TFL(KONN,KONI),RHO(KONN,KONI),IFB(KONN,KONI),FZQ(KONI),DZ(KONN), UEL  370
     3 P(KONN,KONI),XKR(KONN,KONI),XKZ(KONN,KONI),KOM(KONN,KONI),       UEL  380
     4 IFBR(KOMAX),IFBQ(KOMAX),STROM(KONN,KONI),XZSUM(KOMAX),           UEL  390
     5 XNSUM1(KOMAX),STZUK(KOMAX),TFLVOR(KOMAX),VOL(KOMAX),STRBEZ(KONN),UEL  400
     6 IFQROW(KONN)                                                     UEL  410
C                                                                       UEL  420
  207 FORMAT (58X,10(1H-)/58X,1PE10.3,3X,2PF6.2,' %'/)                  UEL  430
  251 FORMAT (//5X,'COMP',3X,'CONV.HS.',4X,'TRANSMIT.HEAT',2X,'TFL-AVG.'UEL  440
     1 ,2X,'PR-VOID',3X,'INVENTORY'/14X,'(KW)',9X,'(KWSEC)',5X,'(DEG C)'UEL  450
     2 ,4X,'(BAR)',7X,'(KG)'/)                                          UEL  460
  252 FORMAT (4X,I4,2X,1PE10.3,5X,1PE10.3,4X,0PF7.2,12X,1PE10.3)        UEL  470
  253 FORMAT (' **WARNING** ASSIGNED VOLUME OF CONVECTIVE HEAT SOURCE ATUEL  480
     1 GRID POINT (',I2,',',I2,') IS ZERO')                             UEL  490
  254 FORMAT (4X,I4,2X,1PE10.3,5X,1PE10.3,4X,0PF7.2,2X,0PF8.4,2X,1PE10.3UEL  500
     1 )                                                                UEL  510
 1000 FORMAT (' **WARNING** PROGRAM MODUL K O N V E K, SR QUELLE --> NO UEL  520
     1CONVERGENCY IN CALCULATION OF SYSTEM PRESSURE'/1X,'PSYS =',1PE12.5UEL  530
     2 ,' , PSYS1 =',1PE12.5,' , ITSYS =',I4)                           UEL  540
C                                                                       UEL  550
C                                                                       UEL  560
      DIF = 0.                                                          UEL  570
      WTRANS = 0.                                                       UEL  580
      WTR1 = 0.                                                         UEL  590
      WSUM = 0.                                                         UEL  600
      IF(IZAEL .NE. 0) GOTO 150                                         UEL  610
      ZEITA = ZEITS                                                     UEL  620
      SYSINV = 0.                                                       UEL  630
      DO 60 K=1,KM                                                      UEL  640
        XKINT(K) = 0.                                                   UEL  650
        XKSUMA(K) = 0.                                                  UEL  660
   60 CONTINUE                                                          UEL  670
  150 CONTINUE                                                          UEL  680
      DELTAZ = ZEITS - ZEITA                                            UEL  690
      DO 50 K=1,KM                                                      UEL  700
        TFMIT(K) = 0.                                                   UEL  710
        XH(K) = 0.                                                      UEL  720
        XKSUM(K) = 0.                                                   UEL  730
   50 CONTINUE                                                          UEL  740
      DO 102 I=1,IM1                                                    UEL  750
        DO 102 N=1,NM1                                                  UEL  760
          RHO(I,N) = 2. * RHO(I,N) * (ABS(XKR(I,N))+ABS(XKZ(I,N)))      UEL  770
          FELD(I,N) = STROM(I,N)                                        UEL  780
  102 CONTINUE                                                          UEL  790
      DO 100 I=1,IM1                                                    UEL  800
        DO 100 N=1,NM1                                                  UEL  810
          STROM(I,N) = 0.                                               UEL  820
          K = KOM(I,N)                                                  UEL  830
C                                                                       UEL  840
          IF(IFBG(I,N,IFB) .EQ. 0) GOTO 100                             UEL  850
C                                                                       UEL  860
          IF(FELD(I,N) .GT. 0.) WTRANS = WTRANS + FELD(I,N)             UEL  870
          IF(FELD(I,N) .LT. 0.) WTR1 = WTR1 - FELD(I,N)                 UEL  880
          WSUM = WSUM + FELD(I,N)                                       UEL  890
          FE1 = 0.                                                      UEL  900
          I1 = I + 1                                                    UEL  910
          N1 = N + 1                                                    UEL  920
          DO 10 II=I,I1                                                 UEL  930
            DO 10 NN=N,N1                                               UEL  940
              K1 = KOM(II,NN)                                           UEL  950
              IF(K1 .EQ. 0) GOTO 10                                     UEL  960
              IF1 = IFBQ(K1)                                            UEL  970
              IF2 = IFB(II,NN)                                          UEL  980
              IF(IF1 .EQ. -1 .OR. IF2 .EQ. 0) GOTO 10                   UEL  990
              FE1 = FE1 + FZQ(NN) * DZ(II) / 4.                         UEL 1000
   10     CONTINUE                                                      UEL 1010
          IF(FE1 .NE. 0.) GOTO 11                                       UEL 1020
          IF(IZAEL .NE. 0 .OR. FELD(I,N) .EQ. 0.) GOTO 103              UEL 1030
          WRITE (6,253) I,N                                             UEL 1040
C                                                                       UEL 1050
          CALL ABEND(2)                                                 UEL 1060
C                                                                       UEL 1070
  103     FELD(I,N) = 0.                                                UEL 1080
          RHO(I,N) = 0.                                                 UEL 1090
          STROM(I,N) = TFL(I,N)                                         UEL 1100
          GOTO 101                                                      UEL 1110
   11     CONTINUE                                                      UEL 1120
          RHO(I,N) = (RHO(I,N)+RHO(I,N+1)+RHO(I+1,N)+RHO(I+1,N+1)) / 4. UEL 1130
          STROM(I,N) = TFL(I,N)                                         UEL 1140
          IF(RHO(I,N) .EQ. 0.) GOTO 12                                  UEL 1150
          STROM(I,N) = T(I,N) + FELD(I,N) / RHO(I,N)                    UEL 1160
C                                                                       UEL 1170
C     STROM IST AEQIVALENTE MASCHENGASTEMPERATUR                        UEL 1180
C                                                                       UEL 1190
          RHO(I,N) = RHO(I,N) / (1.E6*FE1)                              UEL 1200
C                                                                       UEL 1210
C     RHO IST ALPHA*F/VOL                                               UEL 1220
C                                                                       UEL 1230
   12     FELD(I,N) = FELD(I,N) / (1.E6*FE1)                            UEL 1240
  101     IF(IFB(I,N) .EQ. 0) GOTO 100                                  UEL 1250
          IF(IFBQ(K) .EQ. -1) GOTO 100                                  UEL 1260
          FE1 = (FELD(I,N)+FELD(I-1,N)+FELD(I,N-1)+FELD(I-1,N-1)) / 4.  UEL 1270
          VKO = FZQ(N) * DZ(I) * 1.E6                                   UEL 1280
          XKSUM(K) = XKSUM(K) + FE1 * VKO / GEOFAK                      UEL 1290
  100 CONTINUE                                                          UEL 1300
      ZEITA = ZEITS                                                     UEL 1310
      AWS1 = ABS(WTR1)                                                  UEL 1320
      IF(AWS1 .GT. WTRANS) WTRANS = AWS1                                UEL 1330
      WSUM = WSUM / WTRANS * 100.                                       UEL 1340
      WTRANS = WTRANS / 1000.                                           UEL 1350
      WREST = WTRANS * WSUM / 100.                                      UEL 1360
      IF(IFINST .EQ. 0 .AND. IFSTOP .NE. 1) RETURN                      UEL 1370
CFZJ042                                                       09.09.05  UEL 1380
      IF (IPRINT .GE. 1) WRITE (6,251)                                  UEL 1390
      PSYS1 = DRUCK                                                     UEL 1400
      ITSYS = 0                                                         UEL 1410
  204 CONTINUE                                                          UEL 1420
      ITSYS = ITSYS + 1                                                 UEL 1430
      SYSUM1 = 0.                                                       UEL 1440
      SYSUM2 = 0.                                                       UEL 1450
      DO 200 I=2,IM1                                                    UEL 1460
        DO 200 N=2,NM1                                                  UEL 1470
          IFF = IFB(I,N) + 1                                            UEL 1480
          GOTO(200),IFF                                                 UEL 1490
          K = KOM(I,N)                                                  UEL 1500
          TEM = (TFL(I-1,N-1)+TFL(I-1,N)+TFL(I,N-1)+TFL(I,N)) / 4.      UEL 1510
          PB = P(I,N) / 1.E05                                           UEL 1520
          PBEZ = PB + PSYS1                                             UEL 1530
C                                                                       UEL 1540
          RH = DICHTE(PBEZ,TEM)                                         UEL 1550
C                                                                       UEL 1560
          VV = FZQ(N) * DZ(I) * EPSIL(K)                                UEL 1570
          A = VV * RH / (PSYS1+PB) / 1.E05                              UEL 1580
          SYSUM1 = SYSUM1 + A                                           UEL 1590
          SYSUM2 = SYSUM2 + PB * A * 1.E05                              UEL 1600
          IF(IFINST .EQ. 0 .AND. IFSTOP .EQ. 1) SYSINV = SYSINV + VV *  UEL 1610
     1     RH                                                           UEL 1620
          IF(IZAEL .NE. 0 .OR. ITSYS .GT. 1) GOTO 200                   UEL 1630
          SYSINV = SYSINV + VV * RH                                     UEL 1640
  200 CONTINUE                                                          UEL 1650
      IF(IZAEL .EQ. 0 .AND. ITSYS .EQ. 1) SYSINV = SYSINV + PRMINV      UEL 1660
      IF(ITSYS .EQ. 1) SYSINV = SYSINV + DELINV                         UEL 1670
      IF(IFZDR .NE. 2) GOTO 202                                         UEL 1680
C                                                                       UEL 1690
      PSYS = (SYSINV-SYSUM2-PRSUM2) / (SYSUM1+PRSUM1) / 1.E05           UEL 1700
      DPS = PSYS - PSYS1                                                UEL 1710
      DPS = ABS(DPS/PSYS1)                                              UEL 1720
      IF(DPS .LT. 1.E-5) GOTO 203                                       UEL 1730
      IF(ITSYS .GE. 20) GOTO 205                                        UEL 1740
      PSYS1 = PSYS                                                      UEL 1750
      GOTO 204                                                          UEL 1760
  205 WRITE (6,1000) PSYS,PSYS1,ITSYS                                   UEL 1770
C                                                                       UEL 1780
      CALL ABEND(2)                                                     UEL 1790
C                                                                       UEL 1800
      GOTO 203                                                          UEL 1810
  202 PSYS = DRUCK                                                      UEL 1820
  203 DFAK = PSYS / DRUCK                                               UEL 1830
      A1 = 0.                                                           UEL 1840
      A2 = 0.                                                           UEL 1850
      DO 210 I=1,IM1                                                    UEL 1860
        DO 210 N=1,NM1                                                  UEL 1870
          K = KOM(I,N)                                                  UEL 1880
          IFF = IFB(I,N) + 1                                            UEL 1890
          GOTO(210),IFF                                                 UEL 1900
          TEM = (TFL(I-1,N-1)+TFL(I-1,N)+TFL(I,N-1)+TFL(I,N)) / 4.      UEL 1910
          PBEZ = P(I,N) / 1.E05 + PSYS                                  UEL 1920
          VV = FZQ(N) * DZ(I) * EPSIL(K)                                UEL 1930
C                                                                       UEL 1940
          RH = DICHTE(PBEZ,TEM)                                         UEL 1950
C                                                                       UEL 1960
          XH(K) = XH(K) + VV * RH                                       UEL 1970
          TFMIT(K) = TFMIT(K) + TEM * VV * RH                           UEL 1980
  210 CONTINUE                                                          UEL 1990
      DO 201 K=1,KM                                                     UEL 2000
        IF(XH(K) .EQ. 0.) GOTO 201                                      UEL 2010
        A1 = A1 + TFMIT(K)                                              UEL 2020
        A2 = A2 + XH(K)                                                 UEL 2030
        TFMIT(K) = TFMIT(K) / XH(K)                                     UEL 2040
  201 CONTINUE                                                          UEL 2050
      FRAC = A2 / SYSINV                                                UEL 2060
      TGASM = A1 / A2                                                   UEL 2070
      DO 70 K=1,KM                                                      UEL 2080
        XKINT(K) = XKINT(K) + XKSUMA(K) * DELTAZ                        UEL 2090
        XKSUMA(K) = XKSUM(K)                                            UEL 2100
   70 CONTINUE                                                          UEL 2110
CFZJ042                                                       09.09.05  UEL 2120
      IF(IPRINT .LT. 1) GOTO 83                                         UEL 2130
      DO 80 K=1,KM                                                      UEL 2140
        IF(IFBR(K) .EQ. 5) GOTO 81                                      UEL 2150
        WRITE (6,252) K,XKSUM(K),XKINT(K),TFMIT(K),XH(K)                UEL 2160
        GOTO 80                                                         UEL 2170
   81   CONTINUE                                                        UEL 2180
        DO 82 J=1,NHLR                                                  UEL 2190
          KK = KKOM(J)                                                  UEL 2200
          IF(K .EQ. KK) JJ = J                                          UEL 2210
   82   CONTINUE                                                        UEL 2220
        WRITE (6,254) K,XKSUM(K),XKINT(K),TFMIT(K),PHOHL(JJ),XH(K)      UEL 2230
   80 CONTINUE                                                          UEL 2240
      IF(KOMVAR .GT. 0) XKSUMK = XKSUM(KONVAR) * 1000.                  UEL 2250
      WRITE (6,207) A2,FRAC                                             UEL 2260
   83 IF(IZAEL .EQ. 0) IZAEL = 1                                        UEL 2270
      RETURN                                                            UEL 2280
      END                                                               UEL 2290
      FUNCTION REYN(I,N,T,TFL,RHO,IFB,KOM,EPSIL,DHYD,FZQ,FRQ,MZ,MR)     REY   10
C                                                                       REY   20
C     BILDET DIE RE-ZAHL MIT DHYD UND DER LEERVOL.-GESCHWINDIGKEIT      REY   30
C                                                                       REY   40
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      REY   50
C                                                                       REY   60
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             REY   70
C                                                                       REY   80
      DIMENSION T(KONN,KONI),TFL(KONN,KONI),RHO(KONN,KONI),             REY   90
     1 IFB(KONN,KONI),KOM(KONN,KONI),EPSIL(KOMAX),DHYD(KOMAX),FZQ(KONI),REY  100
     2 FRQ(KONN,KONI)                                                   REY  110
C                                                                       REY  120
      REAL MZ(KONN,KONI),MR(KONN,KONI)                                  REY  130
C                                                                       REY  140
C                                                                       REY  150
      IFB1 = IFB(I,N) + 1                                               REY  160
C                                                                       REY  170
      REYN = 0.                                                         REY  180
C                                                                       REY  190
      GOTO(40),IFB1                                                     REY  200
      TEM1 = (TFL(I,N)+T(I,N)) / 2.                                     REY  210
      TEM2 = (TFL(I-1,N)+T(I-1,N)) / 2.                                 REY  220
      TEM3 = (TFL(I,N-1)+T(I,N-1)) / 2.                                 REY  230
      TEM4 = (TFL(I-1,N-1)+T(I-1,N-1)) / 2.                             REY  240
      TEM = (TEM1+TEM2+TEM3+TEM4) / 4.                                  REY  250
      KK = KOM(I,N)                                                     REY  260
      EPSI = EPSIL(KK)                                                  REY  270
C                                                                       REY  280
      ET = ETHAG(TEM)                                                   REY  290
C                                                                       REY  300
      V = VAU(I,N,FZQ,FRQ,MZ,MR,RHO)                                    REY  310
C                                                                       REY  320
      GOTO(40,50),IFB1                                                  REY  330
      GOTO 30                                                           REY  340
C                                                                       REY  350
   50 REYN = REK * RHO(I,N) * V / ET / (1.-EPSI)                        REY  360
C                                                                       REY  370
      RETURN                                                            REY  380
C                                                                       REY  390
   30 REYN = RHO(I,N) * V / EPSI * DHYD(KK) / ET / 100.                 REY  400
C                                                                       REY  410
   40 RETURN                                                            REY  420
      END                                                               REY  430
      SUBROUTINE SCHGAS(FELD,U,ZP,RP,IFB)                               SCH   10
C                                                                       SCH   20
C     AUSGABE DER KNOTENZENTRIERTEN ERGEBNISSE                          SCH   30
C                                                                       SCH   40
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      SCH   50
C                                                                       SCH   60
      COMMON /SCHR/ IF0                                                 SCH   70
C                                                                       SCH   80
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             SCH   90
C                                                                       SCH  100
      DIMENSION FELD(KONN,KONI),ZP(KONN),RP(KONI),IFB(KONN,KONI)        SCH  110
C                                                                       SCH  120
      CHARACTER*4 U(4)                                                  SCH  130
C                                                                       SCH  140
CFZJ042                                                       09.09.05  SCH  150
    1 FORMAT (1H1)                                                      SCH  160
    7 FORMAT (//10X,4A4)                                                SCH  170
   30 FORMAT (/9X,'R=   ',11(F6.1,4X),F6.1)                             SCH  180
   35 FORMAT (1X,'Z=')                                                  SCH  190
   60 FORMAT (1X,0PF7.1,5X,15(1PE8.1,2X))                               SCH  200
   61 FORMAT (1X,0PF7.1,5X,15(F8.1,2X))                                 SCH  210
   62 FORMAT (1X,0PF7.1,5X,15(F8.4,2X))                                 SCH  220
C                                                                       SCH  230
C                                                                       SCH  240
CFZJ042                                                       09.09.05  SCH  250
      WRITE (6,1)                                                       SCH  260
      WRITE (6,7) (U(I),I=1,4)                                          SCH  270
      FMAX = 0.                                                         SCH  280
      DO 5 I=1,IM1                                                      SCH  290
        DO 5 N=1,NM1                                                    SCH  300
C                                                                       SCH  310
          IF(IFBG(I,N,IFB) .NE. 0) GOTO 6                               SCH  320
C                                                                       SCH  330
          IF(IF0 .EQ. 0) GOTO 6                                         SCH  340
          FELD(I,N) = 0.                                                SCH  350
    6     FF = FELD(I,N)                                                SCH  360
          FF = ABS(FF)                                                  SCH  370
          IF(FF .GT. FMAX) FMAX = FF                                    SCH  380
    5 CONTINUE                                                          SCH  390
      IFW = 3                                                           SCH  400
      IF(FMAX .GT. 9999. .OR. FMAX .LT. .02) IFW = 1                    SCH  410
      IF(FMAX .LE. 9999. .AND. FMAX .GT. 1.) IFW = 2                    SCH  420
      N1 = 1                                                            SCH  430
   10 N2 = N1 + 11                                                      SCH  440
      IF(N2 .GT. NM1) N2 = NM1                                          SCH  450
      WRITE (6,30) (RP(N),N=N1,N2)                                      SCH  460
      WRITE (6,35)                                                      SCH  470
      DO 50 I=1,IM1                                                     SCH  480
        GOTO(51,52,53),IFW                                              SCH  490
   52   WRITE (6,61) ZP(I),(FELD(I,N),N=N1,N2)                          SCH  500
        GOTO 50                                                         SCH  510
   53   WRITE (6,62) ZP(I),(FELD(I,N),N=N1,N2)                          SCH  520
        GOTO 50                                                         SCH  530
   51   WRITE (6,60) ZP(I),(FELD(I,N),N=N1,N2)                          SCH  540
   50 CONTINUE                                                          SCH  550
      N1 = N2 + 1                                                       SCH  560
      IF(N2 .LT. NM1) GOTO 10                                           SCH  570
      RETURN                                                            SCH  580
      END                                                               SCH  590
      SUBROUTINE SCHREI(FELD,U,IFB,ZP,RP)                               CHR   10
C                                                                       CHR   20
C     AUSGABE DER MASCHENZENTRIERTEN ERGEBNISSE                         CHR   30
C                                                                       CHR   40
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      CHR   50
C                                                                       CHR   60
      COMMON /SCHR/ IF0                                                 CHR   70
C                                                                       CHR   80
CFZJ055                                                       25.09.07  CHR   90
C                                                                       CHR  100
CFZJ042                                                       09.09.05  CHR  110
      COMMON /COUPL/ IPRINT                                             CHR  120
C                                                                       CHR  130
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             CHR  140
C                                                                       CHR  150
      DIMENSION FELD(KONN,KONI),IFB(KONN,KONI),ZP(KONN),RP(KONI)        CHR  160
C                                                                       CHR  170
      CHARACTER*4 U(4)                                                  CHR  180
C                                                                       CHR  190
    7 FORMAT (1H1,10X,4A4)                                              CHR  200
   30 FORMAT (/3X,'R=   ',15(F6.1,4X))                                  CHR  210
   35 FORMAT (1X,'Z=')                                                  CHR  220
   60 FORMAT (1X,0PF7.1/12X,15(1PE8.1,2X))                              CHR  230
   61 FORMAT (1X,0PF7.1/12X,15(F8.2,2X))                                CHR  240
   62 FORMAT (1X,0PF7.1/12X,15(F8.4,2X))                                CHR  250
C                                                                       CHR  260
C                                                                       CHR  270
CFZJ042                                                       09.09.05  CHR  280
      WRITE (6,7) (U(I),I=1,4)                                          CHR  290
      FMAX = 0.                                                         CHR  300
      IM = IM1 + 1                                                      CHR  310
      NM = NM1 + 1                                                      CHR  320
      DO 5 I=1,IM                                                       CHR  330
        DO 5 N=1,NM                                                     CHR  340
          IF(IFB(I,N) .NE. 0) GOTO 6                                    CHR  350
          IF(IF0 .EQ. 0) GOTO 6                                         CHR  360
          FELD(I,N) = 0.                                                CHR  370
    6     FF = FELD(I,N)                                                CHR  380
          FF = ABS(FF)                                                  CHR  390
          IF(FF .GT. FMAX) FMAX = FF                                    CHR  400
    5 CONTINUE                                                          CHR  410
      IFW = 3                                                           CHR  420
      IF(FMAX .GT. 9999. .OR. FMAX .LT. .01) IFW = 1                    CHR  430
      IF(FMAX .LE. 9999. .AND. FMAX .GT. 1.) IFW = 2                    CHR  440
      N1 = 2                                                            CHR  450
   10 N2 = N1 + 10                                                      CHR  460
      IF(N2 .GT. NM1) N2 = NM1                                          CHR  470
      NN1 = N1 - 1                                                      CHR  480
      WRITE (6,30) (RP(N),N=NN1,N2)                                     CHR  490
      WRITE (6,35)                                                      CHR  500
      DO 50 I=2,IM1                                                     CHR  510
        I1 = I - 1                                                      CHR  520
        GOTO(51,52,53),IFW                                              CHR  530
   52   WRITE (6,61) ZP(I1),(FELD(I,N),N=N1,N2)                         CHR  540
        GOTO 50                                                         CHR  550
   53   WRITE (6,62) ZP(I1),(FELD(I,N),N=N1,N2)                         CHR  560
        GOTO 50                                                         CHR  570
   51   WRITE (6,60) ZP(I1),(FELD(I,N),N=N1,N2)                         CHR  580
   50 CONTINUE                                                          CHR  590
      WRITE (6,60) ZP(I1+1)                                             CHR  600
      N1 = N2 + 1                                                       CHR  610
      IF(N2 .LT. NM1) GOTO 10                                           CHR  620
      RETURN                                                            CHR  630
      END                                                               CHR  640
      SUBROUTINE SETK(IMAX,NMAX,KOM,KMAX,MR)                            SET   10
C                                                                       SET   20
C     BELEGUNG DES GITTERS MIT KOMPOSITIONEN                            SET   30
C                                                                       SET   40
CFZJ042                                                       09.09.05  SET   50
C                                                                       SET   60
CFZJ055                                                       25.09.07  SET   70
C                                                                       SET   80
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             SET   90
C                                                                       SET  100
      DIMENSION KOM(KONN,KONI)                                          SET  110
C                                                                       SET  120
      REAL MR(KONN,KONI)                                                SET  130
C                                                                       SET  140
   80 FORMAT (1X,'**ERROR** IMAX OR NMAX EXCEEDED OR MESH (I,N =',2I3,  SET  150
     1 ' ) NOT OCCUPIED WITH COMPOSITION.')                             SET  160
C                                                                       SET  170
C                                                                       SET  180
      NMAX = NMAX - 1                                                   SET  190
      IMAX = IMAX - 1                                                   SET  200
      DO 90 I=2,IMAX                                                    SET  210
        DO 90 N=2,NMAX                                                  SET  220
          IF(KOM(I,N) .EQ. 0) WRITE (6,80) I,N                          SET  230
   90 CONTINUE                                                          SET  240
      IMAX = IMAX + 1                                                   SET  250
      NMAX = NMAX + 1                                                   SET  260
      IM1 = IMAX - 1                                                    SET  270
      NM1 = NMAX - 1                                                    SET  280
      DO 95 I=2,IM1                                                     SET  290
        DO 95 N=2,NM1                                                   SET  300
          KK = KOM(I,N)                                                 SET  310
          MR(I,N) = FLOAT(KK)                                           SET  320
   95 CONTINUE                                                          SET  330
      RETURN                                                            SET  340
      END                                                               SET  350
      SUBROUTINE SETZT(IMAX,NMAX,RAD,PHI,IDT,NDT,T,IE,NE,PHE,RE,IDE,NDE,ETZ   10
     1 TE,WQR)                                                          ETZ   20
C                                                                       ETZ   30
C     INTERPOLATION DER WERTE ZWISCHEN THERMIX- UND KONVEK-GITTER       ETZ   40
C                                                                       ETZ   50
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             ETZ   60
C                                                                       ETZ   70
      DIMENSION RAD(1),PHI(1),RE(1),PHE(1),WG(IMAZ),WS(IMAZ),T(IDT,NDT),ETZ   80
     1 TE(IDE,NDE),WQR(IMAZ,NMAZ)                                       ETZ   90
C                                                                       ETZ  100
C                                                                       ETZ  110
      DO 7 I=1,IMAX                                                     ETZ  120
        DO 7 N=1,NMAX                                                   ETZ  130
          T(I,N) = 0.                                                   ETZ  140
    7 CONTINUE                                                          ETZ  150
      DO 30 NGE=1,IE                                                    ETZ  160
        DO 40 I=1,NE                                                    ETZ  170
          WG(I) = TE(NGE,I)                                             ETZ  180
   40   CONTINUE                                                        ETZ  190
C                                                                       ETZ  200
        CALL REIPO(NE,RE,WG,IMAX,RAD,WS,0)                              ETZ  210
C                                                                       ETZ  220
        DO 50 N=1,IMAX                                                  ETZ  230
          WQR(NGE,N) = WS(N)                                            ETZ  240
   50   CONTINUE                                                        ETZ  250
   30 CONTINUE                                                          ETZ  260
      DO 60 N=1,IMAX                                                    ETZ  270
        DO 70 I=1,IE                                                    ETZ  280
          WG(I) = WQR(I,N)                                              ETZ  290
   70   CONTINUE                                                        ETZ  300
C                                                                       ETZ  310
        CALL REIPO(IE,PHE,WG,NMAX,PHI,WS,0)                             ETZ  320
C                                                                       ETZ  330
        DO 80 I=1,NMAX                                                  ETZ  340
          T(N,I) = WS(I)                                                ETZ  350
   80   CONTINUE                                                        ETZ  360
   60 CONTINUE                                                          ETZ  370
      RETURN                                                            ETZ  380
      END                                                               ETZ  390
      SUBROUTINE STRLAM(IFXKK,TFL,RHO,IFB,XKK,FZQ,FRQ,DR,DZ,XKR,XKZ,KOM,STR   10
     1 EPSIL,XKON,DHYD,SUMXK,QTV,LTV,XGEO,T,MZ,MR)                      STR   20
C                                                                       STR   30
C     BERECHNET DIE KONSTANTEN FUER DIE STROEMUNGSBERECHNUNG            STR   40
C                                                                       STR   50
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      STR   60
C                                                                       STR   70
      COMMON /VERTI/ NVERTI,KKOMV(30)                                   STR   80
C                                                                       STR   90
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             STR  100
C                                                                       STR  110
      COMMON /UPERR/ OERR                                               STR  120
C                                                                       STR  130
      DIMENSION TFL(KONN,KONI),RHO(KONN,KONI),IFB(KONN,KONI),           STR  140
     1 XKK(KONN,KONI),FZQ(KONI),FRQ(KONN,KONI),DR(KONI),DZ(KONN),       STR  150
     2 XKR(KONN,KONI),XKZ(KONN,KONI),KOM(KONN,KONI),EPSIL(KOMAX),       STR  160
     3 XKON(KOMAX),DHYD(KOMAX),SUMXK(KONI,30),QTV(KOMAX),XGEO(KOMAX),   STR  170
     4 T(KONN,KONI)                                                     STR  180
C                                                                       STR  190
      REAL MZ(KONN,KONI),MR(KONN,KONI),LTV(KOMAX)                       STR  200
C                                                                       STR  210
C                                                                       STR  220
      DO 202 NV=1,NVERTI                                                STR  230
        DO 202 N=2,NM1                                                  STR  240
          SUMXK(N,NV) = 0.                                              STR  250
  202 CONTINUE                                                          STR  260
      IF(IFXKK .EQ. 0) GOTO 120                                         STR  270
      DO 100 I=2,IM1                                                    STR  280
        DO 100 N=2,NM1                                                  STR  290
          XKK(I,N) = 0.                                                 STR  300
          KK = KOM(I,N)                                                 STR  310
          EPSI = EPSIL(KK)                                              STR  320
          IFB1 = IFB(I,N) + 1                                           STR  330
          GOTO(100,20,105,105,105,100),IFB1                             STR  340
   20     CONTINUE                                                      STR  350
          TEM = (TFL(I,N)+TFL(I-1,N)+TFL(I,N-1)+TFL(I-1,N-1)) / 4.      STR  360
C                                                                       STR  370
          ET = ETHAG(TEM)                                               STR  380
C                                                                       STR  390
          RE = REYN(I,N,T,TFL,RHO,IFB,KOM,EPSIL,DHYD,FZQ,FRQ,MZ,MR)     STR  400
C                                                                       STR  410
C     XKLAM=133.+3.663*RE**.9                                           STR  420
C                                                                       STR  430
          ARE = RE**.9                                                  STR  440
          XKLAM = 142.22 + 3.663 * ARE                                  STR  450
C                                                                       STR  460
C     DRUCKVERLUSTGESETZ GEMAESS KTA-REGELENTWURF '81                   STR  470
C                                                                       STR  480
          AEP = 1. - EPSI                                               STR  490
          AEP = AEP**2.                                                 STR  500
          AEP3 = EPSI**3.                                               STR  510
          XKK(I,N) = PK * XKLAM * ET / REK * AEP / AEP3                 STR  520
          GOTO 100                                                      STR  530
C                                                                       STR  540
  105     V = VAU(I,N,FZQ,FRQ,MZ,MR,RHO)                                STR  550
C                                                                       STR  560
          IF(V .EQ. 0.) V = .0002                                       STR  570
C                                                                       STR  580
          RE = REYN(I,N,T,TFL,RHO,IFB,KOM,EPSIL,DHYD,FZQ,FRQ,MZ,MR)     STR  590
C                                                                       STR  600
          IF(IFB1 .NE. 4 .AND. IFB1 .NE. 5) GOTO 106                    STR  610
          PSI = 1. - 0.7854 / QTV(KK)                                   STR  620
C                                                                       STR  630
C     PSI IST DAS MITTLERE LEERVOLUMEN EINER ROHRREIHE                  STR  640
C     WIRD BENOETIGT FUER DRUCKVERLUSTFORMELN NACH VDI-LD (1977)        STR  650
C                                                                       STR  660
          RE = RE * EPSI / PSI                                          STR  670
C                                                                       STR  680
C     RE MIT MITTL.GESCHW. IN DER ROHRREIHE                             STR  690
C                                                                       STR  700
          XG = XGEO(KK)                                                 STR  710
          XQ = QTV(KK)                                                  STR  720
          XL = LTV(KK)                                                  STR  730
C                                                                       STR  740
          AXIRE1 = XIRE1(RE,XG,XQ,XL)                                   STR  750
C                                                                       STR  760
          APSI = PSI**2.                                                STR  770
          XKK(I,N) = V * RHO(I,N) / 2. / APSI * (AXIRE1/DHYD(KK)*100.+  STR  780
     1     XKON(KK)*100.)                                               STR  790
          GOTO 100                                                      STR  800
C                                                                       STR  810
  106     AXIRE = XIRE(RE)                                              STR  820
C                                                                       STR  830
          AEPSI = EPSI**2.                                              STR  840
          XKK(I,N) = V * RHO(I,N) / 2. / AEPSI * (AXIRE/DHYD(KK)*100.+  STR  850
     1     XKON(KK)*100.)                                               STR  860
  100 CONTINUE                                                          STR  870
  120 CONTINUE                                                          STR  880
      DO 200 I=2,IM1                                                    STR  890
        DO 200 N=2,NM1                                                  STR  900
          IFB1 = IFB(I,N) + 1                                           STR  910
          GOTO(200),IFB1                                                STR  920
          KK = KOM(I,N)                                                 STR  930
          IF(XKR(I,N) .EQ. OERR) GOTO 150                               STR  940
          XKR(I,N) = XKK(I,N) / RHO(I,N) / FRQ(I,N) * DR(N) / 2.        STR  950
          XKR(I,N) = XKR(I,N) + XKK(I,N+1) / RHO(I,N+1) / FRQ(I,N+1) *  STR  960
     1     DR(N+1) / 2.                                                 STR  970
          IF(IFB1 .EQ. 5) XKR(I,N) = XKR(I,N) / QTV(KK)                 STR  980
  150     IF(XKZ(I,N) .EQ. OERR) GOTO 200                               STR  990
          XKZ(I,N) = XKK(I,N) / RHO(I,N) / FZQ(N) * DZ(I) / 2.          STR 1000
          XKZ(I,N) = XKZ(I,N) + XKK(I+1,N) / RHO(I+1,N) / FZQ(N) *      STR 1010
     1     DZ(I+1) / 2.                                                 STR 1020
          IF(IFB1 .NE. 3) GOTO 200                                      STR 1030
          KO = KOM(I,N)                                                 STR 1040
          DO 201 NV=1,NVERTI                                            STR 1050
            KO1 = KKOMV(NV)                                             STR 1060
            IF(KO .NE. KO1) GOTO 201                                    STR 1070
            SUMXK(N,NV) = SUMXK(N,NV) + XKZ(I,N)                        STR 1080
            GOTO 200                                                    STR 1090
  201     CONTINUE                                                      STR 1100
  200 CONTINUE                                                          STR 1110
      RETURN                                                            STR 1120
      END                                                               STR 1130
      SUBROUTINE STROEM(IFM1,IFM2,IOVER,IUVER,SUMXK,SUMRG,STRBEZ,IFQROW,TRO   10
     1 ROGG,IFB,FZQ,DZ,MZ,MR,P,XKR,XKZ,KOM,STROM,STZUK,TFL,RHO,XKK,FRQ, TRO   20
     2 DR,EPSIL,XKON,DHYD,QTV,LTV,XGEO,T,XZSUM,XNSUM1,IOKUL,IUKUL,IFBR, TRO   30
     3 TFLVOR,VOL,IPAR,JPAR,NPAR,XKP,ROGP)                              TRO   40
C                                                                       TRO   50
C     STEUERT DIE ITERATIVE BERECHNUNG DES DRUCK- UND STROEMUNGSFELDES. TRO   60
C     KONVERGENZKRITERIUM IST RESTBETRAG DES MASSENSTROMS (REIHENWEISE).TRO   70
C     NUR PSEUSO-LAMINARE STROEMUNGSFELDBERECHNUNG. DAS DRUCKFELD KANN  TRO   80
C     GESONDERT ITERIERT WERDEN (---> KEINE NEUE KONSTANTENBERECHNUNG). TRO   90
C                                                                       TRO  100
      COMMON /VERTI/ NVERTI,KKOMV(30)                                   TRO  110
C                                                                       TRO  120
      COMMON /HOHLR/ KPAR(10),KKOM(10),NHLR,PHOHL(10),NML(10),NMR(10),  TRO  130
     1 IL(10),VOLU(10)                                                  TRO  140
C                                                                       TRO  150
      COMMON /KUEL1/ NKUEL,KKOMK(4)                                     TRO  160
C                                                                       TRO  170
      COMMON /ITPARM/ IFGO1,IFGO3,IFGO4,OVLOOP                          TRO  180
C                                                                       TRO  190
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      TRO  200
C                                                                       TRO  210
      COMMON /ITER/ IB,IT,IT1,IT2,ITM1,ITM2,EPSI1,EPSI2,IFSQ,OVM1,OVM2  TRO  220
C                                                                       TRO  230
      COMMON /COUPL/ IPRINT                                             TRO  240
C                                                                       TRO  250
      COMMON /ITE2/ SMAX,TDIFF,SROWM,IRA,ZRS,NVIT,WTRANS,WSUM,WREST,IFGSTRO  260
C                                                                       TRO  270
CFZJ055                                                       25.09.07  TRO  280
C                                                                       TRO  290
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             TRO  300
C                                                                       TRO  310
      DIMENSION IOVER(KONI,30),IUVER(KONI,30),SUMXK(KONI,30),           TRO  320
     1 STRBEZ(KONN),IFQROW(KONN),ROGG(KONN,KONI),IFB(KONN,KONI),        TRO  330
     2 FZQ(KONI),DZ(KONN),P(KONN,KONI),XKR(KONN,KONI),XKZ(KONN,KONI),   TRO  340
     3 KOM(KONN,KONI),STROM(KONN,KONI),STZUK(KOMAX),TFL(KONN,KONI),     TRO  350
     4 RHO(KONN,KONI),XKK(KONN,KONI),FRQ(KONN,KONI),DR(KONI),           TRO  360
     5 EPSIL(KOMAX),XKON(KOMAX),DHYD(KOMAX),QTV(KOMAX),XGEO(KOMAX),     TRO  370
     6 T(KONN,KONI),XZSUM(KOMAX),XNSUM1(KOMAX),IOKUL(KONI,4),           TRO  380
     7 IUKUL(KONI,4),IFBR(KOMAX),TFLVOR(KOMAX),VOL(KOMAX),PB(4),        TRO  390
     8 IPAR(KONI*2,10),JPAR(KONI*2,10),NPAR(KONI*2,10),XKP(KONI*2),     TRO  400
     9 ROGP(KONI*2),SUMRG(KONI,30)                                      TRO  410
C                                                                       TRO  420
      REAL MZ(KONN,KONI),MR(KONN,KONI),LTV(KOMAX),KB(4),MXB,MYB         TRO  430
C                                                                       TRO  440
 2999 FORMAT (////22X,'*** ITERATIONS OF FLOW-FIELD CALCULATION ***'//  TRO  450
     1 13X,'IT2',2X,'NITERM',4X,'SMAX',5X,'SDIFF/SMAXA',3X,'SDIFF/SMAX',TRO  460
     2 5X,'SROWM',5X,' LINE'/)                                          TRO  470
CFZJ042                                                       09.09.05  TRO  480
 3000 FORMAT (11X,2I5,3X,4(1PE10.3,3X),2X,I3)                           TRO  490
C                                                                       TRO  500
C                                                                       TRO  510
      IFGS = 0                                                          TRO  520
      IFGO1 = 0                                                         TRO  530
      IM = IM1 + 1                                                      TRO  540
      NM = NM1 + 1                                                      TRO  550
C                                                                       TRO  560
C     OVREL = OVM2                                                      TRO  570
C                                                                       TRO  580
      OVREL = 1.                                                        TRO  590
      NVIT = 0                                                          TRO  600
      IFSQQ = IFSQ                                                      TRO  610
      IFXKK = 1                                                         TRO  620
      MXB = 1.                                                          TRO  630
      MYB = 1.                                                          TRO  640
      SMAXA = 1.                                                        TRO  650
      NITA = 1                                                          TRO  660
      MAXIT = 5                                                         TRO  670
      MAXI = 0                                                          TRO  680
  700 IF(IFSQQ .LT. 100) GOTO 701                                       TRO  690
      IFSQQ = IFSQQ - 100                                               TRO  700
      MAXI = MAXI + 1                                                   TRO  710
      MAXIT = MAXI                                                      TRO  720
      GOTO 700                                                          TRO  730
  701 CONTINUE                                                          TRO  740
      IFQ1 = IFSQQ + 1                                                  TRO  750
      ZWEPSI = 2. * EPSI2                                               TRO  760
      FUEPSI = 5. * EPSI2                                               TRO  770
      ZEEPSI = 10. * EPSI2                                              TRO  780
      ZZEPSI = 20. * EPSI2                                              TRO  790
      FZEPSI = 50. * EPSI2                                              TRO  800
      IF(IFM1 .EQ. 1) GOTO 215                                          TRO  810
      IF(IT .LT. 100) GOTO 50                                           TRO  820
      IFXKK = 0                                                         TRO  830
   50 CONTINUE                                                          TRO  840
      NITERM = 1                                                        TRO  850
C                                                                       TRO  860
      CALL WATCH(ENDE)                                                  TRO  870
C                                                                       TRO  880
      START = ENDE                                                      TRO  890
      IT2 = 0                                                           TRO  900
      IFEP = 0                                                          TRO  910
  200 CONTINUE                                                          TRO  920
      IT = 100                                                          TRO  930
C                                                                       TRO  940
      CALL STRLAM(IFXKK,TFL,RHO,IFB,XKK,FZQ,FRQ,DR,DZ,XKR,XKZ,KOM,EPSIL,TRO  950
     1 XKON,DHYD,SUMXK,QTV,LTV,XGEO,T,MZ,MR)                            TRO  960
C                                                                       TRO  970
      IT2 = IT2 + 1                                                     TRO  980
  215 CONTINUE                                                          TRO  990
      IF(IFM1 .EQ. 1) NITERM = 1                                        TRO 1000
      DO 111 NITER=1,NITERM                                             TRO 1010
        DIFF = 1.E10                                                    TRO 1020
        IFMXY = NITERM - NITER + 1                                      TRO 1030
        IF(NHLR .EQ. 0) GOTO 306                                        TRO 1040
        DO 305 J=1,NHLR                                                 TRO 1050
C                                                                       TRO 1060
          CALL ELEM3A(J,PP,IFM2,OVREL,IFMXY,XZSUM,XNSUM1,ROGG,STZUK,MZ, TRO 1070
     1     MR,P,XKR,XKZ,FZQ,DZ,IFB,KOM,IOVER,IUVER,SUMXK,SUMRG,IPAR,JPARTRO 1080
     2     ,NPAR,XKP,ROGP)                                              TRO 1090
C                                                                       TRO 1100
          IF(PP .LT. DIFF) DIFF = PP                                    TRO 1110
  305   CONTINUE                                                        TRO 1120
  306   CONTINUE                                                        TRO 1130
        DO 101 I=2,IM1                                                  TRO 1140
          DO 100 N=2,NM1                                                TRO 1150
            IFB1 = IFB(I,N) + 1                                         TRO 1160
            GOTO(100,307,100,100,307,100,100),IFB1                      TRO 1170
            GOTO 100                                                    TRO 1180
  307       KK = KOM(I,N)                                               TRO 1190
            STZU = STZUK(KK) * FZQ(N) * DZ(I)                           TRO 1200
            PB(1) = P(I,N-1)                                            TRO 1210
            PB(3) = P(I,N+1)                                            TRO 1220
            KB(1) = XKR(I,N-1)                                          TRO 1230
            KB(3) = XKR(I,N)                                            TRO 1240
            IF(IFB(I-1,N) .EQ. 2) GOTO 401                              TRO 1250
            ROGO = ROGG(I-1,N)                                          TRO 1260
            PB(2) = P(I-1,N)                                            TRO 1270
            KB(2) = XKZ(I-1,N)                                          TRO 1280
            GOTO 405                                                    TRO 1290
  401       CONTINUE                                                    TRO 1300
            KK1 = KOM(I-1,N)                                            TRO 1310
            DO 400 NV=1,NVERTI                                          TRO 1320
              KKK = KKOMV(NV)                                           TRO 1330
              IF(KKK .NE. KK1) GOTO 400                                 TRO 1340
              IO = IOVER(N,NV) - 1                                      TRO 1350
              ROGO = SUMRG(N,NV) + ROGG(IO,N)                           TRO 1360
              KB(2) = SUMXK(N,NV) + XKZ(IO,N)                           TRO 1370
              PB(2) = P(IO,N)                                           TRO 1380
              GOTO 405                                                  TRO 1390
  400       CONTINUE                                                    TRO 1400
  405       IF(IFB(I+1,N) .EQ. 2) GOTO 402                              TRO 1410
            ROGU = ROGG(I,N)                                            TRO 1420
            PB(4) = P(I+1,N)                                            TRO 1430
            KB(4) = XKZ(I,N)                                            TRO 1440
            GOTO 444                                                    TRO 1450
  402       CONTINUE                                                    TRO 1460
            KK1 = KOM(I+1,N)                                            TRO 1470
            DO 410 NV=1,NVERTI                                          TRO 1480
              KKK = KKOMV(NV)                                           TRO 1490
              IF(KKK .NE. KK1) GOTO 410                                 TRO 1500
              IO = IUVER(N,NV) + 1                                      TRO 1510
              ROGU = SUMRG(N,NV) + ROGG(I,N)                            TRO 1520
              KB(4) = SUMXK(N,NV) + XKZ(I,N)                            TRO 1530
              PB(4) = P(IO,N)                                           TRO 1540
              GOTO 444                                                  TRO 1550
  410       CONTINUE                                                    TRO 1560
  444       PP = P(I,N)                                                 TRO 1570
C                                                                       TRO 1580
            CALL ELEM3(IFM2,PB,PP,KB,STROMB,ROGO,ROGU,MXB,MYB,STZU,IFMXYTRO 1590
     1       )                                                          TRO 1600
C                                                                       TRO 1610
            MR(I,N) = MXB / 2.                                          TRO 1620
            MZ(I,N) = MYB / 2.                                          TRO 1630
            IF(IFM1 .EQ. 1) GOTO 100                                    TRO 1640
            P(I,N) = P(I,N) + (PP-P(I,N)) * OVREL                       TRO 1650
            IF(P(I,N) .LT. DIFF) DIFF = P(I,N)                          TRO 1660
  100     CONTINUE                                                      TRO 1670
  101   CONTINUE                                                        TRO 1680
C                                                                       TRO 1690
        IF(NVERTI .GT. 0) CALL ELEM3B(DIFF,OVREL,IFM2,IOVER,IUVER,ROGG, TRO 1700
     1   MZ,MR,P,XKZ)                                                   TRO 1710
C                                                                       TRO 1720
        IF(NKUEL .GT. 0) CALL ELEM3C(DIFF,OVREL,IFM2,IOKUL,IUKUL,ROGG,MZTRO 1730
     1   ,MR,P,XKZ)                                                     TRO 1740
C                                                                       TRO 1750
CFZJ042                                                       09.09.05  TRO 1760
        DO 110 I=2,IM1                                                  TRO 1770
          DO 110 N=2,NM1                                                TRO 1780
            P(I,N) = P(I,N) - DIFF                                      TRO 1790
  110   CONTINUE                                                        TRO 1800
  111 CONTINUE                                                          TRO 1810
      IF(NITERM .NE. 1) NVIT = NVIT + 1                                 TRO 1820
      NIT = NITERM                                                      TRO 1830
      NITERM = 1                                                        TRO 1840
      GOTO(310),NIT                                                     TRO 1850
      NITA = NIT                                                        TRO 1860
      GOTO 200                                                          TRO 1870
  310 CONTINUE                                                          TRO 1880
C                                                                       TRO 1890
C     BERECHNUNG DES RESTBETRAGES PRO REIHE                             TRO 1900
C                                                                       TRO 1910
      SMAX = 0.                                                         TRO 1920
      SROWM = 0.                                                        TRO 1930
      DO 300 I=2,IM1                                                    TRO 1940
        SROW = 0.                                                       TRO 1950
        IFQR = IFQROW(I)                                                TRO 1960
        GOTO(302,300),IFQR                                              TRO 1970
  302   CONTINUE                                                        TRO 1980
        SROWV = STRBEZ(I)                                               TRO 1990
        ROWSTR = 0.                                                     TRO 2000
        DO 301 N=2,NM1                                                  TRO 2010
          SROW = SROW + MZ(I,N)                                         TRO 2020
          ASR = ABS(SROW)                                               TRO 2030
          IF(ASR .GT. ROWSTR) ROWSTR = ASR                              TRO 2040
  301   CONTINUE                                                        TRO 2050
        IF(ROWSTR .GT. SMAX) SMAX = ROWSTR                              TRO 2060
        DIFROW = ABS((SROW-SROWV)/SMAXA)                                TRO 2070
        IF(DIFROW .LE. SROWM) GOTO 300                                  TRO 2080
        SROWM = DIFROW                                                  TRO 2090
        IRA = I                                                         TRO 2100
  300 CONTINUE                                                          TRO 2110
      SDIFF = ABS(SMAXA-SMAX)                                           TRO 2120
      TDIFF = SDIFF / SMAX                                              TRO 2130
      SDIFF = SDIFF / SMAXA                                             TRO 2140
      SMAXA = SMAX                                                      TRO 2150
      IF(IPRINT .NE. 2) GOTO 303                                        TRO 2160
      IF(IT2 .EQ. 1) WRITE (6,2999)                                     TRO 2170
      WRITE (6,3000) IT2,NITA,SMAX,SDIFF,TDIFF,SROWM,IRA                TRO 2180
  303 NITA = 1                                                          TRO 2190
      NITERM = 1                                                        TRO 2200
      GOTO(555,501,502,503,504,505,506,507,508,509,510,511,512,513),IFQ1TRO 2210
  501 IF(SROWM .LT. ZWEPSI) NITERM = MAXIT                              TRO 2220
      GOTO 555                                                          TRO 2230
  502 IF(SROWM .LT. FUEPSI) NITERM = MAXIT                              TRO 2240
      GOTO 555                                                          TRO 2250
  503 IF(SROWM .LT. ZEEPSI) NITERM = MAXIT                              TRO 2260
      GOTO 555                                                          TRO 2270
  504 IF(SROWM .LT. ZZEPSI) NITERM = MAXIT                              TRO 2280
      GOTO 555                                                          TRO 2290
  505 IF(SROWM .LT. FZEPSI) NITERM = MAXIT                              TRO 2300
      GOTO 555                                                          TRO 2310
  506 IF(SROWM .GT. ZWEPSI) NITERM = MAXIT                              TRO 2320
      GOTO 555                                                          TRO 2330
  507 IF(SROWM .GT. FUEPSI) NITERM = MAXIT                              TRO 2340
      GOTO 555                                                          TRO 2350
  508 IF(SROWM .GT. ZEEPSI) NITERM = MAXIT                              TRO 2360
      GOTO 555                                                          TRO 2370
  509 IF(SROWM .GT. ZZEPSI) NITERM = MAXIT                              TRO 2380
      GOTO 555                                                          TRO 2390
  510 IF(SROWM .GT. FZEPSI) NITERM = MAXIT                              TRO 2400
      GOTO 555                                                          TRO 2410
  511 NITERM = MAXIT                                                    TRO 2420
      GOTO 555                                                          TRO 2430
  512 IF(SROWM .LT. FZEPSI .AND. SROWM .GT. ZWEPSI) NITERM = MAXIT      TRO 2440
      GOTO 555                                                          TRO 2450
  513 IF(SROWM .LT. ZZEPSI .AND. SROWM .GT. FUEPSI) NITERM = MAXIT      TRO 2460
  555 CONTINUE                                                          TRO 2470
      IF(IFEP .NE. 4) GOTO 115                                          TRO 2480
      DO 116 I=2,IM1                                                    TRO 2490
        DO 116 N=2,NM1                                                  TRO 2500
          STROM(I,N) = P(I,N)                                           TRO 2510
  116 CONTINUE                                                          TRO 2520
  115 CONTINUE                                                          TRO 2530
      IFEP = IFEP + 1                                                   TRO 2540
      IF(IFEP .LT. 10 .OR. IT2 .LT. 30) GOTO 117                        TRO 2550
      DO 118 I=2,IM1                                                    TRO 2560
        DO 118 N=2,NM1                                                  TRO 2570
          IF(IFB(I,N) .EQ. 0) GOTO 118                                  TRO 2580
          P(I,N) = P(I,N) + (P(I,N)-STROM(I,N)) * OVM1                  TRO 2590
  118 CONTINUE                                                          TRO 2600
      IFEP = 0                                                          TRO 2610
  117 CONTINUE                                                          TRO 2620
      IFXKK = 1                                                         TRO 2630
      IT = 100                                                          TRO 2640
      IF(SROWM .LT. EPSI2 .AND. SROWM .GE. 0. .AND. IFGO1 .EQ. 0) GOTO  TRO 2650
     1 103                                                              TRO 2660
      IF(IT2 .LT. ITM2) GOTO 200                                        TRO 2670
  103 CONTINUE                                                          TRO 2680
      IF(IT2 .LE. 3) NITERM = 1                                         TRO 2690
      IF(IT2 .LE. 3) GOTO 200                                           TRO 2700
      IF(SDIFF .GT. 1. .OR. TDIFF .GT. 1. .AND. IT2 .LT. ITM2) GOTO 200 TRO 2710
C                                                                       TRO 2720
      CALL WATCH(ENDE)                                                  TRO 2730
C                                                                       TRO 2740
      ZRS = ENDE-START                                                  TRO 2750
      DO 250 I=1,IM                                                     TRO 2760
        DO 250 N=1,NM                                                   TRO 2770
          STROM(I,N) = 0.                                               TRO 2780
  250 CONTINUE                                                          TRO 2790
      DO 257 I=2,IM1                                                    TRO 2800
        STROM(I,2) = -MZ(I,2) / 2.                                      TRO 2810
        DO 257 N=3,NM                                                   TRO 2820
          STROM(I,N) = STROM(I,N-1) - (MZ(I,N-1)+MZ(I,N)) / 2.          TRO 2830
  257 CONTINUE                                                          TRO 2840
      IF(IT2 .GE. ITM2) IFGS = 1                                        TRO 2850
      RETURN                                                            TRO 2860
      END                                                               TRO 2870
      SUBROUTINE VORHOL(KOM,FZQ,DZ,IPAR,JPAR,NPAR)                      VOR   10
C                                                                       VOR   20
C     ERSTELLUNG DES COMMONS /HOHLR/                                    VOR   30
C                                                                       VOR   40
      COMMON /HOHLR/ KPAR(10),KKOM(10),NHLR,PHOHL(10),NML(10),NMR(10),  VOR   50
     1 IL(10),VOL(10)                                                   VOR   60
C                                                                       VOR   70
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      VOR   80
C                                                                       VOR   90
CFZJ055                                                       25.09.07  VOR  100
C                                                                       VOR  110
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             VOR  120
C                                                                       VOR  130
      DIMENSION KOM(KONN,KONI),FZQ(KONI),DZ(KONN),IPAR(KONI*2,10),      VOR  140
     1 JPAR(KONI*2,10),NPAR(KONI*2,10)                                  VOR  150
C                                                                       VOR  160
   41 FORMAT (' **ERROR** COMPOSITION NO.',I2,' (VOID AREA NO.',I2,') ISVOR  170
     1 IN COLUMN',I3,' NOT OK. ==> PARTIAL VOID AREA IS IMPOSSIBLE')    VOR  180
  500 FORMAT (' **FATAL ERROR** MORE THAN 10 VOID AREAS')               VOR  190
  510 FORMAT (' **ERROR** MORE THAN',I3,' ATTACHED PARTNERS  (SR VORHOL)VOR  200
     1 ')                                                               VOR  210
C                                                                       VOR  220
C                                                                       VOR  230
      IF(NHLR .LE. 10) GOTO 605                                         VOR  240
      WRITE (6,500)                                                     VOR  250
C                                                                       VOR  260
      CALL ABEND(5)                                                     VOR  270
C                                                                       VOR  280
  605 CONTINUE                                                          VOR  290
      DO 300 J=1,NHLR                                                   VOR  300
        IFR = 0                                                         VOR  310
        NMR(J) = 2                                                      VOR  320
        NML(J) = NM1                                                    VOR  330
        KK = KKOM(J)                                                    VOR  340
        DO 300 I=2,IM1                                                  VOR  350
          IFH = 0                                                       VOR  360
          DO 100 N=2,NM1                                                VOR  370
            K1 = KOM(I,N)                                               VOR  380
            IF(K1 .NE. KK) GOTO 100                                     VOR  390
            IL(J) = I                                                   VOR  400
            IFH = 1                                                     VOR  410
            IF(N .NE. 2) GOTO 20                                        VOR  420
            NML(J) = 2                                                  VOR  430
            GOTO 100                                                    VOR  440
   20       IF(N .NE. NM1) GOTO 30                                      VOR  450
            NMR(J) = NM1                                                VOR  460
            IFR = 1                                                     VOR  470
            GOTO 100                                                    VOR  480
   30       K2 = KOM(I,N+1)                                             VOR  490
            K3 = KOM(I,N-1)                                             VOR  500
            IF(K3 .EQ. KK) GOTO 40                                      VOR  510
            NML(J) = N                                                  VOR  520
            IF(IFR .EQ. 0) GOTO 40                                      VOR  530
            WRITE (6,41) KK,J,N                                         VOR  540
C                                                                       VOR  550
            CALL ABEND(3)                                               VOR  560
C                                                                       VOR  570
   40       IF(K2 .EQ. KK) GOTO 100                                     VOR  580
            NMR(J) = N                                                  VOR  590
  100     CONTINUE                                                      VOR  600
          IF(IFH .EQ. 0) GOTO 300                                       VOR  610
          KPAR(J) = (NMR(J)-NML(J)+1) * 2 + 2                           VOR  620
          NN = 1                                                        VOR  630
          IPAR(1,J) = I                                                 VOR  640
          NPAR(1,J) = NML(J) - 1                                        VOR  650
          JPAR(1,J) = 1                                                 VOR  660
          NL = NML(J)                                                   VOR  670
          NR = NMR(J)                                                   VOR  680
          VOL(J) = 0.                                                   VOR  690
          DO 200 N=NL,NR                                                VOR  700
            NN = NN + 1                                                 VOR  710
            IPAR(NN,J) = I - 1                                          VOR  720
            NPAR(NN,J) = N                                              VOR  730
            JPAR(NN,J) = 2                                              VOR  740
            NN = NN + 1                                                 VOR  750
            IPAR(NN,J) = I + 1                                          VOR  760
            NPAR(NN,J) = N                                              VOR  770
            JPAR(NN,J) = 4                                              VOR  780
            VOL(J) = VOL(J) + FZQ(N) * DZ(I)                            VOR  790
  200     CONTINUE                                                      VOR  800
          NN = NN + 1                                                   VOR  810
          IPAR(NN,J) = I                                                VOR  820
          NPAR(NN,J) = NR + 1                                           VOR  830
          JPAR(NN,J) = 3                                                VOR  840
  300 CONTINUE                                                          VOR  850
      IF(NN .LE. KONI*2) RETURN                                         VOR  860
      WRITE (6,510) KONI*2                                              VOR  870
C                                                                       VOR  880
      CALL ABEND(3)                                                     VOR  890
C                                                                       VOR  900
      RETURN                                                            VOR  910
      END                                                               VOR  920
      SUBROUTINE VORKUL(IOKUL,IUKUL,XKUL,KOM,IFBR,DZ)                   ORK   10
C                                                                       ORK   20
C     ERSTELLUNG DES COMMONS /KUEL1/                                    ORK   30
C                                                                       ORK   40
      COMMON /KUEL1/ NKUEL,KKOMK(4)                                     ORK   50
C                                                                       ORK   60
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      ORK   70
C                                                                       ORK   80
CFZJ055                                                       25.09.07  ORK   90
C                                                                       ORK  100
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             ORK  110
C                                                                       ORK  120
      DIMENSION IOKUL(KONI,4),IUKUL(KONI,4),XKUL(KONI,4),KOM(KONN,KONI),ORK  130
     1 IFBR(KOMAX),DZ(KONN)                                             ORK  140
C                                                                       ORK  150
   10 FORMAT (' **FATAL ERROR** MORE THAN 4 TUBULAR RADIATORS , NKUEL = ORK  160
     1',I2)                                                             ORK  170
   20 FORMAT (' **ERROR** IN TUBULAR RADIATOR (REG. ',I2,' ) IS PARTIAL ORK  180
     1NO FLOW THROUGH')                                                 ORK  190
   30 FORMAT (' **ERROR**  AT IO = ',I3,' **N = ',I3,' ** NV = ',I3,' **ORK  200
     1 K = ',I3)                                                        ORK  210
   40 FORMAT (' **ERROR**  AT IU = ',I3,' **N = ',I3,' ** NV = ',I3,' **ORK  220
     1 K = ',I3)                                                        ORK  230
   50 FORMAT (' **ERROR**  AT  I = ',I3,' **N = ',I3,' ** NV = ',I3,' **ORK  240
     1 DZ = ',E10.3)                                                    ORK  250
   60 FORMAT (' **ERROR**  REGION',I3,' IS IN COLUMN',I3,' NOT OK')     ORK  260
C                                                                       ORK  270
C                                                                       ORK  280
      IF(NKUEL .LE. 4) GOTO 805                                         ORK  290
      WRITE (6,10) NKUEL                                                ORK  300
C                                                                       ORK  310
      CALL ABEND(5)                                                     ORK  320
C                                                                       ORK  330
  805 IM2 = IM1 - 1                                                     ORK  340
      DO 100 N=2,NM1                                                    ORK  350
        DO 100 NK=1,NKUEL                                               ORK  360
          NIO = 0                                                       ORK  370
          NIU = 0                                                       ORK  380
          IOKUL(N,NK) = 0                                               ORK  390
          IUKUL(N,NK) = 0                                               ORK  400
          K = KKOMK(NK)                                                 ORK  410
          IFKSP = 0                                                     ORK  420
          DO 200 I=3,IM2                                                ORK  430
            K2 = KOM(I,N)                                               ORK  440
            IF(K2 .NE. K) GOTO 200                                      ORK  450
            IFKSP = 1                                                   ORK  460
            K1 = KOM(I-1,N)                                             ORK  470
            K3 = KOM(I+1,N)                                             ORK  480
            IF(K1 .EQ. K2) GOTO 1000                                    ORK  490
            IF(IFBR(K1) .NE. 0) GOTO 1100                               ORK  500
            WRITE (6,20) K2                                             ORK  510
C                                                                       ORK  520
            CALL ABEND(3)                                               ORK  530
C                                                                       ORK  540
            GOTO 1000                                                   ORK  550
 1100       NIO = NIO + 1                                               ORK  560
            IOKUL(N,NK) = I                                             ORK  570
 1000       IF(K3 .EQ. K2)  GOTO 200                                    ORK  580
            IF(IFBR(K3) .NE. 0) GOTO 1200                               ORK  590
            WRITE (6,20) K2                                             ORK  600
C                                                                       ORK  610
            CALL ABEND(3)                                               ORK  620
C                                                                       ORK  630
            GOTO 200                                                    ORK  640
 1200       CONTINUE                                                    ORK  650
            NIU = NIU + 1                                               ORK  660
            IUKUL(N,NK) = I                                             ORK  670
  200     CONTINUE                                                      ORK  680
          IF(IFKSP .EQ. 0) GOTO 100                                     ORK  690
          XL = 0.                                                       ORK  700
          IO = IOKUL(N,NK)                                              ORK  710
          IU = IUKUL(N,NK)                                              ORK  720
          IF(IO .GE. 2 .AND. IO .LE. IM1) GOTO 901                      ORK  730
          WRITE (6,30) IO,N,NK,K                                        ORK  740
C                                                                       ORK  750
          CALL ABEND(3)                                                 ORK  760
C                                                                       ORK  770
  901     IF(IU .GE. 2 .AND. IU .LE. IM1) GOTO 908                      ORK  780
          WRITE (6,40) IU,N,NK,K                                        ORK  790
C                                                                       ORK  800
          CALL ABEND(3)                                                 ORK  810
C                                                                       ORK  820
  908     CONTINUE                                                      ORK  830
          DO 300 I=IO,IU                                                ORK  840
            IF(DZ(I). GE. 1.E-03) GOTO 909                              ORK  850
            NV = 0                                                      ORK  860
            WRITE (6,50) I,N,NV,DZ(I)                                   ORK  870
C                                                                       ORK  880
            CALL ABEND(3)                                               ORK  890
C                                                                       ORK  900
  909       XL = XL + DZ(I)                                             ORK  910
  300     CONTINUE                                                      ORK  920
          XKUL(N,NK) = XL                                               ORK  930
          IF(NIO .EQ. 1 .AND. NIU .EQ. 1) GOTO 100                      ORK  940
          WRITE (6,60) K2,N                                             ORK  950
C                                                                       ORK  960
          CALL ABEND(3)                                                 ORK  970
C                                                                       ORK  980
  100 CONTINUE                                                          ORK  990
      RETURN                                                            ORK 1000
      END                                                               ORK 1010
      SUBROUTINE VORP(ROGG,TFL,RHO,IFB,DZ,MZ,MR,XKR,XKZ,KOM,STROM,SUMRG)ORP   10
C                                                                       ORP   20
C     BERECHNET STATISCHE DRUCKSAEULE UND SETZT HOHEN STROEMUNGS-       ORP   30
C     WIDERSTAND AN RAENDERN UND IN EINDIMENSIONALEN STROMROEHREN       ORP   40
C     QUER ZUR STROEMUNGSRICHTUNG.                                      ORP   50
C                                                                       ORP   60
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      ORP   70
C                                                                       ORP   80
      COMMON /VERTI/ NVERTI,KKOMV(30)                                   ORP   90
C                                                                       ORP  100
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             ORP  110
C                                                                       ORP  120
      COMMON /UPERR/ OERR                                               ORP  130
C                                                                       ORP  140
      DIMENSION ROGG(KONN,KONI),TFL(KONN,KONI),RHO(KONN,KONI),          ORP  150
     1 IFB(KONN,KONI),DZ(KONN),XKR(KONN,KONI),XKZ(KONN,KONI),           ORP  160
     2 KOM(KONN,KONI),STROM(KONN,KONI),SUMRG(KONI,30)                   ORP  170
C                                                                       ORP  180
      REAL MZ(KONN,KONI),MR(KONN,KONI)                                  ORP  190
C                                                                       ORP  200
C                                                                       ORP  210
      IM = IM1 + 1                                                      ORP  220
      NM = NM1 + 1                                                      ORP  230
      DO 100 N=1,NM                                                     ORP  240
        DO 101 NV=1,NVERTI                                              ORP  250
          SUMRG(N,NV) = 0.                                              ORP  260
  101   CONTINUE                                                        ORP  270
        DO 100 I=1,IM                                                   ORP  280
          RHO(I,N) = 0.                                                 ORP  290
          ROGG(I,N) = 0.                                                ORP  300
          STROM(I,N) = 0.                                               ORP  310
          MZ(I,N) = 0.                                                  ORP  320
          XKR(I,N) = 0.                                                 ORP  330
          XKZ(I,N) = 0.                                                 ORP  340
          MR(I,N) = 0.                                                  ORP  350
          NIFB = IFB(I,N) + 1                                           ORP  360
          GOTO(110,100,110,110,100,100),NIFB                            ORP  370
          GOTO 100                                                      ORP  380
  110     IF(N .NE. 1) XKR(I,N-1) = OERR                                ORP  390
          XKR(I,N) = OERR                                               ORP  400
          IF(NIFB .EQ. 3 .OR. NIFB .EQ. 4) GOTO 100                     ORP  410
          IF(I .NE. 1) XKZ(I-1,N) = OERR                                ORP  420
          XKZ(I,N) = OERR                                               ORP  430
  100 CONTINUE                                                          ORP  440
      DO 160 I=2,IM1                                                    ORP  450
        DO 160 N=2,NM1                                                  ORP  460
          TEM = (TFL(I,N)+TFL(I-1,N)+TFL(I,N-1)+TFL(I-1,N-1)) / 4.      ORP  470
C                                                                       ORP  480
          RHO(I,N) = DICHTE(DRUCK,TEM)                                  ORP  490
C                                                                       ORP  500
  160 CONTINUE                                                          ORP  510
      IM2 = IM1 - 1                                                     ORP  520
      DO 200 I=2,IM2                                                    ORP  530
        DO 200 N=2,NM1                                                  ORP  540
          ROGG(I,N) = (RHO(I,N)*DZ(I)+RHO(I+1,N)*DZ(I+1)) * 9.81 / 2.   ORP  550
          IF(IFB(I,N) .NE. 2) GOTO 200                                  ORP  560
          KO = KOM(I,N)                                                 ORP  570
          DO 201 NV=1,NVERTI                                            ORP  580
            KO1 = KKOMV(NV)                                             ORP  590
            IF(KO .NE. KO1) GOTO 201                                    ORP  600
            SUMRG(N,NV) = SUMRG(N,NV) + ROGG(I,N)                       ORP  610
            GOTO 200                                                    ORP  620
  201     CONTINUE                                                      ORP  630
  200 CONTINUE                                                          ORP  640
      RETURN                                                            ORP  650
      END                                                               ORP  660
      SUBROUTINE VOLSTZ(VOL,STZUK)                                      OLS   10
C                                                                       OLS   20
C     BERECHNET MASSENSTROMQUELLE AUS QUELLDICHTE UND ZUGEHOERIGEM      OLS   30
C     VOLUMEN                                                           OLS   40
C                                                                       OLS   50
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      OLS   60
C                                                                       OLS   70
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             OLS   80
C                                                                       OLS   90
      DIMENSION VOL(KOMAX),STZUK(KOMAX)                                 OLS  100
C                                                                       OLS  110
C                                                                       OLS  120
      DO 10 KK=1,KM                                                     OLS  130
        STZUK(KK) = STZUK(KK) * VOL(KK)                                 OLS  140
   10 CONTINUE                                                          OLS  150
      RETURN                                                            OLS  160
      END                                                               OLS  170
      SUBROUTINE VORVER(IOVER,IUVER,KOM,IFBR,DZ,XVER)                   ORV   10
C                                                                       ORV   20
C     ERSTELLUNG DES COMMON /VERTI/                                     ORV   30
C                                                                       ORV   40
      COMMON /VERTI/ NVERTI,KKOMV(30)                                   ORV   50
C                                                                       ORV   60
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      ORV   70
C                                                                       ORV   80
CFZJ055                                                       25.09.07  ORV   90
C                                                                       ORV  100
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             ORV  110
C                                                                       ORV  120
      DIMENSION IOVER(KONI,30),IUVER(KONI,30),KOM(KONN,KONI),IFBR(KOMAX)ORV  130
     1 ,DZ(KONN),XVER(KONI,30)                                          ORV  140
C                                                                       ORV  150
   10 FORMAT (' **FATAL ERROR** MORE THAN 8 REGIONS WITH VERTICAL PIPES'ORV  160
     1 )                                                                ORV  170
   20 FORMAT (' **ERROR**  PART OF REGION',I3,' IS NO FLOW THROUGH ***')ORV  180
   30 FORMAT (' **ERROR** REGION',I3,' IS IN COLUMN',I3,' NOT OK. SHE ISORV  190
     1 USED',I3,' OR',I3,'-TIMES. (SEE SUBR. VORVER)')                  ORV  200
 1000 FORMAT ( ' **ERROR**  AT IO = ',I3,' **N = ',I3,' ** NV = ',I3,' *ORV  210
     1* K = ',I3)                                                       ORV  220
 2000 FORMAT ( ' **ERROR**  AT IU = ',I3,' **N = ',I3,' ** NV = ',I3)   ORV  230
 4000 FORMAT ( ' **ERROR**  AT  I = ',I3,' **N = ',I3,' ** NV = ',I3,' *ORV  240
     1* DZ = ',E10.3)                                                   ORV  250
C                                                                       ORV  260
C                                                                       ORV  270
      IF(NVERTI .LE. 30) GOTO 805                                       ORV  280
      WRITE (6,10)                                                      ORV  290
C                                                                       ORV  300
      CALL ABEND(5)                                                     ORV  310
C                                                                       ORV  320
  805 IM2 = IM1 - 1                                                     ORV  330
      DO 300 N=2,NM1                                                    ORV  340
        DO 300 NV=1,NVERTI                                              ORV  350
          NIO = 0                                                       ORV  360
          NIU = 0                                                       ORV  370
          IOVER(N,NV) = 0                                               ORV  380
          IUVER(N,NV) = 0                                               ORV  390
          K = KKOMV(NV)                                                 ORV  400
          IFKSP = 0                                                     ORV  410
          DO 200 I=3,IM2                                                ORV  420
            K2 = KOM(I,N)                                               ORV  430
            IF(K2 .NE. K) GOTO 200                                      ORV  440
            K4 = K2                                                     ORV  450
            IFKSP = 1                                                   ORV  460
            K1 = KOM(I-1,N)                                             ORV  470
            K3 = KOM(I+1,N)                                             ORV  480
            IF(K1 .EQ. K2) GOTO 100                                     ORV  490
            IF(IFBR(K1) .NE. 0) GOTO 50                                 ORV  500
            WRITE (6,20) K2                                             ORV  510
C                                                                       ORV  520
            CALL ABEND(3)                                               ORV  530
C                                                                       ORV  540
            GOTO 100                                                    ORV  550
   50       NIO = NIO + 1                                               ORV  560
            IOVER(N,NV) = I                                             ORV  570
  100       IF(K3 .EQ. K2) GOTO 200                                     ORV  580
            IF(IFBR(K3) .NE. 0) GOTO 150                                ORV  590
            WRITE (6,20) K2                                             ORV  600
C                                                                       ORV  610
            CALL ABEND(3)                                               ORV  620
C                                                                       ORV  630
            GOTO 200                                                    ORV  640
  150       CONTINUE                                                    ORV  650
            NIU = NIU + 1                                               ORV  660
            IUVER(N,NV) = I                                             ORV  670
  200     CONTINUE                                                      ORV  680
          IF(IFKSP .EQ. 0) GOTO 300                                     ORV  690
          XL = 0.                                                       ORV  700
          IO = IOVER(N,NV)                                              ORV  710
          IU = IUVER(N,NV)                                              ORV  720
          IF(IO .GE. 2 .AND. IO .LE. IM1) GOTO 901                      ORV  730
          WRITE (6,1000) IO,N,NV,K                                      ORV  740
C                                                                       ORV  750
          CALL ABEND(3)                                                 ORV  760
C                                                                       ORV  770
  901     IF(IU .GE. 2 .AND. IU .LE. IM1) GOTO 902                      ORV  780
          WRITE (6,2000) IU,N,NV                                        ORV  790
C                                                                       ORV  800
          CALL ABEND(3)                                                 ORV  810
C                                                                       ORV  820
  902     CONTINUE                                                      ORV  830
          DO 250 I=IO,IU                                                ORV  840
            IF(DZ(I) .GE. 1.E-3) GOTO 251                               ORV  850
            WRITE (6,4000) I,N,NV,DZ(I)                                 ORV  860
C                                                                       ORV  870
            CALL ABEND(3)                                               ORV  880
C                                                                       ORV  890
  251       XL = XL + DZ(I)                                             ORV  900
  250     CONTINUE                                                      ORV  910
          XVER(N,NV) = XL                                               ORV  920
          IF(NIO .EQ. 1 .AND. NIU .EQ .1) GOTO 910                      ORV  930
          WRITE (6,30) K4,N,NIO,NIU                                     ORV  940
C                                                                       ORV  950
          CALL ABEND(3)                                                 ORV  960
C                                                                       ORV  970
  910     CONTINUE                                                      ORV  980
  300 CONTINUE                                                          ORV  990
      RETURN                                                            ORV 1000
      END                                                               ORV 1010
      FUNCTION XIRE(RE)                                                 XIR   10
C                                                                       XIR   20
C     WIDERSTANDSGESETZ FUER ROHRSTROEMUNG (LAM.UND TURB.).             XIR   30
C     UEBERGANGSBEREICH DURCH E-FKTN ANGEPASST.                         XIR   40
C                                                                       XIR   50
C                                                                       XIR   60
      IF(RE .GT. 3.E4) RE = 3.E4                                        XIR   70
      IF(RE .LT. 0.00001) RE = 0.00001                                  XIR   80
      AXP = -4.E-4 * RE                                                 XIR   90
C                                                                       XIR  100
      XIRE = 64. / RE + .0235 * (1.-EXP(AXP))                           XIR  110
C                                                                       XIR  120
      RETURN                                                            XIR  130
      END                                                               XIR  140
      FUNCTION XIRE1(RE,XGEO,XQT,XLT)                                   IRE   10
C                                                                       IRE   20
C     XI FUER QUERANGESTROEHMTE ROHRBUENDEL NACH VDI'77                 IRE   30
C                                                                       IRE   40
C                                                                       IRE   50
      IF(RE .GT. 3.E05) RE = 3.E05                                      IRE   60
      IF(RE .LT. 0.00001) RE = 0.00001                                  IRE   70
      XIVER = 0.                                                        IRE   80
      IF(XGEO .EQ. 0.) GOTO 201                                         IRE   90
      ARE = RE**.18                                                     IRE  100
      XIVER = 2. * (64./RE+2./ARE)                                      IRE  110
      IF(XGEO .LT. 1.) GOTO 201                                         IRE  120
C                                                                       IRE  130
      XIRE1 = XGEO * XIVER / XLT                                        IRE  140
C                                                                       IRE  150
C     XI FUER REIN VERSETZTE ANORDNUNG, VDI'77,LD3                      IRE  160
C                                                                       IRE  170
      RETURN                                                            IRE  180
  201 CONTINUE                                                          IRE  190
      XILAM = 100. / RE                                                 IRE  200
      A1 = RE**0.16                                                     IRE  210
      A2 = XLT / 1.2                                                    IRE  220
      A2 = A2**1.4                                                      IRE  230
      A3 = XQT - 1.                                                     IRE  240
      A4 = .43 + 1.13 / XLT                                             IRE  250
      A3 = A3**A4                                                       IRE  260
      XITURB = 5. / A1 * (0.044+0.08*A2/A3) + 0.25                      IRE  270
      X = XILAM / XITURB                                                IRE  280
      IF(X .LT. 1.) GOTO 202                                            IRE  290
      XIFLU = XILAM                                                     IRE  300
      GOTO 203                                                          IRE  310
  202 A = X - 0.15                                                      IRE  320
      SGN = 1.                                                          IRE  330
      IF(A .LT. 0.) SGN = -1.                                           IRE  340
      A4 = ABS(A)                                                       IRE  350
      A4 = A4**0.3333                                                   IRE  360
      G = 0.6762988 * (SGN*A4+0.5313628)                                IRE  370
      XIFLU = XITURB + G * (XILAM-XITURB)                               IRE  380
C                                                                       IRE  390
C     XI FUER FLUCHTENDE ROHRBUENDEL NACH VDI'77,DIAGR. SEITE LD2       IRE  400
C     SELBST "ANGEFERTIGTE" FORMEL TRIFFT DIES DIAGR.RECHT GUT          IRE  410
C     FUER MITTLERE TEILUNGSVERHAELTNISSE (1.3 - 2.0)                   IRE  420
C                                                                       IRE  430
  203 XIRE1 = ((1.-XGEO)*XIFLU +XGEO*XIVER) / XLT                       IRE  440
C                                                                       IRE  450
C     HIERDURCH IST UEBERLAGERUNG FUER "GEMISCHTE ANORDNUNG"            IRE  460
C     MOEGLICH   ( NUETZLICH Z.B. BEI HELIX-ANORDNUNG )                 IRE  470
C                                                                       IRE  480
      RETURN                                                            IRE  490
      END                                                               IRE  500
      SUBROUTINE XLTURB(LAMTUR,TFL,RHO,IFB,KOM,XKON,FZQ,FRQ,MZ,MR)      XLT   10
C                                                                       XLT   20
C     BERECHNUNG DER TURBULENTEN QUERLEITFAEHIGKEIT                     XLT   30
C                                                                       XLT   40
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      XLT   50
C                                                                       XLT   60
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             XLT   70
C                                                                       XLT   80
      DIMENSION TFL(KONN,KONI),RHO(KONN,KONI),IFB(KONN,KONI),           XLT   90
     1 KOM(KONN,KONI),XKON(KOMAX),FZQ(KONI),FRQ(KONN,KONI)              XLT  100
C                                                                       XLT  110
      REAL MZ(KONN,KONI),MR(KONN,KONI),LAMTUR(KONN,KONI)                XLT  120
C                                                                       XLT  130
C                                                                       XLT  140
C     STROM ENTSPRICHT HIER LAMBDA-TURB.                                XLT  150
C                                                                       XLT  160
      IM = IM1 + 1                                                      XLT  170
      NM = NM1 + 1                                                      XLT  180
      DO 100 I=1,IM                                                     XLT  190
        DO 100 N=1,NM                                                   XLT  200
          IF(IFB(I,N) .EQ. 1) GOTO 110                                  XLT  210
          LAMTUR(I,N) = 0.                                              XLT  220
          IF(IFB(I,N) .NE. 5) GOTO 100                                  XLT  230
          K = KOM(I,N)                                                  XLT  240
          TEM = (TFL(I,N)+TFL(I-1,N)+TFL(I,N-1)+TFL(I-1,N-1)) / 4.      XLT  250
C                                                                       XLT  260
          AXLH = XLHE(TEM)                                              XLT  270
C                                                                       XLT  280
          LAMTUR(I,N) = (XKON(K)+1.) * AXLH                             XLT  290
          GOTO 100                                                      XLT  300
  110     CONTINUE                                                      XLT  310
C                                                                       XLT  320
          DMC = DMCORE(N)                                               XLT  330
C                                                                       XLT  340
          A1 = 1. - 2. * DKUG / DMC                                     XLT  350
          A1 = A1**2.                                                   XLT  360
          YK = 8. * (2.-A1)                                             XLT  370
C                                                                       XLT  380
          AVAU = VAU(I,N,FZQ,FRQ,MZ,MR,RHO)                             XLT  390
C                                                                       XLT  400
          LAMTUR(I,N) = RHO(I,N) * CP * AVAU * DKUG / 100. / YK         XLT  410
  100 CONTINUE                                                          XLT  420
      RETURN                                                            XLT  430
      END                                                               XLT  440
      FUNCTION ZTPOL(ZVOR,FELD,IPSTEU,IZ,ZEITM)                         ZTP   10
C                                                                       ZTP   20
      REAL ITPL                                                         ZTP   30
C                                                                       ZTP   40
      DIMENSION ZVOR(1),FELD(1)                                         ZTP   50
C                                                                       ZTP   60
C                                                                       ZTP   70
      IF(IPSTEU .EQ. 0) GOTO 10                                         ZTP   80
      IF(IPSTEU .EQ. 1) GOTO 9                                          ZTP   90
C                                                                       ZTP  100
      ZTPOL = FELD(IPSTEU)                                              ZTP  110
C                                                                       ZTP  120
      RETURN                                                            ZTP  130
C                                                                       ZTP  140
    9 ZTPOL = FELD(1)                                                   ZTP  150
C                                                                       ZTP  160
      RETURN                                                            ZTP  170
   10 ZEIT1 = ZVOR(IZ)                                                  ZTP  180
      ZEIT2 = ZVOR(IZ+1)                                                ZTP  190
      FELD1 = FELD(IZ)                                                  ZTP  200
      FELD2 = FELD(IZ+1)                                                ZTP  210
C                                                                       ZTP  220
      ZTPOL = ITPL(ZEITM,ZEIT1,ZEIT2,FELD1,FELD2)                       ZTP  230
C                                                                       ZTP  240
      RETURN                                                            ZTP  250
      END                                                               ZTP  260
      FUNCTION IFBG(I,N,IFB)                                            IFB   10
C                                                                       IFB   20
C     BERECHNET STEUERGROESSE FUER GASTEMPERATURBERECHNUNG              IFB   30
C                                                                       IFB   40
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             IFB   50
C                                                                       IFB   60
      DIMENSION IFB(KONN,KONI)                                          IFB   70
C                                                                       IFB   80
C                                                                       IFB   90
      IFBG = IFB(I,N) + IFB(I,N+1) + IFB(I+1,N) + IFB(I+1,N+1)          IFB  100
C                                                                       IFB  110
      RETURN                                                            IFB  120
      END                                                               IFB  130
      FUNCTION VAU(I,N,FZQ,FRQ,MZ,MR,RHO)                               VAU   10
C                                                                       VAU   20
C     BERECHNET SKALARE GESCHWINDIGKEIT                                 VAU   30
C                                                                       VAU   40
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             VAU   50
C                                                                       VAU   60
      DIMENSION FZQ(KONI),FRQ(KONN,KONI),RHO(KONN,KONI)                 VAU   70
C                                                                       VAU   80
      REAL MZ(KONN,KONI),MR(KONN,KONI)                                  VAU   90
C                                                                       VAU  100
C                                                                       VAU  110
      ZMZ = 0.                                                          VAU  120
      IF(ABS(MZ(I,N)) .GT. 0. .AND. FZQ(N) .GT. 0.) ZMZ = (MZ(I,N)/     VAU  130
     1 FZQ(N))**2.                                                      VAU  140
      RMZ = 0.                                                          VAU  150
      IF(ABS(MR(I,N)) .GT. 0. .AND. FRQ(I,N) .GT. 0.) RMZ = (MR(I,N)/   VAU  160
     1 FRQ(I,N))**2.                                                    VAU  170
C                                                                       VAU  180
      VAU = SQRT(ZMZ+RMZ) / RHO(I,N)                                    VAU  190
C                                                                       VAU  200
      RETURN                                                            VAU  210
      END                                                               VAU  220
      FUNCTION XLHE(T)                                                  XLH   10
C                                                                       XLH   20
C     WAERMELEITFAEHIGKEIT HELIUM EINHEIT  W/M/K                        XLH   30
C                                                                       XLH   40
      COMMON /KONST/ EPSI,REK,PK,REVOR,DRUCK,CP,PI,NM1,IM1,KM,DKUG      XLH   50
C                                                                       XLH   60
C                                                                       XLH   70
      P = DRUCK                                                         XLH   80
      E = 0.71 * (1.-2.E-04*P)                                          XLH   90
      TK = T + 273.                                                     XLH  100
      IF(TK .LT. 1.) TK = 1.                                            XLH  110
      ATK = TK**E                                                       XLH  120
C                                                                       XLH  130
      XLHE = 2.682E-03 * (1.+1.123E-03*P) * ATK                         XLH  140
C                                                                       XLH  150
      RETURN                                                            XLH  160
      END                                                               XLH  170
      FUNCTION DICHTE(P,T)                                              DIC   10
C                                                                       DIC   20
C     DICHTE HELIUM EINHEIT KG/M3,DRUCK P IN BAR                        DIC   30
C                                                                       DIC   40
C                                                                       DIC   50
      TK = T + 273.                                                     DIC   60
      IF(TK .LT. 1.) TK = 1.                                            DIC   70
      ATK = TK**1.2                                                     DIC   80
C                                                                       DIC   90
      DICHTE = 48.14 * P / TK / (1.+0.4446*P/ATK)                       DIC  100
C                                                                       DIC  110
      RETURN                                                            DIC  120
      END                                                               DIC  130
      FUNCTION ETHAG(T)                                                 ETH   10
C                                                                       ETH   20
C     DYNAMISCHE ZAEHIGKEIT HELIUM (KG/M/SEC ODER PA*SEC )              ETH   30
C                                                                       ETH   40
C                                                                       ETH   50
      T0 = -272.                                                        ETH   60
      T = AMAX1(T,T0)                                                   ETH   70
      ETH = T + 273.                                                    ETH   80
      ETH = ETH**0.7                                                    ETH   90
C                                                                       ETH  100
      ETHAG = 3.674E-07 * ETH                                           ETH  110
C                                                                       ETH  120
      RETURN                                                            ETH  130
      END                                                               ETH  140