      SUBROUTINE SUCHET(NHET,HKUG,DI,NHZON,XFWQZ,IFHET,VOLK,VOLS,IFFUEL,SUC   10
     1 VOLZ,VK,IFBH,KKB,KOM,XFAK)                                       SUC   20
C                                                                       SUC   30
C     BESTIMMT LAGE DES HET-GITTERS UND STEUERT ZUWEISUNG DER           SUC   40
C     STEUERGROESSE IFBH                                                SUC   50
C                                                                       SUC   60
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    SUC   70
C                                                                       SUC   80
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                SUC   90
C                                                                       SUC  100
      COMMON /PRINT1/ TITLE(20),INDGEO                                  SUC  110
C                                                                       SUC  120
      COMMON /KOMP1/ KMAX                                               SUC  130
C                                                                       SUC  140
CFZJ055                                                       25.09.07  SUC  150
C                                                                       SUC  160
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             SUC  170
C                                                                       SUC  180
      DIMENSION HKUG(KMAZ),DI(KMAZ,5),NHZON(KMAZ),XFWQZ(KMAZ,5),        SUC  190
     1 IFHET(KMAZ),VOLK(KMAZ,5),VOLS(KMAZ,5),IFFUEL(KMAZ,5),VOLZ(KMAZ,5)SUC  200
     2 ,VK(KMAZ),IFBH(ICO,NCO),KKB(ICO,NCO),KOM(IMAZ,NMAZ),XFAK(KMAZ,5),SUC  210
     3 IFH(4),KK(4)                                                     SUC  220
C                                                                       SUC  230
   20 FORMAT (/' **ERROR** HET-ZONES MUST BE INSIDE A CORRELATED AREA OFSUC  240
     1 ',I2,' * ',I2,' NODES'/)                                         SUC  250
C                                                                       SUC  260
C                                                                       SUC  270
      IDIFF = 100                                                       SUC  280
      NDIFF = 100                                                       SUC  290
      IMH = 0                                                           SUC  300
      NMH = 0                                                           SUC  310
      IF(NHET .EQ. 0) RETURN                                            SUC  320
      IHM = 1                                                           SUC  330
      NHM = 1                                                           SUC  340
      IM1 = IMAX - 1                                                    SUC  350
      NM1 = NMAX - 1                                                    SUC  360
      DO 100 I=1,IM1                                                    SUC  370
        DO 100 N=1,NM1                                                  SUC  380
          K1 = KOM(I,N)                                                 SUC  390
          IF(K1 .EQ. 0) GOTO 100                                        SUC  400
          ITK = IFHET(K1)                                               SUC  410
          IF(ITK .EQ. 0) GOTO 100                                       SUC  420
          ID = I                                                        SUC  430
          ND = N                                                        SUC  440
          IF(ID .LT. IDIFF) IDIFF = ID                                  SUC  450
          IF(ND .LT. NDIFF) NDIFF = ND                                  SUC  460
          IF(ID .GT. IHM) IHM = ID                                      SUC  470
          IF(ND .GT. NHM) NHM = ND                                      SUC  480
  100 CONTINUE                                                          SUC  490
      IDIFF = IDIFF - 1                                                 SUC  500
      NDIFF = NDIFF - 1                                                 SUC  510
      IMH = IHM - IDIFF + 1                                             SUC  520
      NMH = NHM - NDIFF + 1                                             SUC  530
      IF(IMH .LE. ICO .AND. NMH .LE. NCO) GOTO 2020                     SUC  540
      WRITE (6,20) ICO,NCO                                              SUC  550
C                                                                       SUC  560
      CALL ABEND(3)                                                     SUC  570
C                                                                       SUC  580
 2020 DO 200 I=1,IMH                                                    SUC  590
        DO 200 N=1,NMH                                                  SUC  600
          IFBH(I,N) = 0                                                 SUC  610
          KKB(I,N) = 0                                                  SUC  620
          IG = I + IDIFF                                                SUC  630
          NG = N + NDIFF                                                SUC  640
          I1 = IG - 1                                                   SUC  650
          I2 = IG + 1                                                   SUC  660
          N1 = NG - 1                                                   SUC  670
          N2 = NG + 1                                                   SUC  680
          IF(IG .EQ. 1 .AND. INDGEO .EQ. 2 .AND. RAD0 .EQ. 0.) GOTO 203 SUC  690
          IF(I1 .EQ. 0 .OR. I2 .GT. IMAX .OR. N1 .EQ. 0 .OR. N2 .GT.    SUC  700
     1     NMAX) GOTO 200                                               SUC  710
C                                                                       SUC  720
C     AM GITTERRAND KEINE HETEROGENE BERECHNUNG                         SUC  730
C                                                                       SUC  740
          KK(1) = KOM(I1,N1)                                            SUC  750
          KK(2) = KOM(IG,N1)                                            SUC  760
          KK(3) = KOM(I1,NG)                                            SUC  770
          KK(4) = KOM(IG,NG)                                            SUC  780
          IFH(1) = IFHET(KK(1))                                         SUC  790
          IFH(2) = IFHET(KK(2))                                         SUC  800
          IFH(3) = IFHET(KK(3))                                         SUC  810
          IFH(4) = IFHET(KK(4))                                         SUC  820
          GOTO 204                                                      SUC  830
  203     KK(1) = 0                                                     SUC  840
          KK(2) = KOM(IG,N1)                                            SUC  850
          KK(3) = 0                                                     SUC  860
          KK(4) = KOM(IG,NG)                                            SUC  870
          IFH(1) = 0                                                    SUC  880
          IFH(2) = IFHET(KK(2))                                         SUC  890
          IFH(3) = 0                                                    SUC  900
          IFH(4) = IFHET(KK(4))                                         SUC  910
C                                                                       SUC  920
  204     CALL SUCHMI(IFH,IF2,KK,I,N,KKB)                               SUC  930
C                                                                       SUC  940
          IFBH(I,N) = IF2                                               SUC  950
  200 CONTINUE                                                          SUC  960
      DO 300 K=1,KMAX                                                   SUC  970
        IF(IFHET(K) .EQ. 0) GOTO 300                                    SUC  980
C                                                                       SUC  990
C     BERECHNUNG DER KUGELGEOMETRIE                                     SUC 1000
C                                                                       SUC 1010
        NZ = NHZON(K)                                                   SUC 1020
        DIZ1 = HKUG(K)                                                  SUC 1030
        NZM = NZ - 1                                                    SUC 1040
        DO 400 N=1,NZM                                                  SUC 1050
          DIZ = DI(K,N)                                                 SUC 1060
          DBARIT = 0.5 * (DIZ1+DIZ)                                     SUC 1070
          DBGEOM = SQRT(DIZ1*DIZ)                                       SUC 1080
          DB = 0.3 * DBARIT + 0.7 * DBGEOM                              SUC 1090
C                                                                       SUC 1100
C     GRENZEN DER BILANZVOLUMINA (REIN EMPIRISCH)                       SUC 1110
C                                                                       SUC 1120
          DIZ3 = DIZ1**3.                                               SUC 1130
          DB3 = DB**3.                                                  SUC 1140
          VOLK(K,N) = 0.5236 * (DIZ3-DB3)                               SUC 1150
          DI3 = DIZ**3.                                                 SUC 1160
          VOLZ(K,N) = 0.5236 * (DB3-DI3)                                SUC 1170
C                                                                       SUC 1180
C     ZUGEORDNETE BILANZVOLUMEN-ANTEILE                                 SUC 1190
C                                                                       SUC 1200
          VOLS(K,N) = VOLK(K,N) + VOLZ(K,N)                             SUC 1210
C                                                                       SUC 1220
C     SCHALENVOLUMEN WIRD ABGESPEICHERT ZUR ERMITTLUNG VON              SUC 1230
C     MODERATOR- UND BRENNSTOFF-TEMPERATUR (TPROZ)                      SUC 1240
C                                                                       SUC 1250
          DIZ1 = DI(K,N)                                                SUC 1260
  400   CONTINUE                                                        SUC 1270
        VOLK(K,NZ) = .5236 * DI3                                        SUC 1280
        VOLS(K,NZ) = VOLK(K,NZ)                                         SUC 1290
        HK3 = HKUG(K)**3.                                               SUC 1300
        VK(K) = .5236 * HK3                                             SUC 1310
        IFFUEL(K,1) = 0                                                 SUC 1320
        IF(XFWQZ(K,1) .GT. 0.) IFFUEL(K,1) = 1                          SUC 1330
C                                                                       SUC 1340
C     LEISTUNGSANTEIL IM ZUGEORDNETEN BILANZVOLUMEN                     SUC 1350
C                                                                       SUC 1360
        XFAK(K,1) = XFWQZ(K,1) * VOLK(K,1)                              SUC 1370
        DO 600 N=2,NZ                                                   SUC 1380
          IFFUEL(K,N) = 0                                               SUC 1390
          IF(XFWQZ(K,N) .GT. 0.) IFFUEL(K,N) = 1                        SUC 1400
          XFAK(K,N) = XFWQZ(K,N-1) * VOLZ(K,N-1) + XFWQZ(K,N) *         SUC 1410
     1     VOLK(K,N)                                                    SUC 1420
  600   CONTINUE                                                        SUC 1430
        XFNORM = 0.                                                     SUC 1440
        DO 700 N=1,NZ                                                   SUC 1450
          XFNORM = XFNORM + XFAK(K,N)                                   SUC 1460
  700   CONTINUE                                                        SUC 1470
        IF(XFNORM .EQ. 0.) XFNORM = 1.                                  SUC 1480
        DO 800 N=1,NZ                                                   SUC 1490
          XFWQZ(K,N) = XFAK(K,N) / XFNORM                               SUC 1500
  800   CONTINUE                                                        SUC 1510
  300 CONTINUE                                                          SUC 1520
      RETURN                                                            SUC 1530
      END                                                               SUC 1540
      SUBROUTINE SUCHMI(IF1,IF2,KK,I,N,KKB)                             UCH   10
C                                                                       UCH   20
C     SETZT DIE STEUERGROESSE IFBH FUER MISCHMASCHEN                    UCH   30
C                                                                       UCH   40
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                UCH   50
C                                                                       UCH   60
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             UCH   70
C                                                                       UCH   80
      DIMENSION KKB(ICO,NCO),IF1(4),KK(4)                               UCH   90
C                                                                       UCH  100
C                                                                       UCH  110
      IF2 = 0                                                           UCH  120
      IFS = 0                                                           UCH  130
      DO 10 L=1,4                                                       UCH  140
        IFS = IFS + IF1(L)                                              UCH  150
   10 CONTINUE                                                          UCH  160
      IF(IFS .NE. 0) GOTO 30                                            UCH  170
      IF2 = 0                                                           UCH  180
      KKB(I,N) = 0                                                      UCH  190
      RETURN                                                            UCH  200
   30 IF(IFS .NE. 8) GOTO 50                                            UCH  210
      IF2 = 1                                                           UCH  220
      KKB(I,N) = KK(4)                                                  UCH  230
      RETURN                                                            UCH  240
   50 IF(IF1(1) .EQ. 0) IF2 = IF2 + 2                                   UCH  250
      IF(IF1(2) .EQ. 0) IF2 = IF2 + 3                                   UCH  260
      IF(IF1(3) .EQ. 0) IF2 = IF2 + 4                                   UCH  270
      IF(IF1(4). EQ. 0) IF2 = IF2 + 8                                   UCH  280
      IF(IF2 .NE. 2 .AND. IF2 .NE. 5 .AND. IF2 .NE. 6 .AND. IF2 .NE. 9  UCH  290
     1 .AND. IF2 .NE. 10 .AND. IF2 .NE. 13 .AND. IF2 .NE. 14) KKB(I,N) =UCH  300
     2 KK(1)                                                            UCH  310
      IF(IF2 .NE. 3 .AND. IF2 .NE. 5 .AND. IF2 .NE. 7 .AND. IF2 .NE. 9  UCH  320
     1 .AND. IF2 .NE. 11 .AND. IF2 .NE. 13 .AND. IF2 .NE. 15) KKB(I,N) =UCH  330
     2 KK(2)                                                            UCH  340
      IF(IF2 .NE. 4 .AND. IF2 .NE. 6 .AND. IF2 .NE. 7 .AND. IF2 .NE. 9  UCH  350
     1 .AND. IF2 .NE. 12 .AND. IF2 .NE. 14 .AND. IF2 .NE. 15) KKB(I,N) =UCH  360
     2 KK(3)                                                            UCH  370
      IF(IF2 .NE. 8 .AND. IF2 .NE. 10 .AND. IF2 .NE. 11 .AND. IF2 .NE.  UCH  380
     1 12 .AND. IF2 .NE. 13 .AND. IF2 .NE. 14 .AND. IF2 .NE. 15)        UCH  390
     2 KKB(I,N) = KK(4)                                                 UCH  400
      RETURN                                                            UCH  410
      END                                                               UCH  420
      SUBROUTINE SYMBOL(EPS,WI,WL,IFBER,IFLT,KOM,KART)                  SYM   10
C                                                                       SYM   20
C     SETZT DIE STEUERGROESSE IFBER                                     SYM   30
C                                                                       SYM   40
      COMMON /PRINT1/ TITLE(20),INDGEO                                  SYM   50
C                                                                       SYM   60
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    SYM   70
C                                                                       SYM   80
CFZJ055                                                       25.09.07  SYM   90
C                                                                       SYM  100
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             SYM  110
C                                                                       SYM  120
      DIMENSION EPS(KMAZ),WI(IMAZ,NMAZ),WL(IMAZ,NMAZ),IFBER(IMAZ,NMAZ), SYM  130
     1 IFLT(KMAZ),KOM(IMAZ,NMAZ),KART(KMAZ),KZ(4)                       SYM  140
C                                                                       SYM  150
  200 FORMAT (' **ERROR** IN MESH (',I2,',',I2,') IFBER IS INCORRECT')  SYM  160
C                                                                       SYM  170
C                                                                       SYM  180
      IM1 = IMAX - 1                                                    SYM  190
      NM1 = NMAX - 1                                                    SYM  200
      DO 10 I=1,IM1                                                     SYM  210
        DO 10 N=1,NM1                                                   SYM  220
          K = KOM(I,N)                                                  SYM  230
          J1 = IFLT(K)                                                  SYM  240
          IF(EPS(K) .NE. 0.) J1 = 1                                     SYM  250
          K1 = KART(K)                                                  SYM  260
          IF(J1 .EQ. 0 .OR. K1 .EQ. 2) GOTO 20                          SYM  270
          KZ(1) = 5                                                     SYM  280
          KZ(2) = 7                                                     SYM  290
          KZ(3) = 6                                                     SYM  300
          KZ(4) = 8                                                     SYM  310
          GOTO 30                                                       SYM  320
   20     KZ(1) = 1                                                     SYM  330
          KZ(2) = 3                                                     SYM  340
          KZ(3) = 2                                                     SYM  350
          KZ(4) = 4                                                     SYM  360
   30     IF(I .EQ. 1) GOTO 40                                          SYM  370
          K = KOM(I-1,N)                                                SYM  380
          J2 = IFLT(K)                                                  SYM  390
          IF(EPS(K) .NE. 0.) J2 = 1                                     SYM  400
          K2 = KART(K)                                                  SYM  410
          IF(K1 .NE. 1 .OR. K2 .NE. 1) GOTO 41                          SYM  420
          WL(I,N) = 0.                                                  SYM  430
          GOTO 40                                                       SYM  440
   41     IF(J1 .NE. 0 .OR. J2 .NE. 0) GOTO 50                          SYM  450
C                                                                       SYM  460
   40     CALL SETBER(1,6,8,4,KZ)                                       SYM  470
C                                                                       SYM  480
          GOTO 60                                                       SYM  490
C                                                                       SYM  500
   50     CALL SETBER(5,7,3,2,KZ)                                       SYM  510
C                                                                       SYM  520
   60     IF(N .EQ. 1) GOTO 70                                          SYM  530
          K = KOM(I,N-1)                                                SYM  540
          K3 = KART(K)                                                  SYM  550
          J3 = IFLT(K)                                                  SYM  560
          IF(EPS(K) .NE. 0.) J3 = 1                                     SYM  570
          IF(K1 .NE. 1 .OR. K3 .NE. 1) GOTO 81                          SYM  580
          WI(I,N) = 0.                                                  SYM  590
          GOTO 70                                                       SYM  600
   81     IF(J1 .NE. 0 .OR. J3 .NE. 0) GOTO 80                          SYM  610
C                                                                       SYM  620
   70     CALL SETBER(1,7,2,8,KZ)                                       SYM  630
C                                                                       SYM  640
          GOTO 90                                                       SYM  650
C                                                                       SYM  660
   80     CALL SETBER(5,6,3,4,KZ)                                       SYM  670
C                                                                       SYM  680
   90     CONTINUE                                                      SYM  690
          DO 100 II=1,4                                                 SYM  700
            IF(KZ(II) .EQ. 0) GOTO 100                                  SYM  710
            IFBER(I,N) = KZ(II)                                         SYM  720
            KZ(II) = 0                                                  SYM  730
            GOTO 110                                                    SYM  740
  100     CONTINUE                                                      SYM  750
          WRITE (6,200) I,N                                             SYM  760
C                                                                       SYM  770
          CALL ABEND(3)                                                 SYM  780
C                                                                       SYM  790
  110     CONTINUE                                                      SYM  800
          DO 120 II=1,4                                                 SYM  810
            IF(KZ(II) .NE. 0) WRITE (6,200) I,N                         SYM  820
  120     CONTINUE                                                      SYM  830
          IF(N .EQ. 1) GOTO 240                                         SYM  840
          IF(I .NE. 1) GOTO 241                                         SYM  850
          IF(RAD0 .NE. 0 .OR. INDGEO .NE. 2) GOTO 240                   SYM  860
          IF(K1 .EQ. 3 .AND. K3 .EQ. 3) GOTO 230                        SYM  870
          IF(K1 .NE. 3 .AND. K3 .NE. 3) GOTO 240                        SYM  880
          IF(K1 .NE. 4 .AND. K3 .NE. 4) GOTO 230                        SYM  890
          GOTO 240                                                      SYM  900
  241     K = KOM(I-1,N-1)                                              SYM  910
          K4 = KART(K)                                                  SYM  920
          IF(K1 .EQ. 3 .AND. K2 .EQ. 3 .AND. K3 .EQ. 3 .AND. K4 .EQ. 3) SYM  930
     1     GOTO 230                                                     SYM  940
          IF(K1 .NE. 3 .AND. K2 .NE. 3 .AND. K3 .NE. 3 .AND. K4 .NE. 3) SYM  950
     1     GOTO 240                                                     SYM  960
          IF(K1 .NE. 4 .AND. K2 .NE. 4 .AND. K3 .NE. 4 .AND. K4 .NE. 4) SYM  970
     1     GOTO 230                                                     SYM  980
  240     IFBER(I,N) = -IFBER(I,N)                                      SYM  990
          IF(IFBER(I,N) .NE. -1 .OR. K1 .NE. 1) GOTO 230                SYM 1000
          WL(I,N) = 0.                                                  SYM 1010
          WI(I,N) = 0.                                                  SYM 1020
  230     CONTINUE                                                      SYM 1030
   10 CONTINUE                                                          SYM 1040
      DO 130 I=1,IMAX                                                   SYM 1050
        IFBER(I,NMAX) = -1                                              SYM 1060
  130 CONTINUE                                                          SYM 1070
      DO 140 N=1,NM1                                                    SYM 1080
        IFBER(IMAX,N) = -1                                              SYM 1090
  140 CONTINUE                                                          SYM 1100
      RETURN                                                            SYM 1110
      END                                                               SYM 1120
      SUBROUTINE TFELD(ITLAM,OVRM,IFKO1,IFWARN,CP0,IFZENT,NHMAT1,IFBH,  TFE   10
     1 RAD,PHI,KOM,T,AR,IFBER,DOSI,DOS,EPS,IDIR,IFANIS,HKUG,DI,NHZON,   TFE   20
     2 IFHET,WWK,DR,DPH,ALP,KART,WI,WL,WT,RADP,ISO,ISU,NSL,NSR,KSTR,ZDOSTFE   30
     3 ,IFLT,LAM,KKB,WQN,WQK,XFWQZ,ZKUG,TFLU,BU1,BU,WKAPH,THALT,FFTHX,  TFE   40
     4 TCOND,POWPT,VOLPT,DOSPT,THETNEW)                                 TFE   50
C                                                                       TFE   60
CFZJ026                                                       16.03.04  TFE   70
CFZJ042                                                       09.09.05  TFE   80
C                                                                       TFE   90
C     STEUERT DIE ITERATIVE BERECHNUNG DER FESTSTOFFTEMPERATUREN        TFE  100
C                                                                       TFE  110
      COMMON /CPBIL/ CPTER,CPKIN,CPSTR,CPGAS                            TFE  120
C                                                                       TFE  130
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                TFE  140
C                                                                       TFE  150
CFZJ004 enlarged dimensions common trans                      28.11.03  TFE  160
      COMMON /TRANS/ IFINST,INTVAL,DZEIT(300),ZEI(300),NPRINT(300),     TFE  170
     1 NKONV(300)                                                       TFE  180
C                                                                       TFE  190
      COMMON /LAMT/ IFLAMT                                              TFE  200
C                                                                       TFE  210
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    TFE  220
C                                                                       TFE  230
      COMMON /PRINT1/ TITLE(20),INDGEO                                  TFE  240
C                                                                       TFE  250
      COMMON /ITER1/ TRMAX,MIT,TN,OVREL,ITMAX,T1,T2,TRELA,NTMAX,IFKOR,  TFE  260
     1 ETHA,ORMIN,MITMAX,IKORM,IFREL                                    TFE  270
C                                                                       TFE  280
      COMMON /BASIS/ TBASE                                              TFE  290
C                                                                       TFE  300
CFZJ054                                                       09.08.07  TFE  310
C                                                                       TFE  320
CFZJ042                                                       09.09.05  TFE  330
      COMMON /BLINDL/ TMITL,M24,NGEOM,CIZET0                            TFE  340
C                                                                       TFE  350
CFZJ055                                                       25.09.07  TFE  360
C                                                                       TFE  370
CFZJ054                                                       09.08.07  TFE  380
C                                                                       TFE  390
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             TFE  400
C                                                                       TFE  410
      COMMON /PDTX/ PDTHX,RENORM,WQSUM,QNORM1,INORM                     TFE  420
C                                                                       TFE  430
CFZJ042                                                       09.09.05  TFE  440
      COMMON /COUPL/ IPRINT                                             TFE  450
C                                                                       TFE  460
CFZJ026                                                       16.03.04  TFE  470
CFZJ042                                                       09.09.05  TFE  480
      DIMENSION NHMAT1(KMAZ,5),IFBH(ICO,NCO),RAD(IMAZ+1),PHI(NMAZ+1),   TFE  490
     1 KOM(IMAZ,NMAZ),T(IMAZ,NMAZ),AR(IMAZ,NMAZ),IFBER(IMAZ,NMAZ),      TFE  500
     2 DOSI(IMAZ,NMAZ),DOS(KMAZ),EPS(KMAZ),IDIR(KMAZ),IFANIS(KMAZ),     TFE  510
     3 HKUG(KMAZ),DI(KMAZ,5),NHZON(KMAZ),IFHET(KMAZ),WWK(KMAZ,5),       TFE  520
     4 DR(IMAZ),DPH(NMAZ),ALP(KMAZ),KART(KMAZ),WI(IMAZ,NMAZ),           TFE  530
     5 WL(IMAZ,NMAZ),WT(IMAZ,NMAZ),RADP(IMAZ),ISO(NMAZ,19),ISU(NMAZ,19),TFE  540
     6 NSL(IMAZ,19),NSR(IMAZ,19),KSTR(KMAZ),ZDOS(NMAZ),IFLT(KMAZ),      TFE  550
     7 KKB(ICO,NCO),WQN(ICO,NCO),WQK(ICO,NCO),XFWQZ(KMAZ,5),            TFE  560
     8 ZKUG(ICO,NCO),TFLU(IMAZ,NMAZ),BU1(IMAZ,NMAZ),BU(IMAZ,NMAZ),      TFE  570
     9 WKAPH(ICO,NCO,5),THALT(ICO,NCO,5),FFTHX(IMAZ,NMAZ),              TFE  580
     X TCOND(IMAZ,NMAZ),POWPT(IMAZ,NMAZ,15),VOLPT(IMAZ,NMAZ,15),        TFE  590
     Y DOSPT(IMAZ,NMAZ,15),THETNEW(ICO,NCO,5,15)                        TFE  600
C                                                                       TFE  610
  100 FORMAT (4X,I5,1X,F7.4,2X,I4,2X,I5,3X,1PE9.2,4X,0PF7.2,3X,F7.2,2X, TFE  620
     1 F8.2,2X,2PF6.2,' %',3X,0PF8.2,4X,'(',I2,',',I2,')',1X,1PE9.2,2X, TFE  630
     2 0PF7.2)                                                          TFE  640
  101 FORMAT (/5X,' MIT',2X,'OVREL',4X,'IKOR',2X,'IFKO1',5X,'TRMAX',5X, TFE  650
     1 'Z(X)-KR',3X,'R(Y)-KR',5X,'TBASE',2X,'(DT/T)-MAX',2X,'T-DTMAX',2XTFE  660
     2 ,'(I,N)-DTMX',1X,'CPU-TIME    TMAX')                             TFE  670
  281 FORMAT (' **WARNING** SUSPECT OF OSCILLATIONS, REL.FACTOR WAS',I4,TFE  680
     1 '-TIMES CORRECTED')                                              TFE  690
  282 FORMAT (' **WARNING** MAX. NUMBER OF ITERATIONS IS REACHED, MIT = TFE  700
     1',I5)                                                             TFE  710
  501 FORMAT (/1X,'**FATAL ERROR** PROGRAM STOPPED AFTER 30 SEC. (CPU) BTFE  720
     1ECAUSE IFTEST = 1')                                               TFE  730
C                                                                       TFE  740
C                                                                       TFE  750
      CALL WATCH(START)                                                 TFE  760
C                                                                       TFE  770
      MIT = 0                                                           TFE  780
      IM1 = IMAX - 1                                                    TFE  790
      NM1 = NMAX - 1                                                    TFE  800
      IA = 2                                                            TFE  810
      IF(IFZENT .EQ. 1) IA = 1                                          TFE  820
      ITL = 0                                                           TFE  830
      IFKOR = 1                                                         TFE  840
      TRMIN = 1.E10                                                     TFE  850
      IKOR = 0                                                          TFE  860
      OVMAX = OVRM                                                      TFE  870
      OVREL = ORMIN                                                     TFE  880
      OVREL = 1.                                                        TFE  890
      TBASE = 0.                                                        TFE  900
    5 MITB = 0                                                          TFE  910
      TMX = -1.E10                                                      TFE  920
      DO 10 I=1,IMAX                                                    TFE  930
        DO 10 N=1,NMAX                                                  TFE  940
          IF(T(I,N) .GT. TMX) TMX = T(I,N)                              TFE  950
   10 CONTINUE                                                          TFE  960
      DO 40 I=1,IMAX                                                    TFE  970
        DO 40 N=1,NMAX                                                  TFE  980
          T(I,N) = T(I,N) - TMX                                         TFE  990
   40 CONTINUE                                                          TFE 1000
      IF(IFINST .NE. 1) GOTO 41                                         TFE 1010
      DO 42 I=1,IMAX                                                    TFE 1020
        DO 42 N=1,NMAX                                                  TFE 1030
          AR(I,N) = AR(I,N) - TMX                                       TFE 1040
   42 CONTINUE                                                          TFE 1050
   41 CONTINUE                                                          TFE 1060
      TBASE = TBASE + TMX                                               TFE 1070
  290 CONTINUE                                                          TFE 1080
      DTTM = 1.E-10                                                     TFE 1090
      MIT = MIT + 1                                                     TFE 1100
      MITB = MITB + 1                                                   TFE 1110
      IF(MITB .GE. 50) GOTO 5                                           TFE 1120
      ITL = ITL + 1                                                     TFE 1130
      IF(IFLAMT .EQ. 0) GOTO 116                                        TFE 1140
      IF(IFINST .EQ. 1) GOTO 116                                        TFE 1150
      IF(ITL .LT. ITLAM) GOTO 116                                       TFE 1160
C                                                                       TFE 1170
CFZJ026                                                       16.03.04  TFE 1180
      CALL KONST1(0,IFBER,EPS,IDIR,IFANIS,HKUG,DI,NHZON,IFHET,WWK,DR,DPHTFE 1190
     1 ,RAD,KOM,ALP,KART,WI,WL,WT,AR,RADP,ISO,ISU,NSL,NSR,KSTR,DOS,ZDOS,TFE 1200
     2 IFLT,T,PHI,LAM,DOSI,FFTHX,TCOND)                                 TFE 1210
C                                                                       TFE 1220
      ITL = 0                                                           TFE 1230
  116 CONTINUE                                                          TFE 1240
      TRMAX = 0.                                                        TFE 1250
      IF(IFINST .EQ. 1) GOTO 302                                        TFE 1260
C                                                                       TFE 1270
C     STATIONAERE RECHNUNG                                              TFE 1280
C     IFREL: I- ODER N-RICHTUNG                                         TFE 1290
C                                                                       TFE 1300
CFZJ038                                                       14.12.04  TFE 1310
      WQSUM = 0.                                                        TFE 1320
      IF(IFREL .NE. 0) GOTO 112                                         TFE 1330
      DO 300 N=2,NM1                                                    TFE 1340
        DO 300 I=IA,IM1                                                 TFE 1350
          IF(IFBER(I,N) .LT. 1) GOTO 300                                TFE 1360
          K = KOM(I,N)                                                  TFE 1370
          II = I - IDIFF                                                TFE 1380
          NN = N - NDIFF                                                TFE 1390
          IF(II .LT. 1 .OR. NN .LT. 1 .OR. II .GT. IMH .OR. NN .GT. NMH)TFE 1400
     1     GOTO 471                                                     TFE 1410
          IF(IFBH(II,NN) .EQ. 0) GOTO 471                               TFE 1420
          IF(NHMAT1(K,1) .EQ. M24) DOS(K) = DOSI(I,N)                   TFE 1430
C                                                                       TFE 1440
CFZJ042                                                       09.09.05  TFE 1450
          CALL CALTA(II,NN,IFKO1,NHZON,KKB,WQN,WQK,NHMAT1,XFWQZ,ZKUG,WWKTFE 1460
     1     ,T,WI,WL,WT,TFLU,LAM,DOS,ZDOS,POWPT,VOLPT,DOSPT,THETNEW)     TFE 1470
C                                                                       TFE 1480
          TN = TMITL - TBASE                                            TFE 1490
C                                                                       TFE 1500
C     T(I,N) BZW.TN IST HIER DIE OBERFLAECHENTEMPERATUR                 TFE 1510
C                                                                       TFE 1520
          GOTO 472                                                      TFE 1530
C                                                                       TFE 1540
  471     TN = CALT(I,N,IFKO1,T,WI,WL,WT,TFLU,BU1,BU)                   TFE 1550
C                                                                       TFE 1560
  472     T(I,N) = TNEU(I,N,T)                                          TFE 1570
C                                                                       TFE 1580
  300 CONTINUE                                                          TFE 1590
      GOTO 114                                                          TFE 1600
  112 CONTINUE                                                          TFE 1610
      DO 301 I=IA,IM1                                                   TFE 1620
        DO 301 N=2,NM1                                                  TFE 1630
          IF(IFBER(I,N) .LT. 1) GOTO 301                                TFE 1640
          K = KOM(I,N)                                                  TFE 1650
          II = I - IDIFF                                                TFE 1660
          NN = N - NDIFF                                                TFE 1670
          IF(II .LT. 1 .OR. NN .LT. 1 .OR. II .GT. IMH .OR. NN .GT. NMH)TFE 1680
     1     GOTO 571                                                     TFE 1690
          IF(IFBH(II,NN) .EQ. 0) GOTO 571                               TFE 1700
          IF(NHMAT1(K,1) .EQ. M24) DOS(K) = DOSI(I,N)                   TFE 1710
C                                                                       TFE 1720
CFZJ042                                                       09.09.05  TFE 1730
          CALL CALTA(II,NN,IFKO1,NHZON,KKB,WQN,WQK,NHMAT1,XFWQZ,ZKUG,WWKTFE 1740
     1     ,T,WI,WL,WT,TFLU,LAM,DOS,ZDOS,POWPT,VOLPT,DOSPT,THETNEW)     TFE 1750
C                                                                       TFE 1760
          TN = TMITL - TBASE                                            TFE 1770
C                                                                       TFE 1780
C     T(I,N) BZW.TN IST HIER DIE OBERFLAECHENTEMPERATUR                 TFE 1790
C                                                                       TFE 1800
          GOTO 572                                                      TFE 1810
C                                                                       TFE 1820
  571     TN = CALT(I,N,IFKO1,T,WI,WL,WT,TFLU,BU1,BU)                   TFE 1830
C                                                                       TFE 1840
  572     T(I,N) = TNEU(I,N,T)                                          TFE 1850
C                                                                       TFE 1860
  301 CONTINUE                                                          TFE 1870
      GOTO 114                                                          TFE 1880
C                                                                       TFE 1890
C     INSTATIONAERE RECHNUNG                                            TFE 1900
C                                                                       TFE 1910
  302 CONTINUE                                                          TFE 1920
      IF(IFREL .NE. 0) GOTO 113                                         TFE 1930
      DO 360 N=2,NM1                                                    TFE 1940
        DO 360 I=IA,IM1                                                 TFE 1950
          IF(IFBER(I,N) .LT. 1) GOTO 360                                TFE 1960
          K = KOM(I,N)                                                  TFE 1970
          II = I - IDIFF                                                TFE 1980
          NN = N - NDIFF                                                TFE 1990
          IF(II .LT. 1 .OR. NN .LT. 1 .OR. II .GT. IMH .OR. NN .GT. NMH)TFE 2000
     1     GOTO 361                                                     TFE 2010
          IF(IFBH(II,NN) .EQ. 0) GOTO 361                               TFE 2020
          IF(NHMAT1(K,1) .EQ. M24) DOS(K) = DOSI(I,N)                   TFE 2030
C                                                                       TFE 2040
CFZJ042                                                       09.09.05  TFE 2050
          CALL CALT2(II,NN,IFKO1,NHZON,KKB,THALT,WQN,WQK,AR,NHMAT1,XFWQZTFE 2060
     1     ,ZKUG,WWK,WKAPH,T,WI,WL,WT,TFLU,LAM,DOS,ZDOS,THETNEW)        TFE 2070
C                                                                       TFE 2080
          TN = TMITL - TBASE                                            TFE 2090
          GOTO 362                                                      TFE 2100
C                                                                       TFE 2110
C     T(I,N) BZW.TN IST HIER DIE OBERFLAECHENTEMPERATUR                 TFE 2120
C                                                                       TFE 2130
  361     CONTINUE                                                      TFE 2140
C                                                                       TFE 2150
          TN = CALT1(I,N,IFKO1,AR,BU,T,WI,WL,WT,TFLU,BU1)               TFE 2160
C                                                                       TFE 2170
  362     T(I,N) = TNEU(I,N,T)                                          TFE 2180
C                                                                       TFE 2190
  360 CONTINUE                                                          TFE 2200
      GOTO 114                                                          TFE 2210
  113 CONTINUE                                                          TFE 2220
      DO 370 I=IA,IM1                                                   TFE 2230
        DO 370 N=2,NM1                                                  TFE 2240
          IF(IFBER(I,N) .LT. 1) GOTO 370                                TFE 2250
          K = KOM(I,N)                                                  TFE 2260
          II = I - IDIFF                                                TFE 2270
          NN = N - NDIFF                                                TFE 2280
          IF(II .LT. 1 .OR. NN .LT. 1 .OR. II .GT. IMH .OR. NN .GT. NMH)TFE 2290
     1     GOTO 371                                                     TFE 2300
          IF(IFBH(II,NN) .EQ. 0) GOTO 371                               TFE 2310
          IF(NHMAT1(K,1) .EQ. M24) DOS(K) = DOSI(I,N)                   TFE 2320
C                                                                       TFE 2330
CFZJ042                                                       09.09.05  TFE 2340
          CALL CALT2(II,NN,IFKO1,NHZON,KKB,THALT,WQN,WQK,AR,NHMAT1,XFWQZTFE 2350
     1     ,ZKUG,WWK,WKAPH,T,WI,WL,WT,TFLU,LAM,DOS,ZDOS,THETNEW)        TFE 2360
C                                                                       TFE 2370
          TN = TMITL - TBASE                                            TFE 2380
C                                                                       TFE 2390
C     T(I,N) BZW.TN IST HIER DIE OBERFLAECHENTEMPERATUR                 TFE 2400
C                                                                       TFE 2410
          GOTO 372                                                      TFE 2420
  371     CONTINUE                                                      TFE 2430
C                                                                       TFE 2440
          TN = CALT1(I,N,IFKO1,AR,BU,T,WI,WL,WT,TFLU,BU1)               TFE 2450
C                                                                       TFE 2460
  372     T(I,N) = TNEU(I,N,T)                                          TFE 2470
C                                                                       TFE 2480
  370 CONTINUE                                                          TFE 2490
C                                                                       TFE 2500
  114 CONTINUE                                                          TFE 2510
CFZJ033                                                       17.08.04  TFE 2520
CFZJ038                                                       14.12.04  TFE 2530
      IF(IFINST .EQ. 0 .AND. INORM .EQ. 0) RENORM = QNORM1 / WQSUM      TFE 2540
      INORM = 1                                                         TFE 2550
C                                                                       TFE 2560
      IF(IFREF .EQ. 1) CALL REFL(T)                                     TFE 2570
C                                                                       TFE 2580
      IF(TRMAX .LT. ETHA) GOTO 410                                      TFE 2590
      IF(OVMAX .EQ. ORMIN) GOTO 340                                     TFE 2600
      IF(MIT .LE. 2) TRELA = TRMAX * 1.001                              TFE 2610
      DFAK = TRELA / TRMAX                                              TFE 2620
      IF(TRMAX-1.01*TRELA) 340,340,115                                  TFE 2630
  115 CONTINUE                                                          TFE 2640
      IF(ITL .EQ. 0 .AND. ITLAM .GT. 2) GOTO 340                        TFE 2650
      IF(IFKOR .EQ. 1) GOTO 340                                         TFE 2660
      IKOR = IKOR + 1                                                   TFE 2670
      IF(IKOR .GT. IKORM) GOTO 410                                      TFE 2680
      IF(OVMAX .GT. 1.) OVMAX = OVMAX * .97                             TFE 2690
      XXMAX = PHI(NTMAX)                                                TFE 2700
      IF(INDGEO .EQ. 1) XXMAX = XXMAX * 180. / 3.14                     TFE 2710
      YYMAX = RAD(ITMAX)                                                TFE 2720
      OVREL = OVREL * DFAK                                              TFE 2730
      IF(OVREL .LT. ORMIN) OVREL = ORMIN                                TFE 2740
      GOTO 341                                                          TFE 2750
  340 IF(TRELA .EQ. 0.) GOTO 341                                        TFE 2760
      IF(TRMAX .GE. TRMIN .OR. TRELA .GT. TRMIN) GOTO 341               TFE 2770
      OVREL = OVREL * ((DFAK-1)*.5+1.)                                  TFE 2780
      IF(OVREL .GT. OVMAX) OVREL = OVMAX                                TFE 2790
  341 TRELA = TRMAX                                                     TFE 2800
      IF(TRMAX .LT. TRMIN) TRMIN = TRMAX                                TFE 2810
      IF(MIT .LT. MITMAX) GOTO 290                                      TFE 2820
  410 CONTINUE                                                          TFE 2830
      XXMAX = PHI(NTMAX)                                                TFE 2840
      IF(INDGEO .EQ. 1) XXMAX = XXMAX * 180. / 3.14                     TFE 2850
      YYMAX = RAD(ITMAX)                                                TFE 2860
C                                                                       TFE 2870
      CALL WATCH(ENDE)                                                  TFE 2880
C                                                                       TFE 2890
      CP = ENDE                                                         TFE 2900
      IF(IFWARN .NE. 1) GOTO 500                                        TFE 2910
      CPDIFF = CP0 - CP                                                 TFE 2920
      IF(CPDIFF .LT. 30.) GOTO 500                                      TFE 2930
      WRITE (6,501)                                                     TFE 2940
C                                                                       TFE 2950
      CALL ABEND(5)                                                     TFE 2960
C                                                                       TFE 2970
  500 CONTINUE                                                          TFE 2980
      CP = (CP-START)                                                   TFE 2990
      CPTER = CPTER + CP                                                TFE 3000
CFZJ042                                                       09.09.05  TFE 3010
      IF(IPRINT .GE. 1) WRITE (6,101)                                   TFE 3020
      IDT = 0                                                           TFE 3030
      NDT = 0                                                           TFE 3040
      DTTRMX = 0.                                                       TFE 3050
      TTRMX = 0.                                                        TFE 3060
      TMAX = 0.                                                         TFE 3070
      DO 50 I=1,IMAX                                                    TFE 3080
        DO 50 N=1,NMAX                                                  TFE 3090
          IF(IFINST .EQ. 1) AR(I,N) = AR(I,N) + TBASE                   TFE 3100
          T(I,N) = T(I,N) + TBASE                                       TFE 3110
          TIN = T(I,N)                                                  TFE 3120
          TMAX = AMAX1(TMAX,TIN)                                        TFE 3130
          IF(AR(I,N) .EQ. 0.) GOTO 50                                   TFE 3140
          IF(IFINST .NE. 1) GOTO 50                                     TFE 3150
          DT = (T(I,N)-AR(I,N)) / AR(I,N)                               TFE 3160
          DT = ABS(DT)                                                  TFE 3170
          IF(DT .LT. DTTRMX) GOTO 50                                    TFE 3180
          DTTRMX = DT                                                   TFE 3190
          TTRMX = T(I,N)                                                TFE 3200
          IDT = I                                                       TFE 3210
          NDT = N                                                       TFE 3220
   50 CONTINUE                                                          TFE 3230
CFZJ042                                                       09.09.05  TFE 3240
      IF(IPRINT .GE. 1) WRITE (6,100) MIT,OVREL,IKOR,IFKO1,TRMAX,XXMAX, TFE 3250
     1 YYMAX,TBASE,DTTRMX,TTRMX,IDT,NDT,CP,TMAX                         TFE 3260
CFZJ054                                                       09.08.07  TFE 3270
      IF(MIT .LT. MITMAX) GOTO 610                                      TFE 3280
      WRITE (6,282) MIT                                                 TFE 3290
C                                                                       TFE 3300
      CALL ABEND(2)                                                     TFE 3310
C                                                                       TFE 3320
  610 IF(IKOR .LE. IKORM) GOTO 630                                      TFE 3330
      WRITE (6,281) IKOR                                                TFE 3340
C                                                                       TFE 3350
      CALL ABEND(2)                                                     TFE 3360
C                                                                       TFE 3370
  630 CONTINUE                                                          TFE 3380
      TBASE = 0.                                                        TFE 3390
      RETURN                                                            TFE 3400
      END                                                               TFE 3410
      FUNCTION TNEU(I,N,T)                                              TNE   10
C                                                                       TNE   20
C     UEBERRELAXATION                                                   TNE   30
C                                                                       TNE   40
      COMMON /ITER1/ TRMAX,MIT,TN,OVREL,ITMAX,T1,T2,TRELA,NTMAX,IFKOR,  TNE   50
     1 ETHA,ORMIN,MITMAX,IKORM,IFREL                                    TNE   60
C                                                                       TNE   70
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             TNE   80
C                                                                       TNE   90
      DIMENSION T(IMAZ,NMAZ)                                            TNE  100
C                                                                       TNE  110
C                                                                       TNE  120
      DT = TN - T(I,N)                                                  TNE  130
C                                                                       TNE  140
      TNEU = T(I,N) + DT * OVREL                                        TNE  150
C                                                                       TNE  160
      TREL = ABS(DT)                                                    TNE  170
      IF(TRMAX .GT. TREL) GOTO 10                                       TNE  180
      TRMAX = TREL                                                      TNE  190
      IFKOR = 0                                                         TNE  200
      ITMAX = I                                                         TNE  210
      NTMAX = N                                                         TNE  220
      T1 = TN                                                           TNE  230
      T2 = T(I,N)                                                       TNE  240
   10 CONTINUE                                                          TNE  250
      RETURN                                                            TNE  260
      END                                                               TNE  270
      SUBROUTINE TPROZ(NHET,DTEM1,IFKO1,NHZON,XFWQZ,VOLS,IFFUEL,ZKUG,   TPR   10
     1 WWK,IFBH,WKAPH,WQN,WQK,NHMAT1,DOS,ZDOS,KKB,DPH,PHI,KOM,LAM,AU,T, TPR   20
     2 WI,WL,WT,TFLU,DOSI,VOLPT,POWPT,THETNEW,NPR)                      TPR   30
C                                                                       TPR   40
CFZJ042                                                       09.09.05  TPR   50
C                                                                       TPR   60
C     FERTIGT TEMP-VOL-ANALYSE DES CORES AN UND BERECHNET MITTL. FUEL-  TPR   70
C     UND MODERATOR-TEMPERATUREN (FUER 'HET'-REGIONEN)                  TPR   80
C                                                                       TPR   90
      COMMON /VAIZ/ AIZ,BEM                                             TPR  100
C                                                                       TPR  110
      COMMON /DYKOP1/ ANIN(21),NDYN,TMOD(10),TFUEL(10),DZDYN(10),PT(10),TPR  120
     1 ZDYN(10)                                                         TPR  130
C                                                                       TPR  140
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                TPR  150
C                                                                       TPR  160
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    TPR  170
C                                                                       TPR  180
      COMMON /SPECTI/ ITIK(10)                                          TPR  190
C                                                                       TPR  200
CFZJ006 enlarged dimensions common QVAR                       28.11.03  TPR  210
      COMMON /QVAR/ DUM(11),TE(4,300),TA(300),N61,URZ,ZLEKA,ABXEN       TPR  220
C                                                                       TPR  230
CFZJ042                                                       09.09.05  TPR  240
      COMMON /BLINDL/ TMITL,M24,NGEOM,CIZET0                            TPR  250
C                                                                       TPR  260
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             TPR  270
C                                                                       TPR  280
CFZJ042                                                       09.09.05  TPR  290
      COMMON /VRT/ MIXM                                                 TPR  300
C                                                                       TPR  310
CFZJ041                                                       15.02.05  TPR  320
CFZJ058                                                       05.11.08  TPR  330
      COMMON /KEFFT/ TKEFF(5000),RKEFF(5000),IPKEFF,NKEFF,TKUM,POWLMAX, TPR  340
     1 THETMX,POWLM(5000),POWB(5000),BUSCRAP(5000),THETM(5000),         TPR  350
     2 POWERF(5000),TEIN(5000),TAUS(5000),TEINTR,TAUSTR                 TPR  360
C                                                                       TPR  370
CFZJ004 enlarged dimensions common trans                      28.11.03  TPR  380
CFZJ054                                                       09.08.07  TPR  390
      COMMON /TRANS/ IFINST,INTVAL,DZEIT(300),ZEI(300),NPRINT(300),     TPR  400
     1 NKONV(300)                                                       TPR  410
C                                                                       TPR  420
CFZJ054                                                       09.08.07  TPR  430
      COMMON /STA/ IST,SB,TEX,JNS                                       TPR  440
C                                                                       TPR  450
CFZJ042                                                       09.09.05  TPR  460
      DIMENSION NHZON(KMAZ),XFWQZ(KMAZ,5),VOLS(KMAZ,5),IFFUEL(KMAZ,5),  TPR  470
     1 ZKUG(ICO,NCO),WWK(KMAZ,5),IFBH(ICO,NCO),WKAPH(ICO,NCO,5),        TPR  480
     2 WQN(ICO,NCO),WQK(ICO,NCO),NHMAT1(KMAZ,5),DOS(KMAZ),ZDOS(NMAZ),   TPR  490
     3 KKB(ICO,NCO),DPH(NMAZ),PHI(NMAZ+1),KOM(IMAZ,NMAZ),AU(IMAZ,NMAZ), TPR  500
     4 T(IMAZ,NMAZ),WI(IMAZ,NMAZ),WL(IMAZ,NMAZ),WT(IMAZ,NMAZ),          TPR  510
     5 TFLU(IMAZ,NMAZ),DOSI(IMAZ,NMAZ),TG(16),PROZ(15),A3(10),WC(4),    TPR  520
     6 TH(4),TH1(10),TH2(10),VOLPT(IMAZ,NMAZ,15),WQNS(10),WQKS(10),     TPR  530
     7 POWPT(IMAZ,NMAZ,15),VOLSP(15),THETNEW(ICO,NCO,5,15)              TPR  540
C                                                                       TPR  550
      REAL LAM(KMAZ)                                                    TPR  560
C                                                                       TPR  570
      CHARACTER*3 BEM(1000),HE/'HET'/                                   TPR  580
C                                                                       TPR  590
      CHARACTER*4 AIZ(100,2,200,2)                                      TPR  600
C                                                                       TPR  610
   70 FORMAT (///39X,'*** TEMPERATURE/VOLUME-ANALYSIS OF ELEMENT SURFACETPR  620
     1 ***'//5X,'DEG. C',3X,16(0PF7.0)/5X,'PERCENT',5X,15F7.1/)         TPR  630
   71 FORMAT (///39X,'*** TEMPERATURE/VOLUME-ANALYSIS OF FUEL ***'//5X, TPR  640
     1 'DEG. C',3X,16(0PF7.0)/5X,'PERCENT',5X,15F7.1/)                  TPR  650
   80 FORMAT (5X,'AVERAGE:',F11.1,' DEG. C')                            TPR  660
  100 FORMAT (//5X,'*** AVERAGE TEMPERATURES (DEG. C) ***'//5X,'ELEMENT TPR  670
     1SURFACE    MODERATOR    FUEL',40X,'HS-NUCL(W)   HS-CONV(W)        TPR  680
     2NO. OF ELEMENTS')                                                 TPR  690
  200 FORMAT (9X,F7.0,7X,2(F7.0,4X),35X,1PE10.2,1PE13.2,12X,0PF9.0)     TPR  700
  201 FORMAT (82X,21(1H-)/80X,1PE10.2,2X,1PE11.2)                       TPR  710
  202 FORMAT (//'     MAXIMUM FUEL TEMPERATURE (DEG. C):',F7.0,'    CORRTPR  720
     1ESPONDING GAS TEMPERATURE (DEG. C):',F7.0)                        TPR  730
  411 FORMAT (5X,I3,3X,F7.2)                                            TPR  740
C                                                                       TPR  750
C                                                                       TPR  760
      IFPRNT = 0                                                        TPR  770
      VOL = 0.                                                          TPR  780
      TMIN = 1.E20                                                      TPR  790
      TMAX = 0.                                                         TPR  800
CFZJ042                                                       09.09.05  TPR  810
      DO 9 I=1,IMH                                                      TPR  820
        DO 9 N=1,NMH                                                    TPR  830
          DO 8 ID=1,MIXM                                                TPR  840
            TMAX = AMAX1(TMAX,THETNEW(I,N,1,ID))                        TPR  850
            TMIN = AMIN1(TMIN,THETNEW(I,N,1,ID))                        TPR  860
    8     CONTINUE                                                      TPR  870
    9 CONTINUE                                                          TPR  880
      TMIT = 0.                                                         TPR  890
CFZJ042                                                       09.09.05  TPR  900
      DO 10 I=1,IMH-1                                                   TPR  910
        IA = I + IDIFF                                                  TPR  920
        DO 10 N=1,NMH-1                                                 TPR  930
          NA = N + NDIFF                                                TPR  940
          WT(IA,NA) = 4. * AU(IA,NA)                                    TPR  950
          DO 11 ID=1,MIXM                                               TPR  960
            TM = (THETNEW(I,N,1,ID)+THETNEW(I+1,N,1,ID)+                TPR  970
     1       THETNEW(I,N+1,1,ID)+THETNEW(I+1,N+1,1,ID)) / 4.            TPR  980
            TMIT = TMIT + TM * WT(IA,NA) * VOLPT(IA,NA,ID)              TPR  990
            VOL = VOL + WT(IA,NA) * VOLPT(IA,NA,ID)                     TPR 1000
   11     CONTINUE                                                      TPR 1010
   10 CONTINUE                                                          TPR 1020
      TMIN = TMIN - 0.01                                                TPR 1030
      IF(VOL .EQ. 0.) VOL = 1.E-20                                      TPR 1040
      TMIT = TMIT / VOL                                                 TPR 1050
CFZJ042                                                       09.09.05  TPR 1060
      DT = (TMAX-TMIN) / 15.                                            TPR 1070
      TG(1) = TMIN                                                      TPR 1080
      TG(16) = TMAX + 0.01                                              TPR 1090
      DO 30 J=2,15                                                      TPR 1100
        TG(J) = TG(J-1) + DT                                            TPR 1110
   30 CONTINUE                                                          TPR 1120
CFZJ042                                                       09.09.05  TPR 1130
      DO 40 J=1,15                                                      TPR 1140
        PROZ(J) = 0.                                                    TPR 1150
        DO 40 I=1,IMH-1                                                 TPR 1160
          IA = I + IDIFF                                                TPR 1170
          DO 40 N=1,NMH-1                                               TPR 1180
            NA = N + NDIFF                                              TPR 1190
            DO 43 ID=1,MIXM                                             TPR 1200
              TM = (THETNEW(I,N,1,ID)+THETNEW(I+1,N,1,ID)+              TPR 1210
     1         THETNEW(I,N+1,1,ID)+THETNEW(I+1,N+1,1,ID)) / 4.          TPR 1220
              IF(TM .LE. TG(J) .OR. TM .GT. TG(J+1)) GOTO 43            TPR 1230
              PROZ(J) = PROZ(J) + WT(IA,NA) * VOLPT(IA,NA,ID) / VOL *   TPR 1240
     1         100.                                                     TPR 1250
   43       CONTINUE                                                    TPR 1260
   40 CONTINUE                                                          TPR 1270
      IF(NPR .EQ. 0) WRITE (6,70) (TG(J),J=1,16),(PROZ(J),J=1,15)       TPR 1280
CFZJ042                                                       09.09.05  TPR 1290
      TAVSURF = TMIT                                                    TPR 1300
      IT1 = ITIK(1) + 1                                                 TPR 1310
      TE(2,IT1) = TMIT                                                  TPR 1320
      IF(NHET .EQ. 0) RETURN                                            TPR 1330
C                                                                       TPR 1340
C     BERECHNUNG VON BRENNSTOFF- UND MODERATORTEMPERATUR                TPR 1350
C                                                                       TPR 1360
      DO 18 ND=1,NDYN                                                   TPR 1370
        TH1(ND) = 0.                                                    TPR 1380
        TH2(ND) = 0.                                                    TPR 1390
        TFUEL(ND) = 0.                                                  TPR 1400
        TMOD(ND) = 0.                                                   TPR 1410
        A3(ND) = 0.                                                     TPR 1420
        WQNS(ND) = 0.                                                   TPR 1430
        WQKS(ND) = 0.                                                   TPR 1440
   18 CONTINUE                                                          TPR 1450
      ND1 = 1                                                           TPR 1460
CFZJ042                                                       09.09.05  TPR 1470
      THETMIN = 10000.                                                  TPR 1480
      THETMX = 0.                                                       TPR 1490
      DO 12 N=1,NMH                                                     TPR 1500
        NA = N + NDIFF                                                  TPR 1510
        DO 41 I=1,IMH                                                   TPR 1520
          IF1 = IFBH(I,N)                                               TPR 1530
CFZJ042                                                       09.09.05  TPR 1540
          ZKG = ZKUG(I,N)                                               TPR 1550
          IF(IF1 .EQ. 0) GOTO 41                                        TPR 1560
          K = KKB(I,N)                                                  TPR 1570
          NZZ = NHZON(K)                                                TPR 1580
          ISCHA = NZZ                                                   TPR 1590
CFZJ042                                                       09.09.05  TPR 1600
          WQNS(ND1) = WQNS(ND1) + WQN(I,N)                              TPR 1610
          IA = I + IDIFF                                                TPR 1620
          IF(NHMAT1(K,1) .EQ. M24) DOS(K) = DOSI(IA,NA)                 TPR 1630
          TM = T(IA,NA)                                                 TPR 1640
          WC(1) = WL(IA,NA-1)                                           TPR 1650
          WC(3) = WL(IA,NA)                                             TPR 1660
          WC(4) = WI(IA,NA)                                             TPR 1670
          TH(1) = T(IA,NA-1)                                            TPR 1680
          TH(3) = T(IA,NA+1)                                            TPR 1690
          TH(4) = T(IA+1,NA)                                            TPR 1700
          WC(2) = 0.                                                    TPR 1710
          TH(2) = 0.                                                    TPR 1720
          IF(IA .EQ. 1) GOTO 502                                        TPR 1730
          WC(2) = WI(IA-1,NA)                                           TPR 1740
          TH(2) = T(IA-1,NA)                                            TPR 1750
  502     CONTINUE                                                      TPR 1760
          ARAND = 0.                                                    TPR 1770
CFZJ042                                                       09.09.05  TPR 1780
          DO 505 IRND=1,4                                               TPR 1790
            ARAND = ARAND + WC(IRND) * (TH(IRND)-TM)                    TPR 1800
  505     CONTINUE                                                      TPR 1810
          IF(IFKO1 .EQ. 1) GOTO 500                                     TPR 1820
CFZJ042                                                       09.09.05  TPR 1830
          WQKS(ND1) = WQKS(ND1) + WQK(I,N) * (TFLU(IA,NA)-TM)           TPR 1840
          GOTO 501                                                      TPR 1850
  500     CONTINUE                                                      TPR 1860
CFZJ042                                                       09.09.05  TPR 1870
          WQKS(ND1) = WQKS(ND1) + WQK(I,N)                              TPR 1880
  501     A3(ND1) = A3(ND1) + ZKG                                       TPR 1890
          WQKS(ND1) = WQKS(ND1) + ARAND                                 TPR 1900
CFZJ042                                                       09.09.05  TPR 1910
          DO 16 ID=1,MIXM                                               TPR 1920
            DO 14 NZ=1,NZZ                                              TPR 1930
              IF(NZ .NE. NZZ) GOTO 24                                   TPR 1940
              A1 = THETNEW(I,N,NZZ,ID)                                  TPR 1950
              A2 = VOLS(K,NZZ) * ZKG * VOLPT(IA,NA,ID)                  TPR 1960
              GOTO 15                                                   TPR 1970
   24         A1 = (.5*(THETNEW(I,N,NZ,ID)+THETNEW(I,N,NZ+1,ID)))       TPR 1980
              A2 = VOLS(K,NZ) * ZKG * VOLPT(IA,NA,ID)                   TPR 1990
   15         IF(POWPT(IA,NA,ID) .LE. 0.) GOTO 23                       TPR 2000
              IF(IFFUEL(K,NZ) .EQ. 0) GOTO 23                           TPR 2010
              TFUEL(ND1) = TFUEL(ND1) + A1 * A2                         TPR 2020
              TH1(ND1) = TH1(ND1) + A2                                  TPR 2030
   23         TMOD(ND1) = TMOD(ND1) + A1 * A2                           TPR 2040
              TH2(ND1) = TH2(ND1) + A2                                  TPR 2050
              IF(THETNEW(I,N,NZZ,ID) .LE. THETMX) GOTO 26               TPR 2060
              THETMX = THETNEW(I,N,NZZ,ID)                              TPR 2070
              TFLUMX = TFLU(IA,NA)                                      TPR 2080
   26         CONTINUE                                                  TPR 2090
              IF(IFFUEL(K,NZ) .EQ. 0 .OR. NZ .EQ. NZZ) GOTO 14          TPR 2100
              AT = THETNEW(I,N,NZ,ID)                                   TPR 2110
              IF(AT .LT. THETMIN) THETMIN = AT                          TPR 2120
   14       CONTINUE                                                    TPR 2130
   16     CONTINUE                                                      TPR 2140
   41   CONTINUE                                                        TPR 2150
   12 CONTINUE                                                          TPR 2160
CFZJ042                                                       09.09.05  TPR 2170
      TG(1) = THETMIN - 0.01                                            TPR 2180
      TG(16) = THETMX + 0.01                                            TPR 2190
      DT = (TG(16)-TG(1)) / 15.                                         TPR 2200
      DO 50 JG=2,15                                                     TPR 2210
        TG(JG) = TG(JG-1) + DT                                          TPR 2220
   50 CONTINUE                                                          TPR 2230
CFZJ042                                                       09.09.05  TPR 2240
      DO 68 I=1,15                                                      TPR 2250
        VOLSP(I) = 0.                                                   TPR 2260
   68 CONTINUE                                                          TPR 2270
CFZJ042                                                       09.09.05  TPR 2280
      DO 60 N=1,NMH                                                     TPR 2290
        NA = N + NDIFF                                                  TPR 2300
        DO 61 I=1,IMH                                                   TPR 2310
          IF1 = IFBH(I,N)                                               TPR 2320
          IA = I + IDIFF                                                TPR 2330
          IF(IF1 .EQ. 0) GOTO 61                                        TPR 2340
          ZKG = ZKUG(I,N)                                               TPR 2350
          K = KKB(I,N)                                                  TPR 2360
          DO 62 ID=1,MIXM                                               TPR 2370
            DO 63 NZ=1,NZZ                                              TPR 2380
              IF(NZ .NE. NZZ) GOTO 64                                   TPR 2390
              A1 = THETNEW(I,N,NZZ,ID)                                  TPR 2400
              A2 = VOLS(K,NZZ) * ZKG * VOLPT(IA,NA,ID)                  TPR 2410
              GOTO 65                                                   TPR 2420
   64         A1 = (0.5*(THETNEW(I,N,NZ,ID)+THETNEW(I,N,NZ+1,ID)))      TPR 2430
              A2 = VOLS(K,NZ) * ZKG * VOLPT(IA,NA,ID)                   TPR 2440
   65         IF(POWPT(IA,NA,ID) .LE. 0.) GOTO 63                       TPR 2450
              IF(IFFUEL(K,NZ) .EQ. 0) GOTO 63                           TPR 2460
                DO 66 JG=1,15                                           TPR 2470
                  IF(A1 .LT. TG(JG) .OR. A1 .GT. TG(JG+1)) GOTO 66      TPR 2480
                  VOLSP(JG) = VOLSP(JG) + A2                            TPR 2490
   66           CONTINUE                                                TPR 2500
   63       CONTINUE                                                    TPR 2510
   62     CONTINUE                                                      TPR 2520
   61   CONTINUE                                                        TPR 2530
   60 CONTINUE                                                          TPR 2540
CFZJ042                                                       09.09.05  TPR 2550
      DO 67  JG=1,15                                                    TPR 2560
        VOLSP(JG) = 100. * VOLSP(JG) / TH1(ND1)                         TPR 2570
   67 CONTINUE                                                          TPR 2580
      IF(NPR .EQ. 0) WRITE(6,71) (TG(JG),JG=1,16),(VOLSP(JG),JG=1,15)   TPR 2590
CFZJ042                                                       09.09.05  TPR 2600
      DO 19 ND=1,NDYN                                                   TPR 2610
        WQNS(ND) = WQNS(ND) / A3(ND)                                    TPR 2620
        WQKS(ND) = WQKS(ND) / A3(ND)                                    TPR 2630
        TFUEL(ND) = TFUEL(ND) / TH1(ND)                                 TPR 2640
        TMOD(ND) = TMOD(ND) / TH2(ND)                                   TPR 2650
   19 CONTINUE                                                          TPR 2660
      S1 = 0.                                                           TPR 2670
      S2 = 0.                                                           TPR 2680
      WRITE (6,100)                                                     TPR 2690
CFZJ042                                                       09.09.05  TPR 2700
      DO 22 ND=1,NDYN                                                   TPR 2710
        WRITE (6,200) TAVSURF,TMOD(ND),TFUEL(ND),WQNS(ND),WQKS(ND),     TPR 2720
     1   A3(ND)                                                         TPR 2730
        S1 = S1 + WQNS(ND) * A3(ND)                                     TPR 2740
        S2 = S2 + WQKS(ND) * A3(ND)                                     TPR 2750
   22 CONTINUE                                                          TPR 2760
      WRITE (6,201) S1,S2                                               TPR 2770
      WRITE (6,202) THETMX,TFLUMX                                       TPR 2780
CFZJ054                                                       09.08.07  TPR 2790
      IT1 = ITIK(1) + 1                                                 TPR 2800
      TE(1,IT1) = THETMX                                                TPR 2810
      IF(INTVAL .LE. 1) TEX = THETMX                                    TPR 2820
      RETURN                                                            TPR 2830
      END                                                               TPR 2840
      SUBROUTINE VORKON(RAD,PHI,AU,RADP)                                RKO   10
C                                                                       RKO   20
C     BERECHNET ELEMENTEN-VIERTEL-FLAECHEN (--->  AU(I,N) )             RKO   30
C                                                                       RKO   40
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    RKO   50
C                                                                       RKO   60
      COMMON /PRINT1/ TITLE(20),INDGEO                                  RKO   70
C                                                                       RKO   80
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             RKO   90
C                                                                       RKO  100
      DIMENSION RAD(IMAZ+1),PHI(NMAZ+1),AU(IMAZ,NMAZ),RADP(IMAZ)        RKO  110
C                                                                       RKO  120
C                                                                       RKO  130
      IM1 = IMAX - 1                                                    RKO  140
      NM1 = NMAX - 1                                                    RKO  150
      IG = INDGEO + 1                                                   RKO  160
      DO 10 I=1,IM1                                                     RKO  170
        DO 10 N=1,NM1                                                   RKO  180
          DF = (RAD(I+1)-RAD(I)) * (PHI(N+1)-PHI(N)) / 4.               RKO  190
          GOTO(11,20,30),IG                                             RKO  200
   20     DF = DF * RADP(I)                                             RKO  210
          GOTO 11                                                       RKO  220
   30     DF = DF * 3.1416 * (RAD(I+1)+RAD(I))                          RKO  230
   11     AU(I,N) = DF                                                  RKO  240
   10 CONTINUE                                                          RKO  250
      RETURN                                                            RKO  260
      END                                                               RKO  270
      FUNCTION WKN(I,N,WT,KOM,IFTV)                                     WKN   10
C                                                                       WKN   20
C     ERKENNT ELEMENTE MIT KONVEKTIVEN WAERMEQUELLEN                    WKN   30
C                                                                       WKN   40
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             WKN   50
C                                                                       WKN   60
      DIMENSION WT(IMAZ,NMAZ),KOM(IMAZ,NMAZ),IFTV(KMAZ)                 WKN   70
C                                                                       WKN   80
C                                                                       WKN   90
      WKN = 0.                                                          WKN  100
C                                                                       WKN  110
      K = KOM(I,N)                                                      WKN  120
      IFT = IFTV(K) + 2                                                 WKN  130
      GOTO(20,30),IFT                                                   WKN  140
      GOTO 30                                                           WKN  150
C                                                                       WKN  160
   20 WKN = WT(I,N)                                                     WKN  170
C                                                                       WKN  180
   30 RETURN                                                            WKN  190
      END                                                               WKN  200
      SUBROUTINE WKAP(DTEM1,NHMAT2,NHZON,VOLK,ZKUG,VOLZ,IFBH,WKAPH,KKB, WKA   10
     1 KOM,AU,BR,RHO,C,IFBER,T,WT,IFWKT,THETNEW)                        WKA   20
C                                                                       WKA   30
CFZJ042                                                       09.09.05  WKA   40
C                                                                       WKA   50
C     BERECHNET WAERMEKAPAZITAET/ZEIT FUER MASCHEN UND KUGELZONEN       WKA   60
C                                                                       WKA   70
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                WKA   80
C                                                                       WKA   90
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    WKA  100
C                                                                       WKA  110
CFZJ042                                                       09.09.05  WKA  120
      COMMON /BLINDL/ TMITL,M24,NGEOM,CIZET0                            WKA  130
C                                                                       WKA  140
CFZJ055                                                       25.09.07  WKA  150
C                                                                       WKA  160
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             WKA  170
C                                                                       WKA  180
CFZJ042                                                       09.09.05  WKA  190
      COMMON /BLF/ IPASS                                                WKA  200
C                                                                       WKA  210
CFZJ042                                                       09.09.05  WKA  220
      DIMENSION NHMAT2(KMAZ,5),NHZON(KMAZ),VOLK(KMAZ,5),ZKUG(ICO,NCO),  WKA  230
     1 VOLZ(KMAZ,5),IFBH(ICO,NCO),WKAPH(ICO,NCO,5),KKB(ICO,NCO),        WKA  240
     2 KOM(IMAZ,NMAZ),AU(IMAZ,NMAZ),BR(IMAZ,NMAZ),RHO(KMAZ),C(KMAZ),    WKA  250
     3 IFBER(IMAZ,NMAZ),T(IMAZ,NMAZ),WT(IMAZ,NMAZ),IFWKT(KMAZ),WKPH(5), WKA  260
     4 THETNEW(ICO,NCO,5,15)                                            WKA  270
C                                                                       WKA  280
C                                                                       WKA  290
CFZJ042                                                       09.09.05  WKA  300
      IFPRNT = 0                                                        WKA  310
      IM1 = IMAX - 1                                                    WKA  320
      NM1 = NMAX - 1                                                    WKA  330
      DO 10 I=1,IM1                                                     WKA  340
        DO 10 N=1,NM1                                                   WKA  350
          IF3 = 16                                                      WKA  360
          K = KOM(I,N)                                                  WKA  370
          IFZ = 0                                                       WKA  380
          II = I - IDIFF                                                WKA  390
          NN = N - NDIFF                                                WKA  400
          IF(II .GT. 0 .AND. NN .GT. 0 .AND. II .LE. IMH .AND. NN .LE.  WKA  410
     1     NMH) IF3 = IFBH(II,NN)                                       WKA  420
          IFKT = IFWKT(K) + 1                                           WKA  430
          GOTO(30),IFKT                                                 WKA  440
          GOTO 20                                                       WKA  450
   30     WKAPZT = C(K)                                                 WKA  460
          IF(IF3 .EQ. 16) GOTO 1                                        WKA  470
          GOTO 50                                                       WKA  480
   20     IFKT = IFKT - 1                                               WKA  490
          IF(IFBER(I,N) .LT. 1 .AND. IFBER(I+1,N) .LT. 1 .AND.          WKA  500
     1     IFBER(I,N+1) .LT. 1 .AND. IFBER(I+1,N+1) .LT. 1) GOTO 50     WKA  510
          TM = (T(I,N)+T(I,N+1)+T(I+1,N)+T(I+1,N+1)) / 4.               WKA  520
C                                                                       WKA  530
          WKAPZT = WKPT(TM,IFKT,IFPRNT)                                 WKA  540
C                                                                       WKA  550
   50     IF(IF3 .EQ. 16) GOTO 1                                        WKA  560
          IF(IF3 .EQ. 0) GOTO 1                                         WKA  570
          KK1 = KKB(II,NN)                                              WKA  580
          NZ = NHZON(KK1)                                               WKA  590
CFZJ042                                                       09.09.05  WKA  600
          DO 100 NZZ=1,NZ                                               WKA  610
            MAT = NHMAT2(KK1,NZZ)                                       WKA  620
            IF(MAT) 6,5,6                                               WKA  630
    5       WKHAT = C(KK1)                                              WKA  640
            GOTO 7                                                      WKA  650
    6       IF(NZZ .EQ. NZ) IFZ = 1                                     WKA  660
            IF(IFZ .EQ. 1) GOTO 200                                     WKA  670
            TM = (THETNEW(II,NN,NZZ,1)+THETNEW(II,NN,NZZ+1,1)) / 2.     WKA  680
            GOTO 201                                                    WKA  690
  200       TM = THETNEW(II,NN,NZZ,1)                                   WKA  700
  201       CONTINUE                                                    WKA  710
            MAT = NHMAT2(KK1,NZZ)                                       WKA  720
C                                                                       WKA  730
            WKHAT = WKPT(TM,MAT,IFPRNT)                                 WKA  740
C                                                                       WKA  750
    7       WKPH(NZZ) = WKHAT                                           WKA  760
  100     CONTINUE                                                      WKA  770
CFZJ042                                                       09.09.05  WKA  780
          DO 101 NZZ=2,NZ                                               WKA  790
            WKAPH(II,NN,NZZ) = (WKPH(NZZ-1)*VOLZ(KK1,NZZ-1)+WKPH(NZZ)*  WKA  800
     1       VOLK(KK1,NZZ)) / DTEM1                                     WKA  810
  101     CONTINUE                                                      WKA  820
CFZJ042                                                       09.09.05  WKA  830
          WKAPH(II,NN,1) = VOLK(KK1,1) * WKPH(1) / DTEM1                WKA  840
    1     WT(I,N) = AU(I,N) * RHO(K) * WKAPZT                           WKA  850
   10 CONTINUE                                                          WKA  860
      IF(IMH .LT. 1) GOTO 12                                            WKA  870
      DO 11 I1=1,IMH                                                    WKA  880
        DO 11 N1=1,NMH                                                  WKA  890
          IF3 = IFBH(I1,N1)                                             WKA  900
          IF(IF3 .LE. 1) GOTO 11                                        WKA  910
          WKMI = 0.                                                     WKA  920
          I = I1 + IDIFF                                                WKA  930
          N = N1 + NDIFF                                                WKA  940
          IF(I .NE. 1 .AND. (IF3 .EQ. 2 .OR. IF3 .EQ. 5 .OR. IF3 .EQ. 6 WKA  950
     1     .OR. IF3 .EQ. 9 .OR. IF3 .EQ. 10 .OR. IF3 .EQ. 13 .OR. IF3   WKA  960
     2     .EQ. 14)) WKMI = WKMI + WT(I-1,N-1) / ZKUG(I1,N1)            WKA  970
          IF(IF3 .EQ. 3 .OR. IF3 .EQ. 5 .OR. IF3 .EQ. 7 .OR. IF3 .EQ. 9 WKA  980
     1     .OR. IF3 .EQ. 11 .OR. IF3 .EQ. 13 .OR. IF3 .EQ. 15) WKMI =   WKA  990
     2     WKMI + WT(I,N-1) / ZKUG(I1,N1)                               WKA 1000
          IF(I .NE. 1 .AND. (IF3 .EQ. 4 .OR. IF3 .EQ. 6 .OR. IF3 .EQ. 7 WKA 1010
     1     .OR. IF3 .EQ. 9 .OR. IF3 .EQ. 12 .OR. IF3 .EQ. 14 .OR. IF3   WKA 1020
     2     .EQ. 15)) WKMI = WKMI + WT(I-1,N) / ZKUG(I1,N1)              WKA 1030
          IF(IF3 .EQ. 8 .OR. IF3 .EQ. 10 .OR. IF3 .EQ. 11 .OR. IF3 .EQ. WKA 1040
     1     12 .OR. IF3 .EQ. 13 .OR. IF3 .EQ. 14 .OR. IF3 .EQ. 15) WKMI =WKA 1050
     2     WKMI + WT(I,N) / ZKUG(I1,N1)                                 WKA 1060
CFZJ042                                                       09.09.05  WKA 1070
          WKAPH(I1,N1,1) = WKAPH(I1,N1,1) + WKMI / DTEM1                WKA 1080
   11 CONTINUE                                                          WKA 1090
   12 CONTINUE                                                          WKA 1100
      DO 40 I=2,IM1                                                     WKA 1110
        DO 40 N=2,NM1                                                   WKA 1120
          BR(I,N) = (WT(I-1,N-1)+WT(I-1,N)+WT(I,N-1)+WT(I,N)) / DTEM1 * WKA 1130
     1     2.                                                           WKA 1140
   40 CONTINUE                                                          WKA 1150
      DO 41 N=2,NM1                                                     WKA 1160
        BR(1,N) = (WT(1,N-1)+WT(1,N)) / DTEM1 * 2.                      WKA 1170
   41 CONTINUE                                                          WKA 1180
      RETURN                                                            WKA 1190
      END                                                               WKA 1200
      FUNCTION WKPT(TT,IFKT,IFPRNT)                                     WKP   10
C                                                                       WKP   20
C     BERECHNET RHO*C FUER VERSCHIEDENE MATERIALIEN (TEMPERATURAB-      WKP   30
C     HAENGIG)                                                          WKP   40
C     FUER AL2O3 TEMPERATURUNABHAENGIG!                                 WKP   50
C                                                                       WKP   60
CFZJ025                                                       10.03.04  WKP   70
      REAL*8 DCPT,DTM                                                   WKP   80
C                                                                       WKP   90
 2002 FORMAT (' **ERROR** FOR C(T)=',I5,' NO EXISTING RELATION FOR CALCUWKP  100
     1LATION OF THERMAL CAPACITY.'/11X,'FOR TRANSIENT CALCULATION THE PRWKP  110
     2OGRAM INTERRUPTS')                                                WKP  120
 2011 FORMAT (3X,I4,1X,'STEATIT (MAGNESIUM-SILICATE)                    WKP  130
     1  2.7*(0.7641+.00157*T)')                                         WKP  140
 2012 FORMAT (3X,I4,1X,'REACTOR GRAPHITE, SGL GRADE A, NBG10            WKP  150
     1   ',1PE8.2,'*(.035*(T+200.1)+2092.3*(1-EXP(-0.00192*(T+200.1)))) WKP  160
     2         2500')                                                   WKP  170
 2021 FORMAT (3X,I4,1X,'CORE BARREL, SA-240, GRADE 316                  WKP  180
     1   ',F4.2,'*(.4773+0.4852E-3*T-0.6278E-6*T**2+0.3947E-9*T**3)     WKP  190
     2        20  800')                                                 WKP  200
 2031 FORMAT (3X,I4,1X,'PRESSURE VESSEL, SA-508                         WKP  210
     1   ',F4.2,'*(.4365+0.4467E-3*T-0.0858E-6*T**2)                    WKP  220
     2        21  427')                                                 WKP  230
 2071 FORMAT (3X,I4,1X,'REACTOR GRAPHITE (HRB)                          WKP  240
     1   ',F4.2,'*(.645+3.14E-3*T-2.809E-6*T**2+.959E-9*T**3)           WKP  250
     2           1200')                                                 WKP  260
 2081 FORMAT (3X,I4,1X,'CARBON BRICKS                                   WKP  270
     1   1.55*(.645+3.14E-3*T-2.809E-6*T**2+.959E-9*T**3)               WKP  280
     2       1200')                                                     WKP  290
 2111 FORMAT (3X,I4,1X,'V2A-STEEL (HOESCH)                              WKP  300
     1   7.8 *(0.4533+3.82E-4*T)                                        WKP  310
     2     0  200')                                                     WKP  320
 2121 FORMAT (3X,I4,1X,'THERMAL SHIELD (HRB)                            WKP  330
     1   3.5+4.1E-3*T-1.1E-6*T**2')                                     WKP  340
 2161 FORMAT (3X,I4,1X,'ALUMINIUM-OXIDE                                 WKP  350
     1   3.392  (CONSTANT)')                                            WKP  360
C                                                                       WKP  370
C                                                                       WKP  380
      WKPT = 0.                                                         WKP  390
      TM = TT                                                           WKP  400
      IF(IFPRNT .GT. 0) GOTO 2000                                       WKP  410
      GOTO(10,10,20,30,1000,60,70,80,1000,1000,110,120,70,70,70,160),   WKP  420
     1 IFKT                                                             WKP  430
      GOTO 1000                                                         WKP  440
   10 CONTINUE                                                          WKP  450
C                                                                       WKP  460
C     REACTOR GRAPHITE, SGL GRADE A, NBG 10                             WKP  470
CFZJ025                                                       10.03.04  WKP  480
      IF(TM .GT. 2500.) TM = 2500.                                      WKP  490
      DTM = DBLE(TM)                                                    WKP  500
C                                                                       WKP  510
      CALL DHEATCAP(DCPT,DTM)                                           WKP  520
C                                                                       WKP  530
C     Assume DCPT in J/(kg K) and convert to J/(cc K). Density 1.75 g/ccWKP  540
C                                                                       WKP  550
      IF(IFKT .EQ. 1) FAKT = 1.75E-03                                   WKP  560
      IF(IFKT .EQ. 2) FAKT = 1.80E-03                                   WKP  570
      WKPT = FAKT * SNGL(DCPT)                                          WKP  580
      GOTO 1000                                                         WKP  590
   20 CONTINUE                                                          WKP  600
C                                                                       WKP  610
C     CORE BARREL, SA-240 GRADE 316                                     WKP  620
C                                                                       WKP  630
      IF(TM .LT. 20.) TM = 20.                                          WKP  640
      IF(TM .GT. 800.) TM = 800.                                        WKP  650
      TM = TM / 1000.                                                   WKP  660
      WKPT = 0.4773 + (0.4852-(0.6278-0.3947*TM)*TM) * TM               WKP  670
      WKPT = WKPT * 7.8                                                 WKP  680
      GOTO 1000                                                         WKP  690
   30 CONTINUE                                                          WKP  700
C                                                                       WKP  710
C     PRESSURE VESSEL, SA-508                                           WKP  720
C                                                                       WKP  730
      IF(TM .LT. 21.) TM = 21.                                          WKP  740
      IF(TM .GT. 427.) TM = 427.                                        WKP  750
      TM = TM / 1000.                                                   WKP  760
      WKPT = 0.4365 + (0.4467-0.0858*TM) * TM                           WKP  770
      WKPT = WKPT * 7.8                                                 WKP  780
      GOTO 1000                                                         WKP  790
   60 CONTINUE                                                          WKP  800
C                                                                       WKP  810
C     STEATIT (MAGNESIUM-SILIKAT)                                       WKP  820
C                                                                       WKP  830
      WKPT = 0.7641 + 0.00157 * TM                                      WKP  840
C                                                                       WKP  850
      WKPT = WKPT * 2.7                                                 WKP  860
C                                                                       WKP  870
      GOTO 1000                                                         WKP  880
   70 CONTINUE                                                          WKP  890
C                                                                       WKP  900
C     REAKTORGRAPHIT, RHO*C / NACH HRB                                  WKP  910
C                                                                       WKP  920
      FAK = 1.75                                                        WKP  930
      IF(IFKT .EQ. 13) FAK = 1.70                                       WKP  940
      IF(IFKT .EQ. 14) FAK = 1.60                                       WKP  950
      IF(IFKT .EQ. 15) FAK = 1.80                                       WKP  960
      IF(TM .GT. 1200.) TM = 1200.                                      WKP  970
      TM = TM / 1000.                                                   WKP  980
      TM3 = TM**3.                                                      WKP  990
C                                                                       WKP 1000
      WKPT = .645 + 3.14 * TM - 2.809 * TM * TM +.959 * TM3             WKP 1010
C                                                                       WKP 1020
      WKPT = WKPT * FAK                                                 WKP 1030
C                                                                       WKP 1040
      GOTO 1000                                                         WKP 1050
   80 CONTINUE                                                          WKP 1060
C                                                                       WKP 1070
C     KOHLESTEIN, WIE REAKTORGRAPHIT, ABER MIT RHO = 1.55               WKP 1080
C                                                                       WKP 1090
      IF(TM .GT. 1200.) TM = 1200.                                      WKP 1100
      TM = TM / 1000.                                                   WKP 1110
      TM3 = TM**3.                                                      WKP 1120
C                                                                       WKP 1130
      WKPT = .645 + 3.14 * TM - 2.809 * TM * TM + .959 * TM3            WKP 1140
C                                                                       WKP 1150
      WKPT = WKPT * 1.55                                                WKP 1160
C                                                                       WKP 1170
      GOTO 1000                                                         WKP 1180
  110 CONTINUE                                                          WKP 1190
C                                                                       WKP 1200
C     V2A - STAHL (DIN 4541) (HOESCH-INFORMATION)                       WKP 1210
C                                                                       WKP 1220
      IF(TM .LT. 0.) TM = 0.                                            WKP 1230
      IF(TM .GT. 200.) TM = 200.                                        WKP 1240
C                                                                       WKP 1250
      WKPT = 0.4533 + 3.82E-04 * TM                                     WKP 1260
C                                                                       WKP 1270
      WKPT = WKPT * 7.8                                                 WKP 1280
C                                                                       WKP 1290
      GOTO 1000                                                         WKP 1300
  120 CONTINUE                                                          WKP 1310
C                                                                       WKP 1320
C     THERMISCHER SCHILD, RHO*C / NACH HRB                              WKP 1330
C                                                                       WKP 1340
      TM3 = TM / 100.                                                   WKP 1350
      TM3 = TM3**2.                                                     WKP 1360
C                                                                       WKP 1370
      WKPT = 3.5 + 0.41 * (TM/100.) - 0.011 * TM3                       WKP 1380
C                                                                       WKP 1390
      GOTO 1000                                                         WKP 1400
  160 CONTINUE                                                          WKP 1410
C                                                                       WKP 1420
C     ALUMINIUM-OXYD (TEMPERATURUNABHAENGIG)                            WKP 1430
C                                                                       WKP 1440
      WKPT = 3.392                                                      WKP 1450
C                                                                       WKP 1460
 1000 CONTINUE                                                          WKP 1470
      RETURN                                                            WKP 1480
 2000 CONTINUE                                                          WKP 1490
      GOTO(11,11,21,31,2001,61,71,81,2001,2001,111,121,71,71,71,161),   WKP 1500
     1 IFKT                                                             WKP 1510
      GOTO 2001                                                         WKP 1520
   11 CONTINUE                                                          WKP 1530
      IF(IFKT .EQ. 1) FAKT = 1.75E-03                                   WKP 1540
      IF(IFKT .EQ. 2) FAKT = 1.80E-03                                   WKP 1550
      WRITE (6,2012) IFKT,FAKT                                          WKP 1560
      IFPRNT = -1                                                       WKP 1570
      RETURN                                                            WKP 1580
   21 CONTINUE                                                          WKP 1590
      FAK = 7.8                                                         WKP 1600
      WRITE (6,2021) IFKT,FAK                                           WKP 1610
      IFPRNT = -1                                                       WKP 1620
      RETURN                                                            WKP 1630
   31 CONTINUE                                                          WKP 1640
      FAK = 7.8                                                         WKP 1650
      WRITE (6,2031) IFKT,FAK                                           WKP 1660
      IFPRNT = -1                                                       WKP 1670
      RETURN                                                            WKP 1680
   61 CONTINUE                                                          WKP 1690
      WRITE (6,2011) IFKT                                               WKP 1700
      IFPRNT = -1                                                       WKP 1710
      RETURN                                                            WKP 1720
   71 CONTINUE                                                          WKP 1730
      FAK = 1.75                                                        WKP 1740
      IF(IFKT .EQ. 13) FAK = 1.70                                       WKP 1750
      IF(IFKT .EQ. 14) FAK = 1.60                                       WKP 1760
      IF(IFKT .EQ. 15) FAK = 1.80                                       WKP 1770
      WRITE (6,2071) IFKT,FAK                                           WKP 1780
      IFPRNT = -1                                                       WKP 1790
      RETURN                                                            WKP 1800
   81 CONTINUE                                                          WKP 1810
      WRITE (6,2081) IFKT                                               WKP 1820
      IFPRNT = -1                                                       WKP 1830
      RETURN                                                            WKP 1840
  111 CONTINUE                                                          WKP 1850
      WRITE (6,2111) IFKT                                               WKP 1860
      IFPRNT = -1                                                       WKP 1870
      RETURN                                                            WKP 1880
  121 CONTINUE                                                          WKP 1890
      WRITE (6,2121) IFKT                                               WKP 1900
      IFPRNT = -1                                                       WKP 1910
      RETURN                                                            WKP 1920
  161 CONTINUE                                                          WKP 1930
      WRITE (6,2161) IFKT                                               WKP 1940
      IFPRNT = -1                                                       WKP 1950
      RETURN                                                            WKP 1960
 2001 CONTINUE                                                          WKP 1970
      IF(IFPRNT .EQ. 2) GOTO 5002                                       WKP 1980
      WRITE (6,2002) IFKT                                               WKP 1990
C                                                                       WKP 2000
      CALL ABEND(3)                                                     WKP 2010
C                                                                       WKP 2020
 5002 IFPRNT = -1                                                       WKP 2030
      RETURN                                                            WKP 2040
      END                                                               WKP 2050
      SUBROUTINE WPKON(IFKO1,AU,BU,T,WT,TFLU,BU1,KOM,IFTV)              WPK   10
C                                                                       WPK   20
C     BERECHNET DIE KONVEKTIVE QUELLE AUS QUELLDICHTE UND ZUGEORDNETEM  WPK   30
C     VOLUMEN ( ---> WKN ).                                             WPK   40
C     *** ACHTUNG ***  AUF KONSISTENZ DER STEUERGROESSEN IFTV (THERMIX) WPK   50
C     UND IFBQ (KONVEK,SR QUELLE) ACHTEN                                WPK   60
C                                                                       WPK   70
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    WPK   80
C                                                                       WPK   90
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             WPK  100
C                                                                       WPK  110
      DIMENSION AU(IMAZ,NMAZ),BU(IMAZ,NMAZ),T(IMAZ,NMAZ),WT(IMAZ,NMAZ), WPK  120
     1 TFLU(IMAZ,NMAZ),BU1(IMAZ,NMAZ),KOM(IMAZ,NMAZ),IFTV(KMAZ)         WPK  130
C                                                                       WPK  140
C                                                                       WPK  150
      IM1 = IMAX - 1                                                    WPK  160
      NM1 = NMAX - 1                                                    WPK  170
      DO 5 I=1,IM1                                                      WPK  180
        DO 5 N=1,NM1                                                    WPK  190
          WT(I,N) = AU(I,N)                                             WPK  200
    5 CONTINUE                                                          WPK  210
      DO 10 I=1,IM1                                                     WPK  220
        DO 10 N=2,NM1                                                   WPK  230
          IF(I .EQ. 1) GOTO 11                                          WPK  240
C                                                                       WPK  250
          A1 = WKN(I-1,N-1,WT,KOM,IFTV)                                 WPK  260
C                                                                       WPK  270
          A2 = WKN(I-1,N,WT,KOM,IFTV)                                   WPK  280
C                                                                       WPK  290
   11     CONTINUE                                                      WPK  300
C                                                                       WPK  310
          A3 = WKN(I,N-1,WT,KOM,IFTV)                                   WPK  320
C                                                                       WPK  330
          A4 = WKN(I,N,WT,KOM,IFTV)                                     WPK  340
C                                                                       WPK  350
          A = A3 + A4                                                   WPK  360
          IF(I .GT. 1) A = A + A1 + A2                                  WPK  370
          BU(I,N) = A * BU(I,N)                                         WPK  380
          IF(IFKO1 .NE. -1) GOTO 10                                     WPK  390
          TT = TFLU(I,N) - T(I,N)                                       WPK  400
          TT = ABS(TT)                                                  WPK  410
          IF(TT .LT. 1.0) GOTO 1310                                     WPK  420
          BU(I,N) = BU(I,N) / (TFLU(I,N)-T(I,N))                        WPK  430
C                                                                       WPK  440
C     BERECHNUNG DES AEQIVALENTEN ALPHA*F AUS QUELLE UND                WPK  450
C     AEQUIVALENTER GASTEMPERATUR (GEBILDET IN SR QUELLE)               WPK  460
C                                                                       WPK  470
          GOTO 10                                                       WPK  480
 1310     BU(I,N) = A * BU1(I,N)                                        WPK  490
C                                                                       WPK  500
C     AUF BU1 STEHEN INTERPOLIERTE KONVEK ALPHA*F/VOL, WELCHE           WPK  510
C     VERWENDET WERDEN, WENN THERMIX KEINE ALPHA*F DEF. KANN,           WPK  520
C     WEIL TEMPERATURDIFFERENZ ZU KLEIN                                 WPK  530
C                                                                       WPK  540
   10 CONTINUE                                                          WPK  550
      RETURN                                                            WPK  560
      END                                                               WPK  570
      SUBROUTINE WDUKON(IFRED,KOM,AU,DU,E)                              WDU   10
C                                                                       WDU   20
C                                                                       WDU   30
      COMMON /REG/ IMAX,NMAX,RAD0,PHI0,IFRFI,IFRFA,IFRFL,IFRFR,IFREF    WDU   40
C                                                                       WDU   50
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             WDU   60
C                                                                       WDU   70
      DIMENSION KOM(IMAZ,NMAZ),AU(IMAZ,NMAZ),DU(IMAZ,NMAZ),E(IMAZ,NMAZ) WDU   80
C                                                                       WDU   90
C                                                                       WDU  100
      N51 = 51                                                          WDU  110
      IM1 = IMAX - 1                                                    WDU  120
      NM1 = NMAX - 1                                                    WDU  130
      ICOR = 1                                                          WDU  140
      IFR = IABS(IFRED) + 1                                             WDU  150
      GOTO(8,7,7,8),IFR                                                 WDU  160
      DO 5 I=1,IM1                                                      WDU  170
        DO 5 N=2,NM1                                                    WDU  180
          A = 0.                                                        WDU  190
          IF(KOM(I,N) .EQ. ICOR) A = A + AU(I,N)                        WDU  200
          IF(KOM(I,N-1) .EQ. ICOR) A = A + AU(I,N-1)                    WDU  210
          IF(I .EQ. 1) GOTO 6                                           WDU  220
          IF(KOM(I-1,N) .EQ. ICOR) A = A + AU(I-1,N)                    WDU  230
          IF(KOM(I-1,N-1) .EQ. ICOR) A = A + AU(I-1,N-1)                WDU  240
    6     CONTINUE                                                      WDU  250
          IF(A .GT. 0.) A = 1. / A                                      WDU  260
          DU(I,N) = DU(I,N) * A                                         WDU  270
    5 CONTINUE                                                          WDU  280
      GOTO 99                                                           WDU  290
    7 CONTINUE                                                          WDU  300
      DO 1 I=1,IM1                                                      WDU  310
        DO 1 N=2,NM1                                                    WDU  320
          IF(KOM(I,N) .NE. ICOR) GOTO 1                                 WDU  330
          X = 0.                                                        WDU  340
          DO 2 II=1,2                                                   WDU  350
            DO 2 NN=1,2                                                 WDU  360
              I1 = I + II - 1                                           WDU  370
              N1 = N + NN - 1                                           WDU  380
              A = 0.                                                    WDU  390
              DO 3 III=1,2                                              WDU  400
                DO 3 NNN=1,2                                            WDU  410
                  I2 = I1 + III - 2                                     WDU  420
                  N2 = N1 + NNN - 2                                     WDU  430
                  IF(I2 .LT. 1) GOTO 3                                  WDU  440
                  IF(KOM(I2,N2) .EQ. ICOR) A = A + AU(I2,N2)            WDU  450
    3         CONTINUE                                                  WDU  460
              X = DU(I1,N1) * AU(I,N) / A + X                           WDU  470
    2     CONTINUE                                                      WDU  480
C                                                                       WDU  490
C     DU(I,N) = LEISTUNGSDICHTE IM THERMIX-KAESTCHEN I,N                WDU  500
C                                                                       WDU  510
          DU(I,N) = X / (4.*AU(I,N))                                    WDU  520
    1 CONTINUE                                                          WDU  530
      GOTO 99                                                           WDU  540
    8 CONTINUE                                                          WDU  550
      REWIND N51                                                        WDU  560
      READ (N51) DU                                                     WDU  570
      IF(IFRED .NE. 3) GOTO 10                                          WDU  580
      READ (N51) E                                                      WDU  590
      DO 9 I=1,IM1                                                      WDU  600
        DO 9 N=2,NM1                                                    WDU  610
          DU(I,N) = DU(I,N) + E(I,N)                                    WDU  620
    9 CONTINUE                                                          WDU  630
      BACKSPACE N51                                                     WDU  640
   10 CONTINUE                                                          WDU  650
   99 CONTINUE                                                          WDU  660
      RETURN                                                            WDU  670
      END                                                               WDU  680
      FUNCTION XLAM(I,N,XLAMS,ISO,ISU,NSL,NSR,KSTR,DOS,ZDOS,EPS,IDIR,   XLA   10
     1 IFLT,RADP,T,RAD,PHI,KOM,LAM,DOSI,FFTHX,TCOND)                    XLA   20
C                                                                       XLA   30
CFZJ026                                                       16.03.04  XLA   40
C                                                                       XLA   50
C     BERECHNET WAERMELEITFAEHIGKEITEN                                  XLA   60
C                                                                       XLA   70
      COMMON /ITER1/ TRMAX,MIT,TN,OVREL,ITMAX,T1,T2,TRELA,NTMAX,IFKOR,  XLA   80
     1 ETHA,ORMIN,MITMAX,IKORM,IFREL                                    XLA   90
C                                                                       XLA  100
      COMMON /PRINT1/ TITLE(20),INDGEO                                  XLA  110
C                                                                       XLA  120
      COMMON /BASIS/ TBASE                                              XLA  130
C                                                                       XLA  140
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             XLA  150
C                                                                       XLA  160
CFZJ026                                                       16.03.04  XLA  170
      COMMON /YEAR/ IYEAR,RLAM32                                        XLA  180
C                                                                       XLA  190
      DIMENSION ISO(NMAZ,19),ISU(NMAZ,19),NSL(IMAZ,19),NSR(IMAZ,19),    XLA  200
     1 KSTR(KMAZ),DOS(KMAZ),ZDOS(NMAZ),EPS(KMAZ),IDIR(KMAZ),IFLT(KMAZ), XLA  210
     2 RADP(IMAZ),T(IMAZ,NMAZ),RAD(IMAZ+1),PHI(NMAZ+1),KOM(IMAZ,NMAZ),  XLA  220
     3 DOSI(IMAZ,NMAZ),FFTHX(IMAZ,NMAZ),TCOND(IMAZ,NMAZ),IYEAR(1000)    XLA  230
C                                                                       XLA  240
      REAL LAM(KMAZ)                                                    XLA  250
C                                                                       XLA  260
  901 FORMAT (' **ATTENTION** IN COLUMN',I4,' ROW',I4,' COMPOSITION',I4,XLA  270
     1 ' IS TEMP. = 1. USED, BECAUSE LESS THAN -272 C.')                XLA  280
C                                                                       XLA  290
C                                                                       XLA  300
      IFLTX = 0                                                         XLA  310
      IFPRNT = 0                                                        XLA  320
      K = KOM(I,N)                                                      XLA  330
      TM = (T(I,N)+T(I,N+1)+T(I+1,N)+T(I+1,N+1)) / 4. + TBASE           XLA  340
      IFLTK = IFLT(K)                                                   XLA  350
      IF(IFLTK .EQ. 25 .OR. IFLTK .EQ. 26 .OR. IFLTK .EQ. 27) IFLTX = 1 XLA  360
      IF(IFLTX .EQ. 1) DO = DOSI(I,N)                                   XLA  370
CFZJ026                                                       16.03.04  XLA  380
      IF(IFLTX .NE. 1) DO = FFTHX(I,N) * IYEAR(K)                       XLA  390
      IF(IFLTX .EQ. 1) TIRR = 950.                                      XLA  400
      IF(IFLTX .NE. 1) TIRR = TCOND(I,N)                                XLA  410
      ALAM0 = LAM(K)                                                    XLA  420
CFZJ027     MODIFY HEAT CONDUCTIVITY                          31.03.04  XLA  430
      RLAM32 = 0.                                                       XLA  440
      IF(IFLTK .EQ. 32) RLAM32 = LAM(K)                                 XLA  450
C                                                                       XLA  460
      CALL SLAMT(TM,DO,IFLTK,ALAM0,IFPRNT,XLAMT,TIRR)                   XLA  470
C                                                                       XLA  480
      XLAM = XLAMT                                                      XLA  490
C                                                                       XLA  500
      XLAMS = 0.                                                        XLA  510
      IF(EPS(K) .EQ. 0.) GOTO 1100                                      XLA  520
      KS = KSTR(K)                                                      XLA  530
      IDR = IDIR(K) + 1                                                 XLA  540
      GOTO(1005,1010),IDR                                               XLA  550
 1005 CONTINUE                                                          XLA  560
C                                                                       XLA  570
C     STRAHLUNG IN I-RICHTUNG                                           XLA  580
C                                                                       XLA  590
      I1 = ISO(N,KS)                                                    XLA  600
      I2 = ISU(N,KS)                                                    XLA  610
      T1 = (T(I1,N)+T(I1,N+1)) / 2. + 273.                              XLA  620
      T2 = (T(I2,N)+T(I2,N+1)) / 2. + 273.                              XLA  630
      BS = RAD(I2) - RAD(I1)                                            XLA  640
      GEOM = RAD(I1) * ALOG(RAD(I2) / RAD(I1))                          XLA  650
      GOTO 1020                                                         XLA  660
 1010 CONTINUE                                                          XLA  670
C                                                                       XLA  680
C     STRAHLUNG IN N-RICHTUNG                                           XLA  690
C                                                                       XLA  700
      N1 = NSL(I,KS)                                                    XLA  710
      N2 = NSR(I,KS)                                                    XLA  720
      T1 = (T(I,N1)+T(I+1,N1)) / 2. + 273.                              XLA  730
      T2 = (T(I,N2)+T(I+1,N2)) / 2. + 273.                              XLA  740
      BS = PHI(N2) - PHI(N1)                                            XLA  750
      IF(INDGEO .NE. 1) GOTO 1020                                       XLA  760
      BS = BS * RADP(I)                                                 XLA  770
 1020 T1 = T1 + TBASE                                                   XLA  780
      T2 = T2 + TBASE                                                   XLA  790
      T14 = T1 / 100.                                                   XLA  800
      IF(T1-T2) 1021,1022,1021                                          XLA  810
 1021 T14 = T14**4.                                                     XLA  820
      T24 = T2 / 100.                                                   XLA  830
      T24 = T24**4.                                                     XLA  840
      TKK = (T14-T24) / (T1-T2)                                         XLA  850
      GOTO 1023                                                         XLA  860
 1022 T14 = T14**3.                                                     XLA  870
      TKK = .04 * T14                                                   XLA  880
 1023 CONTINUE                                                          XLA  890
      IF(IFLTK .NE. 4) GOTO 1030                                        XLA  900
      P = LAM(K)                                                        XLA  910
      DT = ABS(T1-T2)                                                   XLA  920
      TM = (T1+T2) / 2.                                                 XLA  930
      BSM = BS / 100.                                                   XLA  940
C                                                                       XLA  950
C     ERSATZWAERMELEITF. FUER FREIE KONVEKTION IM SENKRECHTEN EBENEN    XLA  960
C     SPALT                                                             XLA  970
C     FUER HELIUM / LAM(K) IST DRUCK (BAR)                              XLA  980
C                                                                       XLA  990
      IF(TM .GT. -273.) GOTO 900                                        XLA 1000
      TM = -272.                                                        XLA 1010
      WRITE (6,901) I,N,K                                               XLA 1020
  900 CONTINUE                                                          XLA 1030
      PR = 0.66                                                         XLA 1040
      TM1 = TM + 273.                                                   XLA 1050
      TM11 = TM1**1.2                                                   XLA 1060
      XRHO = 48.14 * P / TM1 / (1.+.4446*P/TM11)                        XLA 1070
      TM11 = TM1**.7                                                    XLA 1080
      XETA = 3.674E-7 * TM11                                            XLA 1090
      TM11 = TM1**.71                                                   XLA 1100
      XXLAM = 2.682E-3 * TM11                                           XLA 1110
      BSM3 = BSM**3.                                                    XLA 1120
      XX = XRHO / XETA                                                  XLA 1130
      XX = XX**2.                                                       XLA 1140
      GP = 9.81 * BSM3 * DT / TM1 * PR * XX                             XLA 1150
      GP3 = GP**.306                                                    XLA 1160
      XNU = 0.096 * GP3                                                 XLA 1170
C                                                                       XLA 1180
      XLAM = XNU * XXLAM                                                XLA 1190
C                                                                       XLA 1200
      XLAM = XLAM / 100. + XXLAM / 100.                                 XLA 1210
C                                                                       XLA 1220
 1030 CONTINUE                                                          XLA 1230
      IF(IDR .EQ. 1) BS = GEOM                                          XLA 1240
      XLAMS = 0.                                                        XLA 1250
      IF(EPS(K) .GT. 0.) XLAMS = 5.67E-4 * BS / (2./EPS(K)-1.) * TKK    XLA 1260
 1100 CONTINUE                                                          XLA 1270
      RETURN                                                            XLA 1280
      END                                                               XLA 1290
      FUNCTION XLAM1(I,N,DOS,ZDOS,IFLT,T,KOM,LAM)                       LAM   10
C                                                                       LAM   20
C     BERECHNET WAERMELEITFAEHIGKEITEN (ANISOTROP)                      LAM   30
C                                                                       LAM   40
      COMMON /BASIS/ TBASE                                              LAM   50
C                                                                       LAM   60
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             LAM   70
C                                                                       LAM   80
      DIMENSION DOS(KMAZ),ZDOS(NMAZ),IFLT(KMAZ),T(IMAZ,NMAZ),           LAM   90
     1 KOM(IMAZ,NMAZ)                                                   LAM  100
C                                                                       LAM  110
      REAL LAM(KMAZ)                                                    LAM  120
C                                                                       LAM  130
C                                                                       LAM  140
      IFPRNT = 0                                                        LAM  150
      K = KOM(I,N)                                                      LAM  160
      TM = (T(I,N)+T(I,N+1)+T(I+1,N)+T(I+1,N+1)) / 4. + TBASE           LAM  170
      IFLTK = IFLT(K)                                                   LAM  180
      DO = DOS(K)                                                       LAM  190
      IF(DO .LT. 0) DO = ZDOS(N)                                        LAM  200
      ALAM0 = LAM(K)                                                    LAM  210
C                                                                       LAM  220
      XLAM1 = XLAMT1(TM,DO,IFLTK,ALAM0,IFPRNT)                          LAM  230
C                                                                       LAM  240
      RETURN                                                            LAM  250
      END                                                               LAM  260
      SUBROUTINE SLAMT(TT,DO,MAT,ALAM0,IFPRNT,XLAMT,TIRR)               SLA   10
C                                                                       SLA   20
CFZJ026                                                       10.03.04  SLA   30
C                                                                       SLA   40
C     (OLD FUNCTION XLAMT(TT,DO,MAT,ALAM0,IFPRNT))                      SLA   50
C     BERECHNET TEMPERATURABHAENGIGE WAERMELEITFAEHIGKEITEN             SLA   60
C                                                                       SLA   70
      COMMON /YEAR/ IYEAR,RLAM32                                        SLA   80
C                                                                       SLA   90
      DIMENSION IYEAR(1000)                                             SLA  100
C                                                                       SLA  110
      DOUBLE PRECISION DVAL,DOSE,DTI,DTM                                SLA  120
C                                                                       SLA  130
 2011 FORMAT (3X,I4,1X,'SODIUM (LIQUID)                                 SLA  140
     1   (54.306-1.878E-2*(1.8*T+32.)+2.0914E-6*(1.8T+32)**2.)*.173E-1  SLA  150
     2    90 1300')                                                     SLA  160
 2021 FORMAT (3X,I4,1X,'MATRIX-GRAPHITE (T=IRRAD.TEMP)                  SLA  170
     1((-.3906E-4*T+.06829)/(DOSE+1.931E-4*T+.105)+1.228E-4*T+.042)*1.27SLA  180
     268 450 1300')                                                     SLA  190
 2031 FORMAT (3X,I4,1X,'REFLECTOR-GRAPHITE                              SLA  200
     1   USING INTERPOLATION (SEE IN SUBROUTINE  G F I T)               SLA  210
     2     0 2000')                                                     SLA  220
 2041 FORMAT (3X,I4,1X,'EPS = 0: STATIC HELIUM AT 1 BAR                 SLA  230
     1   2.97E-5*(T+273.)**.69+(9.23E7*T)/(T**5+4.29E14)                SLA  240
     2     0')                                                          SLA  250
 2042 FORMAT (3X,I4,1X,'EPS > 0: HELIUM (F(P))                          SLA  260
     1   CONDUCTION + CONVECTION + RADIATION                            SLA  270
     2     0')                                                          SLA  280
 2061 FORMAT (3X,I4,1X,'ZEHNER/SCHLUENDER FOR STEATIT                   SLA  290
     1   EXPERIMENT (K. VERFONDERN)')                                   SLA  300
 2071 FORMAT (3X,I4,1X,'REACTORGRAPH., LAMBDA(0)=LAM(COMP)              SLA  310
     1   LAMBDA(0)*(1.-1.084E-3*T+.743E-6*T**2-.213E-9*T**3)            SLA  320
     2       1700')                                                     SLA  330
 2081 FORMAT (3X,I4,1X,'CARBON BRICKS                                   SLA  340
     1   0.05+0.03E-3*T                                                 SLA  350
     2       1000')                                                     SLA  360
 2091 FORMAT (3X,I4,1X,'STATIC AIR AT 1 BAR                             SLA  370
     1   5.14E-6*(T+273.)**.7')                                         SLA  380
 2101 FORMAT (3X,I4,1X,'THERMAL SHIELD (HRB)                            SLA  390
     1   (48.4-0.0205*T)/100.')                                         SLA  400
 2111 FORMAT (3X,I4,1X,'V2A-STEEL (THYSSEN)                             SLA  410
     1   0.1517+1.33E-4*T                                               SLA  420
     2  -100  750')                                                     SLA  430
 2131 FORMAT (3X,I4,1X,'STEATIT                                         SLA  440
     1   0.0221+5.8125E-5*T')                                           SLA  450
 2141 FORMAT (3X,I4,1X,'ROBOLD-EXP. (GRAPH.BALLS)                       SLA  460
     1   LAMBDA(0)*(0.0162+1.131E-4*(T-390.))                           SLA  470
     2   390  680')                                                     SLA  480
 2151 FORMAT (3X,I4,1X,'ARMED-CONCRETE (INFO-ALTES, ISF)                SLA  490
     1   LAMBDA(0)*(0.0186-1.18E-5*(T+273.))                            SLA  500
     2        550')                                                     SLA  510
 2161 FORMAT (3X,I4,1X,'PRISMATIC CORE  (AXIAL DIRECTION)               SLA  520
     1   3.964E-15*T**4-1.620E-11*T**3+1.247E-8*T**2+1.243E-4*T+.1690') SLA  530
 2171 FORMAT (3X,I4,1X,'CARBON-FELT IN VACUUM                           SLA  540
     1   0.01163*(3.E-5*T+0.05)                                         SLA  550
     2     0 1000'/59X,                                                 SLA  560
     3  '0.01163*(1.17E-4*T-0.06)                                       SLA  570
     4  1000 1600'/59X,                                                 SLA  580
     5  '0.01163*(3.50E-4*T-0.41)                                       SLA  590
     6  1600')                                                          SLA  600
 2181 FORMAT (3X,I4,1X,'CARBON-FELT IN AR- OR N2-ATHMOSPH.              SLA  610
     1   0.01163*(8.E-5*T+0.07)                                         SLA  620
     2     0 1000'/59X,                                                 SLA  630
     3  '0.01163*(2.75E-4*T-0.14)                                       SLA  640
     4  1000 1400'/59X,                                                 SLA  650
     5  '0.01163*(6.50E-4*T-0.67)                                       SLA  660
     6  1400 1800'/59X,                                                 SLA  670
     7  '0.01163*(1.067E-4*T-1.4)                                       SLA  680
     8  1800')                                                          SLA  690
 2191 FORMAT (3X,I4,1X,'"WAELZLAGER"-STEEL 100CR6                       SLA  700
     1   2.7991E-12*T**4-1.9237E-9*T**3-1.2750E-7*T**2+5.4022E-5*T+.4170SLA  710
     2     0  550')                                                     SLA  720
 2201 FORMAT (3X,I4,1X,'STATIC NITROGEN                                 SLA  730
     1   4.373E-6*(T+273)**.715')                                       SLA  740
 2211 FORMAT (3X,I4,1X,'KAOWOOL-MAT IN AIR (JUEL-992RB)                 SLA  750
     1   2.43191E-9*T**2+8.42352E-7*T+3.55708E-4                        SLA  760
     2    50  600')                                                     SLA  770
 2221 FORMAT (3X,I4,1X,'H A W - GLAS                                    SLA  780
     1   (0.927+3.17106E-3*T-1.12195E-5*T**2+1.48244E-8*T**3)/100.      SLA  790
     2        550')                                                     SLA  800
 2231 FORMAT (3X,I4,1X,'PEBBLE BED (SCHUERENKRAEMER II.84)              SLA  810
     1   < 1300 : (2.549E-4*T**1.545+1.5)/100.                          SLA  820
     2   100')                                                          SLA  830
 2232 FORMAT (59X,                                                      SLA  840
     1  '> 1300 : (2.E-3*(T-135.)**1.287)/100.                          SLA  850
     2       2500')                                                     SLA  860
 2241 FORMAT (3X,I4,1X,'BALL-GRAPHITE                                   SLA  870
     1   EXPLICIT TEMP-DOSE-DEPENDENCE (BINKELE/A3-GRAPHITE)            SLA  880
     2   250')                                                          SLA  890
 2251 FORMAT (3X,I4,1X,'LAMBDA-EFF. PEBBLE BED                          SLA  900
     1   TEMP-DOSE-DEPENDENCE ACCORDING TO ROBOLD                       SLA  910
     2   100 2500')                                                     SLA  920
 2261 FORMAT (3X,I4,1X,'LAMBDA-EFF. PEBBLE BED                          SLA  930
     1   TEMP-DOSE-DEPENDENCE ACCORDING TO ZEHNER-SCHLUENDER            SLA  940
     2   100 2500')                                                     SLA  950
 2271 FORMAT (3X,I4,1X,'LAMBDA-EFF. PEBBLE BED                          SLA  960
     1   TEMP-DOSE-DEPENDENCE ACCORDING TO ZEHNER-SCHLUENDER AND ROBOLD SLA  970
     2   100 2500')                                                     SLA  980
 2281 FORMAT (3X,I4,1X,'LAMBDA OF AL2O3                                 SLA  990
     1   LINEAR FROM SALMANG/SCHOLZ "KERAMIK"                           SLA 1000
     2   100 1000')                                                     SLA 1010
 2291 FORMAT (3X,I4,1X,'GILSONIT COKE AGL-IE 1-24                       SLA 1020
     1   IRRADIATED AT 760 C (BINKELE)')                                SLA 1030
 2301 FORMAT (3X,I4,1X,'STAINLESS STEEL, SA240, GRADE 316               SLA 1040
     1   0.1297+0.1577E-3*T-0.0309*T**2                                 SLA 1050
     2    20  800')                                                     SLA 1060
 2311 FORMAT (3X,I4,1X,'PRESSURE VESSEL, SA508,GRADE 3,CLASS1           SLA 1070
     1   0.3721+0.2542E-3*T-1.1072E-6*T**2+0.9528E-9*T**3               SLA 1080
     2    21  427')                                                     SLA 1090
 2321 FORMAT (3X,I4,1X,'REACTOR GRAPHITE SGL GRADE A NBG10              SLA 1100
     1   "MARK MITCHELL" - CORRELATION                                  SLA 1110
     2       2500')                                                     SLA 1120
 4203 FORMAT (///'  ************  WARNING  ************'/' TEMPERATURE ISLA 1130
     1S OUTSIDE AVAILABLE RANGE FOR THERMAL CONDUCTIVITY OF SA-508: (21 SLA 1140
     2- 427 C). CLOSEST VALUE IS USED.'/                                SLA 1150
     3' (CHECK SUMMARY TABLES FOR DETAILS)'////)                        SLA 1160
 5001 FORMAT (' **WARNING** FOR LAM(T)=',I5,' NO EXISTING RELATION FOR CSLA 1170
     1ALCULATION OF THERMAL CONDUCTIVITY'/13X,'INPUT-VALUE OF LAMBDA IS SLA 1180
     2NOT CHANGED FOR FURTHER CALCULATIONS')                            SLA 1190
 6201 FORMAT (///' *********** WARNING ***********'/                    SLA 1200
     1 ' TEMPERATURE OF ',F8.2,' C IS OUTSIDE AVAILABLE RANGE FOR ',    SLA 1210
     2 'THERMAL CONDUCTIVITY OF SGL GRAPHITE (20-2000 C). CLOSEST ',    SLA 1220
     3 'VALUE IS USED'/)                                                SLA 1230
C                                                                       SLA 1240
C                                                                       SLA 1250
      IFLTK = MAT                                                       SLA 1260
      IFL1 = MAT                                                        SLA 1270
      IF(IFPRNT .GT. 0) GOTO 2000                                       SLA 1280
      TM = TT                                                           SLA 1290
      GOTO(10,20,30,40,990,60,70,80,90,100,110,990,130,140,150,160,170, SLA 1300
     1 180,190,200,210,220,230,240,255,255,255,280,290,300,310,320),    SLA 1310
     2 IFLTK                                                            SLA 1320
      GOTO 990                                                          SLA 1330
   10 CONTINUE                                                          SLA 1340
C                                                                       SLA 1350
C     NATRIUM FLUESSIG                                                  SLA 1360
C                                                                       SLA 1370
      IF(TM .LE. 90.) TM = 90.                                          SLA 1380
      IF(TM .GT. 1300.) TM = 1300.                                      SLA 1390
      TF = 1.8 * TM + 32.                                               SLA 1400
C                                                                       SLA 1410
      XLAMT = (54.306-1.878E-2*TF+2.0914E-6*TF*TF) * .173E-1            SLA 1420
C                                                                       SLA 1430
      GOTO 1000                                                         SLA 1440
   20 CONTINUE                                                          SLA 1450
C                                                                       SLA 1460
C     MATRIX-GRAPHIT                                                    SLA 1470
C                                                                       SLA 1480
      IF(TM .LT. 450.) TM = 450.                                        SLA 1490
      IF(TM .GT. 1300.) TM = 1300.                                      SLA 1500
      IF(DO .GT. 2.5) DO = 2.5                                          SLA 1510
C                                                                       SLA 1520
      XLAMT = (-.3906E-4*TM+.06829) / (DO+1.931E-4*TM+.105) + 1.228E-4 *SLA 1530
     1 TM +.042                                                         SLA 1540
C                                                                       SLA 1550
      XLAMT = XLAMT * 1.12                                              SLA 1560
C                                                                       SLA 1570
C     KORREKTUR NACH SCHOENFELD =1.14                                   SLA 1580
C                                                                       SLA 1590
      XLAMT = XLAMT * 1.14                                              SLA 1600
C                                                                       SLA 1610
      GOTO 1000                                                         SLA 1620
   30 CONTINUE                                                          SLA 1630
C                                                                       SLA 1640
C     REFLEKTOR-GRAPHIT                                                 SLA 1650
C                                                                       SLA 1660
      IF(TM .LT. 0.) TM = 0.                                            SLA 1670
      IF(TM .GT. 2000.) TM = 2000.                                      SLA 1680
      IF(DO .GT. 30.) DO = 30.                                          SLA 1690
C                                                                       SLA 1700
      XLAMT = ALAM0                                                     SLA 1710
C                                                                       SLA 1720
      IF(ALAM0 .EQ. 0.) XLAMT = 1.15                                    SLA 1730
C                                                                       SLA 1740
C     RELATIVE AENDERUNG DER WAERMELEITFAEHIGKEIT MIT DER TEMPERATUR    SLA 1750
C     FUER UNBESTRAHLTEN GRAPHIT                                        SLA 1760
C                                                                       SLA 1770
      DELAM = -156.48 + 31.62 * ALOG(TM+100.)                           SLA 1780
      DELAM = DELAM / 100.                                              SLA 1790
C                                                                       SLA 1800
      XLAMT = XLAMT * (1.-DELAM)                                        SLA 1810
C                                                                       SLA 1820
      IF(DO .EQ. 0.) GOTO 1000                                          SLA 1830
C                                                                       SLA 1840
      CALL GFIT(DO,TM,DLAMM)                                            SLA 1850
C                                                                       SLA 1860
      XLAMT = XLAMT * DLAMM                                             SLA 1870
C                                                                       SLA 1880
      GO TO 1000                                                        SLA 1890
   40 CONTINUE                                                          SLA 1900
C                                                                       SLA 1910
C     STAGN. HELIUM BEI 1BAR                                            SLA 1920
C                                                                       SLA 1930
      IF(TM .LE. 0.) TM = 1.                                            SLA 1940
      TM5 = TM**5.                                                      SLA 1950
      TM6 = TM + 273.                                                   SLA 1960
      TM6 = TM6**.69                                                    SLA 1970
C                                                                       SLA 1980
      XLAMT = 2.97E-5 * TM6 + (9.23E7*TM) / (TM5+4.29E14)               SLA 1990
C                                                                       SLA 2000
      GOTO 1000                                                         SLA 2010
C                                                                       SLA 2020
   60 CONTINUE                                                          SLA 2030
C                                                                       SLA 2040
C     KUGELHAUFEN,FUER STEATIT-EXP., NACH ZEHNER-SCHLUENDER             SLA 2050
C                                                                       SLA 2060
      EPSG = .85                                                        SLA 2070
      DK = 0.9337                                                       SLA 2080
C                                                                       SLA 2090
C     XLGAS:LAM(N2(50 C)) XLKUG:LAMF(STEATIT) LINEAR NACH FIRMENBLATT   SLA 2100
C                                                                       SLA 2110
      XLGAS = 2.78E-04                                                  SLA 2120
      XLKUG = 0.0221 + 5.8125E-05 * TM                                  SLA 2130
      A1 = XLKUG / XLGAS                                                SLA 2140
      B = 2.05                                                          SLA 2150
      TM5 = (TM+273.) / 100.                                            SLA 2160
      TM5 = TM5**3.                                                     SLA 2170
      XLS = 2.3E-5 * DK / (2./EPSG-1.) * TM5                            SLA 2180
      A2 = XLS / XLKUG                                                  SLA 2190
      A3 = XLS / XLGAS                                                  SLA 2200
      A4 = 1. + A2 - B / A1                                             SLA 2210
C                                                                       SLA 2220
      XLAMT = (.219*(1.+.39*A3)+1.56/A4*((1.+A2-1./A1)*B/A4/A4*ALOG((1.+SLA 2230
     1 A2)/B*A1)-(B-1.)/A4-(B+1.)/2./B*A1*(1.-A4))) * XLGAS             SLA 2240
C                                                                       SLA 2250
      GOTO 1000                                                         SLA 2260
   70 CONTINUE                                                          SLA 2270
C                                                                       SLA 2280
C     REAKTORGRAPHIT , MOMENTANE TEMPERATUR NE BESTRAHLUNGSTEMP.        SLA 2290
C     XLAM0 = F(DO UND BESTR.-TEMP.)                                    SLA 2300
C                                                                       SLA 2310
      IF(TM .GT. 1700.) TM = 1700.                                      SLA 2320
      T1 = TM / 1000.                                                   SLA 2330
      XLAM0 = ALAM0                                                     SLA 2340
      XLMI = 0.                                                         SLA 2350
      IF(ALAM0 .LT. 10.) GOTO 72                                        SLA 2360
      LMI = IFIX(ALAM0/10.)                                             SLA 2370
      XLMI = FLOAT(LMI)                                                 SLA 2380
      XLAM0 = ALAM0 - (10.*XLMI)                                        SLA 2390
      XLMI = XLMI / 1000.                                               SLA 2400
   72 CONTINUE                                                          SLA 2410
      TM5 = T1**3.                                                      SLA 2420
C                                                                       SLA 2430
      XLAMT = XLAM0 * (1.-1.084*T1+.743*T1*T1-.213*TM5)                 SLA 2440
C                                                                       SLA 2450
      XLAMT = AMAX1(XLAMT,XLMI)                                         SLA 2460
C                                                                       SLA 2470
      GOTO 1000                                                         SLA 2480
   80 CONTINUE                                                          SLA 2490
C                                                                       SLA 2500
C     KOHLESTEIN , NACH LUKASCEWICZ                                     SLA 2510
C                                                                       SLA 2520
      T1 = TM                                                           SLA 2530
      IF(TM .GE. 1000.) T1 = 1000.                                      SLA 2540
      IF(TM .LT. 0.) T1 = 0.                                            SLA 2550
C                                                                       SLA 2560
      XLAMT = .05 + .03E-3 * T1                                         SLA 2570
C                                                                       SLA 2580
      GOTO 1000                                                         SLA 2590
   90 CONTINUE                                                          SLA 2600
C                                                                       SLA 2610
C     STAGNIERENDE LUFT BEI 1 BAR                                       SLA 2620
C                                                                       SLA 2630
      TM5 = TM + 273.                                                   SLA 2640
      TM5 = TM5**.7                                                     SLA 2650
C                                                                       SLA 2660
      XLAMT = 5.14E-6 * TM5                                             SLA 2670
C                                                                       SLA 2680
      GOTO 1000                                                         SLA 2690
  100 CONTINUE                                                          SLA 2700
C                                                                       SLA 2710
C     THERMISCHER SCHILD  / NACH HRB                                    SLA 2720
C                                                                       SLA 2730
      XLAMT = (48.4-0.0205*TM) / 100.                                   SLA 2740
C                                                                       SLA 2750
      GOTO 1000                                                         SLA 2760
  110 CONTINUE                                                          SLA 2770
C                                                                       SLA 2780
C     V2A - STAHL (DIN 4541) (THYSSEN-INFORMATION)                      SLA 2790
C                                                                       SLA 2800
      IF(TM .LT. -100.) TM = -100.                                      SLA 2810
      IF(TM .GT. 750.) TM = 750.                                        SLA 2820
C                                                                       SLA 2830
      XLAMT = 0.1517 + 1.33E-04 * TM                                    SLA 2840
C                                                                       SLA 2850
      GO TO 1000                                                        SLA 2860
  130 CONTINUE                                                          SLA 2870
C                                                                       SLA 2880
C     STEATIT (FUER HETEROGENE BERECHNUNG)                              SLA 2890
C                                                                       SLA 2900
      XLAMT = 0.0221 + 5.8125E-05 * TM                                  SLA 2910
C                                                                       SLA 2920
      GOTO 1000                                                         SLA 2930
  140 CONTINUE                                                          SLA 2940
C                                                                       SLA 2950
      XLAMT = (0.0162+1.131E-4*(TM-390.)) * ALAM0                       SLA 2960
C                                                                       SLA 2970
      GOTO 1000                                                         SLA 2980
  150 CONTINUE                                                          SLA 2990
C                                                                       SLA 3000
C     STAHLBETON                                                        SLA 3010
C                                                                       SLA 3020
      IF(ALAM0 .EQ. 0.) ALAM0 = 1.                                      SLA 3030
      IF(TM .GT. 550.) TM = 550.                                        SLA 3040
C                                                                       SLA 3050
      XLAMT = (0.0186-1.18E-5*(TM+273.)) * ALAM0                        SLA 3060
C                                                                       SLA 3070
      GOTO 1000                                                         SLA 3080
  160 CONTINUE                                                          SLA 3090
C                                                                       SLA 3100
C     PRISMATISCHES CORE :  IN Z- RICHTUNG                              SLA 3110
C     XLAMT = -ALAM0                                                    SLA 3120
C                                                                       SLA 3130
      A4 = 3.963676E-13                                                 SLA 3140
      A3 = -1.619826E-09                                                SLA 3150
      A2 = 1.246728E-06                                                 SLA 3160
      A1 = 1.243255E-02                                                 SLA 3170
      A0 = 1.690167E+01                                                 SLA 3180
      TM4 = TM**4.                                                      SLA 3190
      TM3 = TM**3.                                                      SLA 3200
      TM2 = TM**2.                                                      SLA 3210
C                                                                       SLA 3220
      XLAMT = -((A4*TM4+A3*TM3+A2*TM2+A1*TM+A0)/100.)                   SLA 3230
C                                                                       SLA 3240
      GOTO 1000                                                         SLA 3250
  170 CONTINUE                                                          SLA 3260
C                                                                       SLA 3270
C     KOHLE- U. GRAPHITFILZE (EFFEKTIVE WAERMELEITFAEHIGKEIT) IN VAKUUM SLA 3280
C     GUELTIGKEITSBEREICH: 0.< TM <= 4000. C                            SLA 3290
C                                                                       SLA 3300
      IF(TM .GT. 4000.) TM = 4000.                                      SLA 3310
      IF(TM .LT. 0.) TM = 0.                                            SLA 3320
      IF(TM .GT. 1600.) GOTO 173                                        SLA 3330
      IF(TM .GT. 1000.) GOTO 172                                        SLA 3340
C                                                                       SLA 3350
      XLAMT = 3.E-5 * TM + .05                                          SLA 3360
C                                                                       SLA 3370
C     1.163E-2 :  UMRECHNUNGSFAKTOR VON KCAL/M H C --> W/CM K           SLA 3380
C                                                                       SLA 3390
      XLAMT = XLAMT * 1.163E-2                                          SLA 3400
C                                                                       SLA 3410
      GOTO 1000                                                         SLA 3420
  172 CONTINUE                                                          SLA 3430
C                                                                       SLA 3440
      XLAMT = 1.167E-4 * TM - .06                                       SLA 3450
C                                                                       SLA 3460
      XLAMT = XLAMT * 1.163E-2                                          SLA 3470
C                                                                       SLA 3480
      GOTO 1000                                                         SLA 3490
  173 CONTINUE                                                          SLA 3500
C                                                                       SLA 3510
      XLAMT = 3.5E-4 * TM - .41                                         SLA 3520
C                                                                       SLA 3530
      XLAMT = XLAMT * 1.163E-2                                          SLA 3540
C                                                                       SLA 3550
      GOTO 1000                                                         SLA 3560
  180 CONTINUE                                                          SLA 3570
C                                                                       SLA 3580
C     KOHLE- U. GRAPHITFILZE (EFFEKTIVE WAERMELEITFAEHIGKEIT) IN ARGON- SLA 3590
C     U. STICKSTOFF-ATMOSPHAERE                                         SLA 3600
C     GUELTIGKEITSBEREICH: 0.< TM <= 4000. C                            SLA 3610
C                                                                       SLA 3620
      IF(TM .GT. 4000.) TM = 4000.                                      SLA 3630
      IF(TM .LT. 0.) TM = 0.                                            SLA 3640
      IF(TM .GT. 1800.) GOTO 184                                        SLA 3650
      IF(TM .GT. 1400.) GOTO 183                                        SLA 3660
      IF(TM .GT. 1000.) GOTO 182                                        SLA 3670
C                                                                       SLA 3680
      XLAMT = 8. E-5 * TM + .07                                         SLA 3690
C                                                                       SLA 3700
C     1.163E-2 :  UMRECHNUNGSFAKTOR VON KCAL/M H C --> W/CM K           SLA 3710
C                                                                       SLA 3720
      XLAMT = XLAMT * 1.163E-2                                          SLA 3730
C                                                                       SLA 3740
      GOTO 1000                                                         SLA 3750
  182 CONTINUE                                                          SLA 3760
C                                                                       SLA 3770
      XLAMT = 2.75E-4 * TM - .14                                        SLA 3780
C                                                                       SLA 3790
      XLAMT = XLAMT * 1.163E-2                                          SLA 3800
C                                                                       SLA 3810
      GOTO 1000                                                         SLA 3820
  183 CONTINUE                                                          SLA 3830
C                                                                       SLA 3840
      XLAMT = 6.5E-4 * TM - .67                                         SLA 3850
C                                                                       SLA 3860
      XLAMT = XLAMT * 1.163E-2                                          SLA 3870
C                                                                       SLA 3880
      GOTO 1000                                                         SLA 3890
  184 CONTINUE                                                          SLA 3900
C                                                                       SLA 3910
      XLAMT = 1.067E-3 * TM - 1.4                                       SLA 3920
C                                                                       SLA 3930
      XLAMT = XLAMT * 1.163E-2                                          SLA 3940
C                                                                       SLA 3950
      GOTO 1000                                                         SLA 3960
  190 CONTINUE                                                          SLA 3970
C                                                                       SLA 3980
C     WAELZLAGERSTAHL   100CR6                                          SLA 3990
C     LAMDA = F(T) EINHEIT: W/CM K                                      SLA 4000
C     GUELTIGKEITSBEREICH: 0.<= TM <= 550. C                            SLA 4010
C     NACH BARTHELS (IRB)(MESSUNG VON BINKELE) PERS.MITTEILUNG          SLA 4020
C                                                                       SLA 4030
      IF(TM .LT. 0.) TM = 0.                                            SLA 4040
      IF(TM .GT. 550.) TM = 550.                                        SLA 4050
      AK4 = 2.79914E-12                                                 SLA 4060
      AK3 = -1.92371E-09                                                SLA 4070
      AK2 = -1.27508E-07                                                SLA 4080
      AK1 = 5.40229E-05                                                 SLA 4090
      AK0 = 4.17020E-01                                                 SLA 4100
      TM4 = TM**4.                                                      SLA 4110
      TM3 = TM**3.                                                      SLA 4120
      TM2 = TM**2.                                                      SLA 4130
C                                                                       SLA 4140
      XLAMT = AK4 * TM4 + AK3 * TM3 + AK2 * TM2 + AK1 * TM + AK0        SLA 4150
C                                                                       SLA 4160
      GOTO 1000                                                         SLA 4170
  200 CONTINUE                                                          SLA 4180
C                                                                       SLA 4190
C     WAERMELEITFAEHIGKEIT STICKSTOFF(N2) W/CM/K                        SLA 4200
C                                                                       SLA 4210
      TM5 = TM + 273.                                                   SLA 4220
      TM5 = TM5**.715                                                   SLA 4230
C                                                                       SLA 4240
      XLAMT = 4.373E-04 * TM5                                           SLA 4250
C                                                                       SLA 4260
C     /100. UMRECHNUNGSFAKTOR W/M/K --> W/CM/K                          SLA 4270
C                                                                       SLA 4280
      XLAMT = XLAMT / 100.                                              SLA 4290
C                                                                       SLA 4300
      GOTO 1000                                                         SLA 4310
  210 CONTINUE                                                          SLA 4320
C                                                                       SLA 4330
C     EFFEKTIVE WAERMELEITFAEHIGKEIT KAOWOOL - MATTEN (IN LUFT) W/CM/K  SLA 4340
C     NACH P.BROECKERHOFF (IRB, JUEL - 992-RB,1973)                     SLA 4350
C     GUELTIGKEITSBEREICH  50.C <= TM <= 600.C                          SLA 4360
C                                                                       SLA 4370
      AL0 = 3.55708E-04                                                 SLA 4380
      AL1 = 8.42352E-07                                                 SLA 4390
      AL2 = 2.43191E-09                                                 SLA 4400
      IF(TM .LT. 50.) TM = 50.                                          SLA 4410
      IF(TM .GT. 600.) TM = 600.                                        SLA 4420
      TM2 = TM**2.                                                      SLA 4430
C                                                                       SLA 4440
      XLAMT = AL2 * TM2 + AL1 * TM + AL0                                SLA 4450
C                                                                       SLA 4460
      GOTO 1000                                                         SLA 4470
  220 CONTINUE                                                          SLA 4480
C                                                                       SLA 4490
C     H A W - GLAS, T < 550 GRDC                                        SLA 4500
C                                                                       SLA 4510
      TM2 = TM**2.                                                      SLA 4520
      TM3 = TM**3.                                                      SLA 4530
C                                                                       SLA 4540
      XLAMT = 0.927 + 3.17106E-3 * TM - 1.12195E-5 * TM2 + 1.48244E-8 * SLA 4550
     1 TM3                                                              SLA 4560
C                                                                       SLA 4570
      XLAMT = XLAMT / 100.                                              SLA 4580
C                                                                       SLA 4590
      GOTO 1000                                                         SLA 4600
  230 CONTINUE                                                          SLA 4610
C                                                                       SLA 4620
C     SCHUERENKRAEMER  /  7.2.84                                        SLA 4630
C                                                                       SLA 4640
      IF(TM .LT. 100.) TM = 100.                                        SLA 4650
      IF(TM .GT. 2500.) TM = 2500.                                      SLA 4660
      TM5 = TM**1.545                                                   SLA 4670
C                                                                       SLA 4680
      IF(TM .LT. 1300.) XLAMT = 2.549E-4 * TM5 + 1.5                    SLA 4690
C                                                                       SLA 4700
      TM4 = TM - 135.                                                   SLA 4710
      TM4 = TM4**1.287                                                  SLA 4720
C                                                                       SLA 4730
      IF(TM .GE. 1300.) XLAMT = 2.E-3 * TM4                             SLA 4740
C                                                                       SLA 4750
      XLAMT = XLAMT * .01                                               SLA 4760
C                                                                       SLA 4770
      GO TO 1000                                                        SLA 4780
  240 CONTINUE                                                          SLA 4790
C                                                                       SLA 4800
C     TEMP.-DOSIS-ABHAENGIGKEIT EXPLIZIT (BINKELE / A3-GRAPHIT)         SLA 4810
C                                                                       SLA 4820
      CALL TEMPK4(TM,DO,TDLAM,IFL1)                                     SLA 4830
C                                                                       SLA 4840
      XLAMT = TDLAM                                                     SLA 4850
C                                                                       SLA 4860
      GO TO 1000                                                        SLA 4870
  255 CONTINUE                                                          SLA 4880
C                                                                       SLA 4890
C     TEMP.-DOSIS-ABHAENGIGKEIT NACH ROBOLD BZW. ZEHNER-SCHLUENDER      SLA 4900
C                                                                       SLA 4910
      CALL TEMPK5(TM,DO,TDLAM,IFL1)                                     SLA 4920
C                                                                       SLA 4930
      XLAMT = TDLAM                                                     SLA 4940
C                                                                       SLA 4950
      GO TO 1000                                                        SLA 4960
  280 CONTINUE                                                          SLA 4970
C                                                                       SLA 4980
C     AL2O3 (LINEAR FROM SALMANG/SCHOLZ "KERAMIK")                      SLA 4990
C                                                                       SLA 5000
      T3 = 100.                                                         SLA 5010
      T2 = 1000.                                                        SLA 5020
      XL1 = 0.25                                                        SLA 5030
      XL2 = 0.06                                                        SLA 5040
      TN = AMAX1(T3,TM)                                                 SLA 5050
      TN = AMIN1(T2,TN)                                                 SLA 5060
C                                                                       SLA 5070
      XLAMT = XL1 + (XL2-XL1) * (TN-T3) / (T2-T3)                       SLA 5080
C                                                                       SLA 5090
      GO TO 1000                                                        SLA 5100
  290 CONTINUE                                                          SLA 5110
C                                                                       SLA 5120
C     GILSONITKOKS AGL-IE 1-24, BESTRAHLT BEI 760 C (BINKELE)           SLA 5130
C                                                                       SLA 5140
      T1 = 100.                                                         SLA 5150
      T2 = 200.                                                         SLA 5160
      XL1 = 0.25                                                        SLA 5170
      XL2 = 0.26                                                        SLA 5180
C                                                                       SLA 5190
      XLAMT = XL2                                                       SLA 5200
C                                                                       SLA 5210
      IF(TM .LT. T2) XLAMT = XL2 + (XL1-XL2) * (TM-T2) / (T1-T2)        SLA 5220
C                                                                       SLA 5230
      GO TO 1000                                                        SLA 5240
  300 CONTINUE                                                          SLA 5250
C                                                                       SLA 5260
C     CORE BARREL, SA-240, GRADE 316                                    SLA 5270
C                                                                       SLA 5280
      IF(TM .LT. 20.) TM = 20.                                          SLA 5290
      IF(TM .GT. 800.) TM = 800.                                        SLA 5300
      TM = TM / 1000.                                                   SLA 5310
      XLAMT = 0.1297 + (0.1577-0.0309*TM) * TM                          SLA 5320
      GOTO 1000                                                         SLA 5330
  310 CONTINUE                                                          SLA 5340
C                                                                       SLA 5350
C     PRESSURE VESSEL, SA-508                                           SLA 5360
C                                                                       SLA 5370
      IF(TM .LT. 21.) THEN                                              SLA 5380
        WRITE (6,4203)                                                  SLA 5390
        TM = 21.                                                        SLA 5400
      ENDIF                                                             SLA 5410
      IF(TM .GT. 427. .AND. IWAN .NE. -1) THEN                          SLA 5420
        WRITE (6,4203)                                                  SLA 5430
        TM = 427.                                                       SLA 5440
        IWAN = -1                                                       SLA 5450
      ENDIF                                                             SLA 5460
      TM = TM / 1000.                                                   SLA 5470
      XLAMT = 0.3721 + (0.2542-(1.1072-0.9528*TM)*TM) * TM              SLA 5480
      GOTO 1000                                                         SLA 5490
  320 CONTINUE                                                          SLA 5500
C                                                                       SLA 5510
C     Reactor Graphite, SGL Grade A, NBG10                              SLA 5520
C                                                                       SLA 5530
      IF(TM .LT. 20.) THEN                                              SLA 5540
        WRITE (6,6201) TM                                               SLA 5550
        TM = 20.                                                        SLA 5560
      ENDIF                                                             SLA 5570
      IF(TM .GT. 2000.) THEN                                            SLA 5580
        WRITE (6,6201) TM                                               SLA 5590
        TM = 2000.                                                      SLA 5600
      ENDIF                                                             SLA 5610
      DOSE = DBLE(DO)                                                   SLA 5620
      DTM = DBLE(TM)                                                    SLA 5630
      DTI = DBLE(TIRR)                                                  SLA 5640
C                                                                       SLA 5650
      CALL DCOND(DVAL,DOSE,DTI,DTM)                                     SLA 5660
C                                                                       SLA 5670
C     Assume DVAL in W/(m K) and convert to W/(cm K)                    SLA 5680
C                                                                       SLA 5690
      XLAMT = 0.01D0 * SNGL(DVAL)                                       SLA 5700
CFZJ027     MODIFY HEAT CONDUCTIVITY                          31.03.04  SLA 5710
      IF(RLAM32 .NE. 0.) XLAMT = XLAMT * RLAM32                         SLA 5720
      GOTO 1000                                                         SLA 5730
  990 XLAMT = ALAM0                                                     SLA 5740
 1000 CONTINUE                                                          SLA 5750
      RETURN                                                            SLA 5760
 2000 CONTINUE                                                          SLA 5770
      GOTO(11,21,31,41,5000,61,71,81,91,101,111,5000,131,141,151,161,175SLA 5780
     1,185,191,201,211,221,231,241,251,261,271,281,291,301,311,321),    SLA 5790
     2 IFLTK                                                            SLA 5800
      GOTO 5000                                                         SLA 5810
   11 CONTINUE                                                          SLA 5820
      WRITE (6,2011) IFLTK                                              SLA 5830
      IFPRNT = -1                                                       SLA 5840
      RETURN                                                            SLA 5850
   21 CONTINUE                                                          SLA 5860
      WRITE (6,2021) IFLTK                                              SLA 5870
      IFPRNT = -1                                                       SLA 5880
      RETURN                                                            SLA 5890
   31 CONTINUE                                                          SLA 5900
      WRITE (6,2031) IFLTK                                              SLA 5910
      IFPRNT = -1                                                       SLA 5920
      RETURN                                                            SLA 5930
   41 CONTINUE                                                          SLA 5940
      WRITE (6,2041) IFLTK                                              SLA 5950
      WRITE (6,2042) IFLTK                                              SLA 5960
      IFPRNT = -1                                                       SLA 5970
      RETURN                                                            SLA 5980
   61 CONTINUE                                                          SLA 5990
      WRITE (6,2061) IFLTK                                              SLA 6000
      IFPRNT = -1                                                       SLA 6010
      RETURN                                                            SLA 6020
   71 CONTINUE                                                          SLA 6030
      WRITE (6,2071) IFLTK                                              SLA 6040
      IFPRNT = -1                                                       SLA 6050
      RETURN                                                            SLA 6060
   81 CONTINUE                                                          SLA 6070
      WRITE (6,2081) IFLTK                                              SLA 6080
      IFPRNT = -1                                                       SLA 6090
      RETURN                                                            SLA 6100
   91 CONTINUE                                                          SLA 6110
      WRITE (6,2091) IFLTK                                              SLA 6120
      IFPRNT = -1                                                       SLA 6130
      RETURN                                                            SLA 6140
  101 CONTINUE                                                          SLA 6150
      WRITE (6,2101) IFLTK                                              SLA 6160
      IFPRNT = -1                                                       SLA 6170
      RETURN                                                            SLA 6180
  111 CONTINUE                                                          SLA 6190
      WRITE (6,2111) IFLTK                                              SLA 6200
      IFPRNT = -1                                                       SLA 6210
      RETURN                                                            SLA 6220
  131 CONTINUE                                                          SLA 6230
      WRITE (6,2131) IFLTK                                              SLA 6240
      IFPRNT = -1                                                       SLA 6250
      RETURN                                                            SLA 6260
  141 CONTINUE                                                          SLA 6270
      WRITE (6,2141) IFLTK                                              SLA 6280
      IFPRNT = -1                                                       SLA 6290
      RETURN                                                            SLA 6300
  151 CONTINUE                                                          SLA 6310
      WRITE (6,2151) IFLTK                                              SLA 6320
      IFPRNT = -1                                                       SLA 6330
      RETURN                                                            SLA 6340
  161 CONTINUE                                                          SLA 6350
      WRITE (6,2161) IFLTK                                              SLA 6360
      IFPRNT = -1                                                       SLA 6370
      RETURN                                                            SLA 6380
  175 CONTINUE                                                          SLA 6390
      WRITE (6,2171) IFLTK                                              SLA 6400
      IFPRNT = -1                                                       SLA 6410
      RETURN                                                            SLA 6420
  185 CONTINUE                                                          SLA 6430
      WRITE (6,2181) IFLTK                                              SLA 6440
      IFPRNT = -1                                                       SLA 6450
      RETURN                                                            SLA 6460
  191 CONTINUE                                                          SLA 6470
      WRITE (6,2191) IFLTK                                              SLA 6480
      IFPRNT = -1                                                       SLA 6490
      RETURN                                                            SLA 6500
  201 CONTINUE                                                          SLA 6510
      WRITE (6,2201) IFLTK                                              SLA 6520
      IFPRNT = -1                                                       SLA 6530
      RETURN                                                            SLA 6540
  211 CONTINUE                                                          SLA 6550
      WRITE (6,2211) IFLTK                                              SLA 6560
      IFPRNT = -1                                                       SLA 6570
      RETURN                                                            SLA 6580
  221 CONTINUE                                                          SLA 6590
      WRITE (6,2221) IFLTK                                              SLA 6600
      IFPRNT = -1                                                       SLA 6610
      RETURN                                                            SLA 6620
  231 CONTINUE                                                          SLA 6630
      WRITE (6,2231) IFLTK                                              SLA 6640
      WRITE (6,2232)                                                    SLA 6650
      IFPRNT = -1                                                       SLA 6660
      RETURN                                                            SLA 6670
  241 CONTINUE                                                          SLA 6680
      WRITE (6,2241) IFLTK                                              SLA 6690
      IFPRNT = -1                                                       SLA 6700
      RETURN                                                            SLA 6710
  251 CONTINUE                                                          SLA 6720
      WRITE (6,2251) IFLTK                                              SLA 6730
      IFPRNT = -1                                                       SLA 6740
      RETURN                                                            SLA 6750
  261 CONTINUE                                                          SLA 6760
      WRITE (6,2261) IFLTK                                              SLA 6770
      IFPRNT = -1                                                       SLA 6780
      RETURN                                                            SLA 6790
  271 CONTINUE                                                          SLA 6800
      WRITE (6,2271) IFLTK                                              SLA 6810
      IFPRNT = -1                                                       SLA 6820
      RETURN                                                            SLA 6830
  281 CONTINUE                                                          SLA 6840
      WRITE (6,2281) IFLTK                                              SLA 6850
      IFPRNT = -1                                                       SLA 6860
      RETURN                                                            SLA 6870
  291 CONTINUE                                                          SLA 6880
      WRITE (6,2291) IFLTK                                              SLA 6890
      IFPRNT = -1                                                       SLA 6900
      RETURN                                                            SLA 6910
  301 CONTINUE                                                          SLA 6920
      WRITE (6,2301) IFLTK                                              SLA 6930
      IFPRNT = -1                                                       SLA 6940
      RETURN                                                            SLA 6950
  311 CONTINUE                                                          SLA 6960
      WRITE (6,2311) IFLTK                                              SLA 6970
      IFPRNT = -1                                                       SLA 6980
      RETURN                                                            SLA 6990
  321 CONTINUE                                                          SLA 7000
CFZJ026                                                                 SLA 7010
      WRITE (6,2321) IFLTK                                              SLA 7020
      IFPRNT = -1                                                       SLA 7030
      RETURN                                                            SLA 7040
 5000 CONTINUE                                                          SLA 7050
      IF(IFPRNT .EQ. 2) GOTO 5002                                       SLA 7060
      WRITE (6,5001) IFLTK                                              SLA 7070
C                                                                       SLA 7080
      CALL ABEND(2)                                                     SLA 7090
C                                                                       SLA 7100
 5002 IFPRNT = -1                                                       SLA 7110
      RETURN                                                            SLA 7120
      END                                                               SLA 7130
      FUNCTION XLAMT1(TT,DO,MAT,ALAM0,IFPRNT)                           AMT   10
C                                                                       AMT   20
C     BERECHNET TEMPERATURABHAENGIGE WAERMELEITFAEHIGKEITEN ANISOTROP   AMT   30
C     IN I-RICHTUNG                                                     AMT   40
C                                                                       AMT   50
 2161 FORMAT (3X,4X,1X,'                    RADIAL         7.475742E-15*AMT   60
     1T**4-6.353831E-11*T**3+1.164280E-7*T**2+7.005595E-5*T+.05880077') AMT   70
 5001 FORMAT (' **WARNING** FOR LAM(T)=',I5,' NO EXISTING RELATION FOR CAMT   80
     1ALCULATION OF THERMAL CONDUCTIVITY'/13X,'INPUT-VALUE OF LAMBDA IS AMT   90
     2NOT CHANGED FOR FURTHER CALCULATIONS')                            AMT  100
C                                                                       AMT  110
C                                                                       AMT  120
      IFLTK = MAT                                                       AMT  130
      TM = TT                                                           AMT  140
      IF(IFPRNT .GT. 0) GOTO 2000                                       AMT  150
      GOTO(990,990,990,990,990,990,990,990,990,990,990,990,990,990,990, AMT  160
     1 160),IFLTK                                                       AMT  170
      GOTO 990                                                          AMT  180
  160 CONTINUE                                                          AMT  190
C                                                                       AMT  200
C     PRISMATISCHES CORE: IN R- RICHTUNG                                AMT  210
C     XLAMT1 = ALAM0                                                    AMT  220
C                                                                       AMT  230
      A4 = 7.475742E-13                                                 AMT  240
      A3 = -6.353831E-09                                                AMT  250
      A2 = 1.164280E-05                                                 AMT  260
      A1 = 7.005595E-03                                                 AMT  270
      A0 = 5.880077E-00                                                 AMT  280
      TM4 = TM**4.                                                      AMT  290
      TM3 = TM**3.                                                      AMT  300
      TM2 = TM**2.                                                      AMT  310
C                                                                       AMT  320
      XLAMT1 = (A4*TM4+A3*TM3+A2*TM2+A1*TM+A0) / 100.                   AMT  330
C                                                                       AMT  340
      GOTO 1000                                                         AMT  350
C                                                                       AMT  360
  990 XLAMT1 = ALAM0                                                    AMT  370
C                                                                       AMT  380
 1000 CONTINUE                                                          AMT  390
      RETURN                                                            AMT  400
 2000 GOTO(991,991,991,991,991,991,991,991,991,991,991,991,991,991,991, AMT  410
     1 161),IFLTK                                                       AMT  420
      GOTO 991                                                          AMT  430
  161 CONTINUE                                                          AMT  440
      WRITE (6,2161)                                                    AMT  450
      RETURN                                                            AMT  460
  991 CONTINUE                                                          AMT  470
      IF(IFPRNT .EQ. 2) GOTO 5002                                       AMT  480
      WRITE (6,5001) IFLTK                                              AMT  490
C                                                                       AMT  500
      CALL ABEND(2)                                                     AMT  510
C                                                                       AMT  520
 5002 IFPRNT = -1                                                       AMT  530
      RETURN                                                            AMT  540
      END                                                               AMT  550
      FUNCTION ZKUGL(II,NN,KK,AU,IFBH,HEPS,HKUG,KKB)                    ZKU   10
C                                                                       ZKU   20
C     BERECHNET DIE ANZAHL DER BE IN HET-MASCHEN UND MISCHMASCHEN       ZKU   30
C                                                                       ZKU   40
      COMMON /FELD2/ IDIFF,NDIFF,IMH,NMH                                ZKU   50
C                                                                       ZKU   60
      COMMON /VARTK/ KMAZ,IMAZ,NMAZ,ICO,NCO,KOMAX,KONI,KONN             ZKU   70
C                                                                       ZKU   80
      DIMENSION AU(IMAZ,NMAZ),IFBH(ICO,NCO),HEPS(KMAZ),HKUG(KMAZ),      ZKU   90
     1 KKB(ICO,NCO)                                                     ZKU  100
C                                                                       ZKU  110
C                                                                       ZKU  120
      I = II + IDIFF                                                    ZKU  130
      N = NN + NDIFF                                                    ZKU  140
C                                                                       ZKU  150
      ZKUGL = 0.                                                        ZKU  160
C                                                                       ZKU  170
      IFF = IFBH(II,NN)                                                 ZKU  180
      KK1 = KKB(II,NN)                                                  ZKU  190
      IF(IFF .EQ. 0) RETURN                                             ZKU  200
      IF(IFF .EQ. 1) GOTO 25                                            ZKU  210
      IF(IFF .EQ. 2 .OR. IFF .EQ. 5 .OR. IFF .EQ. 6 .OR. IFF .EQ. 9 .OR.ZKU  220
     1 IFF .EQ. 10 .OR. IFF .EQ. 13 .OR. IFF .EQ. 14) GOTO 101          ZKU  230
      VOLM1 = AU(I-1,N-1)                                               ZKU  240
      GOTO 200                                                          ZKU  250
  101 VOLM1 = 0.                                                        ZKU  260
  200 IF(IFF .EQ. 3 .OR. IFF .EQ. 5 .OR. IFF .EQ. 7 .OR. IFF .EQ. 9 .OR.ZKU  270
     1 IFF .EQ. 11 .OR. IFF .EQ. 13 .OR. IFF .EQ. 15) GOTO 102          ZKU  280
      VOLM2 = AU(I,N-1)                                                 ZKU  290
      GOTO 300                                                          ZKU  300
  102 VOLM2 = 0.                                                        ZKU  310
  300 IF(IFF .EQ. 4 .OR. IFF .EQ. 6 .OR. IFF .EQ. 7 .OR. IFF .EQ. 9 .OR.ZKU  320
     1 IFF .EQ. 12 .OR. IFF .EQ. 14 .OR. IFF .EQ. 15) GOTO 103          ZKU  330
      VOLM3 = AU(I-1,N)                                                 ZKU  340
      GOTO 400                                                          ZKU  350
  103 VOLM3 = 0.                                                        ZKU  360
  400 IF(IFF .EQ. 8 .OR. IFF .EQ. 10 .OR. IFF .EQ. 11 .OR. IFF .EQ. 12  ZKU  370
     1 .OR. IFF .EQ. 13 .OR. IFF .EQ. 14 .OR. IFF .EQ. 15) GOTO 104     ZKU  380
      VOLM4 = AU(I,N)                                                   ZKU  390
      GOTO 500                                                          ZKU  400
  104 VOLM4 = 0.                                                        ZKU  410
  500 GOTO 30                                                           ZKU  420
   25 VOLM1 = AU(I-1,N-1)                                               ZKU  430
      VOLM2 = AU(I,N-1)                                                 ZKU  440
      VOLM3 = AU(I-1,N)                                                 ZKU  450
      VOLM4 = AU(I,N)                                                   ZKU  460
   30 VOLM = VOLM1 + VOLM2 + VOLM3 + VOLM4                              ZKU  470
      HK3 = HKUG(KK1)**3.                                               ZKU  480
C                                                                       ZKU  490
      ZKUGL = VOLM * (1-HEPS(KK1)) / (0.52*HK3)                         ZKU  500
C                                                                       ZKU  510
      RETURN                                                            ZKU  520
      END                                                               ZKU  530
      SUBROUTINE DLLPAR(DVAL,DOSE,TI)                                   DLL   10
C                                                                       DLL   20
C     COEFFICIENTS FROM REV B OF IRRADIATED GRAPHITE PROPERTIES FOR     DLL   30
C     REFLECTOR GRAPHITE, 011719                                        DLL   40
C                                                                       DLL   50
      IMPLICIT REAL*8 (A-H,O-Z)                                         DLL   60
C                                                                       DLL   70
      DIMENSION A(9),C(3)                                               DLL   80
C                                                                       DLL   90
      DATA A/-0.09371313568358D0,2.222723138241977D-04,                 DLL  100
     1 -1.603946088032374D-07,0.00103062703743D0,-3.873220226138815D-06,DLL  110
     2 2.975496929463531D-09,-3.427742032355107D-06,                    DLL  120
     3 1.406601679252522D-08,-9.670833849135043D-12/                    DLL  130
C                                                                       DLL  140
C                                                                       DLL  150
      C(1) = A(1) + A(2) * TI + A(3) * TI**2.D0                         DLL  160
      C(2) = A(4) + A(5) * TI + A(6) * TI**2.D0                         DLL  170
      C(3) = A(7) + A(8) * TI + A(9) * TI**2.D0                         DLL  180
      DVAL = C(1) * DOSE + C(2) * DOSE**2.D0 + C(3) * DOSE**3.D0        DLL  190
      RETURN                                                            DLL  200
      END                                                               DLL  210
      SUBROUTINE DLLPERP(DVAL,DOSE,TI)                                  LLP   10
C                                                                       LLP   20
C     COEFFICIENTS FROM REV B OF IRRADIATED GRAPHITE PROPERTIES FOR     LLP   30
C     REFLECTOR GRAPHITE, 011719                                        LLP   40
C                                                                       LLP   50
      IMPLICIT REAL*8 (A-H,O-Z)                                         LLP   60
C                                                                       LLP   70
      DIMENSION A(9),C(3)                                               LLP   80
C                                                                       LLP   90
      DATA A/-0.05602068722016D0,1.079817349914243D-04,                 LLP  100
     1 -9.494527201583215D-08,5.260034857106624D-04,                    LLP  110
     2 -1.78119294285359D-06,1.4581488066203D-09,-1.603313098107894D-06,LLP  120
     3 6.338830607542572D-09,-3.712551707845104D-12/                    LLP  130
C                                                                       LLP  140
C                                                                       LLP  150
      C(1) = A(1) + A(2) * TI + A(3) * TI**2.D0                         LLP  160
      C(2) = A(4) + A(5) * TI + A(6) * TI**2.D0                         LLP  170
      C(3) = A(7) + A(8) * TI + A(9) * TI**2.D0                         LLP  180
      DVAL = C(1) * DOSE + C(2) * DOSE**2.D0 + C(3) * DOSE**3.D0        LLP  190
      RETURN                                                            LLP  200
      END                                                               LLP  210
      SUBROUTINE DVVMOD(DVAL,DOSE,TI)                                   DVV   10
C                                                                       DVV   20
      IMPLICIT REAL*8 (A-H,O-Z)                                         DVV   30
C                                                                       DVV   40
C                                                                       DVV   50
      DPAR = 0.D0                                                       DVV   60
      DPER = 0.D0                                                       DVV   70
C                                                                       DVV   80
      CALL DLLPAR(DPAR,DOSE,TI)                                         DVV   90
C                                                                       DVV  100
      CALL DLLPERP(DPER,DOSE,TI)                                        DVV  110
C                                                                       DVV  120
      DVAL = 100.D0 * ((1.D0+DPAR/100.D0)*(1.D0+DPER/100.D0)**2.D0-1.D0)DVV  130
      RETURN                                                            DVV  140
      END                                                               DVV  150
      SUBROUTINE DIRRCOND(DCVAL,IDOM,DOSE,TI)                           IRR   10
C                                                                       IRR   20
C     KKO - FRACTIONAL CHANGE IN RESISTIVITY DUE TO IRRADIATION         IRR   30
C     TI  = IRRADIATION TEMPERATURE [DEGC]                              IRR   40
C     F   = FAST NEUTRON FLUENCE E > 0.1MEV, [10^20 N/CM^2 EDN]         IRR   50
C           MEASURED AT IRRADIATION TEMPERATURE, ACTUALLY APPEARS TO BE IRR   60
C           TYPICAL SATURATION VALUES FOR MEASURED AT REFERENCE         IRR   70
C           TEMPERATURE.                                                IRR   80
C                                                                       IRR   90
      IMPLICIT REAL*8 (A-H,O-Z)                                         IRR  100
C                                                                       IRR  110
C                                                                       IRR  120
      FACT = 3.D0                                                       IRR  130
      GAMMA0 = 0.59D-02                                                 IRR  140
      GAMMA_BA = -0.1082D0 * TI + 179.17D0                              IRR  150
      GAMMA_SAT = GAMMA_BA / FACT                                       IRR  160
      DKK0SAT = 0.4025D0 * DLOG(TI) - 2.188D0                           IRR  170
      RET = 0.D0                                                        IRR  180
      IF(DOSE .LT. GAMMA0) THEN                                         IRR  190
        RET = 1.D0                                                      IRR  200
        IDOM = 0                                                        IRR  210
        ELSE IF(DOSE .LT. GAMMA_SAT) THEN                               IRR  220
          DM = (DKK0SAT-1.D0) / (DLOG(GAMMA_SAT)-DLOG(GAMMA0))          IRR  230
          DC = DKK0SAT - DM * DLOG(GAMMA_SAT)                           IRR  240
          RET = DM * DLOG(DOSE) + DC                                    IRR  250
          IDOM = 1                                                      IRR  260
        ELSE IF(DOSE .LT. GAMMA_BA) THEN                                IRR  270
          RET = DKK0SAT                                                 IRR  280
C                                                                       IRR  290
C     WRITE (*,*) GAMMA_BA,DKK0SAT                                      IRR  300
C                                                                       IRR  310
          IDOM = 2                                                      IRR  320
        ELSE                                                            IRR  330
C                                                                       IRR  340
C     IN BREAK AWAY REGION                                              IRR  350
C                                                                       IRR  360
        DVVMIN = 0.D0                                                   IRR  370
        DVVCUR = 0.D0                                                   IRR  380
C                                                                       IRR  390
        CALL DVVMOD(DVVMIN,DOSE,TI)                                     IRR  400
C                                                                       IRR  410
        CALL DVVMOD(DVVCUR,GAMMA_BA,TI)                                 IRR  420
C                                                                       IRR  430
C     CORRECTED TO USE DOSE UNITS OF 10^20 N/CM^2 EDN.                  IRR  440
C                                                                       IRR  450
        RET = 1.D0 /(1.D0+(1.D0/DKK0SAT-1.1D0)+((DVVMIN/DVVCUR-1.D0)*   IRR  460
     1   (DOSE-GAMMA_BA)*-0.015D0))                                     IRR  470
        IDOM = 3                                                        IRR  480
      END IF                                                            IRR  490
      DCVAL = 1 / RET                                                   IRR  500
      RETURN                                                            IRR  510
      END                                                               IRR  520
      SUBROUTINE DHEATCAP(DCVAL,T)                                      DHE   10
C                                                                       DHE   20
C     THE HEAT CAPACITY AS A FUNCTION OF TEMPERATURE                    DHE   30
C     DCVAL: OUTPUT VALUE, CP [KW/KG.K]                                 DHE   40
C     T    : TEMPERATURE [DEGC]                                         DHE   50
C                                                                       DHE   60
C     FIT MODIFIED IN ACCORDANCE WITH REV B. MNM 9/9/2003               DHE   70
C                                                                       DHE   80
      IMPLICIT REAL*8 (A-H,O-Z)                                         DHE   90
C                                                                       DHE  100
C                                                                       DHE  110
      DA1 = 0.0345722D0                                                 DHE  120
      DA2 = 200.056D0                                                   DHE  130
      DA3 = 2092.31D0                                                   DHE  140
      DA4 = -0.00191585D0                                               DHE  150
      DCVAL = DA1 * (T+DA2) + DA3 * (1-EXP(DA4*(T+DA2)))                DHE  160
C                                                                       DHE  170
C     TE = T / 1000.0D0                                                 DHE  180
C     DCVAL = 0.645D0 + 3.14D0 * TE - 2.809D0 * TE**2.D0 + 0.959D0 *    DHE  190
C    1 TE**3.D0                                                         DHE  200
C                                                                       DHE  210
      RETURN                                                            DHE  220
      END                                                               DHE  230
      SUBROUTINE DVIRCOND(DCVAL,T)                                      DVI   10
C                                                                       DVI   20
C     FRACTIONAL CHANGE IN THERMAL CONDUCTIVITY AS A FUNCTION OF        DVI   30
C     TEMPERATURE                                                       DVI   40
C     DCVAL: OUTPUT VALUE, THERMAL CONDUCTIVITY [W/M.K]                 DVI   50
C     T    : TEMPERATURE [DEGC]                                         DVI   60
C                                                                       DVI   70
      IMPLICIT REAL*8 (A-H,O-Z)                                         DVI   80
C                                                                       DVI   90
C                                                                       DVI  100
      DA = 0.29D0                                                       DVI  110
      DK = -0.00229D0                                                   DVI  120
      DCVAL = DA + (1.D0-DA) * DEXP(DK*(T-20.D0))                       DVI  130
      RETURN                                                            DVI  140
      END                                                               DVI  150
      SUBROUTINE DCOND(DVAL,DOSE,TI,TM)                                 DCO   10
C                                                                       DCO   20
C     COMPUTE THE FINAL RETURN VALUE FOR THE FULL MODEL                 DCO   30
C                                                                       DCO   40
C     INPUTS:                                                           DCO   50
C     DOSE IN UNITS OF [10^20 N/CM^2 EDN]                               DCO   60
C     TI: IRRADIATION TEMP. [DEGC]                                      DCO   70
C     TM: MEASUREMENT TEMP. [DEGC]                                      DCO   80
C                                                                       DCO   90
C     OUTPUTS:                                                          DCO  100
C     DVAL: THE RETURN VALUE. THERMAL CONDUCTIVITY AT THE POINT.        DCO  110
C                                                                       DCO  120
      IMPLICIT REAL*8 (A-H,O-Z)                                         DCO  130
C                                                                       DCO  140
C                                                                       DCO  150
      DOSE = DOSE * 1.D-20                                              DCO  160
      DK0RT = 132.D0                                                    DCO  170
      DKK0 = 0.D0                                                       DCO  180
      ID = -1                                                           DCO  190
C                                                                       DCO  200
      CALL DIRRCOND(DKK0,ID,DOSE,TI)                                    DCO  210
C                                                                       DCO  220
      DK0TM = 0.D0                                                      DCO  230
C                                                                       DCO  240
      CALL DVIRCOND(DK0TM,TM)                                           DCO  250
C                                                                       DCO  260
      DK0TM = DK0TM * DK0RT                                             DCO  270
      DK0TI = 0.D0                                                      DCO  280
C                                                                       DCO  290
      CALL DVIRCOND(DK0TI,TI)                                           DCO  300
C                                                                       DCO  310
      DK0TI = DK0TI * DK0RT                                             DCO  320
      DCPTM = 0.D0                                                      DCO  330
C                                                                       DCO  340
      CALL DHEATCAP(DCPTM,TM)                                           DCO  350
C                                                                       DCO  360
      DCPTI = 0.D0                                                      DCO  370
C                                                                       DCO  380
      CALL DHEATCAP(DCPTI,TI)                                           DCO  390
C                                                                       DCO  400
      DVAL = DK0TM / (1.D0+(DCPTI/DCPTM)*(DK0TM/DK0TI)*(DKK0-1.D0))     DCO  410
      RETURN                                                            DCO  420
      END                                                               DCO  430