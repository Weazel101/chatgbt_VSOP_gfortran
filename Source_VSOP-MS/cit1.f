      SUBROUTINE GRIT(X,I,J,IO,L)                                       GRI   10
C                                                                       GRI   20
CGRIT --003 ***CITATION*** BLOCK DATA TRANSFER ROUTINE                  GRI   30
C                                                                       GRI   40
C     X  IS THE DATA ARRAY                                              GRI   50
C     I  IS ITS LENGTH                                                  GRI   60
C     J  IS THE RECORD ORDER NUMBER                                     GRI   70
C           WHEN J IS 0, THE UNIT IS ASSUMED TO BE POSITIONED AND       GRI   80
C           NO END OF FILE IS WRITTEN                                   GRI   90
C     IO IS THE I/O DEVICE, LOGICAL NUMBER                              GRI  100
C     L  IS THE I/O CONTROL (1- READ, 2- WRITE)                         GRI  110
C                                                                       GRI  120
C     WARNING *** USE SYMBOLIC PARAMETERS, NOT NUMBERS                  GRI  130
C                                                                       GRI  140
      DIMENSION X(I)                                                    GRI  150
C                                                                       GRI  160
C                                                                       GRI  170
      IF(J .EQ. 0) GOTO 102                                             GRI  180
      REWIND IO                                                         GRI  190
      IF(J .LE. 1) GOTO 102                                             GRI  200
      M = J - 1                                                         GRI  210
      DO 101 K=1,M                                                      GRI  220
        READ (IO)                                                       GRI  230
  101 CONTINUE                                                          GRI  240
  102 IF(L .EQ. 1) GOTO 103                                             GRI  250
      WRITE (IO) X                                                      GRI  260
      IF(J .EQ. 0) GOTO 105                                             GRI  270
      END FILE IO                                                       GRI  280
      GOTO 104                                                          GRI  290
  103 READ (IO) X                                                       GRI  300
  104 IF(J .EQ. 0) GOTO 105                                             GRI  310
      REWIND IO                                                         GRI  320
  105 RETURN                                                            GRI  330
      END                                                               GRI  340
      SUBROUTINE RQED(IXED,IND)                                         RQE   10
C                                                                       RQE   20
CRQED --004 ***CITATION*** EDIT REQUIRED/ CF-MANY ROUTINES              RQE   30
C                                            RERT                       RQE   40
C                                                                       RQE   50
C                                                                       RQE   60
C     IXED IS THE EDIT OPTION CYCLE                                     RQE   70
C     IND=0, YES EDIT          IND.NE.0, NO EDIT                        RQE   80
C                                                                       RQE   90
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,RQE  100
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   RQE  110
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), RQE  120
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    RQE  130
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    RQE  140
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   RQE  150
     6 IXPUT(9999),XPUT(9999)                                           RQE  160
C                                                                       RQE  170
C                                                                       RQE  180
      IND = 0                                                           RQE  190
      IF(IXED .EQ. 0) GOTO 101                                          RQE  200
      IF(IX(41) .GT. 0) GOTO 103                                        RQE  210
      IF(IX(14) .EQ. 0) GOTO 103                                        RQE  220
      GOTO 102                                                          RQE  230
  101 IND = 1                                                           RQE  240
      GOTO 103                                                          RQE  250
  102 CONTINUE                                                          RQE  260
      IND = IX(14) - (IX(14)/IXED) * IXED                               RQE  270
  103 CONTINUE                                                          RQE  280
      RETURN                                                            RQE  290
      END                                                               RQE  300
      SUBROUTINE CNTR                                                   CNT   10
C                                                                       CNT   20
CCNTR --025 ***CITATION*** READS INPUT SECTION 001/ CF-IPTM             CNT   30
C                                                                       CNT   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,CNT   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   CNT   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), CNT   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    CNT   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    CNT   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   CNT  100
     6 IXPUT(9999),XPUT(9999)                                           CNT  110
C                                                                       CNT  120
      COMMON /MU/ MU4                                                   CNT  130
C                                                                       CNT  140
      COMMON /IFLUX/ IFLX,YFLX(211),XFLX(211),IPOW                      CNT  150
C                                                                       CNT  160
C                                                                       CNT  170
      IFLX = 0                                                          CNT  180
      IPOW = 0                                                          CNT  190
      READ (IOIN,1000) (NGC(I),I=1,24)                                  CNT  200
      READ (IOIN,1000) (IEDG(I),I=1,24)                                 CNT  210
      IF(IEDG(10) .EQ. 2) IFLX = 2                                      CNT  220
      IF(IEDG(14) .EQ. 2) IPOW = 2                                      CNT  230
      ITMX(1) = 200                                                     CNT  240
      ITMX(2) = 200                                                     CNT  250
      ITMX(3) = 10                                                      CNT  260
      ITMX(4) = 2                                                       CNT  270
      ITMX(5) = 3                                                       CNT  280
      ITMX(19) = 60 / IX(1)                                             CNT  290
      ITMX(20) = 30 / IX(1)                                             CNT  300
      ITMX(21) = 60 / IX(1)                                             CNT  310
      ITMX(22) = 30 / IX(1)                                             CNT  320
      ITMX(23) = 60 / IX(1)                                             CNT  330
      ITMX(24) = 120 / IX(1)                                            CNT  340
      DO 101 N=6,18                                                     CNT  350
        ITMX(N) = 0                                                     CNT  360
  101 CONTINUE                                                          CNT  370
      READ (IOIN,1000) (NXTR1(I),I=1,24)                                CNT  380
      DO 103 I=1,24                                                     CNT  390
        IF(NXTR1(I)) 103,103,102                                        CNT  400
  102   ITMX(I) = NXTR1(I)                                              CNT  410
  103 CONTINUE                                                          CNT  420
      GLIM(1) = 1.5                                                     CNT  430
      GLIM(2) = 0.1                                                     CNT  440
      GLIM(3) = 1.0E+10                                                 CNT  450
      GLIM(4) = 1.0E+24                                                 CNT  460
      GLIM(5) = 0.0                                                     CNT  470
      GLIM(6) = 1.0                                                     CNT  480
      READ (IOIN,1001) (XTR1(I),I=1,6)                                  CNT  490
      DO 105 I=1,6                                                      CNT  500
        IF(XTR1(I)) 104,105,104                                         CNT  510
  104   GLIM(I) = XTR1(I)                                               CNT  520
  105 CONTINUE                                                          CNT  530
      IF(MU4 .GT. 1) GOTO 100                                           CNT  540
      WRITE (IOUT,1002)                                                 CNT  550
      WRITE (IOUT,1003) (NGC(I),I=1,24)                                 CNT  560
      WRITE (IOUT,1004) (IEDG(I),I=1,24)                                CNT  570
      WRITE (IOUT,1004) (ITMX(I),I=1,24)                                CNT  580
      WRITE (IOUT,1005) (GLIM(I),I=1,6)                                 CNT  590
  100 CONTINUE                                                          CNT  600
      IF(GLIM(6)) 107,106,107                                           CNT  610
  106 GLIM(6) = 1.0                                                     CNT  620
  107 IX(5) = NGC(10)                                                   CNT  630
      SPARE(50) = GLIM(6)                                               CNT  640
      RETURN                                                            CNT  650
C                                                                       CNT  660
 1000 FORMAT (24I3)                                                     CNT  670
 1001 FORMAT (6E12.0)                                                   CNT  680
 1002 FORMAT (1H0/1H0,'GENERAL CONTROL INPUT - SECTION 001')            CNT  690
 1003 FORMAT (1H0,24I4)                                                 CNT  700
 1004 FORMAT (1H ,24I4)                                                 CNT  710
 1005 FORMAT (1H ,6(1PE14.6))                                           CNT  720
      END                                                               CNT  730
      SUBROUTINE LVMX                                                   LVM   10
C                                                                       LVM   20
CLVMX --028 ***CITATION*** READS INPUT SECTION 004/ CF-IPTM             LVM   30
C                                                                       LVM   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,LVM   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   LVM   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), LVM   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    LVM   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    LVM   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   LVM  100
     6 IXPUT(9999),XPUT(9999)                                           LVM  110
C                                                                       LVM  120
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  LVM  130
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), LVM  140
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)LVM  150
     3 ,PDK(211)                                                        LVM  160
C                                                                       LVM  170
      DIMENSION NX(6),TX(6)                                             LVM  180
C                                                                       LVM  190
C                                                                       LVM  200
      NREGJ = 0                                                         LVM  210
      IMAX = 1                                                          LVM  220
      KBMAX = 1                                                         LVM  230
      NREGI = 1                                                         LVM  240
      NREGKB = 1                                                        LVM  250
      XSHI(1) = 1.0                                                     LVM  260
      XSHKB(1) = 1.0                                                    LVM  270
      MSHI(1) = 1                                                       LVM  280
      MSHKB(1) = 1                                                      LVM  290
      NDIM = IX(25)                                                     LVM  300
      NGEM = IX(26)                                                     LVM  310
      MGEM = 0                                                          LVM  320
      IF(NGEM-8) 102,101,102                                            LVM  330
  101 MGEM = 8                                                          LVM  340
      GOTO 103                                                          LVM  350
  102 IF(NGEM-12) 103,101,103                                           LVM  360
  103 READ (IOIN,1000) (NX(I),TX(I),I=1,6)                              LVM  370
      DO 105 I=1,6                                                      LVM  380
        IF(NX(I)) 106,106,104                                           LVM  390
  104   NREGJ = NREGJ + 1                                               LVM  400
        MSHJ(NREGJ) = NX(I)                                             LVM  410
        XSHJ(NREGJ) = TX(I)                                             LVM  420
  105 CONTINUE                                                          LVM  430
      GOTO 103                                                          LVM  440
  106 IF(NDIM-2) 116,107,107                                            LVM  450
  107 NREGI = 0                                                         LVM  460
  108 READ (IOIN,1000) (NX(I),TX(I),I=1,6)                              LVM  470
      DO 110 I=1,6                                                      LVM  480
        IF(NX(I)) 111,111,109                                           LVM  490
  109   NREGI = NREGI + 1                                               LVM  500
        MSHI(NREGI) = NX(I)                                             LVM  510
        XSHI(NREGI) = TX(I)                                             LVM  520
  110 CONTINUE                                                          LVM  530
      GOTO 108                                                          LVM  540
  111 IF(NDIM-2) 116,116,112                                            LVM  550
  112 NREGKB = 0                                                        LVM  560
  113 READ (IOIN,1000) (NX(I),TX(I),I=1,6)                              LVM  570
      DO 115 I=1,6                                                      LVM  580
        IF(NX(I)) 116,116,114                                           LVM  590
  114   NREGKB = NREGKB + 1                                             LVM  600
        MSHKB(NREGKB) = NX(I)                                           LVM  610
        XSHKB(NREGKB) = TX(I)                                           LVM  620
  115 CONTINUE                                                          LVM  630
      GOTO 113                                                          LVM  640
  116 LMAX = NREGJ * NREGI * NREGKB                                     LVM  650
      RETURN                                                            LVM  660
C                                                                       LVM  670
 1000 FORMAT (6(I3,E9.0))                                               LVM  680
      END                                                               LVM  690
      SUBROUTINE COMP(ISK,NRGN,NCOMP,IVX,JVX,LVX)                       COM   10
C                                                                       COM   20
CCOMP --030 ***CITATION*** READS INPUT SECTION 005 1,2-D/ CF-IPTM       COM   30
C                                                                       COM   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,COM   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   COM   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), COM   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    COM   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    COM   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   COM  100
     6 IXPUT(9999),XPUT(9999)                                           COM  110
C                                                                       COM  120
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  COM  130
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), COM  140
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)COM  150
     3 ,PDK(211)                                                        COM  160
C                                                                       COM  170
      DIMENSION NRGN(JVX,IVX),NCOMP(LVX)                                COM  180
C                                                                       COM  190
C                                                                       COM  200
      IF(ISK) 100,104,100                                               COM  210
  100 WRITE (IOUT,1000)                                                 COM  220
      NRE = 0                                                           COM  230
      DO 103 IR=1,NREGI                                                 COM  240
        READ (IOIN,1001) (NXTR1(N),N=1,NREGJ)                           COM  250
        DO 102 JR=1,NREGJ                                               COM  260
          NRE = NRE + 1                                                 COM  270
          NCOMP(NRE) = NXTR1(JR)                                        COM  280
          IF(NCOMP(NRE)) 101,101,102                                    COM  290
  101     NER(14) = 14                                                  COM  300
  102   CONTINUE                                                        COM  310
        WRITE (IOUT,1002) (NXTR1(N),N=1,NREGJ)                          COM  320
  103 CONTINUE                                                          COM  330
  104 CONTINUE                                                          COM  340
      I = 0                                                             COM  350
      DO 108 IR=1,NREGI                                                 COM  360
        NIPTS = MSHI(IR)                                                COM  370
        DO 107 INR=1,NIPTS                                              COM  380
          I = I + 1                                                     COM  390
          J = 0                                                         COM  400
          NRE = (IR-1) * NREGJ                                          COM  410
          DO 106 JR=1,NREGJ                                             COM  420
            NJPTS = MSHJ(JR)                                            COM  430
            NREE = NRE + JR                                             COM  440
            DO 105 JNR=1,NJPTS                                          COM  450
              J = J + 1                                                 COM  460
              NRGN(J,I) = NREE                                          COM  470
  105       CONTINUE                                                    COM  480
  106     CONTINUE                                                      COM  490
  107   CONTINUE                                                        COM  500
  108 CONTINUE                                                          COM  510
      IF(ISK) 109,112,109                                               COM  520
  109 CONTINUE                                                          COM  530
      MMAX = 0                                                          COM  540
      DO 110 NR=1,LMAX                                                  COM  550
        MMAX = MAX0(MMAX,NCOMP(NR))                                     COM  560
  110 CONTINUE                                                          COM  570
C                                                                       COM  580
      CALL CMOT(NRGN,NCOMP,IVX,JVX,LVX)                                 COM  590
C                                                                       COM  600
  112 CONTINUE                                                          COM  610
      RETURN                                                            COM  620
C                                                                       COM  630
 1000 FORMAT (1H0/1H0,'ZONE INPUT BY MATERIAL COMPOSITION')             COM  640
 1001 FORMAT (15I5)                                                     COM  650
 1002 FORMAT (1H ,24I5)                                                 COM  660
      END                                                               COM  670
      SUBROUTINE CMOT(NRGN,NCOMP,IVX,JVX,LVX)                           CMO   10
C                                                                       CMO   20
CCMOT --031 ***CITATION*** PRINTS MAP FOR 1- AND 2-D/ CF-COMP,IPTM      CMO   30
C                                                                       CMO   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,CMO   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   CMO   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), CMO   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    CMO   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    CMO   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   CMO  100
     6 IXPUT(9999),XPUT(9999)                                           CMO  110
C                                                                       CMO  120
      DIMENSION NRGN (JVX,IVX),NCOMP(LVX)                               CMO  130
C                                                                       CMO  140
C                                                                       CMO  150
      IS1 = 0                                                           CMO  160
      WRITE (IOUT,1000)                                                 CMO  170
      N1J = JMAX                                                        CMO  180
      N2J = 0                                                           CMO  190
  101 IF(N1J-24) 102,102,103                                            CMO  200
  102 N1L = N2J + 1                                                     CMO  210
      N2J = JMAX                                                        CMO  220
      IS1 = 1                                                           CMO  230
      GOTO 104                                                          CMO  240
  103 N1L = N2J + 1                                                     CMO  250
      N2J = N2J + 24                                                    CMO  260
      N1J = N1J - 24                                                    CMO  270
  104 CONTINUE                                                          CMO  280
      WRITE (IOUT,1001) (J,J=N1L,N2J)                                   CMO  290
      WRITE (IOUT,1002)                                                 CMO  300
      DO 107 I=1,IMAX                                                   CMO  310
        DO 106 J=N1L,N2J                                                CMO  320
          L = NRGN(J,I)                                                 CMO  330
          NXTR1(J) = NCOMP(L)                                           CMO  340
  106   CONTINUE                                                        CMO  350
        WRITE (IOUT,1003) I,(NXTR1(J),J= N1L,N2J)                       CMO  360
  107 CONTINUE                                                          CMO  370
      IF(IS1) 108,108,109                                               CMO  380
  108 WRITE (IOUT,1000)                                                 CMO  390
      GOTO 101                                                          CMO  400
  109 CONTINUE                                                          CMO  410
      RETURN                                                            CMO  420
C                                                                       CMO  430
 1000 FORMAT (1H0,'ZONE NUMBER AT EACH MESH INTERVAL')                  CMO  440
 1001 FORMAT (1H0,I8,23I5)                                              CMO  450
 1002 FORMAT (1H )                                                      CMO  460
 1003 FORMAT (1H ,I3,24I5)                                              CMO  470
      END                                                               CMO  480
      SUBROUTINE KOMP(ISK,NRGNE,NCOMP,IVX,JVX,KBVX,LVX)                 KOM   10
C                                                                       KOM   20
CKOMP --032 ***CITATION*** READS INPUT SECTION 005 FOR 3-D/ CF-IPTM     KOM   30
C                                                                       KOM   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KOM   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KOM   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KOM   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KOM   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KOM   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KOM  100
     6 IXPUT(9999),XPUT(9999)                                           KOM  110
C                                                                       KOM  120
      COMMON /AMESH/ BMESH(30),NREGI,NREGJ,NREGKB,XSHI(200),XSHJ(200),  KOM  130
     1 XSHKB(200),MSHI(200),MSHJ(200),MSHKB(200),Y(211),YY(211),X(211), KOM  140
     2 XX(211),Z(211),ZZ(211),ZONVOL(9999),AVZPD(9999),PDI(211),PDJ(211)KOM  150
     3 ,PDK(211)                                                        KOM  160
C                                                                       KOM  170
      DIMENSION NRGNE(JVX,IVX,KBVX),NCOMP(LVX)                          KOM  180
C                                                                       KOM  190
C                                                                       KOM  200
      IF(ISK) 100,105,100                                               KOM  210
  100 WRITE (IOUT,1000)                                                 KOM  220
      NRE = 0                                                           KOM  230
      DO 104 KBR=1,NREGKB                                               KOM  240
        WRITE (IOUT,1001) KBR                                           KOM  250
        DO 103 IR=1,NREGI                                               KOM  260
          READ (IOIN,1002) (NXTR1(N),N=1,NREGJ)                         KOM  270
          WRITE (IOUT,1003) (NXTR1(N),N=1,NREGJ)                        KOM  280
          DO 102 JR=1,NREGJ                                             KOM  290
            NRE = NRE + 1                                               KOM  300
            NCOMP(NRE) = NXTR1(JR)                                      KOM  310
            IF(NCOMP(NRE)) 101,101,102                                  KOM  320
  101       NER(14) = 14                                                KOM  330
  102     CONTINUE                                                      KOM  340
  103   CONTINUE                                                        KOM  350
  104 CONTINUE                                                          KOM  360
  105 CONTINUE                                                          KOM  370
      KB = 0                                                            KOM  380
      DO 111 KBR=1,NREGKB                                               KOM  390
        NKBPTS = MSHKB(KBR)                                             KOM  400
        DO 110 KBNR=1,NKBPTS                                            KOM  410
          KB = KB + 1                                                   KOM  420
          I = 0                                                         KOM  430
          DO 109 IR=1,NREGI                                             KOM  440
            NIPTS = MSHI(IR)                                            KOM  450
            DO 108 INR=1,NIPTS                                          KOM  460
              I = I + 1                                                 KOM  470
              J = 0                                                     KOM  480
              NRE = (KBR-1) * NREGI * NREGJ + (IR-1) * NREGJ            KOM  490
              DO 107 JR=1,NREGJ                                         KOM  500
                NJPTS = MSHJ(JR)                                        KOM  510
                NREE = NRE + JR                                         KOM  520
                DO 106 JNR=1,NJPTS                                      KOM  530
                  J = J + 1                                             KOM  540
                  NRGNE(J,I,KB) = NREE                                  KOM  550
  106           CONTINUE                                                KOM  560
  107         CONTINUE                                                  KOM  570
  108       CONTINUE                                                    KOM  580
  109     CONTINUE                                                      KOM  590
  110   CONTINUE                                                        KOM  600
  111 CONTINUE                                                          KOM  610
      IF(ISK) 112,115,112                                               KOM  620
  112 CONTINUE                                                          KOM  630
      MMAX = 0                                                          KOM  640
      DO 113 NR=1,LMAX                                                  KOM  650
        MMAX = MAX0(MMAX,NCOMP(NR))                                     KOM  660
  113 CONTINUE                                                          KOM  670
C                                                                       KOM  680
      CALL KMOT(NRGNE,NCOMP,IVX,JVX,KBVX,LVX)                           KOM  690
C                                                                       KOM  700
  115 CONTINUE                                                          KOM  710
      RETURN                                                            KOM  720
C                                                                       KOM  730
 1000 FORMAT (1H0/1H0,'ZONE INPUT BY MATERIAL COMPOSITION')             KOM  740
 1001 FORMAT (1H0,'  PLANE NUMBER',I4)                                  KOM  750
 1002 FORMAT (15I5)                                                     KOM  760
 1003 FORMAT (1H ,24I5)                                                 KOM  770
      END                                                               KOM  780
      SUBROUTINE KMOT(NRGNE,NCOMP,IVX,JVX,KBVX,LVX)                     KMO   10
C                                                                       KMO   20
CKMOT --033 ***CITATION*** PRINTS MAP FOR 3-D/ CF-KOMP,IPTM             KMO   30
C                                                                       KMO   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KMO   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KMO   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KMO   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KMO   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KMO   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KMO  100
     6 IXPUT(9999),XPUT(9999)                                           KMO  110
C                                                                       KMO  120
      DIMENSION NRGNE(JVX,IVX,KBVX),NCOMP(LVX)                          KMO  130
C                                                                       KMO  140
C                                                                       KMO  150
      IS1 = 0                                                           KMO  160
      WRITE (IOUT,1000)                                                 KMO  170
      DO 110 KB=1,KBMAX                                                 KMO  180
        WRITE (IOUT,1001) KB                                            KMO  190
        N1J = JMAX                                                      KMO  200
        N2J = 0                                                         KMO  210
  101   IF(N1J-24) 102,102,103                                          KMO  220
  102   N1L = N2J + 1                                                   KMO  230
        N2J = JMAX                                                      KMO  240
        IS1 = 1                                                         KMO  250
        GOTO 104                                                        KMO  260
  103   N1L = N2J + 1                                                   KMO  270
        N2J = N2J + 24                                                  KMO  280
        N1J = N1J - 24                                                  KMO  290
  104   CONTINUE                                                        KMO  300
        WRITE (IOUT,1002) (J,J=N1L,N2J)                                 KMO  310
        WRITE (IOUT,1003)                                               KMO  320
        DO 107 I=1,IMAX                                                 KMO  330
          DO 106 J=N1L,N2J                                              KMO  340
            L = NRGNE(J,I,KB)                                           KMO  350
            NXTR1(J) = NCOMP(L)                                         KMO  360
  106     CONTINUE                                                      KMO  370
          WRITE (IOUT,1004) I,(NXTR1(J),J= N1L,N2J)                     KMO  380
  107   CONTINUE                                                        KMO  390
        IF(IS1) 108,108,109                                             KMO  400
  108   WRITE (IOUT,1000)                                               KMO  410
        GOTO 101                                                        KMO  420
  109   IS1 = 0                                                         KMO  430
  110 CONTINUE                                                          KMO  440
      RETURN                                                            KMO  450
C                                                                       KMO  460
 1000 FORMAT (1H0,'ZONE NUMBER AT EACH MESH INTERVAL')                  KMO  470
 1001 FORMAT (1H0/1H0,'SPECIFICATION FOR LAYER NUMBER',I5)              KMO  480
 1002 FORMAT (1H0,I8,23I5)                                              KMO  490
 1003 FORMAT (1H )                                                      KMO  500
 1004 FORMAT (1H ,I3,24I5)                                              KMO  510
      END                                                               KMO  520
      SUBROUTINE FXSO(SPAR,BIEMS,PVOL,NRGNE,NCOMP,JVX,IVX,KBVX,KVX,LVX) FXS   10
C                                                                       FXS   20
CFXSO --040 ***CITATION*** READS INPUT SECTION 026         /CF - IPTM   FXS   30
C                                                                       FXS   40
      REAL*8 SAM                                                        FXS   50
C                                                                       FXS   60
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,FXS   70
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   FXS   80
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), FXS   90
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    FXS  100
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    FXS  110
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   FXS  120
     6 IXPUT(9999),XPUT(9999)                                           FXS  130
C                                                                       FXS  140
      DIMENSION SPAR(JVX,IVX,KBVX),BIEMS(KVX),PVOL(LVX),NRGNE(JVX,IVX,  FXS  150
     > KBVX),NCOMP(LVX)                                                 FXS  160
C                                                                       FXS  170
C                                                                       FXS  180
      WRITE (IOUT,1000)                                                 FXS  190
      READ (IOIN,1001) NFX1,NFX2                                        FXS  200
      IX(132) = NFX1                                                    FXS  210
      N5 = NUAC(5)                                                      FXS  220
      DO 102 KB=1,KBMAX                                                 FXS  230
        DO 101 I=1,IMAX                                                 FXS  240
          DO 100 J=1,JMAX                                               FXS  250
            SPAR(J,I,KB) = 0.0                                          FXS  260
  100     CONTINUE                                                      FXS  270
  101   CONTINUE                                                        FXS  280
  102 CONTINUE                                                          FXS  290
      IF(NFX1 .GT. 0) GOTO 141                                          FXS  300
C                                                                       FXS  310
C     INPUT DISTRIBUTION FUNCTION                                       FXS  320
C                                                                       FXS  330
      READ (IOIN,1002) (BIEMS(K),K=1,KMAX)                              FXS  340
      IF(NFX1 .LT. 0) GOTO 122                                          FXS  350
C                                                                       FXS  360
C     INPUT SOURCE BY MESH POINT                                        FXS  370
C                                                                       FXS  380
  103 READ (IOIN,1003) JL,JR,IT,IB,KBF,KBB,X                            FXS  390
      IF(JL .LE. 0) GOTO 130                                            FXS  400
C                                                                       FXS  410
C     CHECK MESH SPECIFICATIONS                                         FXS  420
C                                                                       FXS  430
      IF(JL .GE. 1 .AND. JL .LE. JMAX) GOTO 104                         FXS  440
      GOTO 140                                                          FXS  450
  104 IF(JR .GE. 1 .AND. JR .LE. JMAX) GOTO 105                         FXS  460
      GOTO 140                                                          FXS  470
  105 IF(JL .LE. JR) GOTO 106                                           FXS  480
      GOTO 140                                                          FXS  490
  106 IF(N5 .LE. 5) GOTO 112                                            FXS  500
      IF(IT .GE. 1 .AND. IT .LE. IMAX) GOTO 107                         FXS  510
      GOTO 140                                                          FXS  520
  107 IF(IB .GE. 1 .AND. IB .LE. IMAX) GOTO 108                         FXS  530
      GOTO 140                                                          FXS  540
  108 IF(IT .LE. IB) GOTO 109                                           FXS  550
      GOTO 140                                                          FXS  560
  109 IF(N5 .LE. 10) GOTO 113                                           FXS  570
      IF(KBF .GE. 1 .AND. KBF .LE. KBMAX) GOTO 110                      FXS  580
      GOTO 140                                                          FXS  590
  110 IF(KBB .GE. 1 .AND. KBB .LE. KBMAX) GOTO 111                      FXS  600
      GOTO 140                                                          FXS  610
  111 IF(KBF .LE. KBB) GOTO 114                                         FXS  620
      GOTO 140                                                          FXS  630
  112 IT = 1                                                            FXS  640
      IB = 1                                                            FXS  650
  113 KBF = 1                                                           FXS  660
      KBB = 1                                                           FXS  670
  114 CONTINUE                                                          FXS  680
      IF(X .NE. 0.0) GOTO 117                                           FXS  690
      DO 116 KB=KBF,KBB                                                 FXS  700
        DO 115 I=IT,IB                                                  FXS  710
          READ (IOIN,1002) (SPAR(J,I,KB),J=JL,JR)                       FXS  720
  115   CONTINUE                                                        FXS  730
  116 CONTINUE                                                          FXS  740
      GOTO 121                                                          FXS  750
  117 CONTINUE                                                          FXS  760
      DO 120 KB=KBF,KBB                                                 FXS  770
        DO 119 I=IT,IB                                                  FXS  780
          DO 118 J=JL,JR                                                FXS  790
            SPAR(J,I,KB) = X                                            FXS  800
  118     CONTINUE                                                      FXS  810
  119   CONTINUE                                                        FXS  820
  120 CONTINUE                                                          FXS  830
  121 CONTINUE                                                          FXS  840
      GOTO 103                                                          FXS  850
  122 CONTINUE                                                          FXS  860
C                                                                       FXS  870
C     INPUT SOURCE BY ZONE                                              FXS  880
C                                                                       FXS  890
      IT = 0                                                            FXS  900
      I2 = 0                                                            FXS  910
  123 I1 = I2 + 1                                                       FXS  920
      I2 = I1 + 5                                                       FXS  930
      READ (IOIN,1004) (NXTR1(N),XTR1(N),N=I1,I2)                       FXS  940
      DO 124 N=I1,I2                                                    FXS  950
        IF(NXTR1(N) .EQ. 0) GOTO 125                                    FXS  960
        IT = IT + 1                                                     FXS  970
  124 CONTINUE                                                          FXS  980
      GOTO 123                                                          FXS  990
  125 CONTINUE                                                          FXS 1000
      WRITE (IOUT,1005)                                                 FXS 1010
      WRITE (IOUT,1006) (NXTR1(N),XTR1(N),N=1,IT)                       FXS 1020
      DO 129 N=1,IT                                                     FXS 1030
        M = NXTR1(N)                                                    FXS 1040
        X = XTR1(N)                                                     FXS 1050
        DO 128 KB=1,KBMAX                                               FXS 1060
          DO 127 I=1,IMAX                                               FXS 1070
            DO 126 J=1,JMAX                                             FXS 1080
              L = NRGNE(J,I,KB)                                         FXS 1090
              MM = NCOMP(L)                                             FXS 1100
              IF(M .NE. MM) GOTO 126                                    FXS 1110
              SPAR(J,I,KB) = X                                          FXS 1120
  126       CONTINUE                                                    FXS 1130
  127     CONTINUE                                                      FXS 1140
  128   CONTINUE                                                        FXS 1150
  129 CONTINUE                                                          FXS 1160
  130 CONTINUE                                                          FXS 1170
      DO 133 KB=1,KBMAX                                                 FXS 1180
        DO 132 I=1,IMAX                                                 FXS 1190
          DO 131 J=1,JMAX                                               FXS 1200
            L = NRGNE(J,I,KB)                                           FXS 1210
            SPAR(J,I,KB) = SPAR(J,I,KB) * PVOL(L)                       FXS 1220
  131     CONTINUE                                                      FXS 1230
  132   CONTINUE                                                        FXS 1240
  133 CONTINUE                                                          FXS 1250
      SAM = 0.0D+0                                                      FXS 1260
      DO 136 KB=1,KBMAX                                                 FXS 1270
        DO 135 I=1,IMAX                                                 FXS 1280
          DO 134 J=1,JMAX                                               FXS 1290
            SAM = SAM + SPAR(J,I,KB)                                    FXS 1300
  134     CONTINUE                                                      FXS 1310
  135   CONTINUE                                                        FXS 1320
  136 CONTINUE                                                          FXS 1330
      SPARE(88) = SAM                                                   FXS 1340
      SAM = 0.0D+0                                                      FXS 1350
      DO 137 K=1,KMAX                                                   FXS 1360
        SAM = SAM + BIEMS(K)                                            FXS 1370
  137 CONTINUE                                                          FXS 1380
      DO 138 K=1,KMAX                                                   FXS 1390
        BIEMS(K) = BIEMS(K) / SAM                                       FXS 1400
  138 CONTINUE                                                          FXS 1410
C                                                                       FXS 1420
C     EDIT FIXED SOURCE                                                 FXS 1430
C                                                                       FXS 1440
      WRITE (IOUT,1007) (BIEMS(K),K=1,KMAX),SAM                         FXS 1450
      IF(NFX2 .EQ. 0) GOTO 139                                          FXS 1460
      WRITE (IOUT,1008)                                                 FXS 1470
C                                                                       FXS 1480
      CALL BEER(SPAR,JVX,IVX,KBVX)                                      FXS 1490
C                                                                       FXS 1500
  139 CONTINUE                                                          FXS 1510
      WRITE (IOUT,1009) SPARE(88)                                       FXS 1520
      GOTO 146                                                          FXS 1530
  140 CONTINUE                                                          FXS 1540
      WRITE (IOUT,1010) JL,JR,IT,IB,KBF,KBB                             FXS 1550
      STOP 69                                                           FXS 1560
  141 CONTINUE                                                          FXS 1570
C                                                                       FXS 1580
C     INPUT SOURCE FROM EXTERNAL DEVICE                                 FXS 1590
C                                                                       FXS 1600
      SAM = 0.0D+0                                                      FXS 1610
      IOFS = IX(84)                                                     FXS 1620
C                                                                       FXS 1630
C     IOFS IS LOGICAL 17                                                FXS 1640
C                                                                       FXS 1650
      REWIND IOFS                                                       FXS 1660
      DO 145 K=1,KMAX                                                   FXS 1670
        READ(IOFS) SPAR                                                 FXS 1680
        DO 144 KB=1,KBMAX                                               FXS 1690
          DO 143 I=1,IMAX                                               FXS 1700
            DO 142 J=1,JMAX                                             FXS 1710
              SAM = SAM + SPAR(J,I,KB)                                  FXS 1720
  142       CONTINUE                                                    FXS 1730
  143     CONTINUE                                                      FXS 1740
  144   CONTINUE                                                        FXS 1750
        BIEMS(K) = 1.0                                                  FXS 1760
        IF(NFX2 .EQ. 0) GOTO 145                                        FXS 1770
        WRITE (IOUT,1011) K                                             FXS 1780
C                                                                       FXS 1790
        CALL BEER(SPAR,JVX,IVX,KBVX)                                    FXS 1800
C                                                                       FXS 1810
  145 CONTINUE                                                          FXS 1820
      SPARE(88) = SAM                                                   FXS 1830
      WRITE (IOUT,1009) SPARE(88)                                       FXS 1840
      REWIND IOFS                                                       FXS 1850
  146 CONTINUE                                                          FXS 1860
      IF(SPARE(88) .NE. 0.0) GOTO 147                                   FXS 1870
      WRITE (IOUT,1012)                                                 FXS 1880
      NER(51) = 51                                                      FXS 1890
  147 CONTINUE                                                          FXS 1900
      RETURN                                                            FXS 1910
C                                                                       FXS 1920
 1000 FORMAT (1H0/1H0,'FIXED SOURCE INPUT SECTION 026')                 FXS 1930
 1001 FORMAT (24I3)                                                     FXS 1940
 1002 FORMAT (6E12.0)                                                   FXS 1950
 1003 FORMAT (6I4,E12.0)                                                FXS 1960
 1004 FORMAT (6(I3,E9.0))                                               FXS 1970
 1005 FORMAT (1H0,'ZONE-SOURCE(N/CC-SEC)')                              FXS 1980
 1006 FORMAT (1H ,I6,1PE13.5,I6,E13.5,I6,E13.5,I6,E13.5,I6,E13.5,I6,    FXS 1990
     1 E13.5)                                                           FXS 2000
 1007 FORMAT (1H0,'SOURCE DISTRIBUTION AND SUM'/(1H ,10(1PE12.4)))      FXS 2010
 1008 FORMAT (1H0,'POINT NEUTRON SOURCE- N/SEC')                        FXS 2020
 1009 FORMAT (1H0,'TOTAL FIXED SOURCE',1PE15.6,' N/SEC')                FXS 2030
 1010 FORMAT (1H1,'**** SOMETHING IS WRONG WITH THE FIXED SOURCE INPUT',FXS 2040
     1 6I5,' NOW STOP ****')                                            FXS 2050
 1011 FORMAT (1H0,'POINT NEUTRON SOURCE(N/SEC) FOR GROUP',I5)           FXS 2060
 1012 FORMAT (1H0,'***WARNING- THE TOTAL FIXED SOURCE IS ZERO ***')     FXS 2070
      END                                                               FXS 2080
      SUBROUTINE BEER(SPAR,JVX,IVX,KBVX)                                BEE   10
C                                                                       BEE   20
CBEER --041 ***CITATION*** EDITS FIXED SOURCE              /CF - FXSO   BEE   30
C                                                                       BEE   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,BEE   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   BEE   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), BEE   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    BEE   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    BEE   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   BEE  100
     6 IXPUT(9999),XPUT(9999)                                           BEE  110
C                                                                       BEE  120
      DIMENSION SPAR(JVX,IVX,KBVX)                                      BEE  130
C                                                                       BEE  140
C                                                                       BEE  150
      IS1 = 0                                                           BEE  160
      IS2 = 0                                                           BEE  170
      DO 110 KB=1,KBMAX                                                 BEE  180
        IF(NUAC(5) .GT. 10) WRITE (IOUT,1000) KB                        BEE  190
        N1J = JMAX                                                      BEE  200
        N2J = 0                                                         BEE  210
  100   N1I = IMAX                                                      BEE  220
        N2I = 0                                                         BEE  230
        IF(N1J-11) 101,101,102                                          BEE  240
  101   N1L = N2J + 1                                                   BEE  250
        N2J = JMAX                                                      BEE  260
        IS1 = 1                                                         BEE  270
        GOTO 103                                                        BEE  280
  102   N1L = N2J + 1                                                   BEE  290
        N2J = N2J + 11                                                  BEE  300
        N1J = N1J - 11                                                  BEE  310
  103   CONTINUE                                                        BEE  320
        IF(N1I-50) 104,104,105                                          BEE  330
  104   N2L = N2I + 1                                                   BEE  340
        N2I = IMAX                                                      BEE  350
        IS2 = 1                                                         BEE  360
        GOTO 106                                                        BEE  370
  105   N2L = N2I + 1                                                   BEE  380
        N2I = N2I + 50                                                  BEE  390
        N1I = N1I - 50                                                  BEE  400
  106   WRITE (IOUT,1001) (J,J=N1L,N2J)                                 BEE  410
        DO 107 I=N2L,N2I                                                BEE  420
          WRITE (IOUT,1002) I,(SPAR (J,I,KB),J=N1L,N2J)                 BEE  430
  107   CONTINUE                                                        BEE  440
        IF(IS2) 103,103,108                                             BEE  450
  108   IS2 = 0                                                         BEE  460
        IF(IS1) 100,100,109                                             BEE  470
  109   IS1 = 0                                                         BEE  480
  110 CONTINUE                                                          BEE  490
      RETURN                                                            BEE  500
C                                                                       BEE  510
 1000 FORMAT (1H0,' PLANE NUMBER',I3)                                   BEE  520
 1001 FORMAT (1H0,I10,10I11)                                            BEE  530
 1002 FORMAT (1H ,I3,1PE12.3,10E11.3)                                   BEE  540
      END                                                               BEE  550
      SUBROUTINE KSIG(KVX,MVX,NVX,NSETVX)                               KSI   10
C                                                                       KSI   20
CKSIG --052 ***CITATION*** CALCULATES A(K1-K18)/ CF-IPTM                KSI   30
C                                                                       KSI   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KSI   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KSI   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KSI   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KSI   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KSI   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KSI  100
     6 IXPUT(9999),XPUT(9999)                                           KSI  110
C                                                                       KSI  120
      COMMON /AKADD/ KAY(1),K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13, KSI  130
     1 K131,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,    KSI  140
     2 K28,K29,K30,K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,K41,K42,K43, KSI  150
     3 K44,K45,K46,K47,K48,K49,K50,K51,K52,K53,K54,K55,K56,K57,K58,     KSI  160
     4 K59,K60,K61,K62,K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,     KSI  170
     5 K74,K75,K76,K77,K78,K79,K80,K81,K82,K83,K84,K85,K86,K87,K88,     KSI  180
     6 K89,K90,K91,K92,K93,K94,K95,K96,K97,K98,K99,K100,NDATA,KNRGN,    KSI  190
     7 KNCOMP,KPVOL,KRVOL,MEMORY,MEMX                                   KSI  200
C                                                                       KSI  210
C                                                                       KSI  220
      K1 = 1                                                            KSI  230
      KNNSX = KVX * NVX * NSETVX                                        KSI  240
      K2 = K1 + KNNSX                                                   KSI  250
      K3 = K2 + KNNSX                                                   KSI  260
      K4 = K3 + KNNSX                                                   KSI  270
      K5 = K4 + KNNSX                                                   KSI  280
      K6 = K5 + KNNSX                                                   KSI  290
      K7 = K6 + KVX * KVX                                               KSI  300
      K8 = K7 + KVX * NSETVX                                            KSI  310
      K9 = K8 + NVX * NSETVX * 10                                       KSI  320
      NNSX = NVX * NSETVX                                               KSI  330
      K10 = K9 + NNSX                                                   KSI  340
      K12 = K10 + NSETVX * 9999                                         KSI  350
      K13 = K12 + NNSX                                                  KSI  360
      K14 = K13 + NNSX                                                  KSI  370
      K15 = K14 + KVX * MVX * 10                                        KSI  380
      K16 = K15 + KVX * MVX                                             KSI  390
      K17 = K16 + KVX                                                   KSI  400
      K18 = K17 + NVX * MVX                                             KSI  410
C                                                                       KSI  420
C     OVERLAID STORAGE                                                  KSI  430
C                                                                       KSI  440
      K30 = K18 + NVX * IX(168) + 2 * KVX * MVX                         KSI  450
      K31 = K30 + 20 * NVX * NSETVX                                     KSI  460
      K32 = K31 + NSETVX * 20                                           KSI  470
      K33 = K32 + NSETVX * 20                                           KSI  480
      K34 = K33 + 1000                                                  KSI  490
C                                                                       KSI  500
      RETURN                                                            KSI  510
      END                                                               KSI  520
      SUBROUTINE KRST(IVX,JVX,KBVX,KVX,LVX,MVX,NVX, IVXP1,JVXP1,KBVXP1, KRS   10
     > NSETVX,NVO,IVO,IVZ,KVZ,N3DDIM)                                   KRS   20
C                                                                       KRS   30
CKRST --053 ***CITATION*** CALCULATES A(K19-K62)/ CF-INPT               KRS   40
C                                                                       KRS   50
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KRS   60
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KRS   70
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KRS   80
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KRS   90
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KRS  100
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KRS  110
     6 IXPUT(9999),XPUT(9999)                                           KRS  120
C                                                                       KRS  130
      COMMON /AKADD/ KAY(1),K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13, KRS  140
     1 K131,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,    KRS  150
     2 K28,K29,K30,K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,K41,K42,K43, KRS  160
     3 K44,K45,K46,K47,K48,K49,K50,K51,K52,K53,K54,K55,K56,K57,K58,     KRS  170
     4 K59,K60,K61,K62,K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,     KRS  180
     5 K74,K75,K76,K77,K78,K79,K80,K81,K82,K83,K84,K85,K86,K87,K88,     KRS  190
     6 K89,K90,K91,K92,K93,K94,K95,K96,K97,K98,K99,K100,NDATA,KNRGN,    KRS  200
     7 KNCOMP,KPVOL,KRVOL,MEMORY,MEMX                                   KRS  210
C                                                                       KRS  220
C                                                                       KRS  230
      N3DDIM = IVX * JVX * KBVXP1 * KVX                                 KRS  240
      IF(KBVX .EQ. 1) N3DDIM = 1                                        KRS  250
      JIKBX = JVX * IVX * KBVX                                          KRS  260
      JDP = JIKBX                                                       KRS  270
C                                                                       KRS  280
C     THE STEP ON THE NEXT CARD IS FOR DOUBLE PRECISION, IBM/360        KRS  290
C           REMOVE THE CARD IF IT DOES NOT APPLY ON YOUR MACHINE        KRS  300
C                                                                       KRS  310
      IF(KBVX .EQ. 1) JDP = 2 * JDP                                     KRS  320
      MKX = MVX * KVX                                                   KRS  330
      K38 = K18 + NVO                                                   KRS  340
      K39 = K38 + MKX                                                   KRS  350
      K19 = K39 + MKX                                                   KRS  360
C                                                                       KRS  370
C     THE STEP ON THE NEXT CARD IS FOR DOUBLE PRECISION                 KRS  380
C                                                                       KRS  390
      K19 = 2 * (K19/2) + 1                                             KRS  400
      K20 = K19 + JDP                                                   KRS  410
      K23 = K20 + JDP                                                   KRS  420
      N1 = MKX * KVX                                                    KRS  430
      N2 = KVX * (NSETVX+2) * KVX                                       KRS  440
      N3 = MAX0(N1,N2)                                                  KRS  450
      K24 = K23 + N3                                                    KRS  460
C                                                                       KRS  470
C     THE STEP ON THE NEXT CARD IS FOR DOUBLE PRECISION                 KRS  480
C                                                                       KRS  490
      K24 = 2 * (K24/2) + 1                                             KRS  500
      JIKBKX = JIKBX * KVX                                              KRS  510
      JDP = JIKBKX                                                      KRS  520
C                                                                       KRS  530
C     THE STEP ON THE NEXT CARD IS FOR DOUBLE PRECISION                 KRS  540
C                                                                       KRS  550
      IF(KBVX .EQ. 1) JDP = 2 * JDP                                     KRS  560
      K41 = K24 + JDP                                                   KRS  570
C                                                                       KRS  580
C     OVERLAID STORAGE                                                  KRS  590
C                                                                       KRS  600
      K54 = K34 + NVX                                                   KRS  610
      K55 = K54 + NVX                                                   KRS  620
      K56 = K55 + NVX                                                   KRS  630
      K57 = K56 + NVX                                                   KRS  640
      K58 = K57 + NVX                                                   KRS  650
      K59 = K58 + NVX                                                   KRS  660
      K60 = K59 + IVO                                                   KRS  670
      K61 = K60 + IVO                                                   KRS  680
      K42 = K41 + JIKBX                                                 KRS  690
      NT1 = NVX * MVX * 10                                              KRS  700
      K42 = MAX0(K42,K19+NT1,K61+IVO)                                   KRS  710
      K43 = K42 + 1                                                     KRS  720
      K44 = K43 + NVX * 6                                               KRS  730
      K45 = K44 + MVX * 10                                              KRS  740
      K46 = K45 + KVX                                                   KRS  750
      K47 = K46 + KVX                                                   KRS  760
      K48 = K47 + KVX                                                   KRS  770
      K49 = K48 + KVX * 20                                              KRS  780
C                                                                       KRS  790
C     THE STEP ON THE NEXT CARD IS FOR DOUBLE PRECISION                 KRS  800
C                                                                       KRS  810
      K49 = 2 * (K49/2) + 1                                             KRS  820
      KVXD = KVX                                                        KRS  830
C                                                                       KRS  840
C     THE STEP ON THE NEXT CARD IS FOR DOUBLE PRECISION                 KRS  850
C                                                                       KRS  860
      KVXD = 2 * KVX                                                    KRS  870
      K50 = K49 + KVXD                                                  KRS  880
      K51 = K50 + KVX                                                   KRS  890
      K6X = KVX * 6                                                     KRS  900
      K52 = K51 + K6X                                                   KRS  910
      K62 = K52 + K6X                                                   KRS  920
      RETURN                                                            KRS  930
      END                                                               KRS  940
      SUBROUTINE CNIO(IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,IVZ,KVZ,      CNI   10
     > N3DDIM,LVX,NBLOCK,IOVX,IOVZ)                                     CNI   20
C                                                                       CNI   30
CCNIO --054 ***CITATION*** CALCULATES A(K64-K67)/ CF-IPTM               CNI   40
C                                                                       CNI   50
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,CNI   60
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   CNI   70
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), CNI   80
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    CNI   90
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    CNI  100
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   CNI  110
     6 IXPUT(9999),XPUT(9999)                                           CNI  120
C                                                                       CNI  130
      COMMON /AKADD/ KAY(1),K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13, CNI  140
     1 K131,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,    CNI  150
     2 K28,K29,K30,K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,K41,K42,K43, CNI  160
     3 K44,K45,K46,K47,K48,K49,K50,K51,K52,K53,K54,K55,K56,K57,K58,     CNI  170
     4 K59,K60,K61,K62,K63,K64,K65,K66,K67,K68,K69,K70,K71,K72,K73,     CNI  180
     5 K74,K75,K76,K77,K78,K79,K80,K81,K82,K83,K84,K85,K86,K87,K88,     CNI  190
     6 K89,K90,K91,K92,K93,K94,K95,K96,K97,K98,K99,K100,NDATA,KNRGN,    CNI  200
     7 KNCOMP,KPVOL,KRVOL,MEMVRY,MEMX                                   CNI  210
C                                                                       CNI  220
      COMMON /MU/ MU4                                                   CNI  230
C                                                                       CNI  240
C                                                                       CNI  250
      NHEX = 1                                                          CNI  260
      MVX = MMAX                                                        CNI  270
      LKX = LVX * KVX                                                   CNI  280
      MKX = MVX * KVX                                                   CNI  290
      MKXD = MKX                                                        CNI  300
C                                                                       CNI  310
C     THE STEP ON THE NEXT CARD IS FOR DOUBLE PRECISION, IBM/360        CNI  320
C                                                                       CNI  330
      MKXD = 2 * MKX                                                    CNI  340
      IF(IVX .LT. IVZ) NHEX = 2                                         CNI  350
      N1 = JVX * IVX * KBVX                                             CNI  360
      N2 = JVXP1 * IVZ * KBVX * NHEX                                    CNI  370
      N3 = JVX * IVXP1 * KBVX                                           CNI  380
      N4 = JVX * IVX * KBVXP1                                           CNI  390
      IF(KBVX .EQ. 1) N4 = 1                                            CNI  400
      M1 = N1 * KVX                                                     CNI  410
      M2 = N2 * KVX                                                     CNI  420
      M3 = N3 * KVX                                                     CNI  430
      M4 = N4 * KVX                                                     CNI  440
      IF(KBVX .EQ. 1) M4 = 1                                            CNI  450
      MT = M2 + M3 + M4                                                 CNI  460
      NDQ = MAX0(1+LKX+2*MKX+MKXD+N1,8*MKX,NBLOCK)                      CNI  470
      NDR = MAX0(1+LKX+2*MKX+MKXD+M1,8*MKX,NBLOCK)                      CNI  480
      NT = MT - N2 - N3 - N4 - NDQ + NDR                                CNI  490
      IF(NGC(23) .GT. 0) GOTO 101                                       CNI  500
      NSPACE = MEMVRY - K64 - 3 * LVX - N1 - NDR                        CNI  510
      IF(NSPACE .GE. MT) GOTO 102                                       CNI  520
  101 CONTINUE                                                          CNI  530
      IOVX = 1                                                          CNI  540
      IOVZ = 2                                                          CNI  550
      N3DDIM = N4                                                       CNI  560
      K65 = K64 + N2                                                    CNI  570
      K66 = K65 + N3                                                    CNI  580
      K63 = K66 + N4                                                    CNI  590
      K21 = K63 + N1                                                    CNI  600
C                                                                       CNI  610
C     THE STEP ON THE NEXT CARD IS FOR DOUBLE PRECISION                 CNI  620
C                                                                       CNI  630
      K21 = 2 * (K21/2) + 1                                             CNI  640
      IX(37) = K64                                                      CNI  650
      IX(38) = K21 - 1                                                  CNI  660
      GOTO 103                                                          CNI  670
  102 IOVX = KVX                                                        CNI  680
      IOVZ = KVZ                                                        CNI  690
      K65 = K64 + M2                                                    CNI  700
      K66 = K65 + M3                                                    CNI  710
      K63 = K66 + M4                                                    CNI  720
      K21 = K63 + M1                                                    CNI  730
C                                                                       CNI  740
C     THE STEP ON THE NEXT CARD IS FOR DOUBLE PRECISION                 CNI  750
C                                                                       CNI  760
      K21 = 2 * (K21/2) + 1                                             CNI  770
      NDQ = NDR                                                         CNI  780
      IX(37) = 0                                                        CNI  790
      IX(38) = 0                                                        CNI  800
      N3DDIM = M4                                                       CNI  810
  103 CONTINUE                                                          CNI  820
      K29 = K21 + MKXD                                                  CNI  830
      K36 = K29 + LKX                                                   CNI  840
      K37 = K36 + MKX                                                   CNI  850
      K40 = K63                                                         CNI  860
      K67 = K63                                                         CNI  870
      K68 = K63 + NDQ                                                   CNI  880
      IF(MU4 .EQ. 1) WRITE (IOUT,1000) NT                               CNI  890
      IX(169) = K64                                                     CNI  900
      IX(170) = K21 - 1                                                 CNI  910
      RETURN                                                            CNI  920
C                                                                       CNI  930
 1000 FORMAT (1H0,'CORE STORAGE DIFFERENCE (WORDS) EQUATION CONSTANTS I/CNI  940
     1O INSTEAD OF STORED',I8)                                          CNI  950
      END                                                               CNI  960
      SUBROUTINE BNSB(BBND,BND,KVX,IRV)                                 BNS   10
C                                                                       BNS   20
CBNSB --055 ***CITATION*** PUT BND. COND. CNSTS. IN A(N)/ CF-IPTM       BNS   30
C                                                                       BNS   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,BNS   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   BNS   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), BNS   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    BNS   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    BNS   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   BNS  100
     6 IXPUT(9999),XPUT(9999)                                           BNS  110
C                                                                       BNS  120
      DIMENSION BBND(KVX),BND(6,KVX)                                    BNS  130
C                                                                       BNS  140
C                                                                       BNS  150
      IO19 = IX(86)                                                     BNS  160
      REWIND IO19                                                       BNS  170
      IF(IRV .NE. 1) GOTO 100                                           BNS  180
      WRITE (IO19) ((BND(I,K),I=1,6),K=1,KVX),(BBND(K),K=1,KVX)         BNS  190
      END FILE IO19                                                     BNS  200
      GOTO 121                                                          BNS  210
  100 IF(INNO(3) .EQ. 3) GOTO 101                                       BNS  220
      READ (IO19) ((BND(I,K),I=1,6),K=1,KVX),(BBND(K),K=1,KVX)          BNS  230
      GOTO 121                                                          BNS  240
  101 DO 103 K=1,KVX                                                    BNS  250
        DO 102 I=1,6                                                    BNS  260
          BND(I,K) = 0.0                                                BNS  270
  102   CONTINUE                                                        BNS  280
  103 CONTINUE                                                          BNS  290
      IF(XMIS(1) .GE. 0.0 .AND. XMIS(2) .GE. 0.0) GOTO 107              BNS  300
  104 READ (IO19,END=107) I,(BBND(K),K=1,KVX)                           BNS  310
      IF(I .GT. 6) GOTO 106                                             BNS  320
      IF(I .LT. 1) GOTO 120                                             BNS  330
      DO 105 K=1,KVX                                                    BNS  340
        BND(I,K) = BBND(K)                                              BNS  350
  105 CONTINUE                                                          BNS  360
      GOTO 104                                                          BNS  370
  106 IF(I .NE. 7) GOTO 120                                             BNS  380
  107 CONTINUE                                                          BNS  390
      IF(XMIS(1)) 113,108,109                                           BNS  400
  108 T1 = 0.4692                                                       BNS  410
      GOTO 110                                                          BNS  420
  109 T1 = XMIS(1)                                                      BNS  430
  110 DO 112 I=11,16                                                    BNS  440
        II = I - 10                                                     BNS  450
        IF(NUAC(I) .NE. 0) GOTO 112                                     BNS  460
        DO 111 K=1,KVX                                                  BNS  470
          BND(II,K) = T1                                                BNS  480
  111   CONTINUE                                                        BNS  490
  112 CONTINUE                                                          BNS  500
  113 CONTINUE                                                          BNS  510
      IF(NUAC(17) .GT. 0) GOTO 115                                      BNS  520
      DO 114 K=1,KVX                                                    BNS  530
        BBND(K) = 0.0                                                   BNS  540
  114 CONTINUE                                                          BNS  550
      GOTO 121                                                          BNS  560
  115 CONTINUE                                                          BNS  570
      IF(XMIS(2)) 121,116,117                                           BNS  580
  116 T1 = 0.4692                                                       BNS  590
      GOTO 118                                                          BNS  600
  117 T1 = XMIS(2)                                                      BNS  610
  118 DO 119 K=1,KMAX                                                   BNS  620
        BBND(K) = T1                                                    BNS  630
  119 CONTINUE                                                          BNS  640
      GOTO 121                                                          BNS  650
  120 CONTINUE                                                          BNS  660
      NER(33) = 33                                                      BNS  670
  121 CONTINUE                                                          BNS  680
      REWIND IO19                                                       BNS  690
      RETURN                                                            BNS  700
      END                                                               BNS  710
      SUBROUTINE SIZE(IVX,JVX,KBVX,KVX,MVX,NVX,NSETVX,NRVX)             SIZ   10
C                                                                       SIZ   20
CSIZE --058 ***CITATION*** CHECKS PROB. LIMITS AGAINST MAX/ CF-IPTM     SIZ   30
C                                                                       SIZ   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,SIZ   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   SIZ   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), SIZ   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    SIZ   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    SIZ   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   SIZ  100
     6 IXPUT(9999),XPUT(9999)                                           SIZ  110
C                                                                       SIZ  120
C                                                                       SIZ  130
      IF(IVX .GT. 210) GOTO 101                                         SIZ  140
      IF(JVX .GT. 210) GOTO 101                                         SIZ  150
      IF(KBVX .GT. 210) GOTO 101                                        SIZ  160
C                                                                       SIZ  170
C     REMOVED LIMIT ON GROUPS                                           SIZ  180
C                                                                       SIZ  190
      IF(MVX .GT. 9999) GOTO 101                                        SIZ  200
      IF(NVX .GT. 200) GOTO 101                                         SIZ  210
      IF(NSETVX .GT. 50) GOTO 101                                       SIZ  220
      IF(NRVX .LE. 200) GOTO 102                                        SIZ  230
  101 NER(15) = 15                                                      SIZ  240
      WRITE (IOUT,1000) IVX,JVX,KBVX,KVX,MVX,NVX,NSETVX,NRVX            SIZ  250
  102 CONTINUE                                                          SIZ  260
      RETURN                                                            SIZ  270
C                                                                       SIZ  280
 1000 FORMAT (1H0,10I6)                                                 SIZ  290
      END                                                               SIZ  300
      SUBROUTINE SHOX(HOX,NFO,NNFO,NIC,NVX,NSETVX)                      SHO   10
C                                                                       SHO   20
CSHOX --60.1 ***CITATION*** READ 036 DATA FROM IO10/ CF-IPTM            SHO   30
C                                                                       SHO   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,SHO   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   SHO   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), SHO   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    SHO   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    SHO   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   SHO  100
     6 IXPUT(9999),XPUT(9999)                                           SHO  110
C                                                                       SHO  120
      DIMENSION HOX(NVX,NSETVX,20),NFO(NSETVX,20),NNFO(NSETVX,20),      SHO  130
     1 NIC(1000)                                                        SHO  140
C                                                                       SHO  150
C                                                                       SHO  160
      IO10 = IX(77)                                                     SHO  170
      REWIND IO10                                                       SHO  180
  100 CONTINUE                                                          SHO  190
      READ (IO10,END=103) INSEC,NREC,NSET,I1,I2                         SHO  200
      IF(INSEC .EQ. 1000) GOTO 103                                      SHO  210
      IF(INSEC .EQ. 36) GOTO 102                                        SHO  220
      IF(NREC .LE. 0) GOTO 100                                          SHO  230
      DO 101 N=1,NREC                                                   SHO  240
        READ (IO10)                                                     SHO  250
  101 CONTINUE                                                          SHO  260
      GOTO 100                                                          SHO  270
  102 CONTINUE                                                          SHO  280
      READ (IO10) HOX                                                   SHO  290
      READ (IO10) NFO                                                   SHO  300
      READ (IO10) NNFO                                                  SHO  310
      READ (IO10) NIC                                                   SHO  320
  103 CONTINUE                                                          SHO  330
      REWIND IO10                                                       SHO  340
      RETURN                                                            SHO  350
      END                                                               SHO  360
      SUBROUTINE XSET(SIG,F1,KVX,MVX,IDTI)                              XSE   10
C                                                                       XSE   20
CXSET --078 ***CITATION*** PRINT ZONE MACRO. SIGS/ CF-BIGS              XSE   30
C                                                                       XSE   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,XSE   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   XSE   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), XSE   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    XSE   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    XSE   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   XSE  100
     6 IXPUT(9999),XPUT(9999)                                           XSE  110
C                                                                       XSE  120
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           XSE  130
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    XSE  140
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),XSE  150
     3 NCLASS(9999),NDP(9999)                                           XSE  160
C                                                                       XSE  170
      DIMENSION SIG(KVX,MVX,10),F1(KVX,KVX,MVX),GIS(7)                  XSE  180
C                                                                       XSE  190
C                                                                       XSE  200
      CALL RQED(IX(104),IND)                                            XSE  210
C                                                                       XSE  220
      IF(IND .NE. 0) GOTO 114                                           XSE  230
      WRITE (IOUT,1000)                                                 XSE  240
      WRITE (IOUT,1001)                                                 XSE  250
C                                                                       XSE  260
C********SEARCH OPTIONS                                                 XSE  270
C                                                                       XSE  280
      IF(IX(5)+1) 102,103,102                                           XSE  290
  102 WRITE (IOUT,1002)                                                 XSE  300
      GOTO 104                                                          XSE  310
  103 WRITE (IOUT,1003)                                                 XSE  320
  104 CONTINUE                                                          XSE  330
      DO 113 M=1,MMAX                                                   XSE  340
        IF(M-1) 105,106,105                                             XSE  350
  105   WRITE (IOUT,1004)                                               XSE  360
  106   DO 112 K=1,KMAX                                                 XSE  370
          DO 107 I=1,4                                                  XSE  380
            GIS(I) = SIG(K,M,I)                                         XSE  390
  107     CONTINUE                                                      XSE  400
          GIS(5) = SIG(K,M,6)                                           XSE  410
          GIS(6) = SIG(K,M,7)                                           XSE  420
          N67 = 6                                                       XSE  430
C                                                                       XSE  440
C********SEARCH OPTIONS                                                 XSE  450
C                                                                       XSE  460
          IF(IX(5)+1) 109,108,109                                       XSE  470
  108     N67 = 7                                                       XSE  480
          GIS(6) = SIG(K,M,5)                                           XSE  490
          GIS(7) = SIG(K,M,7)                                           XSE  500
  109     CONTINUE                                                      XSE  510
          IF(K-1) 111,110,111                                           XSE  520
  110     WRITE (IOUT,1005) M,K,(GIS(I),I=1,N67)                        XSE  530
          GOTO 112                                                      XSE  540
  111     WRITE (IOUT,1006) K,(GIS(I),I=1,N67)                          XSE  550
  112   CONTINUE                                                        XSE  560
  113 CONTINUE                                                          XSE  570
  114 IF(IDTI .GT. 0) GOTO 115                                          XSE  580
C                                                                       XSE  590
      CALL RQED(IX(103),IND)                                            XSE  600
C                                                                       XSE  610
      IF(IND .NE. 0) GOTO 133                                           XSE  620
  115 WRITE (IOUT,1007)                                                 XSE  630
      LIM = KMAX / 7                                                    XSE  640
      IF(LIM) 116,116,117                                               XSE  650
  116 LLM1 = 1                                                          XSE  660
      LLM2 = KMAX                                                       XSE  670
      GOTO 120                                                          XSE  680
  117 L1 = LIM * 7                                                      XSE  690
      IF(L1-KMAX) 119,118,118                                           XSE  700
  118 LLM1 = LIM                                                        XSE  710
      LLM2 = 7                                                          XSE  720
      GOTO 120                                                          XSE  730
  119 LLM1 = LIM + 1                                                    XSE  740
      LLM2 = 7                                                          XSE  750
  120 N2 = 0                                                            XSE  760
      DO 132 I=1,LLM1                                                   XSE  770
        N1 = N2 + 1                                                     XSE  780
        N2 = N2 + LLM2                                                  XSE  790
        IF(N2-KMAX) 122,121,121                                         XSE  800
  121   N2 = KMAX                                                       XSE  810
  122   WRITE (IOUT,1008) (J,J=N1,N2)                                   XSE  820
        DO 131 M=1,MMAX                                                 XSE  830
          IF(M-1) 123,124,123                                           XSE  840
  123     WRITE (IOUT,1004)                                             XSE  850
  124     IF(N2-KMAX) 126,125,125                                       XSE  860
  125     N2 = KMAX                                                     XSE  870
  126     DO 130 K=1,KMAX                                               XSE  880
            DO 127 J=N1,N2                                              XSE  890
              XTR1(J) = F1(J,K,M)                                       XSE  900
  127       CONTINUE                                                    XSE  910
            IF(K-1) 129,128,129                                         XSE  920
  128       WRITE (IOUT,1009) M,K,(XTR1(J),J=N1,N2)                     XSE  930
            GOTO 130                                                    XSE  940
  129       WRITE (IOUT,1010) K,(XTR1(J),J=N1,N2)                       XSE  950
  130     CONTINUE                                                      XSE  960
  131   CONTINUE                                                        XSE  970
  132 CONTINUE                                                          XSE  980
  133 CONTINUE                                                          XSE  990
      RETURN                                                            XSE 1000
C                                                                       XSE 1010
 1000 FORMAT (1H0)                                                      XSE 1020
 1001 FORMAT (1H0,' ZONE MACROSCOPIC CROSS SECTIONS')                   XSE 1030
 1002 FORMAT (1H0,' ZONE     NAME      GRP       D          SIGR',9H    XSE 1040
     1     ,'SIGA        NUSIGF         BSQ       POWER/FLUX')          XSE 1050
 1003 FORMAT (1H0,' ZONE     NAME      GRP       D          SIGR',9H    XSE 1060
     1     ,'SIGA        NUSIGF         BSQ',7H       ,'1/V POISON   POWXSE 1070
     2ER/FLUX')                                                         XSE 1080
 1004 FORMAT (1H )                                                      XSE 1090
 1005 FORMAT (1H ,I4,'  ',12X,I4,1PE14.5,6E13.5)                        XSE 1100
 1006 FORMAT (1H ,I22,1PE14.5,6E13.5)                                   XSE 1110
 1007 FORMAT (1H0,'SCATTERING MATRIX')                                  XSE 1120
 1008 FORMAT (1H0,' ZONE GRP  TO GRP',I7,6I14)                          XSE 1130
 1009 FORMAT (1H ,2I4,1PE21.5,6E14.5)                                   XSE 1140
 1010 FORMAT (1H ,I8,1PE21.5,6E14.5)                                    XSE 1150
      END                                                               XSE 1160
      SUBROUTINE CYCR                                                   CYC   10
C                                                                       CYC   20
CCYCR --080 ***CITATION*** CYCLE CONTROL/ CF-CALR                       CYC   30
C                                                                       CYC   40
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,CYC   50
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   CYC   60
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), CYC   70
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    CYC   80
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    CYC   90
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   CYC  100
     6 IXPUT(9999),XPUT(9999)                                           CYC  110
C                                                                       CYC  120
C                                                                       CYC  130
      IX(4) = 0                                                         CYC  140
      SPARE(1) = 0.0                                                    CYC  150
      SPARE(2) = 0.0                                                    CYC  160
      RETURN                                                            CYC  170
      END                                                               CYC  180
      SUBROUTINE GINS(B1,B3,B4,B5,KVX,LVX,XLAMDA,MVX)                   GIN   10
C                                                                       GIN   20
CGINS --081 ***CITATION*** CALCULATION OF DRIVE FACTOR FOR SEARCH       GIN   30
C                                                                       GIN   40
      REAL*8 SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2S,XLL1,D8GIN   50
     1 ,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK,B5LK,D1, GIN   60
     2 D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,YRDB,SPR50GIN   70
     3 ,XLAST,B1,XLAMDA,XAAMDA,TXX1,TXX2,TXX3                           GIN   80
C                                                                       GIN   90
      COMMON /ADUBP/ SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2SGIN  100
     1 ,XLL1,D8,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK, GIN  110
     2 B5LK,D1,D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,  GIN  120
     3 YRDB,SPR50,XLAST                                                 GIN  130
C                                                                       GIN  140
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,GIN  150
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   GIN  160
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), GIN  170
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    GIN  180
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    GIN  190
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   GIN  200
     6 IXPUT(9999),XPUT(9999)                                           GIN  210
C                                                                       GIN  220
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   GIN  230
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XELKGIN  240
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    GIN  250
     3 VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2,BETTA,SAMXI,IX25,IX28,I,J,KB,K,  GIN  260
     4 ITMAX,ITIME,BET(211),DEL(211)                                    GIN  270
C                                                                       GIN  280
      COMMON /ABURN/ BBURN(30),NSIG1(50),NSIG2(50),NSIG3(50),           GIN  290
     1 N1N2R(2,9999),NSIG4(50),NSIG5(50),NSIG6(50),NJM(50),NJMM(50),    GIN  300
     2 NJNQ(50),NCH(50),NZON(9999),NXSET(9999),NXODR(9999),IDXSET(9999),GIN  310
     3 NCLASS(9999),NDP(9999)                                           GIN  320
C                                                                       GIN  330
CFZJ055                                                       25.09.07  GIN  340
C                                                                       GIN  350
      DIMENSION B1(MVX,KVX),B3(MVX,KVX),B4(MVX,KVX),B5(MVX,KVX)         GIN  360
C                                                                       GIN  370
C                                                                       GIN  380
      IF(IX(5) .EQ. (-5)) GOTO 130                                      GIN  390
      CS1S = 1.0                                                        GIN  400
      CS2S = 1.0                                                        GIN  410
      IF(NSRH(20)) 108,108,100                                          GIN  420
  100 IF(XS1DB) 108,108,101                                             GIN  430
  101 D1 = 0                                                            GIN  440
      D2 = 0                                                            GIN  450
      D3 = 0                                                            GIN  460
      DO 103 K=1,KMAX                                                   GIN  470
        DO 102 M=1,MMAX                                                 GIN  480
          D1 = D1 + B4(M,K) * B1(M,1)                                   GIN  490
          D2 = D2 + B5(M,K) * B1(M,1)                                   GIN  500
          D3 = D3 + B3(M,K) * B1(M,1)                                   GIN  510
  102   CONTINUE                                                        GIN  520
  103 CONTINUE                                                          GIN  530
      IF(XS2DB) 105,104,105                                             GIN  540
  104 CS1S = 1.0                                                        GIN  550
      GOTO 106                                                          GIN  560
  105 CS1S = XPDB * D2 / D3 / XS2DB                                     GIN  570
  106 CS2S = XPDB * D1 / D3 / XS1DB / CS1S                              GIN  580
      IF(NSRH(20)-1) 108,108,107                                        GIN  590
  107 WRITE (IOUT,1000) CS1S,CS2S                                       GIN  600
  108 CONTINUE                                                          GIN  610
      IF(NIT .GT. 0) GOTO 110                                           GIN  620
      XKEF1 = XPDB / (XADX+XLEK+XLAMDA*XS1DB)                           GIN  630
      VRGK1 = ABS(XKEF2/XKEF1-1.0)                                      GIN  640
      HOEY = 1.0 - SPARE(51) / SPARE(57) * 0.8                          GIN  650
      HOEX = HOEY                                                       GIN  660
      IF(HOEY .GE. 1.0) HOEY = 0.9                                      GIN  670
      GOTO 129                                                          GIN  680
  110 CONTINUE                                                          GIN  690
      IF(XLAMDA .NE. 0.0) GOTO 900                                      GIN  700
      HOEX = 1.0 - SPARE(51) / SPARE(57) * 0.8                          GIN  710
      IF(HOEX .GE. 1.0) HOEX = 0.9                                      GIN  720
  900 CONTINUE                                                          GIN  730
      TL = XADX + XLEK + XLAMDA * XS1DB                                 GIN  740
      XKEF1 = (XPDB+XLAMDA*XS2DB) / TL                                  GIN  750
      XAAMDA =(SPR50*XPDB-XADX-XLEK) / (CS1S*(XS1DB*CS2S-SPR50*XS2DB))  GIN  760
      HOEZ = (1.0-HOEX) * SPARE(57)                                     GIN  770
      TXX1 = SPARE(51)                                                  GIN  780
      TXX2 = HOEZ                                                       GIN  790
      IF(SPARE(51) .GT. 0.0) GOTO 111                                   GIN  800
      XLAMDA = DMAX1(XAAMDA,TXX1,TXX2)                                  GIN  810
      GOTO 112                                                          GIN  820
  111 XLAMDA = DMIN1(XAAMDA,TXX1,TXX2)                                  GIN  830
  112 CONTINUE                                                          GIN  840
      IF(DABS(XAAMDA) .GT. DABS(TXX2)) GOTO 113                         GIN  850
      IF(DABS(XAAMDA) .LE. DABS(TXX1)) GOTO 115                         GIN  860
  113 IF(NIT .LT. 12) GOTO 115                                          GIN  870
      SPARE(51) = 1.05 * SPARE(51)                                      GIN  880
      TXX1 = SPARE(51)                                                  GIN  890
      IF(DABS(TXX1) .LE. DABS(TXX2)) GOTO 115                           GIN  900
      IF(HOEX .GT. 0.9) GOTO 114                                        GIN  910
      HOEX = 0.9 * HOEX                                                 GIN  920
      GOTO 115                                                          GIN  930
  114 HOEX = HOEX * HOEX                                                GIN  940
  115 VRGK1 = ABS(XKEF1/SPARE(50)-1.0)                                  GIN  950
      SPARE(44) = SPARE(45)                                             GIN  960
      SPARE(45) = XKEF1 - SPARE(50)                                     GIN  970
      IF(SPARE(46)-1.0) 116,120,119                                     GIN  980
  116 XLAMDA = SPARE(46) * XLAMDA + (1.0-SPARE(46)) * SPARE(43)         GIN  990
      IF(NIT .GT. 5) GOTO 117                                           GIN 1000
      SPR5X = SPR50                                                     GIN 1010
      IF(ABS(XKEF1*SPR5X-1.0)-0.05) 117,117,118                         GIN 1020
  117 SPARE(46) = 2.0 * SPARE(46)                                       GIN 1030
  118 IF(SPARE(46)-1.0) 129,129,119                                     GIN 1040
  119 SPARE(46) = 1.0                                                   GIN 1050
      GOTO 129                                                          GIN 1060
  120 IF(VRGK1-EPI2) 129,129,121                                        GIN 1070
  121 IF(SPARE(44)) 122,129,122                                         GIN 1080
  122 IF(ABS(SPARE(45)/SPARE(44))-1.0) 125,123,123                      GIN 1090
  123 IF(VRGK1-0.0010) 125,125,124                                      GIN 1100
  124 TXX3 = SPARE(43)                                                  GIN 1110
      IF(DABS(XLAMDA) .GT. DABS(TXX3)) XLAMDA = 0.5 * (XLAMDA+TXX3)     GIN 1120
  125 IF(XKEF1/SPARE(50)-0.5) 126,127,127                               GIN 1130
  126 SPARE(46) = SPARE(46) * 0.5                                       GIN 1140
  127 IF(DABS(XLAMDA)-1.0E+38) 129,129,128                              GIN 1150
  128 IX(6) = 1                                                         GIN 1160
  129 CONTINUE                                                          GIN 1170
      GOTO 134                                                          GIN 1180
  130 CONTINUE                                                          GIN 1190
      IF(NIT .LE. 0) IX(133) = 0                                        GIN 1200
      SP46 = SPARE(46)                                                  GIN 1210
      HS = (XLEK+XADX-XPDB) / SPARE(88)                                 GIN 1220
      IF(NIT .LE. 0) GOTO 132                                           GIN 1230
      IF(HS .GE. 0.0) GOTO 131                                          GIN 1240
      IF(IX(133) .GT. 0) GOTO 131                                       GIN 1250
      SP46 = 0.1                                                        GIN 1260
      IX(133) = 1                                                       GIN 1270
  131 CONTINUE                                                          GIN 1280
      IF(SP46 .EQ. 1.0) GOTO 132                                        GIN 1290
      XLAMDA = SP46 * HS + (1.0-SP46) * SPARE(43)                       GIN 1300
      SP46 = 2.0 * SP46                                                 GIN 1310
      IF(SP46 .GT. 1.0) SP46 = 1.0                                      GIN 1320
      GOTO 133                                                          GIN 1330
  132 CONTINUE                                                          GIN 1340
      XLAMDA = HS                                                       GIN 1350
  133 CONTINUE                                                          GIN 1360
      SPARE(89) = SPARE(43) / XLAMDA                                    GIN 1370
      VRGK1 = ABS(SPARE(89)-1.0)                                        GIN 1380
      SPARE(46) = SP46                                                  GIN 1390
  134 CONTINUE                                                          GIN 1400
      RETURN                                                            GIN 1410
C                                                                       GIN 1420
 1000 FORMAT (1H ,'      SEARCH WEIGHTING FACTORS',2F12.5)              GIN 1430
      END                                                               GIN 1440
      SUBROUTINE ITED(XLAMDA,XMU3,BETTX,XT1,XT2,XT3)                    ITE   10
C                                                                       ITE   20
CITED --085 ***CITATION*** FLUX CALC. ITERATION EDIT /CF-FLUX,KLUX      ITE   30
C                                                                       ITE   40
      REAL*8 XLAMDA,XMU3                                                ITE   50
C                                                                       ITE   60
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,ITE   70
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   ITE   80
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), ITE   90
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    ITE  100
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    ITE  110
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   ITE  120
     6 IXPUT(9999),XPUT(9999)                                           ITE  130
C                                                                       ITE  140
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   ITE  150
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XELKITE  160
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    ITE  170
     3 VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2,BETTA,SAMXI,IX25,IX28,I,J,KB,K,  ITE  180
     4 ITMAX,ITIME,BAT(211),DAL(211)                                    ITE  190
C                                                                       ITE  200
C********SEARCH OPTIONS                                                 ITE  210
C                                                                       ITE  220
      IF(IX(5) .EQ. 0 .OR. IX(5) .GE. 2) GOTO 101                       ITE  230
      IF(IX(5) .EQ. -5) GOTO 103                                        ITE  240
      IF(IX(32) .EQ. 0) GOTO 100                                        ITE  250
      V = EXFC1 * (1.0-1.0/XT3)                                         ITE  260
      WRITE (IOUT,1001) NIT,VRG1,BETTX,XT1,XT2,XMU3,V,EXFC1,XKEF1,XLAMDAITE  270
      GOTO 105                                                          ITE  280
  100 CONTINUE                                                          ITE  290
      WRITE (IOUT,1000) NIT,VRG1,BETTX,XT1,XT2,XMU3,XKEF1,XLAMDA        ITE  300
      GOTO 105                                                          ITE  310
  101 IF(IX(32) .EQ. 0) GOTO 102                                        ITE  320
      V = EXFC1 * (1.0-1.0/XT3)                                         ITE  330
      WRITE (IOUT,1001) NIT,VRG1,BETTX,XT1,XT2,XMU3,V,EXFC1,XKEF1       ITE  340
      GOTO 105                                                          ITE  350
  102 WRITE (IOUT,1000) NIT,VRG1,BETTX,XT1,XT2,XMU3,XKEF1               ITE  360
      GOTO 105                                                          ITE  370
  103 IF(IX(32) .EQ. 0) GOTO 104                                        ITE  380
      V = EXFC1 * (1.0-1.0/XT3)                                         ITE  390
      WRITE (IOUT,1002) NIT,VRG1,BETTX,XT1,XT2,XMU3,V,EXFC1,XLAMDA,     ITE  400
     1 SPARE(89)                                                        ITE  410
      GOTO 105                                                          ITE  420
  104 WRITE (IOUT,1003) NIT,VRG1,BETTX,XT1,XT2,XMU3,XLAMDA,SPARE(89)    ITE  430
  105 CONTINUE                                                          ITE  440
      RETURN                                                            ITE  450
C                                                                       ITE  460
 1000 FORMAT (1H ,I5,1PE17.5,4(0PF10.5),F14.6,1PE14.5)                  ITE  470
 1001 FORMAT (1H ,I5,1PE17.5,4(0PF10.5)/1H ,5X,1PE17.5,3X,'EXTRAPOLATIONITE  480
     1 WITH',0PF13.4,F20.6,1PE14.5)                                     ITE  490
 1002 FORMAT (1H ,I5,1PE17.5,4(0PF10.5)/1H ,5X,1PE17.5,3X,'EXTRAPOLATIONITE  500
     1 WITH',0PF13.4,6X,1PE14.5,0PF14.6)                                ITE  510
 1003 FORMAT (1H ,I5,1PE17.5,4(0PF10.5),1PE14.5,0PF14.6)                ITE  520
      END                                                               ITE  530
      SUBROUTINE INFX(P2,NRGN,NCOMP,BBND,IVX,JVX,KVX,LVX)               INF   10
C                                                                       INF   20
CINFX --093 ***CITATION*** INITIAL FLUX FOR 1,2-D/ CF-EIGN              INF   30
C                                                                       INF   40
      REAL*8 P2                                                         INF   50
C                                                                       INF   60
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,INF   70
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   INF   80
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), INF   90
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    INF  100
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    INF  110
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   INF  120
     6 IXPUT(9999),XPUT(9999)                                           INF  130
C                                                                       INF  140
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   INF  150
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKINF  160
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    INF  170
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  INF  180
     4 ITMAX,ITIME,BET(211),DEL(211)                                    INF  190
C                                                                       INF  200
CFZJ055                                                       25.09.07  INF  210
C                                                                       INF  220
      DIMENSION P2(JVX,IVX,KVX),NRGN(JVX,IVX),NCOMP(LVX),BBND(KVX)      INF  230
C                                                                       INF  240
C                                                                       INF  250
      IF(IX(24) .EQ. 0) REWIND IOFLX                                    INF  260
      IF(IX(24) .GT. 0) GOTO 101                                        INF  270
      IF(IX(39) .NE. 0) GOTO 101                                        INF  280
      IF(IX(22) .NE. 0) GOTO 101                                        INF  290
      IF(NGC(2) .NE. 0 .AND. NUAC(2) .LT. 2) GOTO 106                   INF  300
      IF(NGC(2) .NE. 0) GOTO 101                                        INF  310
      IF(IX(16) .GT. 0) GOTO 106                                        INF  320
      IF(IX(3) .GT. 1) GOTO 106                                         INF  330
  101 CONTINUE                                                          INF  340
      IF(NGC(6) .LE. 1) GOTO 134                                        INF  350
      WRITE (6,1001)                                                    INF  360
      REWIND IOFLX                                                      INF  370
      DO 132 K=1,KMAX                                                   INF  380
        READ (IOFLX) ((P2(J,I,K),J=1,JMAX),I=1,IMAX)                    INF  390
  132 CONTINUE                                                          INF  400
      READ (IOFLX) XLAMDA                                               INF  410
      WRITE (6,1002) XLAMDA                                             INF  420
      GOTO 106                                                          INF  430
  134 CONTINUE                                                          INF  440
      XLPL = 1.0E-30                                                    INF  450
      IF(IX(24) .GT. 0 .AND. PROD .LE. 10.0) XLPL = 1.0                 INF  460
      DO 105 I=1,IMAX                                                   INF  470
        DO 104 J=1,JMAX                                                 INF  480
          DO 103 K=1,KMAX                                               INF  490
            T1 = 1.0E+12                                                INF  500
            IF(IX(24) .GT. 0) T1 = P2(J,I,1) * XLPL                     INF  510
            P2(J,I,K) = T1                                              INF  520
  103     CONTINUE                                                      INF  530
  104   CONTINUE                                                        INF  540
  105 CONTINUE                                                          INF  550
  106 CONTINUE                                                          INF  560
      DO 109 K=1,KMAX                                                   INF  570
        DO 108 I=1,IMAX                                                 INF  580
          DO 107 J=1,JMAX                                               INF  590
            IF(P2(J,I,K) .EQ. 0.0) GOTO 107                             INF  600
            T2 = P2(J,I,K)                                              INF  610
            GOTO 110                                                    INF  620
  107     CONTINUE                                                      INF  630
  108   CONTINUE                                                        INF  640
  109 CONTINUE                                                          INF  650
      WRITE (IOUT,1000)                                                 INF  660
C                                                                       INF  670
      CALL EXIT                                                         INF  680
C                                                                       INF  690
  110 CONTINUE                                                          INF  700
      DO 113 K=1,KMAX                                                   INF  710
        DO 112 I=1,IMAX                                                 INF  720
          DO 111 J=1,JMAX                                               INF  730
            IF(P2(J,I,K) .EQ. 0.0) P2(J,I,K) = T2                       INF  740
  111     CONTINUE                                                      INF  750
  112   CONTINUE                                                        INF  760
  113 CONTINUE                                                          INF  770
      DO 120 I=1,IMAX                                                   INF  780
        DO 119 J=1,JMAX                                                 INF  790
          L = NRGN(J,I)                                                 INF  800
          M = NCOMP(L)                                                  INF  810
          DO 118 K=1,KMAX                                               INF  820
            IF(M-NUAC(17)) 117,114,117                                  INF  830
  114       IF(XMIS(2)) 115,116,116                                     INF  840
  115       IF(BBND(K)) 116,117,116                                     INF  850
  116       P2(J,I,K) = 0.0                                             INF  860
  117       CONTINUE                                                    INF  870
  118     CONTINUE                                                      INF  880
  119   CONTINUE                                                        INF  890
  120 CONTINUE                                                          INF  900
      IF(IX(39) .NE. 0) GOTO 121                                        INF  910
      IF(IX(22) .NE. 0) GOTO 121                                        INF  920
      IF(NGC(2) .GT. 0 .AND. NUAC(2) .EQ. 0) GOTO 131                   INF  930
  121 CONTINUE                                                          INF  940
      IGOTO = 2                                                         INF  950
      IF(IX(24) .GT. 0) IGOTO = 1                                       INF  960
      BETTA = XMIS(6)                                                   INF  970
      VRGK2 = BETTA                                                     INF  980
      IF(IX(31)) 124,124,128                                            INF  990
  124 IF(IMAX-1) 125,125,126                                            INF 1000
  125 BETTA = 1.0                                                       INF 1010
      GOTO 128                                                          INF 1020
  126 BETOI = 3.141593 / FLOAT(IMXP1)                                   INF 1030
      ARG1 = COS(BETOI)                                                 INF 1040
      BETOI = 3.141593 / FLOAT(JMXP1)                                   INF 1050
      ARG2 = COS(BETOI)                                                 INF 1060
      RHO = ARG1 / (2.0-ARG2)                                           INF 1070
      T1 = KMAX                                                         INF 1080
      T2 = (T1+11.0) / 12.0                                             INF 1090
      RHO = RHO**T2                                                     INF 1100
      BETTA = 2.0 / (1.0+SQRT(1.0-RHO**2))                              INF 1110
      RHO = ARG2 / (2.0-ARG1)                                           INF 1120
      RHO = RHO**T2                                                     INF 1130
      VRGK2 = 2.0 / (1.0+SQRT(1.0-RHO**2))                              INF 1140
  128 CONTINUE                                                          INF 1150
      IF(NUAC(8) .GE. 0) GOTO 131                                       INF 1160
      BETTA = (6.0*BETTA+1.0) / 7.0                                     INF 1170
      VRGK2 = (6.0*VRGK2+1.0) / 7.0                                     INF 1180
  131 CONTINUE                                                          INF 1190
      RETURN                                                            INF 1200
C                                                                       INF 1210
 1000 FORMAT (1H0,'ERROR STOP NUMBER 11')                               INF 1220
 1001 FORMAT ('0******* INITIAL FLUX FROM TAPE/DISK IOFLX (=9) *******'/INF 1230
     1 /)                                                               INF 1240
 1002 FORMAT ('0XLAMDA',1PE15.5)                                        INF 1250
      END                                                               INF 1260
      SUBROUTINE ABPR(P2,B1,B2,B3,B4,B5,NRGN,IVX,JVX,KVX,LVX, SIG,PVOL, ABP   10
     1 NCOMP,MVX)                                                       ABP   20
C                                                                       ABP   30
CABPR --097 ***CITATION*** AVERAGE FLUX,ABS.,PROD CALCULATION/ CF-FLUX  ABP   40
C                                                                       ABP   50
      REAL*8 SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2S,XLL1,D8ABP   60
     1 ,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK,B5LK,D1, ABP   70
     2 D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,YRDB,SPR50ABP   80
     3 ,XLAST,P2,B1                                                     ABP   90
C                                                                       ABP  100
      COMMON /ADUBP/ SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2SABP  110
     1 ,XLL1,D8,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK, ABP  120
     2 B5LK,D1,D2,D3,D4,D5, D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB, ABP  130
     3 YRDB,SPR50,XLAST                                                 ABP  140
C                                                                       ABP  150
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,ABP  160
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   ABP  170
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), ABP  180
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    ABP  190
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    ABP  200
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   ABP  210
     6 IXPUT(9999),XPUT(9999)                                           ABP  220
C                                                                       ABP  230
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   ABP  240
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XELKABP  250
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    ABP  260
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SAMXI,IX25,IX28,I,J,KB,K,  ABP  270
     4 ITMAX,ITIME,BET(211),DEL(211)                                    ABP  280
C                                                                       ABP  290
      DIMENSION P2(JVX,IVX,KVX),B1(MVX,KVX),B2(MVX,KVX),B3(MVX,KVX),    ABP  300
     1 B4(MVX,KVX),B5(MVX,KVX),NRGN(JVX,IVX),SIG(KVX,MVX,10),PVOL(LVX), ABP  310
     2 NCOMP(LVX)                                                       ABP  320
C                                                                       ABP  330
C                                                                       ABP  340
      DO 101 K=1,KVX                                                    ABP  350
        DO 100 M=1,MVX                                                  ABP  360
          B1(M,K) = 0.0                                                 ABP  370
  100   CONTINUE                                                        ABP  380
  101 CONTINUE                                                          ABP  390
      DO 104 K=1,KVX                                                    ABP  400
        DO 103 I=1,IVX                                                  ABP  410
          DO 102 J=1,JVX                                                ABP  420
            L = NRGN(J,I)                                               ABP  430
            M = NCOMP(L)                                                ABP  440
            B1(M,K) = B1(M,K) + P2(J,I,K) * PVOL(L)                     ABP  450
  102     CONTINUE                                                      ABP  460
  103   CONTINUE                                                        ABP  470
  104 CONTINUE                                                          ABP  480
      DO 106 K=1,KMAX                                                   ABP  490
        DO 105 M=1,MMAX                                                 ABP  500
          B2LK = (SIG(K,M,3)+SIG(K,M,9)) * B1(M,K)                      ABP  510
          B3LK = SIG(K,M,4) * B1(M,K)                                   ABP  520
          B4LK = SIG(K,M,5) * B1(M,K)                                   ABP  530
          B5LK = SIG(K,M,8) * B1(M,K)                                   ABP  540
          XADB = XADB + B2LK                                            ABP  550
          XPDB = XPDB + B3LK                                            ABP  560
          XS1DB = XS1DB + B4LK                                          ABP  570
          XS2DB = XS2DB + B5LK                                          ABP  580
          B2(M,K) = B2LK + B2(M,K)                                      ABP  590
          B3(M,K) = B3LK                                                ABP  600
          B4(M,K) = B4LK                                                ABP  610
          B5(M,K) = B5LK                                                ABP  620
  105   CONTINUE                                                        ABP  630
  106 CONTINUE                                                          ABP  640
      RETURN                                                            ABP  650
      END                                                               ABP  660
      SUBROUTINE LOOP(P2,SOUR,NRGN,XII,IVX,JVX,KVX,LVX,XLAMDA,SIG,PVOL, LOO   10
     1 NCOMP,MVX)                                                       LOO   20
C                                                                       LOO   30
CLOOP --098 ***CITATION*** PHIBAR,SOURCE,CONVERGENCE FOR 1,2-D/ CF-FLUX LOO   40
C                                                                       LOO   50
      REAL*8 P2,CKSO,SOUR,T1,XII,XLAMDA                                 LOO   60
C                                                                       LOO   70
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,LOO   80
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   LOO   90
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), LOO  100
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    LOO  110
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    LOO  120
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   LOO  130
     6 IXPUT(9999),XPUT(9999)                                           LOO  140
C                                                                       LOO  150
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   LOO  160
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKLOO  170
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    LOO  180
     3 VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  LOO  190
     4 ITMAX,ITIME,BET(211),DEL(211)                                    LOO  200
C                                                                       LOO  210
      DIMENSION P2(JVX,IVX,KVX),SOUR(JVX,IVX ),NRGN(JVX,IVX),XII(KVX),  LOO  220
     1 SIG(KVX,MVX,10),PVOL(LVX),NCOMP(LVX)                             LOO  230
C                                                                       LOO  240
C                                                                       LOO  250
      IF(IX(24) .GT. 0) GOTO 105                                        LOO  260
      DO 104 I=1,IVX                                                    LOO  270
        DO 103 J=1,JVX                                                  LOO  280
          L = NRGN(J,I)                                                 LOO  290
          M = NCOMP(L)                                                  LOO  300
          CKSO = 0.0                                                    LOO  310
          DO 100 K=1,KVX                                                LOO  320
            T1 = P2(J,I,K)                                              LOO  330
            CKSO = CKSO + SIG(K,M,4) * T1                               LOO  340
  100     CONTINUE                                                      LOO  350
          IF(SPARE(98) .EQ. 0.0) GOTO 102                               LOO  360
          DO 101 K=1,KVX                                                LOO  370
            CKSO = CKSO + XLAMDA * SIG(K,M,8) * P2(J,I,K)               LOO  380
  101     CONTINUE                                                      LOO  390
  102     CONTINUE                                                      LOO  400
          SOUR(J,I) = CKSO * PVOL(L)                                    LOO  410
  103   CONTINUE                                                        LOO  420
  104 CONTINUE                                                          LOO  430
      GOTO 109                                                          LOO  440
  105 CONTINUE                                                          LOO  450
      DO 108 I=1,IVX                                                    LOO  460
        DO 107 J=1,JVX                                                  LOO  470
          CKSO = 0.0                                                    LOO  480
          DO 106 K=1,KVX                                                LOO  490
            T1 = P2(J,I,K)                                              LOO  500
            CKSO = CKSO + XII(K) * T1                                   LOO  510
  106     CONTINUE                                                      LOO  520
          SOUR(J,I) = CKSO                                              LOO  530
  107   CONTINUE                                                        LOO  540
  108 CONTINUE                                                          LOO  550
  109 CONTINUE                                                          LOO  560
      RETURN                                                            LOO  570
      END                                                               LOO  580
      SUBROUTINE DPER(SCAT,P2,DCONB,DCONR,PTSA,NRGN,E1,BET,DEL,IVX,JVX, DPE   10
     1 KVX,IVXP1,JVXP1,IVZ,KVZ,LVX,IOVX,IOVZ)                           DPE   20
C                                                                       DPE   30
CDPER --107 ***CITATION*** LINE RELAXATION ON ROWS AND/OR COLUMNS FOR   DPE   40
C                          PERIODIC BOUNDARY CONDITIONS (2-D)/ CF-DNSD  DPE   50
C                                                                       DPE   60
      REAL*8 ALP(211),Q(211),QQ,TT,D,E,F,P2,BET(211),DEL(211),T,TEMP,TMFDPE   70
     1 ,SCAT,RDEL                                                       DPE   80
C                                                                       DPE   90
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,DPE  100
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   DPE  110
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), DPE  120
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    DPE  130
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    DPE  140
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   DPE  150
     6 IXPUT(9999),XPUT(9999)                                           DPE  160
C                                                                       DPE  170
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   DPE  180
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKDPE  190
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    DPE  200
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  DPE  210
     4 ITMAX,ITIME,BAT(211),DAL(211)                                    DPE  220
C                                                                       DPE  230
      DIMENSION SCAT(JVX,IVX),P2(JVX,IVX,KVX),DCONB(JVX,IVXP1,IOVX),    DPE  240
     1 DCONR(JVXP1,IVZ,IOVZ),PTSA(JVX,IVX,IOVX),NRGN(JVX,IVX),          DPE  250
     2 E1(LVX,KVX)                                                      DPE  260
C                                                                       DPE  270
C                                                                       DPE  280
      JVXM1 = JVX - 1                                                   DPE  290
      N = IX(20)                                                        DPE  300
      DO 124 I=1,IVX                                                    DPE  310
        IM1 = I - 1                                                     DPE  320
        IP1 = I + 1                                                     DPE  330
        DO 102 J=1,JVX                                                  DPE  340
          QQ = SCAT(J,I)                                                DPE  350
          IF(I .LE. 1) GOTO 100                                         DPE  360
          QQ = QQ + P2(J,IM1,K) * DCONB(J,I,N)                          DPE  370
  100     IF(I .GE. IVX) GOTO 101                                       DPE  380
          QQ = QQ + P2(J,IP1,K) * DCONB(J,IP1,N)                        DPE  390
  101     Q(J) = QQ                                                     DPE  400
  102   CONTINUE                                                        DPE  410
        D4 = DCONR(2,I,N)                                               DPE  420
        IF(P2(1,I,K) .EQ. 0.0) GOTO 103                                 DPE  430
        L = NRGN(1,I)                                                   DPE  440
        BET(1) = Q(1) / D4                                              DPE  450
        DEL(1) = D4 / (PTSA(1,I,N)+E1(L,K))                             DPE  460
        ALP(1) = DCONR(1,I,N) / D4                                      DPE  470
        GOTO 104                                                        DPE  480
  103   BET(1) = 0.0                                                    DPE  490
        DEL(1) = 0.0                                                    DPE  500
        ALP(1) = 0.0                                                    DPE  510
  104   CONTINUE                                                        DPE  520
        DO 106 J=2,JVX                                                  DPE  530
          IF(P2(J,I,K) .EQ. 0.0) GOTO 105                               DPE  540
          L = NRGN(J,I)                                                 DPE  550
          T = D4 * DEL(J-1)                                             DPE  560
          D4 = DCONR(J+1,I,N)                                           DPE  570
          BET(J) = (Q(J)+BET(J-1)*T) / D4                               DPE  580
          DEL(J) = D4 / (PTSA(J,I,N)+E1(L,K)-T)                         DPE  590
          ALP(J) = ALP(J-1) * T / D4                                    DPE  600
          GOTO 106                                                      DPE  610
  105     BET(J) = 0.0                                                  DPE  620
          DEL(J) = 0.0                                                  DPE  630
          ALP(J) = 0.0                                                  DPE  640
  106   CONTINUE                                                        DPE  650
        IF(P2(JVX,I,K) .NE. 0.0) GOTO 107                               DPE  660
        TEMP = 0.0                                                      DPE  670
        TT = 0.0                                                        DPE  680
        GOTO 115                                                        DPE  690
  107   CONTINUE                                                        DPE  700
        F = 1.0                                                         DPE  710
        D = 0.0                                                         DPE  720
        E = 0.0                                                         DPE  730
        DO 108 J=1,JVXM1                                                DPE  740
          F = F * DEL(J)                                                DPE  750
          IF(DEL(J) .EQ. 0.0) GOTO 109                                  DPE  760
          D = D + BET(J) * F                                            DPE  770
          E = E + ALP(J) * F                                            DPE  780
  108   CONTINUE                                                        DPE  790
  109   CONTINUE                                                        DPE  800
        RDEL = 0.0                                                      DPE  810
        IF(DEL(JVXM1) .NE. 0.0) RDEL = 1.0D0 / DEL(JVXM1)               DPE  820
        L = NRGN(JVX,I)                                                 DPE  830
        D1 = DCONR(JVX,I,N)                                             DPE  840
        D2 = DCONR(JVX+1,I,N)                                           DPE  850
        D4 = D1 + F * D2                                                DPE  860
        TEMP = ((Q(JVX)+D*D2)*RDEL+D4*BET(JVXM1)) / ((PTSA(JVX,I,N)+    DPE  870
     1   E1(L,K)-E*D2)*RDEL-D4*(1.0+ALP(JVXM1)))                        DPE  880
        TT = TEMP                                                       DPE  890
        T = P2(JVX,I,K)                                                 DPE  900
        TMF = T + BETTA * (TEMP-T)                                      DPE  910
        IF(IEP) 110,114,111                                             DPE  920
  110   P2(JVX,I,K) = TEMP                                              DPE  930
        GOTO 115                                                        DPE  940
  111   IF(TMF-TEMP) 113,114,112                                        DPE  950
  112   TMF = DMIN1(TMF,(TEMP+T))                                       DPE  960
        GOTO 114                                                        DPE  970
  113   TMF = DMAX1(TMF,0.5*TEMP)                                       DPE  980
  114   CONTINUE                                                        DPE  990
        P2(JVX,I,K) = TMF                                               DPE 1000
  115   DO 123 JJ=2,JVX                                                 DPE 1010
          J = JVXP1 - JJ                                                DPE 1020
          T = P2(J,I,K)                                                 DPE 1030
          IF(T .NE. 0.0) GOTO 116                                       DPE 1040
          TEMP = 0.0                                                    DPE 1050
          GOTO 123                                                      DPE 1060
  116     CONTINUE                                                      DPE 1070
          TEMP = DEL(J) * (TEMP+BET(J)+TT*ALP(J))                       DPE 1080
          TMF = T + BETTA * (TEMP-T)                                    DPE 1090
          IF(IEP) 117,121,118                                           DPE 1100
  117     P2(J,I,K) = TEMP                                              DPE 1110
          GOTO 123                                                      DPE 1120
  118     IF(TMF-TEMP) 120,121,119                                      DPE 1130
  119     TMF = DMIN1(TMF,(TEMP+T))                                     DPE 1140
          GOTO 121                                                      DPE 1150
  120     TMF = DMAX1(TMF,0.5*TEMP)                                     DPE 1160
  121     CONTINUE                                                      DPE 1170
          P2(J,I,K) = TMF                                               DPE 1180
  123   CONTINUE                                                        DPE 1190
  124 CONTINUE                                                          DPE 1200
      IF(NUAC(20) .GT. 0) GOTO 148                                      DPE 1210
      DO 147 J=1,JVX                                                    DPE 1220
        JM1 = J - 1                                                     DPE 1230
        JP1 = J + 1                                                     DPE 1240
        DO 128 I=1,IVX                                                  DPE 1250
          QQ = SCAT(J,I)                                                DPE 1260
          IF(J .NE. 1) GOTO 125                                         DPE 1270
          QQ = QQ + P2(JVX,I,K) * DCONR(J,I,N) + P2(JP1,I,K) *          DPE 1280
     1     DCONR(JP1,I,N)                                               DPE 1290
          GOTO 127                                                      DPE 1300
  125     IF(J .NE. JVX) GOTO 126                                       DPE 1310
          QQ = QQ + P2(JVX-1,I,K) * DCONR(JVX,I,N) + P2(1,I,K) *        DPE 1320
     1     DCONR(JVX+1,I,N)                                             DPE 1330
          GOTO 127                                                      DPE 1340
  126     QQ = QQ + P2(JM1,I,K) * DCONR(J,I,N) + P2(JP1,I,K) *          DPE 1350
     1     DCONR(JP1,I,N)                                               DPE 1360
  127     Q(I) = QQ                                                     DPE 1370
  128   CONTINUE                                                        DPE 1380
        D4 = DCONB(J,2,N)                                               DPE 1390
        IF(P2(J,1,K) .EQ. 0.0) GOTO 129                                 DPE 1400
        L = NRGN(J,1)                                                   DPE 1410
        BET(1) = Q(1) / D4                                              DPE 1420
        DEL(1) = D4 / (PTSA(J,1,N)+E1(L,K))                             DPE 1430
        GOTO 130                                                        DPE 1440
  129   DEL(1) = 0.0                                                    DPE 1450
  130   CONTINUE                                                        DPE 1460
        DO 132 I=2,IVX                                                  DPE 1470
          IF(P2(J,I,K) .EQ. 0.0) GOTO 131                               DPE 1480
          L = NRGN(J,I)                                                 DPE 1490
          T = D4 * DEL(I-1)                                             DPE 1500
          D4 = DCONB(J,I+1,N)                                           DPE 1510
          BET(I) = (Q(I)+BET(I-1)*T) / D4                               DPE 1520
          DEL(I) = D4 / (PTSA(J,I,N)+E1(L,K)-T)                         DPE 1530
          GOTO 132                                                      DPE 1540
  131     DEL(I) = 0.0                                                  DPE 1550
  132   CONTINUE                                                        DPE 1560
        TEMP = BET(IVX) * DEL(IVX)                                      DPE 1570
        T = P2(J,IVX,K)                                                 DPE 1580
        TMF = T + VRGK2 * (TEMP-T)                                      DPE 1590
        IF(IEP) 134,138,135                                             DPE 1600
  134   P2(J,IVX,K) = TEMP                                              DPE 1610
        GOTO 139                                                        DPE 1620
  135   IF(TMF-TEMP) 137,138,136                                        DPE 1630
  136   TMF = DMIN1(TMF,(TEMP+T))                                       DPE 1640
        GOTO 138                                                        DPE 1650
  137   TMF = DMAX1(TMF,0.5*TEMP)                                       DPE 1660
  138   CONTINUE                                                        DPE 1670
        P2(J,IVX,K) = TMF                                               DPE 1680
  139   DO 146 JJ=2,IVX                                                 DPE 1690
          I = IVXP1 - JJ                                                DPE 1700
          T = P2(J,I,K)                                                 DPE 1710
          TEMP = DEL(I) * (TEMP+BET(I))                                 DPE 1720
          TMF = T + VRGK2 * (TEMP-T)                                    DPE 1730
          IF(IEP) 140,144,141                                           DPE 1740
  140     P2(J,I,K) = TEMP                                              DPE 1750
          GOTO 146                                                      DPE 1760
  141     IF(TMF-TEMP) 143,144,142                                      DPE 1770
  142     TMF = DMIN1(TMF,(TEMP+ T))                                    DPE 1780
          GOTO 144                                                      DPE 1790
  143     TMF = DMAX1(TMF,0.5*TEMP)                                     DPE 1800
  144     CONTINUE                                                      DPE 1810
          P2(J,I,K) = TMF                                               DPE 1820
  146   CONTINUE                                                        DPE 1830
  147 CONTINUE                                                          DPE 1840
  148 CONTINUE                                                          DPE 1850
      RETURN                                                            DPE 1860
      END                                                               DPE 1870
      SUBROUTINE HWRD(SCAT,P2,DCONB,DCONR,PTSA,IVX,JVX,KVX,IVXP1,JVXP1, HWR   10
     1 IVZ,KVZ,BET,DEL,NRGN,E1,LVX,IOVX,IOVZ)                           HWR   20
C                                                                       HWR   30
CHWRD --108 ***CITATION*** LINE RELAX ROWS FOR 1,2-D HEX/ CF-FLUX       HWR   40
C                                                                       HWR   50
      REAL*8 P2,BET(211),DEL(211),T,TEMP,TMF,SCAT                       HWR   60
C                                                                       HWR   70
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,HWR   80
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   HWR   90
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), HWR  100
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    HWR  110
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    HWR  120
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   HWR  130
     6 IXPUT(9999),XPUT(9999)                                           HWR  140
C                                                                       HWR  150
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   HWR  160
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKHWR  170
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    HWR  180
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  HWR  190
     4 ITMAX,ITIME,BAT(211),DAL(211)                                    HWR  200
C                                                                       HWR  210
      DIMENSION SCAT(JVX,IVX),P2(JVX,IVX,KVX),DCONB(JVX,IVXP1,IOVX),    HWR  220
     1 DCONR(JVXP1,IVZ,IOVZ),PTSA(JVX,IVX,IOVX),NRGN(JVX,IVX),          HWR  230
     2 E1(LVX,KVX)                                                      HWR  240
C                                                                       HWR  250
C                                                                       HWR  260
      N = IX(20)                                                        HWR  270
      JVY = JVX - 1                                                     HWR  280
      KKK = N + IOVX                                                    HWR  290
      DO 144 I=1,IVX                                                    HWR  300
        IP1 = I - 1                                                     HWR  310
        IP2 = I + 1                                                     HWR  320
        DEL(1) = 0.0                                                    HWR  330
        D1 = DCONB(1,I,N)                                               HWR  340
        D2 = DCONB(1,IP2,N)                                             HWR  350
        D4 = DCONR(2,I,N)                                               HWR  360
        IF(I-1) 102,102,108                                             HWR  370
  102   IF(P2(1,1,K)) 103,104,103                                       HWR  380
  103   BET(1) = (P2(1,2,K)*D2+SCAT(1,1)) / D4                          HWR  390
        DEL(1) = D4 / (PTSA(1,1,N)+E1(1,K))                             HWR  400
  104   DO 107 J=2,JVX                                                  HWR  410
          IF(P2(J,1,K)) 106,105,106                                     HWR  420
  105     DEL(J) = 0.0                                                  HWR  430
          GOTO 107                                                      HWR  440
  106     T = D4 * DEL(J-1)                                             HWR  450
          L = NRGN(J,1)                                                 HWR  460
          D4 = DCONR(J+1,1,N)                                           HWR  470
          BET(J) = (P2(J,2,K)*DCONB(J,2,N)+SCAT(J,1)+ P2(J-1,2,K)*      HWR  480
     1     DCONR(J,2,KKK)+BET(J-1)*T) / D4                              HWR  490
          DEL(J) = D4 / (PTSA(J,1,N)-T+E1(L,K))                         HWR  500
  107   CONTINUE                                                        HWR  510
        GOTO 125                                                        HWR  520
  108   IF(I-IVX) 117,109,109                                           HWR  530
  109   IF(P2(1,IVX,K)) 110,111,110                                     HWR  540
  110   BET(1) = (P2(1,IP1,K)*D1+P2(2,IP1,K)*DCONR(2,IVX,KKK)+          HWR  550
     1   SCAT(1,IVX)) / D4                                              HWR  560
        L = NRGN(1,IVX)                                                 HWR  570
        DEL(1) = D4 / (PTSA(1,IVX,N)+E1(L,K))                           HWR  580
  111   DO 114 J=2,JVY                                                  HWR  590
          IF(P2(J,IVX,K)) 113,112,113                                   HWR  600
  112     DEL(J) = 0.0                                                  HWR  610
          GOTO 114                                                      HWR  620
  113     T = D4 * DEL(J-1)                                             HWR  630
          L = NRGN(J,IVX)                                               HWR  640
          D4 = DCONR(J+1,IVX,N)                                         HWR  650
          BET(J) = (P2(J,IP1,K)*DCONB(J,IVX,N)+P2(J+1,IP1,K)*           HWR  660
     1     DCONR(J+1,IVX,KKK)+SCAT(J,IVX)+BET(J-1)*T) / D4              HWR  670
          DEL(J) = D4 / (PTSA(J,IVX,N)-T+E1(L,K))                       HWR  680
  114   CONTINUE                                                        HWR  690
        IF(P2(JVX,IVX,K)) 116,115,116                                   HWR  700
  115   DEL(JVX) = 0                                                    HWR  710
        GOTO 125                                                        HWR  720
  116   T = D4 * DEL(J-1)                                               HWR  730
        L = NRGN(JVX,IVX)                                               HWR  740
        D4 = DCONR(JVXP1,IVX,N)                                         HWR  750
        BET(JVX) = (P2(JVX,IP1,K)*DCONB(JVX,IVX,N)+SCAT(JVX,IVX)+       HWR  760
     1   BET(JVX-1)*T) / D4                                             HWR  770
        DEL(JVX) = D4 / (PTSA(JVX,IVX,N)-T+E1(L,K))                     HWR  780
        GOTO 125                                                        HWR  790
  117   IF(P2(1,I,K)) 118,119,118                                       HWR  800
  118   CONTINUE                                                        HWR  810
        L = NRGN(1,I)                                                   HWR  820
        BET(1) = (P2(1,IP1,K)*D1+P2(1,IP2,K)*D2+P2(2,IP1,K)*            HWR  830
     1   DCONR(2,I,KKK)+SCAT(1,I)) / D4                                 HWR  840
        DEL(1) = D4 / (PTSA(1,I,N)+E1(L,K))                             HWR  850
  119   CONTINUE                                                        HWR  860
        DO 122 J=2,JVY                                                  HWR  870
          IF(P2(J,I,K)) 121,120,121                                     HWR  880
  120     DEL(J) = 0.0                                                  HWR  890
          GOTO 122                                                      HWR  900
  121     T = D4 * DEL(J-1)                                             HWR  910
          L = NRGN(J,I)                                                 HWR  920
          D4 = DCONR(J+1,I,N)                                           HWR  930
          BET(J) = (P2(J,IP1,K)*DCONB(J,I,N)+P2(J,IP2,K)*DCONB(J,IP2,N)+HWR  940
     1     P2(J+1,IP1,K)*DCONR(J+1,I,KKK)+P2(J-1,IP2,K)*DCONR(J,IP2,KKK)HWR  950
     2     +SCAT(J,I)+BET(J-1)*T) / D4                                  HWR  960
          DEL(J) = D4 / (PTSA(J,I,N)-T+E1(L,K))                         HWR  970
  122   CONTINUE                                                        HWR  980
        IF(P2(JVX,I,K)) 124,123,124                                     HWR  990
  123   DEL(JVX) = 0.                                                   HWR 1000
        GOTO 125                                                        HWR 1010
  124   T = D4 * DEL(J-1)                                               HWR 1020
        L = NRGN(JVX,I)                                                 HWR 1030
        D4 = DCONR(JVX+1,I,N)                                           HWR 1040
        BET(JVX) = (P2(JVX,IP1,K)*DCONB(JVX,I,N)+P2(JVX,IP2,K)*         HWR 1050
     1   DCONB(JVX,IP2,N )+P2(JVX-1,IP2,K)*DCONR(JVX,IP2,KKK)+          HWR 1060
     2   SCAT(JVX,I)+BET(JVX-1)*T) / D4                                 HWR 1070
        DEL(JVX) = D4 / (PTSA(JVX,I,N)-T+E1(L,K))                       HWR 1080
  125   CONTINUE                                                        HWR 1090
        TEMP = BET(JVX) * DEL(JVX)                                      HWR 1100
        T = P2(JVX,I,K)                                                 HWR 1110
        TMF = T + BETTA * (TEMP-T)                                      HWR 1120
        IF(IEP) 127,131,128                                             HWR 1130
  127   P2(JVX,I,K) = TEMP                                              HWR 1140
        GOTO 132                                                        HWR 1150
  128   IF(TMF-TEMP) 130,131,129                                        HWR 1160
  129   TMF = DMIN1(TMF,(TEMP+T))                                       HWR 1170
        GOTO 131                                                        HWR 1180
  130   TMF = DMAX1(TMF,0.5*TEMP)                                       HWR 1190
  131   CONTINUE                                                        HWR 1200
        P2(JVX,I,K) = TMF                                               HWR 1210
  132   DO 139 JJ=2,JVX                                                 HWR 1220
          J = JVXP1 - JJ                                                HWR 1230
          T = P2(J,I,K)                                                 HWR 1240
          TEMP = DEL(J) * (TEMP+BET(J))                                 HWR 1250
          TMF = T + BETTA * (TEMP-T)                                    HWR 1260
          IF(IEP) 133,137,134                                           HWR 1270
  133     P2(J,I,K) = TEMP                                              HWR 1280
          GOTO 139                                                      HWR 1290
  134     IF(TMF-TEMP) 136,137,135                                      HWR 1300
  135     TMF = DMIN1(TMF,(TEMP+T))                                     HWR 1310
          GOTO 137                                                      HWR 1320
  136     TMF = DMAX1(TMF,0.5*TEMP)                                     HWR 1330
  137     CONTINUE                                                      HWR 1340
          P2(J,I,K) = TMF                                               HWR 1350
  139   CONTINUE                                                        HWR 1360
        IF(NUAC(8)) 140,144,142                                         HWR 1370
  140   L = IVXP1 - I                                                   HWR 1380
        DO 141 J=1,JVX                                                  HWR 1390
          M = JVXP1 - J                                                 HWR 1400
          P2(M,L,K) = P2(J,I,K)                                         HWR 1410
  141   CONTINUE                                                        HWR 1420
        GOTO 144                                                        HWR 1430
  142   DO 143 J=1,JVX                                                  HWR 1440
          P2(I,J,K) = P2(J,I,K)                                         HWR 1450
  143   CONTINUE                                                        HWR 1460
  144 CONTINUE                                                          HWR 1470
      RETURN                                                            HWR 1480
      END                                                               HWR 1490
      SUBROUTINE HXRD(SCAT,P2,DCONB,DCONR,PTSA,IVX,JVX,KVX,IVXP1,JVXP1, HXR   10
     1 IVZ,KVZ,BET,DEL,NRGN,E1,LVX,IOVX,IOVZ)                           HXR   20
C                                                                       HXR   30
CHXRD --109 ***CITATION*** LINE RELAX COLS FOR 1,2-D HEX/ CF-FLUX       HXR   40
C                                                                       HXR   50
      REAL*8 P2,BET(211),DEL(211),T,TEMP,TMF,SCAT                       HXR   60
C                                                                       HXR   70
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,HXR   80
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   HXR   90
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), HXR  100
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    HXR  110
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    HXR  120
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   HXR  130
     6 IXPUT(9999),XPUT(9999)                                           HXR  140
C                                                                       HXR  150
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   HXR  160
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKHXR  170
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    HXR  180
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  HXR  190
     4 ITMAX,ITIME,BAT(211),DAL(211)                                    HXR  200
C                                                                       HXR  210
      DIMENSION SCAT(JVX,IVX),P2(JVX,IVX,KVX),DCONB(JVX,IVXP1,IOVX),    HXR  220
     1 DCONR(JVXP1,IVZ,IOVZ),PTSA(JVX,IVX,IOVX),NRGN(JVX,IVX),          HXR  230
     2 E1(LVX,KVX)                                                      HXR  240
C                                                                       HXR  250
C                                                                       HXR  260
      N = IX(20)                                                        HXR  270
      KKK = N + IOVX                                                    HXR  280
      IVY = IVX - 1                                                     HXR  290
      DO 141 J=1,JVX                                                    HXR  300
        JP1 = J - 1                                                     HXR  310
        JP2 = J + 1                                                     HXR  320
        DEL(1) = 0.0                                                    HXR  330
        D1 = DCONR(J,1,N)                                               HXR  340
        D2 = DCONR(JP2,1,N)                                             HXR  350
        D4 = DCONB(J,2,N)                                               HXR  360
        IF(J-1) 102,102,108                                             HXR  370
  102   IF(P2(1,1,K)) 103,104,103                                       HXR  380
  103   BET(1) = (P2(2,1,K)*D2+SCAT(1,1)) / D4                          HXR  390
        DEL(1) = D4 / (PTSA(1,1,N)+E1(1,K))                             HXR  400
  104   DO 107 I=2,IVX                                                  HXR  410
          IF(P2(1,I,K)) 106,105,106                                     HXR  420
  105     DEL(I) = 0.0                                                  HXR  430
          GOTO 107                                                      HXR  440
  106     T = D4 * DEL(I-1)                                             HXR  450
          L = NRGN(1,I)                                                 HXR  460
          D4 = DCONB(1,I+1,N)                                           HXR  470
          BET(I) = (P2(2,I,K)*DCONR(2,I,N)+ SCAT(1,I)+P2(2,I-1,K)*      HXR  480
     1     DCONR(2,I,KKK)+BET(I-1)*T) / D4                              HXR  490
          DEL(I) = D4 / (PTSA(1,I,N)-T+E1(L,K))                         HXR  500
  107   CONTINUE                                                        HXR  510
        GOTO 125                                                        HXR  520
  108   IF(J-JVX) 117,109,109                                           HXR  530
  109   IF(P2(JVX,1,K)) 110,111,110                                     HXR  540
  110   BET(1) = (P2(JP1,1,K)*D1+P2(JP1,2,K)*DCONR(JVX,2,KKK)+SCAT(J,1))HXR  550
     1   / D4                                                           HXR  560
        L = NRGN(JVX,1)                                                 HXR  570
        DEL(1) = D4 / (PTSA(JVX,1,N)+E1(L,K))                           HXR  580
  111   DO 114 I=2,IVY                                                  HXR  590
          IF(P2(JVX,I,K)) 113,112,113                                   HXR  600
  112     DEL(I) = 0.0                                                  HXR  610
          GOTO 114                                                      HXR  620
  113     T = D4 * DEL(I-1)                                             HXR  630
          L = NRGN(JVX,I)                                               HXR  640
          D4 = DCONB(J,I+1,N)                                           HXR  650
          BET(I) = (P2(JP1,I,K)*DCONR(JVX,I,N)+P2(JP1,I+1,K)*           HXR  660
     1     DCONR(JVX,I+1,KKK)+SCAT(JVX,I)+BET(I-1)*T) / D4              HXR  670
          DEL(I) = D4 / (PTSA(JVX,I,N)-T+E1(L,K))                       HXR  680
  114   CONTINUE                                                        HXR  690
        IF(P2(JVX,IVX,K)) 116,115,116                                   HXR  700
  115   DEL(IVX) = 0.                                                   HXR  710
        GOTO 125                                                        HXR  720
  116   T = D4 * DEL(I-1)                                               HXR  730
        L = NRGN(JVX,IVX)                                               HXR  740
        D4 = DCONB(J,IVXP1,N)                                           HXR  750
        BET(IVX) = (P2(JP1,IVX,K)*DCONR(JVX,IVX,N)+SCAT(JVX,IVX)+       HXR  760
     1   BET(IVX-1)*T) / D4                                             HXR  770
        DEL(IVX) = D4 / (PTSA(JVX,IVX,N)-T+E1(L,K))                     HXR  780
        GOTO 125                                                        HXR  790
  117   IF(P2(J,1,K)) 118,119,118                                       HXR  800
  118   CONTINUE                                                        HXR  810
        L = NRGN(J,1)                                                   HXR  820
        BET(1) = (P2(JP1,1,K)*D1+P2(JP2,1,K)*D2+P2(JP1,2,K)*            HXR  830
     1   DCONR(J,2,KKK)+SCAT(J,1)) / D4                                 HXR  840
        DEL(1) = D4 / (PTSA(J,1,N)+E1(L,K))                             HXR  850
  119   CONTINUE                                                        HXR  860
        DO 122 I=2,IVY                                                  HXR  870
          IF(P2(J,I,K)) 121,120,121                                     HXR  880
  120     DEL(I) = 0.0                                                  HXR  890
          GOTO 122                                                      HXR  900
  121     T = D4 * DEL(I-1)                                             HXR  910
          L = NRGN(J,I)                                                 HXR  920
          D4 = DCONB(J,I+1,N)                                           HXR  930
          BET(I) = (P2(JP1,I,K)*DCONR(J,I,N)+P2(JP2,I,K)*DCONR(JP2,I,N)+HXR  940
     1     P2(JP1,I+1,K)*DCONR(J,I+1,KKK)+P2(JP2,I-1,K)*DCONR(JP2,I,KKK)HXR  950
     2     +SCAT(J,I)+BET(I-1)*T) / D4                                  HXR  960
          DEL(I) = D4 / (PTSA(J,I,N)-T+E1(L,K))                         HXR  970
  122   CONTINUE                                                        HXR  980
        IF(P2(J,IVX,K)) 124,123,124                                     HXR  990
  123   DEL(IVX) = 0.                                                   HXR 1000
        GOTO 125                                                        HXR 1010
  124   T = D4 * DEL(I-1)                                               HXR 1020
        L = NRGN(J,IVX)                                                 HXR 1030
        D4 = DCONB(J,IVXP1,N)                                           HXR 1040
        BET(IVX) = (P2(JP1,IVX,K)*DCONR(J,IVX,N)+P2(JP2,IVX,K)*         HXR 1050
     1   DCONR(JP2,IVX,N)+P2(JP2,IVX-1,K)*DCONR(JP2,IVX,KKK)+SCAT(J,IVX)HXR 1060
     2   +BET(IVX-1)*T) / D4                                            HXR 1070
        DEL(IVX) = D4 / (PTSA(J,IVX,N)-T+E1(L,K))                       HXR 1080
  125   TEMP = BET(IVX) * DEL(IVX)                                      HXR 1090
        T = P2(J,IVX,K)                                                 HXR 1100
        TMF = T + VRGK2 * (TEMP-T)                                      HXR 1110
        IF(IEP) 126,130,127                                             HXR 1120
  126   P2(J,IVX,K) = TEMP                                              HXR 1130
        GOTO 131                                                        HXR 1140
  127   IF(TMF-TEMP) 129,130,128                                        HXR 1150
  128   TMF = DMIN1(TMF,(TEMP+T))                                       HXR 1160
        GOTO 130                                                        HXR 1170
  129   TMF = DMAX1(TMF,0.5*TEMP)                                       HXR 1180
  130   CONTINUE                                                        HXR 1190
        P2(J,IVX,K) = TMF                                               HXR 1200
  131   DO 138 JJ=2,IVX                                                 HXR 1210
          I = IVXP1 - JJ                                                HXR 1220
          T = P2(J,I,K)                                                 HXR 1230
          TEMP = DEL(I) * (TEMP+BET(I))                                 HXR 1240
          TMF = T + VRGK2 * (TEMP-T)                                    HXR 1250
          IF(IEP) 132,136,133                                           HXR 1260
  132     P2(J,I,K) = TEMP                                              HXR 1270
          GOTO 138                                                      HXR 1280
  133     IF(TMF-TEMP) 135,136,134                                      HXR 1290
  134     TMF = DMIN1(TMF,(TEMP+T))                                     HXR 1300
          GOTO 136                                                      HXR 1310
  135     TMF = DMAX1(TMF,0.5*TEMP)                                     HXR 1320
  136     CONTINUE                                                      HXR 1330
          P2(J,I,K) = TMF                                               HXR 1340
  138   CONTINUE                                                        HXR 1350
        IF(NUAC(8)) 139,141,141                                         HXR 1360
  139   M = JVXP1 - J                                                   HXR 1370
        DO 140 I=1,IVX                                                  HXR 1380
          L = IVXP1 - I                                                 HXR 1390
          P2(M,L,K) = P2(J,I,K)                                         HXR 1400
  140   CONTINUE                                                        HXR 1410
  141 CONTINUE                                                          HXR 1420
      RETURN                                                            HXR 1430
      END                                                               HXR 1440
      SUBROUTINE KBPR(P2E,B1,B2,B3,B4,B5,NRGNE,JVX,IVX,KBVX,KVX,LVX,JIVXKBP   10
     1 ,SIG,PVOL,NCOMP,MVX)                                             KBP   20
C                                                                       KBP   30
CKBPR --113 ***CITATION*** AVERAGE FLUX,ABS.,PROD CALC 3-D/ CF-KLUX     KBP   40
C                                                                       KBP   50
      REAL*8 SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2S,XLL1,D8KBP   60
     1 ,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK,B5LK,D1, KBP   70
     2 D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,YRDB,SPR50KBP   80
     3 ,XLAST,B1                                                        KBP   90
C                                                                       KBP  100
      COMMON /ADUBP/ SUMXI,TPTSA,XADB,XPDB,XS1DB,XS2DB,TL,XRDB,CS1S,CS2SKBP  110
     1 ,XLL1,D8,XADX,YADX,XLL2,XLL3,XLL4,XLL5,XLL6,XLEK,B2LK,B3LK,B4LK, KBP  120
     2 B5LK,D1,D2,D3,D4,D5,D6,D7,YADB,YPDB,YLEK,YS1S,YS2S,YS1DB,YS2DB,  KBP  130
     3 YRDB,SPR50,XLAST                                                 KBP  140
C                                                                       KBP  150
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KBP  160
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KBP  170
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KBP  180
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KBP  190
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KBP  200
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KBP  210
     6 IXPUT(9999),XPUT(9999)                                           KBP  220
C                                                                       KBP  230
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   KBP  240
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XELKKBP  250
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    KBP  260
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SAMXI,IX25,IX28,I,J,KB,K,  KBP  270
     4 ITMAX,ITIME,BET(211),DEL(211)                                    KBP  280
C                                                                       KBP  290
      DIMENSION P2E(JIVX,KBVX,KVX),B1(MVX,KVX),B2(MVX,KVX),B3(MVX,KVX), KBP  300
     1 B4(MVX,KVX),B5(MVX,KVX),NRGNE(JVX,IVX,KBVX),SIG(KVX,MVX,10),     KBP  310
     2 PVOL(LVX),NCOMP(LVX)                                             KBP  320
C                                                                       KBP  330
C                                                                       KBP  340
      DO 101 K=1,KVX                                                    KBP  350
        DO 100 M=1,MVX                                                  KBP  360
          B1(M,K) = 0.0                                                 KBP  370
  100   CONTINUE                                                        KBP  380
  101 CONTINUE                                                          KBP  390
      DO 105 K=1,KVX                                                    KBP  400
        DO 104 KB=1,KBVX                                                KBP  410
          N1 = 0                                                        KBP  420
          DO 103 I=1,IVX                                                KBP  430
            DO 102 J=1,JVX                                              KBP  440
              N1 = N1 + 1                                               KBP  450
              L = NRGNE(J,I,KB)                                         KBP  460
              M = NCOMP(L)                                              KBP  470
              B1(M,K) = B1(M,K) + P2E(N1,KB,K) * PVOL(L)                KBP  480
  102       CONTINUE                                                    KBP  490
  103     CONTINUE                                                      KBP  500
  104   CONTINUE                                                        KBP  510
  105 CONTINUE                                                          KBP  520
      DO 107 K=1,KMAX                                                   KBP  530
        DO 106 M=1,MMAX                                                 KBP  540
          B2LK = (SIG(K,M,3)+SIG(K,M,9)) * B1(M,K)                      KBP  550
          B3LK = SIG(K,M,4) * B1(M,K)                                   KBP  560
          B4LK = SIG(K,M,5) * B1(M,K)                                   KBP  570
          B5LK = SIG(K,M,8) * B1(M,K)                                   KBP  580
          XADB = XADB + B2LK                                            KBP  590
          XPDB = XPDB + B3LK                                            KBP  600
          XS1DB = XS1DB + B4LK                                          KBP  610
          XS2DB = XS2DB + B5LK                                          KBP  620
          B2(M,K) = B2(M,K) + B2LK                                      KBP  630
          B3(M,K) = B3LK                                                KBP  640
          B4(M,K) = B4LK                                                KBP  650
          B5(M,K) = B5LK                                                KBP  660
  106   CONTINUE                                                        KBP  670
  107 CONTINUE                                                          KBP  680
      RETURN                                                            KBP  690
      END                                                               KBP  700
      SUBROUTINE KOOP(P2E,SOURE,NRGNE,XII,IVX,JVX,KBVX,KVX, LVX,JIVX,   KOO   10
     1 XLAMDA,SIG,PVOL,NCOMP,MVX)                                       KOO   20
C                                                                       KOO   30
CKOOP --114 ***CITATION*** PHIBAR,SOURCE,CONVERGENCE FOR 3-D/ CF-KLUX   KOO   40
C                                                                       KOO   50
      REAL*8 XII,XLAMDA                                                 KOO   60
C                                                                       KOO   70
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KOO   80
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KOO   90
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KOO  100
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KOO  110
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KOO  120
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KOO  130
     6 IXPUT(9999),XPUT(9999)                                           KOO  140
C                                                                       KOO  150
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   KOO  160
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKKOO  170
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    KOO  180
     3 VRGABS,LO3,LO4,XLAMDB,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  KOO  190
     4 ITMAX,ITIME,BET(211),DEL(211)                                    KOO  200
C                                                                       KOO  210
      DIMENSION P2E(JIVX,KBVX,KVX),SOURE(JVX,IVX,KBVX),                 KOO  220
     1 NRGNE(JVX,IVX,KBVX),XII(KVX),SIG(KVX,MVX,10),PVOL(LVX),NCOMP(LVX)KOO  230
C                                                                       KOO  240
C                                                                       KOO  250
      IF(IX(24) .GT. 0) GOTO 106                                        KOO  260
      DO 105 KB=1,KBVX                                                  KOO  270
        DO 104 I=1,IVX                                                  KOO  280
          NN1= (I-1) * JVX                                              KOO  290
          DO 103 J=1,JVX                                                KOO  300
            N1= NN1 + J                                                 KOO  310
            L = NRGNE(J,I,KB)                                           KOO  320
            M = NCOMP(L)                                                KOO  330
            CKSO = 0.0                                                  KOO  340
            DO 100 K=1,KVX                                              KOO  350
              T1 = P2E(N1,KB,K)                                         KOO  360
              CKSO = CKSO + SIG(K,M,4) * T1                             KOO  370
  100       CONTINUE                                                    KOO  380
            IF(SPARE(98) .EQ. 0.0) GOTO 102                             KOO  390
            DO 101 K=1,KMAX                                             KOO  400
              CKSO = CKSO + XLAMDA * SIG(K,M,8) * P2E(N1,KB,K)          KOO  410
  101       CONTINUE                                                    KOO  420
  102       CONTINUE                                                    KOO  430
            SOURE(J,I,KB) = CKSO * PVOL(L)                              KOO  440
  103     CONTINUE                                                      KOO  450
  104   CONTINUE                                                        KOO  460
  105 CONTINUE                                                          KOO  470
      GOTO 111                                                          KOO  480
  106 CONTINUE                                                          KOO  490
      DO 110 KB=1,KBVX                                                  KOO  500
        DO 109 I=1,IVX                                                  KOO  510
          NN1= (I-1) * JVX                                              KOO  520
          DO 108 J=1,JVX                                                KOO  530
            N1 = NN1 + J                                                KOO  540
            CKSO = 0.0                                                  KOO  550
            DO 107 K=1,KVX                                              KOO  560
              T1 = P2E(N1,KB,K)                                         KOO  570
              CKSO = CKSO + XII(K) * T1                                 KOO  580
  107       CONTINUE                                                    KOO  590
            SOURE(J,I,KB) = CKSO                                        KOO  600
  108     CONTINUE                                                      KOO  610
  109   CONTINUE                                                        KOO  620
  110 CONTINUE                                                          KOO  630
  111 CONTINUE                                                          KOO  640
      RETURN                                                            KOO  650
      END                                                               KOO  660
      SUBROUTINE KWRD(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR,NRGNE, KWR   10
     1 E1,LVX,IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,JIVX,JIP1VX,JP1IXZ,   KWR   20
     2 IOVX,IOVZ)                                                       KWR   30
C                                                                       KWR   40
CKWRD --121***CITATION*** LINE RELAXATION ON ROWS 3-D/ CF-KNSD          KWR   50
C                                                                       KWR   60
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KWR   70
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KWR   80
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KWR   90
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KWR  100
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KWR  110
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KWR  120
     6 IXPUT(9999),XPUT(9999)                                           KWR  130
C                                                                       KWR  140
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   KWR  150
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKKWR  160
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    KWR  170
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  KWR  180
     4 ITMAX,ITIME,BET(211),DEL(211)                                    KWR  190
C                                                                       KWR  200
      DIMENSION SCATE(JVX,IVX,KBVX),P2E(JIVX,KBVX,KVX),                 KWR  210
     1 DCONBE(JIP1VX,KBVX,IOVX),DCONRE(JP1IXZ,KBVX,IOVZ),               KWR  220
     2 DCONBK(JIVX,KBVXP1,IOVX),PTSAE(JIVX,KBVX,IOVX),E1(LVX,KVX),      KWR  230
     3 NRGNE(JVX,IVX,KBVX),TSOUR(211)                                   KWR  240
C                                                                       KWR  250
CCCC ********** SUBSCRIPT DEFINITIONS (KWRD E-070) ********* CCCCC      KWR  260
C    NEW         OLD            NEW         OLD                         KWR  270
C     N1         J,I             N5 *     J+1,I                         KWR  280
C     N2         J,I-1           N6       JVX,I                         KWR  290
C     N3         J,I+1           N14        M,L                         KWR  300
C     N4         N/A             N15        I,J                         KWR  310
C     N9       JVX,J             N8         I,IVX                       KWR  320
C     N10      JVX,IVXP1-I                                              KWR  330
C                                                                       KWR  340
C     INRB = 1  ORDINARY                                                KWR  350
C     INRB = 2  PERIODIC(REPEATING)  RELAXATION DONE IN KPER            KWR  360
C     INRB = 3  90 DEGREE ROTATIONAL                                    KWR  370
C     INRB = 4  180 DEGREE ROTATIONAL                                   KWR  380
C                                                                       KWR  390
C                                                                       KWR  400
      INRB = IX(72) + 1                                                 KWR  410
      ASSIGN 111 TO ISYM                                                KWR  420
      IF(INRB .EQ. 3) ASSIGN 108 TO ISYM                                KWR  430
      IF(INRB .EQ. 4) ASSIGN 110 TO ISYM                                KWR  440
      N = IX(20)                                                        KWR  450
      DO 136 KB=1,KBVX                                                  KWR  460
        ASSIGN 100 TO IBR1                                              KWR  470
        IF(KB .LE. 1) ASSIGN 101 TO IBR1                                KWR  480
        ASSIGN 102 TO IBR2                                              KWR  490
        IF(KB .GE. KBVX) ASSIGN 103 TO IBR2                             KWR  500
        KBM1 = KB - 1                                                   KWR  510
        KBP1 = KB + 1                                                   KWR  520
        DO 135 I=1,IVX                                                  KWR  530
          ASSIGN 104 TO IBR3                                            KWR  540
          IF(I .LE. 1) ASSIGN 105 TO IBR3                               KWR  550
          ASSIGN 106 TO IBR4                                            KWR  560
          IF(I .GE. IVX) ASSIGN 107 TO IBR4                             KWR  570
          IM1 = I - 1                                                   KWR  580
          NN1 = IM1 * JVX                                               KWR  590
          NN2 = NN1 - JVX                                               KWR  600
          NN3 = NN1 + JVX                                               KWR  610
          N1 = NN1                                                      KWR  620
          N2 = NN2                                                      KWR  630
          N3 = NN3                                                      KWR  640
          DO 112 J=1,JVX                                                KWR  650
            N1 = N1 + 1                                                 KWR  660
            N2 = N2 + 1                                                 KWR  670
            N3 = N3 + 1                                                 KWR  680
            CKSS = SCATE(J,I,KB)                                        KWR  690
            GO TO IBR1,(100,101)                                        KWR  700
  100       CKSS = CKSS + P2E(N1,KBM1,K) * DCONBK(N1,KB,N)              KWR  710
  101       GO TO IBR2,(102,103)                                        KWR  720
  102       CKSS = CKSS + P2E(N1,KBP1,K) * DCONBK(N1,KBP1,N)            KWR  730
  103       GO TO IBR3,(104,105)                                        KWR  740
  104       CKSS = CKSS + P2E(N2,KB,K) * DCONBE(N1,KB,N)                KWR  750
  105       GO TO IBR4,(106,107)                                        KWR  760
  106       CKSS = CKSS + P2E(N3,KB,K) * DCONBE(N3,KB,N)                KWR  770
  107       GO TO ISYM,(111,108,110)                                    KWR  780
  108       CONTINUE                                                    KWR  790
            IF(J .NE. JMAX) GOTO 109                                    KWR  800
            IF(I .EQ. IMAX) GOTO 109                                    KWR  810
            N5 = I * JVXP1                                              KWR  820
            N8 = JIVX - JVX + I                                         KWR  830
            CKSS = CKSS + P2E(N8,KB,K) * DCONRE(N5,KB,N)                KWR  840
  109       IF(I .NE. IMAX) GOTO 111                                    KWR  850
            IF(J .EQ. JMAX) GOTO 111                                    KWR  860
            N3 = JIVX + J                                               KWR  870
            N9 = J * JVX                                                KWR  880
            CKSS = CKSS + P2E(N9,KB,K) * DCONBE(N3,KB,N)                KWR  890
            GOTO 111                                                    KWR  900
  110       CONTINUE                                                    KWR  910
            IF(J .NE. JMAX) GOTO 111                                    KWR  920
            N5 = I * JVXP1                                              KWR  930
            N10 = (IVXP1-I) * JVX                                       KWR  940
            CKSS = CKSS + P2E(N10,KB,K) * DCONRE(N5,KB,N)               KWR  950
  111       TSOUR(J) = CKSS                                             KWR  960
  112     CONTINUE                                                      KWR  970
          NN4 = IM1 * JVXP1                                             KWR  980
          N4 = NN4 + 1                                                  KWR  990
          N5 = N4 + 1                                                   KWR 1000
          N1 = NN1 + 1                                                  KWR 1010
          D4 = DCONRE(N5 ,KB,N)                                         KWR 1020
          IF(P2E(N1,KB,K) .EQ. 0.0) GOTO 113                            KWR 1030
          L = NRGNE(1,I,KB)                                             KWR 1040
          BET(1) = TSOUR(1) / D4                                        KWR 1050
          DEL(1) = D4 / (PTSAE(N1,KB,N)+E1(L,K))                        KWR 1060
          GOTO 114                                                      KWR 1070
  113     DEL(1) = 0.0                                                  KWR 1080
  114     CONTINUE                                                      KWR 1090
          DO 116 J=2,JVX                                                KWR 1100
            N1 = N1 + 1                                                 KWR 1110
            N5 = N5 + 1                                                 KWR 1120
            IF(P2E(N1,KB,K) .EQ. 0.0) GOTO 115                          KWR 1130
            L = NRGNE(J,I,KB)                                           KWR 1140
            T = D4 * DEL(J-1)                                           KWR 1150
            D4 = DCONRE(N5,KB,N)                                        KWR 1160
            BET(J) = (TSOUR(J)+BET(J-1)*T) / D4                         KWR 1170
            DEL(J) = D4 / (PTSAE(N1,KB,N)+E1(L,K)-T)                    KWR 1180
            GOTO 116                                                    KWR 1190
  115       DEL(J) = 0.0                                                KWR 1200
  116     CONTINUE                                                      KWR 1210
          N6 = NN3                                                      KWR 1220
          TEMP = BET(JVX) * DEL(JVX)                                    KWR 1230
          T = P2E(N6,KB,K)                                              KWR 1240
          TMF = T + BETTA * (TEMP-T)                                    KWR 1250
          IF(IEP) 117,121,118                                           KWR 1260
  117     P2E(N6,KB,K) = TEMP                                           KWR 1270
          GOTO 122                                                      KWR 1280
  118     IF(TMF-TEMP) 120,121,119                                      KWR 1290
  119     TMF = AMIN1(TMF,(TEMP+T))                                     KWR 1300
          GOTO 121                                                      KWR 1310
  120     TMF = AMAX1(TMF,0.5*TEMP)                                     KWR 1320
  121     CONTINUE                                                      KWR 1330
          P2E(N6,KB,K) = TMF                                            KWR 1340
  122     DO 129 JJ=2,JVX                                               KWR 1350
            J = JVXP1 - JJ                                              KWR 1360
            N1 = NN1 + J                                                KWR 1370
            T = P2E(N1,KB,K)                                            KWR 1380
            TEMP = DEL(J) * (TEMP+BET(J))                               KWR 1390
            TMF = T + BETTA * (TEMP-T)                                  KWR 1400
            IF(IEP) 123,127,124                                         KWR 1410
  123       P2E(N1,KB,K) = TEMP                                         KWR 1420
            GOTO 129                                                    KWR 1430
  124       IF(TMF-TEMP) 126,127,125                                    KWR 1440
  125       TMF = AMIN1(TMF,(TEMP+T))                                   KWR 1450
            GOTO 127                                                    KWR 1460
  126       TMF = AMAX1(TMF,0.5*TEMP)                                   KWR 1470
  127       CONTINUE                                                    KWR 1480
            P2E(N1,KB,K) = TMF                                          KWR 1490
  129     CONTINUE                                                      KWR 1500
          IF(NUAC(8)) 130,134,132                                       KWR 1510
  130     L = IVXP1 - I                                                 KWR 1520
          NN7 = (L-1) * JVX                                             KWR 1530
          DO 131 J=1,JVX                                                KWR 1540
            M = JVXP1 - J                                               KWR 1550
            N1 = NN1 + J                                                KWR 1560
            N14 = NN7 + M                                               KWR 1570
            P2E(N14,KB,K) = P2E(N1,KB,K)                                KWR 1580
  131     CONTINUE                                                      KWR 1590
          GOTO 134                                                      KWR 1600
  132     DO 133 J=1,JVX                                                KWR 1610
            N1 = NN1 + J                                                KWR 1620
            N15 = (J-1) * JVX + I                                       KWR 1630
            P2E(N15,KB,K) = P2E(N1,KB,K)                                KWR 1640
  133     CONTINUE                                                      KWR 1650
  134     CONTINUE                                                      KWR 1660
  135   CONTINUE                                                        KWR 1670
  136 CONTINUE                                                          KWR 1680
      RETURN                                                            KWR 1690
      END                                                               KWR 1700
      SUBROUTINE KXRD(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR,NRGNE, KXR   10
     1 E1,LVX,IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,JIVX,JIP1VX,JP1IXZ,   KXR   20
     2 IOVX,IOVZ)                                                       KXR   30
C                                                                       KXR   40
CKXRD --122***CTIATION*** LINE RELAXATION ON COLS 3-D/ CF-KNSD          KXR   50
C                                                                       KXR   60
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KXR   70
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KXR   80
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KXR   90
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KXR  100
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KXR  110
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KXR  120
     6 IXPUT(9999),XPUT(9999)                                           KXR  130
C                                                                       KXR  140
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   KXR  150
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKKXR  160
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    KXR  170
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  KXR  180
     4 ITMAX,ITIME,BET(211),DEL(211)                                    KXR  190
C                                                                       KXR  200
      DIMENSION SCATE(JVX,IVX,KBVX),P2E(JIVX,KBVX,KVX),                 KXR  210
     1 DCONBE(JIP1VX,KBVX,IOVX),DCONRE(JP1IXZ,KBVX,IOVZ),               KXR  220
     2 DCONBK(JIVX,KBVXP1,IOVX),PTSAE(JIVX,KBVX,IOVX),E1(LVX,KVX),      KXR  230
     3 NRGNE(JVX,IVX,KBVX),TSOUR(211)                                   KXR  240
C                                                                       KXR  250
CCCCC ********* SUBSCRIPT DEFINITIONS (KXRD E-080) ********* CCCCC      KXR  260
C    NEW         OLD            NEW         OLD                         KXR  270
C     N1         J,I             N13        J,2                         KXR  280
C     N2       J-1,I             N14      J-1,1                         KXR  290
C     N3       J+1,I             N15        J,1                         KXR  300
C     N4         J,I+1           N16      J+1,1                         KXR  310
C     N5         J,IVX           N17        M,L                         KXR  320
C     N6       JVX,I             N18 *      J,I                         KXR  330
C     N7       JVX,1             N19 *    J+1,I                         KXR  340
C     N8         2,I             N20 *    J+1,1                         KXR  350
C     N9         1,I+1           N21 *    JVX,I                         KXR  360
C     N10        1,I             N22 *      2,I                         KXR  370
C     N11        2,1             N23 *      J,1                         KXR  380
C     N12        1,1                                                    KXR  390
C                                                                       KXR  400
C                                                                       KXR  410
      N = IX(20)                                                        KXR  420
      N12 = 1                                                           KXR  430
      N11 = 2                                                           KXR  440
      N7 = JVX                                                          KXR  450
      NN1 = (IVX-1) * JVX                                               KXR  460
      DO 141 KB=1,KBVX                                                  KXR  470
        KBM1 = KB - 1                                                   KXR  480
        KBP1 = KB + 1                                                   KXR  490
        DO 140 J=1,JVX                                                  KXR  500
          DO 103 I=1,IVX                                                KXR  510
            N1 = (I-1) * JVX + J                                        KXR  520
            CKSS = SCATE(J,I,KB)                                        KXR  530
            IF(KB .LE. 1) GOTO 101                                      KXR  540
            CKSS = CKSS + P2E(N1,KBM1,K) * DCONBK(N1,KB,N)              KXR  550
  101       IF(KB .GE. KBVX) GOTO 102                                   KXR  560
            CKSS = CKSS + P2E(N1,KBP1,K) * DCONBK(N1,KBP1,N)            KXR  570
  102       TSOUR(I) = CKSS                                             KXR  580
  103     CONTINUE                                                      KXR  590
          JP1 = J - 1                                                   KXR  600
          JP2 = J + 1                                                   KXR  610
          N13 = JVX + J                                                 KXR  620
          N14 = JP1                                                     KXR  630
          N15 = J                                                       KXR  640
          N16 = JP2                                                     KXR  650
          N5 = NN1 + J                                                  KXR  660
          N23 = J                                                       KXR  670
          N20 = JP2                                                     KXR  680
          DEL(1) = 0.0                                                  KXR  690
          D1 = DCONRE(N23,KB,N)                                         KXR  700
          D2 = DCONRE(N20,KB,N)                                         KXR  710
          D4 = DCONBE(N13,KB,N)                                         KXR  720
          IF(J-1) 105,105,111                                           KXR  730
  105     IF(P2E(N12,KB,K)) 106,107,106                                 KXR  740
  106     BET(1) = (P2E(N11,KB,K)*D2+TSOUR(1)) / D4                     KXR  750
          L = NRGNE(1,1,KB)                                             KXR  760
          DEL(1) = D4 / (PTSAE(N12,KB,N)+E1(L,K))                       KXR  770
  107     DO 110 I=2,IVX                                                KXR  780
            IM1 = I - 1                                                 KXR  790
            NN2 = IM1 * JVX                                             KXR  800
            NN3 = IM1 * JVXP1                                           KXR  810
            N10 = NN2 + 1                                               KXR  820
            N9 = NN2 + JVX + 1                                          KXR  830
            N8 = NN2 + 2                                                KXR  840
            N22 = NN3 + 2                                               KXR  850
            IF(P2E(N10,KB,K)) 109,108,109                               KXR  860
  108       DEL(I) = 0.0                                                KXR  870
            GOTO 110                                                    KXR  880
  109       T = D4 * DEL(I-1)                                           KXR  890
            L = NRGNE(1,I,KB)                                           KXR  900
            D4 = DCONBE(N9,KB,N)                                        KXR  910
            BET(I) = (P2E(N8,KB,K)*DCONRE(N22,KB,N)+TSOUR(I)+BET(I-1)*T)KXR  920
     1       / D4                                                       KXR  930
            DEL(I) = D4 / (PTSAE(N10,KB,N)-T+E1(L,K))                   KXR  940
  110     CONTINUE                                                      KXR  950
          GOTO 124                                                      KXR  960
  111     IF(J-JVX) 118,112,112                                         KXR  970
  112     IF(P2E(N7,KB,K)) 113,114,113                                  KXR  980
  113     BET(1) = (P2E(N14,KB,K)*D1+TSOUR(1)) / D4                     KXR  990
          L = NRGNE(JVX,1,KB)                                           KXR 1000
          DEL(1) = D4 / (PTSAE(N7,KB,N)+E1(L,K))                        KXR 1010
  114     DO 117 I=2,IVX                                                KXR 1020
            IM1 = I - 1                                                 KXR 1030
            NN2 = IM1 * JVX                                             KXR 1040
            NN3 = IM1 * JVXP1                                           KXR 1050
            N6 = NN2 + JVX                                              KXR 1060
            N4 = NN2 + JVX + J                                          KXR 1070
            N2 = NN2 + JP1                                              KXR 1080
            N21 = NN3 + JVX                                             KXR 1090
            IF(P2E(N6,KB,K)) 116,115,116                                KXR 1100
  115       DEL(I) = 0.                                                 KXR 1110
            GOTO 117                                                    KXR 1120
  116       T = D4 * DEL(I-1)                                           KXR 1130
            L = NRGNE(JVX,I,KB)                                         KXR 1140
            D4 = DCONBE(N4,KB,N)                                        KXR 1150
            BET(I) = (P2E(N2,KB,K)*DCONRE(N21,KB,N)+TSOUR(I)+BET(I-1)*T)KXR 1160
     1       / D4                                                       KXR 1170
            DEL(I) = D4 / (PTSAE(N6,KB,N)-T+E1(L,K))                    KXR 1180
  117     CONTINUE                                                      KXR 1190
          GOTO 124                                                      KXR 1200
  118     IF(P2E(N15,KB,K)) 119,120,119                                 KXR 1210
  119     CONTINUE                                                      KXR 1220
          L = NRGNE(J,1,KB)                                             KXR 1230
          BET(1) = (P2E(N14,KB,K)*D1+P2E(N16,KB,K)*D2+TSOUR(1)) / D4    KXR 1240
          DEL(1) = D4 / (PTSAE(N15,KB,N)+E1(L,K))                       KXR 1250
  120     DO 123 I=2,IVX                                                KXR 1260
            IM1 = I - 1                                                 KXR 1270
            NN2 = IM1 * JVX                                             KXR 1280
            NN3 = IM1 * JVXP1                                           KXR 1290
            N1 = NN2 + J                                                KXR 1300
            N4 = NN2 + JVX + J                                          KXR 1310
            N2 = NN2 + JP1                                              KXR 1320
            N3 = NN2 + JP2                                              KXR 1330
            N18 = NN3 + J                                               KXR 1340
            N19 = NN3 + JP2                                             KXR 1350
            IF(P2E(N1,KB,K)) 122,121,122                                KXR 1360
  121       DEL(I) = 0.0                                                KXR 1370
            GOTO 123                                                    KXR 1380
  122       T = D4 * DEL(I-1)                                           KXR 1390
            L = NRGNE(J,I,KB)                                           KXR 1400
            D4 = DCONBE(N4,KB,N)                                        KXR 1410
            BET(I) = (P2E(N2,KB,K)*DCONRE(N18,KB,N)+P2E(N3,KB,K)*       KXR 1420
     1       DCONRE(N19,KB,N)+TSOUR(I)+BET(I-1)*T) / D4                 KXR 1430
            DEL(I) = D4 / (PTSAE(N1,KB,N)-T+E1(L,K))                    KXR 1440
  123     CONTINUE                                                      KXR 1450
  124     TEMP = BET(IVX) * DEL(IVX)                                    KXR 1460
          T = P2E(N5,KB,K)                                              KXR 1470
          TMF = T + VRGK2 * (TEMP-T)                                    KXR 1480
          IF(IEP) 125,129,126                                           KXR 1490
  125     P2E(N5,KB,K) = TEMP                                           KXR 1500
          GOTO 130                                                      KXR 1510
  126     IF(TMF-TEMP) 128,129,127                                      KXR 1520
  127     TMF = AMIN1(TMF,(TEMP+T))                                     KXR 1530
          GOTO 129                                                      KXR 1540
  128     TMF = AMAX1(TMF,0.5*TEMP)                                     KXR 1550
  129     CONTINUE                                                      KXR 1560
          P2E(N5,KB,K) = TMF                                            KXR 1570
  130     DO 137 JJ=2,IVX                                               KXR 1580
            I = IVXP1 - JJ                                              KXR 1590
            N1 = (I-1) * JVX + J                                        KXR 1600
            T = P2E(N1,KB,K)                                            KXR 1610
            TEMP = DEL(I) * (TEMP+BET(I))                               KXR 1620
            TMF = T + VRGK2 * (TEMP-T)                                  KXR 1630
            IF(IEP) 131,135,132                                         KXR 1640
  131       P2E(N1,KB,K) = TEMP                                         KXR 1650
            GOTO 137                                                    KXR 1660
  132       IF(TMF-TEMP) 134,135,133                                    KXR 1670
  133       TMF = AMIN1(TMF,(TEMP+T))                                   KXR 1680
            GOTO 135                                                    KXR 1690
  134       TMF = AMAX1(TMF,0.5*TEMP)                                   KXR 1700
  135       CONTINUE                                                    KXR 1710
            P2E(N1,KB,K) = TMF                                          KXR 1720
  137     CONTINUE                                                      KXR 1730
          IF(NUAC(8)) 138,140,140                                       KXR 1740
  138     M = JVXP1 - J                                                 KXR 1750
          DO 139 I=1,IVX                                                KXR 1760
            L = IVXP1 - I                                               KXR 1770
            N17 = (L-1) * JVX + M                                       KXR 1780
            N1 = (I-1) * JVX + J                                        KXR 1790
            P2E(N17,KB,K) = P2E(N1,KB,K)                                KXR 1800
  139     CONTINUE                                                      KXR 1810
  140   CONTINUE                                                        KXR 1820
  141 CONTINUE                                                          KXR 1830
      RETURN                                                            KXR 1840
      END                                                               KXR 1850
      SUBROUTINE KZRD(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR,NRGNE, KZR   10
     1 E1,LVX,IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,JIVX,JIP1VX,JP1IXZ,   KZR   20
     2 IOVX,IOVZ)                                                       KZR   30
C                                                                       KZR   40
CKZRD --123***CITATION*** LINE RELAXATION BKWD 3-D/ CF-KNSD             KZR   50
C                                                                       KZR   60
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KZR   70
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KZR   80
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KZR   90
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KZR  100
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KZR  110
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KZR  120
     6 IXPUT(9999),XPUT(9999)                                           KZR  130
C                                                                       KZR  140
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   KZR  150
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKKZR  160
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    KZR  170
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  KZR  180
     4 ITMAX,ITIME,BET(211),DEL(211)                                    KZR  190
C                                                                       KZR  200
      DIMENSION SCATE(JVX,IVX,KBVX),P2E(JIVX,KBVX,KVX),                 KZR  210
     1 DCONBE(JIP1VX,KBVX,IOVX),DCONRE(JP1IXZ,KBVX,IOVZ),               KZR  220
     2 DCONBK(JIVX,KBVXP1,IOVX),PTSAE(JIVX,KBVX,IOVX),E1(LVX,KVX),      KZR  230
     3 NRGNE(JVX,IVX,KBVX),TSOUR(211)                                   KZR  240
C                                                                       KZR  250
CCCCC ********* SUBSCRIPT DEFINITIONS (KZRD E-085) ********* CCCCC      KZR  260
C    NEW         OLD            NEW         OLD                         KZR  270
C     N1         J,I             N7         L,I                         KZR  280
C     N2       J-1,I             N8        KB,I                         KZR  290
C     N3       J+1,I             N9  *      J,I                         KZR  300
C     N4         1,I             N10 *    J+1,I                         KZR  310
C     N5         2,I             N11 *      2,I                         KZR  320
C     N6       JVX,I             N12 *     JVX,I                        KZR  330
C                                N13        J,I-1                       KZR  340
C                                N14        J,I+1                       KZR  350
C                                                                       KZR  360
      N = IX(20)                                                        KZR  370
      DO 142 I=1,IVX                                                    KZR  380
        NN1 = (I-1) * JVX                                               KZR  390
        N4 = NN1 + 1                                                    KZR  400
        N5 = NN1 + 2                                                    KZR  410
        N6 = NN1 + JVX                                                  KZR  420
        NN2 = (I-1) * JVXP1                                             KZR  430
        N11 = NN2 + 2                                                   KZR  440
        N12 = NN2 + JVX                                                 KZR  450
        DO 141 J=1,JVX                                                  KZR  460
          N1 = NN1 + J                                                  KZR  470
          N13 = N1 - JVX                                                KZR  480
          N14 = N1 + JVX                                                KZR  490
          DO 103 KB=1,KBVX                                              KZR  500
            CKSS = SCATE(J,I,KB)                                        KZR  510
            IF(I .LE. 1) GOTO 101                                       KZR  520
            CKSS = CKSS + P2E(N13,KB,K) * DCONBE(N1,KB,N)               KZR  530
  101       IF(I .GE. IVX) GOTO 102                                     KZR  540
            CKSS = CKSS + P2E(N14,KB,K) * DCONBE(N14,KB,N)              KZR  550
  102       TSOUR(KB) = CKSS                                            KZR  560
  103     CONTINUE                                                      KZR  570
          JP1 = J - 1                                                   KZR  580
          JP2 = J + 1                                                   KZR  590
          N2 = NN1 + JP1                                                KZR  600
          N3 = NN1 + JP2                                                KZR  610
          N9 = NN2 + J                                                  KZR  620
          N10 = NN2 + JP2                                               KZR  630
          DEL(1) = 0.0                                                  KZR  640
          D1 = DCONRE(N9,1,N)                                           KZR  650
          D2 = DCONRE(N10,1,N)                                          KZR  660
          D4 = DCONBK(N1,2,N)                                           KZR  670
          IF(J-1) 104,104,110                                           KZR  680
  104     IF(P2E(N4,1,K)) 105,106,105                                   KZR  690
  105     BET(1) = (P2E(N5,1,K)*D2+TSOUR(1)) / D4                       KZR  700
          L = NRGNE(1,I,1)                                              KZR  710
          DEL(1) = D4 / (PTSAE(N4,1,N)+E1(L,K))                         KZR  720
  106     DO 109 KB=2,KBVX                                              KZR  730
            IF(P2E(N4,KB,K)) 108,107,108                                KZR  740
  107       DEL(KB) = 0.0                                               KZR  750
            GOTO 109                                                    KZR  760
  108       T = D4 * DEL(KB-1)                                          KZR  770
            L = NRGNE(1,I,KB)                                           KZR  780
            D4 = DCONBK(N4,KB+1,N)                                      KZR  790
            BET(KB) = (P2E(N5,KB,K)*DCONRE(N11,KB,N)+TSOUR(KB)+BET(KB-1)KZR  800
     1       *T) / D4                                                   KZR  810
            DEL(KB) = D4 / (PTSAE(N4,KB,N)-T+E1(L,K))                   KZR  820
  109     CONTINUE                                                      KZR  830
          GOTO 123                                                      KZR  840
  110     IF(J-JVX) 117,111,111                                         KZR  850
  111     IF(P2E(N6,1,K)) 112,113,112                                   KZR  860
  112     BET(1) = (P2E(N2,1,K)*D1+TSOUR(1)) / D4                       KZR  870
          L = NRGNE(JVX,I,1)                                            KZR  880
          DEL(1) = D4 / (PTSAE(N6,1,N)+E1(L,K))                         KZR  890
  113     DO 116 KB=2,KBVX                                              KZR  900
            IF(P2E(N6,KB,K)) 115,114,115                                KZR  910
  114       DEL(KB) = 0.                                                KZR  920
            GOTO 116                                                    KZR  930
  115       T = D4 * DEL(KB-1)                                          KZR  940
            L = NRGNE(JVX,I,KB)                                         KZR  950
            D4 = DCONBK(N1,KB+1,N)                                      KZR  960
            BET(KB) = (P2E(N2,KB,K)*DCONRE(N12,KB,N)+TSOUR(KB)+BET(KB-1)KZR  970
     1       *T) / D4                                                   KZR  980
            DEL(KB) = D4 / (PTSAE(N6,KB,N)-T+E1(L,K))                   KZR  990
  116     CONTINUE                                                      KZR 1000
          GOTO 123                                                      KZR 1010
  117     IF(P2E(N1,1,K)) 118,119,118                                   KZR 1020
  118     CONTINUE                                                      KZR 1030
          L = NRGNE(J,I,1)                                              KZR 1040
          BET(1) = (P2E(N2,1,K)*D1+P2E(N3,1,K)*D2+TSOUR(1)) / D4        KZR 1050
          DEL(1) = D4 / (PTSAE(N1,1,N)+E1(L,K))                         KZR 1060
  119     DO 122 KB=2,KBVX                                              KZR 1070
            IF(P2E(N1,KB,K)) 121,120,121                                KZR 1080
  120       DEL(KB) = 0.                                                KZR 1090
            GOTO 122                                                    KZR 1100
  121       T = D4 * DEL(KB-1)                                          KZR 1110
            L = NRGNE(J,I,KB)                                           KZR 1120
            D4 = DCONBK(N1,KB+1,N)                                      KZR 1130
            BET(KB) = (P2E(N2,KB,K)*DCONRE(N9,KB,N)+P2E(N3,KB,K)*       KZR 1140
     1       DCONRE(N10,KB,N)+TSOUR(KB)+BET(KB-1)*T) / D4               KZR 1150
            DEL(KB) = D4 / (PTSAE(N1,KB,N)-T+E1(L,K))                   KZR 1160
  122     CONTINUE                                                      KZR 1170
  123     TEMP = BET(KBVX) * DEL(KBVX)                                  KZR 1180
          T = P2E(N1,KBVX,K)                                            KZR 1190
          TMF = T + SPARE(39) * (TEMP-T)                                KZR 1200
          IF(IEP) 124,128,125                                           KZR 1210
  124     P2E(N1,KBVX,K) = TEMP                                         KZR 1220
          GOTO 129                                                      KZR 1230
  125     IF(TMF-TEMP) 127,128,126                                      KZR 1240
  126     TMF = AMIN1(TMF,(TEMP+T))                                     KZR 1250
          GOTO 128                                                      KZR 1260
  127     TMF = AMAX1(TMF,0.5*TEMP)                                     KZR 1270
  128     CONTINUE                                                      KZR 1280
          P2E(N1,KBVX,K) = TMF                                          KZR 1290
  129     DO 136 KK=2,KBVX                                              KZR 1300
            KB = KBVXP1 - KK                                            KZR 1310
            T = P2E(N1,KB,K)                                            KZR 1320
            TEMP = DEL(KB) * (TEMP+BET(KB))                             KZR 1330
            TMF = T + SPARE(39) * (TEMP-T)                              KZR 1340
            IF(IEP) 130,134,131                                         KZR 1350
  130       P2E(N1,KB,K) = TEMP                                         KZR 1360
            GOTO 136                                                    KZR 1370
  131       IF(TMF-TEMP) 133,134,132                                    KZR 1380
  132       TMF = AMIN1(TMF,(TEMP+T))                                   KZR 1390
            GOTO 134                                                    KZR 1400
  133       TMF = AMAX1(TMF,0.5*TEMP)                                   KZR 1410
  134       CONTINUE                                                    KZR 1420
            P2E(N1,KB,K) = TMF                                          KZR 1430
  136     CONTINUE                                                      KZR 1440
          IF(NUAC(9)) 137,141,139                                       KZR 1450
  137     L = JVXP1 - J                                                 KZR 1460
          N7 = NN1 + L                                                  KZR 1470
          DO 138 KB=1,KBVX                                              KZR 1480
            M = KBVXP1 - KB                                             KZR 1490
            P2E(N7,M,K) = P2E(N1,KB,K)                                  KZR 1500
  138     CONTINUE                                                      KZR 1510
          GOTO 141                                                      KZR 1520
  139     DO 140 KB=1,KBVX                                              KZR 1530
            N8 = NN1 + KB                                               KZR 1540
            P2E(N8,J,K) = P2E(N1,KB,K)                                  KZR 1550
  140     CONTINUE                                                      KZR 1560
  141   CONTINUE                                                        KZR 1570
  142 CONTINUE                                                          KZR 1580
      RETURN                                                            KZR 1590
      END                                                               KZR 1600
      SUBROUTINE KPER(SCATE,P2E,DCONBE,DCONRE,DCONBK,PTSAE,TSOUR,NRGNE, KPE   10
     1 E1,LVX,IVX,JVX,KBVX,KVX,IVXP1,JVXP1,KBVXP1,JIVX,JIP1VX,JP1IXZ,   KPE   20
     2 IOVX,IOVZ)                                                       KPE   30
C                                                                       KPE   40
CKPER --124 ***CITATION*** LINE RELAXATION ON ROWS FOR PERIODIC         KPE   50
C                          BOUNDARY CONDITIONS (3-D)/ CF-KNSD           KPE   60
C                                                                       KPE   70
      COMMON /ALSUB/ BLSUB(30),TITL1(18),TITL2(18),IMAX,JMAX,KBMAX,KMAX,KPE   80
     1 LMAX,MMAX,NMAX,IMXP1,JMXP1,KBMXP1,NSETMX,NTO,MM1VX,KM1VX,IOIN,   KPE   90
     2 IOUT,IOSIG,IOFLX,IO1,IO2,IO3,IO4,IO7,NER(200),IX(200),INNO(200), KPE  100
     3 NGC(24),IEDG(24),ITMX(24),TIMX(6),GLIM(6),NDPL(24),IEDP1(24),    KPE  110
     4 IEDP2(24),IEDP3(24),DPLH(6),NUAC(24),EPI(6),XMIS(6),NSRH(24),    KPE  120
     5 XSRH1(6),XTR1(200),XTR2(200),NXTR1(200),NXTR2(200),SPARE(200),   KPE  130
     6 IXPUT(9999),XPUT(9999)                                           KPE  140
C                                                                       KPE  150
      COMMON /AFLUX/ BFLUX(30),KXMN8,NIT,NIIT,NIIIT,JXP1,KSCT1,KSCT2,   KPE  160
     1 ISTART,IEP,VRGP1,VRGP2,VRGP3,VRG1,VRG2,VRGK1,VRGK2,XABS,PROD,XLEKKPE  170
     2 ,RMX,RMN,XKEF1,XKEF2,XKEF3,EXFC1,EXFC2,EXFC3,NI3,IEXTR,IRECV,    KPE  180
     3 VRGABS,LO3,LO4,XLAMDA,EPI1,EPI2,BETTA,SUMXI,IX25,IX28,I,J,KB,K,  KPE  190
     4 ITMAX,ITIME,BET(211),DEL(211)                                    KPE  200
C                                                                       KPE  210
      DIMENSION SCATE(JVX,IVX,KBVX),P2E(JIVX,KBVX,KVX),                 KPE  220
     1 DCONBE(JIP1VX,KBVX,IOVX),DCONRE(JP1IXZ,KBVX,IOVZ),               KPE  230
     2 DCONBK(JIVX,KBVXP1,IOVX),PTSAE(JIVX,KBVX,IOVX),E1(LVX,KVX),      KPE  240
     3 NRGNE(JVX,IVX,KBVX),TSOUR(211),ALP(211)                          KPE  250
C                                                                       KPE  260
CCCC ********** SUBSCRIPT DEFINITIONS (KPER E-071) ********* CCCCC      KPE  270
C    NEW         OLD            NEW         OLD                         KPE  280
C     N1         J,I             N5 *     J+1,I                         KPE  290
C     N2         J,I-1           N6       JVX,I                         KPE  300
C     N3         J,I+1           N7 *     JVX,I                         KPE  310
C     N4 *       1,I             N8 *   JVX+1,I                         KPE  320
C                                                                       KPE  330
C                                                                       KPE  340
      N = IX(20)                                                        KPE  350
      JVXM1 = JVX - 1                                                   KPE  360
      DO 127 KB=1,KBVX                                                  KPE  370
        KBM1 = KB - 1                                                   KPE  380
        KBP1 = KB + 1                                                   KPE  390
        DO 126 I=1,IVX                                                  KPE  400
          IM1 = I - 1                                                   KPE  410
          IP1 = I + 1                                                   KPE  420
          NN1 = IM1 * JVX                                               KPE  430
          NN2 = NN1 - JVX                                               KPE  440
          NN3 = NN1 + JVX                                               KPE  450
          N1 = NN1                                                      KPE  460
          N2 = NN2                                                      KPE  470
          N3 = NN3                                                      KPE  480
          DO 104 J=1,JVX                                                KPE  490
            N1 = N1 + 1                                                 KPE  500
            N2 = N2 + 1                                                 KPE  510
            N3 = N3 + 1                                                 KPE  520
            CKSS = SCATE(J,I,KB)                                        KPE  530
            IF(KB .LE. 1) GOTO 100                                      KPE  540
            CKSS = CKSS + P2E(N1,KBM1,K) * DCONBK(N1,KB,N)              KPE  550
  100       IF(KB .GE. KBVX) GOTO 101                                   KPE  560
            CKSS = CKSS + P2E(N1,KBP1,K) * DCONBK(N1,KBP1,N)            KPE  570
  101       IF(I .LE. 1) GOTO 102                                       KPE  580
            CKSS = CKSS + P2E(N2,KB,K) * DCONBE(N1,KB,N)                KPE  590
  102       IF(I .GE. IVX) GOTO 103                                     KPE  600
            CKSS = CKSS + P2E(N3,KB,K) * DCONBE(N3,KB,N)                KPE  610
  103       TSOUR(J) = CKSS                                             KPE  620
  104     CONTINUE                                                      KPE  630
          NN4 = IM1 * JVXP1                                             KPE  640
          N4 = NN4 + 1                                                  KPE  650
          N5 = N4 + 1                                                   KPE  660
          N1 = NN1 + 1                                                  KPE  670
          D4 = DCONRE(N5,KB,N)                                          KPE  680
          IF(P2E(N1,KB,K) .EQ. 0.0) GOTO 105                            KPE  690
          L = NRGNE(1,I,KB)                                             KPE  700
          BET(1) = TSOUR(1) / D4                                        KPE  710
          DEL(1) = D4 / (PTSAE(N1,KB,N)+E1(L,K))                        KPE  720
          ALP(1) = DCONRE(N4,KB,N) / D4                                 KPE  730
          GOTO 106                                                      KPE  740
  105     BET(1) = 0.0                                                  KPE  750
          DEL(1) = 0.0                                                  KPE  760
          ALP(1) = 0.0                                                  KPE  770
  106     CONTINUE                                                      KPE  780
          DO 108 J=2,JVX                                                KPE  790
            N1 = N1 + 1                                                 KPE  800
            N5 = N5 + 1                                                 KPE  810
            IF(P2E(N1,KB,K) .EQ. 0.0) GOTO 107                          KPE  820
            L = NRGNE(J,I,KB)                                           KPE  830
            T = D4 * DEL(J-1)                                           KPE  840
            D4 = DCONRE(N5,KB,N)                                        KPE  850
            BET(J) = (TSOUR(J)+BET(J-1)*T) / D4                         KPE  860
            DEL(J) = D4 / (PTSAE(N1,KB,N)+E1(L,K)-T)                    KPE  870
            ALP(J) = ALP(J-1) * T / D4                                  KPE  880
            GOTO 108                                                    KPE  890
  107       BET(J) = 0.0                                                KPE  900
            DEL(J) = 0.0                                                KPE  910
            ALP(J) = 0.0                                                KPE  920
  108     CONTINUE                                                      KPE  930
          N6 = NN3                                                      KPE  940
          N7 = NN4 + JVX                                                KPE  950
          N8 = N7 + 1                                                   KPE  960
          IF(P2E(N6,KB,K) .NE. 0.0) GOTO 109                            KPE  970
          TEMP = 0.0                                                    KPE  980
          TT = 0.0                                                      KPE  990
          GOTO 117                                                      KPE 1000
  109     F = 1.0                                                       KPE 1010
          D = 0.0                                                       KPE 1020
          E = 0.0                                                       KPE 1030
          DO 110 J=1,JVXM1                                              KPE 1040
            F = F * DEL(J)                                              KPE 1050
            IF(DEL(J) .EQ. 0.0) GOTO 111                                KPE 1060
            D = D + BET(J) * F                                          KPE 1070
            EM30 = 1.E-30                                               KPE 1080
            IF(ABS(ALP(J)) .LT. EM30) GOTO 210                          KPE 1090
            E0 = ALP(J) * F                                             KPE 1100
            GOTO 211                                                    KPE 1110
  210       CONTINUE                                                    KPE 1120
            IF(ALP(J) .NE. 0.) GOTO 212                                 KPE 1130
            E0 = 0.                                                     KPE 1140
            GOTO 211                                                    KPE 1150
  212       CONTINUE                                                    KPE 1160
            X = SIGN(1.,ALP(J)) * SIGN(1.,F)                            KPE 1170
            Y = ALOG10(ABS(ALP(J))) + ALOG10(ABS(F))                    KPE 1180
            IF(Y .GT. -60.) GOTO 213                                    KPE 1190
            E0 = 0.                                                     KPE 1200
            GOTO 211                                                    KPE 1210
  213       CONTINUE                                                    KPE 1220
            E0 = 10.**Y * X                                             KPE 1230
  211       CONTINUE                                                    KPE 1240
            E = E + E0                                                  KPE 1250
  110     CONTINUE                                                      KPE 1260
  111     CONTINUE                                                      KPE 1270
          RDEL = 0.0                                                    KPE 1280
          IF(DEL(JVXM1) .NE. 0.0) RDEL = 1.0 / DEL(JVXM1)               KPE 1290
          L = NRGNE(JVX,I,KB)                                           KPE 1300
          D1 = DCONRE(N7,KB,N)                                          KPE 1310
          D2 = DCONRE(N8,KB,N)                                          KPE 1320
          D4 = D1 + F * D2                                              KPE 1330
          TEMP =((TSOUR(JVX)+D*D2)*RDEL+D4*BET(JVXM1)) /                KPE 1340
     1     ((PTSAE(N6,KB,N)+E1(L,K)-E*D2)*RDEL-D4*(1.0+ALP(JVXM1)))     KPE 1350
          TT = TEMP                                                     KPE 1360
          T = P2E(N6,KB,K)                                              KPE 1370
          TMF = T + BETTA * (TEMP-T)                                    KPE 1380
          IF(IEP) 112,116,113                                           KPE 1390
  112     P2E(N6,KB,K) = TEMP                                           KPE 1400
          GOTO 117                                                      KPE 1410
  113     IF(TMF-TEMP) 115,116,114                                      KPE 1420
  114     TMF = AMIN1(TMF,(TEMP+T))                                     KPE 1430
          GOTO 116                                                      KPE 1440
  115     TMF = AMAX1(TMF,0.5*TEMP)                                     KPE 1450
  116     CONTINUE                                                      KPE 1460
          P2E(N6,KB,K) = TMF                                            KPE 1470
  117     DO 125 JJ=2,JVX                                               KPE 1480
            J = JVXP1 - JJ                                              KPE 1490
            N1 = NN1 + J                                                KPE 1500
            T = P2E(N1,KB,K)                                            KPE 1510
            IF(T .NE. 0.0) GOTO 118                                     KPE 1520
            TEMP = 0.0                                                  KPE 1530
            GOTO 125                                                    KPE 1540
  118       CONTINUE                                                    KPE 1550
            TEMP = DEL(J) * (TEMP+BET(J)+TT*ALP(J))                     KPE 1560
            TMF = T + BETTA * (TEMP-T)                                  KPE 1570
            IF(IEP) 119,123,120                                         KPE 1580
  119       P2E(N1,KB,K) = TEMP                                         KPE 1590
            GOTO 125                                                    KPE 1600
  120       IF(TMF-TEMP) 122,123,121                                    KPE 1610
  121       TMF = AMIN1(TMF,(TEMP+T))                                   KPE 1620
            GOTO 123                                                    KPE 1630
  122       TMF = AMAX1(TMF,0.5*TEMP)                                   KPE 1640
  123       CONTINUE                                                    KPE 1650
            P2E(N1,KB,K) = TMF                                          KPE 1660
  125     CONTINUE                                                      KPE 1670
  126   CONTINUE                                                        KPE 1680
  127 CONTINUE                                                          KPE 1690
      RETURN                                                            KPE 1700
      END                                                               KPE 1710