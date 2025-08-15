      SUBROUTINE INPLIST                                                NPL   10
C                                                                       NPL   20
C     INPUT-LISTING BEFORE STARTING THE EXECUTABLE PROGRAM              NPL   30
C                                                                       NPL   40
      CHARACTER*1 A(80)                                                 NPL   50
C                                                                       NPL   60
  100 FORMAT (80A1)                                                     NPL   70
  200 FORMAT (10X,8(9X,I1))                                             NPL   80
  201 FORMAT (2X,'NR. ',4X,8('123456789 '))                             NPL   90
  202 FORMAT (1X,5('-'),4X,80('-')//)                                   NPL  100
  203 FORMAT (I6,4X,80A1)                                               NPL  110
  204 FORMAT ('1')                                                      NPL  120
C                                                                       NPL  130
C                                                                       NPL  140
      J = 0                                                             NPL  150
      WRITE (6,204)                                                     NPL  160
      WRITE (6,200) (I,I=1,8)                                           NPL  170
      WRITE (6,201)                                                     NPL  180
      WRITE (6,202)                                                     NPL  190
    1 CONTINUE                                                          NPL  200
      READ (5,100,END=2) (A(I),I=1,80)                                  NPL  210
      J = J + 1                                                         NPL  220
      WRITE (6,203) J,(A(I),I=1,80)                                     NPL  230
      GOTO 1                                                            NPL  240
    2 CONTINUE                                                          NPL  250
      RETURN                                                            NPL  260
      END                                                               NPL  270