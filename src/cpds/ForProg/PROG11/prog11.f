        PROGRAM PROG11
C       THE PROGRAM DETERMINES A RELATIONSHIP BETWEEN TWO VARIABLES
C       i.e. ONE DEPENDENT AND ONE INDEPENDENT VARIABLE. THE PROGRAM
C       SELECTS THE CURVE OF BEST FIT BY TESTING THE DATA AGA1NST
C       EQUATIONS OF THE FORM: Y = a + b.F(x)
C       THE PROGRAM GIVES THE CONSTANTS FOR THE EQUATION AND THE CORRELATION COEFFICIENT.

C       THIS PROGRAM WILL CORRELATE X - Y DATA FOR THE FOLLOWING EQUATIONS:
C
C       Y = a + b.X
C       Y = a + b.X^2
C       Y = a + b.X^0.5
C       Y = a.exp(b.X)
C       Y = a + b.ln(X)
C       Y = a.X^b
C       Y = a + b.(1 / X)
C       1 / Y = a + b.(1 / X)

C       X  =  VECTOR OF INDEPENDENT VARIABLE
C       Y  =  VECTOR OF DEPENDENT VARIABLE
C       YCAL  =  VECTOR OF THE ESTIMATED DEPENDENT
C       N  =  NUMBER OF DATA
C       A , B  =  CONSTANTS FOR THE EQUATION
C       COR  =  CORRELATION COEFFICIENT

        DIMENSION X(1:50), Y(1:50)
        DIMENSION YCAL1(1:50), YCAL2(1:50), YCAL3(1:50), YCAL4(1:50)
        DIMENSION YCAL5(1:50), YCAL6(1:50), YCAL7(1:50), YCAL8(1:50)

        COMMON / DATA / N, X, Y
        COMMON / DATA1 / YCAL1, A1, B1
        COMMON / DATA2 / YCAL2, A2, B2
        COMMON / DATA3 / YCAL3, A3, B3
        COMMON / DATA4 / YCAL4, A4, B4
        COMMON / DATA5 / YCAL5, A5, B5
        COMMON / DATA6 / YCAL6, A6, B6
        COMMON / DATA7 / YCAL7, A7, B7
        COMMON / DATA8 / YCAL8, A8, B8

        INTEGER N
        INTEGER J

        OPEN(UNIT = 3, FILE = 'DATA11.dat', STATUS = 'OLD' , ERR = 18)

        OPEN(UNIT = 1, FILE = 'OUTPUT.txt')

        READ(3,  * , ERR = 19) N
        READ(3,  * , ERR = 19) (X(I) , Y(I) , I = 1, N) 

        GO TO 2
18      WRITE(1, 111) 
111     FORMAT(//, 6X, 'FILE DOES NOT EXIST')
        GO TO 999

19      WRITE (1, 222)
222     FORMAT(//, 6X, 'ERROR MESSAGE IN THE DATA VALUE')
        GO TO 999

2       WRITE(1, 100)
100     FORMAT(//, 'CURVE FITTING FOR TWO VARIABLES')

        WRITE(1, 110) 
110     FORMAT(//, 1HO, 25X, 'X', 15X, 'Y')

        DO I = 1, N

        WRITE(1, 120) I, X(I) , Y(I) 
120     FORMAT(1H0, 4X, I4, 6X, F14.3, 6X, E14.6) 
        end do

        WRITE(1, 125) 
125     FORMAT(//, 1H, 25X, "")

        WRITE(1, 130)
130     FORMAT(//, 1H, 25X, 'THE RESULTS ARE:')

        WRITE(1, 140)
140     FORMAT(//, 1H, 'EQUATION REGRESSION COEFFICIENTS CORRELATION')

        
        WRITE(1, 150)
150     FORMAT(//, 1H, 'INTECEPT: A  SLOPE: B COEFFICIENT')


        J = 1
4       GO TO (5, 15, 25, 35, 45, 55, 65, 75) , J

C       CURVE FITTING FOR : Y = A + BX

5       CALL LINFIT(X, Y, N, A1, B1, COR1) 
        
        WRITE(1, 160) A1, B1, COR1
160     FORMAT(2X, '1.', 5X, 'Y = A + BX', 10X, 3(E10.4, 5X) ) 
        
        DO I = 1, N
        YCAL1(I)  = A1 + B1 * X(I) 
        end do


        J = J + 1
        IF (J == 2)  THEN
            GO TO 4
        ELSE
        
        GO TO 800
        ENDIF

C       CURVE FITTING FOR Y = A + BX^2
        
15      DO I = 1, N
        X(I)  = X(I)  * X(I) 
        end do
        CALL LINFIT(X, Y, N, A2, B2, COR2) 
        
        WRITE( l , 170)  A2, B2, COR2
170     FORMAT (2X, '2.', 5X, 'Y = A + B * X^2', 7X, 3(E10.4, 5X) ) 
        
        DO I = 1, N
        YCAL2(I)  = A2 + B2 * X(I)  ** 2
        end do
 
        J = J + 1
        IF (J == 3)  THEN
        GO TO 4
        ELSE
        GO TO 800
        ENDIF

C       CURVE FITTING FOR Y = A + B.X^0.5

25      DO I  =  1 , N
        X(I)  = X(I)  ** 0.5
        end do
        CALL LINFIT(X, Y, N, A3, B3, COR3) 

        WRITE( 1 , 180)  A3, B3, COR3
180     FORMAT(2X, '3.', 5X, 'Y = A + B * X^0.5', 5X, 3(E10.4, 5X) ) 

        DO I  =  1 , N
        YCAL3(I)  = A3 + B3 * X(I)  ** 0.5
        end do

        J = J + 1
        IF (J == 4)  THEN
            GO TO 4
            ELSE
            GO TO 800
        ENDIF

C       CURVE FITTING FOR Y = A.EXP(B.X) 
C       LINEARIZE TO Ln(Y)  = Ln(A)  + BX

35      DO I  =  1 , N
        Y(I)  = ALOG(Y(I) ) 
        end do
        
        CALL LINFIT(X, Y, N, A4, B4, COR4) 
        A4 = EXP(A4) 

        WRITE(1, 190)  A4, B4, COR4
190     FORMAT(2X, '4.', 5X, 'Y = A * EXP(B * X) ', 4X, 3(E10.4, 5X) ) 

        DO I = 1, N
        Y(I)  = EXP(Y(I) ) 
        YCAL4(I)  = A4 * EXP(B4 * X(I) ) 
        end do

        J = J + 1
        IF (J == 5)  THEN
            GO TO 4
            ELSE
            GO TO 800
        ENDIF

C       CURVE FITTING FOR Y = A + B.Ln(X) 

45      DO I = 1, N
        X(I)  = ALOG(X(I) ) 
        end do
        CALL LINFIT(X, Y, N, A5, B5, COR5) 

        WRITE(1, 200) A5, B5, COR5
200     FORMAT(2X, '5.', 5X, 'Y = A + B * Ln(X) ', 5X, 3(E10.4, 5X) ) 

        DO I = 1, N
        X(I)  = EXP(X(I) ) 
        YCAL5(I)  = A5 + B5 * ALOG(X(I) ) 
        end do
        
        J = J + 1
        IF (J == 6)  THEN
            GO TO 4
            ELSE
            GO TO 800
        ENDIF

C       CURVE FITTING FOR Y = A.X^B
C       LINEARIZE TO Ln(Y)  = Ln(A)  + B * Ln(X) 

55      DO I = 1, N
        X(I)  = ALOG(X (I) ) 
        Y(I)  = ALOG(Y(I) ) 
        end do
        
        CALL LINFIT(X, Y, N, A6, B6, COR6) 
        A6 = EXP(A6) 
        
        WRITE( 1 , 210 )  A6 , B6, COR6
210     FORMAT(2X, '6.', 5X, 'Y = A * X^B', 9X, 3(E10.4, 5X) ) 
        DO I = 1, N
        X(I)  = EXP(X(I) ) 
        Y(I)  = EXP(Y(I) ) 
        YCAL6 (I)  = A6 * (X(I)  ** B6) 
        end do

        J = J + 1
        IF (J  ==  7)  THEN
            GO TO 4
            ELSE
            GO TO 800
        ENDIF

C       CURVE FITTING FOR Y  =  A  +  B . I  /  X

65      DO I = 1, N
        X(I)  = 1 / X(I) 
        end do
        CALL LINFIT(X, Y, N, A7, B7, COR7) 
        
        WRITE( 1 , 220 )  A7 , B7, COR7
220     FORMAT(2X, '7.', 5X, 'Y = A + B * 1 / X', 7X, 3(E10.4, 5X) ) 
        
        DO I  =  1 , N
        X(I)  = 1 / X(I) 
        YCAL7 (I)  = A7 + B7 * 1 / X(I) 
        end do

        J = J + 1
        IF (J == 8)  THEN
            GO TO 4
            ELSE
            GO TO 800
        ENDIF
        
C       CURVE FITTING FOR 1 / Y = A + B.1 / X

75      DO I = 1, N
        X(I)  = 1 / X(I) 
        Y(I)  = 1 / Y(I) 
        end do

        CALL LINFIT(X, Y, N, A8, B8, COR8) 

        WRITE( 1 , 230) A8, B8, COR8
230     FORMAT(2X, '8.', 5X, '1 / Y = A + B * 1 / X', 5X, 3(E10.4, 5X) ) 



        DO I = 1, N
        X(I)  = 1 / X(I) 
        Y(I)  = 1 / Y(I) 
        YCAL8(I)  = X(I)  / (A8 * X(I)  + B8) 
        end do

800     CALL COMP(X, Y, N, COR1, COR2, COR3, COR4, COR5, COR6,
     + COR7, COR8)

        CLOSE(3, STATUS  =  'KEEP' ) 
        CLOSE(1) 
999     STOP
        END

C       THIS PROGRAM PERFORMS A LINEAR REGRESSION OF X  -  Y VALUES .

        SUBROUTINE LINFIT(X, Y, N, A , B , COR ) 
        DIMENSION X(1:50) , Y(1:50) 
        INTEGER N

        SUMX = 0.0
        SUMY = 0.0
        SUMXY = 0.0
        SUMX2 = 0.0
        SUMY2 = 0.0
        
        DO I  =  1 , N
        XI = X(I) 
        YI = Y(I) 
        SUMX = SUMX + XI
        SUMY = SUMY + YI
        SUMXY = SUMXY + XI * YI
        SUMX2 = SUMX2 + XI * XI
        SUMY2 = SUMY2 + YI * YI
        end do

        SXX = N * SUMX2 - SUMX * SUMX
        SXY = N * SUMXY - SUMX * SUMY
        SYY = N * SUMY2 - SUMY * SUMY

C       CALCULATE THE CONSTANTS FOR THE EQUATION

        B = SXY / SXX
        XMEAN = SUMX / N
        YMEAN = SUMY / N

C       THIS PRINTS THE RESULTS ON THE SCREEN.
        WRITE( * , 50) XMEAN, YMEAN
50      FORMAT(6X, 'XMEAN:', F12.3, 3X, 'YMEAN:', F12.4) 
        
        A = YMEAN - B * XMEAN

C       CALCULATE THE CORRELATION COEFFICIENT

        COR = ABS( SXY / SQRT(SXX * SYY)  ) 
        WRITE( * , 60) A, B, COR
60      FORMAT('A = ', F12.3, 'B = ', F12.3, 'COR = ', F12.4)

        RETURN
        END

C       THIS PROGRAM COMPARES THE VALUES OF THE CORRELATION COEFFICIENT OF THE REGRESSION ANALYSES
        
        SUBROUTINE COMP(X, Y, N, COR1, COR2, COR3, COR4, COR5, COR6,
     + COR7, COR8)
        DIMENSION X(1:50) , Y(1:50) 
        DIMENSION YCAL1(1:50), YCAL2(1:50), YCAL3(1:50), YCAL4(1:50)
        DIMENSION YCAL5(1:50), YCAL6(1:50), YCAL7(1:50), YCAL8(1:50)
        INTEGER N

        COMMON / DATA1 / YCAL1, A1, B1
        COMMON / DATA2 / YCAL2, A2, B2
        COMMON / DATA3 / YCAL3, A3, B3
        COMMON / DATA4 / YCAL4, A4, B4
        COMMON / DATA5 / YCAL5, A5, B5
        COMMON / DATA6 / YCAL6, A6, B6
        COMMON / DATA7 / YCAL7, A7, B7
        COMMON / DATA8 / YCAL8, A8, B8

C       FIND THE MAXIMUM VALUE OF THE CORRELATION COEFFICIENTS
        CORR = AMAX1(COR1, COR2, COR3, COR4, COR5, COR6, COR7, COR8) 

        WRITE(1, 100) CORR
100     FORMAT(//, 2X, 'MAXIMUM CORRELATION REGRESSION ANALYSES.
     + COEFFICIENT:', F12.4)
        
C       COMPARE THIS VALUE WITH THE CALCULATED VALUES OF THE REGRESSION ANALYSES

        IF (CORR == COR1)  THEN
            CALL OUTPUT ( X , Y, YCAL1, N, A1 , B1 , COR1 ) 
            GO TO 10
        ELSEIF(CORR == COR2)  THEN

        CALL OUTPUT(X, Y, YCAL2, N, A2, B2, COR2) 
        GO TO 10
        ELSEIF(CORR == COR3)  THEN
        CALL OUTPUT ( X , Y, YCAL3, N, A3, B3, COR3) 
        GO TO 10
        ELSEIF(CORR == COR4)  THEN
        CALL OUTPUT (X, Y, YCAL4, N, A4, B4, COR4) 
        GO TO 10
        ELSEIF(CORR == CORS)  THEN
        CALL OUTPUT ( X , Y, YCAL5, N, A5, B5, COR5) 
        GO TO 10
        ELSEIF(CORR == COR6)  THEN
        CALL OUTPUT ( X , Y, YCAL6, N, A6, B6, COR6) 
        GO TO 10
        ELSEIF (CORR == COR7)  THEN
        CALL OUTPUT (X, Y, YCAL7, N, A7, B7, COR7) 
        GO TO 10
        ELSEIF (CORR == COR8)  THEN
        CALL OUTPUT(X, Y, YCAL8, N, AS, BS, COR8)
        ENDIF
10      RETURN
        END

C THIS PROGRAM OUTPUTS THE RESULTS OF THE REGRESSION ANALYSIS
C WITH THE ESTIMATED VALUE OF THE DEPENDENT VARIABLE, 
C THE CONSTANTS AND THE CORRELATION COEFFICIENT.

        SUBROUTINE OUTPUT (X, Y, YCAL, N, A, B, COR) 
        DIMENSION X(1:50) , Y(1:50) , YCAL(1:50) 
        INTEGER N

        WRITE(1, 100) 
100     FORMAT(/, 1H0, 21X, 'X-ACTUAL', 13X, 'Y-ACTUAL', 11X,
     + 'Y-ESTIMATE')
        DO I  =  1, N

        WRITE(1, 110)  I, X(I) , Y(I) , YCAL(I) 
110     FORMAT(1H0, 4X, I4, 6X, F14.3, 2(6X, F14.6) ) 
        end do

        WRITE (1, 120)
120     FORMAT (//, 1H, '')

        WRITE (1, 130) A, B, COR
130     FORMAT(//, 1H0, 6X, 'CONSTANTS FOR THE EQUATION:', /1H0,
     + 6X, 'A:', 6X, F12.4, /1H0, 6X, 'B:', 6X, F12.4, /1H0,
     + 'CORRELATION COEFFICIENT:', F12.4)

        RETURN
        END