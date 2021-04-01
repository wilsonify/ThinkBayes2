      PROGRAM MOM_G
C
C Copyright, Bernd Berg, April 2, 2000.
C MOMENTS FOR NORMALLY DISTRIBUTED RANDOM NUMBERS.
C  
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER (IUO=6,KMAX=10)
      DIMENSION XMO(5),XMOM(5)
C
      CALL RMASET(-1,-1,1,0,'ranmar.d')
      WRITE(IUO,*) ' '
      WRITE(IUO,*) ' CALCULATION OF NORMAL GAUSSIAN MOMENTS',
     &             ' 1, 2, 4, 6 AND 8:'
      WRITE(IUO,*) ' '
      WRITE(IUO,*) ' K    NDAT      G1         G2        ',
     &            'G4        G6        G8'
      WRITE(IUO,*) ' '
      DO K=1,KMAX
        NDAT=2**(2*K-1)
        DO I=1,5
          XMOM(I)=ZERO
        END DO
C
        XMO(1)=ONE
        DO N=1,(NDAT/2)
          CALL RMAGAU(XR,YR)
          XMOM(1)=XMOM(1)+XR+YR
          DO I=2,5
            XMO(I)=XMO(I-1)*XR**2
            XMOM(I)=XMOM(I)+XMO(I)
          END DO
          DO I=2,5
            XMO(I)=XMO(I-1)*YR**2
            XMOM(I)=XMOM(I)+XMO(I)
          END DO
        END DO
C
C NORMALIZATION:
        DO I=1,5
          XMOM(I)=XMOM(I)/NDAT 
        END DO
        WRITE(IUO,100) K,NDAT,(XMOM(I),I=1,5)
      END DO
100   FORMAT(1X,I2,I8,1G12.4,1F7.4,3F10.4)
C
C EXACT MOMENTS:
      XMOM(1)=ZERO
      DO I=2,5
        XN=TWO*(I-1)
        G=EXP(GAMMA_LN(HALF*XN+HALF))
        XMOM(I)=TWO**(HALF*XN)*G/SQRT(PI)
      END DO
      WRITE(IUO,101) (XMOM(I),I=1,5)
101   FORMAT(3X,'  EXACT:',1G12.4,1F7.4,3F10.4)
C
      STOP
      END

      INCLUDE '../../ForLib/gamma_ln.f'
      INCLUDE '../../ForLib/rmaset.f'
      INCLUDE '../../ForLib/ranmar.f'
      INCLUDE '../../ForLib/rmagau.f'

