      PROGRAM MON_UC
C
C SAMPLING FOR UNIFORM AND CAUCHY MOMENTS.
C  
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER (KMAX=10,IUOUT=6)
      DIMENSION XMO(4),XMOM(4)
C
      CALL RMASET(-1,-1,1,0,'rmarin.d')
      WRITE(IUOUT,*) ' '
      WRITE(IUOUT,*) 'MOMENTS 1-4 FOR UNIFORM RANDOM NUMBERS AND 1-2',
     &                           ' FOR CAUCHY RANDOM NUMERS:'
      WRITE(IUOUT,*) ' '
      WRITE(IUOUT,*) ' K    NDAT       U1       U2       U3       U4 ',
     &                           '       C1          C2'
      WRITE(IUOUT,*) ' '
      DO K=1,KMAX
        NDAT=2**(2*K-1)
        XMCAU=ZERO
        X2CAU=ZERO
        DO I=1,4
          XMOM(I)=ZERO
        END DO
C
        DO N=1,NDAT
          CALL RANMAR(XR)
          XMCAU=XMCAU+TAN(TPI*XR)
          X2CAU=X2CAU+TAN(TPI*XR)**2
          XMO(1)=XR
          XMOM(1)=XMOM(1)+XR
          DO I=2,4
            XMO(I)=XMO(I-1)*XR
            XMOM(I)=XMOM(I)+XMO(I)
          END DO
        END DO
C
C NORMALIZATION:
        XMCAU=XMCAU/(NDAT*ONE)
        X2CAU=X2CAU/(NDAT*ONE)
        DO I=1,4
          XMOM(I)=XMOM(I)/(NDAT*ONE) 
        END DO
        WRITE(IUOUT,100) K,NDAT,(XMOM(I),I=1,4),XMCAU,X2CAU
      END DO
100   FORMAT(1X,1I2,I8,4F9.4,1F10.4,1F12.1)
C
      STOP
      END

      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
