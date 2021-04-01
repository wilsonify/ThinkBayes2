      program marsaglia
C
C Bernd Berg, June 2 1999.
C IMPLEMENTATION OF MARSAGLIA'S RANDOM NUMBER GENERATOR.
C (MARSAGLIA, ZAMAN AND TSANG, STAT.& PROB. LETTERS 8 (1990) 35.)
C - The seed parameters are integers, -1801 <= iseed1 <= 29527,
C   -9373 <= iseed2 <= 20708. Distinct pairs give independent sequences.
C - When ranmar.d does not exist, iseed1=1 and iseed2=0 reproduces 
C   table 4 of the Marsaglia et al. paper. 
C - Whenever the file ranmar.d exists: Continuation using the seeds
C   saved by RMASAVE is ensured. Compare the last number of the previous 
C   run with the first of the continuation!
C
      include '../../ForLib/implicit.sta'
      parameter(iuo=6,iud=10,iseed1=1,iseed2=0,ndat=004)
c
      inquire(file='ranmar.d',exist=lexist)
      if(.not.lexist .and. iseed1.eq.1 .and. iseed2.eq.0) then
        write(iuo,*) '  '
        call rmaset(iuo,iud,iseed1,iseed2,'ranmar.d')
        write(iuo,*) 'Table of Marsaglia et al.:'
        DO II=1,20005
          CALL RANMAR(XR)
          IF(II.GT.20000)
     &      WRITE(IUO,'(2X,7I3)') (MOD(INT(XR*16.**I),16),I=1,7)
        END DO
      end if
C
      write(iuo,*) '  '
      call rmaset(iuo,iud,iseed1,iseed2,'ranmar.d')
      do idat=1,ndat
        call ranmar(xr)
        write(iuo,*) 'idat, xr =',idat,xr
      end do
c
      call rmasave(iud,'ranmar.d')
      call ranmar(xr)
      write(iuo,*) 'extra xr =  ',xr
c
      stop
      end

      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmasave.f'
