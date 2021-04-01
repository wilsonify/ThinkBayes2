      program sphere_test
C Copyright, Bernd Berg, Jan 13 2002. MC estimate of the volume occupied 
C     by the nd-dimensional unit sphere in the nd-dimensional hypercube.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      parameter(nrpt=10 000)
C
      call rmaset(-6,10,1,1,'no_file')
      do nd=2,20,2
        p_in=zero
        do irpt=1,nrpt ! nrpt repititions of nmeas measurement sweeps.
          r2=zero
          do id=1,nd
            r2=r2+rmafun()**2
          end do
          if(r2.lt.one) p_in=p_in+one
        end do
        p_in=p_in/nrpt
        print*,"nd,p_in:",nd,p_in
      end do
C
      stop
      end

      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmafun.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/rmasave.f'
