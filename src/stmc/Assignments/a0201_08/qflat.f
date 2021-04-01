      program QFLAT
C
C Copyright, Berg, June 14, 1999.
C Generate 2*10000 Gaussian random numbers, let the difference
C between the mean expectation values be 0,1,2, ....
C Perform each time the Gaussian difference test and histogram
C the resulting Q-distribution.
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
c     parameter(del_mean=ZERO)
      parameter(del_mean=FOUR)
      parameter(iuo=8,iud=10,iseed1=1,iseed2=0,ndat=10000,nhist=10)
      dimension Qdat(ndat),hist(nhist)
C
      call rmaset(-iuo,iud,iseed1,iseed2,'ranmar.d')
      EB1=ONE
      EB2=ONE
      do idat=1,ndat
        call rmagau(x1,x2)
        x2=x2+del_mean
        CALL GAUDIF(X1,EB1,X2,EB2,Q)
        Qdat(idat)=Q
      end do
      print*,'data generated.'
      CALL HIST_GNU(-IUD,NHIST,NDAT,HIST,Qdat,ZERO,ONE)
c
      stop
      end

      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmagau.f'
      include '../../ForLib/gaudif.f'
      include '../../ForLib/error_f.f'
      include '../../ForLib/gamma_p.f'
      include '../../ForLib/gamma_ln.f'
      include '../../ForLib/hist_gnu.f'

