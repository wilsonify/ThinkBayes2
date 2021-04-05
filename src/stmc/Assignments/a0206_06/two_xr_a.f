      program two_xr_a
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      parameter(iuo=6,iud=10,iseed1=1,iseed2=0,ndat=1000,nrpt=1000)
      dimension a(ndat),Q(1000)
c
      call rmaset(-iuo,iud,iseed1,iseed2,'no_file.d')
      do irpt=1,nrpt
        do idat=1,ndat
          call ranmar(xr1)
          call ranmar(xr2)
          a(idat)=(xr1+xr2)
        end do
        call heapsort(ndat,a)
        do idat=1,ndat
          a(idat)=F2x(a(idat))
        end do
        call kolm2_as(ndat,a,DEL,Q(irpt))
        if(irpt==1) write(iuo,'(" irpt,Q:",I6,1F9.4)') irpt,Q(irpt)
      end do
      call heapsort(nrpt,Q)
      open(iud,file="k2_as_a.d",form="formatted",status="unknown")
      do irpt=1,nrpt
        x=(irpt*one)/(nrpt+one)
        peaked=Q(irpt)
        if(peaked>half) peaked=one-peaked
        write(iud,'(3F14.6)') x,Q(irpt),peaked
      end do
      close(iud)
c
      do irpt=1,nrpt
        a(irpt)=F1x(Q(irpt))
      end do
      call kolm2_as(ndat,a,DEL,QQ)
      write(iuo,'(" Q for the Q distribution =",1F9.4)') QQ
c
      stop
      end

      include 'F2x.f'
      include 'F1x.f'
      include '../../ForLib/heapsort.f'
      include '../../ForLib/kolm2_as.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmasave.f'
