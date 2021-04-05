      program ranmar_test
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      parameter(iuo=6,iud=10,iseed1=1,iseed2=0,ndat=10 000,nrpt=2)
c     parameter(iuo=6,iud=10,iseed1=1,iseed2=0,ndat=10 000,nrpt=1 000)
      dimension a(ndat),b(ndat),Q(nrpt)
c
      call rmaset(-iuo,iud,iseed1,iseed2,'no_file.d')
      do irpt=1,nrpt
        do idat=1,ndat
          call ranmar(a(idat))
          call ranmar(b(idat))
        end do
        call heapsort(ndat,a)
        call heapsort(ndat,b)
        call kolm2_as2(ndat,ndat,a,b,DEL,Q(irpt))
     
      end do
c
      call heapsort(nrpt,Q)
      open(iud,file="kolm2_as2.d",form="formatted",status="unknown")
      do irpt=1,nrpt
        x=(irpt*one)/(nrpt+one)
        peaked=Q(irpt)
        if(peaked>half) peaked=one-peaked
        write(iud,'(3F14.6)') x,Q(irpt),peaked
      end do
      close(iud)
      print*,"Plot with  gnuplot kolm2_as2.plt."
c
      stop
      end

      include '../../ForLib/heapsort.f'
      include '../../ForLib/kolm2_as2.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmasave.f'
