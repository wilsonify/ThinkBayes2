      program ran02_test
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      parameter(iuo=6,iud=10,iseed1=1,iseed2=0,ndat=10 000,nrpt=8)
      dimension a(ndat),b(ndat),Q(nrpt)
c
      do irpt=1,nrpt
        isd2=iseed2-1+irpt
        call rmaset(-iuo,iud,iseed1,isd2,'no_file.d')
        do idat=1,ndat
          call ranmar(a(idat))
        end do
        isd2=iseed2+irpt
        call rmaset(-iuo,iud,iseed1,isd2,'no_file.d')
        do idat=1,ndat
          call ranmar(b(idat))
        end do
        call heapsort(ndat,a)
        call heapsort(ndat,b)
        call kolm2_as2(ndat,ndat,a,b,DEL,Q(irpt))
        print*,"irpt,Q:",irpt,Q(irpt)
      end do
c
      stop
      end

      include '../../ForLib/heapsort.f'
      include '../../ForLib/kolm2_as2.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmasave.f'
