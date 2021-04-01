      program isort_test
C BB Feb 14 2003.
C Test of the isort routines.
      include '../../ForLib/implicit.sta'
      character cnp*2
      include '../../ForLib/constants.par'
      parameter(iuo=6,iud0=10,m=9,mdat=3000,lper=.true.)
      dimension adat(mdat),iadat(mdat),iper(mdat),iiper(mdat)
      ltest=.true.
      ltest=.false.
      if(ltest) stop "ts_cor: ltest."

      call read_d(iuo,iud0,mdat,ndat,iadat)
      write(iuo,'(" mdat,ndat:",2I6)') mdat,ndat

      do idat=1,ndat
        adat(idat)=one*iadat(idat)
c         print'(" jdat,sort:",I9,F18.2)',jdat,sort(jdat)
      end do
      
      if(lper) then
        call heap_per(ndat,adat,iper)
        call heap_iper(ndat,iadat,iiper)
      else
        call heapsort(ndat,adat)
        call heapisort(ndat,iadat)
      end if

      do idat=1,ndat
        if(nint(adat(idat)).ne.iadat(idat)) stop "Error!"
        if(lper.and.iper(idat).ne.iiper(idat)) stop "Error iper!"
      end do
      print'(/,"  isort_test: completed with agreement.")'

      stop
      end

      include '../../ForLib/heap_per.f'
      include '../../ForLib/heap_iper.f'
      include '../../ForLib/heapisort.f'
      include '../../ForLib/heapsort.f'


      subroutine read_d(iuo,iud0,mdat,ndat,iadat)
C BB Feb 14 2003.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      parameter(iseed1=1,iseed2=0)
      dimension iadat(mdat),prob(4)
      data prob/4*zero/

C Read data:
      open(iud0,file='ts1.dat',form='formatted',status='old')
      read(iud0,*) ndat
      write(iuo,'(" ndat =",I10,/)') ndat
      do idat=1,ndat
        read(iud0,*) iadat(idat)
        if(iadat(idat).eq.-1) iadat(idat)=3
        if(iadat(idat).eq.-2) iadat(idat)=4
        prob(iadat(idat))= prob(iadat(idat))+one
      end do
      write(iuo,'(" prob:",4G16.8)') (prob(i),i=1,4)
      close(iud0)

      prob(1)=prob(1)/ndat
      do i=2,4
        prob(i)=prob(i-1)+prob(i)/ndat
      end do
      write(iuo,'(" prob:",4G16.8)') (prob(i),i=1,4)

      return
      end
