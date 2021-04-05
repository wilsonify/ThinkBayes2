      program ran_range
C Copyright Bernd Berg, Feb 17 2002. Purpose: To investigate the
C extreme values (the range) of the Marsaglia random number generator.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character*3 c1,c2
      parameter(iuo=6,iud=10,iseed1=1,iseed2=0,xitmax=ten**10-half)
      parameter(xsmall=zero)
      ltest=.false.
      ltest=.true.
      write(iuo,'(/," Stepsize of Marsaglia rnadom numbers: 2**(-24)")')
      write(iuo,'(/," Note: 2**24 =",I12)') 2**24
      if(ltest) stop "ran_range: ltest."
      call rmaset(iuo,iud,iseed1,iseed2,'ranmar.d')
      xiter=zero
      xiter0=zero
      xiter1=zero
      xlarge=half
      write(c1,'(I3.3)') iseed1
      write(c2,'(I3.3)') iseed2
      open(iud,file='r'//c1//"_"//c2//'.d',
     &     form='formatted',status='unknown')
      write(iud,'(/," iseed1,iseed2:",2I4)') iseed1,iseed2
      write(iud,'(/," xiter, xr and xr at xiter+1:",/)')
1     xiter=xiter+one
        call ranmar(xr)
        if(xr>xlarge) then
          xlarge=xr
          xiter1=zero
        end if
        if(xr==xsmall .or. xr==xlarge) then
          x=xr
          call ranmar(xr)
          if(x<half) then
            xiter0=xiter0+one
            write(iud,'(F6.0,F14.0,2G22.16)') xiter0,xiter,x,xr
          else
            xiter1=xiter1+one
            write(iud,'(F6.0,F14.0,2G22.16)') xiter1,xiter,x,xr
          end if
          xiter=xiter+one
        end if 
      if(xiter<xitmax) go to 1
      write(iud,'(/," xiter = ",F14.0," done.")') xiter
      write(iud,'(" xsmall,xlarge:",2G22.16)') xsmall,xlarge
      xl_log2=log(one-xlarge)/log(two)
      write(iud,'(" xiter0,xiter1,log2(xlarge):",2F7.0,1F10.3)') 
     &              xiter0,xiter1,xl_log2
      stop 
      end

      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
