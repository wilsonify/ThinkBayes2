      program addln_test
C
C Bernd Berg, August 20, 2003.
C Illustrates and tests addln.f and addln2.f using random numbers.
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      parameter(iuo=6,iud=10,iseed1=1,iseed2=0,npri=14,ndat=14)
c
      call rmaset(iuo,iud,iseed1,iseed2,'no_file')
      write(iuo,'(/," npri,ndat =",2i10)') npri,ndat
c
      write(iuo,'(/," addln2.f:",/)')
      c=rmafun()
      do idat=1,ndat
        a=c
        b=rmafun()
        c=a+b
        aln=log(a)
        bln=log(b)
        cln=addln_cut(aln,bln)
c       cln=addln(aln,bln)
        cc=exp(cln)
        if(idat<=npri)
     &  write(iuo,'(" idat,a,b,c,c:",i5,4f12.6)') idat,a,b,c,cc
      end do
      write(iuo,'(" Last c,c:",2g16.7)') c,cc
c
      write(iuo,'(/," addln2.f:",/)')
      c=rmafun()
      do idat=1,ndat
        if(c>zero) then
          a=c
          b=rmafun()
          isgn=-1
        else
          a=-c
          b=rmafun()
          isgn=+1
        end if
        c=a+isgn*b
        aln=log(a)
        bln=log(b)
        call addln2(aln,bln,cln,isgn)
        cc=isgn*exp(cln)
        if(idat<=npri)
     &  write(iuo,'(" idat,a,b,c,c:",i5,4f12.6)') idat,a,b,c,cc
      end do
      write(iuo,'(" Last c,c:",2g16.7)') c,cc
c
      stop
      end

      include '../../ForLib/addln.f'
      include '../../ForLib/addln_cut.f'
      include '../../ForLib/addln2.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmafun.f'
