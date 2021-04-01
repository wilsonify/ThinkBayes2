      program lat3d
C Copyright, Bernd Berg, Nov 10 2000.
C Bookkeeping for a 3d hypercubic lattice. Example tuned to 2x2x4. 
C Before compiling: copy lat3d.par to lat.par and lat3d.dat to lat.dat.
      include '../../ForLib/implicit.sta'
      parameter(iuo=6)
      include 'lat.par'
      include '../../ForLib/lat.com'
      include 'lat.dat'
      dimension ix(nd)
C
      write(iuo,*) " "
      write(iuo,'("  Lattice size:  ",2(I1,"x"),I1)') nla
      write(iuo,*) " "
      write(iuo,*) " is   ix(1) (2) (3)",
     &     "   ipf(is,1) (,2) (,3)   ipb(is,1) (,2) (,3)"
      call lat_init(ns,nd,ipf,ipb,nla,ix,nlink) ! lattice
      do is=1,ns
      call ixcor(ix,nla,is,nd)
      write(iuo,'(1I4,3X,3I4,7X,3I5,7X,3I5)') 
     &      is,ix,(ipf(id,is),id=1,nd),(ipb(id,is),id=1,nd)
      end do
C
      stop
      end

      include '../../ForLib/ipointer.f'
      include '../../ForLib/isfun.f'
      include '../../ForLib/ixcor.f'
      include '../../ForLib/lat_init.f'
      include '../../ForLib/nsfun.f'
