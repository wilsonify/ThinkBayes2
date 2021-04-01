      program lat222
C Copyright, Bernd Berg, Nov 8 2000.
C Bookkeeping for a hypercubic 2x2x2 lattice (lat3d.par and lat3d.dat).
      include '../../ForLib/implicit.sta'
      parameter(iuo=6)
      include 'lat3d.par'
      include '../../ForLib/lat.com'
      include 'lat222.dat'
      dimension ix(nd)
C
      write(iuo,*)  " is   ix(1) (2) (3)",
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
