      program lat4d
C Copyrioght, Bernd Berg, Nov 12 2000.
C Bookkeeping for a 4d hypercubic lattice. Example tuned to 2**4.
C Before compiling: copy lat4d.par to lat.par and lat4d.dat to lat.dat.
      include '../../ForLib/implicit.sta'
      parameter(iuo=6)
      include 'lat.par'
      include '../../ForLib/lat.com'
      include 'lat.dat'
      dimension ix(nd)
C
      call lat_init(ns,nd,ipf,ipb,nla,ix,nlink) ! lattice
      if(ns<99) write(iuo,*)  "  "
      if(ns<99) write(iuo,*)  " is   ix:  1 2 3 4",
     &     "   ipf: 1   2   3   4   ipb: 1   2   3   4  "
      do is=1,ns
      call ixcor(ix,nla,is,nd)
      if(ns<99) write(iuo,'(1I4,7X,4I2,5X,4I4,5X,4I4)')
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
