      program lat3d_ckb
C Copyright, Bernd Berg, Sep 26 2003.
C Bookkeeping for a 2*2*4 checkerboard lattice.
      include '../../ForLib/implicit.sta'
      parameter(iuo=6)
      include 'latc.par'
      include '../../ForLib/latc.com'
      include 'lat.dat'
      dimension ix(nd)
c
      ns=nsfun(nla,nd)
      nsc=ns/2
      write(iuo,*) " "
      write(iuo,*) "Lattice size: 2x2x4"
      write(iuo,*) " "
      write(iuo,*) "ns,nsc,nd:",ns,nsc,nd
      write(iuo,*) " "
      call latc_init(ns,nsc,nd,nla,ias,iac,iasc,icpf,icpb,nlink) 
C
      write(iuo,*) " "
      write(iuo,*) " is    ic  isc   ",
     & " icpf(1,isc,ic) (2,) (3,)   icpb(1,isc,ic) (2,) (3,)"
      do is=1,ns
        ic=iac(is)
        isc=iasc(is)
        write(iuo,'(I4,I6,I4,2I11,I5,2I11,I5)') is,ic,isc,
     &       (icpf(id,ic,isc),id=1,nd),(icpb(id,ic,isc),id=1,nd)
      end do
C
      write(iuo,*) " "
      write(iuo,*) "Consistency check:"
      write(iuo,*) " "
      write(iuo,*) " is   ipf(1,is) (2,) (3,)   ipb(1,is) (2,) (3,)"
c
      do is=1,ns
        ic=iac(is)
        isc=iasc(is)
        do id=1,nd
          icpf(id,ic,isc)=ias(1+mod(ic,2),icpf(id,ic,isc))
          icpb(id,ic,isc)=ias(1+mod(ic,2),icpb(id,ic,isc))
        end do
        write(iuo,'(I4,1I8,I7,I5,I10,I7,I5)') is,
     &    (icpf(id,ic,isc),id=1,nd),(icpb(id,ic,isc),id=1,nd)
      end do
C
      stop
      end

      include '../../ForLib/icpointer.f'
      include '../../ForLib/isfun.f'
      include '../../ForLib/ixcor.f'
      include '../../ForLib/latc_init.f'
      include '../../ForLib/nsfun.f'
