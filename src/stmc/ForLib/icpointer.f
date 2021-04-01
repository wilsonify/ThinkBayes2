      subroutine icpointer(ns,nsc,nd,ias,iac,iasc,icpf,icpb,nla,ix,is)
C Copyright, Bernd Berg, Sep 26 2003.
C Input:   is = number of lattice site
C Output:  icpf,icpb: forward and backward checkerboard pointers.
      dimension ias(2,nsc),iac(ns),iasc(ns)
      dimension icpf(nd,2,nsc),icpb(nd,2,nsc),nla(nd),ix(nd)
C
      ic=iac(is)
      isc=iasc(is)
C
      do id=1,nd
        call ixcor(ix,nla,is,nd)
C Forward pointer (periodic bounday conditions):
        ix(id)=mod(ix(id)+1,nla(id)) 
        ipf=isfun(ix,nla,nd)
        iscf=iasc(ipf)
        icpf(id,ic,isc)=iscf
      end do
C
      do id=1,nd
        call ixcor(ix,nla,is,nd)
C Backward pointer (periodic boundary conditions):
        ix(id)=mod(ix(id)-1+nla(id),nla(id))
        ipb=isfun(ix,nla,nd)
        iscb=iasc(ipb)
        icpb(id,ic,isc)=iscb
      end do
C
      return
      end
