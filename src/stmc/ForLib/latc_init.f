      subroutine latc_init(ns,nsc,nd,nla,ias,iac,iasc,icpf,icpb,nlink)
C Copyright, Bernd Berg, Sep 26 2003.
C Checkerboard lattice: Initialize number of lattice sites and links.
C                       Initialize pointer arrays.
      dimension ias(2,nsc),iac(ns),iasc(ns)
      dimension icpf(nd,2,nsc),icpb(nd,2,nsc),nla(nd),ix(nd)
      nlink=nd*ns
C
      isc1=0
      isc2=0
      do is=1,ns
        call ixcor(ix,nla,is,nd)
        ix_sum=0
        do id=1,nd
          ix_sum=ix_sum+ix(id)
        end do
        ic=1+mod(ix_sum,2)
        if(ic==1) then
          isc1=isc1+1
          isct=isc1
          ias(ic,isc1)=is
          iac(is)=1
          iasc(is)=isc1
        else
          isc2=isc2+1
          isct=isc2
          ias(ic,isc2)=is
          iac(is)=2
          iasc(is)=isc2
        endif
      end do
C
C Initialize pointers:
      do is=1,ns
        call icpointer(ns,nsc,nd,ias,iac,iasc,icpf,icpb,nla,ix,is)
      end do
c
      return
      end 
