      subroutine ixcor(ix,nla,is,nd) ! Jun 18, 2000.
C Input:   is = number of lattice site
c Output:  ix   array of nd coordinates of this site
      dimension ix(nd),nla(nd)
      ns=nla(1)
      do id=2,nd
        ns=ns*nla(id)
      end do
      nspart=ns
      js=is
      do id=nd,1,-1
        if(id.lt.nd) js=js-ix(id+1)*nspart
        nspart=nspart/nla(id)
        ix(id)=(js-1)/nspart
      end do
      return
      end
