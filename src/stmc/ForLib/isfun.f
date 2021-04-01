      function isfun(ix,nla,nd)
C Copyright, Bernd Berg, June 10, 1999.
c Input:   coordinates ix(nd), ix(id)=0,...,nla(id)-1.
c Output:  isfun = number of the site (function of ix,nd)
      dimension ix(nd),nla(nd)
      isfun=1
      nsa=1
      do id=1,nd
      isfun=isfun+ix(id)*nsa
      nsa=nsa*nla(id)
      end do
      return
      end
