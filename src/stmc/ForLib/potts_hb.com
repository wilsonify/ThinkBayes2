      common /sites/  ista(ms)
c iamin,iamax:   action minimum, maximum of the sweep.      
      common /links/   nlink, ialink(nd,ms),iact,iamin,iamax
      common /delfct/  idel(0:nqm1,0:nqm1)
      common /weights/ whb_tab(0:n2d,0:nqm1),achg
c ia_min,ia_max: action minimum, maximum for a number of sweeps.      
      common /haction/ ha(0:mlink),ia_min,ia_max
      common /hmag/ hm(0:ms,0:nqm1),nstate(0:nqm1)
