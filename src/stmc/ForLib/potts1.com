      common /sites/  ista(ms)
c iamin,iamax:   action minimum, maximum of the MC sweep.      
      common /links/   nlink, ialink(nd,ms),iact,iamin,iamax
      common /delfct/  idel(0:nqm1,0:nqm1)
c acpt:          counts accepted updates. 
      common /weights/ wrat(-2*nd:2*nd),acpt
c ia_min,ia_max: action minimum, maximum for a number of sweeps.      
      common /haction/ ha(0:mlink),ia_min,ia_max
