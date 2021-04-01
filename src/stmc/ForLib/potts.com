      common /sites/  ista(ms)
c iamin,iamax:   action minimum and maximum of the MC sweep.      
      common /links/ nlink, ialink(nd,ms),iact,iamin,iamax
      common /delfct/  idel(0:nqm1,0:nqm1)
c acpt:          counts accepted updates. 
      common /weights/ wrat(-n2d:n2d,0:mlink+n2d),acpt
c ia_min,ia_max: action minimum and maximum for a series of sweeps.      
      common /haction/ ha(0:mlink),ia_min,ia_max
