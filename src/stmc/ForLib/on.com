      common /sites/  sta(n,ms)
c amin,amax:    action  act  minimum and maximum of the MC sweep.      
c acpt:         counts accepted updates. 
      common /links/ alink(nd,ms),act,amin,amax,acpt,nlink
c tsa: time series action array.
c a_min,a_max:  action minimum and maximum for a series of sweeps.      
      common /ts_action/ tsa(nmeas),a_min,a_max
