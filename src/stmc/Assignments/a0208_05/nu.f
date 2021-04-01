      program nu
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      c2=-1.598126d0
      xnu=-one/c2
      c2p=c2+0.0030305d0
      c2m=c2-0.0030305d0
      xnum=-one/c2m
      xnup=-one/c2p
      ebp=xnup-xnu
      ebm=xnu-xnum
      write(6,'(" nu,ebp,ebm:",3F14.6)') xnu,ebp,ebm
      stop
      end
