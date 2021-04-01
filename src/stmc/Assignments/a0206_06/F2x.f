      function F2x(y)
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      if(y.le.zero) then
        F2y=zero
      return
      end if
      if(y.le.one) then
        F2x=half*y**2
      return
      end if
      if(y.lt.two) then
        F2x=half*y*(-y+four)-one
      return
      end if
      F2x=one
      return
      end
