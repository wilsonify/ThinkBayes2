      function F1x(y)
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      if(y.le.zero) then
        F1y=zero
      return
      end if
      if(y.le.one) then
        F1x=y
      return
      end if
      F1x=one
      return
      end
