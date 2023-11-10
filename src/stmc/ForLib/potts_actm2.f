      FUNCTION potts_actm2(iamin,iamax,ha)
C Copyright, Bernd Berg, Apr 17 2002.
C Calculation of the mean action variable from its histogram.
C Works for the mean of any histogram.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      dimension ha(iamin:iamax)
      potts_actm=zero
      hsum=zero
      do ia=iamin,iamax
        potts_actm=potts_actm+ia*ha(ia)
        hsum=hsum+ha(ia)
      end do
      potts_actm2=potts_actm/hsum ! Average.
      return
      end
