      FUNCTION potts_actm(nlink,ha)
C Copyright, Bernd Berg, Dec 12 2000.
C Calculation of the mean action variable from its histogram.
C Works for the mean of any histogram.
      include 'implicit.sta'
      dimension ha(0:nlink)
      potts_actm=0
      hsum=0
      do ilink=0,nlink
        potts_actm=potts_actm+ilink*ha(ilink)
        hsum=hsum+ha(ilink)
      end do
      potts_actm=potts_actm/(hsum*nlink) ! iact per link.
      return
      end
