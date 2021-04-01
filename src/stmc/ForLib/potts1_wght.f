      subroutine potts1_wght(wrat,beta,nd)
C Copyright, Bernd Berg, Sep 15 2000.
C Initialize ratios of weights for MUCA or canonical Metropolis:
      include 'implicit.sta'
      include 'constants.par'
      dimension wrat(-2*nd:2*nd)
      do id=-2*nd,2*nd
        wrat(id)=exp(two*beta*id)
      end do
      return
      end
