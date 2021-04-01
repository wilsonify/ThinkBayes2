      subroutine potts_wght(wrat,beta,nlink,nd)
C Copyright, Bernd Berg, Nov 24 2000.
C Initialize ratios of weights for MUCA or canonical Metropolis:
      include 'implicit.sta'
      include 'constants.par'
      dimension wrat(-2*nd:2*nd,0:nlink)
      do id=-2*nd,2*nd
        do ilink=0,nlink
          wrat(id,ilink)=exp(two*beta*id)
        end do
      end do
      return
      end
