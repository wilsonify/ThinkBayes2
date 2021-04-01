      subroutine p_mu_init
C Copyright, Bernd Berg, Nov 10 2000.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include 'muca.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/potts_muca.com'
      iastep=1
      if(nq.eq.2) iastep=2 ! The Ising case has a large stepsize.
c
      do ia=0,mlink
        ha(ia)=zero
        hmuca(ia)=zero
        hup(ia)=zero
        hdn(ia)=zero
        gw(ia)=zero
      end do
      do ia=(mlink+1),(mlink+n2d)
        hmuca(ia)=zero
        hup(ia)=zero
      end do
c
      do ia=namin,namax
C Initial values of ndel_muca() point outside the physical range:
        ndel_muca(ia)=namax+1-ia
      end do
c
      return
      end
