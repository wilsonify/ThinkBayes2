      SUBROUTINE write_bak(cbak,irec,mu_swp,na_swp,ntun,ltun0,
     &                     iequi, irpt,imeas,ntu_m)
      include '../../ForLib/implicit.sta'
      character*(*) cbak
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include 'muca.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/potts_muca.com'
      call rmasave(iud1,cbak)
      open(iud1,file=cbak,form="unformatted",status="old",
     &          access="append")
      write(iud1) wrat, ha,acpt,  hmuca,hup,hdn, gw,
     & ista,nlink,iact,iamin,iamax, ia_min,ia_max, ndel_muca,iastep,
     & irec,mu_swp,na_swp,ntun,ltun0, iequi,irpt,imeas,ntu_m
      close(iud1)
C
      open(iud1,file="progress.d",form="formatted",status="unknown")
      write(iud1,'(/," nb_swp =",I10,6X,"Last backup with:")') nb_swp
      write(iud1,'(" irec1, mu_swp,   ntun  =",3I12)') irec,mu_swp,ntun
      write(iud1,'(" iequ1,irp1,imea1,ntu_m =",4I12,/)')
     &           (iequi+1),irpt,(imeas+1),ntu_m
      close(iud1)
      return
      end
