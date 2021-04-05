      subroutine XY_met1
C Copyright Bernd Berg, Feb 10 2002.
C XY model: Sequential Metropolis updating. Single angle variable.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/xy1.com'
      amin=act
      amax=act
      do is=1,ns
        staold=sta(is)
        stanew=tpi*rmafun()
        dact=zero
        do id=1,nd
          staf=sta(ipf(id,is))
          dact=dact+cos(abs(staf-stanew))-cos(abs(staf-staold))
          stab=sta(ipb(id,is))
          dact=dact+cos(abs(stab-stanew))-cos(abs(stab-staold))
        end do
        if(dact>=zero .or. rmafun()<exp(beta*dact)) then
          act=act+dact
          acpt=acpt+one
          amin=min(amin,act)
          sta(is)=stanew
        end if
      end do
      a_min=min(a_min,amin)
      a_max=max(a_max,amax)
      return
      end
