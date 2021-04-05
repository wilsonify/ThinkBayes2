      subroutine On_metm(actm1)
C Copyright Bernd Berg, Jan 23 2004.
C O(n) model: Sequential Metropolis updating (inefficient code).
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'on.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/on.com'
      dimension stanew(n)
      amin=act
      amax=act
      actm1=zero
      do is=1,ns
1       sum=zero ! Propose new spin stanew().
          do i=1,n
            stanew(i)=two*(rmafun()-half)
            sum=sum+stanew(i)**2
          end do
        if(sum>one) go to 1 ! stanew() inside the unit sphere.
        dact=zero
        fn=one/sqrt(sum)
        do id=1,nd
          do i=1,n
            dact=dact+sta(i,ipf(id,is))*(fn*stanew(i)-sta(i,is))
            dact=dact+sta(i,ipb(id,is))*(fn*stanew(i)-sta(i,is))
          end do
        end do
        if(rmafun()<exp(beta*dact)) then
          do i=1,n
            sta(i,is)=fn*stanew(i)
          end do
          act=act+dact
          acpt=acpt+one
          amin=min(amin,act)
          amax=max(amax,act)
        end if
        actm1=actm1+act
      end do
      actm1=actm1/ns
      a_min=min(a_min,amin)
      a_max=max(a_max,amax)
      return
      end
