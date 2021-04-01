      subroutine O3_mchbm(actm1)
C Copyright Bernd Berg, June 9 2002.
C O3 model: Sequential heat bath updating.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      parameter(n=3,eps=one/ten**10)
      include 'lat.par'
      include 'mc.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/on.com'
      amin=act
      amax=act
      if(ns.le.0 .or. ns.gt.ms) stop "O3_mchb: ns false."

      actm1=zero
      do is=1,ns

        sum1=sta(1,ipf(1,is))+sta(1,ipb(1,is))
        sum2=sta(2,ipf(1,is))+sta(2,ipb(1,is))
        sum3=sta(3,ipf(1,is))+sta(3,ipb(1,is))
        do id=2,nd
          sum1=sum1+sta(1,ipf(id,is))+sta(1,ipb(id,is))
          sum2=sum2+sta(2,ipf(id,is))+sta(2,ipb(id,is))
          sum3=sum3+sta(3,ipf(id,is))+sta(3,ipb(id,is))
        end do

        dact_in=sta(1,is)*sum1+sta(2,is)*sum2+sta(3,is)*sum3
        Snorm=sqrt(sum1**2+sum2**2+sum3**2)
        phi=tpi*rmafun()
        call ranmar(xr)
        rho=zero
        if(Snorm.lt.eps) then
          print*,"O3_mchb: Warning Snorm.lt.eps encountered."
          sta(3,is)=two*(xr-half) ! Occasionally xr=zero => sta(3,is)=-one.
          if(abs(sta(3,is)).lt.one) rho=sqrt(one-sta(3,is)**2)
          sta(1,is)=rho*cos(phi)
          sta(2,is)=rho*sin(phi)
        else
C Heat bath choice of the new spin:
          Sbeta=beta*Snorm
          Sexp=exp(Sbeta)
          snew3=log(Sexp+xr*(exp(-Sbeta)-Sexp))/Sbeta ! xr=zero => snew3=one.
          if(snew3.lt.one) rho=sqrt(one-snew3**2)
          snew1=rho*cos(phi)
          snew2=rho*sin(phi)
          fn=one/Snorm
          zh1=fn*sum1
          zh2=fn*sum2
          zh3=fn*sum3 ! = cos_theta

          if(abs(one-zh3**2).lt.eps) then
            print*,"O3_mchb: Warning |one-zh3**2|.lt.eps encountered."
            xh1=zh3
            xh2=zero
            xh3=zero
            yh1=zero
            yh2=abs(zh3)
          else ! 
            rho=sqrt(one-zh3**2) ! = sin_theta 
            if(rho.lt.eps) print*,
     &        "O3_mchb: Warning rho.lt.eps encountered."
            yh1=-zh2/rho         ! =-sin_phi
            yh2=+zh1/rho         ! = cos_phi; yh3 no needed.
            xh1=+zh3*yh2         ! = cos_theta*cos_phi
            xh2=-zh3*yh1         ! =-cos_theta*(-sin_phi)
            xh3=-rho             ! =-sin+theta
          endif

C Final storage:
          sta(1,is)=snew1*xh1+snew2*yh1+snew3*zh1
          sta(2,is)=snew1*xh2+snew2*yh2+snew3*zh2
          sta(3,is)=snew1*xh3           +snew3*zh3
        end if

        act=act+sta(1,is)*sum1+sta(2,is)*sum2+sta(3,is)*sum3-dact_in
        amin=min(amin,act)
        amax=max(amax,act)

      actm1=actm1+act
      end do
      actm1=actm1/ns

      a_min=min(a_min,amin)
      a_max=max(a_max,amax)
      return
      end
