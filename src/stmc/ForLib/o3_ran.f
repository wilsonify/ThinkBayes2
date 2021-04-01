      subroutine O3_ran(sta,ns)
C Copyright, Bernd Berg, Jan 13 2002.
C Assigns random (i.e. beta=0) values to the states sta(3,is).
      include 'implicit.sta'
      include 'constants.par'
      dimension sta(3,ns)
      do is=1,ns
        z=two*(rmafun()-half)  ! z=cos(theta) uniform in (-1,1).
        sta(3,is)=z
        rho=sqrt(one-z**2)
        phi=tpi*rmafun()
        sta(2,is)=rho*sin(phi) ! y=sin(theta)*sin(phi).
        sta(1,is)=rho*cos(phi) ! x=sin(theta)*cos(phi).
      end do
      return
      end
