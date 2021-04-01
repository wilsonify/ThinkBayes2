      subroutine parabola(x,y,a,b,c,xext,yext)
C 3-point determination of the parabola y=a*x**2+b*x+c. The parameters 
C         a, b, c  and the extremum (minimum or maximum) abscissa xext 
C         and ordinate yext are returned.
      include 'implicit.sta'
      include 'constants.par'
      dimension x(3),y(3)
      a=( (y(3)-y(2))/(x(3)-x(2))-(y(3)-y(1))/(x(3)-x(1)) ) /
     & ( (x(3)**2-x(2)**2)/(x(3)-x(2))-(x(3)**2-x(1)**2)/(x(3)-x(1)) )
      b=( y(3)-y(1)-a*(x(3)**2-x(1)**2) )/(x(3)-x(1))
      c=y(1)-(a*x(1)+b)*x(1)
C Extrema: 2*a*xext+b=0 =>
      xext=-half*b/a
      yext=xext*(a*xext+b)+c
      return
      end


