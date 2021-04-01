      subroutine wrat_to_b(n2d,mlink,namin,namax,wrat,ndel_muca,b)
C Copyright Bernd Berg, Nov 27 2000.
C Calculation of the microcanonical inverse temperature
C b(ia)=1/T(ia) from given weight ratio and stepsize arrays. 
      include 'implicit.sta'
      include 'constants.par'
      dimension b(0:mlink),wrat(-n2d:n2d,0:mlink+n2d),ndel_muca(0:mlink)
      do ilink=0,mlink
        b(ilink)=zero
      end do
      ilink=namin
1     continue
        ndel=ndel_muca(ilink)
        b(ilink+ndel)=log(wrat(ndel,ilink))/(two*ndel)
        ilink=ilink+ndel
      if(ilink.lt.namax) go to 1
      return
      end
