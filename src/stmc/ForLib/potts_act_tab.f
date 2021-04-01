      subroutine potts_act_tab(idel,nqm1)
      dimension idel(0:nqm1,0:nqm1)
C Initialize delta function table for Action per Link:
      do ip1=0,nqm1
        do ip2=0,nqm1
        idel(ip1,ip2)=0
        end do
      idel(ip1,ip1)=1
      end do
      return
      end
