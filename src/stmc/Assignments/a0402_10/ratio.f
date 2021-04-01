      program ratio
C Copyright, Bernd Berg, June 14 2002.
C Improvement ratios due to one-sweep averaging.
      include '../../ForLib/implicit.sta'
      parameter(iuo=6)

c O3 at beta=1.1:
      tau_int_s =2.968d00
      tau_int_se=0.057d00
      sigma0_s =0.23253D-04 
      sigma0_se=0.00064D-04
      sigma_s =tau_int_s*sigma0_s
      tau_int_a =3.738d00
      tau_int_ae=0.069d00
      sigma0_a =0.18433d-04
      sigma0_ae=0.00066d-04
      sigma_a =tau_int_a*sigma0_a
      write(iuo,'(/," beta=1.1 sigma_s,sigma_a,ratio:",2G16.7,F8.3)')
     &                         sigma_s,sigma_a,(sigma_s/sigma_a)

c O3 at beta=1.5:
      tau_int_s=13.09d00 
      tau_int_se=0.44d00
      sigma0_s =0.18562E-04
      sigma0_se=0.00052D-04
      sigma_s =tau_int_s*sigma0_s
      tau_int_a=15.95d00
      tau_int_ae=0.54d00
      sigma0_a =0.15226d-04
      sigma0_ae=0.00054d-04
      sigma_a =tau_int_a*sigma0_a
      write(iuo,'(/," beta=1.5 sigma_s,sigma_a,ratio:",2G16.7,F8.3)')
     &                          sigma_s,sigma_a,(sigma_s/sigma_a)

      stop
      end

      
