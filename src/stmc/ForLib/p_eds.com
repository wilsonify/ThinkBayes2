C Tables and variables for event driven simulations:
      common /etabs/ qmove(mcase),Peds1(0:n2d,mcase),Pstay,Pmove,
     & Pcase(mcase),Pcsum(mcase),NAcase(mcase),IAcase(nconf),
     & IScase(ms),Index(ms,mcap1),ia_array(mcase),Icase_per(mcase)
