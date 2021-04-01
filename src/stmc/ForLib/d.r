4c4
< C Potts model, normalization of the partition function Z at beta0=0.
---
> C Potts model, normalization of the partition function Z at beta=0.
10,11c10
<       iloop_ha=0 ! Counts runs through the ha_iact.gt.half loop.
<       beta0=zero
---
>       beta=zero
13a13
>       ilink_min=nlink
15,21c15,23
<       do iact=0,nlink
<         ha_iact=hasum(iact)-ha(iact) ! Jackknife histogram.
<         if(ha_iact.gt.half) then
<           iloop_ha=iloop_ha+1
<           if(iloop_ha.eq.1) then
<             a=-(beta0-b(iact))*namin ! Gets Ferdinand-Fisher normalization.
<             Zln=log(ha_iact)+two*((beta0-b(iact))*iact+a)
---
>       do ilink=0,nlink
>         ha_ilink=hasum(ilink)-ha(ilink) ! Jackknife histogram.
>         if(ha_ilink.gt.half) then
>           iact=ilink-namin
>           if(ilink_min.gt.ilink) then
>             ilink_min=ilink
>             a=zero
>             Zln=log(ha_ilink)+two*(beta-b(ilink))*iact
>             Zln1_max=Zln
23,27c25,30
<             a=a+(b(iact)-b(iact_old))*iact_old
<             if(iact.gt.namin) then
<               if(iact.ne.iact_next) then
<                 print'(" iact,_next,has:",2I10,2G15.6)',
<      &                   iact,iact_next,hasum(iact),ha(iact)
---
>             if(ilink.eq.namin) a=b(namin)*iact_old
>             if(ilink.gt.namin) then
>               a=a+(b(ilink)-b(ilink_old))*iact_old
>               if(ilink.ne.iact_next) then
>                 print'(" ilink,_next,has:",2I10,2G15.6)',
>      &                   ilink,iact_next,hasum(ilink),ha(ilink)
29c32
<                 if(iact.eq.nlink) stop "POTTS_Z0LN."
---
>                 if(ilink.eq.nlink) stop "POTTS_Z0LN."
31c34
<               iact_next=iact+ndel_muca(iact)
---
>               iact_next=ilink+ndel_muca(ilink)
33c36,37
<             Zln1=log(ha_iact)+two*((beta0-b(iact))*iact+a)
---
>             Zln1=log(ha_ilink)+two*((beta-b(ilink))*iact+a)
>             Zln1_max=max(Zln1_max,Zln1)
34a39,40
>             iact_old=iact
>             ilink_old=ilink
36d41
<           iact_old=iact
