###########################################################################################################################################
## (1) Spartio-temporal random effect: additive                                                                                          ##
###########################################################################################################################################
#####################################################################
## (1.1) Spatial random effect: LCAR                               ##
#####################################################################
## Temporal random effect: iid
lcar.iid.ad<- O ~ f(ID.area, model="generic1", Cmatrix=Q_Leroux, constr=TRUE, hyper=list(prec=list(prior=sdunif),beta=list(prior=lunif))) +
                  f(ID.year, model="iid", constr=TRUE, hyper=list(theta=list(prior="loggamma",param=c(1,0.01)))) +
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW1
lcar.rw1.ad<- O ~ f(ID.area, model="generic1", Cmatrix=Q_Leroux, constr=TRUE, hyper=list(prec=list(prior=sdunif),beta=list(prior=lunif))) +
                  f(ID.year, model="rw1", constr=TRUE, hyper=list(prec=list(prior=sdunif))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
lcar.rw2.ad<- O ~ f(ID.area, model="generic1", Cmatrix=Q_Leroux, constr=TRUE, hyper=list(prec=list(prior=sdunif),beta=list(prior=lunif))) +
                  f(ID.year, model="rw2", constr=TRUE, hyper=list(prec=list(prior=sdunif))) + 
                  x1_stand+x5_stand+x6_stand
#####################################################################
## (1.2) Spatial random effect: BYM2                               ##
#####################################################################
## Temporal random effect: iid
bym2.iid.ad<- O ~ f(ID.area, model="bym2", graph=g, scale.model=TRUE, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="iid", constr=TRUE, hyper=list(theta=list(prior="loggamma",param=c(1,0.01)))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW1
bym2.rw1.ad<- O ~ f(ID.area, model="bym2", graph=g, scale.model=TRUE, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw1", scale.model = TRUE, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5)))  + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
bym2.rw2.ad<- O ~ f(ID.area, model="bym2", graph=g, scale.model=TRUE, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw2", scale.model = TRUE, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5)))  + 
                  x1_stand+x5_stand+x6_stand
#####################################################################
## (1.3) Spatial random effect: DCAR                               ##
#####################################################################
## Temporal random effect: iid
dcar.iid.ad<- O ~ f(ID.area, model="bym2", graph=g, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3),initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="iid", constr=TRUE, hyper=list(theta=list(prior="loggamma",param=c(1,0.01)))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW1
dcar.rw1.ad<- O ~ f(ID.area, model="bym2", graph=g, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw1", scale.model = FALSE, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01),initial=5))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
dcar.rw2.ad<- O ~ f(ID.area, model="bym2", graph=g, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw2", scale.model = FALSE, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01),initial=5))) + 
                  x1_stand+x5_stand+x6_stand
#####################################################################
## (1.4) Spatial random effect: ICAR                               ##
#####################################################################
## Temporal random effect: iid
icar.iid.ad<- O ~ f(ID.area, model="besag", graph=g, constr=TRUE, hyper=list(prec=list(prior=sdunif)) ) +
                  f(ID.year, model="iid", constr=TRUE, hyper=list(theta=list(prior="loggamma",param=c(1,0.01))))  + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW1
icar.rw1.ad<- O ~ f(ID.area, model="besag", graph=g, constr=TRUE, hyper=list(prec=list(prior=sdunif)) ) +
                  f(ID.year, model="rw1", constr=TRUE, hyper=list(prec=list(prior=sdunif))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
icar.rw2.ad<- O ~ f(ID.area, model="besag", graph=g, constr=TRUE, hyper=list(prec=list(prior=sdunif)) ) +
                  f(ID.year, model="rw2", constr=TRUE, hyper=list(prec=list(prior=sdunif))) + 
                  x1_stand+x5_stand+x6_stand
###########################################################################################################################################
## (2) Spartio-temporal random effect: Type I                                                                                            ##
###########################################################################################################################################
#####################################################################
## (2.1) Spatial random effect: LCAR                               ##
#####################################################################
## Temporal random effect: iid
lcar.iid.t1<- O ~ f(ID.area, model="generic1", Cmatrix=Q_Leroux, constr=TRUE, hyper=list(prec=list(prior=sdunif),beta=list(prior=lunif))) +
                  f(ID.year, model="iid", constr=TRUE,  hyper=list(theta=list(prior="loggamma",param=c(1,0.01)))) +
                  f(ID.area.year, model="iid", constr=TRUE, hyper=list(prec=list(prior=sdunif))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW1
lcar.rw1.t1<- O ~ f(ID.area, model="generic1", Cmatrix=Q_Leroux, constr=TRUE, hyper=list(prec=list(prior=sdunif),beta=list(prior=lunif))) +
                  f(ID.year, model="rw1", constr=TRUE, hyper=list(prec=list(prior=sdunif))) +
                  f(ID.area.year, model="iid", constr=TRUE, hyper=list(prec=list(prior=sdunif))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
lcar.rw2.t1<- O ~ f(ID.area, model="generic1", Cmatrix=Q_Leroux, constr=TRUE, hyper=list(prec=list(prior=sdunif),beta=list(prior=lunif))) +
                  f(ID.year, model="rw2", constr=TRUE, hyper=list(prec=list(prior=sdunif))) +
                  f(ID.area.year, model="iid", constr=TRUE, hyper=list(prec=list(prior=sdunif)), extraconstr=list(A=matrix(rep(1:t,n),1,n*t),e=0)) + 
                  x1_stand+x5_stand+x6_stand
#####################################################################
## (2.2) Spatial random effect: BYM2                               ##
#####################################################################
## Temporal random effect: iid
bym2.iid.t1<- O ~ f(ID.area, model="bym2", graph=g, scale.model=TRUE, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="iid", constr=TRUE, hyper=list(theta=list(prior="loggamma",param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=id_nt, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5)), extraconstr=list(A=matrix(rep(1:t,n),1,n*t),e=0)) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW1
bym2.rw1.t1<- O ~ f(ID.area, model="bym2", graph=g, scale.model=TRUE, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw1", scale.model = TRUE, constr=TRUE, hyper=list(theta = list(prior="pc.prec", param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=id_nt, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5)), extraconstr=list(A=matrix(rep(1:t,n),1,n*t),e=0)) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
bym2.rw2.t1<- O ~ f(ID.area, model="bym2", graph=g, scale.model=TRUE, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw2", scale.model = TRUE, constr=TRUE, hyper=list(theta = list(prior="pc.prec", param=c(1,0.01)))) + 
                  f(ID.area.year, model="generic0", Cmatrix=id_nt, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5) ), extraconstr=list(A=matrix(rep(1:t,n),1,n*t),e=0)) + 
                  x1_stand+x5_stand+x6_stand
#####################################################################
## (2.3) Spatial random effect: DCAR                               ##
#####################################################################
## Temporal random effect: iid
dcar.iid.t1<- O ~ f(ID.area, model="bym2", graph=g, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="iid", constr=TRUE, hyper=list(theta=list(prior="loggamma",param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=id_nt, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5)), extraconstr=list(A=matrix(rep(1:t,n),1,n*t),e=0)) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW1
dcar.rw1.t1<- O ~ f(ID.area, model="bym2", graph=g, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw1", scale.model = FALSE, constr=TRUE, hyper=list(theta = list(prior="pc.prec", param=c(1,0.01)))) + 
                  f(ID.area.year, model="generic0", Cmatrix=id_nt, constr=TRUE,  hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5) ), extraconstr=list(A=matrix(rep(1:t,n),1,n*t),e=0))  + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
dcar.rw2.t1<- O ~ f(ID.area, model="bym2", graph=g, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw2", scale.model = FALSE, constr=TRUE, hyper=list(theta = list(prior="pc.prec", param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=id_nt, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5) ), extraconstr=list(A=matrix(rep(1:t,n),1,n*t),e=0)) + 
                  x1_stand+x5_stand+x6_stand
#####################################################################
## (2.4) Spatial random effect: ICAR                               ##
#####################################################################
## Temporal random effect: iid
icar.iid.t1<- O ~ f(ID.area, model="besag", graph=g, constr=TRUE, hyper=list(prec=list(prior=sdunif)) ) +
                  f(ID.year, model="iid", constr=TRUE,  hyper=list(theta=list(prior="loggamma",param=c(1,0.01)))) +
                  f(ID.area.year, model="iid", constr=TRUE, hyper=list(prec=list(prior=sdunif))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW1
icar.rw1.t1<- O ~ f(ID.area, model="besag", graph=g, constr=TRUE, hyper=list(prec=list(prior=sdunif)) ) +
                  f(ID.year, model="rw1", constr=TRUE, hyper=list(prec=list(prior=sdunif))) +
                  f(ID.area.year, model="iid", constr=TRUE, hyper=list(prec=list(prior=sdunif))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
icar.rw2.t1<- O ~ f(ID.area, model="besag", graph=g, constr=TRUE, hyper=list(prec=list(prior=sdunif)) ) +
                  f(ID.year, model="rw2", constr=TRUE, hyper=list(prec=list(prior=sdunif))) +
                  f(ID.area.year, model="iid", constr=TRUE, hyper=list(prec=list(prior=sdunif)), extraconstr=list(A=matrix(rep(1:t,n),1,n*t),e=0)) + 
                  x1_stand+x5_stand+x6_stand
###########################################################################################################################################
## (3) Spartio-temporal random effect: Type II                                                                                           ##
###########################################################################################################################################
#####################################################################
## (3.1) Spatial random effect: LCAR                               ##
#####################################################################
## Temporal random effect: RW1
lcar.rw1.t2<- O ~ f(ID.area, model="generic1", Cmatrix=Q_Leroux, constr=TRUE, hyper=list(prec=list(prior=sdunif),beta=list(prior=lunif))) +
                  f(ID.year, model="rw1", constr=TRUE, hyper=list(prec=list(prior=sdunif))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_1_2, rankdef=r_def_1_2, constr=TRUE, hyper=list(prec=list(prior=sdunif)), extraconstr=list(A=A_constr_1_2, e=rep(0,n))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
lcar.rw2.t2<- O ~ f(ID.area, model="generic1", Cmatrix=Q_Leroux, constr=TRUE, hyper=list(prec=list(prior=sdunif),beta=list(prior=lunif))) +
                  f(ID.year, model="rw2", constr=TRUE, hyper=list(prec=list(prior=sdunif)), extraconstr=list(A=matrix(1:t,1,t),e=0)) +
                  f(ID.area.year, model="generic0", Cmatrix=R_2_2, rankdef=r_def_2_2, constr=TRUE, hyper=list(prec=list(prior=sdunif)), extraconstr=list(A=A_constr_2_2, e=rep(0,n))) + 
                  x1_stand+x5_stand+x6_stand
#####################################################################
## (3.2) Spatial random effect: BYM2                               ##
#####################################################################
## Temporal random effect: RW1
bym2.rw1.t2<- O ~ f(ID.area, model="bym2", graph=g, scale.model=TRUE, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw1", scale.model = TRUE, constr=TRUE, hyper=list(theta = list(prior="pc.prec",param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_1_2_scaled, rankdef=r_def_1_2, constr=TRUE, hyper=list(prec=list(prior="pc.prec",param=c(1,0.01),initial=5) ), extraconstr=list(A=A_constr_1_2, e=rep(0,n))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
bym2.rw2.t2<- O ~ f(ID.area, model="bym2", graph=g, scale.model=TRUE, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw2", scale.model = TRUE, constr=TRUE, hyper=list(theta = list(prior="pc.prec", param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_2_2_scaled, rankdef=r_def_2_2, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5) ), extraconstr=list(A=A_constr_2_2, e=rep(0,n))) + 
                  x1_stand+x5_stand+x6_stand
#####################################################################
## (3.3) Spatial random effect: DCAR                               ##
#####################################################################
## Temporal random effect: RW1
dcar.rw1.t2<- O ~ f(ID.area, model="bym2", graph=g, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw1", scale.model = FALSE, constr=TRUE, hyper=list(theta = list(prior="pc.prec", param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_1_2, rankdef=r_def_1_2, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5)), extraconstr=list(A=A_constr_1_2, e=rep(0,n))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
dcar.rw2.t2<- O ~ f(ID.area, model="bym2", graph=g, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw2", scale.model = FALSE, constr=TRUE, hyper=list(theta = list(prior="pc.prec", param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_2_2, rankdef=r_def_2_2, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5) ), extraconstr=list(A=A_constr_2_2, e=rep(0,n))) + 
                  x1_stand+x5_stand+x6_stand
#####################################################################
## (3.4) Spatial random effect: ICAR                               ##
#####################################################################
## Temporal random effect: RW1
icar.rw1.t2<- O ~ f(ID.area, model="besag", graph=g, constr=TRUE, hyper=list(prec=list(prior=sdunif)) ) +
                  f(ID.year, model="rw1", constr=TRUE, hyper=list(prec=list(prior=sdunif))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_1_2, rankdef=r_def_1_2, constr=TRUE, hyper=list(prec=list(prior=sdunif)), extraconstr=list(A=A_constr_1_2, e=rep(0,n))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
icar.rw2.t2<- O ~ f(ID.area, model="besag", graph=g, constr=TRUE, hyper=list(prec=list(prior=sdunif)) ) +
                  f(ID.year, model="rw2", constr=TRUE, hyper=list(prec=list(prior=sdunif)), extraconstr=list(A=matrix(1:t,1,t),e=0)) +
                  f(ID.area.year, model="generic0", Cmatrix=R_2_2, rankdef=r_def_2_2, constr=TRUE, hyper=list(prec=list(prior=sdunif)), extraconstr=list(A=A_constr_2_2, e=rep(0,n))) + 
                  x1_stand+x5_stand+x6_stand
###########################################################################################################################################
## (4) Spartio-temporal random effect: Type III                                                                                          ##
###########################################################################################################################################
#####################################################################
## (4.1) Spatial random effect: LCAR                               ##
#####################################################################
## Temporal random effect: iid
lcar.iid.t3<- O ~ f(ID.area, model="generic1", Cmatrix=Q_Leroux, constr=TRUE, hyper=list(prec=list(prior=sdunif),beta=list(prior=lunif))) +
                  f(ID.year, model="iid", constr=TRUE, hyper=list(theta=list(prior="loggamma",param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_0_3, rankdef=r_def_0_3, constr=TRUE, hyper=list(prec=list(prior=sdunif)), extraconstr=list(A=A_constr_0_3, e=rep(0,t))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW1
lcar.rw1.t3<- O ~ f(ID.area, model="generic1", Cmatrix=Q_Leroux, constr=TRUE, hyper=list(prec=list(prior=sdunif),beta=list(prior=lunif))) +
                  f(ID.year, model="rw1", constr=TRUE, hyper=list(prec=list(prior=sdunif))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_1_3, rankdef=r_def_1_3, constr=TRUE, hyper=list(prec=list(prior=sdunif)), extraconstr=list(A=A_constr_1_3, e=rep(0,t))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
lcar.rw2.t3<- O ~ f(ID.area, model="generic1", Cmatrix=Q_Leroux, constr=TRUE, hyper=list(prec=list(prior=sdunif),beta=list(prior=lunif))) +
                  f(ID.year, model="rw2", constr=TRUE,hyper=list(prec=list(prior=sdunif))) + 
                  f(ID.area.year, model="generic0", Cmatrix=R_2_3, rankdef=r_def_2_3, constr=TRUE, hyper=list(prec=list(prior=sdunif)),extraconstr=list(A=A_constr_2_3, e=rep(0,t))) + 
                  x1_stand+x5_stand+x6_stand
#####################################################################
## (4.2) Spatial random effect: BYM2                               ##
#####################################################################
## Temporal random effect: iid
bym2.iid.t3<- O ~ f(ID.area, model="bym2", graph=g, scale.model=TRUE, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="iid", constr=TRUE, hyper=list(theta=list(prior="loggamma",param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_0_3_scaled, rankdef=r_def_0_3, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5) ), extraconstr=list(A=A_constr_0_3, e=rep(0,t))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW1
bym2.rw1.t3<- O ~ f(ID.area, model="bym2", graph=g, scale.model=TRUE, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw1", scale.model = TRUE, constr=TRUE, hyper=list(theta = list(prior="pc.prec", param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_1_3_scaled, rankdef=r_def_1_3, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5)), extraconstr=list(A=A_constr_1_3, e=rep(0,t))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
bym2.rw2.t3<- O ~ f(ID.area, model="bym2", graph=g, scale.model=TRUE, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw2", scale.model = TRUE, constr=TRUE, hyper=list(theta = list(prior="pc.prec", param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_2_3_scaled, rankdef=r_def_2_3, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5) ), extraconstr=list(A=A_constr_2_3, e=rep(0,t))) + 
                  x1_stand+x5_stand+x6_stand
#####################################################################
## (4.3) Spatial random effect: DCAR                               ##
#####################################################################
## Temporal random effect: iid
dcar.iid.t3<- O ~ f(ID.area, model="bym2", graph=g, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="iid", constr=TRUE, hyper=list(theta=list(prior="loggamma", param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_0_3, rankdef=r_def_0_3, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5)), extraconstr=list(A=A_constr_0_3, e=rep(0,t))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW1
dcar.rw1.t3<- O ~ f(ID.area, model="bym2", graph=g, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw1", scale.model = FALSE, constr=TRUE, hyper=list(theta = list(prior="pc.prec", param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_1_3, rankdef=r_def_1_3, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5) ), extraconstr=list(A=A_constr_1_3, e=rep(0,t))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
dcar.rw2.t3<- O ~ f(ID.area, model="bym2", graph=g, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw2", scale.model = FALSE, constr=TRUE, hyper=list(theta = list(prior="pc.prec", param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_2_3, rankdef=r_def_2_3, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5)), extraconstr=list(A=A_constr_2_3, e=rep(0,t))) + 
                  x1_stand+x5_stand+x6_stand
#####################################################################
## (4.4) Spatial random effect: ICAR                               ##
#####################################################################
## Temporal random effect: iid
icar.iid.t3<- O ~ f(ID.area, model="besag", graph=g, constr=TRUE, hyper=list(prec=list(prior=sdunif)) ) +
                  f(ID.year, model="iid", constr=TRUE, hyper=list(theta=list(prior="loggamma",param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_0_3, rankdef=r_def_0_3, constr=TRUE, hyper=list(prec=list(prior=sdunif)), extraconstr=list(A=A_constr_0_3, e=rep(0,t))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW1
icar.rw1.t3<- O ~ f(ID.area, model="besag", graph=g, constr=TRUE, hyper=list(prec=list(prior=sdunif)) ) +
                  f(ID.year, model="rw1", constr=TRUE, hyper=list(prec=list(prior=sdunif))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_1_3, rankdef=r_def_1_3, constr=TRUE, hyper=list(prec=list(prior=sdunif)), extraconstr=list(A=A_constr_1_3, e=rep(0,t))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
icar.rw2.t3<- O ~ f(ID.area, model="besag", graph=g, constr=TRUE, hyper=list(prec=list(prior=sdunif)) ) +
                  f(ID.year, model="rw2", constr=TRUE,hyper=list(prec=list(prior=sdunif))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_2_3, rankdef=r_def_2_3, constr=TRUE, hyper=list(prec=list(prior=sdunif)),extraconstr=list(A=A_constr_2_3, e=rep(0,t))) + 
                  x1_stand+x5_stand+x6_stand
###########################################################################################################################################
## (5) Spartio-temporal random effect: Type IV                                                                                           ##
###########################################################################################################################################
#####################################################################
## (5.1) Spatial random effect: LCAR                               ##
#####################################################################
## Temporal random effect: RW1
lcar.rw1.t4<- O ~ f(ID.area, model="generic1", Cmatrix=Q_Leroux, constr=TRUE, hyper=list(prec=list(prior=sdunif),beta=list(prior=lunif))) +
                  f(ID.year, model="rw1", constr=TRUE, hyper=list(prec=list(prior=sdunif))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_1_4, rankdef=r_def_1_4,constr=TRUE, hyper=list(prec=list(prior=sdunif)),extraconstr=list(A=A_constr_1_4, e=rep(0,n+t))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
lcar.rw2.t4<- O ~ f(ID.area, model="generic1", Cmatrix=Q_Leroux, constr=TRUE, hyper=list(prec=list(prior=sdunif),beta=list(prior=lunif))) +
                  f(ID.year, model="rw2", constr=TRUE, hyper=list(prec=list(prior=sdunif))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_2_4, rankdef=r_def_2_4, constr=TRUE, hyper=list(prec=list(prior=sdunif)),extraconstr=list(A=A_constr_2_4, e=rep(0,n+t))) + 
                  x1_stand+x5_stand+x6_stand
#####################################################################
## (5.2) Spatial random effect: BYM2                               ##
#####################################################################
## Temporal random effect: RW1
bym2.rw1.t4<- O ~ f(ID.area, model="bym2", graph=g, scale.model=TRUE, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="iid", constr=TRUE, hyper=list(theta=list(prior="loggamma",param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_1_4_scaled, rankdef=r_def_1_4, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5)), extraconstr=list(A=A_constr_1_4, e=rep(0,n+t))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
bym2.rw2.t4<- O ~ f(ID.area, model="bym2", graph=g, scale.model=TRUE, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw2", scale.model = TRUE, constr=TRUE, hyper=list(theta = list(prior="pc.prec", param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_2_4_scaled, rankdef=r_def_2_4, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5) ), extraconstr=list(A=A_constr_2_4, e=rep(0,n+t))) + 
                  x1_stand+x5_stand+x6_stand
#####################################################################
## (5.3) Spatial random effect: DCAR                               ##
#####################################################################
## Temporal random effect: RW1
dcar.rw1.t4<- O ~ f(ID.area, model="bym2", graph=g, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw1", scale.model = FALSE, constr=TRUE, hyper=list(theta = list(prior="pc.prec", param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_1_4, rankdef=r_def_1_4, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5) ), extraconstr=list(A=A_constr_1_4, e=rep(0,n+t))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
dcar.rw2.t4<- O ~ f(ID.area, model="bym2", graph=g, constr=TRUE, hyper=list(phi=list(prior="pc", param=c(0.5,2/3), initial=-3), prec=list(prior="pc.prec", param=c(0.2/0.31,0.01), initial=5))) +
                  f(ID.year, model="rw2", scale.model = FALSE, constr=TRUE, hyper=list(theta = list(prior="pc.prec", param=c(1,0.01)))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_2_4, rankdef=r_def_2_4, constr=TRUE, hyper=list(prec=list(prior="pc.prec", param=c(1,0.01), initial=5)), extraconstr=list(A=A_constr_2_4, e=rep(0,n+t))) + 
                  x1_stand+x5_stand+x6_stand
#####################################################################
## (5.4) Spatial random effect: ICAR                               ##
#####################################################################
## Temporal random effect: RW1
icar.rw1.t4<- O ~ f(ID.area, model="besag", graph=g, constr=TRUE, hyper=list(prec=list(prior=sdunif)) ) +
                  f(ID.year, model="rw1", constr=TRUE, hyper=list(prec=list(prior=sdunif))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_1_4, rankdef=r_def_1_4,constr=TRUE, hyper=list(prec=list(prior=sdunif)),extraconstr=list(A=A_constr_1_4, e=rep(0,n+t))) + 
                  x1_stand+x5_stand+x6_stand

## Temporal random effect: RW2
icar.rw2.t4<- O ~ f(ID.area, model="besag", graph=g, constr=TRUE, hyper=list(prec=list(prior="loggamma")) ) +
                  f(ID.year, model="rw2", constr=TRUE, hyper=list(prec=list(prior=sdunif))) +
                  f(ID.area.year, model="generic0", Cmatrix=R_2_4, rankdef=r_def_2_4, constr=TRUE, hyper=list(prec=list(prior=sdunif)),extraconstr=list(A=A_constr_2_4, e=rep(0,n+t))) + 
                  x1_stand+x5_stand+x6_stand
###########################################################################################################################################
## Formulas list                                                                                                                         ##
###########################################################################################################################################
## without covariates
formulas<-c(lcar.iid.ad=lcar.iid.ad, lcar.rw1.ad=lcar.rw1.ad, lcar.rw2.ad=lcar.rw2.ad, bym2.iid.ad=bym2.iid.ad, bym2.rw1.ad=bym2.rw1.ad, bym2.rw2.ad=bym2.rw2.ad, dcar.iid.ad=dcar.iid.ad, dcar.rw1.ad=dcar.rw1.ad, dcar.rw2.ad=dcar.rw2.ad, icar.iid.ad=icar.iid.ad, icar.rw1.ad=icar.rw1.ad, icar.rw2.ad=icar.rw2.ad,
            lcar.iid.t1=lcar.iid.t1, lcar.rw1.t1=lcar.rw1.t1, lcar.rw2.t1=lcar.rw2.t1, bym2.iid.t1=bym2.iid.t1, bym2.rw1.t1=bym2.rw1.t1, bym2.rw2.t1=bym2.rw2.t1, dcar.iid.t1=dcar.iid.t1, dcar.rw1.t1=dcar.rw1.t1, dcar.rw2.t1=dcar.rw2.t1, icar.iid.t1=icar.iid.t1, icar.rw1.t1=icar.rw1.t1, icar.rw2.t1=icar.rw2.t1,
            lcar.rw1.t2=lcar.rw1.t2, lcar.rw2.t2=lcar.rw2.t2, bym2.rw1.t2=bym2.rw1.t2, bym2.rw2.t2=bym2.rw2.t2, dcar.rw1.t2=dcar.rw1.t2, dcar.rw2.t2=dcar.rw2.t2, icar.rw1.t2=icar.rw1.t2, icar.rw2.t2=icar.rw2.t2,
            lcar.iid.t3=lcar.iid.t3, lcar.rw1.t3=lcar.rw1.t3, lcar.rw2.t3=lcar.rw2.t3, bym2.iid.t3=bym2.iid.t3, bym2.rw1.t3=bym2.rw1.t3, bym2.rw2.t3=bym2.rw2.t3, dcar.iid.t3=dcar.iid.t3, dcar.rw1.t3=dcar.rw1.t3, dcar.rw2.t3=dcar.rw2.t3, icar.iid.t3=icar.iid.t3, icar.rw1.t3=icar.rw1.t3, icar.rw2.t3=icar.rw2.t3,
            lcar.rw1.t4=lcar.rw1.t4, lcar.rw2.t4=lcar.rw2.t4, bym2.rw1.t4=bym2.rw1.t4, bym2.rw2.t4=bym2.rw2.t4, dcar.rw1.t4=dcar.rw1.t4, dcar.rw2.t4=dcar.rw2.t4, icar.rw1.t4=icar.rw1.t4, icar.rw2.t4=icar.rw2.t4
            )
## rm
rm(list = c("lcar.iid.ad","lcar.rw1.ad","lcar.rw2.ad", "bym2.iid.ad","bym2.rw1.ad","bym2.rw2.ad", "dcar.iid.ad","dcar.rw1.ad","dcar.rw2.ad", "icar.iid.ad","icar.rw1.ad","icar.rw2.ad",
            "lcar.iid.t1","lcar.rw1.t1","lcar.rw2.t1", "bym2.iid.t1","bym2.rw1.t1","bym2.rw2.t1", "dcar.iid.t1","dcar.rw1.t1","dcar.rw2.t1", "icar.iid.t1","icar.rw1.t1","icar.rw2.t1",
            "lcar.rw1.t2","lcar.rw2.t2", "bym2.rw1.t2","bym2.rw2.t2", "dcar.rw1.t2","dcar.rw2.t2", "icar.rw1.t2","icar.rw2.t2",
            "lcar.iid.t3","lcar.rw1.t3","lcar.rw2.t3", "bym2.iid.t3","bym2.rw1.t3","bym2.rw2.t3", "dcar.iid.t3","dcar.rw1.t3", "dcar.rw2.t3", "icar.iid.t3","icar.rw1.t3","icar.rw2.t3",
            "lcar.rw1.t4","lcar.rw2.t4", "bym2.rw1.t4","bym2.rw2.t4", "dcar.rw1.t4","dcar.rw2.t4", "icar.rw1.t4","icar.rw2.t4"))
###########################################################################################################################################
###########################################################################################################################################