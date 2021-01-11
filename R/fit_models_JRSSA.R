##########################################################################################
## Title: Crime against women in India: unveiling spatial patterns and temporal trends  ##
##        of dowry deaths in the districs of Uttar Pradesh                              ##
##                                                                                      ##
## Authors: Vicente, G. - Goicoa, T. - Fernandez-Rasines, P. - Ugarte, M.D.             ##
##                                                                                      ##
## https://doi.org/10.1111/rssa.12545                                                   ##
##                                                                                      ##
##########################################################################################
rm(list = ls())

## Load library
library(spdep); library(INLA); library(tmap)

## Working directory
dir.main<-""              # Set an appropiate directory
setwd(dir.main)

## Load data and cartographies
load("data_JRSSA.RData")

##################################
## Data organization
##################################
## Expected number of cases and Standardized mortality ratio (SMR)
data$exp<- data$pop_linear * (sum(data$obs)/sum(data$pop_linear))
data$smr<- data$obs/data$exp

## Identifiers: "ID_area", "ID_year" and "ID_area_year"
ID<- data.frame(dist=carto_up$dist, ID_area=carto_up$ID_area)
data<- merge(data, ID, by=c("dist"))
data <- data[order(data$year, data$ID_area),]; rownames(data)<-NULL
data$ID_year<- rep(1:length(unique(data$year)), each=length(unique(data$ID_area)))
data$ID_area_year<-seq(1,length(unique(data$ID_year))*length(unique(data$ID_area)))

## Number of areas and number of time periods
n<- length(unique(data$ID_area))
t<- length(unique(data$ID_year))

## Initial and final time periods
t.from<- min(data$year)
t.to<- max(data$year)

## Covariates standardization
for(i in c(1,5,6)){ eval(parse(text= paste0("data$x",i,"_stand<- scale(data$x",i,")" ))) }

## Spatial neighborhood matrix (Q_{xi})
spdep::nb2INLA("uttar_pradesh_nb.graph", spdep::poly2nb(carto_up))
g <- INLA::inla.read.graph("uttar_pradesh_nb.graph")
Q_xi <- matrix(0, g$n, g$n)
for (i in 1:g$n){
  Q_xi[i,i]=g$nnbs[[i]]
  Q_xi[i,g$nbs[[i]]]=-1
}

## Structure matrix to implement the LCAR prior.
Q_Leroux <- diag(n)-Q_xi

## Temporal structure matrix for a RW1 prior
D1 <- diff(diag(t), differences=1)
Q_gammaRW1 <- t(D1)%*%D1

## Temporal structure matrix for a RW2 prior
D2 <- diff(diag(t),differences=2)
Q_gammaRW2 <- t(D2)%*%D2

##########################################################################################
## Fitting models: log(p_{it}) = eta + xi_{i} + gamma_{t} + delta_{it} + beta*x         ##
## mu_{it}= n_{it} * p_{it}                                                             ## 
##########################################################################################
## Define appropriate hyperprior distributions (for LCAR)
sdunif="expression:
logdens=-log_precision/2;
return(logdens)"

lunif = "expression:
a = 1;
b = 1;
beta = exp(theta)/(1+exp(theta));
logdens = lgamma(a+b)-lgamma(a)-lgamma(b)+(a-1)*log(beta)+(b-1)*log(1-beta);
log_jacobian = log(beta*(1-beta));
return(logdens+log_jacobian)"

## Define appropriate constraint matrices
## (a) Spatio-temporal random effect: Type I 
id_nt<-diag(1,nrow=n*t)

## (b) Spatio-temporal random effect: Type II
  ## (b.1) Temporal random effect: RW1
R_1_2 <- kronecker(Q_gammaRW1,diag(n)); r_def_1_2 <- n;   A_constr_1_2 <- kronecker(matrix(1,1,t),diag(n)) # LCAR, DCAR, ICAR
A_constr_1_2 <- A_constr_1_2[-1,]
R_1_2_scaled<- R_1_2*exp(mean(log(diag(INLA:::inla.ginv(R_1_2)))))                                         # BYM2
  ## (b.2) Temporal random effect: RW2
R_2_2 <- kronecker(Q_gammaRW2,diag(n)); r_def_2_2 <- 2*n; A_constr_2_2 <- kronecker(matrix(1,1,t),diag(n)) # LCAR, DCAR, ICAR
A_constr_2_2 <- A_constr_2_2[-1,]
R_2_2_scaled<- R_2_2*exp(mean(log(diag(INLA:::inla.ginv(R_2_2)))))                                         # BYM2

## (c) Spatio-temporal random effect: Type III
  ## (c.1) Temporal random effect: iid
R_0_3 <- kronecker(diag(t),Q_xi); r_def_0_3 <- t; A_constr_0_3 <- kronecker(diag(t),matrix(1,1,n))   # LCAR, DCAR, ICAR
A_constr_0_3 <- A_constr_0_3[-1,]
R_0_3_scaled<- R_0_3*exp(mean(log(diag(INLA:::inla.ginv(R_0_3)))))                                   # BYM2
  ## (c.2) Temporal random effect: RW1
R_1_3 <- kronecker(diag(t),Q_xi); r_def_1_3 <- t; A_constr_1_3 <- kronecker(diag(t),matrix(1,1,n))   # LCAR, DCAR, ICAR
A_constr_1_3 <- A_constr_1_3[-1,]
R_1_3_scaled<- R_1_3*exp(mean(log(diag(INLA:::inla.ginv(R_1_3)))))                                   # BYM2
  ## (c.3) Temporal random effect: RW2
R_2_3 <- kronecker(diag(t),Q_xi); r_def_2_3 <- t; A_constr_2_3 <- kronecker(diag(t),matrix(1,1,n))   # LCAR, DCAR, ICAR
A_constr_2_3 <- A_constr_2_3[-1,]
R_2_3_scaled<- R_2_3*exp(mean(log(diag(INLA:::inla.ginv(R_2_3)))))                                   # BYM2  

## (d) Spatio-temporal random effect: Type IV
  ## (d.1) Temporal random effect: RW1
R_1_4 <- kronecker(Q_gammaRW1,Q_xi); r_def_1_4 <- n+t-1;   A.1.1 <- kronecker(matrix(1,1,t),diag(n)); A.1.2 <- kronecker(diag(t),matrix(1,1,n)); A_constr_1_4 <- rbind(A.1.1[-1,],A.1.2[-1,]) # LCAR, DCAR, ICAR
R_1_4_scaled<- R_1_4*exp(mean(log(diag(INLA:::inla.ginv(R_1_4)))))  # BYM2
  ## (d.2) Temporal random effect: RW1
R_2_4 <- kronecker(Q_gammaRW2,Q_xi); r_def_2_4 <- 2*n+t-2; A.2.1 <- kronecker(matrix(1,1,t),diag(n)); A.2.2 <- kronecker(diag(t),matrix(1,1,n)); A_constr_2_4 <- rbind(A.2.1[-1,],A.2.2[-1,]) # LCAR, DCAR, ICAR
R_2_4_scaled<- R_2_4*exp(mean(log(diag(INLA:::inla.ginv(R_2_4))))) # BYM2

## Load formulas
source("inla_formulas_JRSSA.R")

##################################
## Fitting models with x1, x5, and x6 
##################################
Data.INLA<- data.frame(O=data$obs, E=data$exp, pop=data$pop_linear,
                       x1_stand=data$x1_stand, x5_stand=data$x5_stand, x6_stand=data$x6_stand, 
                       ID.area= data$ID_area, ID.year=data$ID_year, ID.area.year=data$ID_area_year)

models<-list(lcar.iid.ad=NULL, lcar.rw1.ad=NULL, lcar.rw2.ad=NULL, bym2.iid.ad=NULL, bym2.rw1.ad=NULL, bym2.rw2.ad=NULL, dcar.iid.ad=NULL, dcar.rw1.ad=NULL, dcar.rw2.ad=NULL, icar.iid.ad=NULL, icar.rw1.ad=NULL, icar.rw2.ad=NULL,
             lcar.iid.t1=NULL, lcar.rw1.t1=NULL, lcar.rw2.t1=NULL, bym2.iid.t1=NULL, bym2.rw1.t1=NULL, bym2.rw2.t1=NULL, dcar.iid.t1=NULL, dcar.rw1.t1=NULL, dcar.rw2.t1=NULL, icar.iid.t1=NULL, icar.rw1.t1=NULL, icar.rw2.t1=NULL,
             lcar.rw1.t2=NULL, lcar.rw2.t2=NULL, bym2.rw1.t2=NULL, bym2.rw2.t2=NULL, dcar.rw1.t2=NULL, dcar.rw2.t2=NULL, icar.rw1.t2=NULL, icar.rw2.t2=NULL,
             lcar.iid.t3=NULL, lcar.rw1.t3=NULL, lcar.rw2.t3=NULL, bym2.iid.t3=NULL, bym2.rw1.t3=NULL, bym2.rw2.t3=NULL, dcar.iid.t3=NULL, dcar.rw1.t3=NULL, dcar.rw2.t3=NULL, icar.iid.t3=NULL, icar.rw1.t3=NULL, icar.rw2.t3=NULL,
             lcar.rw1.t4=NULL, lcar.rw2.t4=NULL, bym2.rw1.t4=NULL, bym2.rw2.t4=NULL, dcar.rw1.t4=NULL, dcar.rw2.t4=NULL, icar.rw1.t4=NULL, icar.rw2.t4=NULL)
for(i in 1:length(formulas)){
  models[[i]]<- INLA::inla(formulas[[i]], family="poisson", data=Data.INLA, E=pop, control.predictor=list(compute=TRUE, cdf=c(log(1))), control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE), control.inla=list(strategy="laplace", npoints=21))
}

## save models
##########################################################################################
##########################################################################################
