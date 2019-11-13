##########################################################################################
## Title: Crime against women in India: unveiling spatial patterns and temporal trends  ##
##        of dowry deaths in the districs of Uttar Pradesh                              ##
## Authors: Vicente, G. - Goicoa, T. - Fernandez-Rasines, P. - Ugarte, M.D.             ##
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

## Standardized of covariates
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

## fc risk
R<- sum(data$obs)/sum(data$pop_linear) 
fc.risks<-function(x){ x/R }

##################################
## load results
##################################
## load the results obtained with "fit_models_JRSSA.R"

model<- "" ## selected model 

##########################################################################################
## Figures                                                                              ##
##########################################################################################
##################################
## Figure 3.  PIT histograms for the additive model (left) and the Type II interaction model (right)
##################################
## function Probability Integral Transform
pit <- function(x, Px, Px1, n.bins=10, y.max=2.75, my.title="PIT histogram") {
  a.mat <- matrix(0,n.bins,length(x))
  k.vec <- pmax(ceiling(n.bins*Px1),1)
  m.vec <- ceiling(n.bins*Px)
  d.vec <- Px-Px1
  for (i in 1:length(x))
  {
    if (k.vec[i]==m.vec[i]) {a.mat[k.vec[i],i]=1}
    else 
    { 
      a.mat[k.vec[i],i]=((k.vec[i]/n.bins)-Px1[i])/d.vec[i]
      if ((k.vec[i]+1)<=(m.vec[i]-1))
      {for (j in ((k.vec[i]+1):(m.vec[i]-1))) {a.mat[j,i]=(1/(n.bins*d.vec[i]))}}
      a.mat[m.vec[i],i]=(Px[i]-((m.vec[i]-1)/n.bins))/d.vec[i]     
    }
  }
  a <- apply(a.mat,1,sum)
  a <- (n.bins*a)/(length(x))
  p <- (0:n.bins)/n.bins
  PIT <- "Probability Integral Transform"
  RF <- "Relative Frequency"
  plot(p, p, ylim=c(0,y.max), type="n", xlab=PIT, ylab=RF, main=my.title, cex.axis=1.2, cex.lab=1.2, cex.main=1.2) 
  temp1 <- ((1:n.bins)-1)/n.bins
  temp2 <- ((1:n.bins)/n.bins)
  o.vec <- rep(0,n.bins)
  segments(temp1,o.vec,temp1,a)
  segments(temp1,a,temp2,a)
  segments(temp2,o.vec,temp2,a)
  segments(0,0,1,0)
}

## print figure 3
par(mfrow=c(1,2))
pit(x=data$obs, Px=models$icar.rw1.ad$cpo$pit, Px1=models$icar.rw1.ad$cpo$pit-models$icar.rw1.ad$cpo$cpo, n.bins=20, y.max=2, my.title="Additive model")
pit(x=data$obs, Px=models$icar.rw1.t2$cpo$pit, Px1=models$icar.rw1.t2$cpo$pit-models$icar.rw1.t2$cpo$cpo, n.bins=20, y.max=2, my.title="Type II interaction model")

## rm
rm(list = c("pit"))

##################################
## Figure 4.  Relative risk temporal trends for six selected districts in Uttar Pradesh: 
##            Aligarth, Kanpur Dehat, Kheri, Shrawasti Sitapur, and Varanasi
## The additive model(left) and Type II interaction model (right)
##################################
## ID selected districts
id.area<- c(2,40,43,64,66,70)

## log(risks) additive model
marg<- lapply(models$icar.rw1.ad $marginals.fitted.values, function(x){inla.tmarginal(fc.risks, x)})
log.risk.1<- matrix(unlist(lapply(marg, function(x){inla.emarginal(log, x)})), nrow=n, ncol=t, byrow = FALSE)

## log(risks) Type II model
marg<- lapply(models$icar.rw1.t2$marginals.fitted.values, function(x){inla.tmarginal(fc.risks, x)})
log.risk.2<- matrix(unlist(lapply(marg, function(x){inla.emarginal(log, x)})), nrow=n, ncol=t, byrow = FALSE)

## minimum and maximum values
inf<- round(min(log.risk.1[id.area,], log.risk.2[id.area,]),2) -0.1 
top<- round(max(log.risk.1[id.area,], log.risk.2[id.area,]),2) +0.5
x<- 1:t

## color
colores<-c("darkolivegreen","chocolate1", "blue4", "darkorchid2", "deeppink1", "darkgoldenrod3", "cyan4") 

## print figure 4
par(mfrow=c(1,2))
plot(range(x), c(inf, top),type="n",xlab="Year",ylab="", xaxt="n", cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
axis(1, at=seq(1,t), labels=seq(t.from,t.to), las=0, cex.axis=1.2)
title(ylab=expression(log(R[it])), line=2.2, cex.lab=1.2)
for(i in id.area){
  lines(log.risk.1[i,], col=colores[which(id.area==i)] ,lwd=2, type = "l") 
  abline(h=0,lty=2)
}
legend("top", as.character(unique(carto_up$dist)[id.area]), ncol=2, pch=c("-","-","-","-","-","-"), col=colores, bty="n",lwd=2, cex=1.2)
plot(range(x), c(inf, top),type="n",xlab="Year",ylab="", xaxt="n", cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
axis(1, at=seq(1,t), labels=seq(t.from,t.to), las=0, cex.axis=1.2)
title(ylab=expression(log(R[it])), line=2.2, cex.lab=1.2)
for(i in id.area){
  lines(log.risk.2[i,], col=colores[which(id.area==i)] ,lwd=2, type = "l") 
  abline(h=0,lty=2)
}
legend("top", as.character(unique(carto_up$dist)[id.area]), ncol=2, pch=c("-","-","-","-","-","-"), col=colores, bty="n",lwd=2, cex=1.2)


## rm
rm(list = c("id.area", "x", "colores","marg", "log.risk.1", "log.risk.2", "inf", "top","i"))

##################################
## Figure 5. Posterior mean of the distric-specific relative risk, exp(xi_i), and posterior 
##           probabilities P(exp(xi_i))> 1|O) that the relative risks are greater than one
##################################
carto<- carto_up

## Posterior mean of the distric-specific relative risk
carto$zeta <- unlist(lapply(model$marginals.random$ID.area, function(x) inla.emarginal(exp,x)))[1:n]

## P(exp(xi_i))> 1|O)
prob_zeta <- unlist(lapply(model$marginals.random$ID.area, function(x){1-inla.pmarginal(0, x)}))[1:n]
carto$prob_zeta <- Hmisc::cut2(prob_zeta,c(0,0.1,0.2,0.8,0.9,1))

## print figure 5
gridExtra::grid.arrange(sp::spplot(carto, "zeta", names.attr="", main="", col.regions=RColorBrewer::brewer.pal(9,"YlOrRd"),
                                   colorkey=list(labels=c("0.00", "0.25","0.50","0.75","1.00","1.25","1.50","1.75","2.00","2.15"), at=(0:9)/9), at=c(seq(0.00, 2.00,0.25),2.15)),
                        sp::spplot(carto, "prob_zeta", names.attr="", cuts=4, col.regions=RColorBrewer::brewer.pal(5,"PuBu"),
                                   colorkey=list(labels=list(at=c(.5,1.5,2.5,3.5,4.5,5.5),labels = c("0","0.1","0.2","0.8","0.9","1")))),
                        ncol=2)

## rm
rm(list=c("carto","prob_zeta"))

##################################
## Figure 6.  Temporal pattern (posterior mean of exp(gamma_t)) of incidence risks for 
##            dowry deaths in Uttar Pradesh
##################################
## posterior mean of exp(gamma_t)
temporal <- unlist(lapply(model$marginals.random$ID.year, function(x) inla.emarginal(exp,x)))  

## posterior quantiles of exp(gamma_t)
aux <- lapply(model$marginals.random$ID.year, function(x) inla.tmarginal(exp,x))
q1 <- unlist(lapply(aux, function(x) inla.qmarginal(0.025,x)))    
q2 <- unlist(lapply(aux, function(x) inla.qmarginal(0.975,x)))    

## minimum and maximum values
x <- 1:t
inf<- round(min(q1),2)-0.05
top<- round(max(q2),2)+0.05

## print figure 6
plot(range(x),c(inf,top),type="n",xlab="Year",ylab=expression(exp(gamma[t])), xaxt="n", cex.lab=1, cex.axis=1, cex.main=1)
axis(1, at=seq(1,t), labels=seq(t.from,t.to), las=0, cex.axis=1)
X.Vec <- c(x, tail(x, 1), rev(x), x[1])
Y.Vec <- c(q1, tail(q2, 1), rev(q2), q1[1])
polygon(X.Vec, Y.Vec, col = "grey", border = NA)
lines(temporal)
abline(h=1,lty=2)

## rm
rm(list=c("temporal", "aux", "q1", "q2", "x", "X.Vec", "Y.Vec","inf","top"))

##################################
## Figure 7.  Specific temporal trends (posterior mean of exp(delta_it)) for four selected 
##            districts: 43 (Khery), 48 (Mahrajganj), 60 (Saharanpur), and 64 (Shrawasti)
##################################
## posterior mean of exp(delta_it)
exp_Delta<- matrix(unlist(lapply(model$marginals.random$ID.area.year, function(x) inla.emarginal(exp,x))), n, t, byrow=F)

## posterior quantiles of exp(delta_it)
aux <- lapply(model$marginals.random$ID.area.year, function(x) inla.tmarginal(exp,x))
q1 <- matrix(unlist(lapply(aux, function(x) inla.qmarginal(0.025,x))), n, t, byrow=F)
q2 <- matrix(unlist(lapply(aux, function(x) inla.qmarginal(0.975,x))), n, t, byrow=F)

## minimum and maximum values
inf<- round(min(q1),2)-0.05
top<- round(max(q2),2)+0.05
x <- 1:t

## print figure 7
par(mfrow=c(2,2))
for (i in c(2,43,64,70)){ ## ID selected districts
  plot(range(x),c(inf, top), type="n",xlab="Year", ylab="", xaxt="n", cex.main=1.5, cex.lab=1.5, cex.axis=1.5, 
       main=paste0(unique(ID$dist)[i]," (ID district ", ID[ID$ID_area==i,"ID_area"],")")) # 
  title(ylab=expression(exp(delta[it])), line=2.2, cex.lab=1.5)
  axis(1, at=seq(1,t), labels=seq(t.from,t.to), las=0, cex.axis=1.5)  # 
  X.Vec <- c(x, tail(x, 1), rev(x), x[1])
  Y.Vec <- c(q1[i,], tail(q2[i,], 1), rev(q2[i,]), q1[i,1])  
  polygon(X.Vec, Y.Vec, col = "grey", border = NA)
  lines(exp_Delta[i,]) 
  abline(h=1,lty=2)
}

## rm
rm(list=c("x", "exp_Delta", "aux", "q1", "q2", "i", "X.Vec", "Y.Vec","inf", "top"))

##################################
## Figure 8.  Map of estimated incidence risks for dowry deaths in Uttar Pradesh 
##            (posterior means of R_{it})                              ##
## Figure 9. Posterior probability for dowry deaths in Uttar Pradesh, P(R_it>1|O)
##################################
carto<-carto_up

## Figure 8. Map of estimated incidence risks for dowry deaths in Uttar Pradesh
risks <- unlist(lapply(model$marginals.fitted.values, function(x) inla.emarginal(fc.risks,x)))
RME_matrix <- matrix(risks, n, t, byrow=FALSE)
RME_data_frame <- data.frame(carto$dist, RME_matrix)
colnames(RME_data_frame)<- c("dist",paste("RME",seq(t.from,t.to),sep=""))
attr(carto, "data") = data.frame(attr(carto,"data"), RME_data_frame)

## print figure 8
sp::spplot(obj=carto, zcol=paste("RME",seq(t.from,t.to),sep=""), main="", col.regions=RColorBrewer::brewer.pal(8,"YlOrRd"),
           names.attr=as.character(seq(t.from,t.to)),  axes=TRUE, at=c(0.25,0.5,0.75,1,1.25,1.5,1.75,2.25,3.15),
           as.table=TRUE, layout=c(5,3), colorkey=list(labels=c("0.25","0.5","0.75","1","1.25","1.5","1.75","2.25","3.15"),at=(0:8)/8))

## Figure 9. Posterior probability for dowry deaths in Uttar Pradesh, P(Rit>1|O)
marg<- lapply(model$marginals.fitted.values, function(x) {inla.tmarginal(fc.risks,x)})
prp<- unlist(lapply(marg, function(x){1-inla.pmarginal(1, x)}))
prp_matrix <- matrix(Hmisc::cut2(prp,c(0,0.1,0.2,0.8,0.9,1)), n, t, byrow=FALSE)
prp_data_frame <- data.frame(carto$dist)
k=2001
for (i in 1:t) {
  prp_data_frame$aux <- as.factor(prp_matrix)[((i-1)*n+1):(i*n)]
  colnames(prp_data_frame)[i+1] <- paste("PRP",k,sep="")
  k=k+1
}
attr(carto, "data") = data.frame(attr(carto,"data"), prp_data_frame)

## print figure 9
sp::spplot(obj=carto, zcol=paste("PRP",seq(t.from,t.to),sep=""), main="", as.table=TRUE, layout=c(5,3),
           col.regions=RColorBrewer::brewer.pal(5,"PuBu"), cuts=4, names.attr=as.character(seq(t.from,t.to)),
           colorkey=list(labels=list(at=c(.5,1.5,2.5,3.5,4.5,5.5),labels = c("0","0.1","0.2","0.8","0.9","1"))))

## rm
rm(list = c("carto", "risks", "RME_matrix", "RME_data_frame", "marg", "prp", "prp_matrix", "prp_data_frame", "i","k"))

##################################
## Figure 10. Temporal evolution of final risk estimates for some districts in Uttar Pradesh: 
##            Aligarh, Kanpur Dehat, Kheri, Shrawasti, Sitapur, and Varanasi
##################################
## ID selected districts
id.area<- c(2,40,43,64,66,70)

## Function region plot color
region.plot<- function(Cart, region, model, datos, ID.area="ID.area", ID.year="ID.year", O="O", E="E", color, plot.x.values,	plot.x.labels, y.range=NULL, 
                       line.color="blue", area.name=NULL, plot.SMR=TRUE){
  if(is.null(y.range)) y.range=range(datos[,O]/datos[,E])
  n.region <- length(region)
  
  marg<- lapply(model$marginals.fitted.values, function(x) {inla.tmarginal(fc.risks,x)})
  prp<- unlist(lapply(marg, function(x){1-inla.pmarginal(1, x)}))
  
  fitted.values <- data.frame(r_it= as.vector(unlist(lapply(model$marginals.fitted.values, function(x) inla.emarginal(fc.risks,x)))),
                              q1=as.vector(unlist(lapply(marg, function(x){inla.qmarginal(0.025, x)}))),
                              q2=as.vector(unlist(lapply(marg, function(x){inla.qmarginal(0.975, x)}))),
                              ID.area=datos[,ID.area], ID.year=datos[,ID.year],
                              PRP_it=as.vector(matrix(Hmisc::cut2(prp ,c(0.1,0.2,0.8,0.9)),n,t,byrow=F))
  )
  
  for(k in 1:n.region) {
    if(is.null(area.name)){
      region.name <- unique(paste("Area",region[k]))
    }else{
      region.name <- area.name[k] 
    }
    region.fitted.values <- fitted.values[fitted.values$ID.area==region[k],]
    SMR <- datos[datos[,ID.area]==region[k],O]/datos[datos[,ID.area]==region[k],E]
    t <- region.fitted.values$ID.year-min(region.fitted.values$ID.year)+1
    par(mar=c(5,5,4,2))
    plot(x=range(t),y=c(min(fitted.values$q1)-0.1,max(fitted.values$q2)+.1),type="n", xlab="Year", ylab=expression(R[it]), xaxt="n" , cex.axis=1.5, cex.lab=1.5, cex.main=2.5) # 
    for (i in min(plot.x.values):max(plot.x.values)) {
      X.Vec <- c(i-0.5, i+0.5, i+0.5, i-0.5, i-0.5)
      Y.Vec <- c(0,0,10,10,0)
      level <- match(region.fitted.values$PRP_it[i], levels(region.fitted.values$PRP_it))
      polygon(X.Vec, Y.Vec, col=color[level], border=NA)
    }
    
    lines(min(t):max(t),region.fitted.values$q1, col="grey")
    lines(min(t):max(t),region.fitted.values$q2, col="grey")
    
    X.Vec <- c(t[1], length(t), rev(t), t[1])
    Y.inf <- c(-0.5, -0.5, rev(region.fitted.values$q1), -0.5)
    polygon(X.Vec, Y.inf, col = "white", border=NA)
    
    X.Vec <- c(t, length(t), t[1], t[1])
    Y.sup <- c(region.fitted.values$q2, 10, 10, region.fitted.values$q2[1])
    polygon(X.Vec, Y.sup, col = "white", border=NA)
    
    polygon(c(min(t)-0.5,min(t),min(t),min(t)-0.5,min(t)-0.5), c(-0.5,-0.5,10,10,-0.5), col = "white", border=NA)
    polygon(c(max(t),max(t)+0.5,max(t)+0.5,max(t),max(t)), c(-0.5,-0.5,10,10,-0.5), col = "white", border=NA)
    
    box()
    axis(1, at=plot.x.values, labels=plot.x.labels, las=0, cex.axis=1.5)
    lines(region.fitted.values$r_it)
    title(as.character(region.name), cex.lab=2.5)
    
    abline(h=1, lty=2)
  }
}

## print figure 10
par(mfrow=c(3,2))
region.plot(Cart = carto_up, region =  id.area, model = model, datos = data, ID.area = "ID_area", ID.year = "ID_year", O = "obs", E = "exp",
            color = RColorBrewer::brewer.pal(5,"PuBu"), plot.x.values=seq(1,t,1), plot.x.labels=seq(t.from,t.to, 1), 
            area.name=paste0(unique(ID$dist)[id.area]," (ID district ", id.area,")")
            )

## rm
rm(list = c("id.area", "region.plot"))

##########################################################################################
## Tables                                                                               ##
##########################################################################################
##################################
## Table 3. Posterior means, standard deviations and medians of the fixed effects together 
##          with a 95% credible interval
##################################
table.3<- as.data.frame(round(model$summary.fixed[, c(1,2,4,3,5)],3))  
colnames(table.3)<- c("mean", "sd", "median", "q.025", "q.975")
rownames(table.3)<- c("alpha", "x1 (sex ratio)", "x5 (murders)", "x6 (burglaries)")

##########################################################################################
## Supplementary materials                                                              ##
##########################################################################################
##################################
## Figure S.1.  Geographical pattern of SIRs of dowry deaths for years 2001 and 2011 and 
##              spatial pattern of sex ratio, and murders in the same years
##################################
carto<- carto_up
years<- c(2001,2011)
eval(parse(text= paste0("carto$smr_",years,"<-data[data$year==",years,",]$smr") ) )
j<-1:6
for(i in 1:length(years)){ eval(parse(text=  paste0("carto$x",j,"_",years[i],"<-data[data$year==",years[i],",]$x",j)) ) }

## print figure S.1
gridExtra::grid.arrange(sp::spplot(obj=carto, zcol=paste("smr_",years,sep=""), main="",  col.regions=RColorBrewer::brewer.pal(6,"YlOrRd"), 
                                   names.attr=paste0(years," SMR"),  axes=TRUE, at=c(0.25,0.75,1,1.25,1.75,2.25,3),
                                   as.table=TRUE, layout=c(1,2),  colorkey=list(labels=c("0.25","0.75","1","1.25","1.75","2.25","3"),at=(0:6)/6)),
                        sp::spplot(obj=carto, zcol=paste("x1_",years,sep=""), main="",  col.regions=RColorBrewer::brewer.pal(5,"YlOrRd"), 
                                   names.attr=paste0(years," Sex ratio"),  axes=TRUE, at=c(825,875,925,975,1000,1025),
                                   as.table=TRUE, layout=c(1,2),  colorkey=list(labels=c("825","875","925","975","1000","1025"),at=(0:5)/5)),
                        sp::spplot(obj=carto, zcol=paste("x5_",years,sep=""), main="",  col.regions=RColorBrewer::brewer.pal(5,"YlOrRd"), 
                                   names.attr=paste0(years," Murder"),  axes=TRUE, at=c(0.50,2.88,5.25,7.62,10.00,16.00),
                                   as.table=TRUE, layout=c(1,2),  colorkey=list(labels=c("0.50","2.88","5.25","7.62","10.0","16.0"),at=(0:5)/5)),
                        ncol=3)

## rm
rm(list=c("years","carto", "i","j"))

##################################
## Figure S.2.  Relative risk temporal trends for three different districts in Uttar Pradesh: 
##              Aligarth, Kheri, and  Varanasi
##################################
model.1<- models$icar.rw1.ad 
model.2<- models$icar.rw1.t2

## ID selected districts
id.area<- c(2,43,70)

## Standardized mortality ratio
SIR<- matrix(data$smr, nrow=n, ncol=t, byrow=FALSE)

## relative risk model.1
risk.1<- matrix(unlist(lapply(model.1$marginals.fitted.values, function(x){inla.emarginal(fc.risks, x)})), nrow=n, ncol=t, byrow = FALSE)
marg<- lapply(model.1$marginals.fitted.values, function(x){inla.tmarginal(fc.risks, x)})
q1.1<- matrix(unlist(lapply(marg, function(x){inla.qmarginal(0.025, x)})), nrow=n, ncol=t, byrow = FALSE)
q2.1<- matrix(unlist(lapply(marg, function(x){inla.qmarginal(0.975, x)})), nrow=n, ncol=t, byrow = FALSE)

## relative risk model.2
risk.2<- matrix(unlist(lapply(model.2$marginals.fitted.values, function(x){inla.emarginal(fc.risks, x)})), nrow=n, ncol=t, byrow = FALSE)
marg<- lapply(model.2$marginals.fitted.values, function(x){inla.tmarginal(fc.risks, x)})
q1.2<- matrix(unlist(lapply(marg, function(x){inla.qmarginal(0.025, x)})), nrow=n, ncol=t, byrow = FALSE)
q2.2<- matrix(unlist(lapply(marg, function(x){inla.qmarginal(0.975, x)})), nrow=n, ncol=t, byrow = FALSE)

## minimum and maximum values
inf<-0.15; top<- 3.5
x<-1:t

## print figure S.2
par(mfrow=c(3,2))
for(i in id.area){
  plot(range(x), c(inf, top),type="n",xlab="Year",ylab="", xaxt="n", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, main=paste0(as.character(unique(carto_up$dist)[id.area][which(id.area==i)]), " (ID district ",i,")") )
  title(ylab=expression(R[it]), line=2.2, cex.lab=1.5)
  axis(1, at=seq(1,t), labels=seq(t.from,t.to), las=0, cex.axis=1.5)
  X.Vec <- c(x, tail(x, 1), rev(x), x[1])
  Y.Vec <- c(q1.1[i,], tail(q2.1[i,], 1), rev(q2.1[i,]), q1.1[1])
  polygon(X.Vec, Y.Vec, col = "azure3", border = NA)
  lines(SIR[i,], col="chocolate1",lwd=3, type = "l")
  lines(risk.1[i,], col="black",lwd=3, type = "l")
  lines(risk.2[i,], col="darkorchid2",lwd=3, type = "l")
  legend("topleft",  c("Standardized mortality ratio (SMR)", "Estimated incidence risks (Additive)", "Estimated incidence risks (Type II)"), ncol=1, pch=c("-","-","-"), col=c("chocolate1","black","darkorchid2"), bty="n",lwd=3, cex=1.3)
  abline(h=1,lty=2)
  plot(range(x), c(inf, top),type="n",xlab="Year",ylab="", xaxt="n", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, main=paste0(as.character(unique(carto_up$dist)[id.area][which(id.area==i)]), " (ID district ",i,")") )
  title(ylab=expression(R[it]), line=2.2, cex.lab=1.5)
  axis(1, at=seq(1,t), labels=seq(t.from,t.to), las=0, cex.axis=1.5)
  X.Vec <- c(x, tail(x, 1), rev(x), x[1])
  Y.Vec <- c(q1.2[i,], tail(q2.2[i,], 1), rev(q2.2[i,]), q1.2[1])
  polygon(X.Vec, Y.Vec, col = "azure3", border = NA)
  lines(SIR[i,], col="chocolate1",lwd=3, type = "l")
  lines(risk.2[i,], col="darkorchid2",lwd=3, type = "l")
  lines(risk.1[i,], col="black",lwd=3, type = "l")
  legend("topleft",  c("Standardized mortality ratio (SMR)", "Estimated incidence risks (Type II)", "Estimated incidence risks (Additive)"), ncol=1, pch=c("-","-","-"), col=c("chocolate1","darkorchid2","black"), bty="n",lwd=3, cex=1.3)
  abline(h=1,lty=2)
}

## rm
rm(list=c("model.1", "model.2", "id.area", "x","SIR", "marg","risk.1", "q1.1", "q2.1","risk.2", "q1.2", "q2.2", "inf", "top", "X.Vec", "Y.Vec","i" ))

##################################
## Figure S.3.  Dispersion plots of the final relative risks obtained with the Type II
##              interaction model with an ICAR spatial prior vs. Type II interaction models
##              with LCAR, DCAR, and BYM2      
##################################
models.s3<- list(icar.rw1.t2=models$icar.rw1.t2, lcar.rw1.t2=models$lcar.rw1.t2, dcar.rw1.t2=models$dcar.rw1.t2, bym2.rw1.t2=models$bym2.rw1.t2)

## relative risk
risks<-list()
for(i in 1:length(models.s3)){
  risks[[i]]<- unlist(lapply(models.s3[[i]]$marginals.fitted.values, function(x) inla.emarginal(fc.risks,x)))
}
md<-c("ICAR", "LCAR", "DCAR", "BYM2")

## print figure S.3
par(mfrow=c(2,2))
for(i in c(2:4)){
  plot(risks[[1]], risks[[i]], col=2, xlab=paste0("Model ", md[1], ": Rit"), ylab=paste0("Model ", md[i], ": Rit"), cex=1, cex.axis=1.5, cex.lab=1.5)
  abline(a=0,b=1)
}

## rm
rm(list=c("models.s3","risks", "md"))

##################################
## Table S.1. Posterior means, standard deviations and medians of the fixed effects together
##            with a 95% credible interval (x1, x2, x3, x4, x5, x6)
##################################
table.s1<- as.data.frame(round(models_all$icar.rw1.t2$summary.fixed[, c(1,2,4,3,5)],3))  
colnames(table.s1)<- c("mean", "sd", "median", "q.025", "q.975")
rownames(table.s1)<- c("alpha (x0 BJP)", "x0 (BSP)", "x0 (SP)", "x1 (sex ratio)", "x2 (population density)", "x3 (female literacy)", 
                       "x4 (per capita income)", "x5 (murders)", "x6 (burglaries)")

##################################
## Table S.2. Model selection criteria for different models including the covariates, 
##            x1, x5, and x6
##################################
models_s2<- list(lcar.rw1.ad=models$lcar.rw1.ad, lcar.rw2.ad=models$lcar.rw2.ad, bym2.rw1.ad=models$bym2.rw1.ad, bym2.rw2.ad=models$bym2.rw2.ad,
                 dcar.rw1.ad=models$dcar.rw1.ad, dcar.rw2.ad=models$dcar.rw2.ad, icar.rw1.ad=models$icar.rw1.ad, icar.rw2.ad=models$icar.rw2.ad,
                 lcar.rw1.t1=models$lcar.rw1.t1, lcar.rw2.t1=models$lcar.rw2.t1, bym2.rw1.t1=models$bym2.rw1.t1, bym2.rw2.t1=models$bym2.rw2.t1,
                 dcar.rw1.t1=models$dcar.rw1.t1, dcar.rw2.t1=models$dcar.rw2.t1, icar.rw1.t1=models$icar.rw1.t1, icar.rw2.t1=models$icar.rw2.t1,
                 lcar.rw1.t2=models$lcar.rw1.t2, lcar.rw2.t2=models$lcar.rw2.t2, bym2.rw1.t2=models$bym2.rw1.t2, bym2.rw2.t2=models$bym2.rw2.t2,
                 dcar.rw1.t2=models$dcar.rw1.t2, dcar.rw2.t2=models$dcar.rw2.t2, icar.rw1.t2=models$icar.rw1.t2, icar.rw2.t2=models$icar.rw2.t2,
                 lcar.rw1.t3=models$lcar.rw1.t3, lcar.rw2.t3=models$lcar.rw2.t3, bym2.rw1.t3=models$bym2.rw1.t3, bym2.rw2.t3=models$bym2.rw2.t3,
                 dcar.rw1.t3=models$dcar.rw1.t3, dcar.rw2.t3=models$dcar.rw2.t3, icar.rw1.t3=models$icar.rw1.t3, icar.rw2.t3=models$icar.rw2.t3,
                 lcar.rw1.t4=models$lcar.rw1.t4, lcar.rw2.t4=models$lcar.rw2.t4, bym2.rw1.t4=models$bym2.rw1.t4, bym2.rw2.t4=models$bym2.rw2.t4,
                 dcar.rw1.t4=models$dcar.rw1.t4, dcar.rw2.t4=models$dcar.rw2.t4, icar.rw1.t4=models$icar.rw1.t4, icar.rw2.t4=models$icar.rw2.t4)

table.s2<- matrix(NA, nrow=length(models_s2), ncol=5)
colnames(table.s2)<-c("Mean Post D","pD","DIC","WAIC","LS")
random.effect<- strsplit(names(models_s2),".", fixed = TRUE)

spatemp<-spat<-temp<-c()
for(i in 1:length(models_s2)){
  aux<-models_s2[[i]]
  table.s2[i,]<-round(c(aux$dic$mean.deviance, aux$dic$p.eff, aux$dic$dic, aux$waic$waic, -mean(log(aux$cpo$cpo),na.rm=T)), 3)
  spatemp[i]<- random.effect[[i]][3]
  spat[i]<- random.effect[[i]][1]
  temp[i]<- random.effect[[i]][2]
}
table.s2<-as.data.frame(table.s2)
table.s2<-cbind(spatemp, spat, temp, table.s2)

## rm
rm(list=c("models_s2", "random.effect","spatemp", "spat", "temp", "i", "aux"))

##################################
## Table S.3. Estimated incidence rate of dowry deaths by year per 100,000 women aged
##            between 15 and 49 in districts Aligarh, Kanpur Dehat, Kheri, Shrawasti,
##            Sitapur, and Varanasi
##################################
## fc rate(100000)
fc.rate<- function(x){x*100000 }

## rate(100000)
pit_r <- matrix(unlist(lapply(model$marginals.fitted.values, function(x) inla.emarginal(fc.rate,x))), n, t, byrow=FALSE)

## ID selected districts
id.area<- c(2,40,43,64,66,70)

## table
table.s3<- as.data.frame(cbind(ID$dist[id.area], pit_r[id.area,])) 
colnames(table.s3)<- c("District",as.character(t.from:t.to))
table.s3$District<- ID$dist[id.area]

## rm
rm(list=c("fc.rate", "id.area"))

##########################################################################################
##########################################################################################