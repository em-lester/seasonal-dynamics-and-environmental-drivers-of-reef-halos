###################################
## Run a GAMM
##################################

# Clear memory ----
rm(list=ls())

# Install and load packages ----

library(tidyverse)
library(maptools)
library(MetBrewer)
library(viridisLite)
library(lubridate)
library(MuMIn)
library(sjPlot)
library(sjmisc)
library(mgcv)
library(patchwork)
library(png)
library(FSSgam)

# Study name---
study<-"seasonal.halos.ningaloo"

## Set your working directory ----
working.dir <-  "~/Repositories/Seasonal_Halos_Ningaloo"

## Save these directory names to use later---- 

data.dir<-paste(working.dir,"Data",sep="/")
plots.dir=paste(working.dir,"Plots",sep="/")
tidy.dir<-paste(data.dir,"Tidy",sep="/")
raw.dir<-paste(data.dir,"Raw",sep="/")
error.dir=paste(data.dir,"Errors to check",sep="/")
shapefile.dir <- paste(data.dir,"Shapefiles",sep="/")
m.dir <- paste(working.dir, "Model Outputs", sep="/")

# Read in the data----

setwd(tidy.dir)
dir()

dat <- read.csv("halo_patchreef_enviro_lag_data.csv")%>%
  mutate_at(vars(id, Location, Status), list(as.factor)) %>%
  dplyr::rename(meanlength = MeanLength)%>%
  dplyr::rename(status = Status)%>%
  dplyr::rename(patchreefarea = area)%>%
  group_by(id)%>%
  mutate(FiveYearMean =mean(meanlength))%>%
  ungroup()%>%
  group_by(id, month, year)%>%
  mutate(Relative_Length =  (FiveYearMean - meanlength)*-1)%>%
  dplyr::rename(response =Relative_Length )%>%
  glimpse()

sumdat <- dat %>%
  dplyr::select(id, month, year)%>%
  group_by(year, month)%>%
  count()%>%
  glimpse()
  
write.csv(sumdat, "halo_counts.csv")

glimpse(dat)
gplot <- ggplot(data = dat, aes(x = month, y = response, colour = year)) +
  geom_point(size=3, colour="#06485E")+
  theme_classic()
gplot

name<-"seasonal.halos"

# Set predictor variables---
head(dat)
colnames(dat)
pred.vars=c("mean_sst", "mean_chla", "total_monthly_rain", "monthly_PAR","hs","vwnd",
            "mean_chla_t1", "monthly_PAR_t1", "total_monthly_rain_t1","hs_t1", "vwnd_t1",
            "mean_sst_t2", "mean_chla_t2", "monthly_PAR_t2", "total_monthly_rain_t2", "hs_t2", "vwnd_t2")

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat[,pred.vars], use = "complete.obs"),2)

# All correlated but not huge so should be fine if use full subsets model selection code

# Plot of likely transformations
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-dat[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

# Transformations

par(mfrow=c(1,1))
# Log rainfall t1 and t2
# log chla t1 and t2

colnames(dat)
dat$log.total_monthly_rain <- log(dat$total_monthly_rain+1)
dat$log.total_monthly_rain_t1 <- log(dat$total_monthly_rain_t1+1)
dat$log.total_monthly_rain_t2 <- log(dat$total_monthly_rain_t2+1)


dat$log.mean_chla <- log(dat$mean_chla+1)
dat$log.mean_chla_t1 <- log(dat$mean_chla_t1+1)
dat$log.mean_chla_t2 <- log(dat$mean_chla_t2+1)

# # Re-set the predictors for modeling----
#update removed chl-a

pred.vars=c("mean_sst", "log.total_monthly_rain", "monthly_PAR","hs","vwnd",
             "monthly_PAR_t1", "log.total_monthly_rain_t1","hs_t1", "vwnd_t1",
            "mean_sst_t2", "monthly_PAR_t2", "log.total_monthly_rain_t2", "hs_t2", "vwnd_t2") 
plot(dat$response)
hist(dat$response)

# Check to make sure Response vector has not more than 80% zeros----

# Add in dummy var as not looping through? 
dat <- cbind(loop = "X", dat)

unique.vars=unique(as.character(dat$loop))

unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$loop==unique.vars[i]),]
  if(length(which(temp.dat$response==0))/nrow(temp.dat)<0.8){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}
unique.vars.use     
colnames(dat)

# Run the full subset model selection----

resp.vars=unique.vars.use
use.dat=dat
factor.vars=NA # no factor variables
out.all=list()
var.imp=list()
glimpse(dat)
summary(dat)

setwd(m.dir)

dat <- na.omit(dat)
dat <- as.data.frame(dat)
head(dat)
pred.vars
str(use.dat)
use.dat <- as.data.frame(use.dat)

# Loop through the FSS function for each Taxa----

for(i in 1:length(resp.vars)){
  use.dat=dat[which(dat$loop==resp.vars[i]),]
  
  Model1=gam(response~s(mean_sst,k=3,bs='cr') + s(year,bs='re') + s(month,bs='re'), 
             family=gaussian(),  data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               pred.vars.cont=pred.vars,
                               pred.vars.fact=NA,
                               factor.factor.interactions = F, #?
                               factor.smooth.interactions = F,
                               #list(
                               #fact.vars=NA,
                               #cont.vars=c("mean_sst", "log.mean_chla", "log.total_monthly_rain", "monthly_PAR")), #?
                               k=3,
                               cov.cutoff=0.3,
                               null.terms="s(year,bs='re')", "s(month,bs='re')")
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=3),]
  out.all=c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2
  
  # plot the best models 
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    
    png(file=paste(name,m,resp.vars[i],"mod_fits.png",sep="_"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}  
    dev.off()
  }
}

# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits[,-2],file=paste(name,"all.mod.fits.csv",sep="_"))
write.csv(all.var.imp,file=paste(name,"all.var.imp.csv",sep="_"))
all.mod.fits

## Fin