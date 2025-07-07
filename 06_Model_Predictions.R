###################################
## Make nice plots of predictions
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
library(MoMAColors)

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

dat <- read.csv("halo_patchreef_enviro_data.csv")%>%
  mutate_at(vars(id, Location, Status), list(as.factor)) %>%
  dplyr::rename(meanlength = MeanLength)%>%
  dplyr::rename(status = Status)%>%
  dplyr::rename(patchreefarea = area)%>%
  #dplyr::rename(response = meanlength)%>%
  dplyr::mutate(log.total_monthly_rain =log(total_monthly_rain+1)) %>%
  dplyr::mutate(log.mean_chla = log(mean_chla+1))%>%  
  dplyr::mutate(log.mean_sst = log(mean_sst+1))%>% 
  group_by(id)%>%
  mutate(FiveYearMean =mean(meanlength))%>%
  ungroup()%>%
  group_by(id, month, year)%>%
  mutate(Relative_Length =  (FiveYearMean - meanlength)*-1)%>%
  dplyr::rename(response =Relative_Length )%>%
  distinct()%>%
  glimpse()


# MODEL  (response ~ mean_sst + vwnd) ----

gamm=gam(response~ s(mean_sst,k=3,bs='cr')+ vwnd + s(year,bs="re"), family=gaussian,data=dat)
summary(gamm)
gam.check(gamm)

# predict - mean_sst ----
mod<-gamm
testdata.mean_sst <- expand.grid(mean_sst=seq(min(dat$mean_sst),max(dat$mean_sst),length.out = 20),
                                 vwnd=mean(mod$model$vwnd),
                                 year=(mod$model$year))%>%
  distinct()%>%
  glimpse()

fits.mean_sst <- predict.gam(mod, newdata=testdata.mean_sst, type='response', se.fit=T)
glimpse(fits.mean_sst)

predicts.mean_sst = testdata.mean_sst%>%data.frame(fits.mean_sst)%>%
  group_by(mean_sst)%>% #only change here
  dplyr::summarise(response=mean(fit),se.fit=(mean(se.fit)*2))%>%
  ungroup()

predicts.mean_sst 

# predict - vwnd ----
mod<-gamm

plot(dat$vwnd)

testdata.vwnd <- expand.grid(vwnd=seq(min(dat$vwnd),max(dat$vwnd),length.out = 20),
                                    mean_sst=mean(mod$model$mean_sst),
                                    year=(mod$model$year))%>%
  distinct()%>%
  glimpse()

fits.vwnd <- predict.gam(mod, newdata=testdata.vwnd, type='response', se.fit=T)

predicts.vwnd = testdata.vwnd%>%data.frame(fits.vwnd)%>%
  group_by(vwnd)%>% #only change here
  dplyr::summarise(response=mean(fit),se.fit=mean(se.fit)*2)%>%
  ungroup()
predicts.vwnd
# PLOTS  ----

# Summary data for plots ----

#summarySE function

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Summary data SST ----

summary_dat <- summarySE(testdata.mean_sst, measurevar="mean_sst", groupvars=c("mean_sst"))
summary_dat
# Set a theme ----

Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    text=element_text(size=12),
    strip.text.y = element_text(size = 13,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=13),
    axis.title.y=element_text(vjust=0.6, angle=90, size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())


# mean_sst ----

ggmod.mean_sst <- ggplot(aes(x=mean_sst,y=response,fill=mean_sst,colour=mean_sst), data=predicts.mean_sst) +
  xlab("Mean Monthly SST (°C)")+
  ylab('Relative Halo Size (m)')+
  scale_colour_gradientn(colours="#EF6E5E")+
  scale_fill_gradientn(colours="#EF6E5E")+
  ylim(-50,50)+
  geom_point(data=dat, aes(x=mean_sst,y=response), alpha=0.2)+
  geom_line(data=predicts.mean_sst,show.legend=TRUE)+
  geom_ribbon(aes(ymin = response - se.fit, ymax = response + se.fit), alpha = 0.4, fill = "#EF6E5E", colour="#AA7C6B", linetype = 'blank') +
  theme_classic()+
  Theme1

ggmod.mean_sst

head(predicts.mean_sst)

# predicts.vwnd ----

ggmod.vwnd <- ggplot(aes(x=vwnd,y=response,fill=vwnd,colour=vwnd), data=predicts.vwnd) +
  xlab(expression("V Wind (m s"^"-1"*")"))+
  ylab('Relative Halo Size (m)')+
  scale_colour_gradientn(colours="#6FA9F1")+
  scale_fill_gradientn(colours="#6FA9F1")+
  ylim(-50,50)+
  geom_point(data=dat, aes(x=vwnd,y=response), alpha=0.2)+
  geom_line(data=predicts.vwnd,show.legend=TRUE)+
  geom_ribbon(aes(ymin = response - se.fit, ymax = response + se.fit), alpha = 0.4, fill = "#6FA9F1", colour="#2a9d8f", linetype = 'blank') +
  theme_classic()+
  Theme1

ggmod.vwnd

# Combine with patchwork ----

model_predictions <- (ggmod.mean_sst/ggmod.vwnd)+ plot_annotation(tag_levels = 'A')
model_predictions

setwd(plots.dir)
ggsave("gamm_predictions_R1_600dpi.tiff",model_predictions, dpi=600, width=4, height=7)

#Fin