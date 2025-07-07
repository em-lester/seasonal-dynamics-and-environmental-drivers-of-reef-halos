###################################
## Prep data for models
##################################

# Clear memory ----
rm(list=ls())

# Install and load packages ----
#install.packages("GGally")
install.packages("tidyr")
library(tidyr)
library(GGally)
library(tidyverse)
library(ggplot2)
library(maptools)
library(MetBrewer)
library(viridisLite)
library(lubridate)
library(MuMIn)
library(sjPlot)
library(sjmisc)

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

# Read in the data----

setwd(tidy.dir)
dir()

halo_dat <- read.csv("2017_2021_mean_lengths.csv")%>%
  mutate(
    date=ymd(date),
    year = factor(year(date)),
    month = factor(month(date, label = FALSE)))%>%
  dplyr::select(id, Location, Status, date, MeanLength, month, year)%>%
  mutate_at(vars(id, Location, Status), list(as.factor)) %>%
  unique()%>%
  glimpse()

env_dat <- read.csv("SST_Chla_2017_2021.csv") %>%
  dplyr::select(mean_sst, mean_chla, month, year)%>%
  mutate_at(vars(month, year), list(as.factor)) %>%
  glimpse()

patchreef_dat <- read.csv("patchreef_area_cloates.csv")%>%
  mutate_at(vars(id, Location), list(as.factor)) %>%
  glimpse()

rainfall_PAR_dat <- read.csv("Rainfall_PAR.csv")%>%
  mutate_at(vars(month, year), list(as.factor)) %>%
  dplyr::select(month, year, total_monthly_rain, monthly_PAR)%>%
  glimpse()

wind_wave_dat <- read.csv("wind_wave_2017_2021.csv")%>%
  mutate(year = sprintf("20%s", year))%>%
  mutate_at(vars(month, year), list(as.factor)) %>%
  dplyr::select(month, year, hs, vwnd)%>%
  glimpse()

# joining data frames together ----

halo_env_dat <- left_join(halo_dat, env_dat, by=c("month", "year"))%>%
  glimpse()

halo_env_dat <- left_join(halo_env_dat, rainfall_PAR_dat, by=c("month", "year"))%>%
  glimpse()

data <- left_join(patchreef_dat, halo_env_dat, by=c("id", "Location"))%>%
  na.omit()%>%
  glimpse()                          

dat <- left_join(data, wind_wave_dat, by =c("month", "year"))%>%
  glimpse()


setwd(tidy.dir)

# Plot vars  ----

Theme1 <- 
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=25),
    legend.title = element_blank(),
    plot.title=element_text(size=17),
    text=element_text(size=25),
    strip.text.y = element_text(size = 27,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=16),
    axis.title.y=element_text(vjust=0.6, angle=90, size=18),
    axis.text.x=element_text(size=15),
    axis.text.y=element_text(size=15),
    strip.background = element_blank())
  

ggplot_sst_month <-  ggplot(aes(x=month,y=mean_sst,  color=year), data=dat)+
  geom_point()+
  ylab("Mean SST")+
  xlab('Month')+
  scale_color_manual(values=met.brewer("Renoir", 5))+
  theme_classic()
ggplot_sst_month

setwd(plots.dir)
ggsave("monthly_temp.tiff", ggplot_sst_month, dpi=300 )

ggplot_chla_month <-  ggplot(aes(x=month,y=mean_chla,  color=year), data=dat)+
  geom_point()+
  ylab("Mean Chla")+
  xlab('Month')+
  scale_color_manual(values=met.brewer("Renoir", 5))+
  theme_classic()
ggplot_chla_month

setwd(plots.dir)
ggsave("monthly_chla.tiff", ggplot_chla_month, dpi=300 )

ggplot_sst <-  ggplot(aes(x=mean_sst,y=MeanLength,  color=id), data=dat)+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('SST')+
  scale_color_manual(values=met.brewer("Renoir", 22))+
  theme_classic()
ggplot_sst


ggplot_chla <-  ggplot(aes(x=mean_chla,y=MeanLength,  color=year), data=dat)+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('Chl a')+
  scale_color_manual(values=met.brewer("Renoir", 5))+
  theme_classic()
ggplot_chla

ggplot_rain <-  ggplot(aes(x=total_monthly_rain,y=MeanLength,  color=year), data=dat)+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('Total Rainfall per month (mm)')+
  scale_color_manual(values=met.brewer("Renoir", 5))+
  theme_classic()
ggplot_rain

ggplot_PAR<-  ggplot(aes(x=monthly_PAR,y=MeanLength,  color=year), data=dat)+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('Mean PAR')+
  scale_color_manual(values=met.brewer("Renoir", 5))+
  theme_classic()
ggplot_PAR

ggplot_patchreef <-  ggplot(aes(x=area,y=MeanLength,  color=year), data=dat)+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('Patchreef Area')+
  scale_color_manual(values=met.brewer("Renoir", 5))+
  theme_classic()
ggplot_patchreef

colnames(dat)
ggplot_wind <-  ggplot(aes(x=vwnd,y=MeanLength,  color=year), data=dat)+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('Wind Speed (kmph')+
  scale_color_manual(values=met.brewer("Renoir", 5))+
  theme_classic()
ggplot_wind

ggplot_wave <-  ggplot(aes(x=hs,y=MeanLength,  color=year), data=dat)+
  geom_point()+
  ylab("Mean halo length (m)")+
  xlab('Sig. wave height')+
  scale_color_manual(values=met.brewer("Renoir", 5))+
  theme_classic()
ggplot_wave

# Check distribution of variables

# Set predictor variables ----

pred.vars=c("mean_sst","area", "monthly_PAR", "total_monthly_rain") 
response = dat$MeanLength

# Check for correlation of predictor variables----
head(dat)
library(GGally)
ggpairs(select(dat, MeanLength, mean_sst, monthly_PAR,total_monthly_rain, hs, vwnd,))

round(cor(dat[,pred.vars]),2)
summary(response)


# Plot of likely transformations - thanks to Anna Cresswell for this loop!
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

par(mfrow=c(1,1))

# Can sqrt patch reef area
# Log rainfall
# log chla 


dat$sqrtmean_chla <- sqrt(dat$mean_chla)
dat$logmean_chla <- (log(dat$mean_chla))+1
dat$log_rainfall <- (log(dat$total_monthly_rain))+1

plot(dat$sqrtmean_chla)
plot(dat$logmean_chla)
plot(dat$log_rainfall)
plot(dat$monthly_PAR)
summary(dat$sqrtmean_chla)
summary(dat$logmean_chla)

# Add in lag for 0, -1 and -2 months prior to halo measurements ----
glimpse(dat)

dat_lag <- dat %>%
  group_by(id) %>%
  arrange(date) %>%
  mutate(
    mean_sst_t1 = lag(mean_sst, order_by = date, n=1),
    mean_chla_t1 = lag(mean_chla, order_by = date, n=1),
    monthly_PAR_t1 = lag(monthly_PAR, order_by = date, n=1),
    total_monthly_rain_t1 = lag(total_monthly_rain, order_by=date, n=1),
    hs_t1 = lag(hs, order_by = date, n=1),
    vwnd_t1 = lag(vwnd, order_by = date, n=1),
    mean_sst_t2 = lag(mean_sst, order_by = date, n=2),
    mean_chla_t2 = lag(mean_chla, order_by = date, n=2),
    monthly_PAR_t2 = lag(monthly_PAR, order_by = date, n=2),
    total_monthly_rain_t2 = lag(total_monthly_rain, order_by=date, n=2),
    hs_t2 = lag(hs, order_by = date, n=2),
    vwnd_t2 = lag(vwnd, order_by = date, n=2)) %>%
  ungroup()%>%
  glimpse()

# write csv file in tidy dir ----

setwd(tidy.dir)
dir()

write.csv( dat_lag, "halo_patchreef_enviro_lag_data.csv")
#fin